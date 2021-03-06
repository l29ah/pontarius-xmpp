{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
--
-- Submodule for functionality related to SASL negotation:
-- authentication functions, SASL functionality, bind functionality,
-- and the legacy `{urn:ietf:params:xml:ns:xmpp-session}session'
-- functionality.

module Network.Xmpp.Sasl
    ( xmppSasl
    , digestMd5
    , scramSha1
    , plain
    , auth
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Text (Text)
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           System.Log.Logger (debugM, errorM, infoM)

-- | Uses the first supported mechanism to authenticate, if any. Updates the
-- state with non-password credentials and restarts the stream upon
-- success. Returns `Nothing' on success, an `AuthFailure' if
-- authentication fails, or an `XmppFailure' if anything else fails.
xmppSasl :: [SaslHandler] -- ^ Acceptable authentication mechanisms and their
                       -- corresponding handlers
         -> Stream
         -> IO (Either XmppFailure (Maybe AuthFailure))
xmppSasl handlers stream = do
    debugM "Pontarius.Xmpp" "xmppSasl: Attempts to authenticate..."
    flip withStream stream $ do
        -- Chooses the first mechanism that is acceptable by both the client and the
        -- server.
        mechanisms <- gets $ streamFeaturesMechanisms . streamFeatures
        case (filter (\(name, _) -> name `elem` mechanisms)) handlers of
            [] -> return $ Right $ Just $ AuthNoAcceptableMechanism mechanisms
            (_name, handler):_ -> do
                cs <- gets streamConnectionState
                case cs of
                    Closed -> do
                        lift $ errorM "Pontarius.Xmpp" "xmppSasl: Stream state closed."
                        return . Left $ XmppNoStream
                    _ -> runExceptT $ do
                           -- TODO: Log details about handler? SaslHandler "show" instance?
                           lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Performing handler..."
                           r <- ExceptT handler
                           case r of
                               Just ae -> do
                                   lift $ lift $ errorM "Pontarius.Xmpp" $
                                       "xmppSasl: AuthFailure encountered: " ++
                                           show ae
                                   return $ Just ae
                               Nothing -> do
                                   lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Authentication successful, restarting stream."
                                   _ <- ExceptT restartStream
                                   lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Stream restarted."
                                   return Nothing

-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> Stream
     -> IO (Either XmppFailure (Maybe AuthFailure))
auth mechanisms resource con = runExceptT $ do
    mbAuthFail <- ExceptT $ xmppSasl mechanisms con
    case mbAuthFail of
        Nothing -> do
            _jid <- ExceptT $ xmppBind resource con
            ExceptT $ flip withStream' con $ do
                s <- get

                case sendStreamElement s of
                    False -> return $ Right Nothing
                    True -> do
                        _ <- liftIO $ startSession con
                        return $ Right Nothing
        f -> return f
  where
    sendStreamElement s =
        and [ -- Check that the stream feature is set and not optional
              streamFeaturesSession (streamFeatures s) == Just False
            ]


-- Produces a `bind' element, optionally wrapping a resource.
bindBody :: Maybe Text -> Element
bindBody = pickleElem $
               -- Pickler to produce a
               -- "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>"
               -- element, with a possible "<resource>[JID]</resource>"
               -- child.
               xpBind . xpOption $ xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}resource" (xpContent xpId)

-- Sends a (synchronous) IQ set request for a (`Just') given or server-generated
-- resource and extract the JID from the non-error response.
xmppBind  :: Maybe Text -> Stream -> IO (Either XmppFailure Jid)
xmppBind rsrc c = runExceptT $ do
    lift $ debugM "Pontarius.Xmpp" "Attempts to bind..."
    answer <- ExceptT $ pushIQ "bind" Nothing Set Nothing (bindBody rsrc) c
    case answer of
        Right IQResult{iqResultPayload = Just b} -> do
            lift $ debugM "Pontarius.Xmpp" "xmppBind: IQ result received; unpickling JID..."
            let j = unpickleElem xpJid' b
            case j of
                Right jid' -> do
                    lift $ infoM "Pontarius.Xmpp" $ "Bound JID: " ++ show jid'
                    _ <- lift $ withStream ( do modify $ \s ->
                                                    s{streamJid = Just jid'})
                                           c
                    return jid'
                _ -> do
                    lift $ errorM "Pontarius.Xmpp"
                        $ "xmppBind: JID could not be unpickled from: "
                          ++ show b
                    throwError $ XmppOtherFailure
        _ -> do
            lift $ errorM "Pontarius.XMPP" "xmppBind: IQ error received."
            throwError XmppOtherFailure
  where
    -- Extracts the character data in the `jid' element.
    xpJid' :: PU [Node] Jid
    xpJid' = xpBind $ xpElemNodes jidName (xpContent xpJid)
    jidName = "{urn:ietf:params:xml:ns:xmpp-bind}jid"

-- A `bind' element pickler.
xpBind  :: PU [Node] b -> PU [Node] b
xpBind c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c

sessionXml :: Element
sessionXml = pickleElem
    (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session")
    ()

-- Sends the session IQ set element and waits for an answer. Throws an error if
-- if an IQ error stanza is returned from the server.
startSession :: Stream -> IO Bool
startSession con = do
    debugM "Pontarius.XMPP" "startSession: Pushing `session' IQ set stanza..."
    answer <- pushIQ "session" Nothing Set Nothing sessionXml con
    case answer of
        Left e -> do
            errorM "Pontarius.XMPP" $ "startSession: Error stanza received (" ++ (show e) ++ ")"
            return False
        Right _ -> do
            debugM "Pontarius.XMPP" "startSession: Result stanza received."
            return True
