{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

import Control.Applicative((<$>))

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.ByteString as BS
import Data.Text(Text)

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Hexpat as HXC
import Data.Conduit.List as CL
import Data.Conduit.Text as CT

import qualified Data.Text as Text

import Network.XMPP.Types
import Network.XMPP.Marshal
import Network.XMPP.Pickle

import System.IO

import Text.XML.Expat.SAX
import Text.XML.Expat.Tree
import Text.XML.Expat.Format

parseOpts = ParseOptions (Just UTF8) Nothing

pushN :: Element -> XMPPMonad ()
pushN x = do
  sink <- gets sConPush
  liftIO . sink $ formatNode' x

push :: Stanza -> XMPPMonad ()
push = pushN . pickleElem stanzaP

pushOpen :: Element -> XMPPMonad ()
pushOpen (Element name attrs children) = do
  sink <- gets sConPush
  let sax = StartElement name attrs
  liftIO . sink $ formatSAX' [sax]
  forM children pushN
  return ()


pulls :: Sink Event IO a -> XMPPMonad a
pulls snk = do
  source <- gets sConSrc
  lift $ source $$ snk

pullE :: XMPPMonad Element
pullE = do
  pulls elementFromEvents

pullPickle p = unpickleElem p <$> pullE

pull :: XMPPMonad Stanza
pull = pullPickle stanzaP

-- pull :: XMPPMonad Stanza
-- pull = elementToStanza <$> pullE

xmppFromHandle
  :: Handle -> Text -> Text -> Maybe Text
     -> XMPPMonad a
     -> IO (a, XMPPState)
xmppFromHandle handle hostname username resource f = runResourceT $ do
  liftIO $ hSetBuffering handle NoBuffering
  raw <- bufferSource $ CB.sourceHandle handle
  src <- bufferSource $ raw $= HXC.parseBS parseOpts
  let st = XMPPState
             src
             raw
             (liftIO . BS.hPut handle)
             (Just handle)
             def
             False
             hostname
             username
             resource
  runStateT f st
