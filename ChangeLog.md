# 0.5.1 to 0.6.0
* Changed roster update callback to take RosterUpdate type
* Added onrosterPushL lens
* Removed legacy `crypto-random` (this caused issues when compiling with GHC 9.2.5)

# 0.5.0 to 0.5.1
* Fixed input logger choking on long non-ascii messages

# 0.4.5 to 0.5.0
* Support for the session element is now determined from stream features, the
  establishSession option was removed
* The parser can now handle nonzas. Nonzas can be handled with a plugin
* An initial roster can now be set with the initialRoster session configuration
  option
* The JID parser can now handle "/" and "@" characters in the resource part
