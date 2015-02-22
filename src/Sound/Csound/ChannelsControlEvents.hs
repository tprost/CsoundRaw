{-# LANGUAGE ForeignFunctionInterface #-}
-------------------------------------------------------------------------------
-- |
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Sound.Csound.ChannelsControlEvents (
    --csoundGetChannelPtr,
    --csoundListChannels,
    --csoundDeleteChannelList,
    --csoundSetControlChannelHints,
    --csoundGetControlChannelHints,
    --csoundGetChannelLock,
    --csoundGetControlChannel,
    --csoundSetControlChannel,
    --csoundGetAudioChannel,
    --csoundSetAudioChannel,
    --csoundGetStringChannel,
    --csoundSetStringChannel,
    --csoundGetChannelDatasize,
    --csoundSetInputChannelCallback,
    --csoundSetOutputChannelCallback,
    --csoundSetPvsChannel,
    --csoundGetPvsChannel,
    --csoundScoreEvent,
    --csoundScoreEventAbsolute,
    --csoundInputMessage,
    --csoundKillInstance,
    --csoundRegisterSenseEventCallback,
    csoundKeyPress
    --csoundRegisterKeyboardCallback,
    --csoundRemoveKeyboardCallback
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundGetChannelPtr" csoundGetChannelPtr'
--foreign import ccall "csound.h csoundListChannels" csoundListChannels'
--foreign import ccall "csound.h csoundDeleteChannelList" csoundDeleteChannelList'
--foreign import ccall "csound.h csoundSetControlChannelHints" csoundSetControlChannelHints'
--foreign import ccall "csound.h csoundGetControlChannelHints" csoundGetControlChannelHints'
--foreign import ccall "csound.h csoundGetChannelLock" csoundGetChannelLock'
--foreign import ccall "csound.h csoundGetControlChannel" csoundGetControlChannel'
--foreign import ccall "csound.h csoundSetControlChannel" csoundSetControlChannel'
--foreign import ccall "csound.h csoundGetAudioChannel" csoundGetAudioChannel'
--foreign import ccall "csound.h csoundSetAudioChannel" csoundSetAudioChannel'
--foreign import ccall "csound.h csoundGetStringChannel" csoundGetStringChannel'
--foreign import ccall "csound.h csoundSetStringChannel" csoundSetStringChannel'
--foreign import ccall "csound.h csoundGetChannelDatasize" csoundGetChannelDatasize'
--foreign import ccall "csound.h csoundSetInputChannelCallback" csoundSetInputChannelCallback'
--foreign import ccall "csound.h csoundSetOutputChannelCallback" csoundSetOutputChannelCallback'
--foreign import ccall "csound.h csoundSetPvsChannel" csoundSetPvsChannel'
--foreign import ccall "csound.h csoundGetPvsChannel" csoundGetPvsChannel'
--foreign import ccall "csound.h csoundScoreEvent" csoundScoreEvent'
--foreign import ccall "csound.h csoundScoreEventAbsolute" csoundScoreEventAbsolute'
--foreign import ccall "csound.h csoundInputMessage" csoundInputMessage'
--foreign import ccall "csound.h csoundKillInstance" csoundKillInstance'
--foreign import ccall "csound.h csoundRegisterSenseEventCallback" csoundRegisterSenseEventCallback'
foreign import ccall "csound.h csoundKeyPress" csoundKeyPress' :: Ptr () -> CChar -> IO ()
--foreign import ccall "csound.h csoundRegisterKeyboardCallback" csoundRegisterKeyboardCallback'
--foreign import ccall "csound.h csoundRemoveKeyboardCallback" csoundRemoveKeyboardCallback'

--csoundGetChannelPtr
--csoundGetChannelPtr

--csoundListChannels
--csoundListChannels

--csoundDeleteChannelList
--csoundDeleteChannelList

--csoundSetControlChannelHints
--csoundSetControlChannelHints

--csoundGetControlChannelHints
--csoundGetControlChannelHints
   
--csoundGetChannelLock
--csoundGetChannelLock
   
--csoundGetControlChannel
--csoundGetControlChannel
   
--csoundSetControlChannel
--csoundSetControlChannel

--csoundGetAudioChannel
--csoundGetAudioChannel
   
--csoundSetAudioChannel
--csoundSetAudioChannel

--csoundGetStringChannel
--csoundGetStringChannel
   
--csoundSetStringChannel
--csoundSetStringChannel
   
--csoundGetChannelDatasize
--csoundGetChannelDatasize
   
--csoundSetInputChannelCallback
--csoundSetInputChannelCallback

--csoundSetOutputChannelCallback
--csoundSetOutputChannelCallback
   
--csoundSetPvsChannel
--csoundSetPvsChannel
   
--csoundGetPvsChannel
--csoundGetPvsChannel

--csoundScoreEvent
--csoundScoreEvent
  
--csoundScoreEventAbsolute
--csoundScoreEventAbsolute

--csoundInputMessage
--csoundInputMessage

--csoundKillInstance
--csoundKillInstance
   
--csoundRegisterSenseEventCallback
--csoundRegisterSenseEventCallback

csoundKeyPress :: MonadIO m => Ptr () -> CChar -> m ()
csoundKeyPress csnd c = liftIO (csoundKeyPress' csnd c)
   
--csoundRegisterKeyboardCallback
--csoundRegisterKeyboardCallback

--csoundRemoveKeyboardCallback
--csoundRemoveKeyboardCallback
