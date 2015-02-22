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
module Sound.Csound.RealtimeAudioIO (
    --csoundSetRTAudioModule,
    --csoundGetModule,
    csoundGetInputBufferSize,
    csoundGetOutputBufferSize
    --csoundGetInputBuffer,
    --csoundGetOutputBuffer,
    --csoundGetSpin,
    --csoundAddSpinSample,
    --csoundGetSpout,
    --csoundGetSpoutSample,
    --csoundGetRtRecordUserData,
    --csoundGetRtPlayUserData,
    --csoundSetHostImplementedAudioIO,
    --csoundGetAudioDevList,
    --csoundSetPlayOpenCallback,
    --csoundSetRtPlayCallback,
    --csoundSetRecOpenCallback,
    --csoundSetRtRecordCallback,
    --csoundSetRtCloseCallback,
    --csoundSetAudioDeviceListCallback
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundSetRTAudioModule" csoundSetRTAudioModule'
--foreign import ccall "csound.h csoundGetModule" csoundGetModule'
foreign import ccall "csound.h csoundGetInputBufferSize" csoundGetInputBufferSize' :: Ptr () -> IO CLong
foreign import ccall "csound.h csoundGetOutputBufferSize" csoundGetOutputBufferSize' :: Ptr () -> IO CLong
--foreign import ccall "csound.h csoundGetInputBuffer" csoundGetInputBuffer'
--foreign import ccall "csound.h csoundGetOutputBuffer" csoundGetOutput'
--foreign import ccall "csound.h csoundGetSpin" csoundGetSpin'
--foreign import ccall "csound.h csoundAddSpinSample" csoundAddSpinSample'
--foreign import ccall "csound.h csoundGetSpout" csoundGetSpout'
--foreign import ccall "csound.h csoundGetSpoutSample" csoundGetSpoutSample'
--foreign import ccall "csound.h csoundGetRtRecordUserData" csoundGetRtRecordUserData'
--foreign import ccall "csound.h csoundGetRtPlayUserData" csoundGetRtPlayUserData'
--foreign import ccall "csound.h csoundSetHostImplementedAudioIO" csoundSetHostImplementedAudioIO'
--foreign import ccall "csound.h csoundGetAudioDevList" csoundGetAudioDevList'
--foreign import ccall "csound.h csoundSetPlayopenCallback" csoundSetPlayopenCallback'
--foreign import ccall "csound.h csoundSetRtplayCallback" csoundSetRtplayCallback'
--foreign import ccall "csound.h csoundSetRecopenCallback" csoundSetrecopenCallback'
--foreign import ccall "csound.h csoundSetRtrecordCallback" csoundSetRtrecordCallback'
--foreign import ccall "csound.h csoundSetRtcloseCallback" csoundSetRtcloseCallback'
--foreign import ccall "csound.h csoundSetAudioDeviceListCallback" csoundSetAudioDeviceListCallback'

--csoundSetRTAudioModule
--csoundSetRTAudioModule

--csoundGetModule
--csoundGetModule

csoundGetInputBufferSize :: MonadIO m => Ptr () -> m CLong
csoundGetInputBufferSize csnd = liftIO (csoundGetInputBufferSize' csnd)

csoundGetOutputBufferSize :: MonadIO m => Ptr () -> m CLong
csoundGetOutputBufferSize csnd = liftIO (csoundGetOutputBufferSize' csnd)

--csoundGetInputBuffer
--csoundGetInputBuffer

--csoundGetOutputBuffer
--csoundGetOutputBuffer

--csoundGetSpin
--csoundGetSpin

--csoundAddSpinSample
--csoundAddSpinSample

--csoundGetSpout
--csoundGetSpout

--csoundGetSpoutSample
--csoundGetSpoutSample

--csoundGetRtRecordUserData
--csoundGetRtRecordUserData

--csoundGetRtPlayUserData
--csoundGetRtPlayUserData

--csoundSetHostImplementedAudioIO
--csoundSetHostImplementedAudioIO

--csoundGetAudioDevList
--csoundGetAudioDevList

--csoundSetPlayOpenCallback
--csoundSetPlayOpenCallback

--csoundSetRtPlayCallback
--csoundSetRtPlayCallback

--csoundSetRecOpenCallback
--csoundSetRecOpenCallback

--csoundSetRtRecordCallback
--csoundSetRtRecordCallback

--csoundSetRtCloseCallback
--csoundSetRtCloseCallback

--csoundSetAudioDeviceListCallback
--csoundSetAudioDeviceListCallback
