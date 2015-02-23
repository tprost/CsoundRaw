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
module Sound.Csound.RealtimeMidiIO (
    csoundSetMIDIModule
    --csoundSetHostImplementedMIDIIO,
    --csoundGetMIDIDevList,
    --csoundSetExternalMidiInOpenCallback,
    --csoundSetExternalMidiReadCallback,
    --csoundSetExternalMidiInCloseCallback,
    --csoundSetExternalMidiOutOpenCallback,
    --csoundSetExternalMidiWriteCallback,
    --csoundSetExtrenalMidiOutCloseCallback,
    --csoundSetExternalMidiErrorStringCallback,
    --csoundSetMIDIDeviceListCallback
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundSetMIDIModule" csoundSetMIDIModule' :: Ptr () -> Ptr CChar -> IO ()
--foreign import ccall "csound.h csoundSetHostImplementedMIDIIO" csoundSetHostImplementedMIDIIO'
--foreign import ccall "csound.h csoundGetMIDIDevList" csoundGetMIDIDevList'
--foreign import ccall "csound.h csoundSetExternalMidiInOpenCallback" csoundSetExternalMidiInOpenCallback'
--foreign import ccall "csound.h csoundSetExternalMidiReadCallback" csoundSetExternalMidiReadCallback'
--foreign import ccall "csound.h csoundSetExternalMidiInCloseCallback" csoundSetExternalMidiInCloseCallback'
--foreign import ccall "csound.h csoundSetExternalMidiOutOpenCallback" csoundSetExternalMidiOpenCallback'
--foreign import ccall "csound.h csoundSetExternalMidiWriteCallback" csoundSetExternalMidiWriteCallback'
--foreign import ccall "csound.h csoundSetExternalMidiOutCloseCallback" csoundSetExternalMidiOutCloseCallback'
--foreign import ccall "csound.h csoundSetExternalMidiErrorStringCallback" csoundSetExternalMidiErrorStringCallback'
--foreign import ccall "csound.h csoundSetExternalMidiDeviceListCallback" csoundSetExternalMidiDeviceListCallback'

csoundSetMIDIModule :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetMIDIModule csnd c_module = liftIO (csoundSetMIDIModule' csnd c_module)

--csoundSetHostImplementedMIDIIO
--csoundSetHostImplementedMIDIIO

--csoundGetMIDIDevList
--csoundGetMIDIDevList

--csoundSetExternalMidiInOpenCallback
--csoundSetExternalMidiInOpenCallback

--csoundSetExternalMidiReadCallback
--csoundSetExternalMidiReadCallback

--csoundSetExternalMidiInCloseCallback
--csoundSetExternalMidiInCloseCallback

--csoundSetExternalMidiOutOpenCallback
--csoundSetExternalMidiOutOpenCallback

--csoundSetExternalMidiWriteCallback
--csoundSetExternalMidiWriteCallback

--csoundSetExtrenalMidiOutCloseCallback
--csoundSetExtrenalMidiOutCloseCallback

--csoundSetExternalMidiErrorStringCallback
--csoundSetExternalMidiErrorStringCallback

--csoundSetMIDIDeviceListCallback
--csoundSetMIDIDeviceListCallback
