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
module Sound.Csound.GeneralIO (
    --csoundGetOutputName,
    csoundSetOutput,
    csoundSetInput
    --csoundSetMIDIInput,
    --csoundSetMIDIFileInput,
    --csoundSetMIDIOutput,
    --csoundSetMIDIFileOutput,
    --csoundSetFileOpenCallback
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundGetOutputName" csoundGetOutputName'
foreign import ccall "csound.h csoundSetOutput" csoundSetOutput' :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetInput" csoundSetInput' :: Ptr () -> Ptr CChar -> IO ()
--foreign import ccall "csound.h csoundSetMIDIInput" csoundSetMIDIInput'
--foreign import ccall "csound.h csoundSetMIDIFileInput" csoundSetMIDIFileInput'
--foreign import ccall "csound.h csoundSetMIDIOutput" csoundSetMIDIOutput'
--foreign import ccall "csound.h csoundSetMIDIFileOutput" csoundSetMIDIFileOutput'
--foreign import ccall "csound.h csoundSetFileOpenCallback" csoundSetFileOpenCallback'

--csoundGetOutputName
--csoundGetOutputName

csoundSetOutput :: MonadIO m => Ptr () -> Ptr CChar -> Ptr CChar -> Ptr CChar -> m ()
csoundSetOutput cs_ptr c_name c_type c_format = liftIO (csoundSetOutput' cs_ptr c_name c_type c_format)

csoundSetInput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetInput csoundptr name = liftIO (csoundSetInput' csoundptr name)

--csoundSetMIDIInput
--csoundSetMIDIInput

--csoundSetMIDIFileInput
--csoundSetMIDIFileInput

--csoundSetMIDIOutput
--csoundSetMIDIOutput

--csoundSetMIDIFileOutput
--csoundSetMIDIFileOutput

--csoundSetFileOpenCallback
--csoundSetFileOpenCallback
