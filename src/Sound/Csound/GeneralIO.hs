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
    csoundGetOutputName,
    csoundSetOutput,
    csoundSetInput,
    csoundSetMIDIInput,
    csoundSetMIDIFileInput,
    csoundSetMIDIOutput,
    csoundSetMIDIFileOutput,
    csoundSetFileOpenCallback
) where

-- | External Imports
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

-- | Internal Imports
import Sound.Csound.Types

foreign import ccall "csound.h csoundGetOutputName" csoundGetOutputName' :: Ptr () -> IO (Ptr CChar)
foreign import ccall "csound.h csoundSetOutput" csoundSetOutput' :: Ptr () -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetInput" csoundSetInput' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetMIDIInput" csoundSetMIDIInput' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetMIDIFileInput" csoundSetMIDIFileInput' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetMIDIOutput" csoundSetMIDIOutput' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetMIDIFileOutput" csoundSetMIDIFileOutput' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetFileOpenCallback" csoundSetFileOpenCallback' :: Ptr () -> CsoundFileOpenCallback -> IO ()

csoundGetOutputName :: MonadIO m => Ptr () -> m (Ptr CChar)
csoundGetOutputName csnd = liftIO (csoundGetOutputName' csnd)

csoundSetOutput :: MonadIO m => Ptr () -> Ptr CChar -> Ptr CChar -> Ptr CChar -> m ()
csoundSetOutput cs_ptr c_name c_type c_format = liftIO (csoundSetOutput' cs_ptr c_name c_type c_format)

csoundSetInput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetInput csnd name = liftIO (csoundSetInput' csnd name)

csoundSetMIDIInput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetMIDIInput csnd name = liftIO (csoundSetMIDIInput' csnd name)

csoundSetMIDIFileInput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetMIDIFileInput csnd name = liftIO (csoundSetMIDIFileInput' csnd name)

csoundSetMIDIOutput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetMIDIOutput csnd name = liftIO (csoundSetMIDIOutput' csnd name)

csoundSetMIDIFileOutput :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetMIDIFileOutput csnd name = liftIO (csoundSetMIDIFileOutput' csnd name)

csoundSetFileOpenCallback :: MonadIO m => Ptr () -> CsoundFileOpenCallback -> m ()
csoundSetFileOpenCallback csnd funPtr = liftIO (csoundSetFileOpenCallback' csnd funPtr)
