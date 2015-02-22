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
module Sound.Csound.Attributes (
    csoundGetSr,
    csoundGetKr,
    csoundGetKsmps,
    csoundGetNchnls,
    csoundGetNchnlsInput,
    csoundGet0dBFS,
    csoundGetCurrentTimeSamples,
    csoundGetSizeOfMYFLT,
    csoundGetHostData,
    csoundSetHostData,
    csoundSetOption,
    csoundSetParams,
    csoundGetParams,
    csoundGetDebug,
    csoundSetDebug
) where

-- | External Imports
import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

-- | Internal Imports
import Sound.Csound.Types

foreign import ccall "csound.h csoundGetSr" csoundGetSr' :: Ptr () -> IO MYFLT
foreign import ccall "csound.h csoundGetKr" csoundGetKr' :: Ptr () -> IO MYFLT
foreign import ccall "csound.h csoundGetKsmps" csoundGetKsmps' :: Ptr () -> IO CUInt
foreign import ccall "csound.h csoundGetNchnls" csoundGetNchnls' :: Ptr () -> IO CUInt
foreign import ccall "csound.h csoundGetNchnlsInput" csoundGetNchnlsInput' :: Ptr () -> IO CUInt
foreign import ccall "csound.h csoundGet0dBFS" csoundGet0dBFS' :: Ptr () -> IO MYFLT
foreign import ccall "csound.h csoundGetCurrentTimeSamples" csoundGetCurrentTimeSamples' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundGetSizeOfMYFLT" csoundGetSizeOfMYFLT' :: IO CInt
foreign import ccall "csound.h csoundGetHostData" csoundGetHostData' :: Ptr () -> IO (Ptr ())
foreign import ccall "csound.h csoundSetHostData" csoundSetHostData' :: Ptr () -> Ptr () -> IO (Ptr ())
foreign import ccall "csound.h csoundSetOption" csoundSetOption' :: Ptr () -> Ptr CChar -> IO ()
foreign import ccall "csound.h csoundSetParams" csoundSetParams' :: Ptr () -> Ptr () -> IO ()
foreign import ccall "csound.h csoundGetParams" csoundGetParams' :: Ptr () -> Ptr () -> IO ()
foreign import ccall "csound.h csoundGetDebug" csoundGetDebug' :: Ptr () -> IO CInt
foreign import ccall "csound.h csoundSetDebug" csoundSetDebug' :: Ptr () -> CInt -> IO ()

csoundGetSr :: MonadIO m => Ptr () -> m MYFLT
csoundGetSr csnd = liftIO (csoundGetSr' csnd)

csoundGetKr :: MonadIO m => Ptr () -> m MYFLT
csoundGetKr csnd = liftIO (csoundGetSr' csnd)

csoundGetKsmps :: MonadIO m => Ptr () -> m CUInt
csoundGetKsmps csnd = liftIO (csoundGetKsmps' csnd)

csoundGetNchnls :: MonadIO m => Ptr () -> m CUInt
csoundGetNchnls csnd = liftIO (csoundGetNchnls' csnd)

csoundGetNchnlsInput :: MonadIO m => Ptr () -> m CUInt
csoundGetNchnlsInput csnd = liftIO (csoundGetNchnlsInput' csnd)

csoundGet0dBFS :: MonadIO m => Ptr () -> m MYFLT
csoundGet0dBFS csnd = liftIO (csoundGet0dBFS' csnd)

csoundGetCurrentTimeSamples :: MonadIO m => Ptr () -> m CInt
csoundGetCurrentTimeSamples csnd = liftIO (csoundGetCurrentTimeSamples' csnd)

csoundGetSizeOfMYFLT :: MonadIO m => m CInt
csoundGetSizeOfMYFLT = liftIO csoundGetSizeOfMYFLT'

csoundGetHostData :: MonadIO m => Ptr () -> m (Ptr ())
csoundGetHostData csnd = liftIO (csoundGetHostData' csnd)

csoundSetHostData :: MonadIO m => Ptr () -> Ptr () -> m (Ptr ())
csoundSetHostData csnd hostdata = liftIO (csoundSetHostData' csnd hostdata)

csoundSetOption :: MonadIO m => Ptr () -> Ptr CChar -> m ()
csoundSetOption csnd option = liftIO (csoundSetOption' csnd option)

csoundSetParams :: MonadIO m => Ptr () -> Ptr () -> m ()
csoundSetParams csnd p = liftIO (csoundSetParams' csnd p)

csoundGetParams :: MonadIO m => Ptr () -> Ptr () -> m ()
csoundGetParams csnd p = liftIO (csoundSetParams' csnd p)

csoundGetDebug :: MonadIO m => Ptr () -> m CInt
csoundGetDebug csnd = liftIO (csoundGetDebug' csnd)

csoundSetDebug :: MonadIO m => Ptr () -> CInt -> m ()
csoundSetDebug csnd debug = liftIO (csoundSetDebug' csnd debug)
