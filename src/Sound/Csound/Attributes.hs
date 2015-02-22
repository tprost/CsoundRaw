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
    --csoundGetSr,
    --csoundGetKr,
    --csoundGetKsmps,
    --csoundGetNchnls,
    --csoundGetNchnlsInput,
    --csoundGet0dBFS,
    --csoundGetCurrentTimeSamples,
    csoundGetSizeOfMYFLT,
    csoundGetHostData,
    csoundSetHostData
    --csoundSetOption,
    --csoundSetParams,
    --csoundGetParams,
    --csoundGetDebug,
    --csoundSetDebug
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.Ptr
import Foreign.C.Types

--foreign import ccall "csound.h csoundGetSr" csoundGetSr'
--foreign import ccall "csound.h csoundGetKr" csoundGetKr'
--foreign import ccall "csound.h csoundGetKsmps" csoundGetKsmps'
--foreign import ccall "csound.h csoundNchnls" csoundNchnls'
--foreign import ccall "csound.h csoundNchnlsInput" csoundNchnlsInput'
--foreign import ccall "csound.h csound0dBFS" csound0dBFS'
--foreign import ccall "csound.h csoundGetCurrentTimeSamples" csoundGetCurrentTimeSamples'
foreign import ccall "csound.h csoundGetSizeOfMYFLT" csoundGetSizeOfMYFLT' :: IO CInt
foreign import ccall "csound.h csoundGetHostData" csoundGetHostData' :: Ptr () -> IO (Ptr ())
foreign import ccall "csound.h csoundSetHostData" csoundSetHostData' :: Ptr () -> Ptr () -> IO (Ptr ())
--foreign import ccall "csound.h csoundSetOption" csoundSetOption'
--foreign import ccall "csound.h csoundSetParams" csoundSetParams'
--foreign import ccall "csound.h csoundGetParams" csoundGetParams'
--foreign import ccall "csound.h csoundGetDebug" csoundGetDebug'
--foreign import ccall "csound.h csoundSetDebug" csoundSetDebug'

--csoundGetSr
--csoundGetSr

--csoundGetKr
--csoundGetKr

--csoundGetKsmps
--csoundGetKsmps

--csoundGetNchnls
--csoundGetNchnls

--csoundGetNchnlsInput
--csoundGetNchnlsInput

--csoundGet0dBFS
--csoundGet0dBFS

--csoundGetCurrentTimeSamples
--csoundGetCurrentTimeSamples

csoundGetSizeOfMYFLT :: MonadIO m => m CInt
csoundGetSizeOfMYFLT = liftIO csoundGetSizeOfMYFLT'

csoundGetHostData :: MonadIO m => Ptr () -> m (Ptr ())
csoundGetHostData csoundptr = liftIO (csoundGetHostData' csoundptr)

csoundSetHostData :: MonadIO m => Ptr () -> Ptr () -> m (Ptr ())
csoundSetHostData csoundptr hostdata = liftIO (csoundSetHostData' csoundptr hostdata)

--csoundSetOption
--csoundSetOption

--csoundSetParams
--csoundSetParams

--csoundGetParams
--csoundGetParams

--csoundGetDebug
--csoundGetDebug

--csoundSetDebug
--csoundSetDebug
