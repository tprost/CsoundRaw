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
    csoundGetSizeOfMYFLT,
    csoundGetHostData,
    csoundSetHostData
) where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundGetSizeOfMYFLT" csoundGetSizeOfMYFLT' :: IO CInt
foreign import ccall "csound.h csoundGetHostData" csoundGetHostData' :: Ptr () -> IO (Ptr ())
foreign import ccall "csound.h csoundSetHostData" csoundSetHostData' :: Ptr () -> Ptr () -> IO (Ptr ())

csoundGetSizeOfMYFLT :: MonadIO m => m CInt
csoundGetSizeOfMYFLT = liftIO csoundGetSizeOfMYFLT'

csoundGetHostData :: MonadIO m => Ptr () -> m (Ptr ())
csoundGetHostData csoundptr = liftIO (csoundGetHostData' csoundptr)

csoundSetHostData :: MonadIO m => Ptr () -> Ptr () -> m (Ptr ())
csoundSetHostData csoundptr hostdata = liftIO (csoundSetHostData' csoundptr hostdata)
