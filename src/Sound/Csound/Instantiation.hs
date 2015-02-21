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
module Sound.Csound.Instantiation (
    csoundInitialize,
    csoundCreate,
    csoundDestroy,
    csoundGetAPIVersion,
    csoundGetVersion
) where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundInitialize" csoundInitialize' :: CInt -> IO CInt
foreign import ccall "csound.h csoundCreate" csoundCreate' :: Ptr () -> IO (Ptr ())
foreign import ccall "csound.h csoundDestroy" csoundDestroy' :: Ptr () -> IO ()
foreign import ccall "csound.h csoundGetAPIVersion" csoundGetAPIVersion' :: IO CInt
foreign import ccall "csound.h csoundGetVersion" csoundGetVersion' :: IO CInt

csoundInitialize :: MonadIO m => CInt -> m CInt
csoundInitialize flag = liftIO (csoundInitialize' flag)

csoundCreate :: MonadIO m => Ptr () -> m (Ptr ())
csoundCreate hostdata = liftIO (csoundCreate' hostdata)

csoundDestroy :: MonadIO m => Ptr () -> m ()
csoundDestroy csoundptr = liftIO (csoundDestroy' csoundptr)

csoundGetAPIVersion :: MonadIO m => m CInt
csoundGetAPIVersion = liftIO csoundGetAPIVersion'

csoundGetVersion :: MonadIO m => m CInt
csoundGetVersion = liftIO csoundGetVersion'
