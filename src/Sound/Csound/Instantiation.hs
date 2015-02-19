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
    csoundGetAPIVersion,
    csoundGetVersion
) where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.C.Types

foreign import ccall "csound.h csoundGetAPIVersion" csoundGetAPIVersion' :: IO CInt
foreign import ccall "csound.h csoundGetVersion" csoundGetVersion' :: IO CInt

csoundGetAPIVersion :: MonadIO m => m CInt
csoundGetAPIVersion = liftIO csoundGetAPIVersion'

csoundGetVersion :: MonadIO m => m CInt
csoundGetVersion = liftIO csoundGetVersion'
