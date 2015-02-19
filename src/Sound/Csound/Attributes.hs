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
    csoundGetSizeOfMYFLT
) where

import Control.Monad.IO.Class
import Foreign.C
import Foreign.C.Types

foreign import ccall "csound.h csoundGetSizeOfMYFLT" csoundGetSizeOfMYFLT' :: IO CInt

csoundGetSizeOfMYFLT :: MonadIO m => m CInt
csoundGetSizeOfMYFLT = liftIO csoundGetSizeOfMYFLT'
