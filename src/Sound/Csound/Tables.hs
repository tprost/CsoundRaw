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
module Sound.Csound.Tables (
    csoundTableLength
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.C.Types

foreign import ccall "csound.h csoundTableLength" csoundTableLength' :: Ptr () -> CInt -> IO CInt

csoundTableLength :: MonadIO m => Ptr () -> CInt -> m CInt
csoundTableLength csoundptr table = liftIO (csoundTableLength' csoundptr table)
