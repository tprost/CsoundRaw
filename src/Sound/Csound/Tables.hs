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
    --csoundTableGet,
    --csoundTableSet,
    --csoundTableCopyOut,
    --csoundTableCopyIn,
    --csoundGetTable
) where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "csound.h csoundTableLength" csoundTableLength' :: Ptr () -> CInt -> IO CInt
--foreign import ccall "csound.h csoundTableGet" csoundTableGet'
--foreign import ccall "csound.h csoundTableSet" csoundTableSet'
--foreign import ccall "csound.h csoundTableCopyOut" csoundTableCopyOut'
--foreign import ccall "csound.h csoundTableCopyIn" csoundTableCopyIn'
--foreign import ccall "csound.h csoundGetTable" csoundGetTable'

csoundTableLength :: MonadIO m => Ptr () -> CInt -> m CInt
csoundTableLength csoundptr table = liftIO (csoundTableLength' csoundptr table)

--csoundTableGet
--csoundTableGet

--csoundTableSet
--csoundTableSet

--csoundTableCopyOut
--csoundTableCopyOut

--csoundTableCopyIn
--csoundTableCopyIn

--csoundGetTable
--csoundGetTable
