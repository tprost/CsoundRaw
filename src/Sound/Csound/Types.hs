-------------------------------------------------------------------------------
-- |
-- Copyright    : (c) 2015 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : portable
--
-------------------------------------------------------------------------------
module Sound.Csound.Types (
    MYFLT,
    CsoundFileOpenCallback
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

type MYFLT = CDouble

type CsoundFileOpenCallback = FunPtr (Ptr () -> CString -> CInt -> CInt -> CInt -> IO ())
