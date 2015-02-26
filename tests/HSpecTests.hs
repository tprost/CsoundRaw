
-- | External Imports
import Foreign.Ptr

-- | Internal Imports
import Sound.Csound.Instantiation

main :: IO ()
main = do
    csound <- csoundCreate(nullPtr)
    putStrLn "This should show."
    csoundDestroy csound
