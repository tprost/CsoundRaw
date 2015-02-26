import Foreign.Ptr

import Sound.Csound.Instantiation
import Sound.Csound.Attributes

main :: IO ()
main = do
    csound <- csoundCreate(nullPtr)
    ---------------------------------------------------------------------------
    -- Attributes
    ---------------------------------------------------------------------------
    sr <- csoundGetSr csound
    kr <- csoundGetKr csound
    ksmps <- csoundGetKsmps csound
    nchnls <- csoundGetNchnls csound
    dbfs <- csoundGet0dBFS csound
    timeSamples <- csoundGetCurrentTimeSamples csound
    sizeofmyflt <- csoundGetSizeOfMYFLT
    hostdata <- csoundGetHostData csound
    newhostdata <- csoundSetHostData csound hostdata
    putStrLn $ ("Sr: " ++ show sr)
    putStrLn $ ("Kr: " ++ show kr)
    putStrLn $ ("Ksmps: " ++ show ksmps)
    putStrLn $ ("Nchnls: " ++ show nchnls)
    putStrLn $ ("0dBFS: " ++ show dbfs)
    putStrLn $ ("Time Samples: " ++ show timeSamples)
    putStrLn $ ("Size of MYFLT: " ++ show sizeofmyflt)
    putStrLn $ ("Host Data: " ++ show hostdata)
    putStrLn $ ("New Host Data: " ++ show newhostdata)
    ---------------------------------------------------------------------------
    csoundDestroy csound
