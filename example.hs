-- | this example is a simple oil paint mixer

import Knife

main :: IO ()
main = do
    putStrLn "what hue would you like to mix: "
    line <- getLine
    let hue = read line :: Float
    getMixingInstructions hue
    main -- loop

getMixingInstructions :: Float -> IO ()
getMixingInstructions h = do
    print ans
        where ans = zip c d
              c = [degreesDiff h (hue col) | col <- (values stock)]
              d = keys stock
