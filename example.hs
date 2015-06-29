import Knife

main :: IO ()
main = do
    putStrLn "this"
    line <- getLine
    convert line
    main

convert :: String -> IO ()
convert s = do
    print getRGB $ (HSV s)
