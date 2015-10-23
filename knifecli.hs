import Knife
import System.Environment

-- main = print (map toRGB (cool . complementary $ getStock "wheat" ))
main = do
    argv <- getArgs
    putStrLn $ action argv $ interpret argv
    -- print (getArgs !! 0)

interpret :: [String] -> Palette
interpret [] = analogous $ getStock "burnt sienna"
-- interpret s = eval s

action :: [String] -> Palette -> String
action strings = toClojure


