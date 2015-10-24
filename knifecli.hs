import Knife
import System.Environment
import Data.List
import Data.Maybe

-- main = print (map toRGB (cool . complementary $ getStock "wheat" ))
main = do
    argv <- getArgs
    putStrLn (interpret argv)
    -- print (getArgs !! 0)

interpret :: [String] -> String
interpret args = format $ map colourmodel $ descriptors $ harmony $ colour 
    where colour 
               | any (`elem` arggroups) stockNames = (map snd stock) !! (head $ map fromJust $ filter (/=Nothing) $ map (`elemIndex` arggroups) stockNames)
               | otherwise = getStock "wheat"
               where arggroups = (map (\(a,b)->a++" "++b) $ zip args (tail args)) ++ (map (\(a,b,c)->a++" "++b++" "++c) $ zip3 args (tail args) (tail $ tail args))
                     stockNames = (map fst stock)
          harmony  
               | "complementary" `elem` args = complementary
               | otherwise = complementary
          descriptors = cool . brilliant
          colourmodel
               | "hsv" `elem` args = toHSV
               | "cmyk" `elem` args = toCMYK
               | "rgb" `elem` args = toRGB
               | otherwise = id
          format 
               | any (`elem` args) ["clj","clojure"] = toClojure
               | otherwise = show


