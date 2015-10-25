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
               | any (`elem` arggroups) stockNames = (map snd stock) !! (fromJust $ elemIndex (head $ filter (`elem` arggroups) stockNames) stockNames)
               | otherwise = getStock "wheat"
               where arggroups = args ++ (map (\(a,b)->a++" "++b) $ zip args (tail args)) ++ (map (\(a,b,c)->a++" "++b++" "++c) $ zip3 args (tail args) (tail $ tail args))
                     stockNames = (map fst stock)
          harmony  
               | "complementary" `elem` args && "split" `elem` args = splitComplementary
               | "complementary" `elem` args = complementary
               | "analogous" `elem` args = analogous
               | "triadic" `elem` args = triadic
               -- | "tetradic" `elem` args = tetradic -- this wont work except with two colour inputs
               | otherwise = complementary
          descriptors 
               | "bright" `elem` args = brilliant
               | "cool" `elem` args = cool
               | otherwise = id
          colourmodel
               | "hsv" `elem` args = toHSV
               | "cmyk" `elem` args = toCMYK
               | "rgb" `elem` args = toRGB
               | otherwise = id
          format 
               | any (`elem` args) ["clj","clojure"] = toClojure
               | any (`elem` args) ["sass","scss"] = toSASS
               | otherwise = show


