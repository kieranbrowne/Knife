module Knife
( Colour(..)
, toRGB , toHSV , toCMYK
, getRGB , getHSV , getCMYK
, red , green , blue
, hue , saturation , value
, monochromatic , analogous , complementary
, splitComplementary, triadic, tetradic
, decimal, rotate, degreesDiff
, stock, getStock, find, keys, values
) where

import Data.Fixed
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

data Colour = RGB Int Int Int | HSV Int Int Int | CMYK Int Int Int Int deriving (Show)

-- | Colour Conversions
toRGB :: Colour -> Colour
toRGB (RGB r g b) = RGB r g b
toRGB (HSV h s v) 
    | h >= 0   && h < 60  = RGB a' b' c'
    | h >= 60  && h < 120 = RGB b' a' c' 
    | h >= 120 && h < 180 = RGB c' a' b' 
    | h >= 180 && h < 240 = RGB c' b' a' 
    | h >= 240 && h < 300 = RGB b' c' a'
    | h >= 300 && h < 360 = RGB a' c' b'
    where col = HSV h s v
          c = decimal col!!1 * decimal col!!2
          x = c * (1-abs(((toFloat(h) /60) `mod'` 2)-1)) :: Float
          m = decimal col!!2 - c
          a' = toInt ((c+m)*255)
          b' = toInt ((x+m)*255)
          c' = toInt (m*255)
toRGB (CMYK c m y k) = 
    let col = (CMYK c m y k)
        r = toInt (255 * (1-decimal col!!0) * (1-decimal col!!3))
        g = toInt (255 * (1-decimal col!!1) * (1-decimal col!!3))
        b = toInt (255 * (1-decimal col!!1) * (1-decimal col!!3))
    in RGB r g b

toHSV :: Colour -> Colour
toHSV (RGB r g b) = 
    let col = RGB r g b
        r' = decimal col !! 0
        g' = decimal col !! 1
        b' = decimal col !! 2
        cmax = maximum (decimal col)
        cmin = minimum (decimal col)
        δ = cmax - cmin
        h   | δ == 0  = 0 :: Int
            | cmax == r' = toInt (60 * (((g'-b')/δ) `mod'` 6)) 
            | cmax == g' = toInt (60 * (((b'-r')/δ) + 2))
            | cmax == b' = toInt (60 * (((r'-g')/δ) + 4))
        s   | cmax == 0 = 0 :: Int
            | cmax /= 0 = toInt ((δ / cmax)*100) 
        v = toInt (cmax*100) 
    in HSV h s v
toHSV (HSV h s v) = HSV h s v
toHSV (CMYK c m y k) = toHSV ( toRGB (CMYK c m y k) ) 

toCMYK :: Colour -> Colour
toCMYK (RGB r g b) = 
    let col = RGB r g b
        rgb = decimal col
        k = 1-(maximum rgb)
        c = toInt (((1-rgb!!0 -k)/(1-k))*100)
        m = toInt (((1-rgb!!1 -k)/(1-k))*100)
        y = toInt (((1-rgb!!2 -k)/(1-k))*100)
        k' = toInt (k * 100)
    in CMYK c m y k'
toCMYK (HSV h s v) = toCMYK ( toRGB ( HSV h s v ) )
toCMYK (CMYK c m y k) = CMYK c m y k

-- | get values
getRGB :: Colour -> (Int,Int,Int)
getRGB col = (r,g,b)
    where (RGB r g b) = toRGB(col)

getHSV :: Colour -> (Int,Int,Int)
getHSV col = (h,s,v)
    where (HSV h s v) = toHSV(col)

getCMYK :: Colour -> (Int,Int,Int,Int)
getCMYK col = (c,m,y,k)
    where (CMYK c m y k) = toCMYK(col)

red :: Colour -> Int
red col = r
    where (r,_,_) = getRGB col

green :: Colour -> Int
green col = g
    where (_,g,_) = getRGB col

blue :: Colour -> Int
blue col = b
    where (_,_,b) = getRGB col

hue :: Colour -> Int
hue col = h
    where (h,_,_) = getHSV col

saturation :: Colour -> Int
saturation col = s
    where (_,s,_) = getHSV col

value :: Colour -> Int
value col = v
    where (_,_,v) = getHSV col

cyan :: Colour -> Int
cyan col = c
    where (c,_,_,_) = getCMYK col
magenta :: Colour -> Int
magenta col = m
    where (_,m,_,_) = getCMYK col
yellow :: Colour -> Int
yellow col = y
    where (_,_,y,_) = getCMYK col
key :: Colour -> Int
key col = k
    where (_,_,_,k) = getCMYK col
black = key -- | synonym

-- | utils

decimal :: Colour -> [Float]
decimal (RGB r g b) = [toFloat(x)/255 | x <- [r,g,b]]
decimal (HSV h s v) = [toFloat(h)/360,toFloat(s)/100,toFloat(v)/100]
decimal (CMYK c m y k) = [toFloat(x)/100 | x <- [c,m,y,k]]

rotate :: Int -> Int -> Int
rotate initial diff = newDegrees
    where newDegrees = (initial + diff) `mod` 360

degreesDiff :: Float -> Float -> Float
degreesDiff a b = minimum d
    where d = map abs ops
          ops = [a-b, a-b-360, a-b+360]

toInt :: RealFrac a => a -> Int
toInt x = fromIntegral ( round x ) :: Int

toFloat :: Integral a => a -> Float
toFloat x = fromIntegral x :: Float

-- | Palettes

type Palette = [Colour]

-- | Palettes from colour harmonies
monochromatic :: Colour -> Palette
monochromatic col = colours
    where colours = [col,col]

analogous :: Colour -> Palette
analogous col = colours
    where (h,s,v) = getHSV col
          left = HSV (rotate h (-30)) s v 
          right = HSV (rotate h 30) s v 
          colours = [left, col, right]

complementary :: Colour -> Palette
complementary col = colours
    where (h,s,v) = getHSV col
          comp = HSV (rotate (hue col) 180) s v
          colours = [col,comp]

splitComplementary :: Colour -> Palette
splitComplementary col = colours
    where colours = []

triadic :: Colour -> Palette
triadic col = colours
    where (h,s,v) = getHSV col
          col1  = HSV (rotate (hue col) 120) s v
          col2  = HSV (rotate (hue col) 240) s v
          colours = [col,col1,col2]

tetradic :: Colour -> Colour -> Palette
tetradic col1 col2 = colours
    where colours = complementary col1 ++ complementary col2

-- | mod palettes
-- moody :: Palette -> Palette
--moody = []



-- | Stock Colours
stock = 
    [("azure"                 , HSV 0 0 0)
    ,("burgundy"              , HSV 0 0 0 )
    ,("burnt sienna"          , HSV 14 65 91 )
    ,("burnt umber"           , HSV 9  74 54 )
    ,("cadmium green"         , HSV 0 0 0 )
    ,("cadmium green pale"    , HSV  0 0 0 )
    ,("cadmium orange"        , HSV  0 0 0 )
    ,("cadmium red"           , HSV  0 0 0 )
    ,("cadmium scarlet"       , HSV  0 0 0 )
    ,("cadmium yellow medium" , HSV  0 0 0 )
    ,("cadmium yellow pale"   , HSV  0 0 0 )
    ,("canary yellow"         , HSV  0 0 0 )
    ,("dioxazine violet"      , HSV  0 0 0 )
    ,("mars black"            , HSV  0 0 0 )
    ,("quindcridone rose"     , HSV  0 0 0 )
    ,("titanium white"        , HSV  0 0 0 )
    ,("thalo blue"            , HSV  0 0 0 )
    ,("thalo green"           , HSV  0 0 0 )
    ,("ultramarine blue"      , HSV  0 0 0 )
    ,("venetian red"          , HSV  0 0 0 )
    ,("wheat"                 , HSV  0 0 0 )
    ,("white"                 , HSV  0 0 0 )
    ,("white smoke"           , HSV  0 0 0 )
    ,("wine"                  , HSV  0 0 0 )
    ,("yale blue"             , HSV  0 0 0 )
    ]

getStock :: String -> Colour
getStock name 
        | Map.member name (Map.fromList stock) = find name stock
        | otherwise = RGB 0 0 0

keys map = [k | (k,v) <- map]
values map = [v | (k,v) <- map]

find :: (Eq k) => k -> [(k,v)] -> v
find key xs = snd . head . filter (\(k,v) -> key == k) $ xs
