module Knife
( Colour(..)
, toRGB , toHSV , toCMYK
, getRGB , getHSV , getCMYK
, red , green , blue
, hue , saturation , value
, monochromatic , analogous , complementary
, splitComplementary, triadic, tetradic
, decimal, rotate, degreesDiff
, stock, find, keys, values
) where

import Data.Fixed
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

data Colour = RGB Float Float Float | HSV Float Float Float | CMYK Float Float Float Float deriving (Show)

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
          x = c * (1-abs(((h /60) `mod'` 2)-1)) :: Float
          m = decimal col!!2 - c
          a' = (c+m)*255
          b' = (x+m)*255
          c' = m*255
toRGB (CMYK c m y k) = 
    let col = (CMYK c m y k)
        r = 255 * (1-decimal col!!0) * (1-decimal col!!3)
        g = 255 * (1-decimal col!!1) * (1-decimal col!!3)
        b = 255 * (1-decimal col!!1) * (1-decimal col!!3)
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
        h   | δ == 0  = 0
            | cmax == r' = 60 * (((g'-b')/δ) `mod'` 6) 
            | cmax == g' = 60 * (((b'-r')/δ) + 2) 
            | cmax == b' = 60 * (((r'-g')/δ) + 4) 
        s   | cmax == 0 = 0
            | cmax /= 0 = (δ / cmax)*100
        v = cmax*100
    in HSV h s v
toHSV (HSV h s v) = HSV h s v
toHSV (CMYK c m y k) = toHSV ( toRGB (CMYK c m y k) ) 

toCMYK :: Colour -> Colour
toCMYK (RGB r g b) = 
    let col = RGB r g b
        rgb = decimal col
        k = 1-(maximum rgb)
        c = ((1-rgb!!0 -k)/(1-k))*100
        m = ((1-rgb!!1 -k)/(1-k))*100
        y = ((1-rgb!!2 -k)/(1-k))*100
        k' = k * 100
    in CMYK c m y k'
toCMYK (HSV h s v) = toCMYK ( toRGB ( HSV h s v ) )
toCMYK (CMYK c m y k) = CMYK c m y k

-- | get values
getRGB :: Colour -> (Float,Float,Float)
getRGB col = (r,g,b)
    where (RGB r g b) = toRGB(col)

getHSV :: Colour -> (Float,Float,Float)
getHSV col = (h,s,v)
    where (HSV h s v) = toHSV(col)

getCMYK :: Colour -> (Float,Float,Float,Float)
getCMYK col = (c,m,y,k)
    where (CMYK c m y k) = toCMYK(col)

red :: Colour -> Float
red col = r
    where (r,_,_) = getRGB col

green :: Colour -> Float
green col = g
    where (_,g,_) = getRGB col

blue :: Colour -> Float
blue col = b
    where (_,_,b) = getRGB col

hue :: Colour -> Float
hue col = h
    where (h,_,_) = getHSV col

saturation :: Colour -> Float
saturation col = s
    where (_,s,_) = getHSV col

value :: Colour -> Float
value col = v
    where (_,_,v) = getHSV col

cyan :: Colour -> Float
cyan col = c
    where (c,_,_,_) = getCMYK col
magenta :: Colour -> Float
magenta col = m
    where (_,m,_,_) = getCMYK col
yellow :: Colour -> Float
yellow col = y
    where (_,_,y,_) = getCMYK col
key :: Colour -> Float
key col = k
    where (_,_,_,k) = getCMYK col
black = key -- | synonym

-- | utils

decimal :: Colour -> [Float]
decimal (RGB r g b) = [x/255 | x <- [r,g,b]]
decimal (HSV h s v) = [h/360,s/100,v/100]
decimal (CMYK c m y k) = [x/100 | x <- [c,m,y,k]]

rotate :: Float -> Float -> Float
rotate initial diff = newDegrees
    where newDegrees = (initial + diff) `mod'` 360.0

degreesDiff :: Float -> Float -> Float
degreesDiff a b = minimum d
    where d = map abs ops
          ops = [a-b, a-b-360, a-b+360]

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
    [("burnt sienna",HSV 13.8 65.2 91.4)
    ,("burnt umber",HSV 8.8 73.9 54.1)]

getStock :: String -> Colour
getStock name 
        | Map.member name (Map.fromList stock) = find name stock
        | otherwise = RGB 0 0 0

keys map = [k | (k,v) <- map]
values map = [v | (k,v) <- map]

find :: (Eq k) => k -> [(k,v)] -> v
find key xs = snd . head . filter (\(k,v) -> key == k) $ xs
