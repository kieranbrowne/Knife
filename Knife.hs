module Knife
( Colour(..)
, getRGB
, getHSV
, getCMYK
, getHue
, decimal
) where

import Data.Fixed

data Colour = RGB Float Float Float | HSV Float Float Float | CMYK Float Float Float Float deriving (Show)

-- | Colour Conversions

getRGB :: Colour -> (Float,Float,Float)
getRGB (RGB r g b) = (r,g,b)
getRGB (HSV h s v) 
    | h >= 0   && h < 60  = ((c+m)*255,(x+m)*255,m*255) 
    | h >= 60  && h < 120 = ((x+m)*255,(c+m)*255,m*255) 
    | h >= 120 && h < 180 = (m*255,(c+m)*255,(x+m)*255) 
    | h >= 180 && h < 240 = (m*255,(x+m)*255,(c+m)*255) 
    | h >= 240 && h < 300 = ((x+m)*255,m*255,(c+m)*255) 
    | h >= 300 && h < 360 = ((c+m)*255,m*255,(x+m)*255) 
    where col = HSV h s v
          c = decimal col!!1 * decimal col!!2
          x = c * (1-abs(((h /60) `mod'` 2)-1)) :: Float
          m = decimal col!!2 - c
getRGB (CMYK c m y k) = 
    let col = (CMYK c m y k)
        r = 255 * (1-decimal col!!0) * (1-decimal col!!3)
        g = 255 * (1-decimal col!!1) * (1-decimal col!!3)
        b = 255 * (1-decimal col!!1) * (1-decimal col!!3)
    in (r,g,b)

getHSV :: Colour -> (Float,Float,Float)
--getHSV (RGB r g b) = (r,g,b)
getHSV (HSV h s v) = (h,s,v)
--getHSV (CMYK c m y k) = (c,m,y)

getCMYK :: Colour -> (Float,Float,Float,Float)
getCMYK (RGB r g b) = 
    let rgb = decimal (RGB r g b)
        k = 1-(maximum rgb)
        c = ((1-rgb!!0)/(1-k))*100
        m = ((1-rgb!!1)/(1-k))*100
        y = ((1-rgb!!2)/(1-k))*100
    in (c,m,y,k)
getCMYK (CMYK c m y k) = (c,m,y,k)

getRed :: Colour -> Float
getRed col = r
    where (r,_,_) = (getRGB col)
getGreen :: Colour -> Float
getGreen col = g
    where (_,g,_) = (getRGB col)
getBlue :: Colour -> Float
getBlue col = b
    where (_,_,b) = (getRGB col)
getHue :: Colour -> Float
getHue col = h
    where (h,_,_) = (getHSV col)

-- | utils

decimal :: Colour -> [Float]
decimal (RGB r g b) = [x/255 | x <- [r,g,b]]
decimal (HSV h s v) = [h/360,s/100,v/100]
decimal (CMYK c m y k) = [x/100 | x <- [c,m,y,k]]

rotate :: Float -> Float -> Float
rotate initial diff = newDegrees
    where newDegrees = (initial + diff) `mod'` 360.0

-- | Palette
type Palette = [Colour]

-- | colour harmonies
analogous :: Colour -> Palette
analogous col = colours
    where (h,s,v) = getHSV col
          left = HSV (rotate h (-30)) 0 0 
          right = HSV (rotate h 30) 0 0 
          colours = [left, col, right]

