# Knife!


![knife](https://cloud.githubusercontent.com/assets/5771172/8354117/bd3eb12c-1b88-11e5-99e6-a8e7b6b27329.png)

Colour palettes with an emotional focus. This project is in very early development. Colour harmonies based on the twelve spoke colour wheel.

### How to use
Import the module
```haskell
ghci> import Knife
```
###### The Colour() class
Create a new colour as RGB, HSV or CMYK
```haskell
ghci> RGB 255 0 0
RGB 255 0 0
```

Convert between colour systems
```haskell
ghci> toCMYK $ HSV 0 100 100
CMYK 0 100 100 0
ghci> toHSV (RGB 255 0 0)
HSV 0 100 100
```

Palettes can be created automatically from colour harmonies
```haskell
ghci> analogous $ HSV 40 100 100
[HSV 10 100 100,HSV 40 100 100,HSV 70 100 100]
ghci> triadic $ HSV 40 100 100
[HSV 40 100 100,HSV 160 100 100,HSV 280 100 100]
```

```haskell
ghci> map hue (complementary $ HSV 60 100 100) 
[60,240]
ghci> map getRGB (analogous $ HSV 0 50 100)
[(255,128,191),(255,128,128),(255,191,128)]
```

The `stock` associated list comes with a series of common colours and their HSV values. These can be piped directly into any other fuction using the `getStock` function
```haskell
ghci> analogous $ getStock "burnt sienna" 
[HSV 344 65 91,HSV 14 65 91,HSV 44 65 91]
```

### Very Basic Example
`basicExample.hs`
Shell command:
```
runhaskell basicExample.hs | ./render.py
```
Output:

![palette](https://cloud.githubusercontent.com/assets/5771172/8350803/fc261f96-1b6b-11e5-8420-de8ecf6288b2.png)


### Colour harmonies
harmony | description
------------- | -------------
monochromatic col | palette from a single hue
analogous col  | palette from colours adjacent on the twelve spoke colour wheel
triadic col | palette from three hues equally spaced around the colour wheel
complementary col  | palette from opposite hues
splitComplementary col  | palette from two hues adjaced to the input colour's complement
tetradic col1 col2  | palette from two complementary pairs
