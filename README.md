# Knife!

![knife_logo svg 2015_06_25_21_54_34 0](https://cloud.githubusercontent.com/assets/5771172/8354039/0cfa2468-1b88-11e5-99b4-ff9a59bdab93.png)

Colour palettes with an emotional focus. This project is in very early development. Colour harmonies based on the twelve spoke colour wheel.

### How to use
Import the module
```python
>>> import knife
```
###### The Colour() class
Create a new colour
```python
>>> col = knife.Colour()
```

Colour can be set by RGB or HSV
```python
# set col to red 
>>> col.setRGB((255,0,0))
# is equivalent to:
>>> col.setHSV((0,100,100))
```

Colour values for RGB or HSV can be accessed regardless of how the colour was set
```python
>>> col.setHSV((0,100,100))
>>> col.getRGB()
(255, 0, 0)
>>> col.r
255
```

###### Stock colours
The library comes with a set of RGB values for useful colours
```python
# get list of stock colours
>>> knife.stock.keys()
# get RGB tuple for burntUmber
>>> knife.stock['burntUmber']
(138, 51, 36)
>>> col.setRGB(knife.stock['burntUmber'])
```


###### The Palette() class
Palettes can be created from colours
```python
>>> pal = knife.Palette()
>>> pal.addColour(col)
```
Or produced automatically based on colour harmonies
```python
>>> pal.splitComplementary(col)
```

Pointers to the palette's colours are stored in the `colours` list
```python
>>> pal.colours
[<__main__.Colour instance at 0x7f73ddd86fc8>, <__main__.Colour instance at 0x7f73e0d973b0>, <__main__.Colour instance at 0x7f73ddd86f38>]
```
Their values can be accessed and changed using the same fuctions as the Colour class
```python
>>> pal.colours[0].getRGB()
(255, 0, 0)
```

To view the created palette use createImage()
```python
>>> pal.createImage()
```

###### Basic Example
```python
import knife

col = knife.Colour() # create colour
col.setRGB(knife.stock['burntSienna'])

pal = knife.Palette() # create palette
# create palette from analogous colour harmony
pal.analogous(col) 
# export colour palette to `palette.png`
pal.createImage()
```

Output:

![palette](https://cloud.githubusercontent.com/assets/5771172/8350803/fc261f96-1b6b-11e5-8420-de8ecf6288b2.png)

### Testing with doctest
The module can be error tested by running `python knife.py`

### Colour harmonies
harmony | description
------------- | -------------
monochromatic(col) | palette from a single hue
analogous(col) | palette from colours adjacent on the twelve spoke colour wheel
triadic(col) | palette from three hues equally spaced around the colour wheel
complementary(col) | palette from opposite hues
splitComplementary(col) | palette from two hues adjaced to the input colour's complement
tetradic(col) | palette from two complementary pairs
