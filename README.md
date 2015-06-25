# Knife
Colour palettes with an emotional focus

### How to use
```python
>>> import knife
```

Create a new colour
```python
>>> col = knife.Colour()
```

Colour can be set by RGB or HSV
```python
# set col to red 
>>> col.setRGB(255,0,0)
# is equivalent to:
>>> col.setHSV(0,100,100)
```

Colour values for RGB or HSV can be accessed regardless of how the colour was set
```python
>>> col.setHSV(0,100,100)
>>> col.getRGB()
(255, 0, 0)
>>> col.r
255
```


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

### Testing with doctest
