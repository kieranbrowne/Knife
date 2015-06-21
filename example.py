import knife

# create new colour
col = knife.Colour()
col.setRGB((255,0,0)) # set col to red

# create colour from stockColour
print knife.stock.keys() # print all stock colours
col = knife.Colour()
col.setRGB(knife.stock['burntSienna'])

# create new palette
pal = knife.Palette()
pal.addColour(col)
pal.colours[0].getRGB() # get RGB tuple from first item

pal.createImage() # draw palette to palette.jpg

