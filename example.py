import knife

# create new colour
col = knife.Colour()
col.setRGB((255,0,0)) # set col to red
col.setRGB(knife.stock['burnt umber'])
print "burnt umber's complimentary in RGB"
print col.compRGB()

# create colour from stockColour
print knife.stock.keys() # print all stock colours
print knife.stock['burnt sienna']
col.setRGB(knife.stock['burnt sienna'])

# create new palette
pal = knife.Palette()
pal.addColour(col)
pal.colours[0].getRGB() # get RGB tuple from first item
pal.tetradic(col)

pal.createImage() # draw palette to palette.jpg

print knife.HSVtoRGB((0,2,3))
print knife.HSVtoRGB((0,100,3))
print knife.HSVtoRGB((0,100,100))
print knife.HSVtoRGB(1)

print knife.RGBtoHSV(1)
print knife.RGBtoHSV((255,0,0))
print knife.HSVtoRGB(knife.RGBtoHSV((255,0,0)))
print knife.HSVtoRGB(knife.RGBtoHSV((138,51,36)))
