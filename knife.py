#!/usr/bin/python

import Image

class Colour():
    r,g,b = None,None,None
    def setRGB(self,(r,g,b)):
        self.r,self.g,self.b = r,g,b
    def getRGB(self):
        return (self.r,self.g,self.b)
    def setCMY():
        return r
    def getCMY():
        return r
    def getComplimentary():
        None
        #return(b,255-g,r)
    def relativeLuminance():
        None
    def contrastRatio():
        None

## -- stock colours
stock = {
        'burntUmber'   : (138,51,36),
        'burntSienna'  : (233,116,81),
        };

class Palette():
    colours = []

    def addColour(self,col):
        if isinstance(col,Colour): self.colours.append(col)     
        else: print "addColour only accepts arguments from the knife.Colour() class"
    # harmonies
    def monochromatic():
        '''palette in a single hue'''
        None
    def analogous():
        '''palette from adjacent hues'''
        None
    def triadic():
        '''three equidistant colours'''
        None
    def complimentary():
        '''palette from complementary pair'''
        None
    def splitComplimentary():
        '''palette from complementary end extended'''
        None
    def tetratic():
        '''palette from two complimentary pairs'''
        None

    # pull colour 
    def spike():
        '''most saturated colour in palette'''
    def prime():
        '''colour to be used in most of image'''
    def createImage(self):
        width, height = 400, 200
        img = Image.new('RGB', (width,height), 'white')
        pixels = img.load()
        for x in range(width):
            for y in range(height):
                pixels[x,y] = self.colours[0].getRGB()
        img.save('palette.jpg')
