#!/usr/bin/python

class Colour():
    r,g,b = None,None,None
    def setRGB(self,r,g,b):
        self.r,self.g,self.b = r,g,b
    def getRGB(self):
        return (self.r,self.g,self.b)
    def setCMY():
        return r
    def getCMY():
        return r
    def getComplimentary():
        return(b,255-g,r)
        

## -- stock colours
BURNTUMBER = Colour()
BURNTUMBER.setRGB(138,51,36)


class Palette():
    palette = []
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


