#!/usr/bin/python

import Image
import colorsys
import math

class Colour():
    r,g,b = None,None,None
    h,s,v = None,None,None
    def setRGB(self,RGB):
        if validRGB(RGB):
            self.r,self.g,self.b = RGB
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

def validRGB(tup):
    ''' checks that input is of appropriate form for RGB 
    >>> validRGB((255,0,100))
    True
    >>> validRGB((-1,0,0))
    False
    >>> validRGB((0,256,0))
    False
    '''
    return True if isinstance(tup,tuple) \
            and len(tup)==3 \
            and all(item >= 0 and item <=255 for item in tup) \
            else False

def validHSV(tup):
    ''' checks that input is of appropriate form for HSV 
    >>> validHSV((359,100,100))
    True
    >>> validHSV((-1,1000,255))
    False
    '''
    return True if isinstance(tup,tuple) \
            and len(tup)==3 \
            and tup[0]>=0 and tup[0] <=359 \
            and all(item >= 0 and item <=100 for item in tup[1:2]) \
            else False

def HSVtoRGB(HSV):
    ''' colour conversion 
    pure red and burnt umber:
    >>> HSVtoRGB((0,100,100))
    (255.0, 0.0, 0.0)
    >>> HSVtoRGB((9,74,54))
    (138.0, 51.0, 36.0)
    '''
    if validHSV(HSV):
        h,s,v = tuple(float(i) for i in HSV)
        r,g,b = tuple(colorsys.hsv_to_rgb(h/360,s/100,v/100))
        return (round(r*255),round(g*255),round(b*255))
    else:
        return "fail :("

def RGBtoHSV(RGB):
    ''' colour conversion 
    pure red and burnt umber:
    >>> RGBtoHSV((255,0,0))
    (0.0, 100.0, 100.0)
    >>> RGBtoHSV((138,51,36))
    (9.0, 74.0, 54.0)
    '''
    if validHSV(RGB):
        r,g,b = tuple(float(i) for i in RGB)
        h,s,v = tuple(colorsys.rgb_to_hsv(r/255,g/255,b/255))
        return (round(h*360),round(s*100),round(v*100))
    else:
        return "fail :("

    
    

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
    def tetradic():
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
                if self.colours:
                    index = int(math.floor(x/(float(width)/len(self.colours))))
                    pixels[x,y] = self.colours[index].getRGB()
        img.save('palette.png')


## -- test function when run as `python knife.py`
if __name__ == '__main__':
    import doctest
    doctest.testmod()
