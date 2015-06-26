#!/usr/bin/python

import Image
import colorsys
#import math

################## Colour class ##################
class Colour():
    r,g,b = None,None,None
    h,s,v = None,None,None
    def setRGB(self,RGB):
        if validRGB(RGB):
            self.r,self.g,self.b = RGB
            self.h,self.s,self.v = RGBtoHSV(RGB)
    def getRGB(self):
        return (self.r,self.g,self.b)
    def setCMYK():
        return r
    def getCMYK():
        return r
    def setHSV(self,HSV):
        if validHSV(HSV):
            self.h,self.s,self.v = HSV
            self.r,self.g,self.b = HSVtoRGB(HSV)
    def getHSV(self):
        return (self.h,self.s,self.v)
    def compRGB(self):
        '''complimentary colour in RGB'''
        return HSVtoRGB((self.h+180 % 360,self.s,self.v))
    def relativeLuminance():
        None
    def contrastRatio():
        None
    def cycleHue(self,degrees):
        '''complimentary colour in RGB'''
        return HSVtoRGB((self.h+degrees % 360,self.s,self.v))

################## colour checks ##################
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

def validCMYK(tup):
    ''' checks that input is of appropriate form for CMYK 
    >>> validCMYK((0,56,10,100))
    True
    >>> validCMYK((80,20,10))
    False
    >>> validCMYK((-1,56,100,0))
    False
    '''
    return True if isinstance(tup,tuple) \
            and len(tup)==4 \
            and all(item >= 0 and item <=100 for item in tup) \
            else False


################## colour conversions ##################
def HSVtoRGB(HSV):
    ''' colour conversion 
    pure red and burnt umber:
    >>> HSVtoRGB((0,100,100))
    (255, 0, 0)
    >>> HSVtoRGB((9,74,54))
    (138, 51, 36)
    '''
    if validHSV(HSV):
        h,s,v = (float(i) for i in HSV)
        r,g,b = (int(round(i*255)) for i in colorsys.hsv_to_rgb(h/360,s/100,v/100))
        return (r,g,b)
    else:
        print "invalid HSV tuple"
        return None

def RGBtoHSV(RGB):
    ''' colour conversion 
    pure red and burnt umber:
    >>> RGBtoHSV((255,0,0))
    (0, 100, 100)
    >>> RGBtoHSV((138,51,36))
    (9, 74, 54)
    '''
    if validRGB(RGB):
        r,g,b = (float(i) for i in RGB)
        HSV = (colorsys.rgb_to_hsv(r/255,g/255,b/255))
        mult = (360,100,100)
        h,s,v = (int(round(HSV[int(i)]*mult[int(i)])) for i in range(3))
        return (h,s,v)
    else:
        print "invalid RGB tuple"
        return None

def RGBtoCMYK(RGB):
    ''' colour conversion 
    pure red and burnt umber:
    >>> RGBtoCMYK((255,0,0))
    (0, 100, 100, 0)
    >>> RGBtoCMYK((138,51,36))
    (0, 63, 74, 46)
    '''
    if validRGB(RGB):
        RGBpercent = tuple(float(i)/2.55 for i in RGB)
        k = int(round(100 - max(RGBpercent)))
        c,m,y = (int(round((100-RGBpercent[i] - k)/(100-k)*100)) \
                for i in range(3))
        return (c,m,y,k)
    else:
        print "invalid RGB tuple"
        return None

def CMYKtoRGB(CMYK):
    ''' colour conversion 
    pure red and burnt umber:
    >>> CMYKtoRGB((255,0,0))
    (0.0, 100.0, 100.0)
    >>> CMYKtoRGB((138,51,36))
    (9.0, 74.0, 54.0)
    '''
    if validCMYK(CMYK):
        r,g,b = (float(i) for i in RGB)
    else:
        print "invalid CMYK tuple"
        return None

  
    
################## stock colours ##################
stock = {
        'burnt umber'   : (138,51,36),
        'burnt sienna'  : (233,116,81)
        };


################## Palette class ##################
class Palette():
    colours = []

    def addColour(self,col):
        if isinstance(col,Colour): self.colours.append(col)     
        else: print "addColour only accepts arguments from the knife.Colour() class"
    # harmonies
    def monochromatic():
        '''palette in a single hue'''
        None
    def analogous(self,col):
        '''palette from adjacent hues'''
        self.colours = []
        comp1, comp2 = Colour(), Colour()
        comp1.setRGB(col.cycleHue(-30))
        self.colours.append(comp1)
        self.colours.append(col)
        comp2.setRGB(col.cycleHue(30))
        self.colours.append(comp2)
    def triadic(self,col):
        '''three equidistant colours'''
        self.colours = []
        self.colours.append(col)
        comp1, comp2 = Colour(), Colour()
        comp1.setRGB(col.cycleHue(120))
        self.colours.append(comp1)
        comp2.setRGB(col.cycleHue(240))
        self.colours.append(comp2)
    def complementary(self,col):
        '''palette from complementary pair'''
        self.colours = []
        self.colours.append(col)
        comp = Colour()
        comp.setRGB(col.cycleHue(180))
        self.colours.append(comp)
    def splitComplementary(self,col):
        '''palette from complementary end extended'''
        self.colours = []
        comp1, comp2 = Colour(), Colour()
        comp1.setRGB(col.cycleHue(150))
        self.colours.append(comp1)
        self.colours.append(col)
        comp2.setRGB(col.cycleHue(210))
        self.colours.append(comp2)
    def tetradic(self,col):
        '''palette from two complimentary pairs'''
        self.colours = []
        self.colours.append(col)
        comp1, comp2, comp3 = Colour(), Colour(), Colour()
        comp1.setRGB(col.cycleHue(60))
        self.colours.append(comp1)
        comp2.setRGB(col.cycleHue(180))
        self.colours.append(comp2)
        comp3.setRGB(col.cycleHue(240))
        self.colours.append(comp3)
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
                    index = int(x/(float(width)/len(self.colours)))
                    pixels[x,y] = tuple(i for i in self.colours[index].getRGB())
        img.save('palette.png')



## -- test function when run as `python knife.py`
if __name__ == '__main__':
    import doctest
    doctest.testmod()
