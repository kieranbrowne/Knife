#!/usr/bin/env python
import Image
import sys

def readPalette(data):
    data = ''.join([ i for i in data if i not in '[]\n' ])
    RGBs = [ i for i in data.split(',') if i[:3] == 'RGB' ]
    return [ tuple(map(int,item[4:].split(' '))) for item in RGBs ]

def createImage(colours):
    width, height = 400, 200
    img = Image.new('RGB', (width,height), 'white')
    pixels = img.load()
    for x in range(width): 
        for y in range(height): 
            if colours:
                pixels[x,y] = colours[int(x/(float(width)/len(colours)))]
    img.save('palette.png')

createImage(readPalette(sys.stdin.readline()))
