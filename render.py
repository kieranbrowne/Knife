#!/usr/bin/env python
import Image
import sys


def readPalette(data):
    RGBs = []
    data = data.replace('[','')
    data = data.replace(']','')
    data = data.replace('\n','')
    items = data.split(',')
    for item in items:
        if item[:3] == 'RGB':
            RGBs.append(tuple(map(int,item[4:].split(' '))))
            
    print RGBs
    return RGBs

def createImage(RGBs):
    width, height = 400, 200
    img = Image.new('RGB', (width,height), 'white')
    pixels = img.load()
    for x in range(width):
        for y in range(height):
            if RGBs:
                index = int(x/(float(width)/len(RGBs)))
                pixels[x,y] = RGBs[index]
    img.save('palette.png')


data = sys.stdin.readline()
createImage(readPalette(data))
