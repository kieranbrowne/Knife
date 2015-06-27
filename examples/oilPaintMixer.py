import sys, os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import knife

oils = [ 'cadmium yellow pale','cadmium yellow medium', 'cadmium orange', 'cadmium scarlet', 'cadmium red', 'quindcridone rose', 'dioxazine violet', 'ultramarine blue', 'thalo blue', 'thalo green', 'cadmium green', 'cadmium green pale' ]


def getHues(colourNames):
    ''' return list of hues from list of colour names '''
    return [knife.RGBtoHSV(RGB)[0] for RGB in [knife.stock[name] for name in colourNames]]

def getMixingInstructions(desiredHue,availableHues):
    if desiredHue in zip(*availableHues)[1]:
        index = zip(*availableHues)[1].index(desiredHue)
        return "just use " + str(zip(*availableHues)[0][index])
    else:



if all(colour in knife.stock.keys() for colour in oils):
    #print zip(oils,getHues(oils))
    hue = input('What hue would you like to create?: ')
    print getMixingInstructions(hue,zip(oils,getHues(oils)))
