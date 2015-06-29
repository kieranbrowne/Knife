import sys, os, inspect

# import knife from relative path
cmd_folder = os.path.realpath(os.path.abspath(os.path.split(inspect.getfile( inspect.currentframe() ))[0]))
if cmd_folder not in sys.path:
    sys.path.insert(0, cmd_folder)
cmd_subfolder = os.path.realpath(os.path.abspath(os.path.join(os.path.split(inspect.getfile( inspect.currentframe() ))[0],"..")))
if cmd_subfolder not in sys.path:
    sys.path.insert(0, cmd_subfolder)
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
        mix = []



if all(colour in knife.stock.keys() for colour in oils):
    #print zip(oils,getHues(oils))
    hue = input('What hue would you like to create?: ')
    print getMixingInstructions(hue,zip(oils,getHues(oils)))
