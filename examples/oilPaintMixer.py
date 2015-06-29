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
    closest = closestHues(desiredHue,availableHues)
    if len(closest)==1:
        return 'just use ' + getPaintByHue(closest[0],)
       

def closestHues(desiredHue,hues):
    answer = []
    if desiredHue in hues:
        answer.append(desiredHue)
    else:
        upper = desiredHue
        while upper not in hues:
            upper = (upper+1) % 360
        answer.append(upper)
        lower = desiredHue
        while lower not in hues:
            lower = (lower+1) % 360
        answer.append(lower)
    return answer

def getPaintByHue(hue,hues):
    index = zip(*hues).index(hue)
    return zip(*availableHues)[0][index]
    
def closestDegree(search,direction,options):
    i = int(search)
    if   direction.lower() == 'cw' or 'clockwise':
        while i not in options:
            i = (i+1) % 360
    elif direction.lower() == 'ccw' or 'counterclockwise' \
            or 'acw' or 'anticlockwise':
        while i not in options:
            i = (i-1) % 360
    return i


if all(colour in knife.stock.keys() for colour in oils):
    #print zip(oils,getHues(oils))
    hue = input('What hue would you like to create?: ')
    print getMixingInstructions(hue,getHues(oils))
