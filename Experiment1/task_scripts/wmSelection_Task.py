#import libraries to use

import numpy    as np
import os.path  as op
import pandas   as pd
from psychopy   import visual
from psychopy   import event, core, gui, data, logging
import numpy.random as npr
import math
import os
from copy import copy, deepcopy

wd = '/Users/sammi/Desktop/Experiments/DPhil/wmSelection'
#wd = 'C:\experiments\Sammi\wmSelection' #Anna Watts
#wd = 'C:/Users/testing/Desktop/sammi' #programming booth in ohba eeg lab

os.chdir(wd)

datafolder    = op.join(wd,'data')
infofolder    = op.join(datafolder,'info')
behfolder     = op.join(datafolder,'datafiles')
logfolder     = op.join(datafolder, 'logs')
eyefolder     = op.join(datafolder, 'eyes')
if not op.exists(datafolder):
    os.mkdir(datafolder)
if not op.exists(infofolder):
    os.mkdir(infofolder)
if not op.exists(behfolder):
    os.mkdir(behfolder)
if not op.exists(logfolder):
    os.mkdir(logfolder)
if not op.exists(eyefolder):
    os.mkdir(eyefolder)


participant = gui.Dlg(title="WM Selection", pos=(200,400));
participant.addText('Participant information');
participant.addField('subject id (s##)', tip='or subject code');
participant.addField('Age:', tip="years");
participant.addField('Gender:', tip="m/f");
participant.addField('date:',tip = "ddmmyyyy");
participant.addField('handedness:');
participant.addField('test_location:', tip = 'laptop = 0, eeglab = 1, eyeetracking booth = 2, shielded eeglab = 3, Anna Watts booth = 4');
#participant.addField('eyelinked:');
#participant.addField('send triggers:')
#participant.addField('diode_test:')
participant.show();

subject = {}
subject['ID']           = int(participant.data[0])
subject['Age']          = str(participant.data[1])
subject['Gender']       = str(participant.data[2])
subject['date']         = str(participant.data[3])
subject['handedness']   = str(participant.data[4])
test_loc                = int(participant.data[5]);
#iseyelinked             = int(participant.data[6]);
#sendtriggers            = int(participant.data[7]);
#pd_test                 = int(participant.data[8])


subfname    = '%s/wmSelection_S%02d_info.csv' %(infofolder,subject['ID'])
subfile     = open(subfname,'w')
subfile.write('{},{},{},{},{} \n'.format(subject['ID'], subject['Age'], subject['Gender'], subject['handedness'], subject['date']))
subfile.close()
#
fname       = '%s/wmSelection_S%02d_%s.csv' %(behfolder,subject['ID'],subject['date'])
#edfname     = 'wmS_S%02d.edf' %(subject['ID'])

#set screen parameters
if test_loc == 0: #laptop for testing, macbook pro (retina atm)
    resxy   = [1680, 1050] #laptop pixel resolution (mac)
    resxy   = [1920, 1080] #windows laptop
    reshz   = 60 #refresh rate of the screen
    sw      = 28.5 #screen width
    vdistcm = 50 #distance from screen
    mon     = 'mbp' #identifying string for the monitor on the computer!
    mon     = 'dell' #this is for my windows laptop
    retina  = True
elif test_loc   == 1: #testing in the AW booth (bigger screen now
    resxy       = [1920, 1080]
    reshz       = 100
    screenwidth = 60
    vdistcm     = 100
    mon         = 'AWB'
    retina      = True
elif test_loc == 4: #Anna Watts booth
    resxy   = [1920, 1080]
    reshz   = 100 #refresh rate of screen
    sw      = 53 #screen width
    vdistcm = 90 #distance from screen
    mon = 'AWB' #identifying string for the monitor on the computer
    retina = True
elif test_loc == 5: #piloting station in ohba eeg lab
    resxy = [1920, 1080]
    reshz = 100
    sw = 53
    vdistcm = 40
    mon = 'testpc'
    retina = True
#should add in others here, but right now its not necessary

#
#if sendtriggers:
#    from psychopy import parallel
#    IOport = parallel.ParallelPort(address = '0xC050') #need to change address to your parallel port address, whatever
#    holdvalue = 0 #reset the port to this after every trigger
#    IOport.setData(holdValue) #initialise the parallel port at 0 here, if not done then it causes problems for some reason


#build the task structure here

nitems  = 2 #load 2 wm
ncues   = 1 #only one possible cue
ndelays = 3 #array -> cue, cue -> probe, response -> confidence judgement
ntrls   = 32
nblocks = 8 #for now


cond  = np.array([0, 0, 1, 1]) #neutral = 0, cued = 1
cues  = np.array([0, 0, 1, 1]) #basically same as above lols
pside = np.array([0, 1, 0, 1]) #which side the item thats probed was presented on (needs counterbalancing for lateralisation analyses)

colours = np.array([
        [255, 255, 255], #white?
        [255, 255,  21], #yellow ?
        [  0,   0, 255], #blue
        [150, 150, 150]  #dark grey (neutral)
        ])
colcond = np.array([0,1]) #two colour conditions for left and right colours (i)
colours_cond = np.array([[1,2], [2,1]])


colcond = np.sort(np.tile(colcond, int(ntrls/2/colcond.size)))
colcond = np.tile(colcond, int(ntrls/colcond.size))

cond  = np.tile(cond,  int(ntrls/cond.size))
cues  = np.tile(cues,  int(ntrls/cues.size))
pside = np.sort(np.tile(pside, int(ntrls/pside.size)))

struct = np.stack([cond, cues, colcond, pside]).T
struct = pd.DataFrame(struct, columns = ['cond', 'cue', 'colcond', 'pside'])

#subject = {}; subject['ID'] = 1 #force for now
struct['subid'] = subject['ID']

#set arbitrary integers for some values we're about to pre-allocate
struct['stimcol1']  = -1
struct['stimcol2']  = -1
struct['cuecol']    = -1
struct['probedcol'] = -1
struct['pcol']      = -1

#colours:  yellow = 1, blue = 2, neutral (dark dark grey?) = 3
struct['stimcol1']  = np.where(struct.colcond == 0, colours_cond[0][0], colours_cond[1][0]) #if colcond ==0, the first stimcol (left) is yellow, else (colcond == 1) it's blue
struct['stimcol2']  = np.where(struct.colcond == 0, colours_cond[0][1], colours_cond[1][1]) #if colcond ==0, the second stimcol (right) is blue, else (colcond == 1) it's yellow

struct['cuecol'] = np.where(struct.cue ==0, 0, struct.cuecol) #neutral cue, cuecol == 3 [for indexing the colours array]
struct['cuecol'] = np.where(np.logical_and(struct.cue == 1, struct.pside == 0), struct.stimcol1, struct.cuecol) #if the probed item is from the  left, get the  left stimulus colour
struct['cuecol'] = np.where(np.logical_and(struct.cue == 1, struct.pside == 1), struct.stimcol2, struct.cuecol) #if the probed item is from the right, get the right stimulus colour

struct['probedcol'] = np.where(struct.pside   == 0, struct.stimcol1, struct.stimcol2) #just get the colour of the probed item and log (not for anything important just check there isn't an acc diff between colours

struct['pcol']      = np.where(struct.cue == 1, 0, struct.pcol) #if there's a cue, no information about item in the probe (valid retrocues only)
struct['pcol']      = np.where(np.logical_and(struct.cue == 0, pside == 0), struct.stimcol1, struct.pcol) #if no cue, but probing left item, present  left items colour at probe
struct['pcol']      = np.where(np.logical_and(struct.cue == 0, pside == 1), struct.stimcol2, struct.pcol) #if no cue but probing right item, present right items colour at probe


#set arbitrary values for inter-stimulus delays. this can easily be changed to jitter across trials here if needed
struct['delay1'] = 2 #in seconds -- array -> cue delay
struct['delay2'] = 2 #in seconds -- cue -> probe delay

#if this bit here isn't in the template struct then it fills the column with nans after block 1, causing problems later on when setting type to int

template_struct     = deepcopy(struct) #take a copy as a template

struct              = struct.sample(frac = 1).sample(frac = 1).reset_index(drop = True) #shuffle twice

#loop over the rest of the desired blocks, and add to struct
for i in range(nblocks-1):
    tmp           = deepcopy(template_struct)
    delay2        = np.round(npr.uniform(low = 2, high = 4, size = ntrls), decimals = 1) #delay 2 randomly sampled from uniform distribution between 2-4 seconds, 100ms steps
    tmp['delay2'] = delay2
    tmp           = tmp.sample(frac = 1).sample(frac = 1).reset_index(drop = True)
    struct        = pd.concat([struct,tmp], axis = 0, ignore_index = True) #bind rows
#struct now contains the full task structure not just one block -- each block is balanced so you can end at the end of any block if necessary

#finalise some other details (orientations, trial number, block number, etc)

struct['trialnum']   = np.arange(1,struct.shape[0]+1, dtype = 'int') #after shuffling set the trial number (for logs)
struct['block']      = np.sort(np.tile(np.add(np.arange(nblocks),1),ntrls)) #and set the block number (so can look at block diffs if you really want, but just tracks what block they were in

struct['ori1']       = npr.randint(0,180, size = struct.shape[0]) #set the left stimulus orientation
struct['ori2']       = npr.randint(0,180, size = struct.shape[0]) #set the right stimulus orientation
struct['targori']    = np.where(struct.pside == 0, struct.ori1, struct.ori2) #if probing the left item, then the target orientation is the left (if right, get the right)
struct['nontargori'] = np.where(struct.pside == 0, struct.ori2, struct.ori1) #if probing left, the right item is the nontarget ori (this just makes mixture modelling easier later on)
struct['pstartang']  = npr.randint(0,180, size = struct.shape[0]) #generate random number for the angle of the dial to first appear after pressing space

#set arbitrary integers for behavioural response data we'll collect on each trial of the task
struct['DT']         = -1
struct['CT']         = -1
struct['resp']       = -1
struct['ITI']        = -1

struct = struct.astype(int) #coerce to integers, saves from problems with using == for numbers in loops later on


#set possible locations for the array bars
locs = np.array([[+6, 0],                   #right location
                 [-6, 0]])                  #left location
barsize = (5.7, 0.8)                        #height and width of bars in the task (degrees visual angle)


#some functions to help run the task

def runescape(): #this just quits the task (safely) without saving anything extra
    screen.close()
    core.quit()

def runescape_task(block, blockDT, blockCT, blockResp, blockRespClicked, blockConfStartTime, blockConfClicked, blockConfClickTime, blockConfAngle, blockDialConfInterv, ntrls = 60): #quits the task, saving data up until the present point in time
    #write current data before quitting
    blockstruct['DT']           = blockDT
    blockstruct['CT']           = blockCT
    blockstruct['resp']         = blockResp
    blockstruct['clickresp']    = blockRespClicked
    blockstruct['confDT']       = blockConfStartTime
    blockstruct['confclicked']  = blockConfClicked
    blockstruct['confCT']       = blockConfClickTime
    blockstruct['confang']      = blockConfAngle

    blockstruct['ITI']            = blockITI
    blockstruct['DialConfInterv'] = blockDialConfInterv
    blockstruct.to_csv(op.join(behfolder, 'wmSelection_S%02d_block_%02d.csv'%(subject['ID'], block+1)), na_rep = np.nan, index = False)
    runescape()

def getMouseAngle(): #function getting the bearing of the cursor from the centre of the screen
    currPos  = mouse.getPos() #get current position
    currAng  = math.atan2(currPos[1], currPos[0]) #atan2 appears to have zero set at -90 degrees (270 from vertical)
    currAng  = np.degrees(currAng)
    roundAng = np.round(currAng)
    if int(roundAng) == np.nan:
        roundAng = 0
    else:
        roundAng = int(roundAng)
    return [(-roundAng+180), (-roundAng - 180)]

def wrap(x):
    #wrap angle between -180 and 180 degrees
    y = (x+180)%360 - 180
    return y

def wrap90(x):
    #wrap between -90 and 90 degrees
    #vectorised to use on arrays (for feedback phase)
    y = np.subtract(np.mod(np.add(x,90),180),90)
    return y

def getAngleFromMouse():
    currPos = mouse.getPos()
    currAng = math.atan2(currPos[1], currPos[0])
    currAng = np.round(np.degrees(currAng))
    return wrap(-currAng + 180)

def getStartPosFromAngle(angle):
    if angle > 90:
        ang = np.radians(180-angle)
        x = 6 * math.cos(ang)
        y = 6 * math.sin(ang)
        return [x,y]
    elif angle < 90:
        ang = np.radians(angle)
        x = 6 * math.cos(ang)
        y = 6 * math.sin(ang)
        return [-x,y]
    elif angle == 90:
        return [0,6]
    elif angle == 0:
        return [-6, 0]

#def getstartpos(angle):
#    x = math.atan2(

def wrap_bound(x, bound = 180):
    y = (x + bound)%2*bound - bound
    return y

def mirrorAngle(x,ref): #mirror two angles around a reference orientation
    #ref is the angle/orientation of the response given by the participant
    #x is the angle given by the cursor position, and must be mirror symmetrically around the ref
    ang1 = ref - x
    ang2 = ref + ang1
    #ang2 = wrap(ang2)
    #ang2 = wrap(ang2) + 180
    ang2 = ang2%360
    return [x, ang2]

def check_quitting():
    keys = event.getKeys(keyList = ['q'])
    if 'q' in keys:
        runescape_task(block, blockDT, blockCT, blockResp, blockRespClicked, blockConfStartTime, blockConfClicked, blockConfClickTime, blockConfAngle,blockDialConfInterv, ntrls = 60) #change to runescape_task() later on

#set some task parameters (unless these end up getting jittered, where we'll set in the task structure creation)
array_dur = 0.25
cue_dur   = 0.25
probe_max = 2  #max 2s to make the dialup response
conf_max  = 2  #max 2s to make the confidence judgement response

#create the screen
screen = visual.Window(size                 = resxy,
                       units                = 'deg',
                       colorSpace           = 'rgb',
                       color                = (-1,-1,-1), #change to black background!
                       fullscr              = True,
                       monitor              = mon, #will need changing based on monitor in use
                       useRetina            = retina, #comment out if not on a retina screen (basically only needed for my laptop)
                       winType              = 'pyglet') #this always needs setting as there are problems with pygame.

#probe circle appearance
probecirc = visual.Circle(screen, radius = barsize[0]/2, edges = 64, lineColor = colours[0], lineWidth = 5,units = 'deg',pos = (0,0), lineColorSpace = 'rgb255') #black probe circle

#initialise a mouse
mouse = event.Mouse(win = screen, visible = True) #set up mouse for the task (`visible = False` will stop it being visible on screen. default is visible)

#draw the array bars here
leftbar  = visual.Rect(screen, width = barsize[0], height = barsize[1], pos = locs[1], ori = 0, units = 'deg', fillColorSpace = 'rgb255', lineWidth = 0)
rightbar = visual.Rect(screen, width = barsize[0], height = barsize[1], pos = locs[0], ori = 0, units = 'deg', fillColorSpace = 'rgb255', lineWidth = 0)
respbar  = visual.Rect(screen, width = barsize[0], height = barsize[1]/8, pos = (0,0), ori = 0, units = 'deg', fillColorSpace = 'rgb255', lineWidth = 0, opacity = .5) #only used during confidence phase

#draw the lines that will be part of the rotating probe
pline1 = visual.Line(screen, start = [-barsize[0]/2 - .3,0], end = [-barsize[0]/2 +.3,0], lineWidth = 6, lineColor = colours[0], lineColorSpace = 'rgb255')
pline2 = visual.Line(screen, start = [ barsize[0]/2 - .3,0], end = [ barsize[0]/2 +.3,0], lineWidth = 6, lineColor = colours[0], lineColorSpace = 'rgb255')

#confidence interval lines
cline1 = visual.Rect(screen, width = barsize[0], height = barsize[1]/8, pos = (0,0), fillColor = colours[0], fillColorSpace = 'rgb255', lineWidth = 0, units = 'deg')
cline2 = visual.Rect(screen, width = barsize[0], height = barsize[1]/8, pos = (0,0), fillColor = colours[0], fillColorSpace = 'rgb255', lineWidth = 0, units = 'deg')

startmsg = visual.TextStim(screen, text = 'press any key to begin the block'   , units = 'deg', height = .7, pos = (0,-3))
endmsg   = visual.TextStim(screen, text = 'press space to leave the experiment', units = 'deg', height = .7, pos = (0,-6))

fbmsg1   = visual.TextStim(screen, text = '', units = 'deg', height = .7, pos = (0,4))
fbmsg2   = visual.TextStim(screen, text = '', units = 'deg', height = .7, pos = (0,2))


#draw the types of cues we can have in the task
whtfix = visual.TextStim(screen, text = '+', font = '', pos = (0,0), color = colours[0], height = 2, colorSpace = 'rgb255') #white (i.e. neutral coloured --probe--)
yelfix = visual.TextStim(screen, text = '+', font = '', pos = (0,0), color = colours[1], height = 2, colorSpace = 'rgb255') #yellow fixation cross
blufix = visual.TextStim(screen, text = '+', font = '', pos = (0,0), color = colours[2], height = 2, colorSpace = 'rgb255') #blue   fixation cross
gryfix = visual.TextStim(screen, text = '+', font = '', pos = (0,0), color = colours[3], height = 2, colorSpace = 'rgb255') #grey   fixation cross

cues = [whtfix, yelfix, blufix, gryfix] #assign to list, indexing same as the colours array - can index from the trial structure now
fix_ind = 3

def runTrial(trialnum, nframes, trlstruct):
    #timings (frame-time) for event onsets / offsets
    array_onset     =                int(trliti            * reshz)
    array_offset    = array_onset  + int(array_dur         * reshz)
    cue_onset       = array_offset + int(trlstruct.delay1  * reshz)
    cue_offset      = cue_onset    + int(cue_dur           * reshz)
    probe_onset     = cue_offset   + int(trlstruct.delay2  * reshz)
    #probe offset and confidence judgement don't begin with the same syntax, and are pretty much nested loops within the trial structure

    #set some params before the display loop to hopefully improve timing during the trial (jitter in the ITI doesn't matter much)
    leftbar.setOri(trlstruct.ori1)
    leftbar.setFillColor(colours[trlstruct.stimcol1])
    rightbar.setOri(trlstruct.ori2)
    rightbar.setFillColor(colours[trlstruct.stimcol2])

    probe_started       = False
    probedelay_over     = False
    clicked             = False
    clickresp           = False
    responded           = False
    dialup              = False
    probe_over          = False
    confidence_started  = False
    resptimer           = core.Clock()
    conftimer           = core.Clock()

    trldt      = np.nan
    trlct      = np.nan
    trlconfclicktime = np.nan
    trlconfstarttime = np.nan

    mouse.setVisible(True)

    for i in np.add(np.arange(nframes, dtype = 'int'),1): #loop over frames, starting at 1
        check_quitting() #check that the participant isn't quitting the task now

        if i == 1:
            cues[fix_ind].setAutoDraw(True)
            logging.exp('iti_start', t = globalClock.getTime())

        if i == array_onset:
            leftbar.setAutoDraw(True)
            rightbar.setAutoDraw(True)
            logging.exp('array_on', t = globalClock.getTime())

        if i == array_offset:
            leftbar.setAutoDraw(False)
            rightbar.setAutoDraw(False)
            logging.exp('array_off', t = globalClock.getTime())

        if i == cue_onset:
            cues[fix_ind].setAutoDraw(False)
            cues[trlstruct.cuecol].setAutoDraw(True)
            logging.exp('cue_onset', t = globalClock.getTime())

        if i == cue_offset:
            cues[trlstruct.cuecol].setAutoDraw(False)
            cues[fix_ind].setAutoDraw(True)
            logging.exp('cue_offset', t = globalClock.getTime())

        if i == probe_onset: #onset time of the probe, start the probe routine
            probe_started = True


        #because everything after the probe has variable durations, we stop the normal checkpoint-based timing and will have sub-loops that loop over screen flips to control durations
        if probe_started:                           #probe routine
            probe_started = False                   #once this is quitted, it should never re-enter the routine again on the same trial
            resptimer.reset()                       #decision time calculated relative to probe onset
            cues[fix_ind].setAutoDraw(False)        #stop drawing the fixation cross
            dialup      = False                     #can't start the dialup before the mouse has moved (dial probe not on screen at this point)
            startpos    = mouse.getPos()            #need to reference it to init the mouse or it won't work when you try to check if it's been moved
            logging.exp('probe_onset', t = globalClock.getTime())

            event.clearEvents(eventType='keyboard')
            prbstartkeys = event.getKeys(keyList = ['space'])
            while not dialup:
                if 'space' in prbstartkeys:
                    dialup  = True                  #break out of this while loop to start the next bit
                    trldt   = resptimer.getTime()   #get this point in time, as this is the 'decision time' or access time
                else:                               # if they haven't moved the mouse...
                    prbstartkeys = event.getKeys(keyList = ['space'])
                    check_quitting()                #check they're not trying to leave the task
                    probecirc.draw()                #
                    cues[trlstruct.pcol].draw()     # in this phase, all that is present on the screen is the probe circle, and the fixation cross appropriately coloured
                    screen.flip()                   # white if previously retrocued, coloured if not (probe presents target information to enable recall)

            clickresp = 0                           #force this -- participant hasn't clicked to respond just yet
            mouse.clickReset()

            logging.exp('dial_start', t = globalClock.getTime()) #log when the space bar was pressed, so the dial starts
            #pstartpos = getStartPosFromAngle(int(trlstruct.pstartang)) #force an integer here in case
            #mouse.setPos(newPos = pstartpos)  #this does set the position, just need to check that it works on the testing computers
            for f in range(respframes):             #start the responding phase!
                if not responded:                   #if they haven't clicked just yet...
                    check_quitting()                #check they aren't trying to leave the task
                    cues[trlstruct.pcol].draw()     #keep the appropriately coloured central cross on screen
                    probecirc.draw()                #and the probe circle
                    newOrient = getMouseAngle()     #get the angle between the origin and the mouse, referenced to west (left)
                    pline1.setOri(newOrient[0])     #set the probe lines (help with the response) to the ends of an imaginary bar, following the angle generated by the mouse
                    pline2.setOri(newOrient[1])
                    pline1.draw()                   #draw these probe lines
                    pline2.draw()
                    screen.flip()                   #present all of it
                    mouse1, _, _ = mouse.getPressed()   #check if they've clicked to finalise report of orientation
                    if (mouse1):                        #if they clicked to report the orientation
                        logging.exp('response_click', t = globalClock.getTime())
                        responded       = True
                        dialup          = False
                        finalori        = newOrient[0]%180  #record the final orientation of the imaginary bar (reported orientation) - wrapped, as remains in logfile (between -180 -> 180)
                        respori         = newOrient[0]      #store the non-wrapped response ori to orient the bar in the confidence judgement
                        clickresp       = 1                 #log that they clicked to respond (clickresp goes into the logfile)
                        trlct           = resptimer.getTime()
                        probe_over      = True              #probe is now over given they clicked to respond
                        probe_started   = False             #force these two back to false to ensure probe routine is not revisited
                        dialup          = False
                        mouse.clickReset()                  #reset clicks now, as clicks are used in the confidence judgement routine
                elif responded:                             #for this response duration, if they've already clicked to respond then ...
                    continue

            probe_over      = True                          #if the probe phase is over, let's move on
            if probe_over and clickresp == 0:               #if probe finished but they didn't click to respond
                finalori    = newOrient[0]%180              #still log the final orientation generated by the mouse (but record that they didn't click to respond, as can be factored into analysis)
                respori     = newOrient[0]
            dialup          = False
            probe_start     = False

            if probe_over: #need to delay here for the dial-conf interval

                for x in range(respconfdelayframes): #for the duration of the dial-confidence delay ...
                    cues[fix_ind].draw() #just present the fixation cross for this interval!
                    screen.flip()

            probedelay_over = True


            if probedelay_over:                                          #now head to the confidence judgement...
                probe_over      = False                             #once this phase is exited, this boolean stops it re-entering on the same trial
                probedelay_over = False
                respbar_ori     = finalori                               #orientation we'll use for the basis of the confidence judgement
                respbar.setOri(respbar_ori)                         #set the orientation of a bar that is used to set the centre of the confidence judgement
                respbar.setFillColor(colours[trlstruct.probedcol])  #set its colour to the probed colour
                mouse.clickReset()                                  #reset clicks in case they randomly clicked between phases
                conftimer.reset()                                   #and reset the confidence judgement timer to more accurately get confidence onset time
                logging.exp('conf_appear', t = globalClock.getTime()) #log onset of the confidence initial probe

                event.clearEvents(eventType='keyboard')
                confstartkeys = event.getKeys(keyList = ['space'])
                while not confidence_started:
#                    if mouse.mouseMoved():                          #if they move the mouse to start the confidence judgement
                    if 'space' in confstartkeys:
                        confidence_started = True
                        trlconfstarttime   = conftimer.getTime()    #get onset of movement on the confidence judgement
                        #mouse.setPos(newPos = getStartPosFromAngle(int(finalori))) #set mouse to start at the end of the bar where they reported the orientation initially
                        #mouse.setVisible(True)
                    else:                                           # and while they haven't moved the mouse..
                        confstartkeys = event.getKeys(keyList = ['space'])
                        check_quitting()                            #if they haven't moved the mouse, first check they aren't trying to quit
                        probecirc.draw()                            #
                        respbar.draw()                              # and present the probe circle and the response they made on this trial, so they can decide on their confidence
                        screen.flip()                               #

                if confidence_started:                              #if they moved the mouse, start the confidence report routine
                    logging.exp('conf_dial', t = globalClock.getTime()) #log the onset of the confidence dial
                    clicked = 0                                     #they can't have clicked to confirm yet, so enforce 0

                    for c in range(confframes):                     #loop over frames in confidence report duration
                    #while not clicked:                              #this will just keep in the confidence phase to help you out with diagnostics
                        check_quitting()                            #check if they're quitting the task or not
                        if clicked == 0:                            #while they haven't clicked to respond..
                            probecirc.draw()
                            respbar.draw()                          #draw the bar indicating how they reported the target orientation
                            responded_ori = respbar.ori
                            newOri        = getAngleFromMouse()                 #get the angle of the mouse
                            mirrored_angs = mirrorAngle(newOri, responded_ori)  #mirror this angle, centred on the responded orientation (so the interval is symmetrical around their response)
                            if newOri     > responded_ori + 180:                #if 180
                                newOri    = newOri%180 #wrap(newOri)               #fixes issue of when to wrap angles..
                            elif newOri < 90 and responded_ori > 270:
                                newOri += 360                       #this fixes situation where the mouse angle goes from 360 to 0 degrees, and auto wraps the confidence angle to +90 degrees.
                                #Adding 360 in this scenario ensures that the below comparison of the mouse orientation and responded orientation can still be performed without bugs
                            elif responded_ori > 90 and newOri < 0: #this compensates for wrapping the output of getAngleFromMouse function
                                newOri += 360
                            if abs(newOri - responded_ori) >= 90:
                                newOri = responded_ori + 90
                                mirrored_angs[1] = responded_ori - 90
                            cline1.setOri(newOri)
                            cline2.setOri(mirrored_angs[1])
                            cline1.draw() # draw the first confidence line
                            cline2.draw()
                            screen.flip()
                            mouse1, _, _ = mouse.getPressed()   #check to see if the participant has clicked to confirm response of confidence range
                            if (mouse1):                        #if they have clicked
                                logging.exp('conf_clicked', t = globalClock.getTime())
                                clicked             = 1
                                confidence_angle    = getAngleFromMouse()
                                trlconfclicktime    = conftimer.getTime()
                                probe_over          = False
                        elif clicked == 1:
                            cues[fix_ind].draw()
                            screen.flip

                    if not clicked: #if they didn't clicked to respond and finalise the confidence judgement
                        confidence_angle = newOri #get the last angle that they made instead
                        #and keep clicked to 0 (false) as will get stored in the datafile
                    confidence_started  = False
        screen.flip()


    blockDT[trialnum]               = trldt
    blockCT[trialnum]               = trlct #this is time from probe onset to time to click
    blockResp[trialnum]             = finalori
    blockRespClicked[trialnum]      = clickresp
    blockConfStartTime[trialnum]    = trlconfstarttime
    blockConfClicked[trialnum]      = clicked
    blockConfClickTime[trialnum]    = trlconfclicktime
    blockConfAngle[trialnum]        = confidence_angle



timinglog = logging.LogFile(f = op.join(logfolder, 'wmSelection_S%02d_logfile.txt'%(subject['ID'])), level = 22, filemode = 'a')
globalClock = core.Clock() #set a global clock to log things with and check stimulus timings in a log file

for block in range(nblocks):
    if block > 0:
        #after the first block, inter-block screen will present feedback
        #need to get some info to do this before blockstruct is overwritten/cleared
        
        #these are the data I need
        resps    = np.array(blockstruct['resp'].tolist())
        targoris = np.array(blockstruct['targori'].tolist())
        DTs      = np.array(blockstruct['DT'].tolist())
        confangs = np.array(blockstruct['confang'].tolist())
        
        #calculate the info to give back to people
        rdif      = wrap90(np.subtract(resps, targoris))
        confwidth = wrap90(np.subtract(resps, confangs))
        confdiff  = np.subtract(np.abs(rdif), confangs)
        ave_rdif      = np.abs(rdif).mean() #mean absolute deviation
        ave_DT        = DTs.mean()          #mean decision time
        ave_confwidth = confdiff.mean()
        #if this value is negative, confidence interval was X degrees wider than needed
        #if this value is positive, then on average the confidence interval was X degrees too narrow (and didn't capture the target orientation)
        
        fbmsg1.setText('Average reaction time: %03dms \nAverage deviation: %s degrees'%(ave_DT*1000, str(np.round(ave_rdif, decimals = 1))))
        if ave_confwidth < 0:
            fbmsg2.setText('average confidence interval was %s degrees wider than necessary'%(str(np.round(np.abs(ave_confwidth), decimals = 1))))
        elif ave_confwidth > 0:
            fbmsg2.setText('average confidence interval was %s degrees too narrow'%(str(np.round(np.abs(ave_confwidth), decimals = 1))))
        
        fbmsg1.draw()
        fbmsg2.draw()

    blockstruct = deepcopy(struct.query('block == %d' %(block+1)))

    blockDT             = [-1]*blockstruct.shape[0]
    blockCT             = [-1]*blockstruct.shape[0]
    blockResp           = [-1]*blockstruct.shape[0]
    blockRespClicked    = [-1]*blockstruct.shape[0]
    blockConfStartTime  = [-1]*blockstruct.shape[0]
    blockConfClicked    = [-1]*blockstruct.shape[0]
    blockConfClickTime  = [-1]*blockstruct.shape[0]
    blockConfAngle      = [-1]*blockstruct.shape[0]
    blockITI            = [-1]*blockstruct.shape[0]
    blockDialConfInterv = [-1]*blockstruct.shape[0]

    startmsg.setText('press space to start block ' + str(block+1))
    startmsg.draw()
    screen.flip()

    keys = event.waitKeys(keyList = ['space', 'q'])
    if 'q' in keys:
        runescape_task() #needs changing to runescape_task eventually

    event.clearEvents(eventType = 'keyboard')

    for i in range(ntrls):
        trlstruct           = blockstruct.iloc[i,:].astype(int) #force to integer - everything should be an integer but lets you index by these values in the trial structure more readily
        trliti              = float(np.round(npr.random() + 1, decimals = 1)) #random ITI between 1 and 2 seconds
        trlDialConfInterv   = float(np.round(npr.uniform(low = 2, high = 4,size = 1), decimals = 1)) #random delay between dial end and conf start, between 2-4s from uniform (flat) distribution
        trlduration         = float(trliti + array_dur + trlstruct.delay1 + cue_dur + trlstruct.delay2)
        nframes             = int(trlduration * reshz) + 1
        respframes          = int(probe_max   * reshz)
        respconfdelayframes = int(trlDialConfInterv * reshz) #number of frames between end of response and start of confidence interval
        confframes          = int(conf_max    * reshz)

        blockITI[i] = trliti
        blockDialConfInterv[i] = trlDialConfInterv
        runTrial(i, nframes, trlstruct)

    #at the end of each block, save the blocked data to file...
    #first, write them to the blockstruct
    blockstruct['DT']             = blockDT
    blockstruct['CT']             = blockCT
    blockstruct['resp']           = blockResp
    blockstruct['clickresp']      = blockRespClicked
    blockstruct['confDT']         = blockConfStartTime
    blockstruct['confclicked']    = blockConfClicked
    blockstruct['confCT']         = blockConfClickTime
    blockstruct['confang']        = blockConfAngle
    blockstruct['ITI']            = blockITI
    blockstruct['DialConfInterv'] = blockDialConfInterv

    #write to a file
    blockstruct.to_csv(op.join(behfolder, 'wmSelection_s%02d_block_%02d.csv'%(subject['ID'], block+1)), na_rep = np.nan, index = False)

    #assign back into overall structure file. full completion of task means you dont need to concatenate blocks
    struct[struct.block == block+1] = blockstruct



#at the end of all the trials, save whole structure (presumably the task hasn't been quit before now
struct.to_csv(op.join(behfolder, 'wmSelection_S%02d_allBlocks.csv'%(subject['ID'])), na_rep = np.nan, index = False)

endmsg.draw()
screen.flip()

event.waitKeys(keyList = ['space']) #wait for them to press space before quitting
runescape() #exit the script
