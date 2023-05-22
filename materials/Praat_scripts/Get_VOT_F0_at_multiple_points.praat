# specify directory where concatenated sounds and textgrid are saved
dir$ = "/Users/mata4004/Desktop/Swedish_BeliefUpdating_VOT/materials/stimuli_SWE/annotations/"
sound$ = "SW_exposure_stimuli.wav"
textGrid$ = "SW_exposure_stimuli_annotations.TextGrid"

sound = Read from file: dir$ + sound$
textgrid = Read from file: dir$ + textGrid$
minusObject: sound
# select the textgrid and assign it a variable
selectObject: textgrid 
textGrid$ = selected$ (1)

# Get the number of sound files in the long text grid as marked on tier 1
nSoundFiles = Count intervals where: 1, "contains", "VOT"
appendInfoLine: "nSoundFiles_",  nSoundFiles

# Create a table to store results -- change these columns to correspond to annotated segments
resultsTable = Create Table with column names: "results", 0, "filename vowel f0_5ms_into_vowel f0_10ms_into_vowel f0_15ms_into_vowel f0_20ms_into_vowel f0_25ms_into_vowel"

# Loop over each sound file in the textgrid; get the interval label, their start and end times.
for n to nSoundFiles

	selectObject: textGrid$

	intervalName$ = Get label of interval: 1, n
	intervalStart = Get start time of interval: 1, n
	intervalEnd = Get end time of interval: 1, n

#  Extract textgrid of the nth sound file segment that is part of the longer sound file
	newPart = Extract part: intervalStart, intervalEnd, "yes"

	# Within the extracted textgrid, get duration of the NEG_VOT segment
	#selectObject: newPart
	#startingPoints = Get starting points: 2, "is equal to", "NEG_VOT"
	#startTime = Get time from index: 1
	#appendInfoLine: "NEG_VOT start_is", startTime
	#selectObject: newPart
	#endPoints = Get end points: 2, "is equal to", "NEG_VOT"
	#endTime = Get time from index: 1
	#durationNEG_VOT = endTime - startTime
	
	#removeObject: startingPoints, endPoints

	# Within the extracted textgrid, get duration of the VOT segment
	#selectObject: newPart
	#startingPoints = Get starting points: 2, "is equal to", "VOT"
	#startTime = Get time from index: 1
	#appendInfoLine: "VOT start_is", startTime
	#selectObject: newPart
	#endPoints = Get end points: 2, "is equal to", "VOT"
	#endTime = Get time from index: 1
	#durationVOT = endTime - startTime
	
	#removeObject: startingPoints, endPoints

	# Within the extracted textgrid, get duration of the vowel segment
	selectObject: newPart
	startingPoints = Get starting points: 2, "is equal to", "vowel"
	startTime = Get time from index: 1
	appendInfoLine: "vowel start_is", startTime
	selectObject: newPart
	endPoints = Get end points: 2, "is equal to", "vowel"
	endTime = Get time from index: 1
	durationVowel = endTime - startTime
	#vowel_quarter = durationVowel/4


		# Within the extracted textgrid of sound file, mark first 5ms and 10ms of the vowel
		selectObject: newPart
		vowel_5ms =  startTime + 0.005
		appendInfoLine: "vowel_5ms_ is", vowel_5ms
		vowel_10ms = startTime + 0.010
		appendInfoLine: "vowel_10ms_ is", vowel_10ms
		vowel_15ms = startTime + 0.015
		appendInfoLine: "vowel_15ms_ is", vowel_15ms
		vowel_20ms = startTime + 0.020
		appendInfoLine: "vowel_20ms_ is", vowel_20ms
		vowel_25ms = startTime + 0.025
		appendInfoLine: "vowel_25ms_ is", vowel_25ms
		#30ms_into_vowel = startTime + 0.030

		#vowel_quarter_point =  startTime + vowel_quarter


		selectObject: sound
		appendInfoLine: sound$
		pitch = To Pitch: 0, 75, 600
		selectObject: pitch
		start = startTime
		end = vowel_5ms
		f0_5ms_into_vowel = Get mean: start, end, "Hertz"
		selectObject: pitch
		appendInfoLine: sound$
		start = startTime
		end = vowel_10ms
		f0_10ms_into_vowel = Get mean: start, end, "Hertz"
		selectObject: pitch
		start = startTime
		end = vowel_15ms
		f0_15ms_into_vowel = Get mean: start, end, "Hertz"
		selectObject: pitch
		start = startTime
		end = vowel_20ms
		f0_20ms_into_vowel = Get mean: start, end, "Hertz"
		selectObject: pitch
		start = startTime
		end = vowel_25ms
		f0_25ms_into_vowel = Get mean: start, end, "Hertz"
		removeObject: pitch


	removeObject: startingPoints, endPoints


	# Append to table
	selectObject: resultsTable
	Append row
	Set string value: n, "filename", intervalName$
	#Set numeric value: n, "NEG_VOT", durationNEG_VOT
	#Set numeric value: n, "VOT", durationVOT
	Set numeric value: n, "vowel", durationVowel	
	Set numeric value: n, "f0_5ms_into_vowel", f0_5ms_into_vowel
	Set numeric value: n, "f0_10ms_into_vowel", f0_10ms_into_vowel
	Set numeric value: n, "f0_15ms_into_vowel", f0_15ms_into_vowel
	Set numeric value: n, "f0_20ms_into_vowel", f0_20ms_into_vowel
	Set numeric value: n, "f0_25ms_into_vowel", f0_25ms_into_vowel

	removeObject: newPart
	
endfor

# Done!

