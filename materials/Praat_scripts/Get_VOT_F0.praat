# specify directory where concatenated sounds and textgrid are saved
dir$ = "/Users/mata4004/Box/swedish_Beliefupdating_soundfiles/voicing-manipulation-swedish/speaker_015/"
sound$ = "SW015_d_t_chain.wav"
textGrid$ = "SW015_d_t_annotation.TextGrid"

sound = Read from file: dir$ + sound$
textgrid = Read from file: dir$ + textGrid$
minusObject: sound
# select the textgrid and assign it a variable
selectObject: textgrid 
textGrid$ = selected$ (1)

# Get the number of original files in the long text grid
nIntervals = Count intervals where: 1, "contains", "SW_015"

# Create a table to store results
resultsTable = Create Table with column names: "results", 0, "filename NEG_VOT VOT VOWEL F0_quarter F0_vowel_onset"

# Loop over all words in the textgrid; count the number of word intervals
for n to nIntervals

	selectObject: textGrid$

	intervalName$ = Get label of interval: 1, n
	intervalStart = Get start time of interval: 1, n
	intervalEnd = Get end time of interval: 1, n

	newPart = Extract part: intervalStart, intervalEnd, "yes"

	# Get duration of the NEG_VOT segment
	selectObject: newPart
	startingPoints = Get starting points: 2, "is equal to", "NEG_VOT"
	startTime = Get time from index: 1
	selectObject: newPart
	endPoints = Get end points: 2, "is equal to", "NEG_VOT"
	endTime = Get time from index: 1
	durationNEG_VOT = endTime - startTime
	
	removeObject: startingPoints, endPoints

	# Get duration of the VOT segment
	selectObject: newPart
	startingPoints = Get starting points: 2, "is equal to", "VOT"
	startTime = Get time from index: 1
	selectObject: newPart
	endPoints = Get end points: 2, "is equal to", "VOT"
	endTime = Get time from index: 1
	durationVOT = endTime - startTime
	
	removeObject: startingPoints, endPoints

	# Get duration of the vowel segment
	selectObject: newPart
	startingPoints = Get starting points: 2, "is equal to", "VOWEL"
	startTime = Get time from index: 1
	selectObject: newPart
	endPoints = Get end points: 2, "is equal to", "VOWEL"
	endTime = Get time from index: 1
	durationVowel = endTime - startTime
	vowel_quarter = durationVowel/4


		# Get first quarter of vowel, f0 measurements
		selectObject: newPart
		vowel_quarter_point =  startTime + vowel_quarter

		selectObject: sound
		View & Edit
		editor: sound 
		Select: startTime, vowel_quarter_point
		Zoom to selection
		f0_quarter = Get pitch
		Close
		selectObject: sound
		View & Edit
		editor: sound 
		Select: startTime, endTime
		Zoom to selection
		Move cursor to start of selection
		f0_vowel_onset = Get pitch
		Close
		endeditor


	removeObject: startingPoints, endPoints


	# Append to table
	selectObject: resultsTable
	Append row
	Set string value: n, "filename", intervalName$
	Set numeric value: n, "NEG_VOT", durationNEG_VOT
	Set numeric value: n, "VOT", durationVOT
	Set numeric value: n, "VOWEL", durationVowel	
	Set numeric value: n, "F0_quarter", f0_quarter
	Set numeric value: n, "F0_vowel_onset", f0_vowel_onset

	removeObject: newPart
	
endfor

# Done!

