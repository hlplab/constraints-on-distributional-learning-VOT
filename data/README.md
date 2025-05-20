This directory contains all necessary data files for the reproduction of "Learning to Understand and Unfamiliar Talker: Testing distributional learning as a model of rapid adaptive speech perception".

1.  The Chodroff & Wilson 2017 corpus data cited in the paper is in "./chodroff_wilson_speech_db.csv", a dataframe of isolated and connected speech. 
    This file is an amalgamation of their Mixer 6 corpus (production_db/chodroff_wilson_mixer6_non-missing_vot_cog_f0.csv) and 
    (production_db/cueAnalysis_engCVC.txt with production_db/engCVC_gender.csv). 
    Please refer to "../scripts/production_database.R" to see how the dataframe of isolated and connected speech was created. 

2.  The perceptual experiment data are stored in ./experiment-results.csv

3.  Exposure stimuli and its cue measurements for that experiment are in ./exposure_stimuli_cue_measurements.csv