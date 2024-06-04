/*
 * Author: Dave Kleinschmidt
 *
 *    Copyright 2012 Dave Kleinschmidt and
 *        the University of Rochester BCS Department
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License version 2.1 as
 *    published by the Free Software Foundation.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with this program.
 *    If not, see <http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html>.
 *
 * Modified by:Florian Jaeger (2020)
 *
 *    The code was further modified for the replication of Wade et al (2007)
 */

 var RESP_DELIM = ';';
 // Experiment object to control everything
 var e;


$(document).ready(function() {
  ////////////////////////////////////////////////////////////////////////
  // General setup
  ////////////////////////////////////////////////////////////////////////
  // take break every ... trials
  var breakEvery = 60;

  ////////////////////////////////////////////////////////////////////////
  // Create experiment
  ////////////////////////////////////////////////////////////////////////
  e = new Experiment({
      platform: 'mturk',
      rsrbProtocolNumber: 'RSRB00045955',
      rsrbConsentFormURL: 'https://www.hlp.rochester.edu/mturk/consent/RSRB45955_Consent_2023-01-11.pdf',
      survey: 'surveys/post_survey.html',
      cookie: 'DLVOT' // vowel perception L2
  });
  e.init();

  // Setting up exposure-test structure here: What type of block should occur in what order?
  // and how often should that block be repeated?
  var block_type = ['test', 'exposure', 'test', 'exposure', 'test', 'exposure', 'test', 'test', 'test'];
  var block_num = [1, 1, 1, 1, 1, 1, 1, 1, 1];
  var test_items = ['dilltill', 'dintin', 'diptip']; // all items (minimal word pairs)
  var test_locations = ['VOT-5_F0246', 'VOT5_F0246', 'VOT15_F0247','VOT25_F0247', 'VOT30_F0247', 'VOT35_F0247', 'VOT40_F0248',  'VOT45_F0248', 'VOT50_F0248', 'VOT55_F0248', 'VOT65_F0249', 'VOT70_F0249']; // each cue combination repeated as often as it should occur within 1 block

  ////////////////////////////////////////////////////////////////////////
  // Parse relevant URL parameters -- USER DEFINED
  ////////////////////////////////////////////////////////////////////////

  // Use the following parameter to skip parts of the experiment for debugging:
  var skipTo = e.urlparams['skipTo'];             // pre-[l]oading, p[ractice], e[xposure], t[est], s[urvey]
  var image_selection = e.urlparams['image_selection'];  // how should response pictures be ordered throughout exposure and test?
  if ($.inArray(image_selection, ['forward', 'backward']) < 0) throwError('unrecognized image_selection');

  var autoAdvanceReady = e.urlparams['autoAdvanceReady'];  // how should response pictures be ordered throughout exposure and test?
  if (typeof(autoAdvanceReady) === 'undefined') autoAdvanceReady = 'true';
  autoAdvanceReady = (autoAdvanceReady.toLowerCase() === 'true');
  if ($.inArray(autoAdvanceReady, [false, true]) < 0) throwError('unrecognized image_selection');

  // exposure lists are assumed to follow the format:
  //    lists/exposure_{cond_exp}_blockOrder{list_exp_block_order}_materials{list_exp_materials}_block{block_number}.csv
  //
  // block_number should be the order of the block counting both exposure and test blocks.
  //
  // e.g., lists/exposure_Shift0_blockOrderA_materialsA_block2.csv
  var cond_exp = e.urlparams['cond_exp'];  // condition name of exposure lists (without path or file extension)
  var list_exp_block_order = e.urlparams['list_exp_block_order'];  // Will LSQ designed exposure block order will be shown? {A, B, ...}
  var list_exp_materials = e.urlparams['list_exp_materials']; // Which randomization ('materials') will be used: {A, B, ...}

  // set instructions based on experimental condition (based on whether there is an exposure phase or not)
  var instruction_payment, instruction_experiment, instruction_test;

  // determine duration and pay based on number of trials plus fixed time for instructions and survey(s), plus 3 minutes per break:
  // 15 + round((n_trials * 3 / 60) + 3 * round(n_trials / breakEvery, 0). But we're adding a buffer to this.
  n_trials = 228;
  experiment_duration = 60;
  experiment_payment = '8.00';
  instruction_experiment = 'In this experiment, you will hear a female speaker saying words. Your task is to determine which word the speaker is saying by using ' +
                           'your mouse to click on the answer.';
  instruction_block1 = "<p>Now let's begin the experiment. <strong>It is important that you read the following instructions carefully.</strong></p>" +
                       "<p>This experiment investigates how listeners understand different types of talkers. You will hear many speech recordings of a female voice speaking one of several words. " +
                       "<strong>The female speaker you will hear might have an unfamiliar accent and sound different from other speakers you are familiar with</strong>." +
                       "<p>Overall you will hear " + n_trials + " trials, which should take approximately 40 minutes to complete. " +
                       "Before each recording, two words will be displayed on the screen (e.g., 'tin' and 'din'). <strong>To hear the recording, you need to press the green button " +
                       "in the center of the screen when it lights up.</strong> After listening to the recording, please click on the word the talker said. Your response will be " +
                       "recorded, and the experiment will advance to the next screen. (Please note that you can only indicate your response <i>after</i> you click the green button.)</p>" +

                       "<p><strong>Listen carefully, and answer as quickly and accurately as possible.</strong> Each trial should only take a few seconds to respond to. " +
                       "Many of the trials might sound similar to each other. We asked the speaker to produce the same words dozens of times, so that you can learn how that talker typically pronounces those words.</p>" +
                       "<p><strong>We require your full attention during the entire experiment</strong> for accurate results. We understand the experiment may feel tedious, and have provided two optional short breaks " +
                       "after each third of the trials. We appreciate your patience and compliance while completing this experiment. Your data will help us understand the neural foundations of how humans understand speech. ";

  instruction_test = "<p>Now let's begin the experiment.</p>" +
                     "<p>Your task is to decide which of two words displayed the speaker said by clicking on it. <strong>Listen carefully, and answer as quickly " +
                     "and accurately as possible (we estimate that each trial on average takes about 3-5 seconds to complete).</strong> " +
                     " You might feel that recordings are repeated as many of the recordings differ only in rather subtle ways.</p>";
  instruction_payment = 'The experiment will take up to ' + experiment_duration + ' minutes to complete and you will be paid $' + experiment_payment + ' USD.';

  ////////////////////////////////////////////////////////////////////////
  // Create and add instructions
  ////////////////////////////////////////////////////////////////////////
  if ($.inArray(skipTo, ['l', 'p', 't1', 'e1', 't2', 'e2', 't3', 's']) < 0) {
    var instructions = new InstructionsSubsectionsBlock(
        {
            logoImg: 'JSEXP/img/logo.png',
            title: 'Listen and click',
            mainInstructions: ['Thank you for your interest in our study!  This is a psychology experiment about how people understand speech. ' +
                               'You will listen to recorded speech, and tell us what you heard.',
                               '<span style="font-weight:bold;">Please read through each of the following requirements. ' +
                               'If you do not meet all requirements, please do not take this experiment.</span> You can click the names below to expand ' +
                               'or close each section.'],
            subsections: [
                {
                    title: 'Experiment length',
                    content: instruction_payment
                },
                {
                    title: 'Language requirements (grew up speaking American English)',
                    content: "You must be a native speaker of American English. " +
                             "<font color='red'><strong>If you have not spent almost all of your time until the age of 10 speaking English and living in the United States, " +
                             "you are not eligible to participate.</strong></font>",
                    checkboxText: 'I am a native American English speaker.'
                },
                {
                    title: 'Environment requirements (quiet room)',
                    content: 'Please complete this experiment in one sitting and in a quiet room, away from other noise. Please do NOT look at other web pages or other programs ' +
                             'while completing this experiment. It is important that you give this experiment your full attention.',
                    checkboxText: 'I am in a quiet room and will complete this experiment in one sitting.'
                },
                {
                    title: 'Hardware requirements (mouse + headphones)',
                    content: [{
                      subtitle: 'Mouse',
                      content: 'This experiment requires a mouse.',
                    },
                    {
                      subtitle: 'Headphones',
                      content: "<font color='red'><strong>It is essential that you wear headphones for this experiment.</strong></font> Otherwise we will NOT " +
                               "be able to use your data.<img id='audiopic' src='JSEXP/img/audiotypes.png' width='600'/>"
                    }],
                    checkboxText: 'I am wearing headphones and I am using a mouse.'
                },
                {
                  title: 'Headphone check',
                  content: ['Please complete the following headphone test to make sure your audio setup is compatible with this experiment, and that your headphones ' +
                            'are set to a comfortable volume.',
                            function() {
                                var headphoneCheckBlock = new HeadphoneCheckBlock(
                                    {
                                        instructions: '',
                                        implementation: 'McDermottLab'
                                    }
                                );
                                return(headphoneCheckBlock.init());
                            }, "<p></p>"]
                },
                {
                    title: 'Additional requirements',
                    content: ["<font color='red'><strong>Please do NOT take this experiment multiple times, and do NOT reload this page.</strong></font> " +
                              'If you share an MTurk/Prolific account with others who have taken this experiment, please make sure that they have not yet taken this experiment. ' +
                              "We cannot use data from reloaded or repeated experiments, and won't be able to approve your work.",
                              "We use cookies and MTurk/Prolific qualifications to make it easy for you to recognize whether you have taken this experiment previously. " +
                              "If you accept our cookies and do not delete them, this should prevent you from accidentally taking the experiment more than once."],
                    checkboxText: 'I (or others with the same worker ID) have not taken this experiment previously.'
                },
                {
                    title: 'Reasons work can be rejected',
                    content: ['If you pay attention to the instructions and <span style="font-weight:bold;">do not click randomly </span> your work will be approved. ' +
                              '<span style="color:red;"><strong>Please do NOT reload this page, even if you think you made a mistake.</strong></span> ' +
                              'We will not be able to use your data for scientific purposes, and you will not be able to finish the experiment. ' +
                              "We anticipate some mistakes will be made, but those will NOT affect the approval of your work. ",
                              'We will only reject work if you a) <strong>clearly</strong> do not pay attention to the instructions, b) reload the page, or c) repeat ' +
                              'the experiment. We reject far less than 1% of all completed experiments.'],
                    checkboxText: 'I understand the reasons my work might get rejected.'
                },
                {
                    title: 'Experiment instructions',
                    content: instruction_experiment,
                    checkboxText: 'I have read and understand the instructions.'
                },
                {
                    title: 'Informed consent',
                    content: e.consentFormDiv,
                    checkboxText: 'I consent to participating in this experiment'
                },
                {
                    title: 'Further (optional) information',
                    content: ['Sometimes it can happen that technical difficulties cause experimental scripts to freeze so that you will not be able to submit a experiment. ' +
                              'We are trying our best to avoid these problems. Should they nevertheless occur, we urge you to (1) take a screen shot of your browswer ' +
                              'window, (2) if you know how to also take a screen shot of your Javascript console, and (3) ' +
                              '<a href="mailto:hlplab@gmail.com">email us</a> this information along with the HIT/Experiment ID and your worker/Prolific ID. ',
                              'If you are interested in hearing how the experiments you are participating in help us to understand the human brain, feel free to ' +
                              'subscribe to our <a href="http://hlplab.wordpress.com/">lab blog</a> where we announce new findings. Note that typically about 1-2 years ' +
                              'pass before an experiment is published.'],
                    finallyInfo: true
                }
            ]
        }
    );
    e.addBlock({
        block: instructions,
        onPreview: true});
  } // end of instruction block

  ////////////////////////////////////////////////////////////////////////
  // Function that adds all the blocks when everything's ready and runs the experiment
  // This function is run every time a papa parse completes. Just add all the stimuli together
  // until the final block is reached. Then start creating and adding blocks.
  ////////////////////////////////////////////////////////////////////////
  // declared here so that papaparse can fill these vars and continue_experiment can read them
  var blocks_already_read_in = 0;
  var stimulus_list = [];
  var stimulus_list_length = [];
  var all_audio_filenames = [];
  var continue_experiment = function(block, filenames) {
    blocks_already_read_in++;
    throwMessage("Adding stimuli from block " + (block + 1) + " to overall stimulus list.");
    // Add stimuli to those that need to be preloaded and add path prefix to all filenames
    all_audio_filenames = all_audio_filenames.concat(filenames);
    throwMessage('Updated list of all stimuli: ' + all_audio_filenames);

    // When last block has been constructed
    if ((blocks_already_read_in) === block_type.length) {
      ////////////////////////////////////////////////////////////////////////
      // Create and add PRELOADING block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['p', 't1', 'e1', 't2', 'e2', 't3', 's']) < 0) {
        throwMessage("Preparing preloading block.");
        // Get all the unique filenames
        var unique_audio_filenames = all_audio_filenames.filter(function(item, pos, self) { return self.indexOf(item) == pos; });
        throwMessage('Preparing list of unique audio files for preloading: ' + unique_audio_filenames);

        var preloadingBlock = new MediaLoadingBlock({
          stimuli: new ExtendedStimuliFileList({
            prefix: '',
            mediaType: 'audio',
            filenames:   unique_audio_filenames,
            subtitles:   Array.apply(null, Array(unique_audio_filenames.length)).map(function(){return ""})
          }),
          totalLoadingThreshold: -1, // For 1 minute: 60000
          namespace: 'preload'
        });

        e.addBlock({
          block: preloadingBlock,
          instructions: "<p>Before you begin the experiment, " +
          "we need to pre-load the audio files now so they don't cause interruptions " +
          "during the rest of the experiment.</p>" +
          '<p>This will also give you an idea of your connection speed to our server. ' +
          'If for some reason the files are loading very slowly, you can return this experiment and move on, ' +
          'without wasting your time on the rest of the experiment.</p>',
          onPreview: false
        });
      } // end of preloading block

      ////////////////////////////////////////////////////////////////////////
      // Create and add PRACTICE block
      ////////////////////////////////////////////////////////////////////////
      if ($.inArray(skipTo, ['t1', 'e1', 't2', 'e2', 't3', 's']) < 0 && typeof(list_exp) !== 'undefined') {
        throwMessage("Starting practice block.");
        throwError("... practice block not yet implemented");
      } // end of practice block

      // Calculate relative start and end proportions of progress bar for blocks (to hide block transitions from participants)
      var all_stims_length = 0;
      var block_progressBarStartProportion = [];
      var block_progressBarEndProportion = [];
      for (let i = 0; i < block_type.length; i++) {

        // Count all stimuli so far (for progress bar adjustment)
        stimulus_list_length[i] = 0;
        for (let j = 0; j < stimulus_list[i].filenames.length; j++) {
          stimulus_list_length[i] += parseFloat(stimulus_list[i].reps[j]);
        }
        throwMessage("Adding " + stimulus_list_length[i] + " stimuli to overall stim length");
        all_stims_length += stimulus_list_length[i];
      }
      for (let i = 0; i < block_type.length; i++) {
        if (i === 0) {
          block_progressBarStartProportion[i] = 0;
        } else {
          block_progressBarStartProportion[i] = block_progressBarEndProportion[i - 1];
        }
        block_progressBarEndProportion[i] = block_progressBarStartProportion[i] + stimulus_list_length[i] / all_stims_length;
        throwMessage("Block " + (i + 1) + " progress bar start set to " + block_progressBarStartProportion[i] + "; end set to " + block_progressBarEndProportion[i]);
      }

      if ($.inArray(skipTo, ['e1', 't2', 'e2', 't3', 's']) < 0) {
        var current_instructions, current_stimOrderMethod, current_blockOrderMethod;

        for (let i = 0; i < block_type.length; i++) {
          if (i === 0) {
            current_instructions = instruction_block1;
          } else if (i === 3 || i === 5) {
            current_instructions = 'You have the option to take a short break of 2-3 minutes. Please click continue when you are ready for the next part of the experiment.'
          } else current_instructions = undefined;

          if (block_type[i] === 'exposure') {
            current_stimOrderMethod = 'shuffle_across_blocks';
            current_blockOrderMethod = 'shuffle_blocks';
          } else {
            current_stimOrderMethod = 'dont_randomize';
            current_blockOrderMethod = 'dont_randomize';
          }

          throwMessage("Adding block " + (i + 1) + " of type " + block_type[i])
          var current_block = new VisualGridBlock({
            stimuli: stimulus_list[i],
            images: stimImages,
            instructions: current_instructions,
            imageMapping: imageMapping,
            namespace: block_type[i] + (i + 1),
            allowFeedback: false,
            autoAdvanceReady: autoAdvanceReady,
            ITI_trialStartToImages: 750,  // time from trial start to showing pictures
            ITI_imagesToNextEvent: 1500,  // if autoAdvanceReady == T: time from trial to start to audio play; if autoAdvanceReady == F": time to green dot changing color indicating that it now can be clicked.
            ITI_responseToTrialEnd: 0,
            OnNegativeFeedback_blinkInterval: 200, // how long is the blink on and off?
            OnNegativeFeedback_blinkNumber: 4,     // how many blinks are shown? (takes blinkInterval ms per blink)
            progressBarStartProportion: block_progressBarStartProportion[i],
            progressBarEndProportion: block_progressBarEndProportion[i],
            itemOrder: undefined, // if undefined than order is generated from stimOrder and blockOrder methods. Alternatively, array of numbers.
            stimOrderMethod: current_stimOrderMethod,
            blockOrderMethod: current_blockOrderMethod,
            breakEvery: breakEvery,  // Take a break every x trials
            imagePositions: ['topleft', 'topright'],
            randomizeImagePositions: false, // Is true by default. If false, then just uses the list order above
            showFamiliarization: false,
            debugMode: e.debugMode
          });

          e.addBlock({
            block: current_block,
            onPreview: false,
            showInTest: true
          });
        }

      } // end of exposure-test block

      $("#continue").hide();
      e.nextBlock();
    } // All blocks have been added -- end of if (block === block_type.length)
  } // end of continue_experiment() function

  // Prevent loading of stimuli on preview.
  if (e.previewMode) {
    e.nextBlock();
  } else {
    ////////////////////////////////////////////////////////////////////////
    // Define general materials and mappings for visual grids
    ////////////////////////////////////////////////////////////////////////
    /* Define the images corresponding to each spoken sound/vowel/word */
    var stimImages = {
        dill: 'stimuli/images/dill_image.png',
        till: 'stimuli/images/till_image.png',
        dim: 'stimuli/images/dim_image.png',
        tim: 'stimuli/images/tim_image.png',
        din: 'stimuli/images/din_image.png',
        tin: 'stimuli/images/tin_image.png',
        dip: 'stimuli/images/dip_image.png',
        tip: 'stimuli/images/tip_image.png',
        flare: 'stimuli/images/flare_image.png',
        share: 'stimuli/images/share_image.png',
        rare: 'stimuli/images/rare_image.png',
        mock: 'stimuli/images/mock_image.png',
        space: 'stimuli/images/space_image.png',
        luck: 'stimuli/images/luck_image.png',
        other: 'stimuli/images/other_image.png'
    };

    // Define which images will show for a given word depending on the the condition (forward/backward) of that trial
    //
    //    'condition_name': {
    //           'target word_1': ['img_1', .., 'img_k'],
    //           ..
    //           'target word_j': ['img_1', .., 'img_k'],
    //     }
    var imageMapping = {
      'forward': {
        'TEST': ['dill', 'till', 'dim', 'tim', 'din', 'tin', 'dip', 'tip'],
        'dilltill': ['dill', 'till'],
        'dill': ['dill', 'till'], // kept for backward compatibility
        'till': ['dill', 'till'], // kept for backward compatibility
        'dill_unlabeled': ['dill', 'till'], // Added to allow labeled and unlabeled trials
        'till_unlabeled': ['dill', 'till'], // Added to allow labeled and unlabeled trials
        'dill_labeled': ['dill', 'dip'], // Added to allow labeled and unlabeled trials
        'till_labeled': ['tip', 'till'], // Added to allow labeled and unlabeled trials
        'dimtim': ['dim', 'tim'],
        'dim': ['dim', 'tim'], // kept for backward compatibility
        'tim': ['dim', 'tim'], // kept for backward compatibility
        'dintin': ['din', 'tin'],
        'din': ['din', 'tin'], // kept for backward compatibility
        'tin': ['din', 'tin'], // kept for backward compatibility
        'din_unlabeled': ['din', 'tin'], // Added to allow labeled and unlabeled trials
        'tin_unlabeled': ['din', 'tin'], // Added to allow labeled and unlabeled trials
        'din_labeled': ['din', 'dill'], // Added to allow labeled and unlabeled trials
        'tin_labeled': ['till', 'tin'], // Added to allow labeled and unlabeled trials
        'diptip': ['dip', 'tip'],
        'dip': ['dip', 'tip'], // kept for backward compatibility
        'tip': ['dip', 'tip'], // kept for backward compatibility
        'dip_unlabeled': ['dip', 'tip'], // Added to allow labeled and unlabeled trials
        'tip_unlabeled': ['dip', 'tip'], // Added to allow labeled and unlabeled trials
        'dip_labeled': ['dip', 'din'], // Added to allow labeled and unlabeled trials
        'tip_labeled': ['tin', 'tip'], // Added to allow labeled and unlabeled trials
        'flare': ['share', 'flare'],
        'share': ['share', 'rare'],
        'rare': ['flare', 'rare'],
        'mock': ['mock', 'dip'],
        'space': ['dill', 'space'],
        'luck': ['din', 'luck']},
      'backward': {
        'TEST': ['dill', 'till', 'dim', 'tim', 'din', 'tin', 'dip', 'tip'].reverse(),
        'dilltill': ['dill', 'till'].reverse(),
        'dill': ['dill', 'till'].reverse(),
        'till': ['dill', 'till'].reverse(),
        'dill_unlabeled': ['dill', 'till'].reverse(), // Added to allow labeled and unlabeled trials
        'till_unlabeled': ['dill', 'till'].reverse(), // Added to allow labeled and unlabeled trials
        'dill_labeled': ['dill', 'dip'].reverse(), // Added to allow labeled and unlabeled trials
        'till_labeled': ['tip', 'till'].reverse(), // Added to allow labeled and unlabeled trials
        'dimtim': ['dim', 'tim'].reverse(),
        'dim': ['dim', 'tim'].reverse(),
        'tim': ['dim', 'tim'].reverse(),
        'dintin': ['din', 'tin'].reverse(),
        'din': ['din', 'tin'].reverse(),
        'tin': ['din', 'tin'].reverse(),
        'din_unlabeled': ['din', 'tin'].reverse(), // Added to allow labeled and unlabeled trials
        'tin_unlabeled': ['din', 'tin'].reverse(), // Added to allow labeled and unlabeled trials
        'din_labeled': ['din', 'dill'].reverse(), // Added to allow labeled and unlabeled trials
        'tin_labeled': ['till', 'tin'].reverse(), // Added to allow labeled and unlabeled trials
        'diptip': ['dip', 'tip'].reverse(),
        'dip': ['dip', 'tip'].reverse(),
        'tip': ['dip', 'tip'].reverse(),
        'dip_unlabeled': ['dip', 'tip'].reverse(), // Added to allow labeled and unlabeled trials
        'tip_unlabeled': ['dip', 'tip'].reverse(), // Added to allow labeled and unlabeled trials
        'dip_labeled': ['dip', 'din'].reverse(), // Added to allow labeled and unlabeled trials
        'tip_labeled': ['tin', 'tip'].reverse(), // Added to allow labeled and unlabeled trials
        'flare': ['share', 'flare'].reverse(),
        'share': ['share', 'rare'].reverse(),
        'rare': ['flare', 'rare'].reverse(),
        'mock': ['mock', 'dip'].reverse(),
        'space': ['dill', 'space'].reverse(),
        'luck': ['din', 'luck'].reverse()}
    };

    for (let i = 0; i < block_type.length; i++) {
      throwMessage("Preparing block " + (i + 1) + " of type " + block_type[i]);
      if (block_type[i] === 'exposure') {
        ////////////////////////////////////////////////////////////////////////
        // Create and add EXPOSURE stimuli
        ////////////////////////////////////////////////////////////////////////
        var exposureList = 'lists/exposure-' + cond_exp + "_imageSelection-" + image_selection + "_blockOrder-" + list_exp_block_order + "_materials-" + list_exp_materials + "_block-" + (i + 1) + '.csv';

        throwMessage("Parsing exposure list: " + exposureList);
        Papa.parse(exposureList, {
          download: true,
          header: true,
          delimiter: ',',
          skipEmptyLines: true,
          complete: function(list) {
            // NOTE: it is important that the order of attributed (prefix, filenames, etc.) is held constant
            //       across blocks (incl. test and exposure blocks) since the R scripts reading in the results
            //       (specifically, the function formatData) assume as much. A failure to hold the order of
            //       the attribute fields constant won't prevent accurate data recording but it will make
            //       data import into R more complicated.
            stimulus_list[i] = new ExtendedStimuliFileList({
              prefix: '',
              filenames: getFromPapa(list, 'filename'),
              target_words: getFromPapa(list, 'target_word'),
              image_selections: repeat(image_selection, getFromPapa(list, 'filename').length),
              feedback: getFromPapa(list, 'feedback'),
              reps: getFromPapa(list, 'reps')
            });

            throwMessage("Done parsing exposure list for block " + (i + 1));
            continue_experiment(i, stimulus_list[i].filenames);
          }
        });
      } else if (block_type[i] === 'test') {
        throwMessage("Constructing test list for block " + (i + 1));
        ////////////////////////////////////////////////////////////////////////
        // Create and add TEST stimuli
        ////////////////////////////////////////////////////////////////////////
        stimulus_list[i] = get_pseudorandomized_test_list(test_items, test_locations, block_num[i], image_selection);

        throwMessage("Done constructing test list for block " + (i + 1));
        continue_experiment(i, stimulus_list[i].filenames);
      } else {
        throwError("unrecognized block_type for block " + (i + 1));
      }
    }



  } // end of everything that is only shown when not in preview mode
}); // end of document ready function
