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
  instruction_block1 = "<p>Now let's begin the experiment.</p>" +
                       "<p>This experiment investigates how listeners understand different types of talkers. You will hear many speech recordings of a female voice speaking one of several words. " +
                       "<p>Overall you will hear " + n_trials + " trials, which should take approximately 40 minutes to complete. " +
                       "Before each recording, two words will be displayed on the screen (e.g., 'tin' and 'din'). <strong>To hear the recording, you need to press the green button " +
                       "in the center of the screen when it lights up.</strong> After listening to the recording, please click on the word the talker said. Your response will be " +
                       "recorded, and the experiment will advance to the next screen. (Please note that you can only indicate your response <i>after</i> you click the green button.)</p>" +

                       "<p><strong>Listen carefully, and answer as quickly and accurately as possible.</strong> Each trial should only take a few seconds to respond to. " +
                       "Many of the trials might sound similar to each other. We asked the speaker to produce the same words dozens of times, so that you can learn how that talker typically pronounces those words.</p>" +
                       "<p><strong>We require your full attention during the entire experiment</strong> for accurate results. We understand the experiment may feel tedious, and have provided two optional short breaks " +
                       "after each third of the trials. We appreciate your patience and compliance while completing this HIT. Your data will help us understand the neural foundations of how humans understand speech. ";

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
            mainInstructions: ['Thank you for your interest in our study!  This HIT is a psychology experiment about how people understand speech. ' +
                               'You will listen to recorded speech, and tell us what you heard.',
                               '<span style="font-weight:bold;">Before you accept this HIT, please read through each of the following requirements. ' +
                               'If you do not meet all requirements, please do not accept this HIT.</span> You can click the names below to expand ' +
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
                    content: 'Please complete this HIT in one sitting and in a quiet room, away from other noise. Please do NOT look at other web pages or other programs ' +
                             'while completing this HIT. It is important that you give this experiment your full attention.',
                    checkboxText: 'I am in a quiet room and will complete this HIT in one sitting.'
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
                            'are set to a comfortable volume. It is important that you keep your headphones at the same volume throughout the experiment and that you ' +
                            'do not remove them after adjusting your sound. ',
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
                    content: ["<font color='red'><strong>Please do NOT take multiple HITs of this experiment and do not reload this page once you have accepted the HIT.</strong></font> " +
                              'If you are sharing an account with other MTurkers who have taken this experiment, please make sure that they have NOT yet taken this experiment. ' +
                              "We cannot use data from reloaded or repeated HITs, and your work will NOT be approved.",
                              "We use cookies and MTurk qualifications to make it easy for you to recognize whether you have taken this experiment previously. " +
                              "If you accept our cookies and do not delete them, this should prevent you from accidentally taking the experiment more than once."],
                    checkboxText: 'I (or others with the same worker ID) have not taken this HIT previously.'
                },
                // {
                //     title: 'Sound check',
                //     content: ['Please complete the following sound test to make sure your browser is compatible with this experiment, and that your headphones are set to a ' +
                //               'comfortable volume. It is important that you keep your headphones at the same volume throughout the experiment and that you do not remove your ' +
                //               'headphones after adjusting your sound. Enter the word that you hear into each box.',
                //               function() {
                //                   var soundcheck = new SoundcheckBlock(
                //                       {
                //                           items: [
                //                               {
                //                                   filename: 'JSEXP/sounds/cabbage',
                //                                   answer: 'cabbage'
                //                               },
                //                               {
                //                                   filename: 'JSEXP/sounds/lemonade',
                //                                   answer: 'lemonade'
                //                               }
                //                           ],
                //                           instructions: ''
                //                       }
                //                   );
                //                   return(soundcheck.init());
                //               }]
                // },
                {
                    title: 'Reasons work can be rejected',
                    content: ['If you pay attention to the instructions and <span style="font-weight:bold;">do not click randomly </span> your work will be approved. ' +
                              '<span style="color:red;"><strong>Please do NOT reload this page, even if you think you made a mistake.</strong></span> ' +
                              'We will not be able to use your data for scientific purposes, and you will not be able to finish the HIT. ' +
                              "We anticipate some mistakes will be made, and those will NOT affect the approval of your work. ",
                              'We will only reject work if you a) <strong>clearly</strong> do not pay attention to the instructions, b) reload the page, or c) repeat ' +
                              'the experiment. We reject far less than 1% of all completed HITs.'],
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
                    content: ['Sometimes it can happen that technical difficulties cause experimental scripts to freeze so that you will not be able to submit a HIT. ' +
                              'We are trying our best to avoid these problems. Should they nevertheless occur, we urge you to (1) take a screen shot of your browswer ' +
                              'window, (2) if you know how to also take a screen shot of your Javascript console, and (3) ' +
                              '<a href="mailto:hlplab@gmail.com">email us</a> this information along with the HIT ID and your worker ID. ',
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

    e.nextBlock();
}); // end of document ready function
