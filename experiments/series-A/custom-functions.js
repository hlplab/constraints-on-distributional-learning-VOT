/* Creates a pseudo-randomized extended stimulus list of filenames that obeys the following constraints.
   Each block contains exactly one instance of each element of features. The number
   of instances of each item is held as balanced as possible within and across blocks,
   as is the assignment of features to items. Within each block the order of the selected
   items is entirely random.

   Assumes that files are called {items}_{features}.wav

   items    - an array of unique item IDs (minimal word pairs, e.g., dimtim, etc.)
   features - an array of VOT and f0 values (e.g., VOT10_F0246, etc.). These do *not*
              have to be unique. Each feature value should occur as often as it should
              be shown in one block (e.g., to form a normal or uniform distribution)
   numBlock - how many blocks to have in total, where each block goes through each
              *feature* values exactly once.
   image_selection - 'forward' or 'backward'

*/
function get_pseudorandomized_test_list(items, features, numBlock, image_selection) {
  if (!(items.constructor === Array)) items = Array.from(new Array(1), (x) => items);

  var filenames = [];
  var items_leftover = [];
  var target_words = [];
  var items_copy = [];
  var features_copy = [];
  for (b = 0; b < numBlock; b++) {
    // Make maximally balanced list of items in randomized order.
    features_copy = shuffle(features);
    items_copy = shuffle(repeat(items, Math.floor(features.length / items.length)).flat());

    // Make sure that items stay as balanced as possible *across* blocks, too.
    if (items_copy.length < features.length) {
      if (items_leftover.length > 0) {
        items_copy = items_copy.concat(items_leftover);
        items_leftover = items_copy.splice(features.length);
      } else items_copy = items_copy.concat(shuffle(items).flat());
    }

    // Concatenate stimulus filenames
    for (i = 0; i < features.length; i++) {
      filenames.push('stimuli/test/' + items_copy[i] + "_" + features_copy[i] + ".wav");
      target_words.push(items_copy[i]);
    }
  }
  throwMessage(filenames);

  // NOTE: it is important that the order of attributed (prefix, filenames, etc.) is held constant
  //       across blocks (incl. test and exposure blocks) since the R scripts reading in the results
  //       (specifically, the function formatData) assume as much. A failure to hold the order of
  //       the attribute fields constant won't prevent accurate data recording but it will make
  //       data import into R more complicated.
  return new ExtendedStimuliFileList({
    prefix: '',
    filenames: filenames,
    target_words: target_words,
    image_selections: repeat(image_selection, filenames.length),
    feedback: repeat(false, filenames.length),
    reps: repeat(1, filenames.length)
  });
}
