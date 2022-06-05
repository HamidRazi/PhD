// specify stimuli
var face_conditioning_stimuli = [

  // free choice trials
  { choice_type: "free", left_image_type: "face", left_image_number: 0, right_image_type: "face", right_image_number: 1 },
  { choice_type: "free", left_image_type: "face", left_image_number: 1, right_image_type: "face", right_image_number: 0 },
  { choice_type: "free", left_image_type: "face", left_image_number: 0, right_image_type: "face", right_image_number: 1 },
  { choice_type: "free", left_image_type: "face", left_image_number: 1, right_image_type: "face", right_image_number: 0 },
  { choice_type: "free", left_image_type: "face", left_image_number: 0, right_image_type: "face", right_image_number: 1 },
  { choice_type: "free", left_image_type: "face", left_image_number: 1, right_image_type: "face", right_image_number: 0 },

  // forced choice trials
  { choice_type: "forced", left_image_type: "face", left_image_number: null, right_image_type: null, right_image_number: null },
  { choice_type: "forced", left_image_type: null, left_image_number: null, right_image_type: "face", right_image_number: null },
  { choice_type: "forced", left_image_type: "face", left_image_number: null, right_image_type: null, right_image_number: null },
  { choice_type: "forced", left_image_type: null, left_image_number: null, right_image_type: "face", right_image_number: null }

]

// specify stimuli
var place_conditioning_stimuli = [

    // free choice trials
    { choice_type: "free", left_image_type: "place", left_image_number: 0, right_image_type: "place", right_image_number: 1 },
    { choice_type: "free", left_image_type: "place", left_image_number: 1, right_image_type: "place", right_image_number: 0 },
    { choice_type: "free", left_image_type: "place", left_image_number: 0, right_image_type: "place", right_image_number: 1 },
    { choice_type: "free", left_image_type: "place", left_image_number: 1, right_image_type: "place", right_image_number: 0 },
    { choice_type: "free", left_image_type: "place", left_image_number: 0, right_image_type: "place", right_image_number: 1 },
    { choice_type: "free", left_image_type: "place", left_image_number: 1, right_image_type: "place", right_image_number: 0 },

    // forced choice trials
    { choice_type: "forced", left_image_type: "place", left_image_number: null, right_image_type: null, right_image_number: null },
    { choice_type: "forced", left_image_type: null, left_image_number: null, right_image_type: "place", right_image_number: null },
    { choice_type: "forced", left_image_type: "place", left_image_number: null, right_image_type: null, right_image_number: null },
    { choice_type: "forced", left_image_type: null, left_image_number: null, right_image_type: "place", right_image_number: null }

]


// specify stimuli
var bugmush_conditioning_stimuli = [

  // free choice trials
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 0, right_image_type: "bugmush", right_image_number: 1 },
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 1, right_image_type: "bugmush", right_image_number: 0 },
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 0, right_image_type: "bugmush", right_image_number: 1 },
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 1, right_image_type: "bugmush", right_image_number: 0 },
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 0, right_image_type: "bugmush", right_image_number: 1 },
  { choice_type: "free", left_image_type: "bugmush", left_image_number: 1, right_image_type: "bugmush", right_image_number: 0 },

]

// specify stimuli
var infosource_conditioning_stimuli = [

    // free choice trials
    { choice_type: "free", left_image_type: "infosource", left_image_number: 0, right_image_type: "infosource", right_image_number: 1 },
    { choice_type: "free", left_image_type: "infosource", left_image_number: 1, right_image_type: "infosource", right_image_number: 0 },
    { choice_type: "free", left_image_type: "infosource", left_image_number: 0, right_image_type: "infosource", right_image_number: 1 },
    { choice_type: "free", left_image_type: "infosource", left_image_number: 1, right_image_type: "infosource", right_image_number: 0 },
    { choice_type: "free", left_image_type: "infosource", left_image_number: 0, right_image_type: "infosource", right_image_number: 1 },
    { choice_type: "free", left_image_type: "infosource", left_image_number: 1, right_image_type: "infosource", right_image_number: 0 },
]

var other_conditioning_stimuli = [

    // free choice trials
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 0, right_info_image_type: "infosource", right_info_image_number: 1, left_bug_image_type: "bugmush", left_bug_image_number: 0, right_bug_image_type: "bugmush", right_bug_image_number: 1, InfoAnswerNumber: 0, numberq: 1
    },
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 1, right_info_image_type: "infosource", right_info_image_number: 0, left_bug_image_type: "bugmush", left_bug_image_number: 1, right_bug_image_type: "bugmush", right_bug_image_number: 0, InfoAnswerNumber: 1, numberq: 2
    },
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 0, right_info_image_type: "infosource", right_info_image_number: 1, left_bug_image_type: "bugmush", left_bug_image_number: 0, right_bug_image_type: "bugmush", right_bug_image_number: 1, InfoAnswerNumber: 1, numberq: 3
    },
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 1, right_info_image_type: "infosource", right_info_image_number: 0, left_bug_image_type: "bugmush", left_bug_image_number: 1, right_bug_image_type: "bugmush", right_bug_image_number: 0, InfoAnswerNumber: 0, numberq: 4
    },
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 0, right_info_image_type: "infosource", right_info_image_number: 1, left_bug_image_type: "bugmush", left_bug_image_number: 0, right_bug_image_type: "bugmush", right_bug_image_number: 1, InfoAnswerNumber: 1, numberq: 5
    },
    {
        choice_type: "free", left_info_image_type: "infosource", left_info_image_number: 1, right_info_image_type: "infosource", right_info_image_number: 0, left_bug_image_type: "bugmush", left_bug_image_number: 1, right_bug_image_type: "bugmush", right_bug_image_number: 0, InfoAnswerNumber: 0, numberq: 6
    },


    //    // forced choice trials
    //{
    //    choice_type: "forced", left_info_image_type: "infosource", left_info_image_number: null, right_info_image_type: null, right_info_image_number: null, left_bug_image_type: "bugmush", left_bug_image_number: null, right_bug_image_type: null, right_bug_image_number: null, InfoAnswerNumber: 1, numberq: 7
    //},
    //{
    //    choice_type: "forced", left_info_image_type: null, left_info_image_number: null, right_info_image_type: "infosource", right_info_image_number: null, left_bug_image_type: null, left_bug_image_number: null, right_bug_image_type: "bugmush", right_bug_image_number: null, InfoAnswerNumber: 0, numberq: 8
    //},
    //{
    //    choice_type: "forced", left_info_image_type: "infosource", left_info_image_number: null, right_info_image_type: null, right_info_image_number: null, left_bug_image_type: "bugmush", left_bug_image_number: null, right_bug_image_type: null, right_bug_image_number: null, InfoAnswerNumber: 0, numberq: 9
    //},
    //{
    //    choice_type: "forced", left_info_image_type: null, left_info_image_number: null, right_info_image_type: "infosource", right_info_image_number: null, left_bug_image_type: "bugmush", left_bug_image_number: null, right_bug_image_type: null, right_bug_image_number: null, InfoAnswerNumber: 1, numberq: 10
    //}


]