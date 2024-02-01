NON_COMPOSITE_VARS <- c("group", "race", "eth", "sex", "gender", 
                        "orient", "marital", 
                        "educ", "empl", "disab",
                        "insur", "ther", "trt", "med", "mh", "psych_med", 
                        "crave", "cageaid", "heavy", 
                        "eot_crave", "eot_heavy",
                        "mid_crave", "mid_heavy",
                        "followup_crave", "followup_heavy")

SUMMED_COMPOSITE_VARS <- c('phq', 'gad', 'sps', 'dast', 'sipad', 'taa',
                           'eot_phq', 'eot_gad', 'eot_sps', 'eot_dast', 'eot_sipad', 'eot_taa',
                           'eot_csq_grp1', 'eot_csq_grp2', 'eot_urpi_a', 'eot_urpi_f', 
                           'eot_waisr_g', 'eot_waisr_t', 'eot_waisr_b',
                           'mid_phq', 'mid_gad', 'mid_sps', 'mid_sipad', 
                           'mid_waisr_g', 'mid_waisr_t', 'mid_waisr_b',
                           'followup_phq', 'followup_gad', 'followup_sipad')
MULTIPLIED_COMPOSITE_VARS <- c('qds', 'eot_qds', 'mid_qds', 'followup_qds')
AVERAGED_COMPOSITE_VARS <- c('bscq', 'eot_bscq', 'mid_bscq', 'followup_bscq')

P30_VARS <- c('p30', 'eot_p30', 'mid_p30', 'followup_p30')

BASELINE_OUTCOME_VARS <- c('p30', 'bscq', 'dast', 'gad', 'phq', 'sps', 'crave', 'heavy', 'sipad', 'taa', 'qds')
EOT_OUTCOME_VARS <- c('p30', 'bscq', 'dast', 'gad', 'phq', 'sps', 'crave', 'heavy', 'sipad', 'taa', 'qds')
MID_OUTCOME_VARS <- c('p30', 'bscq', 'gad', 'phq', 'crave',  'heavy', 'sipad', 'qds')
FOLLOWUP_OUTCOME_VARS <- c('p30', 'bscq', 'gad', 'phq', 'crave', 'heavy', 'sipad', 'qds')

OUTCOME_VARS <- c(BASELINE_OUTCOME_VARS, EOT_OUTCOME_VARS, MID_OUTCOME_VARS, FOLLOWUP_OUTCOME_VARS)
OUTCOME_VARS_DICT <- list('baseline' = BASELINE_OUTCOME_VARS,
                          'eot' = EOT_OUTCOME_VARS,
                          'mid' = MID_OUTCOME_VARS,
                          'followup' = FOLLOWUP_OUTCOME_VARS)

DISCRETE_FIXED_EFFECT_VARS <- c("group", "screen", "race", "eth", "gender", "orient",
                                "marital", "educ", "empl", "disab", "insur", "mh", "ther_status")
ORDERED_DISCRETE_FIXED_EFFECT_VARS <- c("crave", "sps", "cageaid")
CONTINUOUS_FIXED_EFFECT_VARS <- c("age", "phq", "gad", "sipad", "bscq")
FIXED_EFFECT_VARS <- c(DISCRETE_FIXED_EFFECT_VARS, ORDERED_DISCRETE_FIXED_EFFECT_VARS, CONTINUOUS_FIXED_EFFECT_VARS)
ORDERED_DISCRETE_WEIGHT_VARS <- c("dast")
WEIGHT_VARS <- c(FIXED_EFFECT_VARS, ORDERED_DISCRETE_WEIGHT_VARS)

SUBGROUP_VARS <- c('age',
                   'cageaid',
                   'dast',
                   'ther_status',
                   'p30_tob',
                   'pri_sub_is_alc',
                   'phq',
                   'gad',
                   'bscq',
                   'sipad',
                   'screen',
                   'p30',
                   'qds',
                   'taa',
                   'crave',
                   'mh')