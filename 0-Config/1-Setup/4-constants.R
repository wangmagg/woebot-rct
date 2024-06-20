### Sets of variables used in analyses

# Variables that are not composite, i.e. not calculated by combining subitems
NON_COMPOSITE_VARS <- c("group", "race", "eth", "sex", "gender", 
                        "orient", "marital", 
                        "educ", "empl", "disab",
                        "insur", "ther", "trt", "med", "mh", "psych_med", 
                        "crave", "cageaid", "heavy", 
                        "mid_crave", "mid_heavy",
                        "eot_crave", "eot_heavy",
                        "followup_crave", "followup_heavy")

# Composite variables that are calculated by summing subitems
SUMMED_COMPOSITE_VARS <- c('phq', 'mid_phq', 'eot_phq', 'followup_phq',
                           'gad', 'mid_gad','eot_gad', 'followup_gad', 
                           'sps', 'mid_sps','eot_sps', 
                           'dast', 'eot_dast',
                           'sipad', 'mid_sipad', 'eot_sipad', 'followup_sipad',
                           'taa', 'eot_taa',
                           'mid_waisr_g', 'eot_waisr_g',
                           'mid_waisr_t', 'eot_waisr_t',
                           'mid_waisr_b', 'eot_waisr_b',
                           'eot_urpi_a', 'eot_urpi_f',
                           'eot_csq_grp1', 'eot_csq_grp2')
# Composite variables that are calculated by multiplying subitems
MULTIPLIED_COMPOSITE_VARS <- c('qds', 'mid_qds', 'eot_qds', 'followup_qds')
# Composite variables that are calculated by taking the mean of subitems
AVERAGED_COMPOSITE_VARS <- c('bscq', 'mid_bscq', 'eot_bscq', 'followup_bscq')

# Variables for past30-day substance use
P30_VARS <- c('p30', 'mid_p30', 'eot_p30', 'followup_p30')
PST_P30_VARS <- c('pst_p30', 'mid_pst_p30', 'eot_pst_p30', 'followup_pst_p30')

# Outcome variables at baseline, 4-weeks (mid), 8-weeks (eot), and 12-weeks (followup)
BASELINE_OUTCOME_VARS <- c('p30', 'pst_p30', 'bscq', 'dast', 'gad', 'phq', 'sps', 'crave', 'heavy', 'sipad', 'taa', 'qds')
MID_OUTCOME_VARS <- c('p30', 'pst_p30', 'bscq', 'gad', 'phq', 'crave',  'heavy', 'sipad', 'qds')
EOT_OUTCOME_VARS <- c('p30', 'pst_p30', 'bscq', 'dast', 'gad', 'phq', 'sps', 'crave', 'heavy', 'sipad', 'taa', 'qds')
FOLLOWUP_OUTCOME_VARS <- c('p30', 'pst_p30', 'bscq', 'gad', 'phq', 'crave', 'heavy', 'sipad', 'qds')

OUTCOME_VARS <- c(BASELINE_OUTCOME_VARS, EOT_OUTCOME_VARS, MID_OUTCOME_VARS, FOLLOWUP_OUTCOME_VARS)
OUTCOME_VARS_DICT <- list('baseline' = BASELINE_OUTCOME_VARS,
                          'mid' = MID_OUTCOME_VARS,
                          'eot' = EOT_OUTCOME_VARS,
                          'followup' = FOLLOWUP_OUTCOME_VARS)

# Unordered categorical variables used in regression analyses
DISCRETE_FIXED_EFFECT_VARS <- c("group", "screen", "race", "eth", "gender", "orient",
                                "marital", "educ", "empl", "disab", "insur", "mh", "ther_status")
# Ordered categorical variables used in regression analyses
ORDERED_DISCRETE_FIXED_EFFECT_VARS <- c("crave", "sps", "cageaid")
# Continuous variables used in regression analyses
CONTINUOUS_FIXED_EFFECT_VARS <- c("age", "phq", "gad", "sipad", "bscq")

# Candidate variables included in regression analysis
FIXED_EFFECT_VARS <- c(DISCRETE_FIXED_EFFECT_VARS, ORDERED_DISCRETE_FIXED_EFFECT_VARS, CONTINUOUS_FIXED_EFFECT_VARS)

# Candidate variables included in fitting retention weights
ORDERED_DISCRETE_WEIGHT_VARS <- c("dast")
WEIGHT_VARS <- c(FIXED_EFFECT_VARS, ORDERED_DISCRETE_WEIGHT_VARS)

# Variables used to define subgroups
SUBGROUP_VARS <- c('gender',
                   'race',
                   'eth',
                   'age',
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
                   'mh',
                   'csq')