source('Analysis/config.R')

# Load helper functions
for (file in list.files('Analysis/0-Functions', full.names=TRUE)) {
  source(file)
}

save_dir <- file.path(ANALYSIS_OUT_DIR, "5-BivarCorr-Output")
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_descriptive <- read.csv(
  file.path(DATA_OUT_DIR, "dat_analysis_descriptive.csv")
)

# Set factors and do column-mean imputation for BSCQ
dat_descriptive <- dat_descriptive |> 
  fill_column_mean(c("bscq")) |>
  set_delta_vars(c("bscq"), "eot") |>
  set_delta_vars(c("bscq"), "mid") |>
  set_delta_vars(c("bscq"), "followup")

timepts <- c("eot", "mid", "followup")
vars <- c("p30", "bscq", "phq", "gad", "sps")

# Get Pearson correlations between pair of variables 
bivar_cor_single_pair <- function(dat, pair) {
  dat_complete <- dat |>
    select(all_of(pair)) |>
    drop_na()
  cor.test(dat_complete[,pair[1]], 
           dat_complete[,pair[2]]) |>
    broom::tidy() |>
    mutate(var_1 = pair[1], var_2 = pair[2])
}

# Compare Pearson correlation between 
co_cor_single_pair <- function(dat1, dat2, pair) {
  dat1_complete <- dat1 |>
    select(all_of(pair)) |>
    drop_na()
  dat2_complete <- dat2 |>
    select(all_of(pair)) |>
    drop_na()
  vars <- paste(pair[1], "+", pair[2])
  formula <- paste(paste("~", vars, sep = ""), "|", vars)
  cocor_obj <- cocor(formula = as.formula(formula),
        data = list(dat2_complete, dat1_complete))
  p_val <- cocor_obj@fisher1925$p.value
  z_score <- cocor_obj@fisher1925$statistic
  data.frame(var_1 = pair[1], var_2 = pair[2], p_val = p_val, z_score = z_score)
}

# Get bivariate correlations across change scores for
# substance use, confidence, depression, anxiety, work productivity
for (timept in timepts) {
  retain_var <- str_c("retained_", timept)
  delta_vars_timept <- str_c("delta", timept, vars, sep="_")

  dat_to_analyze <- dat_descriptive |>
    filter(!!sym(retain_var) == 1) |>
    select(any_of(delta_vars_timept), group)
  dat1 <- dat_to_analyze |> filter(group == 1)
  dat2 <- dat_to_analyze |> filter(group == 2)

  pair_comps <- combn(colnames(dat_to_analyze |> select(-group)), 2)
  bivar_cor_control <- apply(pair_comps, 2, bivar_cor_single_pair, dat = dat1)
  bivar_cor_treatment <- apply(pair_comps, 2, bivar_cor_single_pair, dat = dat2)

  # Get correlations in the control and treatment arms separately
  bivar_cor_control <- do.call(bind_rows, bivar_cor_control)
  bivar_cor_treatment <- do.call(bind_rows, bivar_cor_treatment)
  write.csv(
    bivar_cor_control,
    file.path(save_dir, sprintf("bivar_cor_control_%s.csv", timept))
  )
  write.csv(
    bivar_cor_treatment,
    file.path(save_dir, sprintf("bivar_cor_treatment_%s.csv", timept))
  )
  
  # Compare correlations across treatment and control arms
  co_cor <- apply(pair_comps, 2, co_cor_single_pair, dat1 = dat1, dat2 = dat2)
  co_cor <- do.call(bind_rows, co_cor)
  write.csv(co_cor, file.path(save_dir, sprintf("co_cor_%s.csv", timept)))
}
