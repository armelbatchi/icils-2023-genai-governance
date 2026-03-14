############################################################
# Supplementary Code S1
# 02_construct_variables.R
# Purpose: Construct governance indicators, readiness index,
# and survey design objects from the cleaned dataset.
############################################################

rep_candidates <- names(bcg)[stringr::str_detect(names(bcg), "^CRWGT\\d+$")]
rep_num  <- as.integer(stringr::str_replace(rep_candidates, "^CRWGT", ""))
rep_cols <- rep_candidates[order(rep_num)][1:75]

bcg <- bcg |>
  mutate(
    TOTWGTC  = as_num(TOTWGTC),
    across(all_of(rep_cols), as_num),
    P_CIMPOS = as_num(P_CIMPOS),
    P_CONEG  = as_num(P_CONEG)
  ) |>
  filter(!is.na(TOTWGTC), TOTWGTC > 0) |>
  mutate(across(all_of(rep_cols), ~ tidyr::replace_na(., 0)))

bcg2 <- bcg |>
  mutate(across(all_of(ia3g_raw), to_na)) |>
  mutate(
    policy_teach_now = IA3G02A %in% 4,
    policy_stud_now  = IA3G02B %in% 4,
    rec_teach_now    = IA3G03A %in% 4,
    rec_stud_now     = IA3G03B %in% 4,
    auth_teach_yes   = IA3G04A %in% 2,
    auth_stud_yes    = IA3G04B %in% 2,
    cur_verify       = IA3G07A %in% 2,
    cur_appropriate  = IA3G07B %in% 2,
    cur_ethics       = IA3G07C %in% 2,
    cur_any          = cur_verify | cur_appropriate | cur_ethics,
    teach_allowed    = IA3G05 %in% c(1, 2),
    teach_restrict   = IA3G05 %in% 2,
    stud_allowed     = IA3G06 %in% c(1, 2),
    stud_restrict    = IA3G06 %in% 2,
    comp_policy      = policy_teach_now | policy_stud_now,
    comp_rec         = rec_teach_now | rec_stud_now,
    comp_auth        = auth_teach_yes | auth_stud_yes,
    comp_curr        = cur_any,
    readiness_0_4    = as.integer(comp_policy) +
      as.integer(comp_rec) +
      as.integer(comp_auth) +
      as.integer(comp_curr)
  )

des_rep <- survey::svrepdesign(
  data             = bcg2,
  weights          = ~TOTWGTC,
  repweights       = as.matrix(bcg2[, rep_cols]),
  type             = "other",
  combined.weights = TRUE,
  scale            = 0.5,
  rscales          = rep(1, 75),
  mse              = TRUE,
  compress         = FALSE
)