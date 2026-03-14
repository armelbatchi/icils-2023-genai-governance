############################################################
# Supplementary Code S1
# 03_analysis_tables.R
# Purpose: Produce weighted tables and analytic summaries.
############################################################

binary_inds <- c(
  "policy_teach_now", "policy_stud_now",
  "rec_teach_now", "rec_stud_now",
  "auth_teach_yes", "auth_stud_yes",
  "cur_any",
  "teach_allowed", "stud_allowed",
  "teach_restrict", "stud_restrict"
)

ind_labels <- c(
  policy_teach_now = "Policy: teachers",
  policy_stud_now  = "Policy: students",
  rec_teach_now    = "Recommendations: teachers",
  rec_stud_now     = "Recommendations: students",
  auth_teach_yes   = "Authority guidance: teachers",
  auth_stud_yes    = "Authority guidance: students",
  cur_any          = "Curriculum reference (any)",
  teach_allowed    = "Teachers: allowed (any)",
  stud_allowed     = "Students: allowed (any)",
  teach_restrict   = "Teachers: allowed, restricted",
  stud_restrict    = "Students: allowed, restricted"
)

svy_stat <- function(var, percent = FALSE) {
  purrr::map_dfr(genai_cntry, function(cc) {
    dsub  <- subset(des_rep, CNTRY3 == cc)
    n_obs <- sum(!is.na(bcg2[[var]][bcg2$CNTRY3 == cc]))
    
    res <- tryCatch({
      f   <- as.formula(paste0("~I(as.numeric(", var, "))"))
      est <- survey::svymean(f, dsub, na.rm = TRUE)
      m   <- as.numeric(coef(est))[1]
      se  <- as.numeric(survey::SE(est))[1]
      z   <- qnorm(0.975)
      tibble(est = m, lo = m - z * se, hi = m + z * se, se = se)
    }, error = function(e) {
      tibble(est = NA_real_, lo = NA_real_, hi = NA_real_, se = NA_real_)
    })
    
    if (percent && !is.na(res$est)) {
      res <- res |>
        mutate(
          est = est * 100,
          lo  = pmax(lo * 100, 0),
          hi  = pmin(hi * 100, 100),
          se  = se * 100
        )
    }
    
    tibble(CNTRY3 = cc, indicator = var, n_nonmiss = n_obs,
           est = res$est, lo = res$lo, hi = res$hi, se = res$se)
  })
}

tab1 <- bind_rows(
  purrr::map_dfr(binary_inds, \(v) svy_stat(v, percent = TRUE)),
  svy_stat("readiness_0_4", percent = FALSE)
) |>
  mutate(
    Country   = dplyr::recode(CNTRY3, !!!cntry_lab),
    Indicator = dplyr::recode(indicator, !!!c(ind_labels, readiness_0_4 = "Governance Readiness Index (0-4)"))
  )

readiness_order <- tab1 |>
  filter(indicator == "readiness_0_4") |>
  arrange(est) |>
  pull(Country)

readr::write_csv(tab1, file.path(project_out, "Table1.csv"))