############################################################
# Supplementary Code S1
# 01_load_and_clean_data.R
# Purpose: Load raw ICILS 2023 school files and produce a
# cleaned school-level dataset for GenAI governance analyses.
############################################################

rm(list = ls())

pkgs <- c(
  "foreign", "readxl", "readr", "dplyr", "tidyr",
  "purrr", "stringr", "forcats", "tibble", "survey",
  "ggplot2", "ggrepel", "patchwork", "scales"
)

invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}))
invisible(lapply(pkgs, library, character.only = TRUE))

options(survey.lonely.psu = "adjust")

data_dir      <- "your_SAVData_path"
project_out   <- "your_figure_file"
codebook_path <- "ICILS2023MS_Codebook_PUF__final_IDB_.xlsx"

dir.create(project_out, recursive = TRUE, showWarnings = FALSE)

cntry_lab <- c(
  CHL = "Chile", TWN = "Chinese Taipei", CYP = "Cyprus",
  DNK = "Denmark", GRC = "Greece", KOR = "Korea, Rep.",
  NOR = "Norway", ROU = "Romania", SVK = "Slovak Rep.",
  SVN = "Slovenia", SWE = "Sweden", URY = "Uruguay"
)
genai_cntry <- names(cntry_lab)

ia3g_raw <- c(
  "IA3G02A","IA3G02B","IA3G03A","IA3G03B","IA3G04A","IA3G04B",
  "IA3G05","IA3G06","IA3G07A","IA3G07B","IA3G07C"
)

as_num <- function(x) {
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  if (is.character(x)) return(suppressWarnings(as.numeric(x)))
  suppressWarnings(as.numeric(x))
}

to_na <- function(x) {
  x <- as_num(x)
  x[x %in% c(7, 8, 9, 97, 98, 99)] <- NA
  x
}

read_bcg_sav <- function(path) {
  df <- foreign::read.spss(
    file = path,
    to.data.frame = TRUE,
    use.value.labels = FALSE,
    reencode = "UTF-8"
  ) |>
    tibble::as_tibble()
  
  df$CNTRY3 <- stringr::str_match(basename(path), "^BCG([A-Z]{3})I3\\.sav$")[, 2]
  df
}

bcg_files <- list.files(
  data_dir,
  pattern = "^BCG[A-Z]{3}I3\\.sav$",
  full.names = TRUE
)

codes <- stringr::str_match(basename(bcg_files), "^BCG([A-Z]{3})I3\\.sav$")[, 2]
bcg_files_12 <- bcg_files[codes %in% genai_cntry]

bcg <- purrr::map_dfr(bcg_files_12, read_bcg_sav) |>
  filter(CNTRY3 %in% genai_cntry)