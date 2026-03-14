############################################################
# Supplementary Code S1
# 04_make_figures.R
# Purpose: Generate all manuscript and supplementary figures.
# Assumes 01_load_and_clean_data.R, 02_construct_variables.R,
# and 03_analysis_tables.R have already been run.
############################################################

# -----------------------------
# Check required objects
# -----------------------------
required_objects <- c(
  "bcg2", "tab1", "des_rep", "rep_cols",
  "cntry_lab", "genai_cntry", "readiness_order",
  "binary_inds", "ind_labels"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0) {
  stop(
    "The following required objects are missing: ",
    paste(missing_objects, collapse = ", "),
    ". Run scripts 01-03 first."
  )
}

# -----------------------------
# Output directory
# -----------------------------
if (!exists("project_out")) {
  project_out <- "your_output_file"
}
out_dir <- file.path(project_out, "your_figure_file")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Save helper
# -----------------------------
save_plot <- function(plot_obj, stem, width, height, dir = out_dir, dpi = 400) {
  ggsave(
    filename = file.path(dir, paste0(stem, ".png")),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  ggsave(
    filename = file.path(dir, paste0(stem, ".pdf")),
    plot = plot_obj,
    width = width,
    height = height,
    bg = "white"
  )
}

# -----------------------------
# Themes
# -----------------------------
theme_pub <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.caption     = element_text(size = base_size - 2, colour = "grey40",
                                      hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1, colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "grey70"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size - 1),
      strip.background = element_rect(fill = "grey93", colour = "grey70"),
      strip.text       = element_text(face = "bold", size = base_size - 1)
    )
}

theme_panel <- function() {
  theme_minimal(base_size = 12.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.title = element_text(color = "#111827"),
      axis.text = element_text(color = "#374151"),
      plot.title = element_text(face = "bold", size = 13, color = "#111827"),
      plot.subtitle = element_text(size = 10.2, color = "#4B5563"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "#374151"),
      plot.margin = margin(8, 14, 8, 8)
    )
}

# -----------------------------
# Reusable colors
# -----------------------------
col_teacher <- "#2C7FB8"
col_student <- "#1FA187"
col_pos     <- "#2C7FB8"
col_neg     <- "#D95F5F"

col_neither <- "#E9EEF4"
col_tonly   <- "#7FB3D5"
col_sonly   <- "#D9A441"
col_both    <- "#3A84B8"

# -----------------------------
# Shared pooled dataset
# -----------------------------
plot_df_pool <- bcg2 |>
  dplyr::mutate(
    readiness = as.integer(readiness_0_4),
    readiness_lab = factor(readiness, levels = 0:4, labels = paste0("Readiness ", 0:4)),
    teach_allowed_num  = as.numeric(teach_allowed == TRUE),
    stud_allowed_num   = as.numeric(stud_allowed == TRUE),
    teach_restrict_num = as.numeric(teach_restrict == TRUE),
    stud_restrict_num  = as.numeric(stud_restrict == TRUE),
    perm_regime = dplyr::case_when(
      teach_allowed == TRUE  & stud_allowed == TRUE  ~ "Both allowed",
      teach_allowed == TRUE  & stud_allowed != TRUE  ~ "Teachers only",
      teach_allowed != TRUE  & stud_allowed == TRUE  ~ "Students only",
      teach_allowed != TRUE  & stud_allowed != TRUE  ~ "Neither allowed",
      TRUE ~ NA_character_
    )
  )

des_pool <- survey::svrepdesign(
  data             = plot_df_pool,
  weights          = ~TOTWGTC,
  repweights       = as.matrix(plot_df_pool[, rep_cols]),
  type             = "other",
  combined.weights = TRUE,
  scale            = 0.5,
  rscales          = rep(1, length(rep_cols)),
  mse              = TRUE,
  compress         = FALSE
)

svy_by_readiness <- function(var, design_obj, label, pct = TRUE) {
  tmp <- survey::svyby(
    as.formula(paste0("~", var)),
    ~readiness,
    design_obj,
    survey::svymean,
    vartype = "se",
    na.rm = TRUE,
    keep.names = FALSE
  ) |>
    tibble::as_tibble()
  
  est_col <- names(tmp)[2]
  se_col  <- names(tmp)[3]
  
  out <- tmp |>
    dplyr::transmute(
      readiness = as.integer(readiness),
      est = .data[[est_col]],
      se  = .data[[se_col]],
      series = label
    )
  
  if (pct) {
    out <- out |>
      dplyr::mutate(est = est * 100, se = se * 100)
  }
  
  out |>
    dplyr::mutate(
      lo = est - 1.96 * se,
      hi = est + 1.96 * se,
      lo = if (pct) pmax(0, lo) else lo,
      hi = if (pct) pmin(100, hi) else hi
    )
}

# ============================================================
# FIGURE 1
# heatmap
# ============================================================
row_order <- rev(c(
  "Policy: teachers",
  "Policy: students",
  "Recommendations: teachers",
  "Recommendations: students",
  "Authority guidance: teachers",
  "Authority guidance: students",
  "Curriculum reference (any)",
  "Teachers: allowed (any)",
  "Teachers: allowed, restricted",
  "Students: allowed (any)",
  "Students: allowed, restricted"
))

domain_map <- c(
  "Policy: teachers"              = "Formal Governance",
  "Policy: students"              = "Formal Governance",
  "Recommendations: teachers"     = "Formal Governance",
  "Recommendations: students"     = "Formal Governance",
  "Authority guidance: teachers"  = "Formal Governance",
  "Authority guidance: students"  = "Formal Governance",
  "Curriculum reference (any)"    = "Formal Governance",
  "Teachers: allowed (any)"       = "Use Permission",
  "Teachers: allowed, restricted" = "Use Permission",
  "Students: allowed (any)"       = "Use Permission",
  "Students: allowed, restricted" = "Use Permission"
)

heat_dat <- tab1 |>
  dplyr::filter(indicator %in% binary_inds) |>
  dplyr::mutate(
    Country   = factor(Country, levels = readiness_order),
    Indicator = dplyr::recode(indicator, !!!ind_labels),
    Indicator = factor(Indicator, levels = row_order),
    Domain    = dplyr::recode(Indicator, !!!domain_map),
    txt_col   = dplyr::case_when(
      is.na(est) ~ "#4B5563",
      est >= 40  ~ "#F9FAFB",
      TRUE       ~ "#1F2937"
    )
  )

fig1 <- ggplot(heat_dat, aes(x = Country, y = Indicator, fill = est)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = if_else(is.na(est), "-", sprintf("%.0f%%", est)),
        colour = txt_col),
    size = 3.2,
    fontface = "bold"
  ) +
  geom_hline(yintercept = 4.5, linewidth = 1.2, colour = "white") +
  scale_fill_distiller(
    palette   = "Blues",
    direction = 1,
    na.value  = "grey78",
    name      = "Schools (%)",
    limits    = c(0, 100),
    breaks    = seq(0, 100, 25),
    labels    = function(x) paste0(x, "%")
  ) +
  scale_colour_identity() +
  scale_x_discrete(guide = guide_axis(angle = 45), position = "bottom") +
  facet_grid(
    rows = vars(Domain),
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  labs(
    title = "Country-level governance and permission indicators",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Countries ordered left to right by ascending GenAI Governance Readiness score.\n",
      "A horizontal divider separates formal governance indicators from use permission indicators."
    )
  ) +
  theme_pub(base_size = 11) +
  theme(
    strip.placement   = "outside",
    strip.text.y.left = element_text(angle = 90, face = "bold", size = 10),
    panel.spacing     = grid::unit(0, "lines"),
    legend.key.width  = grid::unit(1.5, "cm"),
    legend.position   = "right"
  )

save_plot(fig1, "Figure1_heatmap", width = 14, height = 7, dpi = 400)

# ============================================================
# Shared panels
# ============================================================

# Pooled readiness distribution
dist_df <- plot_df_pool |>
  dplyr::count(readiness, wt = TOTWGTC, name = "wt_n") |>
  dplyr::mutate(
    pct = 100 * wt_n / sum(wt_n),
    readiness_lab = factor(readiness, levels = 0:4, labels = paste0("Readiness ", 0:4))
  )

p_readiness_dist <- ggplot(dist_df, aes(x = readiness_lab, y = pct, fill = readiness_lab)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    vjust = -0.35,
    size = 4.0,
    colour = "#1F2937"
  ) +
  scale_fill_manual(values = c("#DCEAF7", "#B9D6F2", "#8EC1EA", "#5EA7DA", "#2C7FB8")) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Pooled readiness distribution",
    subtitle = "Most schools are concentrated at the lowest readiness levels",
    x = NULL,
    y = "Weighted share of schools"
  ) +
  theme_panel() +
  theme(legend.position = "none")

# Permission regime by readiness
perm_regime_df <- plot_df_pool |>
  dplyr::filter(!is.na(perm_regime)) |>
  dplyr::count(readiness, readiness_lab, perm_regime, wt = TOTWGTC, name = "wt_n") |>
  dplyr::group_by(readiness, readiness_lab) |>
  dplyr::mutate(pct = 100 * wt_n / sum(wt_n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    perm_regime = factor(
      perm_regime,
      levels = c("Neither allowed", "Teachers only", "Students only", "Both allowed")
    )
  )

p_perm_regime <- ggplot(perm_regime_df, aes(x = readiness_lab, y = pct, fill = perm_regime)) +
  geom_col(width = 0.76, colour = "white", linewidth = 0.7) +
  geom_text(
    data = subset(perm_regime_df, pct >= 8),
    aes(label = paste0(round(pct), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.8,
    colour = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Neither allowed" = col_neither,
    "Teachers only"   = col_tonly,
    "Students only"   = col_sonly,
    "Both allowed"    = col_both
  )) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Permission regime by readiness",
    subtitle = "Low readiness is dominated by neither allowed; high readiness by both allowed",
    x = NULL,
    y = "Weighted share of schools",
    fill = NULL
  ) +
  theme_panel() +
  theme(legend.position = "top")

# Pooled permission gradient
allow_t <- svy_by_readiness("teach_allowed_num", des_pool, "Teachers")
allow_s <- svy_by_readiness("stud_allowed_num",  des_pool, "Students")
allow_df <- dplyr::bind_rows(allow_t, allow_s)

tmp_figureA_panel <- ggplot(allow_df, aes(x = readiness, y = est, color = series, fill = series)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 1.6, show.legend = FALSE) +
  geom_point(size = 3.6, show.legend = FALSE) +
  annotate(
    "segment",
    x = 4.02, xend = 4.26, y = 100.0, yend = 100.0,
    colour = col_teacher, linewidth = 0.9,
    arrow = arrow(length = unit(0.10, "inches"), type = "closed")
  ) +
  annotate(
    "segment",
    x = 4.02, xend = 4.26, y = 97.9, yend = 97.9,
    colour = col_student, linewidth = 0.9,
    arrow = arrow(length = unit(0.10, "inches"), type = "closed")
  ) +
  annotate(
    "text",
    x = 4.30, y = 100.0,
    label = "Teachers 100%",
    hjust = 0, vjust = 0.5,
    size = 4.0, colour = col_teacher, fontface = "bold"
  ) +
  annotate(
    "text",
    x = 4.30, y = 97.9,
    label = "Students 97.9%",
    hjust = 0, vjust = 0.5,
    size = 4.0, colour = col_student, fontface = "bold"
  ) +
  scale_color_manual(values = c("Teachers" = col_teacher, "Students" = col_student)) +
  scale_fill_manual(values  = c("Teachers" = col_teacher, "Students" = col_student)) +
  scale_x_continuous(
    breaks = 0:4,
    limits = c(0, 4.95),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Permission rises sharply with governance readiness",
    subtitle = "Higher-readiness schools are much more likely to allow GenAI use",
    x = "Governance readiness score",
    y = "Schools allowing GenAI in any form"
  ) +
  coord_cartesian(clip = "off") +
  theme_panel() +
  theme(
    legend.position = "none",
    plot.margin = margin(8, 90, 8, 8)
  )

save_plot(tmp_figureA_panel, "FigureA_pooled_dose_response", width = 14, height = 8.5, dpi = 400)

# Restrictions among allowed schools
restrict_df <- dplyr::bind_rows(
  svy_by_readiness("teach_restrict_num", subset(des_pool, teach_allowed_num == 1), "Teachers"),
  svy_by_readiness("stud_restrict_num",  subset(des_pool, stud_allowed_num == 1), "Students")
)

restrict_labels <- restrict_df |>
  dplyr::group_by(series) |>
  dplyr::filter(readiness == max(readiness)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    x_lab = readiness + 0.16,
    y_lab = dplyr::case_when(
      series == "Students" ~ 92.9,
      series == "Teachers" ~ 85.1,
      TRUE ~ est
    )
  )

p_restrict <- ggplot(restrict_df, aes(x = readiness, y = est, color = series, fill = series)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 1.5, show.legend = FALSE) +
  geom_point(size = 3.4, show.legend = FALSE) +
  geom_text(
    data = restrict_labels,
    aes(x = x_lab, y = y_lab, label = paste0(series, "  ", round(est, 1), "%")),
    hjust = 0,
    size = 3.9,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Teachers" = col_teacher, "Students" = col_student)) +
  scale_fill_manual(values  = c("Teachers" = col_teacher, "Students" = col_student)) +
  scale_x_continuous(
    breaks = 0:4,
    limits = c(0, 4.68),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Restrictions among schools that allow GenAI",
    subtitle = "Restrictions are consistently more frequent for students",
    x = "Governance readiness score",
    y = "Allowed schools using explicit restrictions"
  ) +
  coord_cartesian(clip = "off") +
  theme_panel() +
  theme(legend.position = "none")

save_plot(p_restrict, "Figure2_restrictions_among_allowed", width = 12.5, height = 8.0, dpi = 400)

# Pooled impacts by readiness
impact_df <- dplyr::bind_rows(
  svy_by_readiness("P_CIMPOS", des_pool, "Positive impacts", pct = FALSE),
  svy_by_readiness("P_CONEG",  des_pool, "Negative impacts", pct = FALSE)
)

impact_labels <- impact_df |>
  dplyr::group_by(series) |>
  dplyr::filter(readiness == max(readiness)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    x_lab = readiness + 0.16,
    y_lab = dplyr::case_when(
      series == "Positive impacts" ~ 55.9,
      series == "Negative impacts" ~ 41.2,
      TRUE ~ est
    )
  )

figureC <- ggplot(impact_df, aes(x = readiness, y = est, color = series, fill = series)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 1.5, show.legend = FALSE) +
  geom_point(size = 3.4, show.legend = FALSE) +
  geom_text(
    data = impact_labels,
    aes(x = x_lab, y = y_lab, label = series),
    hjust = 0,
    size = 4.0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Positive impacts" = col_pos, "Negative impacts" = col_neg)) +
  scale_fill_manual(values  = c("Positive impacts" = col_pos, "Negative impacts" = col_neg)) +
  scale_x_continuous(
    breaks = 0:4,
    limits = c(0, 4.72),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Higher-readiness schools tend to view GenAI more positively",
    subtitle = "Descriptive pooled school-level means",
    x = "Governance readiness score",
    y = "Mean IEA scale score"
  ) +
  coord_cartesian(clip = "off") +
  theme_panel() +
  theme(legend.position = "none")

save_plot(figureC, "FigureC_pooled_impacts_by_readiness", width = 14, height = 8.5, dpi = 400)

# ============================================================
# FIGURE 2
# Country-level allowance/restriction + restrictions among allowed
# ============================================================
fig2_country_df <- tab1 |>
  dplyr::filter(indicator %in% c("teach_allowed", "teach_restrict",
                                 "stud_allowed", "stud_restrict")) |>
  dplyr::mutate(
    Panel   = if_else(stringr::str_starts(indicator, "teach"), "Teachers", "Students"),
    Type    = if_else(stringr::str_ends(indicator, "restrict"),
                      "Allowed, with restrictions", "Allowed (any form)"),
    Country = factor(Country, levels = readiness_order)
  )

fig2A <- ggplot(fig2_country_df, aes(x = est, y = Country, shape = Type, colour = Type)) +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.3,
    linewidth = 0.5,
    colour = "grey65",
    na.rm = TRUE
  ) +
  geom_point(size = 3.2) +
  facet_wrap(~ Panel) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    name   = "Weighted percentage of schools"
  ) +
  scale_shape_manual(
    values = c("Allowed (any form)" = 19, "Allowed, with restrictions" = 17)
  ) +
  scale_colour_manual(
    values = c("Allowed (any form)" = "#1a5276", "Allowed, with restrictions" = "#c0392b")
  ) +
  labs(
    title = "Country-level allowance and restriction",
    subtitle = "Countries ordered by ascending governance readiness",
    y = NULL
  ) +
  theme_pub(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.key.size = grid::unit(0.8, "lines")
  )

fig2 <- (
  fig2A /
    p_restrict
) +
  plot_layout(heights = c(1.05, 0.95)) +
  plot_annotation(
    title = "Allowance, restriction, and governance gradients in GenAI use",
    subtitle = "Country-level estimates above and pooled restriction patterns below",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, color = "#111827"),
      plot.subtitle = element_text(size = 11, color = "#4B5563"),
      plot.tag = element_text(face = "bold", size = 15, color = "#111827")
    )
  )

save_plot(fig2, "Figure2_allowed_restricted", width = 14, height = 14, dpi = 400)

# ============================================================
# FIGURE 3
# Permission regime + pooled permission gradient + country gap
# ============================================================
permission_gap_country_df <- tab1 |>
  dplyr::filter(indicator %in% c("teach_allowed", "stud_allowed")) |>
  dplyr::group_by(CNTRY3, Country) |>
  dplyr::summarise(use_score = mean(est, na.rm = TRUE), .groups = "drop")

gov_df <- tab1 |>
  dplyr::filter(indicator == "readiness_0_4") |>
  dplyr::select(CNTRY3, gov_score_raw = est) |>
  dplyr::mutate(gov_score = (gov_score_raw / 4) * 100)

fig3_df <- permission_gap_country_df |>
  dplyr::left_join(gov_df, by = "CNTRY3") |>
  dplyr::mutate(gap = use_score - gov_score) |>
  dplyr::arrange(gap) |>
  dplyr::mutate(Country = factor(Country, levels = Country))

fig3_long <- fig3_df |>
  tidyr::pivot_longer(
    cols = c(gov_score, use_score),
    names_to = "Measure",
    values_to = "Value"
  ) |>
  dplyr::mutate(
    Measure = dplyr::recode(
      Measure,
      gov_score = "Governance readiness (rescaled to %)",
      use_score = "Use permission (mean of teachers and students)"
    )
  )

fig3_gap <- ggplot(fig3_df) +
  geom_segment(
    aes(x = gov_score, xend = use_score, y = Country, yend = Country),
    colour = "grey65",
    linewidth = 1.1
  ) +
  geom_point(
    data = fig3_long,
    aes(x = Value, y = Country, colour = Measure, shape = Measure),
    size = 4
  ) +
  geom_text(
    aes(
      x = (gov_score + use_score) / 2,
      y = Country,
      label = paste0("Gap ", sprintf("%.1f", gap), " pp")
    ),
    vjust = -0.65,
    size = 3.0,
    colour = "grey30"
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0.01, 0.05)),
    name   = "Percentage of schools (%)"
  ) +
  scale_colour_manual(
    values = c(
      "Governance readiness (rescaled to %)"           = "#1a5276",
      "Use permission (mean of teachers and students)" = "#c0392b"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Governance readiness (rescaled to %)"           = 19,
      "Use permission (mean of teachers and students)" = 17
    )
  ) +
  labs(
    title = "Country-level governance-permission gap",
    subtitle = "Permission exceeds formal governance across countries",
    y = NULL,
    caption = NULL
  ) +
  theme_pub(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.key.size = grid::unit(0.8, "lines"),
    panel.grid.major.y = element_blank()
  )

Figure3_permission_governance <- (
  (p_perm_regime | tmp_figureA_panel) /
    fig3_gap
) +
  plot_layout(heights = c(0.95, 1.10)) +
  plot_annotation(
    title = "Permission, governance, and readiness gradients in GenAI use",
    subtitle = "Pooled school-level patterns above and country-level governance-permission gaps below",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, color = "#111827"),
      plot.subtitle = element_text(size = 11, color = "#4B5563"),
      plot.tag = element_text(face = "bold", size = 15, color = "#111827")
    )
  )

save_plot(fig3_gap, "Figure3_permission_governance_gap", width = 10, height = 6.5, dpi = 300)
save_plot(Figure3_permission_governance, "Figure3_permission_governance", width = 15.8, height = 14, dpi = 400)

# ============================================================
# FIGURE 4
# Pooled readiness distribution + country readiness means
# ============================================================
fig4_df <- tab1 |>
  dplyr::filter(indicator == "readiness_0_4") |>
  dplyr::mutate(
    Country = factor(Country, levels = readiness_order),
    label_x = pmin(hi + 0.06, 3.95)
  )

fig4_country <- ggplot(fig4_df, aes(x = est, y = Country)) +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.25,
    colour = "grey55",
    na.rm = TRUE
  ) +
  geom_point(size = 3.5, colour = "#1a5276") +
  geom_text(
    aes(x = label_x, label = sprintf("%.2f", est)),
    hjust  = 0,
    size   = 3.0,
    colour = "grey30"
  ) +
  scale_x_continuous(
    limits = c(0, 4),
    breaks = 0:4,
    expand = expansion(mult = c(0.01, 0.16)),
    name   = "Mean Governance Readiness score (0-4)"
  ) +
  labs(
    title = "Country mean governance readiness",
    subtitle = "Error bars show 95% confidence intervals",
    y = NULL,
    caption = NULL
  ) +
  theme_pub(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    plot.margin = margin(5.5, 20, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

Figure4_readiness <- (
  p_readiness_dist | fig4_country
) +
  plot_layout(widths = c(0.90, 1.10)) +
  plot_annotation(
    title = "Pooled and country-level readiness distributions",
    subtitle = "Overall distribution at left and country means at right",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, color = "#111827"),
      plot.subtitle = element_text(size = 11, color = "#4B5563"),
      plot.tag = element_text(face = "bold", size = 15, color = "#111827")
    )
  )

save_plot(fig4_country, "Figure4_readiness_mean_CI", width = 8, height = 6, dpi = 300)
save_plot(Figure4_readiness, "Figure4_readiness", width = 14, height = 7.5, dpi = 400)

# ============================================================
# FIGURE 5
# Country-level associations above and pooled impacts below
# ============================================================
safe_mean_cntry <- function(var) {
  purrr::map_dfr(genai_cntry, function(cc) {
    dsub <- subset(des_rep, CNTRY3 == cc)
    res  <- tryCatch({
      f   <- as.formula(paste0("~I(as.numeric(", var, "))"))
      est <- survey::svymean(f, dsub, na.rm = TRUE)
      m   <- as.numeric(coef(est))[1]
      se  <- as.numeric(survey::SE(est))[1]
      if (!is.na(se) && is.finite(se) && se > 0) {
        tibble::tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
      } else {
        tibble::tibble(mean = m, lo = NA_real_, hi = NA_real_)
      }
    }, error = function(e) {
      tibble::tibble(mean = NA_real_, lo = NA_real_, hi = NA_real_)
    })
    tibble::tibble(CNTRY3 = cc, !!var := res$mean)
  })
}

imp_pos_country <- safe_mean_cntry("P_CIMPOS")
imp_neg_country <- safe_mean_cntry("P_CONEG")

fig5_df <- fig4_df |>
  dplyr::select(CNTRY3, Country, mean_ready = est, school_n = n_nonmiss) |>
  dplyr::left_join(imp_pos_country, by = "CNTRY3") |>
  dplyr::left_join(imp_neg_country, by = "CNTRY3") |>
  dplyr::filter(!is.na(mean_ready), !is.na(P_CIMPOS), !is.na(P_CONEG))

wr_pos <- cov.wt(
  fig5_df[, c("mean_ready", "P_CIMPOS")],
  wt = fig5_df$school_n,
  cor = TRUE
)$cor[1, 2]

wr_neg <- cov.wt(
  fig5_df[, c("mean_ready", "P_CONEG")],
  wt = fig5_df$school_n,
  cor = TRUE
)$cor[1, 2]

med_ready <- median(fig5_df$mean_ready)
med_pos   <- median(fig5_df$P_CIMPOS)
med_neg   <- median(fig5_df$P_CONEG)

scatter_theme <- theme_pub(base_size = 12) +
  theme(
    panel.grid.major = element_line(colour = "grey92"),
    plot.tag = element_text(face = "bold", size = 12.5),
    panel.border = element_rect(colour = "grey45", linewidth = 1.25, fill = NA),
    axis.line = element_blank()
  )

p5a <- ggplot(fig5_df, aes(x = mean_ready, y = P_CIMPOS, label = Country)) +
  geom_vline(xintercept = med_ready, linetype = "dashed", colour = "grey55", linewidth = 0.75) +
  geom_hline(yintercept = med_pos,   linetype = "dashed", colour = "grey55", linewidth = 0.75) +
  geom_point(size = 2.8, colour = "#1a5276") +
  ggrepel::geom_text_repel(
    size = 4.2,
    colour = "grey20",
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.20,
    segment.colour = "grey20",
    segment.size = 0.40
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = sprintf("weighted r = %.2f", wr_pos),
    hjust = 1.05, vjust = -0.6,
    size = 3.7, colour = "grey20"
  ) +
  scale_x_continuous(name = "Mean Governance Readiness score (0-4)") +
  scale_y_continuous(name = "Mean perceived positive impacts (P_CIMPOS)") +
  labs(tag = "A") +
  scatter_theme

p5b <- ggplot(fig5_df, aes(x = mean_ready, y = P_CONEG, label = Country)) +
  geom_vline(xintercept = med_ready, linetype = "dashed", colour = "grey55", linewidth = 0.75) +
  geom_hline(yintercept = med_neg,   linetype = "dashed", colour = "grey55", linewidth = 0.75) +
  geom_point(size = 2.8, colour = "#c0392b") +
  ggrepel::geom_text_repel(
    size = 4.2,
    colour = "grey20",
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.20,
    segment.colour = "grey20",
    segment.size = 0.40
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = sprintf("weighted r = %.2f", wr_neg),
    hjust = 1.05, vjust = -0.6,
    size = 3.7, colour = "grey20"
  ) +
  scale_x_continuous(name = "Mean Governance Readiness score (0-4)") +
  scale_y_continuous(name = "Mean perceived negative impacts (P_CONEG)") +
  labs(tag = "B") +
  scatter_theme

fig5_country_inner <- patchwork::wrap_plots(p5a, p5b, ncol = 1) +
  patchwork::plot_annotation(
    caption = paste0(
      "P_CIMPOS and P_CONEG are IEA composite scales with mean approximately 50 and SD approximately 10.\n",
      "Weighted r is a descriptive Pearson correlation across 12 country means, weighted by country school N.\n",
      "Dashed lines indicate sample medians."
    ),
    theme = theme(
      plot.caption = element_text(size = 9.5, colour = "grey35", hjust = 0)
    )
  )

save_plot(fig5_country_inner, "Figure5_readiness_vs_impacts", width = 9, height = 12, dpi = 300)

fig5_country_panel <- patchwork::wrap_elements(full = fig5_country_inner)

figureC_small <- figureC +
  labs(
    title = "C. Pooled school-level perceptions by readiness",
    subtitle = "Descriptive pooled school-level means",
    caption = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, color = "#111827"),
    plot.subtitle = element_text(size = 10.8, color = "#4B5563"),
    axis.text = element_text(size = 11.5),
    axis.title = element_text(size = 12.5)
  )

figureC_small <- patchwork::wrap_elements(full = figureC_small)

fig5_bottom_centered <- (
  patchwork::plot_spacer() | figureC_small | patchwork::plot_spacer()
) +
  patchwork::plot_layout(widths = c(0.10, 0.80, 0.10))

Figure5_readiness_impacts <- (
  fig5_country_panel /
    fig5_bottom_centered
) +
  patchwork::plot_layout(ncol = 1, heights = c(1.85, 0.65)) +
  patchwork::plot_annotation(
    title = "Governance readiness and perceived impacts of GenAI",
    subtitle = "Country-level associations above and pooled school-level gradients below",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, color = "#111827"),
      plot.subtitle = element_text(size = 11, color = "#4B5563")
    )
  )

save_plot(Figure5_readiness_impacts, "Figure5_readiness_impacts", width = 13, height = 18.5, dpi = 400)

# -----------------------------
# Print figures
# -----------------------------
print(fig1)
print(fig2)
print(Figure3_permission_governance)
print(Figure4_readiness)
print(Figure5_readiness_impacts)

# -----------------------------
# Final message
# -----------------------------
message("\nDONE. Outputs saved to: ", out_dir)
message(
  "Files: ",
  paste(
    c(
      "Table1_corrected.csv",
      "Figure1_heatmap",
      "Figure2_allowed_restricted",
      "Figure2_restrictions_among_allowed",
      "Figure3_permission_governance",
      "Figure3_permission_governance_gap",
      "Figure4_readiness",
      "Figure4_readiness_mean_CI",
      "Figure5_readiness_vs_impacts",
      "Figure5_readiness_impacts",
      "FigureA_pooled_dose_response",
      "FigureC_pooled_impacts_by_readiness"
    ),
    collapse = " + "
  )
)