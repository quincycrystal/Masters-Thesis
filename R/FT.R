gap_tbl <- villa_gap_tests_comparison %>%
  mutate(
    stars = p_stars(p_value),
    estimate_star = sprintf("%.3f%s", estimate, stars),
    ci = ifelse(is.na(ci_lower) | is.na(ci_upper),
                NA_character_,
                sprintf("[%.3f, %.3f]", ci_lower, ci_upper)),
    p_value = sprintf("%.4f", p_value)
  ) %>%
  select(period, metric_gap, n, estimate_star, ci, p_value)

ft_gap <- flextable(gap_tbl) %>%
  set_header_labels(
    period = "Period",
    metric_gap = "Metric (gap)",
    n = "N",
    estimate_star = "Mean gap (vs 0)",
    ci = "95% CI",
    p_value = "p-value"
  ) %>%
  autofit()

did_tbl <- did_results %>%
  mutate(
    stars = p_stars(p_value),
    did_estimate_star = sprintf("%.3f%s", did_estimate, stars),
    std_error = sprintf("%.3f", std_error),
    p_value = sprintf("%.4f", p_value)
  ) %>%
  select(metric, did_estimate_star, std_error, p_value)

ft_did <- flextable(did_tbl) %>%
  set_header_labels(
    metric = "Metric",
    did_estimate_star = "DiD (treated×post)",
    std_error = "Std. Error",
    p_value = "p-value"
  ) %>%
  autofit()

