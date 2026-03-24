did_data <- bind_rows(
  villa_series %>% mutate(treated = 1),
  ll %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2024, 1, 0))

did_results <- map_dfr(metrics, function(m) {
  model <- lm(as.formula(paste(m, "~ treated + post + treated:post")), 
              data = did_data)
  
  tibble(
    metric = m,
    did_estimate = coef(model)["treated:post"],
    std_error = summary(model)$coefficients["treated:post", "Std. Error"],
    p_value = summary(model)$coefficients["treated:post", "Pr(>|t|)"]
  )
})

did_data %>%
  group_by(treated) %>%
  summarise(
    n_clubs = n_distinct(squad),
    clubs = paste(unique(squad), collapse = ", "),
    n_observations = n()
  )

