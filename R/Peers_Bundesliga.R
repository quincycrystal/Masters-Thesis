########## Bundesliga
library(dplyr)
club_profiles <- bl %>%
  group_by(squad) %>%
  summarise(
    pts_per_game = sum(pts) / sum(matchday),
    avg_goals_for = mean(gf),
    avg_goals_against = mean(ga),
    total_points = sum(pts),
    total_trophies = sum(trophies),
    total_wins = sum(w),
    avg_rank = mean(rk),
    .groups = "drop"
  )

club_profiles <- club_profiles %>%
  filter(!is.na(squad)) %>%
  mutate(across(where(is.numeric), as.numeric))

club_profiles2 <- club_profiles %>%
  mutate(
    total_points_log   = log1p(total_points),
    total_trophies_log = log1p(total_trophies)
  )

features <- club_profiles2 %>%
  select(pts_per_game, avg_goals_for, avg_goals_against,
         total_points_log, total_trophies_log, total_wins) %>%
  scale()

library(FNN)

nn <- get.knn(features, k = 7)

peers <- tibble(
  squad = club_profiles2$squad,
  peer_1 = club_profiles2$squad[nn$nn.index[, 1]],
  peer_2 = club_profiles2$squad[nn$nn.index[, 2]],
  peer_3 = club_profiles2$squad[nn$nn.index[, 3]],
  peer_4 = club_profiles2$squad[nn$nn.index[, 4]],
  peer_5 = club_profiles2$squad[nn$nn.index[, 5]],
  peer_6 = club_profiles2$squad[nn$nn.index[, 6]],
  peer_7 = club_profiles2$squad[nn$nn.index[, 7]],
  dist_1 = nn$nn.dist[, 1],
  dist_2 = nn$nn.dist[, 2],
  dist_3 = nn$nn.dist[, 3],
  dist_4 = nn$nn.dist[, 4],
  dist_5 = nn$nn.dist[, 5],
  dist_6 = nn$nn.dist[, 6],
  dist_7 = nn$nn.dist[, 7]
)





### Augsburg
bl34 <- bl %>%
  filter(matchday == 34, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Augsburg_peers <- peers %>%
  filter(squad == "Augsburg") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Augsburg_peers$peer_1, Augsburg_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Augsburg_series <- bl34 %>%
  filter(squad == "Augsburg") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- bl34 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Augsburg_vs_peers <- Augsburg_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Augsburg_vs_peers[[paste0(m, "_gap")]] <-
    Augsburg_vs_peers[[m]] - Augsburg_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Augsburg_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Augsburg_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2021, y = Inf),
              label = "MCO Entry 2021",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2021,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Augsburg vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Augsburg - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Augsburg - peer mean)")




Augsburg_vs_peers_post2022 <- Augsburg_vs_peers %>%
  filter(end_year >= 2022)

Augsburg_vs_peers_pre2022 <- Augsburg_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2022 <- map_dfr(gap_cols, \(gc) {
  x <- Augsburg_vs_peers_post2022[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2021",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2022 <- map_dfr(gap_cols, \(gc) {
  x <- Augsburg_vs_peers_pre2022[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2021",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Augsburg_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
Augsburg_gap_tests_comparison

did_data <- bind_rows(
  Augsburg_series %>% mutate(treated = 1),
  bl34 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2022, 1, 0))

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


### Leipzig
bl34 <- bl %>%
  filter(matchday == 34, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Leipzig_peers <- peers %>%
  filter(squad == "Leipzig") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Leipzig_peers$peer_1, Leipzig_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Leipzig_series <- bl34 %>%
  filter(squad == "Leipzig") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- bl34 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Leipzig_vs_peers <- Leipzig_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Leipzig_vs_peers[[paste0(m, "_gap")]] <-
    Leipzig_vs_peers[[m]] - Leipzig_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Leipzig_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Leipzig_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2016, y = Inf),
              label = "MCO Entry 2016",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2016,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Leipzig vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Leipzig - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Leipzig - peer mean)")




Leipzig_vs_peers_post2017 <- Leipzig_vs_peers %>%
  filter(end_year >= 2017)

Leipzig_vs_peers_pre2017 <- Leipzig_vs_peers %>%
  filter(end_year < 2017)


gap_tests_post2017 <- map_dfr(gap_cols, \(gc) {
  x <- Leipzig_vs_peers_post2017[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2016",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2017 <- map_dfr(gap_cols, \(gc) {
  x <- Leipzig_vs_peers_pre2017[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2016",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Leipzig_gap_tests_comparison <- bind_rows(gap_tests_pre2017, gap_tests_post2017)
Leipzig_gap_tests_comparison

did_data <- bind_rows(
  Leipzig_series %>% mutate(treated = 1),
  bl34 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2017, 1, 0))

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
