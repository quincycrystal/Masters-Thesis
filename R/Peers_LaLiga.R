########## LaLiga
library(dplyr)
club_profiles <- ll %>%
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





### Atlético
ll38 <- ll %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Atlético_peers <- peers %>%
  filter(squad == "Atlético") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Atlético_peers$peer_1, Atlético_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Atlético_series <- ll38 %>%
  filter(squad == "Atlético") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- ll38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Atlético_vs_peers <- Atlético_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Atlético_vs_peers[[paste0(m, "_gap")]] <-
    Atlético_vs_peers[[m]] - Atlético_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Atlético_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Atlético_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2018, y = Inf),
              label = "MCO Entry 2018",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2018,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Atlético vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Atlético - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Atlético - peer mean)")




Atlético_vs_peers_post2019 <- Atlético_vs_peers %>%
  filter(end_year >= 2019)

Atlético_vs_peers_pre2019 <- Atlético_vs_peers %>%
  filter(end_year < 2019)


gap_tests_post2019 <- map_dfr(gap_cols, \(gc) {
  x <- Atlético_vs_peers_post2019[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2018",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2019 <- map_dfr(gap_cols, \(gc) {
  x <- Atlético_vs_peers_pre2019[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2018",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Atlético_gap_tests_comparison <- bind_rows(gap_tests_pre2019, gap_tests_post2019)
Atlético_gap_tests_comparison

did_data <- bind_rows(
  Atlético_series %>% mutate(treated = 1),
  ll38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2019, 1, 0))

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


### Girona
ll38 <- ll %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Girona_peers <- peers %>%
  filter(squad == "Girona") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Girona_peers$peer_1, Girona_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Girona_series <- ll38 %>%
  filter(squad == "Girona") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- ll38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Girona_vs_peers <- Girona_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Girona_vs_peers[[paste0(m, "_gap")]] <-
    Girona_vs_peers[[m]] - Girona_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Girona_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Girona_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2022, y = Inf),
              label = "MCO Entry 2022",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2022,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Girona vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Girona - peer mean)")
plot_gap("rk_gap", "Rank gap (Girona - peer mean)")




Girona_vs_peers_post2018 <- Girona_vs_peers %>%
  filter(end_year >= 2018)

Girona_vs_peers_pre2018 <- Girona_vs_peers %>%
  filter(end_year < 2018)


gap_tests_post2018 <- map_dfr(gap_cols, \(gc) {
  x <- Girona_vs_peers_post2018[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2017",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2018 <- map_dfr(gap_cols, \(gc) {
  x <- Girona_vs_peers_pre2018[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2017",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Girona_gap_tests_comparison <- bind_rows(gap_tests_pre2018, gap_tests_post2018)
Girona_gap_tests_comparison

did_data <- bind_rows(
  Girona_series %>% mutate(treated = 1),
  ll38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2018, 1, 0))

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



### Sevilla FC
ll38 <- ll %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Sevilla_FC_peers <- peers %>%
  filter(squad == "Sevilla FC") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Sevilla_FC_peers$peer_1, Sevilla_FC_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Sevilla_FC_series <- ll38 %>%
  filter(squad == "Sevilla FC") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- ll38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Sevilla_FC_vs_peers <- Sevilla_FC_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Sevilla_FC_vs_peers[[paste0(m, "_gap")]] <-
    Sevilla_FC_vs_peers[[m]] - Sevilla_FC_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Sevilla_FC_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Sevilla_FC_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Sevilla FC vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Sevilla FC - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Sevilla FC - peer mean)")




Sevilla_FC_vs_peers_post2022 <- Sevilla_FC_vs_peers %>%
  filter(end_year >= 2022)

Sevilla_FC_vs_peers_pre2022 <- Sevilla_FC_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2022 <- map_dfr(gap_cols, \(gc) {
  x <- Sevilla_FC_vs_peers_post2022[[gc]]
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
  x <- Sevilla_FC_vs_peers_pre2022[[gc]]
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


Sevilla_FC_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
Sevilla_FC_gap_tests_comparison

did_data <- bind_rows(
  Sevilla_FC_series %>% mutate(treated = 1),
  ll38 %>% 
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



### Valencia
ll38 <- ll %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Valencia_peers <- peers %>%
  filter(squad == "Valencia") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Valencia_peers$peer_1, Valencia_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Valencia_series <- ll38 %>%
  filter(squad == "Valencia") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- ll38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Valencia_vs_peers <- Valencia_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Valencia_vs_peers[[paste0(m, "_gap")]] <-
    Valencia_vs_peers[[m]] - Valencia_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Valencia_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Valencia_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2015, y = Inf),
              label = "MCO Entry 2015",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_text(aes(x = 2024, y = Inf),
              label = "MCO Exit 2024",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2015,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    geom_vline(xintercept = 2024,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("Valencia vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Valencia- peer mean)")
plot_gap("trophies_gap", "Trophies gap (Valencia - peer mean)")




Valencia_vs_peers_post2016 <- Valencia_vs_peers %>%
  filter(end_year >= 2016 & end_year <= 2024)

Valencia_vs_peers_pre2016 <- Valencia_vs_peers %>%
  filter(end_year < 2016)


gap_tests_post2016 <- map_dfr(gap_cols, \(gc) {
  x <- Valencia_vs_peers_post2016[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2015",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2016 <- map_dfr(gap_cols, \(gc) {
  x <- Valencia_vs_peers_pre2016[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2015",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Valencia_gap_tests_comparison <- bind_rows(gap_tests_pre2016, gap_tests_post2016)
Valencia_gap_tests_comparison

did_data <- bind_rows(
  Valencia_series %>% mutate(treated = 1),
  ll38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2016 & end_year <= 2024, 1, 0))

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










