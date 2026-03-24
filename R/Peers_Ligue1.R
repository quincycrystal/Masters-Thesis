########## Ligue1
library(dplyr)
club_profiles <- l1 %>%
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




### FC Lorient

Lorient_peers <- peers %>%
  filter(squad == "FC Lorient") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Lorient_peers$peer_1, Lorient_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Lorient_series <- l1 %>%
  filter(squad == "FC Lorient") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Lorient_vs_peers <- Lorient_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Lorient_vs_peers[[paste0(m, "_gap")]] <-
    Lorient_vs_peers[[m]] - Lorient_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Lorient_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Lorient_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2023, y = Inf),
              label = "MCO Entry 2023",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2023,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("Lorient vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Lorient - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Lorient - peer mean)")







Lorient_vs_peers_post2024 <- Lorient_vs_peers %>%
  filter(end_year >= 2024)

Lorient_vs_peers_pre2024 <- Lorient_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- Lorient_vs_peers_post2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2024 <- map_dfr(gap_cols, \(gc) {
  x <- Lorient_vs_peers_pre2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Lorient_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
Lorient_gap_tests_comparison

did_data <- bind_rows(
  Lorient_series %>% mutate(treated = 1),
  l1 %>% 
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



### R. Strasbourg

RCS_peers <- peers %>%
  filter(squad == "R. Strasbourg") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(RCS_peers$peer_1, RCS_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

RCS_series <- l1 %>%
  filter(squad == "R. Strasbourg") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

RCS_vs_peers <- RCS_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  RCS_vs_peers[[paste0(m, "_gap")]] <-
    RCS_vs_peers[[m]] - RCS_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- RCS_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(RCS_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2023, y = Inf),
              label = "MCO Entry 2023",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2023,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("RC Strasbourg vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (RC Strasbourg  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (RC Strasbourg  - peer mean)")







RCS_vs_peers_post2024 <- RCS_vs_peers %>%
  filter(end_year >= 2024)

RCS_vs_peers_pre2024 <- RCS_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- RCS_vs_peers_post2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2024 <- map_dfr(gap_cols, \(gc) {
  x <- RCS_vs_peers_pre2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


RCS_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
RCS_gap_tests_comparison

did_data <- bind_rows(
  RCS_series %>% mutate(treated = 1),
  l1 %>% 
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



### Monaco

Monaco_peers <- peers %>%
  filter(squad == "Monaco") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Monaco_peers$peer_1, Monaco_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Monaco_series <- l1 %>%
  filter(squad == "Monaco") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Monaco_vs_peers <- Monaco_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Monaco_vs_peers[[paste0(m, "_gap")]] <-
    Monaco_vs_peers[[m]] - Monaco_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Monaco_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Monaco_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2017, y = Inf),
              label = "MCO Entry 2017",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2017,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("Monaco vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Monaco  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Monaco  - peer mean)")







Monaco_vs_peers_post2018 <- Monaco_vs_peers %>%
  filter(end_year >= 2018)

Monaco_vs_peers_pre2018 <- Monaco_vs_peers %>%
  filter(end_year < 2018)


gap_tests_post2018 <- map_dfr(gap_cols, \(gc) {
  x <- Monaco_vs_peers_post2018[[gc]]
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
  x <- Monaco_vs_peers_pre2018[[gc]]
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


Monaco_gap_tests_comparison <- bind_rows(gap_tests_pre2018, gap_tests_post2018)
Monaco_gap_tests_comparison

did_data <- bind_rows(
  Monaco_series %>% mutate(treated = 1),
  l1 %>% 
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



### Marseille

Marseille_peers <- peers %>%
  filter(squad == "Marseille") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Marseille_peers$peer_1, Marseille_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Marseille_series <- l1 %>%
  filter(squad == "Marseille") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Marseille_vs_peers <- Marseille_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Marseille_vs_peers[[paste0(m, "_gap")]] <-
    Marseille_vs_peers[[m]] - Marseille_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Marseille_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Marseille_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season", y = ylab,
         title = paste0("Marseille vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Marseille  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Marseille  - peer mean)")







Marseille_vs_peers_post2022 <- Marseille_vs_peers %>%
  filter(end_year >= 2022)

Marseille_vs_peers_pre2022 <- Marseille_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2022 <- map_dfr(gap_cols, \(gc) {
  x <- Marseille_vs_peers_post2022[[gc]]
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
  x <- Marseille_vs_peers_pre2022[[gc]]
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


Marseille_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
Marseille_gap_tests_comparison

did_data <- bind_rows(
  Marseille_series %>% mutate(treated = 1),
  l1 %>% 
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



### Nice

Nice_peers <- peers %>%
  filter(squad == "Nice") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Nice_peers$peer_1, Nice_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Nice_series <- l1 %>%
  filter(squad == "Nice") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Nice_vs_peers <- Nice_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Nice_vs_peers[[paste0(m, "_gap")]] <-
    Nice_vs_peers[[m]] - Nice_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Nice_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Nice_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2019, y = Inf),
              label = "MCO Entry 2019",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2019,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("Nice vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Nice  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Nice  - peer mean)")







Nice_vs_peers_post2020 <- Nice_vs_peers %>%
  filter(end_year >= 2020)

Nice_vs_peers_pre2020 <- Nice_vs_peers %>%
  filter(end_year < 2020)


gap_tests_post2020 <- map_dfr(gap_cols, \(gc) {
  x <- Nice_vs_peers_post2020[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2019",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2020 <- map_dfr(gap_cols, \(gc) {
  x <- Nice_vs_peers_pre2020[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2019",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Nice_gap_tests_comparison <- bind_rows(gap_tests_pre2020, gap_tests_post2020)
Nice_gap_tests_comparison

did_data <- bind_rows(
  Nice_series %>% mutate(treated = 1),
  l1 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2020, 1, 0))

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



### Lyon

Lyon_peers <- peers %>%
  filter(squad == "Lyon") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Lyon_peers$peer_1, Lyon_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Lyon_series <- l1 %>%
  filter(squad == "Lyon") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Lyon_vs_peers <- Lyon_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Lyon_vs_peers[[paste0(m, "_gap")]] <-
    Lyon_vs_peers[[m]] - Lyon_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Lyon_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Lyon_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2023, y = Inf),
              label = "MCO Entry 2023",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2023,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("Lyon vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Lyon  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Lyon  - peer mean)")







Lyon_vs_peers_post2024 <- Lyon_vs_peers %>%
  filter(end_year >= 2024)

Lyon_vs_peers_pre2024 <- Lyon_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- Lyon_vs_peers_post2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2024 <- map_dfr(gap_cols, \(gc) {
  x <- Lyon_vs_peers_pre2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


Lyon_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
Lyon_gap_tests_comparison

did_data <- bind_rows(
  Lyon_series %>% mutate(treated = 1),
  l1 %>% 
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



### Lens

Lens_peers <- peers %>%
  filter(squad == "Lens") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Lens_peers$peer_1, Lens_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Lens_series <- l1 %>%
  filter(squad == "Lens") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Lens_vs_peers <- Lens_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Lens_vs_peers[[paste0(m, "_gap")]] <-
    Lens_vs_peers[[m]] - Lens_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Lens_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Lens_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season", y = ylab,
         title = paste0("Lens vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Lens  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Lens  - peer mean)")







Lens_vs_peers_post2017 <- Lens_vs_peers %>%
  filter(end_year >= 2017)

Lens_vs_peers_pre2017 <- Lens_vs_peers %>%
  filter(end_year < 2017)


gap_tests_post2017 <- map_dfr(gap_cols, \(gc) {
  x <- Lens_vs_peers_post2017[[gc]]
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
  x <- Lens_vs_peers_pre2017[[gc]]
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


Lens_gap_tests_comparison <- bind_rows(gap_tests_pre2017, gap_tests_post2017)
Lens_gap_tests_comparison

did_data <- bind_rows(
  Lens_series %>% mutate(treated = 1),
  l1 %>% 
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



### PSG

PSG_peers <- peers %>%
  filter(squad == "PSG") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(PSG_peers$peer_1, PSG_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

PSG_series <- l1 %>%
  filter(squad == "PSG") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

PSG_vs_peers <- PSG_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  PSG_vs_peers[[paste0(m, "_gap")]] <-
    PSG_vs_peers[[m]] - PSG_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- PSG_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(PSG_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2023, y = Inf),
              label = "MCO Entry 2023",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2023,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("PSG vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (PSG  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (PSG  - peer mean)")







PSG_vs_peers_post2024 <- PSG_vs_peers %>%
  filter(end_year >= 2024)

PSG_vs_peers_pre2024 <- PSG_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- PSG_vs_peers_post2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2024 <- map_dfr(gap_cols, \(gc) {
  x <- PSG_vs_peers_pre2024[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2023",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


PSG_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
PSG_gap_tests_comparison

did_data <- bind_rows(
  PSG_series %>% mutate(treated = 1),
  l1 %>% 
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



### Toulouse

Toulouse_peers <- peers %>%
  filter(squad == "Toulouse") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Toulouse_peers$peer_1, Toulouse_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Toulouse_series <- l1 %>%
  filter(squad == "Toulouse") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Toulouse_vs_peers <- Toulouse_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Toulouse_vs_peers[[paste0(m, "_gap")]] <-
    Toulouse_vs_peers[[m]] - Toulouse_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Toulouse_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Toulouse_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season", y = ylab,
         title = paste0("Toulouse vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Toulouse  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Toulouse  - peer mean)")







Toulouse_vs_peers_post2022 <- Toulouse_vs_peers %>%
  filter(end_year >= 2022)

Toulouse_vs_peers_pre2022 <- Toulouse_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2021 <- map_dfr(gap_cols, \(gc) {
  x <- Toulouse_vs_peers_post20212[[gc]]
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
  x <- Toulouse_vs_peers_pre2022[[gc]]
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


Toulouse_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
Toulouse_gap_tests_comparison

did_data <- bind_rows(
  Toulouse_series %>% mutate(treated = 1),
  l1 %>% 
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



### Troyes

Troyes_peers <- peers %>%
  filter(squad == "Troyes") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Troyes_peers$peer_1, Troyes_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Troyes_series <- l1 %>%
  filter(squad == "Troyes") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- l1 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Troyes_vs_peers <- Troyes_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Troyes_vs_peers[[paste0(m, "_gap")]] <-
    Troyes_vs_peers[[m]] - Troyes_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Troyes_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Troyes_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season", y = ylab,
         title = paste0("Troyes vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Troyes  - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Troyes  - peer mean)")


Troyes_vs_peers_post2022 <- Troyes_vs_peers %>%
  filter(end_year >= 2022)

Troyes_vs_peers_pre2022 <- Troyes_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2021 <- map_dfr(gap_cols, \(gc) {
  x <- Troyes_vs_peers_post2022[[gc]]
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
  x <- Troyes_vs_peers_pre2022[[gc]]
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


Troyes_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
Troyes_gap_tests_comparison

did_data <- bind_rows(
  Troyes_series %>% mutate(treated = 1),
  l1 %>% 
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


