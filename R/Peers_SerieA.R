########## Serie A
library(dplyr)
club_profiles <- sa %>%
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

### AC Milan
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

acm_peers <- peers %>%
  filter(squad == "AC Milan") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(acm_peers$peer_1, acm_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

milan_series <- sa38 %>%
  filter(squad == "AC Milan") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

milan_vs_peers <- milan_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  milan_vs_peers[[paste0(m, "_gap")]] <-
    milan_vs_peers[[m]] - milan_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- milan_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(milan_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("AC Milan vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (AC Milan - peer mean)")
plot_gap("trophies_gap", "Trophies gap (AC Milan - peer mean)")



milan_vs_peers_post2023 <- milan_vs_peers %>%
  filter(end_year >= 2023)

milan_vs_peers_pre2023 <- milan_vs_peers %>%
  filter(end_year < 2023)


gap_tests_post2023 <- map_dfr(gap_cols, \(gc) {
  x <- milan_vs_peers_post2023[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2022",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2023 <- map_dfr(gap_cols, \(gc) {
  x <- milan_vs_peers_pre2023[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2022",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


milan_gap_tests_comparison <- bind_rows(gap_tests_pre2023, gap_tests_post2023)
milan_gap_tests_comparison

did_data <- bind_rows(
  milan_series %>% mutate(treated = 1),
  sa38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2023, 1, 0))

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


### Atalanta
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

atalanta_peers <- peers %>%
  filter(squad == "Atalanta") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(atalanta_peers$peer_1, atalanta_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

atalanta_series <- sa38 %>%
  filter(squad == "Atalanta") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

atalanta_vs_peers <- atalanta_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  atalanta_vs_peers[[paste0(m, "_gap")]] <-
    atalanta_vs_peers[[m]] - atalanta_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- atalanta_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(atalanta_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Atalanta vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Atalanta - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Atalanta - peer mean)")




atalanta_vs_peers_post2023 <- atalanta_vs_peers %>%
  filter(end_year >= 2023)

atalanta_vs_peers_pre2023 <- atalanta_vs_peers %>%
  filter(end_year < 2023)


gap_tests_post2023 <- map_dfr(gap_cols, \(gc) {
  x <- atalanta_vs_peers_post2023[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2022",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2023 <- map_dfr(gap_cols, \(gc) {
  x <- atalanta_vs_peers_pre2023[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2022",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


atalanta_gap_tests_comparison <- bind_rows(gap_tests_pre2023, gap_tests_post2023)
atalanta_gap_tests_comparison

did_data <- bind_rows(
  atalanta_series %>% mutate(treated = 1),
  sa38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2023, 1, 0))

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



### Bologna
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

bologna_peers <- peers %>%
  filter(squad == "Bologna") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(bologna_peers$peer_1, bologna_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

bologna_series <- sa38 %>%
  filter(squad == "Bologna") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

bologna_vs_peers <- bologna_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  bologna_vs_peers[[paste0(m, "_gap")]] <-
    bologna_vs_peers[[m]] - bologna_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- bologna_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(bologna_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2015, y = Inf),
              label = "MCO Entry 2015",
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
    labs(x = "Season", y = ylab,
         title = paste0("Bologna vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Bologna - peer mean)")
plot_gap("w_gap", "Wins gap (Bologna - peer mean)")




bologna_vs_peers_post2016 <- bologna_vs_peers %>%
  filter(end_year >= 2016)

bologna_vs_peers_pre2016 <- bologna_vs_peers %>%
  filter(end_year < 2016)


gap_tests_post2016 <- map_dfr(gap_cols, \(gc) {
  x <- bologna_vs_peers_post2016[[gc]]
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
  x <- bologna_vs_peers_pre2016[[gc]]
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


bologna_gap_tests_comparison <- bind_rows(gap_tests_pre2016, gap_tests_post2016)
bologna_gap_tests_comparison

did_data <- bind_rows(
  bologna_series %>% mutate(treated = 1),
  sa38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2016, 1, 0))

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



### Fiorentina
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Fiorentina_peers <- peers %>%
  filter(squad == "Fiorentina") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Fiorentina_peers$peer_1, Fiorentina_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Fiorentina_series <- sa38 %>%
  filter(squad == "Fiorentina") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Fiorentina_vs_peers <- Fiorentina_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Fiorentina_vs_peers[[paste0(m, "_gap")]] <-
    Fiorentina_vs_peers[[m]] - Fiorentina_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Fiorentina_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Fiorentina_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Fiorentina vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Fiorentina - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Fiorentina - peer mean)")




Fiorentina_vs_peers_post2020 <- Fiorentina_vs_peers %>%
  filter(end_year >= 2020)

Fiorentina_vs_peers_pre2020 <- Fiorentina_vs_peers %>%
  filter(end_year < 2020)


gap_tests_post2020 <- map_dfr(gap_cols, \(gc) {
  x <- Fiorentina_vs_peers_post2020[[gc]]
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
  x <- Fiorentina_vs_peers_pre2020[[gc]]
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


Fiorentina_gap_tests_comparison <- bind_rows(gap_tests_pre2020, gap_tests_post2020)
Fiorentina_gap_tests_comparison

did_data <- bind_rows(
  Fiorentina_series %>% mutate(treated = 1),
  sa38 %>% 
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


### HellasVerona
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

HellasVerona_peers <- peers %>%
  filter(squad == "Hellas Verona") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(HellasVerona_peers$peer_1, HellasVerona_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

HellasVerona_series <- sa38 %>%
  filter(squad == "Hellas Verona") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

HellasVerona_vs_peers <- HellasVerona_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  HellasVerona_vs_peers[[paste0(m, "_gap")]] <-
    HellasVerona_vs_peers[[m]] - HellasVerona_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- HellasVerona_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(HellasVerona_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Hellas Verona vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Hellas Verona - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Hellas Verona - peer mean)")




HellasVerona_vs_peers_post2019 <- HellasVerona_vs_peers %>%
  filter(end_year >= 2019)

HellasVerona_vs_peers_pre2019 <- HellasVerona_vs_peers %>%
  filter(end_year < 2019)


gap_tests_post2019 <- map_dfr(gap_cols, \(gc) {
  x <- HellasVerona_vs_peers_post2019[[gc]]
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
  x <- HellasVerona_vs_peers_pre2019[[gc]]
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


HellasVerona_gap_tests_comparison <- bind_rows(gap_tests_pre2019, gap_tests_post2019)
HellasVerona_gap_tests_comparison

did_data <- bind_rows(
  HellasVerona_series %>% mutate(treated = 1),
  sa38 %>% 
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



### Napoli
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Napoli_peers <- peers %>%
  filter(squad == "Napoli") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Napoli_peers$peer_1, Napoli_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Napoli_series <- sa38 %>%
  filter(squad == "Napoli") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Napoli_vs_peers <- Napoli_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Napoli_vs_peers[[paste0(m, "_gap")]] <-
    Napoli_vs_peers[[m]] - Napoli_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Napoli_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Napoli_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Napoli vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Napoli - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Napoli - peer mean)")




Napoli_vs_peers_post2019 <- Napoli_vs_peers %>%
  filter(end_year >= 2019)

Napoli_vs_peers_pre2019 <- Napoli_vs_peers %>%
  filter(end_year < 2019)


gap_tests_post2019 <- map_dfr(gap_cols, \(gc) {
  x <- Napoli_vs_peers_post2019[[gc]]
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
  x <- Napoli_vs_peers_pre2019[[gc]]
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


Napoli_gap_tests_comparison <- bind_rows(gap_tests_pre2019, gap_tests_post2019)
Napoli_gap_tests_comparison

did_data <- bind_rows(
  Napoli_series %>% mutate(treated = 1),
  sa38 %>% 
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



### Udinese
sa38 <- sa %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

Udinese_peers <- peers %>%
  filter(squad == "Udinese") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(Udinese_peers$peer_1, Udinese_peers$peer_2)

metrics <- c("rk", "p", "w", "g_diff", "pts", "pts_game", "trophies")

Udinese_series <- sa38 %>%
  filter(squad == "Udinese") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- sa38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

Udinese_vs_peers <- Udinese_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  Udinese_vs_peers[[paste0(m, "_gap")]] <-
    Udinese_vs_peers[[m]] - Udinese_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- Udinese_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(Udinese_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Udinese vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Udinese - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Udinese - peer mean)")




Udinese_vs_peers_post2017 <- Udinese_vs_peers %>%
  filter(end_year >= 2017)

Udinese_vs_peers_pre2017 <- Udinese_vs_peers %>%
  filter(end_year < 2017)


gap_tests_post2017 <- map_dfr(gap_cols, \(gc) {
  x <- Udinese_vs_peers_post2017[[gc]]
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
  x <- Udinese_vs_peers_pre2017[[gc]]
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


Udinese_gap_tests_comparison <- bind_rows(gap_tests_pre2017, gap_tests_post2017)
Udinese_gap_tests_comparison

did_data <- bind_rows(
  Udinese_series %>% mutate(treated = 1),
  sa38 %>% 
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


