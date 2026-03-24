########## EPL
library(dplyr)
club_profiles <- epl %>%
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

peers

setwd("/Users/sbeems/Library/CloudStorage/Dropbox/08 Universität/01 BWL/Masters Thesis/Data/LeagueTables")
write_xlsx(peers, "EPLpeers.xlsx")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
### Arsenal
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

ars_peers <- peers %>%
  filter(squad == "Arsenal") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(ars_peers$peer_1, ars_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

arsenal_series <- epl38 %>%
  filter(squad == "Arsenal") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

arsenal_vs_peers <- arsenal_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  arsenal_vs_peers[[paste0(m, "_gap")]] <-
    arsenal_vs_peers[[m]] - arsenal_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- arsenal_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(arsenal_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Arsenal vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}


plot_gap("pts_game_gap", "Points per game gap (Arsenal - peer mean)")
plot_gap("trophies_gap", "Trophies gap (Arsenal - peer mean)")


arsenal_vs_peers_post2018 <- arsenal_vs_peers %>%
  filter(end_year >= 2018)

arsenal_vs_peers_pre2018 <- arsenal_vs_peers %>%
  filter(end_year < 2018)


gap_tests_post2018 <- map_dfr(gap_cols, \(gc) {
  x <- arsenal_vs_peers_post2018[[gc]]
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
  x <- arsenal_vs_peers_pre2018[[gc]]
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


arsenal_gap_tests_comparison <- bind_rows(gap_tests_pre2018, gap_tests_post2018)
arsenal_gap_tests_comparison

did_data <- bind_rows(
  arsenal_series %>% mutate(treated = 1),
  epl38 %>% 
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






### Aston Villa
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

avl_peers <- peers %>%
  filter(squad == "Aston Villa") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(avl_peers$peer_1, avl_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

villa_series <- epl38 %>%
  filter(squad == "Aston Villa") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

villa_vs_peers <- villa_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  villa_vs_peers[[paste0(m, "_gap")]] <-
    villa_vs_peers[[m]] - villa_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- villa_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(villa_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Aston Villa vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (AVL - peer mean)")
plot_gap("trophies_gap", "Trophies gap (AVL - peer mean)")


villa_vs_peers_post2024 <- villa_vs_peers %>%
  filter(end_year >= 2024)

villa_vs_peers_pre2024 <- villa_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- villa_vs_peers_post2024[[gc]]
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
  x <- villa_vs_peers_pre2024[[gc]]
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


villa_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
villa_gap_tests_comparison

did_data <- bind_rows(
  villa_series %>% mutate(treated = 1),
  epl38 %>% 
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





### Bournemouth
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

bor_peers <- peers %>%
  filter(squad == "Bournemouth") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(bor_peers$peer_1, bor_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

bournemouth_series <- epl38 %>%
  filter(squad == "Bournemouth") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

bournemouth_vs_peers <- bournemouth_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  bournemouth_vs_peers[[paste0(m, "_gap")]] <-
    bournemouth_vs_peers[[m]] - bournemouth_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- bournemouth_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(bournemouth_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Bournemouth vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (BOR - peer mean)")
plot_gap("trophies_gap", "Trophies gap (BOR - peer mean)")





bournemouth_vs_peers_post2024 <- bournemouth_vs_peers %>%
  filter(end_year >= 2024)

bournemouth_vs_peers_pre2024 <- bournemouth_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- bournemouth_vs_peers_post2024[[gc]]
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
  x <- bournemouth_vs_peers_pre2024[[gc]]
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


bournemouth_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
bournemouth_gap_tests_comparison


did_data <- bind_rows(
  bournemouth_series %>% mutate(treated = 1),
  epl38 %>% 
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





### Brentford
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

bre_peers <- peers %>%
  filter(squad == "Brentford") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(bre_peers$peer_1, bre_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

brentford_series <- epl38 %>%
  filter(squad == "Brentford") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

brentford_vs_peers <- brentford_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  brentford_vs_peers[[paste0(m, "_gap")]] <-
    brentford_vs_peers[[m]] - brentford_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- brentford_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(brentford_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Brentford vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (BOR - peer mean)")
plot_gap("trophies_gap", "Trophies gap (BOR - peer mean)")




brentford_vs_peers_post2015 <- brentford_vs_peers %>%
  filter(end_year >= 2015 & end_year <= 2023)

brentford_vs_peers_pre2015 <- brentford_vs_peers %>%
  filter(end_year < 2015)


gap_tests_post2015 <- map_dfr(gap_cols, \(gc) {
  x <- brentford_vs_peers_post2015[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2014",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2015 <- map_dfr(gap_cols, \(gc) {
  x <- brentford_vs_peers_pre2015[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2014",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


brentford_gap_tests_comparison <- bind_rows(gap_tests_pre2015, gap_tests_post2015)
brentford_gap_tests_comparison


did_data <- bind_rows(
  brentford_series %>% mutate(treated = 1),
  epl38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2015 & end_year <= 2023, 1, 0))

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





### Brighton
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

bha_peers <- peers %>%
  filter(squad == "Brighton") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(bha_peers$peer_1, bha_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

brighton_series <- epl38 %>%
  filter(squad == "Brighton") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

brighton_vs_peers <- brighton_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  brighton_vs_peers[[paste0(m, "_gap")]] <-
    brighton_vs_peers[[m]] - brighton_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- brighton_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(brighton_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Brighton vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (BOR - peer mean)")
plot_gap("trophies_gap", "Trophies gap (BOR - peer mean)")


gap_models <- lapply(gap_cols, function(gc) {
  lm(brighton_vs_peers[[gc]] ~ 1)
})

names(gap_models) <- gap_cols

library(stargazer)

stargazer(
  gap_models,
  type = "text",     
  title = "Brighton vs Non-MCO Peers: Mean End-of-Season Gaps",
  dep.var.labels = gap_cols,
  covariate.labels = "Constant (Mean Gap)",
  omit.stat = c("f", "ser"),
  digits = 3
)





brighton_vs_peers_post2019 <- brighton_vs_peers %>%
  filter(end_year >= 2019)

brighton_vs_peers_pre2019 <- brighton_vs_peers %>%
  filter(end_year < 2019)


gap_tests_post2019 <- map_dfr(gap_cols, \(gc) {
  x <- brighton_vs_peers_post2019[[gc]]
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
  x <- brighton_vs_peers_pre2019[[gc]]
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


brighton_gap_tests_comparison <- bind_rows(gap_tests_pre2019, gap_tests_post2019)
brighton_gap_tests_comparison


did_data <- bind_rows(
  brighton_series %>% mutate(treated = 1),
  epl38 %>% 
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





### Chelsea
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

che_peers <- peers %>%
  filter(squad == "Chelsea") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(che_peers$peer_1, che_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

chelsea_series <- epl38 %>%
  filter(squad == "Chelsea") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

chelsea_vs_peers <- chelsea_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  chelsea_vs_peers[[paste0(m, "_gap")]] <-
    chelsea_vs_peers[[m]] - chelsea_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- chelsea_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(chelsea_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Chelsea vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

p1 <- plot_gap("rk_gap", "Rank gap (CHE - peer mean)")
p2 <- plot_gap("trophies_gap", "Trophy gap (CHE - peer mean)")


combined <- grid.arrange(p1, p2, ncol = 2)


chelsea_vs_peers_post2024 <- chelsea_vs_peers %>%
  filter(end_year >= 2024)

chelsea_vs_peers_pre2024 <- chelsea_vs_peers %>%
  filter(end_year < 2024)


gap_tests_post2024 <- map_dfr(gap_cols, \(gc) {
  x <- chelsea_vs_peers_post2024[[gc]]
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
  x <- chelsea_vs_peers_pre2024[[gc]]
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


chelsea_gap_tests_comparison <- bind_rows(gap_tests_pre2024, gap_tests_post2024)
chelsea_gap_tests_comparison

did_data <- bind_rows(
  chelsea_series %>% mutate(treated = 1),
  epl38 %>% 
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
ggplot(did_data %>% filter(squad %in% c("Chelsea", peer_vec)), 
       aes(x = end_year, y = pts_game, color = squad, linetype = factor(treated))) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2023.5, linetype = "dashed", color = "red") +
  annotate("text", x = 2023, y = max(did_data$pts_game, na.rm = TRUE), 
           label = "MCO Entry", color = "red", angle = 90, vjust = -0.5) +
  scale_linetype_manual(values = c("0" = "dotted", "1" = "solid"),
                        labels = c("Peers", "Chelsea"),
                        name = "Group") +
  labs(x = "Season", y = "Points per Game",
       title = "Chelsea vs. Peers: Points per Game Over Time") +
  theme_minimal()

### Crystal Palace
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

cry_peers <- peers %>%
  filter(squad == "Crystal Palace") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(cry_peers$peer_1, cry_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

cry_series <- epl38 %>%
  filter(squad == "Crystal Palace") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

cry_vs_peers <- cry_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  cry_vs_peers[[paste0(m, "_gap")]] <-
    cry_vs_peers[[m]] - cry_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- cry_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(cry_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2020, y = Inf),
              label = "MCO Entry 2020",
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
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Crystal Palace vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (CRY - peer mean)")
plot_gap("pts_gap", "Trophies gap (CRY - peer mean)")



cry_vs_peers_post2021 <- cry_vs_peers %>%
  filter(end_year >= 2021)

cry_vs_peers_pre2021 <- cry_vs_peers %>%
  filter(end_year < 2021)


gap_tests_post2021 <- map_dfr(gap_cols, \(gc) {
  x <- cry_vs_peers_post2021[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2020",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2021 <- map_dfr(gap_cols, \(gc) {
  x <- cry_vs_peers_pre2021[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2020",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


cry_gap_tests_comparison <- bind_rows(gap_tests_pre2021, gap_tests_post2021)
cry_gap_tests_comparison



did_data <- bind_rows(
  cry_series %>% mutate(treated = 1),
  epl38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2021, 1, 0))

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




### Liverpool
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

liv_peers <- peers %>%
  filter(squad == "Liverpool") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(liv_peers$peer_1, liv_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

liv_series <- epl38 %>%
  filter(squad == "Liverpool") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

liv_vs_peers <- liv_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  liv_vs_peers[[paste0(m, "_gap")]] <-
    liv_vs_peers[[m]] - liv_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- liv_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(liv_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Liverpool vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (LIV - peer mean)")
plot_gap("pts_gap", "Points gap (LIV - peer mean)")



liv_vs_peers_post2022 <- liv_vs_peers %>%
  filter(end_year >= 2022)

liv_vs_peers_pre2022 <- liv_vs_peers %>%
  filter(end_year < 2022)


gap_tests_post2022 <- map_dfr(gap_cols, \(gc) {
  x <- liv_vs_peers_post2022[[gc]]
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
  x <- liv_vs_peers_pre2022[[gc]]
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


liv_gap_tests_comparison <- bind_rows(gap_tests_pre2022, gap_tests_post2022)
liv_gap_tests_comparison


did_data <- bind_rows(
  liv_series %>% mutate(treated = 1),
  epl38 %>% 
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





### Man City
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

city_peers <- peers %>%
  filter(squad == "Man City") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(city_peers$peer_1, city_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

city_series <- epl38 %>%
  filter(squad == "Man City") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

city_vs_peers <- city_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  city_vs_peers[[paste0(m, "_gap")]] <-
    city_vs_peers[[m]] - city_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- city_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(city_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2014, y = Inf),
              label = "MCO Entry 2014",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2014,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season", y = ylab,
         title = paste0("City vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_gap", "Points gap (City - peer mean)")
plot_gap("rk_gap", "Rank gap (City - peer mean)")

p1 <- plot_gap("pts_gap", "Points gap (City - peer mean)")
p2 <- plot_gap("rk_gap", "Rank gap (City - peer mean)")

combined <- grid.arrange(p1, p2, ncol = 2)


city_vs_peers_post2015 <- city_vs_peers %>%
  filter(end_year >= 2015)

city_vs_peers_pre2015 <- city_vs_peers %>%
  filter(end_year < 2015)


gap_tests_post2015 <- map_dfr(gap_cols, \(gc) {
  x <- city_vs_peers_post2015[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2014",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2015 <- map_dfr(gap_cols, \(gc) {
  x <- city_vs_peers_pre2015[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2014",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


city_gap_tests_comparison <- bind_rows(gap_tests_pre2015, gap_tests_post2015)
city_gap_tests_comparison

did_data <- bind_rows(
  city_series %>% mutate(treated = 1),
  epl38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2015, 1, 0))

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


### Man Utd
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

united_peers <- peers %>%
  filter(squad == "Man Utd") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(united_peers$peer_1, united_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

united_series <- epl38 %>%
  filter(squad == "Man Utd") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

united_vs_peers <- united_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  united_vs_peers[[paste0(m, "_gap")]] <-
    united_vs_peers[[m]] - united_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- united_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(united_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2024, y = Inf),
              label = "MCO Entry 2024",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2024,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Arsenal vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Utd - peer mean)")
plot_gap("pts_gap", "Points gap (Utd - peer mean)")

united_vs_peers_post2025 <- united_vs_peers %>%
  filter(end_year >= 2025)

united_vs_peers_pre2025 <- united_vs_peers %>%
  filter(end_year < 2025)


gap_tests_post2025 <- map_dfr(gap_cols, \(gc) {
  x <- united_vs_peers_post2025[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Post-2024",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


gap_tests_pre2025 <- map_dfr(gap_cols, \(gc) {
  x <- united_vs_peers_pre2025[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 2) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(
    metric_gap = gc,
    period = "Pre-2024",
    n = length(x),
    estimate = unname(tt$estimate),
    p_value = tt$p.value,
    ci_lower = tt$conf.int[1],
    ci_upper = tt$conf.int[2]
  )
})


united_gap_tests_comparison <- bind_rows(gap_tests_pre2025, gap_tests_post2025)
united_gap_tests_comparison


did_data <- bind_rows(
  united_series %>% mutate(treated = 1),
  epl38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2025, 1, 0))

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


### Nott'm Forest
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

forest_peers <- peers %>%
  filter(squad == "Nott'm Forest") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(forest_peers$peer_1, forest_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

forest_series <- epl38 %>%
  filter(squad == "Nott'm Forest") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

forest_vs_peers <- forest_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  forest_vs_peers[[paste0(m, "_gap")]] <-
    forest_vs_peers[[m]] - forest_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- forest_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(forest_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("Forest vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (Forest - peer mean)")
plot_gap("pts_gap", "Points gap (Forest - peer mean)")


gap_models <- lapply(gap_cols, function(gc) {
  lm(forest_vs_peers[[gc]] ~ 1)
})



forest_vs_peers_post2023 <- forest_vs_peers %>%
  filter(end_year >= 2023)

forest_vs_peers_pre2023 <- forest_vs_peers %>%
  filter(end_year < 2023)


gap_tests_post2023 <- map_dfr(gap_cols, \(gc) {
  x <- forest_vs_peers_post2023[[gc]]
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
  x <- forest_vs_peers_pre2023[[gc]]
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


forest_gap_tests_comparison <- bind_rows(gap_tests_pre2023, gap_tests_post2023)
forest_gap_tests_comparison

did_data <- bind_rows(
  forest_series %>% mutate(treated = 1),
  epl38 %>% 
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



### West Ham
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

whu_peers <- peers %>%
  filter(squad == "West Ham") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(whu_peers$peer_1, whu_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

whu_series <- epl38 %>%
  filter(squad == "West Ham") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

whu_vs_peers <- whu_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  whu_vs_peers[[paste0(m, "_gap")]] <-
    whu_vs_peers[[m]] - whu_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- whu_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(whu_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
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
         title = paste0("West Ham vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (West Ham - peer mean)")
plot_gap("trophies_gap", "Points gap (West Ham - peer mean)")


whu_vs_peers_post2023 <- whu_vs_peers %>%
  filter(end_year >= 2023)

whu_vs_peers_pre2023 <- whu_vs_peers %>%
  filter(end_year < 2023)


gap_tests_post2023 <- map_dfr(gap_cols, \(gc) {
  x <- whu_vs_peers_post2023[[gc]]
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
  x <- whu_vs_peers_pre2023[[gc]]
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


whu_gap_tests_comparison <- bind_rows(gap_tests_pre2023, gap_tests_post2023)
whu_gap_tests_comparison

did_data <- bind_rows(
  whu_series %>% mutate(treated = 1),
  epl38 %>% 
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



### Wolves
epl38 <- epl %>%
  filter(matchday == 38, !is.na(squad)) %>%
  mutate(end_year == as.integer(end_year))

wolves_peers <- peers %>%
  filter(squad == "Wolves") %>%
  transmute(peer_1 = as.character(peer_1),
            peer_2 = as.character(peer_2)) %>%
  slice(1)

peer_vec <- c(wolves_peers$peer_1, wolves_peers$peer_2)

metrics <- c("rk", "w", "g_diff", "pts", "pts_game", "trophies")

wolves_series <- epl38 %>%
  filter(squad == "Wolves") %>%
  select(end_year, squad, all_of(metrics)) %>%
  arrange(end_year)

peer_series <- epl38 %>%
  filter(squad %in% peer_vec) %>%
  select(end_year, squad, all_of(metrics)) %>%
  group_by(end_year) %>%
  summarise(across(all_of(metrics), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~ paste0(.x, "_peer_mean"), all_of(metrics)) %>%
  arrange(end_year)

wolves_vs_peers <- wolves_series %>%
  left_join(peer_series, by = "end_year")

for (m in metrics) {
  wolves_vs_peers[[paste0(m, "_gap")]] <-
    wolves_vs_peers[[m]] - wolves_vs_peers[[paste0(m, "_peer_mean")]]
}

gap_cols <- paste0(metrics, "_gap")

gap_tests <- map_dfr(gap_cols, \(gc) {
  x <- wolves_vs_peers[[gc]]
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(metric_gap = gc, n = length(x), estimate = NA_real_, p_value = NA_real_))
  tt <- t.test(x, mu = 0)
  tibble(metric_gap = gc, n = length(x), estimate = unname(tt$estimate), p_value = tt$p.value)
})

gap_tests

plot_gap <- function(gap_var, ylab) {
  ggplot(wolves_vs_peers, aes(x = end_year, y = .data[[gap_var]])) +
    geom_text(aes(x = 2020, y = Inf),
              label = "MCO Entry 2020",
              color = "red",
              angle = 90,
              vjust = -0.5,
              hjust = 1.1,
              size = 3) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2020,
               linetype = "dashed",
               color = "red",
               linewidth = 0.8) +
    labs(x = "Season (end_year)", y = ylab,
         title = paste0("Arsenal vs peers (gap): ", gap_var)) +
    theme_minimal(base_family = "Palatino")
}

plot_gap("pts_game_gap", "Points per game gap (West Ham - peer mean)")
plot_gap("pts_gap", "Points gap (West Ham - peer mean)")






wolves_vs_peers_post2021 <- wolves_vs_peers %>%
  filter(end_year >= 2021 & end_year <= 2024)

wolves_vs_peers_pre2021 <- wolves_vs_peers %>%
  filter(end_year < 2021)


gap_tests_post2021 <- map_dfr(gap_cols, \(gc) {
  x <- wolves_vs_peers_post2021[[gc]]
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


gap_tests_pre2021 <- map_dfr(gap_cols, \(gc) {
  x <- wolves_vs_peers_pre2021[[gc]]
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


wolves_gap_tests_comparison <- bind_rows(gap_tests_pre2021, gap_tests_post2021)
wolves_gap_tests_comparison

did_data <- bind_rows(
  wolves_series %>% mutate(treated = 1),
  epl38 %>% 
    filter(squad %in% peer_vec) %>%
    select(end_year, squad, all_of(metrics)) %>%
    mutate(treated = 0)
) %>%
  mutate(post = if_else(end_year >= 2021, 1, 0))

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


