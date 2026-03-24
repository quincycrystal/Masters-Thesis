library(readxl)
library(dplyr)
library(fixest)      
library(modelsummary) 
library(ggplot2)
library(tidyr)



nrow(players)
n_distinct(players$player_id)
min(players$year)
max(players$year)

print(table(players$mco_period))
print(table(table(players$player_id)))

players <- players |>
  mutate(
    weight = time / max(time, na.rm = TRUE),
    year_factor = factor(year),
    player_factor = factor(player_id)
  )

main_outcomes <- c(
  "npxG90",           
  "xA90",             
  "npxG90_plus_xA90"  
)


mechanism_vars <- c(
  "shots90",          
  "key_passes90",
  "xGChain90",
  "xGBuildup90"
)

realized_outcomes <- c(
  "goals90",           
  "assists90",       
  "npg90"            
)

market_outcome <- "market_value"


run_fe_model <- function(data, outcome_var, weight_var = "weight") {
  
  if (!outcome_var %in% names(data)) {
    return(NULL)
  }
  
  formula_str <- paste0(outcome_var, " ~ mco_period | player_id + year")
  
  model <- feols(
    as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = ~player_id  
  )
  
  return(model)
}


### Main Performance

main_models <- list()

for (var in main_outcomes) {
  main_models[[var]] <- run_fe_model(players, var)
}

modelsummary(
  main_models,
  output = "mainperformance_outcomes.docx",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_rename = c("mco_period" = "POST-MCO"),
  gof_map = c("nobs", "r.squared", "FE: player_id", "FE: year"),
  title = "Main Performance Metrics (Per 90 Minutes)"
)


### Mechanism Performance 

mechanism_models <- list()

for (var in mechanism_vars) {
  mechanism_models[[var]] <- run_fe_model(players, var)
}

modelsummary(
  mechanism_models,
  output = "mechanism_outcomes.docx",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_rename = c("mco_period" = "POST-MCO"),
  gof_map = c("nobs", "r.squared", "FE: player_id", "FE: year"),
  title = "Mechanism Variables (Per 90 Minutes)"
)


### Realized Outcomes

realized_models <- list()

for (var in realized_outcomes) {
  realized_models[[var]] <- run_fe_model(players, var)
}

modelsummary(
  output = "realized_outcomes.docx",
  realized_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_rename = c("mco_period" = "POST-MCO"),
  gof_map = c("nobs", "r.squared", "FE: player_id", "FE: year"),
  title = "Realized Outcomes (Per 90 Minutes)",
  notes = "SE clustered at player level"
)


### Market Value 

market_model <- run_fe_model(players, market_outcome)

if (!is.null(market_model)) {
  modelsummary(
    list("Market Value" = market_model),
    output = "marketvalue.docx",
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
    coef_rename = c("mco_period" = "POST-MCO"),
    gof_map = c("nobs", "r.squared", "FE: player_id", "FE: year"),
    title = "Market Value Analysis"
  )
}


### Coefficient Plot
extract_coef <- function(model_list) {
  results <- data.frame()
  
  for (name in names(model_list)) {
    if (!is.null(model_list[[name]])) {
      coef <- coef(model_list[[name]])["mco_period"]
      se <- se(model_list[[name]])["mco_period"]
      
      results <- rbind(results, data.frame(
        variable = name,
        estimate = coef,
        std_error = se,
        ci_lower = coef - 1.96 * se,
        ci_upper = coef + 1.96 * se
      ))
    }
  }
  
  return(results)
}

main_coefs <- extract_coef(main_models) |> mutate(category = "Main Performance")
mech_coefs <- extract_coef(mechanism_models) |> mutate(category = "Mechanisms")
real_coefs <- extract_coef(realized_models) |> mutate(category = "Realized Outcomes")
all_coefs <- bind_rows(main_coefs, mech_coefs, real_coefs)

coeff_plot <- ggplot(all_coefs, aes(x = estimate, y = reorder(variable, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  facet_wrap(~category, scales = "free_y", ncol = 1) +
  labs(
    title = "MCO Effect on Player Performance",
    subtitle = "Fixed Effects Estimates with 95% Confidence Intervals",
    x = "Estimated Effect of POST-MCO",
    y = ""
  ) +
  theme_minimal(base_family = "Palatino") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )
coeff_plot


### Descriptive Table Main Performance  
desc_stats_mp <- players |>
  group_by(mco_period) |>
  summarise(
    n = n(),
    npxG90_mean = mean(npxG90, na.rm = TRUE),
    npxG90_sd = sd(npxG90, na.rm = TRUE),
    xA90_mean = mean(xA90, na.rm = TRUE),
    xA90_sd = sd(xA90, na.rm = TRUE),
    npxG90_plus_xA90_mean = mean(npxG90_plus_xA90, na.rm = TRUE),
    npxG90_plus_xA90_sd = sd(npxG90_plus_xA90, na.rm = TRUE),
    market_value_mean = mean(market_value, na.rm = TRUE),
    market_value_sd = sd(market_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mco_period = ifelse(mco_period == 1, "POST-MCO", "PRE-MCO"))

print(desc_stats_mp)

### Descriptive Table Mechanism Variable
desc_stats_mv <- players |>
  group_by(mco_period) |>
  summarise(
    n = n(),
    shots90_mean = mean(shots90, na.rm = TRUE),
    shots90_sd = sd(shots90, na.rm = TRUE),
    key_passes90_mean = mean(key_passes90, na.rm = TRUE),
    key_passes90_sd = sd(key_passes90, na.rm = TRUE),
    xGChain90_mean = mean(xGChain90, na.rm = TRUE),
    xGChain90_sd = sd(xGChain90, na.rm = TRUE),
    xGBuildup90_mean = mean(xGBuildup90, na.rm = TRUE),
    xGBuildup90_sd = sd(xGBuildup90, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mco_period = ifelse(mco_period == 1, "POST-MCO", "PRE-MCO"))

print(desc_stats_mv)

### Realized Outcomes
desc_stats_ro <- players |>
  group_by(mco_period) |>
  summarise(
    n = n(),
    goals90_mean = mean(goals90, na.rm = TRUE),
    goals90_sd = sd(goals90, na.rm = TRUE),
    assists90_mean = mean(assists90, na.rm = TRUE),
    assists90_sd = sd(assists90, na.rm = TRUE),
    npg90_mean = mean(npg90, na.rm = TRUE),
    npg90_sd = sd(npg90, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mco_period = ifelse(mco_period == 1, "POST-MCO", "PRE-MCO"))

print(desc_stats_ro)



desc_stats_marketv <- players |>
  group_by(mco_period) |>
  summarise(
    n = n(),
    market_value_mean = mean(market_value, na.rm = TRUE),    
    market_value_sd = sd(market_value, na.rm = TRUE),         
    .groups = "drop"
  ) |>
  mutate(mco_period = ifelse(mco_period == 1, "POST-MCO", "PRE-MCO"))

print(desc_stats_marketv)


write_xlsx(desc_stats_mp, "mp.xlsx")

