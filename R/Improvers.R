calculate_player_changes <- function(data, outcome_var) {
  
  if (!outcome_var %in% names(data)) {
    return(NULL)
  }
  
  changes <- data |>
    dplyr::group_by(player_id, player_name, mco_period) |>
    dplyr::summarise(
      mean_value = mean(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = mco_period,
      values_from = mean_value,
      names_prefix = "period_"
    ) |>
    dplyr::mutate(
      change = period_1 - period_0,  
      pct_change = (change / abs(period_0)) * 100
    ) |>
    dplyr::arrange(desc(pct_change))
  
  return(changes)
}

npxG90_plus_xA90_changes <- calculate_player_changes(players, "npxG90_plus_xA90")
keyPasses90_changes <- calculate_player_changes(players, "key_passes90")
assists90_changes <- calculate_player_changes(players, "assists90")
market_value_changes <- calculate_player_changes(players, "market_value")

top_improvers_npxG90_plus_xA90 <- npxG90_plus_xA90_changes |>
  dplyr::select(player_name, period_0, period_1, change, pct_change) |>
  dplyr::rename(
    Player = player_name,
    PRE_MCO = period_0,
    POST_MCO = period_1,
    Change = change,
    Pct_Change = pct_change
  ) |>
  head(10)

print(top_improvers_npxG90_plus_xA90)




top_improvers_keyPasses90 <- keyPasses90_changes |>
  dplyr::select(player_name, period_0, period_1, change, pct_change) |>
  dplyr::rename(
    Player = player_name,
    PRE_MCO = period_0,
    POST_MCO = period_1,
    Change = change,
    Pct_Change = pct_change
  ) |>
  head(10)

print(top_improvers_keyPasses90)


top_improvers_assists90 <- assists90_changes |>
  dplyr::select(player_name, period_0, period_1, change, pct_change) |>
  dplyr::rename(
    Player = player_name,
    PRE_MCO = period_0,
    POST_MCO = period_1,
    Change = change,
    Pct_Change = pct_change
  ) |>
  head(20)

print(top_improvers_assists90)


top_improvers_market_value <- market_value_changes |>
  dplyr::select(player_name, period_0, period_1, change, pct_change) |>
  dplyr::rename(
    Player = player_name,
    PRE_MCO = period_0,
    POST_MCO = period_1,
    Change = change,
    Pct_Change = pct_change
  ) |>
  head(10)

print(top_improvers_market_value)



