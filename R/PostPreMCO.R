library(dplyr)

players_with_MCO <- players |>
  group_by(player_id) |>
  arrange(player_id, year) |>
  mutate(
    obs_number = row_number(),
    total_obs = n(),
    mco_period = if_else(obs_number > (total_obs - 2), "post-MCO", "pre-MCO")
  ) |> 
  select(-obs_number, -total_obs) |>
  ungroup()


players_with_MCO |>
  select(player_id, player_name, year, mco_period) |>
  head(20) |>
  print()


table(players_with_MCO$mco_period)


players_with_MCO |>
  filter(player_id == 65) |>
  select(player_id, player_name, year, season, mco_period) |>
  print()

write_xlsx(players_with_MCO, "players.xlsx")


