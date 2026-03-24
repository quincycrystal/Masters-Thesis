## Get player stats 
###################England 
chelsea <- get_team_players_stats(team_name = "Chelsea", year = 2023)

years <- 2021:2025
library(purrr)
chelsea_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Chelsea", year = .x) %>%
    mutate(season = .x)
)

bournemouth <- get_team_players_stats(team_name = "Bournemouth", year = 2023)
years <- 2021:2025
library(purrr)
bournemouth_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Bournemouth", year = .x) %>%
    mutate(season = .x)
)

mancity <- get_team_players_stats(team_name = "Manchester City", year = 2011)
years <- 2016:2017
mancity_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Manchester City", year = .x) %>%
    mutate(season = .x)
)

westham <- get_team_players_stats(team_name = "West Ham", year = 2020)
years <- 2020:2024
westham_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "West Ham", year = .x) %>%
    mutate(season = .x)
)

palace <- get_team_players_stats(team_name = "Crystal Palace", year = 2017)
years <- 2020:2021
palace_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Crystal Palace", year = .x) %>%
    mutate(season = .x)
)

years <- 2022:2025
forest_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Nottingham Forest", year = .x) %>%
    mutate(season = .x)
)

years <- 2018:2022
wolves_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Wolverhampton Wanderers", year = .x) %>%
    mutate(season = .x)
)

years <- 2023:2025
manunited_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Manchester United", year = .x) %>%
    mutate(season = .x)
)

years <- 2015:2019
arsenal_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Arsenal", year = .x) %>%
    mutate(season = .x)
)

years <- 2021:2025
brentford_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Brentford", year = .x) %>%
    mutate(season = .x)
)

years <- 2019:2023
liverpool_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Liverpool", year = .x) %>%
    mutate(season = .x)
)

years <- 2018:2019
brighton_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Brighton", year = .x) %>%
    mutate(season = .x)
)

years <- 2021:2024
villa_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Aston Villa", year = .x) %>%
    mutate(season = .x)
)


library(dplyr)
all_names <- ls(pattern = "_all$")
all_list <- lapply(all_names, get)
PL <- bind_rows(all_list, .id = "source_dataset")
write.csv(PL, "PL.csv", row.names = FALSE)

###################Italy 
years <- 2022:2024
atalanta_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Atalanta", year = .x) %>%
    mutate(season = .x)
)

years <- 2016:2020
napoli_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Napoli", year = .x) %>%
    mutate(season = .x)
)

years <- 2016:2020
verona_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Verona", year = .x) %>%
    mutate(season = .x)
)

years <- 2014:2018
udinese_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Udinese", year = .x) %>%
    mutate(season = .x)
)

years <- 2022:2023
milan_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "AC Milan", year = .x) %>%
    mutate(season = .x)
)

years <- 2017:2021
fiorentina_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Fiorentina", year = .x) %>%
    mutate(season = .x)
)

years <- 2015:2019
bologna_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Bologna", year = .x) %>%
    mutate(season = .x)
)

###################Spain
years <- 2019:2023
sevilla_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Sevilla", year = .x) %>%
    mutate(season = .x)
)

years <- 2019:2023
atletico_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Atletico Madrid", year = .x) %>%
    mutate(season = .x)
)

years <- 2022:2025
girona_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Girona", year = .x) %>%
    mutate(season = .x)
)

years <- 2014:2018
valencia_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Valencia", year = .x) %>%
    mutate(season = .x)
)

###################Germany
years <- 2019:2023
augsburg_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Augsburg", year = .x) %>%
    mutate(season = .x)
)

years <- 2016:2020
leipzig_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "RasenBallsport Leipzig", year = .x) %>%
    mutate(season = .x)
)

###################France
years <- 2021:2025
lorient_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Lorient", year = .x) %>%
    mutate(season = .x)
)

years <- 2021:2025
strasbourg_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Strasbourg", year = .x) %>%
    mutate(season = .x)
)

years <- 2015:2019
monaco_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Monaco", year = .x) %>%
    mutate(season = .x)
)

years <- 2019:2023
marseille_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Marseille", year = .x) %>%
    mutate(season = .x)
)

years <- 2017:2021
nice_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Nice", year = .x) %>%
    mutate(season = .x)
)

years <- 2021:2025
lyon_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Lyon", year = .x) %>%
    mutate(season = .x)
)

###CHECK
years <- 2014:2018
lens_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Lens", year = .x) %>%
    mutate(season = .x)
)

years <- 2021:2025
psg_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Paris Saint Germain", year = .x) %>%
    mutate(season = .x)
)

years <- 2022:2023
toulouse_all <- map_df(
  years,
  ~ get_team_players_stats(team_name = "Toulouse", year = .x) %>%
    mutate(season = .x)
)



all_names <- ls(pattern = "_all$")
all_list <- lapply(all_names, get)
AllTeams <- bind_rows(all_list, .id = "source_dataset")
write.csv(AllTeams, "AllTeams.csv", row.names = FALSE, fileEncoding = "UTF-8")


###Multiple IDs at once 
library(purrr)
library(dplyr)

player_ids <- c(1776, 3585, 531, 7365, 2335, 935, 986, 6935, 8288, 1245, 528, 
                534, 2674, 3278, 6424, 8965, 11242)

players_rem <- map_df(
  player_ids,
  ~ get_player_seasons_stats(player_id = .x)
)

players_rem <- players_rem %>% 
  filter(year %in% 2020:2021)

missing_cols <- setdiff(colnames(AllTeams), colnames(players_rem))
players_rem[missing_cols] <- NA
players_rem <- players_rem[, colnames(AllTeams)]
write_xlsx(players_rem, "WestHam.xlsx")


