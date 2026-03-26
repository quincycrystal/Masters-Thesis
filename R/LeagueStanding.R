library(worldfootballR)
library(purrr)
library(usethis)
library(dplyr)



start_years <- 2005:2022

#EPL
epl_tables_list <- map(start_years, function(yr) {
  
  out <- tryCatch(
    tm_matchday_table(
      country_name = "England",
      start_year   = as.character(yr),
      matchday     = 38
    ) %>% mutate(start_year = yr),
    error = function(e) {
      message(" Failed for season ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  out
})

epl_20_seasons <- bind_rows(epl_tables_list)

#SerieA
serieA_tables_list <- map(start_years, function(yr) {
  
  out <- tryCatch(
    tm_matchday_table(
      country_name = "Italy",
      start_year   = as.character(yr),
      matchday     = 38
    ) %>% mutate(start_year = yr),
    error = function(e) {
      message(" Failed for season ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  out
})

serieA_20_seasons <- bind_rows(serieA_tables_list)

#LaLiga
laliga_tables_list <- map(start_years, function(yr) {
  
  out <- tryCatch(
    tm_matchday_table(
      country_name = "Spain",
      start_year   = as.character(yr),
      matchday     = 38
    ) %>% mutate(start_year = yr),
    error = function(e) {
      message(" Failed for season ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  out
})

laliga_20_seasons <- bind_rows(laliga_tables_list)

#Bundesliga 
bundesliga_tables_list <- map(start_years, function(yr) {
  
  out <- tryCatch(
    tm_matchday_table(
      country_name = "Germany",
      start_year   = as.character(yr),
      matchday     = 34
    ) %>% mutate(start_year = yr),
    error = function(e) {
      message(" Failed for season ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  out
})

bundesliga_20_seasons <- bind_rows(bundesliga_tables_list)

#Ligue1 
ligue1_tables_list <- map(start_years, function(yr) 
  
  out <- tryCatch(
    tm_matchday_table(
      country_name = "France",
      start_year   = as.character(yr),
      matchday     = 38
    ) %>% mutate(start_year = yr),
    error = function(e) {
      message(" Failed for season ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  out
})

ligue1_20_seasons <- bind_rows(ligue1_tables_list)

write_xlsx(epl_20_seasons, "EPL20.xlsx")
write_xlsx(serieA_20_seasons, "SeriaA20.xlsx")
write_xlsx(laliga_20_seasons, "LaLiga20.xlsx")
write_xlsx(bundesliga_20_seasons, "Bundesliga20.xlsx")
write_xlsx(ligue1_20_seasons, "Ligue120.xlsx")


