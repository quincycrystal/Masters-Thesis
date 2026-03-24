library(dplyr)

filter_player_id <- NULL
filter_year <- NULL 

filtered <- players 

if(!is.null(filter_player_id)) {
  filtered <- filtered |> filter(player_id == filter_player_id)
}

if(!is.null(filter_year)) {
  filtered <- filtered |> filter(player_id == filter_year)
}

filtered <- filtered |> arrange(player_id, year)

cat("Rows returned:", nrow(filtered), "\n\n")

print(filtered)
library(writexl)

write_xlsx(filtered, "players1.xlsx")
