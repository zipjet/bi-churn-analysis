active_fleets <- list(
  "London"=c("Central", "West", "North East", "South"),
  "Berlin"=c("Zentrallager"),
  "Paris"=c("North Paris", "South Paris")
)

active.fleets <- list(
  "London"=c("Central", "West", "North East", "South"),
  "Berlin"=c("Zentrallager"),
  "Paris"=c("North Paris", "South Paris")
)

all.fleets <- unlist(active.fleets, use.names = F)

old_fleets_ids <- c(
  "568fecbce4b03af23bd8beef", "56b9f5c4e4b0b07f53e2b0a4",
  "57335e0be4b08e8ef0d96134", "56a7669be4b03ca5028d06b0",
  "589361ede4b015c31b791d1e", "593a77cce4b04d774672f5de",
  "57601f53e4b0b321faa9065b")

city_codes <- c("Berlin" = "DE", "Paris" = "FR", "London" = "GB")

fleets_db <- list(
  "London" = c("City", "West (morning)", "Soho", "NE", "South"),
  "Paris" = c("North", "South")
)

city_ids <- c("London"="zipjet-gb-london", "Paris"="zipjet-fr-paris")

fleets_db_edited <- list(
  "London"=c("City", "West", "Soho", "North East", "South"),
  "Berlin"=c("Zentrallager"),
  "Paris"=c("North Paris", "South Paris")
)

city_tz <- list(
  "London"="Europe/London",
  "Berlin"="Europe/Berlin",
  "Paris"="Europe/Paris"
)

morning_shift <- "MS"
evening_shift <- "ES"

fleet.hubs <- c("Zentrallager" = "Zentrallager",
                "Central - Camden Road" = "Central",
                "York House - West" = "West",
                "South" = "South",
                "NE Camden Road" = "North East",
                "North hub" = "North Paris",
                "South hub" = "South Paris")