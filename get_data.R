# import libraries
library(tidyverse)
library(httr)
library(jsonlite)

# global variables

request_headers = c(
  "accept-encoding" = "gzip, deflate, sdch",
  "accept-language" = "en-US,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)

# Player Data 

players_url <- "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=0"


request <- GET(players_url, add_headers(request_headers))


players_data <- fromJSON(content(request, as = "text"))
players <- as_tibble(data.frame(players_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players) <- tolower(players_data$resultSets$headers[[1]])

players <- mutate(players,
                 person_id = as.numeric(person_id),
                 rosterstatus = as.logical(as.numeric(rosterstatus)),
                 from_year = as.numeric(from_year),
                 to_year = as.numeric(to_year),
                 team_id = as.numeric(team_id)
)


# Shots Data


# get_shots_data function -- returns dataframe of shot data given playerid and season

# playerid = numeric
# season = character of format [20XX] - [XX]
# season type default is 'Regular Season'


get_shots_data <- function(player_id, season, seasontype = 'Regular Season') {
  shotsrequest <-  GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      SeasonType = seasontype, 
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )

shots <- content(shotsrequest)
raw_shots_data <- shots$resultSets[[1]]$rowSet
shots_col_names <- tolower(as.character(shots$resultSets[[1]]$headers))

if (length(raw_shots_data) == 0) {
  shots <- data.frame(
    matrix(nrow = 0, ncol = length(shots_col_names))
  )
} else {
  shots <- data.frame(
    matrix(
      unlist(raw_shots_data),
      ncol = length(shots_col_names),
      byrow = TRUE
    )
  )
}

shots <- as_tibble(shots)
names(shots) <- shots_col_names
shots <- mutate(shots,
                  loc_x = as.numeric(as.character(loc_x)),
                  loc_y = as.numeric(as.character(loc_y))
                  )

return(shots)

}
