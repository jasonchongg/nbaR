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
# Gets List of All players in NBA History

get_players_data <- function() {
# make request
playersrequest <- GET(
        "http://stats.nba.com/stats/commonallplayers",
        query = list (
          LeagueID = '00',
          Season = '2017-18',
          IsOnlyCurrentSeason = '0'
        ),
        add_headers(request_headers))
# read JSON file into R
players <- fromJSON(content(playersrequest, as = "text"))
# convert to df then tibble for readability
players_df <- as_tibble(data.frame(players$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
# rename columns
names(players_df) <- tolower(players$resultSets$headers[[1]])
# refactor data types
players_df <- mutate(players_df,
                 person_id = as.numeric(person_id),
                 rosterstatus = as.logical(as.numeric(rosterstatus)),
                 from_year = as.numeric(from_year),
                 to_year = as.numeric(to_year),
                 team_id = as.numeric(team_id)
)

# return data
return(players_df)
}


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

shots <- fromJSON(content(shotsrequest, as = 'text'))
shots_df <- as.tibble(data.frame(shots$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
if (length(shots_df) != 0) {
names(shots_df) <- tolower(as.character(shots$resultSets$headers[[1]]))


shots_df <- mutate(shots_df,
                  loc_x = as.numeric(as.character(loc_x)),
                  loc_y = as.numeric(as.character(loc_y))
                  )

return(shots_df)
}
  else {
    return(NULL)
  }

}

#teamgamelog endpoint for game id's??

# playbyplay 
#data returns event_ids(event_num) of individual 'plays' that correspond to a list of movements
# merge with movement data to visualize play.
get_pbp_data <- function(gameid) {
    pbprequest <-  GET(
      "http://stats.nba.com/stats/playbyplayv2",
      query = list(
        GameID = '0021500431',
        StartPeriod = '1',
        EndPeriod = '4'
      ),
      add_headers(request_headers)
    )
    
    pbp <- fromJSON(content(pbprequest, as ='text'))
    pbp_df <- as.tibble(data.frame(pbp$resultSet$rowSet[[1]], stringsAsFactors=FALSE))
    names(pbp_df) <- tolower(as.character(pbp$resultSets$headers[[1]]))
    
    
    
    
    return(pbp_df)
    
}
