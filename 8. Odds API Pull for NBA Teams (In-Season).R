
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 1. ---- START ---- Pull Spreads, Totals and Head to Head Odds for NBA from the Odds API (Full File Aggregation)
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Remove all data objects
rm(list = setdiff(ls(), c(
  "current_date",
  "formatted_date",
  "formatted_year",
  "next_year_date",
  "pbp_season",
  "season_token",
  "season_token2"
)))

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Your API key
api_key <- "4e644f6b78c9bd755f7ce726ee410e2f"

schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_",
  season_token,
  ".csv"
)

nba_schedule <- read.csv(schedule_path, stringsAsFactors = FALSE)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Date Comparison Check ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 1.4 Format game_date as Date objects in both data frames
# Convert dates back to their original string format
lineup_dataprev$PopMac_Date <- as.character(lineup_dataprev$PopMac_Date)
nba_schedule$PopMac_Game_Date <- as.character(nba_schedule$PopMac_Game_Date)

# 1.5 Debugging: Check for NA values in game_date
if (any(is.na(lineup_dataprev$PopMac_Date))) {
  print("Warning: NA values found in BaseStats_Player$game_date")
}
if (any(is.na(nba_schedule$PopMac_Game_Date))) {
  print("Warning: NA values found in nba_player_box$game_date")
}

# 1.6 Create unique lists of game dates
PM_Dates <- unique(lineup_dataprev$PopMac_Date)  # Unique dates from BaseStats_Player
NBASC_Dates <- unique(nba_schedule$PopMac_Game_Date)   # Unique dates from nba_player_box

# Convert dates back to their original string format
PM_Dates <- as.character(PM_Dates)
NBASC_Dates <- as.character(NBASC_Dates)


# 1.7 Identify missing dates in BaseStats_Player
MISSING_DATES <- setdiff(NBASC_Dates, PM_Dates)


# Debugging: Print missing dates
print("Missing dates identified (formatted):")
print(MISSING_DATES)

# 1.8 Filter nba_player_box for rows with missing dates
lineup_dataprev_filtered <- nba_schedule %>%
  filter(PopMac_Game_Date %in% MISSING_DATES)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Date Comparison Check ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# Explicitly parse MM/DD/YYYY format
date_df$Date <- as.Date(date_df$Date, format = "%m/%d/%Y")

# Use all dates for full season
game_dates <- date_df$Date

# Define markets to pull
markets_to_pull <- c("spreads", "totals", "h2h")

# Store results
all_results <- list()

# Loop through each date
for (i in seq_along(game_dates)) {
  raw_val <- game_dates[i]
  cat("Date:", raw_val, " | class:", class(raw_val), "\n")
  
  date_val <- paste0(raw_val, "T00:00:00Z")
  date_as_date <- as.Date(date_val)
  
  # Loop through each market
  for (market in markets_to_pull) {
    cat(paste0("🔲 Pulling ", market, " data for: ", date_val, "\n"))
    
    url <- paste0("https://api.the-odds-api.com/v4/historical/sports/basketball_nba/odds",
                  "?regions=us",
                  "&markets=", market,
                  "&oddsFormat=american",
                  "&date=", URLencode(date_val),
                  "&apiKey=", api_key)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      raw_json <- content(response, as = "text", encoding = "UTF-8")
      parsed <- fromJSON(raw_json, simplifyVector = FALSE)
      
      if (!is.null(parsed$data) && length(parsed$data) > 0) {
        games <- parsed$data
        
        flat <- lapply(games, function(game) {
          rows <- list()
          for (bookmaker in game[["bookmakers"]]) {
            last_update <- bookmaker[["last_update"]]
            for (mkt in bookmaker[["markets"]]) {
              for (outcome in mkt[["outcomes"]]) {
                rows[[length(rows) + 1]] <- tibble(
                  snapshot_datetime_utc = date_val,
                  market = mkt[["key"]],
                  game_id = game[["id"]],
                  sport = game[["sport_key"]],
                  commence_time = game[["commence_time"]],
                  home_team = game[["home_team"]],
                  away_team = game[["away_team"]],
                  bookmaker = bookmaker[["key"]],
                  last_update = last_update,
                  outcome_team = outcome$name,
                  spread_or_total = outcome$point,
                  odds = outcome$price
                )
              }
            }
          }
          bind_rows(rows)
        })
        
        final_df <- bind_rows(flat)
        
        # Convert time + filter to games happening on the date you're pulling
        final_df$commence_time_utc <- as.POSIXct(final_df$commence_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        final_df$commence_time_est <- as.POSIXct(final_df$commence_time_utc, tz = "America/New_York")
        final_df$commence_date_est <- as.Date(final_df$commence_time_est)
        
        final_df <- final_df %>%
          filter(commence_date_est == as.Date(raw_val))
        
        
        # Append to results list (or write line-by-line if scaling)
        if (nrow(final_df) > 0) {
          cat("📦 Rows in final_df for", raw_val, ":", nrow(final_df), "\n")
          all_results[[length(all_results) + 1]] <- final_df
          cat("🟢 Success for ", market, " on ", date_val, "\n")
        } else {
          cat("⚠️ No rows in final_df for ", market, " on ", date_val, "\n")
        }
        
      }
      
    } else {
      cat("🔴 API error for ", market, " on ", date_val, ": ", status_code(response), "\n")
    }
    
    Sys.sleep(1)  # Delay between market pulls
  }
  
  Sys.sleep(1)  # Delay between dates
}

# Combine and write final CSV
if (length(all_results) > 0) {
  full_df <- bind_rows(all_results)
  write.csv(full_df, paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/8. Historical Odds/nba_historical_odds_", season_token, ".csv"), row.names = FALSE)
  cat("🔳 Final CSV saved: nba_historical_odds.csv\n")
} else {
  cat("⚠️ No data collected.\n")
}


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 1. ---- END ---- Pull Spreads, Totals and Head to Head Odds for NBA from the Odds API (Full File Aggregation)
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀