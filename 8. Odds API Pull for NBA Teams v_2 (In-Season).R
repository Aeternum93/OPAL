#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Pull All Team odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
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

library(lubridate)
library(tidyr)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(data.table)

# Your API key
api_key <- "4e644f6b78c9bd755f7ce726ee410e2f"


schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_",
  season_token,
  ".csv"
)

nba_schedule <- read.csv(schedule_path, stringsAsFactors = FALSE)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Date Comparison Check (Schedule vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Path to historical odds file
hist_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/8. Historical Odds/nba_historical_odds_",
  season_token,
  ".csv"
)

# 1.1 Ensure game_date column exists and is parsed correctly (m/d/YYYY)
if (!"game_date" %in% names(nba_schedule)) {
  stop("nba_schedule is missing `game_date` column.")
}

nba_schedule$game_date <- as.Date(nba_schedule$game_date, format = "%Y-%m-%d")

# Optional sanity check
if (any(is.na(nba_schedule$game_date))) {
  warning("Some game_date values could not be parsed. Check source CSV format.")
}

# 1.2 Unique, sorted schedule dates
NBASC_Dates <- sort(unique(nba_schedule$game_date))

# 1.3 Clamp to current_date if it exists
if (exists("current_date")) {
  current_date_val <- as.Date(current_date)
  NBASC_Dates <- NBASC_Dates[NBASC_Dates <= current_date_val]
}

# 1.4 Branch: historical odds file exists or not
if (!file.exists(hist_odds_path) || file.info(hist_odds_path)$size == 0) {
  cat("No existing historical odds file found. Building from scratch using full schedule range.\n")
  
  # Fresh build: all schedule dates are missing
  MISSING_DATES <- NBASC_Dates
  
} else {
  cat("Existing historical odds file found. Checking for missing dates...\n")
  
  hist_df <- read.csv(hist_odds_path, stringsAsFactors = FALSE)
  
  if (!"commence_date_est" %in% names(hist_df)) {
    stop("Historical odds file is missing `commence_date_est` column.")
  }
  
  hist_df$commence_date_est <- as.Date(hist_df$commence_date_est)
  
  # Unique existing dates in historical odds file
  PM_Dates <- sort(unique(hist_df$commence_date_est))
  
  # 1.5 Identify missing dates (present in schedule, not in historical file)
  MISSING_DATES <- setdiff(NBASC_Dates, PM_Dates)
  
  # >>> ADD THIS FIX <<<
  # Convert missing dates to proper Date objects
  MISSING_DATES <- as.Date(MISSING_DATES)
  
  cat("Missing dates identified (formatted):\n")
  print(MISSING_DATES)
}

# 1.6 Build date_df used by the Odds API loop
# (store as character MM/DD/YYYY so the existing parsing line still works)
if (length(MISSING_DATES) == 0) {
  cat("No missing dates to pull. date_df will be empty.\n")
  date_df <- data.frame(
    Date = character(0),
    stringsAsFactors = FALSE
  )
} else {
  date_df <- data.frame(
    Date = format(MISSING_DATES, "%m/%d/%Y"),
    stringsAsFactors = FALSE
  )
}


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Date Comparison Check (Schedule vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Refomrmat Dates for input into the Odds API ====
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
  

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Refomrmat Dates for input into the Odds API ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  

  
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Loop through the Odds API for missing dates ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  
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
        final_df$commence_time_utc <- as.POSIXct(
          final_df$commence_time,
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
        
        # This line is fine – just for printing in EST
        final_df$commence_time_est <- with_tz(
          final_df$commence_time_utc,
          tzone = "America/New_York"
        )
        
        # ✅ Get the calendar date in America/New_York, not UTC
        final_df$commence_date_est <- as.Date(
          format(final_df$commence_time_utc, tz = "America/New_York", usetz = TRUE)
        )
        
        
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

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Loop through the Odds API for missing dates ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Use oddsapi_team_mapping file to normalize and create odds_api game Ids locally ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Combine and write final CSV
if (length(all_results) > 0) {
  full_df <- bind_rows(all_results)
  
  # === NEW: Map OddsAPI team names to ESPN IDs/abbrevs ===
  oddsapi_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/6. Data Cleanup/oddsapi_team_mapping.csv"
  oddsapi_df  <- read.csv(oddsapi_path, stringsAsFactors = FALSE)
  
  # Home team mapping
  home_map <- oddsapi_df %>%
    dplyr::select(team_name, ESPN.ID, team_abv) %>%
    dplyr::rename(
      home_team          = team_name,
      espn_team_id_home  = ESPN.ID,
      espn_team_abv_home = team_abv
    )
  
  # Away team mapping
  away_map <- oddsapi_df %>%
    dplyr::select(team_name, ESPN.ID, team_abv) %>%
    dplyr::rename(
      away_team          = team_name,
      espn_team_id_away  = ESPN.ID,
      espn_team_abv_away = team_abv
    )
  
  # Join mappings into historical odds and build concat key
  full_df <- full_df %>%
    dplyr::left_join(home_map, by = "home_team") %>%
    dplyr::left_join(away_map, by = "away_team") %>%
    dplyr::mutate(
      espn_matchup_key = paste0(
        espn_team_abv_home, "_",
        espn_team_abv_away, "_",
        commence_date_est
      )
    )
  
  full_df <- full_df %>%
    mutate(
      outcome_team_abv = case_when(
        outcome_team == home_team ~ espn_team_abv_home,
        outcome_team == away_team ~ espn_team_abv_away,
        TRUE ~ NA_character_
      ),
      opponent_team_abv = case_when(
        outcome_team_abv == espn_team_abv_home ~ espn_team_abv_away,
        outcome_team_abv == espn_team_abv_away ~ espn_team_abv_home,
        TRUE ~ NA_character_
      )
    )
  
  full_df <- full_df %>%
    mutate(
      espn_matchup_key = paste0(
        outcome_team_abv, "_",
        opponent_team_abv, "_",
        commence_date_est
      )
    )
  
  # Build the full output path
  out_path <- paste0(
    "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/8. Historical Odds/nba_historical_odds_",
    season_token,
    ".csv"
  )
  
  # Append to file (no header)
  fwrite(
    full_df,
    file = out_path,
    append = TRUE,
    col.names = FALSE
  )
  
  cat("✔️ Appended to:", out_path, "\n")
  
  cat("🔳 Final CSV saved: nba_historical_odds.csv\n")
} else {
  cat("⚠️ No data collected.\n")
}


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Use oddsapi_team_mapping file to normalize and create odds_api game Ids locally ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Map Odds API odds back into nba_schedule ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(tidyr)

# --- 1. Load historical odds file ---
hist_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/8. Historical Odds/nba_historical_odds_",
  season_token,
  ".csv"
)

hist_df <- read.csv(hist_odds_path, stringsAsFactors = FALSE)

# Make sure date is Date type
hist_df$commence_date_est <- as.Date(hist_df$commence_date_est)

# --- 1b. Build espn_matchup_key from mapped abbrevs + date, and set team-side ---

keep_books <- c("fanduel", "draftkings", "betmgm", "fanatics")

hist_df <- hist_df %>%
  filter(bookmaker %in% keep_books) %>%
  mutate(
    # ESPN abbrev for the team the odds entry belongs to
    espn_team_abv_outcome = case_when(
      market %in% c("spreads", "h2h") & outcome_team == home_team ~ espn_team_abv_home,
      market %in% c("spreads", "h2h") & outcome_team == away_team ~ espn_team_abv_away,
      TRUE ~ NA_character_
    ),
    
    # Opponent abbrev
    espn_team_abv_opponent = case_when(
      espn_team_abv_outcome == espn_team_abv_home ~ espn_team_abv_away,
      espn_team_abv_outcome == espn_team_abv_away ~ espn_team_abv_home,
      TRUE ~ NA_character_
    ),
    
    # Team-centric matchup key (same rule as full_df)
    espn_matchup_key = ifelse(
      !is.na(espn_team_abv_outcome) & !is.na(espn_team_abv_opponent),
      paste0(espn_team_abv_outcome, "_", espn_team_abv_opponent, "_", commence_date_est),
      NA_character_
    ),
    
    # Bookmaker shortcode
    book_code = case_when(
      bookmaker == "fanduel"    ~ "FD",
      bookmaker == "draftkings" ~ "DK",
      bookmaker == "betmgm"     ~ "BMGM",
      bookmaker == "fanatics"   ~ "FNT",
      TRUE ~ NA_character_
    )
  )


# --- 2. Team-specific odds (spreads + h2h) ---

odds_team_df <- hist_df %>%
  filter(market %in% c("spreads", "h2h"),
         !is.na(espn_matchup_key),
         !is.na(espn_team_abv_outcome)) %>%
  mutate(
    col_suffix = if_else(market == "spreads", "spread", "h2h"),
    col_name   = paste0(book_code, "_", col_suffix)
  ) %>%
  select(espn_matchup_key, espn_team_abv_outcome, col_name, odds)

odds_team_wide <- odds_team_df %>%
  tidyr::pivot_wider(
    id_cols    = c(espn_matchup_key, espn_team_abv_outcome),
    names_from = col_name,
    values_from = odds,
    values_fn   = function(x) x[1]  # first line if multiple
  )

# --- 3. Game-level totals (same for both teams) ---

odds_total_df <- hist_df %>%
  filter(market == "totals",
         !is.na(espn_matchup_key)) %>%
  mutate(
    col_name = paste0(book_code, "_totals")
  ) %>%
  select(espn_matchup_key, col_name, odds)

odds_total_wide <- odds_total_df %>%
  tidyr::pivot_wider(
    id_cols    = espn_matchup_key,
    names_from = col_name,
    values_from = odds,
    values_fn   = function(x) x[1]
  )

# --- 4. Build matchup key in nba_schedule (team-centric) ---

nba_schedule$game_date <- as.Date(nba_schedule$game_date)

nba_schedule <- nba_schedule %>%
  mutate(
    oddsapi_map = paste0(
      team, "_",
      opp,  "_",
      format(game_date, "%Y-%m-%d")
    )
  )

# --- 5. Join odds into nba_schedule, *by team* ---

nba_schedule <- nba_schedule %>%
  # spreads + h2h: per team
  left_join(
    odds_team_wide,
    by = c(
      "oddsapi_map" = "espn_matchup_key",
      "team"        = "espn_team_abv_outcome"
    )
  ) %>%
  # totals: per game (same for both team rows)
  left_join(
    odds_total_wide,
    by = c("oddsapi_map" = "espn_matchup_key")
  )


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Map Odds API odds back into nba_schedule ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Write nba_schedule to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- 10. Export nba_schedule by APPENDING to the specified path ---

# ---- Overwrite schedule file every time ----
nba_schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_",
  season_token,
  ".csv"
)

# Always overwrite
fwrite(
  nba_schedule,
  file = nba_schedule_path,
  append = FALSE,   # overwrite
  col.names = TRUE  # include header
)

cat("nba_schedule_", season_token, "has been overwritten at:\n", nba_schedule_path, "\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Write nba_schedule to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Pull All Team odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀