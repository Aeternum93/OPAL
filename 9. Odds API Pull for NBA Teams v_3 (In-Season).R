#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  1. START: Pull All Team odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Remove all data objects
rm(list = setdiff(ls(), c(
  # core globals
  "current_date",
  "formatted_date",
  "formatted_year",
  "next_year_date",
  "pbp_season",
  "season_token",
  "season_token2",
  
  # paths / config
  "base_path",
  "logo_dir",
  "essential_names",
  
  # runner config + helpers
  "scripts",
  "batches",
  "run_script",
  "run_batch"
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

schedule_path <- paste0( "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_", season_token, ".csv" ) 


currentslate_path <- paste0( "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate/nba_schedule_", formatted_date, ".csv" ) 

#Read csv files from this location
nba_schedule <- read.csv(schedule_path, stringsAsFactors = FALSE)
current_slate <- read.csv(currentslate_path, stringsAsFactors = FALSE)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  1. END: Pull All Team odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. START: Date Comparison Check (Schedule vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================================================
# === START: Load BaseStats_Team and determine dates to pull from API ===
# ======================================================================

# --- Load BaseStats_Team ---
baseST_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token,
  ".csv"
)

BaseStats_Team <- read_csv(baseST_path, col_types = cols(.default = "c")) %>%
  mutate(game_date = as.Date(game_date))

# BaseStats_Team dates
base_dates_raw <- sort(unique(BaseStats_Team$game_date))

# Clamp to current_date (inclusive)
if (exists("current_date")) {
  current_date_val <- as.Date(current_date)
  base_dates <- c(base_dates_raw, current_date_val) |> unique() |> sort()
} else {
  base_dates <- base_dates_raw
}

cat("BaseStats_Team date range: ",
    as.character(min(base_dates)), "to", as.character(max(base_dates)), "\n")
cat("BaseStats_Team unique dates:", length(base_dates), "\n\n")

# Extend base_dates by one day to allow tomorrow's odds
base_dates_extended <- c(base_dates, max(base_dates) + 2)
base_dates_extended <- sort(unique(base_dates_extended))

# --- Path to historical odds file ---
hist_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_",
  season_token,
  ".csv"
)

# --- Determine the full window we SHOULD cover ---
season_start <- min(base_dates)

if (exists("current_date")) {
  current_date_val <- as.Date(current_date)
} else {
  current_date_val <- max(base_dates)
}

# 🔥 **VALID WINDOW = full season start → TODAY (inclusive)**
valid_window <- seq(season_start, current_date_val, by = "day")


# ======================================================================
# === Existing historical file: determine what dates are still missing ===
# ======================================================================

if (!file.exists(hist_odds_path) || file.info(hist_odds_path)$size == 0) {
  
  cat("No existing historical odds file found. Full backfill.\n")
  MISSING_DATES <- valid_window   # Pull ALL
  
} else {
  
  cat("Existing historical odds file found. Checking for missing dates vs window...\n")
  
  hist_df <- read.csv(hist_odds_path, stringsAsFactors = FALSE)
  
  # Ensure commence_date_est column exists and is Date
  if (!"commence_date_est" %in% names(hist_df)) {
    
    if (!"commence_time" %in% names(hist_df)) {
      stop("Historical odds file is missing `commence_date_est` and `commence_time`.")
    }
    
    hist_df$commence_time_utc <- as.POSIXct(
      hist_df$commence_time,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    
    hist_df$commence_date_est <- as.Date(
      format(hist_df$commence_time_utc, tz = "America/New_York", usetz = TRUE)
    )
    
  } else {
    hist_df$commence_date_est <- as.Date(hist_df$commence_date_est)
  }
  
  PM_Dates <- sort(unique(hist_df$commence_date_est))
  
  # 🔥 Pull ANY date in the valid window that is NOT in the historical file
  MISSING_DATES <- setdiff(valid_window, PM_Dates)
  
  cat("Season window:", as.character(season_start), "→", as.character(current_date_val), "\n")
  cat("Historical odds dates loaded:", length(PM_Dates), "\n")
  cat("Missing (to pull now):", length(MISSING_DATES), "\n\n")
}


# ======================================================================
# === Build date_df for the API loop (no prettyNum errors) ===
# ======================================================================

MISSING_DATES <- as.Date(MISSING_DATES)
MISSING_DATES <- MISSING_DATES[!is.na(MISSING_DATES)]

if (length(MISSING_DATES) == 0) {
  
  cat("No missing dates to pull. date_df will be empty.\n")
  date_df <- data.frame(Date = character(0), stringsAsFactors = FALSE)
  
} else {
  
  date_df <- data.frame(
    Date = format(MISSING_DATES, "%m/%d/%Y"),  # MM/DD/YYYY for API
    stringsAsFactors = FALSE
  )
}

# ======================================================================
# === END: Determine dates to pull from API ===
# ======================================================================


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. END: Date Comparison Check (Schedule vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 3. START: Refomrmat Dates for input into the Odds API ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ==== START: Reformat Dates for input into the Odds API ====

# Final list of dates to run (already Date objects)
game_dates <- sort(MISSING_DATES)

cat("Total dates to pull:", length(game_dates), "\n\n")

# Define markets to pull
markets_to_pull <- c("spreads", "totals", "h2h")

# Store results
all_results <- list()

# Loop through each date
for (i in seq_along(game_dates)) {
  raw_val <- game_dates[i]  # Date object
  cat("Date:", raw_val, " | class:", class(raw_val), "\n")
  
  # If you decide you need +1 day for the API, change `raw_val` to `raw_val + 1` here
  # request_date <- raw_val + 1
  # date_val     <- paste0(format(request_date, "%Y-%m-%d"), "T00:00:00Z")
  
  date_val <- paste0(format(raw_val, "%Y-%m-%d"), "T12:00:00Z")
  
  
  
  #🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  # ==== 3. END: Refomrmat Dates for input into the Odds API ====
  #🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  
  
  
  #🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  # ==== 4. START: Loop through the Odds API for missing dates ====
  #🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
  
  # Loop through each market
  for (market in markets_to_pull) {
    cat(paste0("🔲 Pulling ", market, " data for: ", date_val, "\n"))
    
    url <- paste0(
      "https://api.the-odds-api.com/v4/historical/sports/basketball_nba/odds",
      "?regions=us",
      "&markets=", market,
      "&oddsFormat=american",
      "&date=", URLencode(date_val),
      "&apiKey=", api_key
    )
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      raw_json <- content(response, as = "text", encoding = "UTF-8")
      parsed   <- fromJSON(raw_json, simplifyVector = FALSE)
      
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
                  market                = mkt[["key"]],
                  game_id               = game[["id"]],
                  sport                 = game[["sport_key"]],
                  commence_time         = game[["commence_time"]],
                  home_team             = game[["home_team"]],
                  away_team             = game[["away_team"]],
                  bookmaker             = bookmaker[["key"]],
                  last_update           = last_update,
                  outcome_team          = outcome$name,
                  spread_or_total       = outcome$point,
                  odds                  = outcome$price
                )
              }
            }
          }
          bind_rows(rows)
        })
        
        final_df <- bind_rows(flat)
        
        final_df$commence_time_utc <- as.POSIXct(
          final_df$commence_time,
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
        
        final_df$commence_time_est <- with_tz(
          final_df$commence_time_utc,
          tzone = "America/New_York"
        )
        
        final_df$commence_date_est <- as.Date(
          format(final_df$commence_time_utc, tz = "America/New_York", usetz = TRUE)
        )
        
        # Keep only games whose date exists in BaseStats_Team window
        final_df <- final_df %>%
          dplyr::filter(commence_date_est %in% base_dates)
        
        if (nrow(final_df) == 0) {
          cat("⚠ No rows remaining after filtering to BaseStats_Team dates for ", date_val, "\n")
        } else {
          cat("📦 Rows in final_df for", raw_val, "after date filter:", nrow(final_df), "\n")
          all_results[[length(all_results) + 1]] <- final_df
          cat("🟢 Success for ", market, " on ", date_val, "\n")
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
# ==== 4. END: Loop through the Odds API for missing dates ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 5. START: Use oddsapi_team_mapping file to normalize and create odds_api game Ids locally ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

if (length(all_results) > 0) {
  
  full_df <- bind_rows(all_results)
  
  # === Load team mapping file ===
  oddsapi_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/oddsapi_team_mapping.csv"
  oddsapi_df  <- read.csv(oddsapi_path, stringsAsFactors = FALSE)
  
  # --- Home mapping ---
  home_map <- oddsapi_df %>%
    dplyr::select(team_name, ESPN.ID, team_abv) %>%
    dplyr::rename(
      home_team          = team_name,
      espn_team_id_home  = ESPN.ID,
      espn_team_abv_home = team_abv
    )
  
  # --- Away mapping ---
  away_map <- oddsapi_df %>%
    dplyr::select(team_name, ESPN.ID, team_abv) %>%
    dplyr::rename(
      away_team          = team_name,
      espn_team_id_away  = ESPN.ID,
      espn_team_abv_away = team_abv
    )
  
  # --- Join home/away team mapping ---
  full_df <- full_df %>%
    dplyr::left_join(home_map, by = "home_team") %>%
    dplyr::left_join(away_map, by = "away_team")
  
  # --- Build perspective + game keys ---
  full_df <- full_df %>%
    dplyr::mutate(
      # team-perspective abbrevs (valid for spreads/h2h)
      outcome_team_abv = dplyr::case_when(
        outcome_team == home_team ~ espn_team_abv_home,
        outcome_team == away_team ~ espn_team_abv_away,
        TRUE ~ NA_character_
      ),
      opponent_team_abv = dplyr::case_when(
        outcome_team_abv == espn_team_abv_home ~ espn_team_abv_away,
        outcome_team_abv == espn_team_abv_away ~ espn_team_abv_home,
        TRUE ~ NA_character_
      ),
      
      # directional home_vs_away game key
      game_matchup_key = paste0(
        espn_team_abv_home, "_",
        espn_team_abv_away, "_",
        commence_date_est
      ),
      
      # order-independent pair key (for patching both team rows later)
      pair_team1 = pmin(espn_team_abv_home, espn_team_abv_away),
      pair_team2 = pmax(espn_team_abv_home, espn_team_abv_away),
      game_pair_key = paste0(
        pair_team1, "_",
        pair_team2, "_",
        commence_date_est
      ),
      
      # FINAL espn_matchup_key:
      # - spreads/h2h: team-centric (outcome vs opponent)
      # - totals:      home_vs_away game key
      espn_matchup_key = dplyr::if_else(
        !is.na(outcome_team_abv) & !is.na(opponent_team_abv),
        paste0(outcome_team_abv, "_", opponent_team_abv, "_", commence_date_est),
        game_matchup_key
      )
    ) %>%
    dplyr::select(-pair_team1, -pair_team2)  # helpers no longer needed
  
  # --- Output path ---
  out_path <- paste0(
    "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_",
    season_token,
    ".csv"
  )
  
  file_exists <- file.exists(out_path) && file.info(out_path)$size > 0
  
  # --- Write/append file ---
  fwrite(
    full_df,
    file = out_path,
    append = file_exists,
    col.names = !file_exists
  )
  
  cat("✔️ Appended to:", out_path, "\n")
  cat("🔳 Final CSV saved: nba_historical_odds.csv\n")
  
} else {
  cat("⚠️ No data collected.\n")
}

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 5. END: Use oddsapi_team_mapping file to normalize and create odds_api game Ids locally ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 6. START: Map Odds API TOTALS + SPREAD/H2H into nba_schedule & current_slate ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(tidyr)

# --- 1. Load historical odds file (same season_token as elsewhere) -----------

hist_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_",
  season_token,
  ".csv"
)

hist_df <- read.csv(hist_odds_path, stringsAsFactors = FALSE)
hist_df$commence_date_est <- as.Date(hist_df$commence_date_est)

keep_books <- c("fanduel", "draftkings", "betmgm", "fanatics")
book_suffix <- c(
  fanduel    = "FD",
  draftkings = "DK",
  betmgm     = "BMGM",
  fanatics   = "FNT"
)

# --- 2. Build game-level totals table with an order-insensitive key ----------

totals_long <- hist_df %>%
  filter(
    market == "totals",
    bookmaker %in% keep_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker],
    ou_flag = dplyr::case_when(
      tolower(outcome_team) %in% c("over", "o")  ~ "O",
      tolower(outcome_team) %in% c("under", "u") ~ "U",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ou_flag)) %>%
  # canonical pair key: alphabetical team order + date
  mutate(
    team_low  = pmin(espn_team_abv_home, espn_team_abv_away),
    team_high = pmax(espn_team_abv_home, espn_team_abv_away),
    pair_key  = paste0(team_low, "_", team_high, "_", commence_date_est)
  )

# Lines (total points)
totals_line_wide <- totals_long %>%
  mutate(
    line_col = paste0(book_code, "_total_", ou_flag)
  ) %>%
  select(
    pair_key,
    line_col,
    line_value = spread_or_total
  ) %>%
  tidyr::pivot_wider(
    id_cols    = pair_key,
    names_from = line_col,
    values_from = line_value,
    values_fn  = function(x) x[1]
  )

# Odds for Over/Under
totals_odds_wide <- totals_long %>%
  mutate(
    odds_col = paste0(book_code, "_total_odds_", ou_flag)
  ) %>%
  select(
    pair_key,
    odds_col,
    odds_value = odds
  ) %>%
  tidyr::pivot_wider(
    id_cols    = pair_key,
    names_from = odds_col,
    values_from = odds_value,
    values_fn  = function(x) x[1]
  )

totals_wide_for_patch_sched <- totals_line_wide %>%
  full_join(totals_odds_wide, by = "pair_key")

total_cols <- setdiff(names(totals_wide_for_patch_sched), "pair_key")

# --- 3. Helper: patch totals into a generic df (nba_schedule / current_slate) -

patch_totals_into_df <- function(df) {
  df <- df %>%
    mutate(
      game_date   = as.Date(game_date),
      game_date_c = format(game_date, "%Y-%m-%d"),
      # infer home/away abbreviations from existing columns
      home_abv = if_else(is_home == 1, team, opp),
      away_abv = if_else(is_home == 1, opp,  team),
      # order-insensitive pair key
      team_low  = pmin(home_abv, away_abv),
      team_high = pmax(home_abv, away_abv),
      pair_key  = paste0(team_low, "_", team_high, "_", game_date_c)
    )
  
  # align rows by pair_key
  idx <- match(df$pair_key, totals_wide_for_patch_sched$pair_key)
  
  for (col in total_cols) {
    # numeric cast for safety
    new_vals_num <- suppressWarnings(as.numeric(totals_wide_for_patch_sched[[col]][idx]))
    
    # create column if it doesn't exist
    if (!col %in% names(df)) {
      df[[col]] <- new_vals_num
    } else {
      existing_num <- suppressWarnings(as.numeric(df[[col]]))
      # patch only where existing is NA and we have a non-NA new value
      df[[col]] <- dplyr::if_else(
        is.na(existing_num) & !is.na(new_vals_num),
        new_vals_num,
        existing_num
      )
    }
  }
  
  df
}

# --- 4. Apply totals patch to nba_schedule and current_slate -----------------

nba_schedule <- patch_totals_into_df(nba_schedule)
current_slate <- patch_totals_into_df(current_slate)

# --- 5. Build team-level spreads + h2h from hist_df (team-centric) ----------

# spreads
spread_wide_sched <- hist_df %>%
  filter(
    market == "spreads",
    bookmaker %in% keep_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker]
  ) %>%
  select(
    espn_matchup_key,
    outcome_team_abv,
    book_code,
    spread_line = spread_or_total
  ) %>%
  distinct(espn_matchup_key, outcome_team_abv, book_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols     = c(espn_matchup_key, outcome_team_abv),
    names_from  = book_code,
    values_from = spread_line,
    names_glue  = "{book_code}_spread"
  )

# h2h
h2h_wide_sched <- hist_df %>%
  filter(
    market == "h2h",
    bookmaker %in% keep_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker]
  ) %>%
  select(
    espn_matchup_key,
    outcome_team_abv,
    book_code,
    h2h_odds = odds
  ) %>%
  distinct(espn_matchup_key, outcome_team_abv, book_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols     = c(espn_matchup_key, outcome_team_abv),
    names_from  = book_code,
    values_from = h2h_odds,
    names_glue  = "{book_code}_h2h"
  )

# combine into one table and add team_key
odds_by_team_sched <- spread_wide_sched %>%
  full_join(h2h_wide_sched, by = c("espn_matchup_key", "outcome_team_abv")) %>%
  mutate(
    team_key = paste0(espn_matchup_key, "||", outcome_team_abv)
  )

odds_cols_team <- setdiff(
  names(odds_by_team_sched),
  c("espn_matchup_key", "outcome_team_abv", "team_key")
)

# --- 6. Helper: patch spreads + h2h into a generic df -----------------------

patch_spreads_h2h_into_df <- function(df) {
  df <- df %>%
    mutate(
      game_date   = as.Date(game_date),
      game_date_c = format(game_date, "%Y-%m-%d"),
      oddsapi_map = paste0(team, "_", opp, "_", game_date_c),
      team_key    = paste0(oddsapi_map, "||", team)
    )
  
  idx_odds <- match(df$team_key, odds_by_team_sched$team_key)
  
  for (col in odds_cols_team) {
    new_vals_num <- suppressWarnings(as.numeric(odds_by_team_sched[[col]][idx_odds]))
    
    if (!col %in% names(df)) {
      df[[col]] <- new_vals_num
    } else {
      existing_num <- suppressWarnings(as.numeric(df[[col]]))
      
      df[[col]] <- dplyr::if_else(
        is.na(existing_num) & !is.na(new_vals_num),
        new_vals_num,
        existing_num
      )
    }
  }
  
  df$team_key <- NULL
  df
}

# --- 7. Apply spreads + h2h patch to nba_schedule and current_slate ---------

nba_schedule <- patch_spreads_h2h_into_df(nba_schedule)
current_slate <- patch_spreads_h2h_into_df(current_slate)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 6. END: Map Odds API TOTALS + SPREAD/H2H into nba_schedule & current_slate ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 7. START: Map Odds API odds back into BaseStats_Team ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# 1) Build odds_api_map on BaseStats_Team
#-------------------------------------------------------------------------------

BaseStats_Team <- BaseStats_Team %>%
  mutate(
    game_date   = as.Date(game_date),
    game_date_c = format(game_date, "%Y-%m-%d"),
    odds_api_map = paste0(team, "_", opp, "_", game_date_c)
  )

#-------------------------------------------------------------------------------
# 2) Helper lookups for full_df
#-------------------------------------------------------------------------------

target_books <- c("draftkings", "fanduel", "betmgm", "fanatics")
book_suffix  <- c(
  draftkings = "DK",
  fanduel    = "FD",
  betmgm     = "BMGM",
  fanatics   = "FNT"
)

market_col <- intersect(c("market_key", "market", "bet_type"), names(full_df))[1]
if (length(market_col) == 0 || is.na(market_col)) {
  stop("Could not find a market column in full_df (expected one of: market_key, market, bet_type).")
}

side_col <- intersect(c("over_under", "side", "label", "name", "selection"), names(full_df))[1]
if (length(side_col) == 0 || is.na(side_col)) {
  side_col <- NA_character_
}

#-------------------------------------------------------------------------------
# 3) Spreads: DK_spread, FD_spread, FNT_spread, BMGM_spread
#-------------------------------------------------------------------------------

spread_wide <- full_df %>%
  filter(
    .data[[market_col]] == "spreads",
    bookmaker %in% target_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker]
  ) %>%
  select(
    espn_matchup_key,
    outcome_team_abv,
    book_code,
    spread_line = spread_or_total
  ) %>%
  distinct(espn_matchup_key, outcome_team_abv, book_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols     = c(espn_matchup_key, outcome_team_abv),
    names_from  = book_code,
    values_from = spread_line,
    names_glue  = "{book_code}_spread"
  )

#-------------------------------------------------------------------------------
# 4) H2H: DK_h2h, FD_h2h, FNT_h2h, BMGM_h2h
#-------------------------------------------------------------------------------

h2h_wide <- full_df %>%
  filter(
    .data[[market_col]] == "h2h",
    bookmaker %in% target_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker]
  ) %>%
  select(
    espn_matchup_key,
    outcome_team_abv,
    book_code,
    h2h_odds = odds
  ) %>%
  distinct(espn_matchup_key, outcome_team_abv, book_code, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols     = c(espn_matchup_key, outcome_team_abv),
    names_from  = book_code,
    values_from = h2h_odds,
    names_glue  = "{book_code}_h2h"
  )

#-------------------------------------------------------------------------------
# 5) Combine spreads + h2h into one odds-by-team table, then PATCH (no join)
#-------------------------------------------------------------------------------

odds_by_team <- spread_wide %>%
  full_join(h2h_wide, by = c("espn_matchup_key", "outcome_team_abv")) %>%
  mutate(
    team_key = paste0(espn_matchup_key, "||", outcome_team_abv)
  )

odds_cols <- setdiff(
  names(odds_by_team),
  c("espn_matchup_key", "outcome_team_abv", "team_key")
)

# Build matching key on BaseStats_Team
BaseStats_Team <- BaseStats_Team %>%
  mutate(
    team_key = paste0(odds_api_map, "||", team)
  )

idx_odds <- match(BaseStats_Team$team_key, odds_by_team$team_key)

for (col in odds_cols) {
  # cast new values to numeric
  new_vals_num <- suppressWarnings(as.numeric(odds_by_team[[col]][idx_odds]))
  
  if (!col %in% names(BaseStats_Team)) {
    # brand new column: numeric
    BaseStats_Team[[col]] <- new_vals_num
  } else {
    # existing column: coerce existing to numeric too, then patch
    existing_num <- suppressWarnings(as.numeric(BaseStats_Team[[col]]))
    
    BaseStats_Team[[col]] <- dplyr::if_else(
      is.na(existing_num) & !is.na(new_vals_num),
      new_vals_num,
      existing_num
    )
  }
}

# we don't need team_key anymore
BaseStats_Team$team_key <- NULL

#===============================================================================
# 7) Normalize types BEFORE generating win/loss flags
#===============================================================================

BaseStats_Team <- BaseStats_Team %>%
  mutate(
    # Do NOT alter team_winner — keep TRUE/FALSE
    # ensure FD/DK/FNT/BMGM are numeric
    FD_h2h   = suppressWarnings(as.numeric(FD_h2h)),
    DK_h2h   = suppressWarnings(as.numeric(DK_h2h)),
    FNT_h2h  = suppressWarnings(as.numeric(FNT_h2h)),
    BMGM_h2h = suppressWarnings(as.numeric(BMGM_h2h)),
    
    is_home = suppressWarnings(as.integer(is_home)),
    is_away = suppressWarnings(as.integer(is_away))
  )

#===============================================================================
# 8) Outcome classification flags (dog/fav, home/away)
#===============================================================================

BaseStats_Team <- BaseStats_Team %>%
  mutate(
    # --- Dog/Favorite wins & losses ---
    dog_win  = if_else(FD_h2h > 0  & team_winner == TRUE, 1, 0),
    fav_win  = if_else(FD_h2h < 0  & team_winner == TRUE, 1, 0),
    dog_loss = if_else(FD_h2h > 0  & team_winner == FALSE, 1, 0),
    fav_loss = if_else(FD_h2h < 0  & team_winner == FALSE, 1, 0),
    
    # --- Home/Away wins & losses ---
    home_win  = if_else(is_home == 1 & team_winner == TRUE, 1, 0),
    home_loss = if_else(is_home == 1 & team_winner == FALSE, 1, 0),
    away_win  = if_else(is_away == 1 & team_winner == TRUE, 1, 0),
    away_loss = if_else(is_away == 1 & team_winner == FALSE, 1, 0),
    
    # --- Home/Away Dog/Fav combos ---
    home_dog_win  = if_else(FD_h2h > 0 & is_home == 1 & team_winner == TRUE, 1, 0),
    home_fav_loss = if_else(FD_h2h < 0 & is_home == 1 & team_winner == TRUE, 1, 0),
    
    away_dog_win  = if_else(FD_h2h > 0 & is_away == 1 & team_winner == TRUE, 1, 0),
    away_fav_loss = if_else(FD_h2h < 0 & is_away == 1 & team_winner == TRUE, 1, 0)
  )


#===============================================================================
# 9) Patch totals (Over/Under line + odds) into BaseStats_Team (game-level)
#     Using order-insensitive pair_key, then patching instead of join.
#===============================================================================

# 9.1 Build game-level totals table from full_df (pair_key-based)
totals_long <- full_df %>%
  filter(
    .data[[market_col]] == "totals",
    bookmaker %in% target_books
  ) %>%
  mutate(
    book_code = book_suffix[bookmaker],
    ou_flag = dplyr::case_when(
      tolower(outcome_team) %in% c("over", "o")  ~ "O",
      tolower(outcome_team) %in% c("under", "u") ~ "U",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ou_flag)) %>%
  mutate(
    team_low  = pmin(espn_team_abv_home, espn_team_abv_away),
    team_high = pmax(espn_team_abv_home, espn_team_abv_away),
    pair_key  = paste0(team_low, "_", team_high, "_", commence_date_est)
  )

# Lines (total points)
totals_line_wide <- totals_long %>%
  mutate(
    line_col = paste0(book_code, "_total_", ou_flag)
  ) %>%
  select(
    pair_key,
    line_col,
    line_value = spread_or_total
  ) %>%
  tidyr::pivot_wider(
    id_cols    = pair_key,
    names_from = line_col,
    values_from = line_value,
    values_fn  = function(x) x[1]
  )

# Odds for Over/Under
totals_odds_wide <- totals_long %>%
  mutate(
    odds_col = paste0(book_code, "_total_odds_", ou_flag)
  ) %>%
  select(
    pair_key,
    odds_col,
    odds_value = odds
  ) %>%
  tidyr::pivot_wider(
    id_cols    = pair_key,
    names_from = odds_col,
    values_from = odds_value,
    values_fn  = function(x) x[1]
  )

totals_wide_for_patch <- totals_line_wide %>%
  full_join(totals_odds_wide, by = "pair_key")

total_cols <- setdiff(names(totals_wide_for_patch), "pair_key")

# 9.2 Build matching pair_key in BaseStats_Team and patch totals in
BaseStats_Team <- BaseStats_Team %>%
  mutate(
    game_date   = as.Date(game_date),
    game_date_c = format(game_date, "%Y-%m-%d"),
    home_abv = if_else(is_home == 1, team, opp),
    away_abv = if_else(is_home == 1, opp,  team),
    team_low  = pmin(home_abv, away_abv),
    team_high = pmax(home_abv, away_abv),
    pair_key  = paste0(team_low, "_", team_high, "_", game_date_c)
  )

idx_tot <- match(BaseStats_Team$pair_key, totals_wide_for_patch$pair_key)

for (col in total_cols) {
  # cast new values to numeric
  new_vals_num <- suppressWarnings(as.numeric(totals_wide_for_patch[[col]][idx_tot]))
  
  if (!col %in% names(BaseStats_Team)) {
    BaseStats_Team[[col]] <- new_vals_num
  } else {
    existing_num <- suppressWarnings(as.numeric(BaseStats_Team[[col]]))
    
    BaseStats_Team[[col]] <- dplyr::if_else(
      is.na(existing_num) & !is.na(new_vals_num),
      new_vals_num,
      existing_num
    )
  }
}

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 7. END: Map Odds API odds back into BaseStats_Team ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 8. START: Write nba_schedule to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- 10. Export nba_schedule by APPENDING to the specified path ---

# ---- Overwrite schedule file every time ----
nba_schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_",
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


# ---- Write CURRENT SLATE ----
current_slate_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate/nba_schedule_",
  formatted_date,
  ".csv"
)

fwrite(
  current_slate,
  file = current_slate_path,
  append = FALSE,
  col.names = TRUE
)

cat("✔ current_slate written to:\n", current_slate_path, "\n")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 8. END: Write nba_schedule to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 9. START: Format and write BaseStats_Team to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


BaseStats_Team <- BaseStats_Team %>%
  # 1. Remove intermediate helper columns
  select(
    -team_low,
    -team_high,
    -home_abv,
    -away_abv
  ) %>%
  # 2. Move game_matchup_key and odds_api_map to end of dataset
  relocate(
    pair_key, odds_api_map,
    .after = last_col()
  )


# ---- Write BaseStats_Team ----
basestats_team_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token,
  ".csv"
)

fwrite(
  BaseStats_Team,
  file      = basestats_team_path,
  append    = FALSE,   # overwrite for each season
  col.names = TRUE
)

cat("✓ BaseStats_Team written to:\n", basestats_team_path, "\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 9. END: Write BaseStats_Team to csv ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀