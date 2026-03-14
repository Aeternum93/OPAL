# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#   BuildNBAPBP_v2.R
#   — Zero ESPN dependency version —
#   PBP sourced from NBA Stats API via nba_pbp() (PlayByPlayV3)
#   Game IDs sourced from nba_teamgamelog() 
#   Schedule sourced from local .rds
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Setup ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

rm(list = setdiff(ls(), c(
  "current_date", "formatted_date", "formatted_year", "next_year_date",
  "pbp_season", "season_token", "season_token2",
  "base_path", "logo_dir", "essential_names",
  "scripts", "batches", "run_script", "run_batch"
)))

library(hoopR)
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)

base_data_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR"

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Setup ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Load existing PBP + schedule, get all NBA game IDs ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 1.1 Load existing PBP file
nba_pbp_path <- file.path(base_data_path, "7. MC PBP Data", paste0("nbaapi_nbapbp_", season_token, ".csv"))

if (file.exists(nba_pbp_path) && file.info(nba_pbp_path)$size > 0) {
  nbapbp_df <- data.table::fread(nba_pbp_path, stringsAsFactors = FALSE)
  nbapbp_df <- as.data.frame(nbapbp_df)
  if ("game_date" %in% names(nbapbp_df)) nbapbp_df$game_date <- as.Date(nbapbp_df$game_date)
  if ("nba_game_id" %in% names(nbapbp_df)) nbapbp_df$nba_game_id <- as.character(nbapbp_df$nba_game_id)
  message("Loaded existing PBP: ", nrow(nbapbp_df), " rows.")
} else {
  nbapbp_df <- data.frame()
  message("No existing PBP file found — will build from scratch.")
}

# 1.2 Load schedule to get game context (home/away teams, dates, etc.)
schedule_rds_path <- file.path(base_data_path, "10. NBA Schedule", paste0("nba_schedule_", season_token, ".rds"))
schedule_csv_path <- file.path(base_data_path, "10. NBA Schedule", paste0("nba_schedule_", season_token, ".csv"))

if (file.exists(schedule_rds_path)) {
  nba_schedule <- readRDS(schedule_rds_path)
} else {
  nba_schedule <- read.csv(schedule_csv_path, stringsAsFactors = FALSE)
}
nba_schedule$game_date <- as.Date(nba_schedule$game_date)

# 1.3 Collect all NBA game IDs from team game logs
#     (This gives us the 10-digit NBA game IDs for every completed game)
nba_teams_list <- nba_teams()
all_nba_game_ids <- data.frame()

for (tid in nba_teams_list$team_id) {
  message("Getting game IDs for team_id: ", tid)
  
  tgl <- tryCatch({
    nba_teamgamelog(
      date_from   = "",
      date_to     = "",
      league_id   = "00",
      season      = season_token2,
      season_type = "Regular Season",
      team_id     = tid
    )
  }, error = function(e) {
    message("Error for team_id: ", tid, " -> ", e$message)
    return(NULL)
  })
  
  if (!is.null(tgl) && nrow(tgl$TeamGameLog) > 0) {
    log_data <- tgl$TeamGameLog %>%
      dplyr::mutate(
        game_date   = as.Date(GAME_DATE, format = "%b %d, %Y"),
        nba_game_id = as.character(Game_ID),
        nba_team_id = as.character(Team_ID),
        home_away   = ifelse(grepl("vs\\.", MATCHUP), "home", "away"),
        team_abbrev = trimws(sub("^(\\S+)\\s+(vs\\.|@).*", "\\1", MATCHUP)),
        opp_abbrev  = trimws(sub(".*(?:vs\\.|@)\\s+(\\S+)$", "\\1", MATCHUP))
      ) %>%
      dplyr::select(nba_game_id, nba_team_id, game_date, home_away, team_abbrev, opp_abbrev)
    
    all_nba_game_ids <- dplyr::bind_rows(all_nba_game_ids, log_data)
  }
  
  Sys.sleep(0.50)
}

# Deduplicate to get one row per game (take the home team's row)
game_id_lookup <- all_nba_game_ids %>%
  dplyr::filter(home_away == "home") %>%
  dplyr::distinct(nba_game_id, .keep_all = TRUE) %>%
  dplyr::rename(
    home_team_abbrev = team_abbrev,
    away_team_abbrev = opp_abbrev
  ) %>%
  dplyr::select(nba_game_id, game_date, home_team_abbrev, away_team_abbrev)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Load existing PBP + get game IDs ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Identify missing games and pull PBP from NBA Stats API ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 2.1 Determine which game IDs we already have PBP for
have_game_ids <- if (nrow(nbapbp_df) > 0 && "nba_game_id" %in% names(nbapbp_df)) {
  unique(nbapbp_df$nba_game_id)
} else {
  character(0)
}

# All completed NBA game IDs
all_completed_ids <- unique(game_id_lookup$nba_game_id)

# Games we need to pull
missing_ids <- setdiff(all_completed_ids, have_game_ids)
message("Total completed games: ", length(all_completed_ids),
        " | Already have: ", length(have_game_ids),
        " | Need to pull: ", length(missing_ids))

# 2.2 Pull PBP for missing games using NBA CDN directly
#     hoopR::nba_pbp() is broken, so we hit the CDN endpoint ourselves
#     URL: https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_{game_id}.json
if (length(missing_ids) > 0) {
  
  new_pbp_rows <- list()
  
  for (idx in seq_along(missing_ids)) {
    gid <- missing_ids[idx]
    
    message("Pulling PBP for nba_game_id: ", gid, " (", idx, " of ", length(missing_ids), ")")
    
    pbp_raw <- tryCatch({
      url <- paste0("https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_", gid, ".json")
      resp <- jsonlite::fromJSON(url)
      resp$game$actions
    }, error = function(e) {
      message("  Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(pbp_raw) && nrow(pbp_raw) > 0) {
      
      # Get game context from our lookup
      game_ctx <- game_id_lookup %>% dplyr::filter(nba_game_id == gid)
      
      g_date          <- if (nrow(game_ctx) > 0) game_ctx$game_date[1]          else NA
      g_home_abbrev   <- if (nrow(game_ctx) > 0) game_ctx$home_team_abbrev[1]   else NA
      g_away_abbrev   <- if (nrow(game_ctx) > 0) game_ctx$away_team_abbrev[1]   else NA
      
      # Map NBA CDN PBP columns → ESPN-style column names
      # CDN fields: actionNumber, clock, timeActual, period, actionType, subType,
      #   personId, xLegacy, yLegacy, isFieldGoal, scoreHome, scoreAway,
      #   description, teamId, teamTricode, pointsTotal, shotDistance, shotResult,
      #   playerName, playerNameI, assistPersonId, stealPersonId, blockPersonId, etc.
      
      pbp_mapped <- pbp_raw %>%
        dplyr::mutate(
          # Core play info
          game_play_number = dplyr::row_number(),
          id               = as.character(actionNumber),
          sequence_number  = as.integer(orderNumber),
          type_id          = NA_integer_,
          type_text        = as.character(actionType),
          text             = as.character(description),
          away_score       = as.integer(scoreAway),
          home_score       = as.integer(scoreHome),
          
          # Period / clock
          period_number        = as.integer(period),
          period_display_value = paste0("Quarter ", period),
          clock_display_value  = as.character(clock),
          
          # Scoring
          scoring_play = ifelse(!is.na(pointsTotal) & as.numeric(pointsTotal) > 0, TRUE, FALSE),
          score_value  = ifelse(!is.na(pointsTotal), as.integer(pointsTotal), 0L),
          
          # IDs — safely handle optional columns that may not exist in every game
          team_id_mapped = as.character(teamId),
          athlete_id_1   = as.character(personId),
          athlete_id_2   = NA_character_,
          athlete_id_3   = NA_character_,
          
          # Timestamps — CDN has timeActual
          wallclock = as.character(timeActual),
          
          # Shot info
          shooting_play    = ifelse(!is.na(isFieldGoal) & isFieldGoal == 1, TRUE, FALSE),
          coordinate_x_raw = as.numeric(xLegacy),
          coordinate_y_raw = as.numeric(yLegacy),
          points_attempted = dplyr::case_when(
            !shooting_play ~ NA_integer_,
            grepl("3pt|3PT|3-Point|Three Point", actionType, ignore.case = TRUE) ~ 3L,
            TRUE ~ 2L
          ),
          short_description = as.character(subType),
          
          # Game context
          game_id             = gid,
          nba_game_id         = gid,
          season              = as.integer(pbp_season),
          season_type         = 2L,
          home_team_id        = NA_character_,
          home_team_name      = NA_character_,
          home_team_mascot    = NA_character_,
          home_team_abbrev    = g_home_abbrev,
          home_team_name_alt  = NA_character_,
          away_team_id        = NA_character_,
          away_team_name      = NA_character_,
          away_team_mascot    = NA_character_,
          away_team_abbrev    = g_away_abbrev,
          away_team_name_alt  = NA_character_,
          
          # Spread info (not available from NBA CDN)
          game_spread           = NA_real_,
          home_favorite         = NA,
          game_spread_available = FALSE,
          home_team_spread      = NA_real_,
          
          # Derived time fields
          qtr           = as.integer(period),
          time          = as.character(clock),
          clock_minutes = as.integer(sub("^PT(\\d+)M.*", "\\1", clock)),
          clock_seconds = as.integer(sub(".*M(\\d+)\\..*", "\\1", clock)),
          
          # Timeout flags
          home_timeout_called = ifelse(grepl("timeout", actionType, ignore.case = TRUE) &
                                         teamTricode == g_home_abbrev, TRUE, FALSE),
          away_timeout_called = ifelse(grepl("timeout", actionType, ignore.case = TRUE) &
                                         teamTricode == g_away_abbrev, TRUE, FALSE),
          
          # Half / quarter tracking
          half      = ifelse(period <= 2, 1L, 2L),
          game_half = ifelse(period <= 2, "Half 1", "Half 2"),
          lead_qtr  = NA_character_,
          lead_half = NA_character_,
          
          # Seconds remaining (12 min quarters = 720 seconds)
          start_quarter_seconds_remaining = ifelse(!is.na(clock_minutes) & !is.na(clock_seconds),
                                                   clock_minutes * 60L + clock_seconds, NA_integer_),
          start_half_seconds_remaining    = ifelse(!is.na(start_quarter_seconds_remaining),
                                                   start_quarter_seconds_remaining + ifelse(period %% 2 == 1, 720L, 0L),
                                                   NA_integer_),
          start_game_seconds_remaining    = ifelse(!is.na(start_quarter_seconds_remaining),
                                                   start_quarter_seconds_remaining + (4L - as.integer(period)) * 720L,
                                                   NA_integer_),
          end_quarter_seconds_remaining   = NA_integer_,
          end_half_seconds_remaining      = NA_integer_,
          end_game_seconds_remaining      = NA_integer_,
          
          # Period (keep as-is for compatibility)
          period = as.integer(period),
          
          # Lag columns
          lag_qtr  = NA_character_,
          lag_half = NA_character_,
          
          # Coordinates (xLegacy/yLegacy from CDN)
          coordinate_x = as.numeric(xLegacy),
          coordinate_y = as.numeric(yLegacy),
          
          # Game date
          game_date      = g_date,
          game_date_time = as.character(timeActual)
        )
      
      # Safely populate athlete_id_2 and athlete_id_3 from optional CDN columns
      if ("assistPersonId" %in% names(pbp_raw)) {
        pbp_mapped$athlete_id_2 <- ifelse(!is.na(pbp_raw$assistPersonId) & pbp_raw$assistPersonId != 0,
                                          as.character(pbp_raw$assistPersonId), pbp_mapped$athlete_id_2)
      }
      if ("stealPersonId" %in% names(pbp_raw)) {
        pbp_mapped$athlete_id_2 <- ifelse(is.na(pbp_mapped$athlete_id_2) & !is.na(pbp_raw$stealPersonId) & pbp_raw$stealPersonId != 0,
                                          as.character(pbp_raw$stealPersonId), pbp_mapped$athlete_id_2)
      }
      if ("blockPersonId" %in% names(pbp_raw)) {
        pbp_mapped$athlete_id_3 <- ifelse(!is.na(pbp_raw$blockPersonId) & pbp_raw$blockPersonId != 0,
                                          as.character(pbp_raw$blockPersonId), pbp_mapped$athlete_id_3)
      }
      if ("foulDrawnPersonId" %in% names(pbp_raw)) {
        pbp_mapped$athlete_id_3 <- ifelse(is.na(pbp_mapped$athlete_id_3) & !is.na(pbp_raw$foulDrawnPersonId) & pbp_raw$foulDrawnPersonId != 0,
                                          as.character(pbp_raw$foulDrawnPersonId), pbp_mapped$athlete_id_3)
      }
      
      pbp_mapped <- pbp_mapped %>%
        dplyr::select(
          game_play_number, id, sequence_number, type_id, type_text, text,
          away_score, home_score, period_number, period_display_value,
          clock_display_value, scoring_play, score_value,
          team_id = team_id_mapped,
          athlete_id_1, athlete_id_2, athlete_id_3, wallclock,
          shooting_play, coordinate_x_raw, coordinate_y_raw,
          points_attempted, short_description, game_id, season, season_type,
          home_team_id, home_team_name, home_team_mascot, home_team_abbrev,
          home_team_name_alt, away_team_id, away_team_name, away_team_mascot,
          away_team_abbrev, away_team_name_alt,
          game_spread, home_favorite, game_spread_available, home_team_spread,
          qtr, time, clock_minutes, clock_seconds,
          home_timeout_called, away_timeout_called,
          half, game_half, lead_qtr, lead_half,
          start_quarter_seconds_remaining, start_half_seconds_remaining,
          start_game_seconds_remaining, end_quarter_seconds_remaining,
          end_half_seconds_remaining, end_game_seconds_remaining,
          period, lag_qtr, lag_half,
          coordinate_x, coordinate_y, game_date, game_date_time,
          nba_game_id
        )
      
      new_pbp_rows[[length(new_pbp_rows) + 1]] <- pbp_mapped
    }
    
    Sys.sleep(0.60)
  }
  
  # 2.3 Combine all new PBP rows
  if (length(new_pbp_rows) > 0) {
    new_pbp_df <- data.table::rbindlist(new_pbp_rows, use.names = TRUE, fill = TRUE)
    new_pbp_df <- as.data.frame(new_pbp_df)
    message("Pulled PBP for ", length(new_pbp_rows), " new games (",  nrow(new_pbp_df), " total plays).")
  } else {
    new_pbp_df <- data.frame()
    message("No new PBP data pulled.")
  }
  
} else {
  new_pbp_df <- data.frame()
  message("All games already have PBP — nothing to pull.")
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Pull PBP from NBA Stats API ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Append new PBP to existing and write to CSV ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 3.1 Safe append function (same pattern as your v1)
stack_as_char <- function(a, b) {
  a <- data.table::as.data.table(a)
  b <- data.table::as.data.table(b)
  
  cols <- union(names(a), names(b))
  for (cn in setdiff(cols, names(a))) a[, (cn) := NA_character_]
  for (cn in setdiff(cols, names(b))) b[, (cn) := NA_character_]
  data.table::setcolorder(a, cols)
  data.table::setcolorder(b, cols)
  
  for (cn in cols) {
    data.table::set(a, j = cn, value = as.character(a[[cn]]))
    data.table::set(b, j = cn, value = as.character(b[[cn]]))
  }
  
  data.table::rbindlist(list(a, b), use.names = TRUE, fill = TRUE)
}

# 3.2 Append new rows
if (nrow(new_pbp_df) > 0) {
  nbapbp_df <- stack_as_char(nbapbp_df, new_pbp_df)
  message("Appended ", nrow(new_pbp_df), " new rows. Total PBP: ", nrow(nbapbp_df))
} else {
  message("Nothing to append.")
  nbapbp_df <- data.table::as.data.table(nbapbp_df)
}

# 3.3 Cast key columns back
if ("game_date"    %in% names(nbapbp_df)) nbapbp_df[, game_date    := as.Date(game_date)]
if ("game_id"      %in% names(nbapbp_df)) nbapbp_df[, game_id      := as.character(game_id)]
if ("nba_game_id"  %in% names(nbapbp_df)) nbapbp_df[, nba_game_id  := as.character(nba_game_id)]

# 3.4 Normalize blanks to NA, drop fully-empty rows
nbapbp_df <- as.data.frame(nbapbp_df) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(trimws(.), ""))) %>%
  dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

# 3.5 Write combined file
data.table::fwrite(
  nbapbp_df,
  file.path(base_data_path, "7. MC PBP Data", paste0("nbaapi_nbapbp_", season_token, ".csv"))
)
message("PBP file written: ", nrow(nbapbp_df), " total rows.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Write PBP to CSV ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

rm(new_pbp_rows, new_pbp_df, pbp_raw, pbp_mapped, game_ctx,
   all_nba_game_ids, game_id_lookup, nba_teams_list, nba_schedule)