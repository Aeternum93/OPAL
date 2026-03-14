# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#   BuildNBAPBP_espn_direct.R
#   — Replaces load_nba_pbp() with direct espn_nba_pbp() calls —
#   Outputs exact 64-column structure matching load_nba_pbp() format
#   PBP source of truth — no dependency on hoopR data repo
#
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
library(jsonlite)

base_data_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR"
pbp_path <- file.path(base_data_path, "7. MC PBP Data", paste0("nbapbp_", season_token, ".csv"))

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 1. Load existing PBP and find missing dates ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

if (file.exists(pbp_path) && file.info(pbp_path)$size > 0) {
  nbapbp_df <- data.table::fread(pbp_path, stringsAsFactors = FALSE)
  nbapbp_df <- as.data.frame(nbapbp_df)
  nbapbp_df$game_date <- as.Date(nbapbp_df$game_date)
  message("Loaded existing PBP: ", nrow(nbapbp_df), " rows, ",
          length(unique(nbapbp_df$game_id)), " unique games.")
} else {
  nbapbp_df <- data.frame()
  message("No existing PBP file — will build from scratch.")
}

have_dates <- if (nrow(nbapbp_df) > 0) unique(nbapbp_df$game_date) else as.Date(character(0))
max_have_date <- if (length(have_dates) > 0) max(have_dates) else as.Date("2025-10-01")
message("Latest date in existing PBP: ", max_have_date)

yesterday <- Sys.Date() - 1
dates_to_check <- seq.Date(from = max_have_date + 1, to = yesterday, by = "day")

if (length(dates_to_check) == 0) {
  message("PBP is already up to date through ", max_have_date, ". Nothing to pull.")
} else {
  message("Checking ", length(dates_to_check), " date(s): ",
          min(dates_to_check), " to ", max(dates_to_check))
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. For each missing date: hit ESPN scoreboard -> get game IDs -> pull PBP ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

new_pbp_rows <- list()

if (length(dates_to_check) > 0) {
  
  for (d in seq_along(dates_to_check)) {
    check_date <- dates_to_check[d]
    date_str <- format(check_date, "%Y%m%d")
    
    message("\n--- Date: ", check_date, " (", d, " of ", length(dates_to_check), ") ---")
    
    scoreboard <- tryCatch({
      url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?dates=", date_str)
      jsonlite::fromJSON(url)
    }, error = function(e) {
      message("  Scoreboard error: ", e$message)
      return(NULL)
    })
    
    if (is.null(scoreboard) || length(scoreboard$events$id) == 0) {
      message("  No games found for ", check_date)
      next
    }
    
    game_ids <- scoreboard$events$id
    message("  Found ", length(game_ids), " game(s): ", paste(game_ids, collapse = ", "))
    
    for (gid in game_ids) {
      if (nrow(nbapbp_df) > 0 && as.character(gid) %in% as.character(nbapbp_df$game_id)) {
        message("  Skipping ", gid, " -- already in file.")
        next
      }
      
      message("  Pulling PBP for game_id: ", gid)
      
      pbp <- tryCatch({
        espn_nba_pbp(game_id = gid)
      }, error = function(e) {
        message("    Error: ", e$message)
        return(NULL)
      })
      
      if (!is.null(pbp) && nrow(pbp) > 0) {
        new_pbp_rows[[length(new_pbp_rows) + 1]] <- pbp
        message("    Got ", nrow(pbp), " plays.")
      } else {
        message("    No PBP data returned.")
      }
      
      Sys.sleep(0.75)
    }
  }
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 3. Enrich, format columns, append and write ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

if (length(new_pbp_rows) > 0) {
  
  # 3a. Combine all new games
  new_pbp_df <- data.table::rbindlist(new_pbp_rows, use.names = TRUE, fill = TRUE)
  new_pbp_df <- as.data.frame(new_pbp_df)
  
  # 3b. Enrich with derived columns
  new_pbp_df <- new_pbp_df %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(
      game_play_number = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      home_team_name_alt = home_team_name,
      away_team_name_alt = away_team_name,
      
      qtr    = as.integer(period_number),
      period = as.integer(period_number),
      
      time          = as.character(clock_display_value),
      clock_minutes = as.integer(sub("^(\\d+):.*", "\\1", clock_display_value)),
      clock_seconds = as.integer(sub(".*:(\\d+)$", "\\1", clock_display_value)),
      
      home_timeout_called = ifelse(grepl("timeout", type_text, ignore.case = TRUE) &
                                     as.character(team_id) == as.character(home_team_id), TRUE, FALSE),
      away_timeout_called = ifelse(grepl("timeout", type_text, ignore.case = TRUE) &
                                     as.character(team_id) == as.character(away_team_id), TRUE, FALSE),
      
      half      = as.integer(ifelse(qtr <= 2, 1L, 2L)),
      game_half = as.integer(ifelse(qtr <= 2, 1L, 2L)),
      
      start_quarter_seconds_remaining = ifelse(!is.na(clock_minutes) & !is.na(clock_seconds),
                                               clock_minutes * 60L + clock_seconds, NA_integer_),
      start_half_seconds_remaining    = ifelse(!is.na(start_quarter_seconds_remaining),
                                               start_quarter_seconds_remaining +
                                                 ifelse(qtr %in% c(1L, 3L), 720L, 0L),
                                               NA_integer_),
      start_game_seconds_remaining    = ifelse(!is.na(start_quarter_seconds_remaining),
                                               start_quarter_seconds_remaining +
                                                 (4L - qtr) * 720L,
                                               NA_integer_)
    ) %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(
      end_quarter_seconds_remaining = dplyr::lead(start_quarter_seconds_remaining),
      end_half_seconds_remaining    = dplyr::lead(start_half_seconds_remaining),
      end_game_seconds_remaining    = dplyr::lead(start_game_seconds_remaining),
      lead_qtr  = as.integer(dplyr::lead(qtr)),
      lead_half = as.integer(dplyr::lead(half)),
      lag_qtr   = as.integer(dplyr::lag(qtr)),
      lag_half  = as.integer(dplyr::lag(half))
    ) %>%
    dplyr::ungroup()
  
  # 3c. Select the 64 base columns, then add downstream placeholders as NA
  base_cols <- c(
    "game_play_number", "id", "sequence_number", "type_id", "type_text", "text",
    "away_score", "home_score", "period_number", "period_display_value",
    "clock_display_value", "scoring_play", "score_value", "team_id",
    "athlete_id_1", "athlete_id_2", "athlete_id_3", "wallclock",
    "shooting_play", "coordinate_x_raw", "coordinate_y_raw",
    "points_attempted", "short_description", "game_id", "season", "season_type",
    "home_team_id", "home_team_name", "home_team_mascot", "home_team_abbrev",
    "home_team_name_alt", "away_team_id", "away_team_name", "away_team_mascot",
    "away_team_abbrev", "away_team_name_alt",
    "game_spread", "home_favorite", "game_spread_available", "home_team_spread",
    "qtr", "time", "clock_minutes", "clock_seconds",
    "home_timeout_called", "away_timeout_called",
    "half", "game_half", "lead_qtr", "lead_half",
    "start_quarter_seconds_remaining", "start_half_seconds_remaining",
    "start_game_seconds_remaining", "end_quarter_seconds_remaining",
    "end_half_seconds_remaining", "end_game_seconds_remaining",
    "period", "lag_qtr", "lag_half",
    "coordinate_x", "coordinate_y", "game_date", "game_date_time"
  )
  
  # Fill any missing base columns with NA
  for (col in setdiff(base_cols, names(new_pbp_df))) new_pbp_df[[col]] <- NA
  new_pbp_df <- new_pbp_df[, base_cols]
  
  # Add downstream placeholder columns (populated by later scripts)
  new_pbp_df$type_abbreviation <- NA
  new_pbp_df$nba_game_id       <- NA_character_
  new_pbp_df$nba_team_id       <- NA_character_
  new_pbp_df$PLAYER_NAME       <- NA_character_
  new_pbp_df$SHOT_ZONE_BASIC   <- NA_character_
  new_pbp_df$SHOT_ZONE_AREA    <- NA_character_
  new_pbp_df$SHOT_ZONE_RANGE   <- NA_character_
  new_pbp_df$SHOT_DISTANCE     <- NA_integer_
  new_pbp_df$LOC_X             <- NA_integer_
  new_pbp_df$LOC_Y             <- NA_integer_
  
  message("\nEnriched ", nrow(new_pbp_df), " new plays across ", length(new_pbp_rows), " game(s).")
  
  # 3d. Append — keep existing data as-is, just align columns for bind
  if (nrow(nbapbp_df) > 0) {
    all_cols <- union(names(nbapbp_df), names(new_pbp_df))
    for (cn in setdiff(all_cols, names(nbapbp_df)))  nbapbp_df[[cn]]  <- NA
    for (cn in setdiff(all_cols, names(new_pbp_df)))  new_pbp_df[[cn]] <- NA
    new_pbp_df <- new_pbp_df[, names(nbapbp_df)]
    
    # Coerce to character for safe bind
    for (cn in names(nbapbp_df)) {
      nbapbp_df[[cn]]  <- as.character(nbapbp_df[[cn]])
      new_pbp_df[[cn]] <- as.character(new_pbp_df[[cn]])
    }
    
    nbapbp_df <- dplyr::bind_rows(nbapbp_df, new_pbp_df)
  } else {
    nbapbp_df <- new_pbp_df
  }
  
  # 3e. Write
  data.table::fwrite(nbapbp_df, pbp_path)
  message("PBP file updated: ", nrow(nbapbp_df), " total rows. Saved to: ", pbp_path)
  
} else {
  message("\nNo new PBP data to append.")
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== Done ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

rm(new_pbp_rows, new_pbp_df, scoreboard, pbp)
message("ESPN direct PBP update complete.")