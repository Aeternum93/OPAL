# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Pull Shot Detail Chart from function for data aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

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


gc()
library(dplyr)
library(readr)
library(hoopR)
library(data.table)

# ---------------------------- Paths ----------------------------
base_stats_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_",
  season_token, ".csv"
)
output_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data"

# --------------------- Load BaseStats_Player -------------------
base_stats_player <- read_csv(
  base_stats_path,
  col_types = cols(
    espn_player_id = col_character(),
    espn_game_id   = col_character(),
    nba_game_id    = col_character(),
    nba_player_id  = col_character(),
    nba_team_id    = col_character(),
    espn_team_id   = col_character(),
    player_name    = col_character(),
    team_id        = col_character(),
    headshot       = col_character(),
    team_logo      = col_character(),
    opp_logo       = col_character(),
    game_date      = col_date()
  )
) %>% mutate(nba_game_id = paste0("00", nba_game_id))

game_dates <- base_stats_player %>% distinct(game_date) %>% arrange(game_date) %>% pull(game_date)



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Previous Data Load and Date Check Logic Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Replace your current game_dates assignment with this controller:

# 1) Dates present in BaseStats_Player (already updated)
PLAYER_DATES <- base_stats_player %>%
  distinct(game_date) %>%
  arrange(game_date) %>%
  pull(game_date)

# 2) Dates already tracked in closest-defender CSV (if it exists)
cdef_path <- file.path(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data",
  paste0("nba_closetdefendertracked_", season_token, ".csv")
)

if (file.exists(cdef_path)) {
  cdef_df <- data.table::fread(cdef_path, colClasses = "character")
  TRACKED_DATES <- unique(as.Date(cdef_df$game_date))
  TRACKED_DATES <- TRACKED_DATES[!is.na(TRACKED_DATES)]
} else {
  TRACKED_DATES <- as.Date(character())
}

# 3) Only pull dates that are in BaseStats_Player but NOT in tracked file
MISSING_DATES <- as.Date(setdiff(PLAYER_DATES, TRACKED_DATES), origin = "1970-01-01")

if (length(MISSING_DATES) == 0) {
  message("ClosestDefender tracked file is up to date — no API pulls needed.")
} else {
  message(sprintf("ClosestDefender missing dates to pull: %d", length(MISSING_DATES)))
}

# 4) Feed these into your main loop
game_dates <- sort(MISSING_DATES)


# --- Fix section before your main loop ---
as_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(as.numeric(x), origin = "1970-01-01"))
  suppressWarnings(as.Date(x))
}

PLAYER_DATES  <- as_date_any(base_stats_player$game_date)
if (file.exists(cdef_path)) {
  cdef_df <- data.table::fread(cdef_path, colClasses = "character")
  TRACKED_DATES <- as_date_any(cdef_df$game_date)
} else {
  TRACKED_DATES <- as.Date(character())
}

MISSING_DATES <- setdiff(
  as.Date(unique(PLAYER_DATES)),
  as.Date(unique(TRACKED_DATES))
)

game_dates <- sort(unique(as_date_any(MISSING_DATES)))

stopifnot(inherits(game_dates, "Date"))

MISSING_DATES <- as.Date(MISSING_DATES, origin = "1970-01-01")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Previous Data Load and Date Check Logic Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# -------------------------- Helpers ----------------------------
append_context_fields <- function(df, base_stats, current_date) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df$game_date <- current_date
  df$espn_game_id <- df$espn_player_id <- df$nba_game_id <- NA
  df$team <- df$team_logo <- df$team_score <- df$player_name <- df$headshot <- NA
  df$opp <- df$opp_logo <- df$opp_score <- df$team_winner <- df$home_away <- df$position <- NA
  
  for (i in seq_len(nrow(df))) {
    player_id <- as.character(df$PLAYER_ID[i])
    if (is.na(player_id) || player_id == "") next
    
    player_row <- base_stats %>%
      filter(!is.na(nba_player_id), nba_player_id == player_id, game_date == current_date)
    
    if (nrow(player_row) >= 1) {
      player_row <- dplyr::slice(player_row, 1)
      df$espn_game_id[i]   <- as.character(player_row$espn_game_id[[1]])
      df$espn_player_id[i] <- as.character(player_row$espn_player_id[[1]])
      df$nba_game_id[i]    <- as.character(player_row$nba_game_id[[1]])
      df$team[i]           <- as.character(player_row$team[[1]])
      df$nba_team_id[i]    <- as.character(player_row$nba_team_id[[1]])
      df$espn_team_id[i]   <- as.character(player_row$espn_team_id[[1]])
      df$team_logo[i]      <- as.character(player_row$team_logo[[1]])
      df$team_score[i]     <- as.character(player_row$team_score[[1]])
      df$player_name[i]    <- as.character(player_row$player_name[[1]])
      df$headshot[i]       <- as.character(player_row$headshot[[1]])
      df$opp[i]            <- as.character(player_row$opp[[1]])
      df$opp_logo[i]       <- as.character(player_row$opp_logo[[1]])
      df$opp_score[i]      <- as.character(player_row$opp_score[[1]])
      df$team_winner[i]    <- as.character(player_row$team_winner[[1]])
      df$home_away[i]      <- as.character(player_row$home_away[[1]])
      df$position[i]       <- as.character(player_row$position[[1]])
    }
  }
  df
}

parse_range_fields <- function(df, range_col, low_name, high_name, desc_name = NULL, file_name = NULL) {
  raw_vals <- df[[range_col]]
  df[[low_name]]  <- NA_character_
  df[[high_name]] <- NA_character_
  if (!is.null(desc_name)) df[[desc_name]] <- NA_character_
  
  for (i in seq_along(raw_vals)) {
    entry <- raw_vals[i]
    
    if (grepl("dribblestracked", file_name)) {
      clean <- gsub("Dribbles|Dribble|\\+", "", entry)
      parts <- unlist(strsplit(trimws(clean), "-"))
      if (length(parts) == 1) { df[[high_name]][i] <- trimws(parts[1]) }
      else if (length(parts) == 2) { df[[low_name]][i] <- trimws(parts[1]); df[[high_name]][i] <- trimws(parts[2]) }
      
    } else if (grepl("touchtimetracked", file_name)) {
      clean <- gsub("Touch|Seconds|\\+|<", "", entry)
      parts <- unlist(strsplit(trimws(clean), "-"))
      if (length(parts) == 1 && trimws(parts[1]) %in% c("2","6")) { df[[low_name]][i] <- "0"; df[[high_name]][i] <- trimws(parts[1]) }
      else if (length(parts) == 1 && grepl("1", parts[1])) { df[[low_name]][i] <- "0"; df[[high_name]][i] <- "2" }
      else if (length(parts) == 2) { df[[low_name]][i] <- trimws(parts[1]); df[[high_name]][i] <- trimws(parts[2]) }
      
    } else if (grepl("closetdefendertracked", file_name)) {
      nums <- unlist(regmatches(entry, gregexpr("[0-9]+", entry)))
      label_match <- regmatches(entry, regexpr("Very Tight|Tight|Open|Wide Open", entry))
      if (length(label_match) > 0) df[[desc_name]][i] <- trimws(label_match)
      if (length(nums) == 1 && nums[1] == "6") { df[[low_name]][i] <- "0"; df[[high_name]][i] <- "6" }
      else if (length(nums) == 2) { df[[low_name]][i] <- nums[1]; df[[high_name]][i] <- nums[2] }
      
    } else if (grepl("shotclocktracked", file_name)) {
      nums <- unlist(regmatches(entry, gregexpr("[0-9]+", entry)))
      label_match <- regmatches(entry, regexpr("Very Early|Early|Average|Late|Very Late", entry))
      if (length(label_match) > 0) df[[desc_name]][i] <- trimws(label_match)
      if (length(nums) == 1 && nums[1] == "6") { df[[low_name]][i] <- "0"; df[[high_name]][i] <- "6" }
      else if (length(nums) == 2) { df[[high_name]][i] <- nums[1]; df[[low_name]][i] <- nums[2] }
    }
  }
  
  df[[range_col]] <- NULL
  df
}

# returns a CLEAN DATA.TABLE; no writing here
cleanup_transform <- function(df, file_tag, range_col, low, high, desc = NULL) {
  if (is.null(df) || !nrow(df)) return(data.table())
  df <- df %>% rename(nba_player_id = PLAYER_ID) %>% select(-PLAYER_NAME_LAST_FIRST)
  df <- parse_range_fields(df, range_col, low, high, desc, file_tag)
  
  ordered_cols <- c(
    "nba_player_id", "espn_player_id", "player_name", "headshot", "game_date",
    "espn_game_id", "nba_game_id", "espn_team_id", "nba_team_id",
    "team", "team_logo", "team_score", "opp", "opp_logo", "opp_score",
    "team_winner", "home_away",
    if (!is.null(desc)) desc, low, high,
    "FGM", "FGA", "FG_PCT", "EFG_PCT", "FG2M", "FG2A", "FG2_PCT", "FG3M", "FG3A", "FG3_PCT"
  )
  df <- df[, intersect(ordered_cols, names(df)), drop = FALSE]
  as.data.table(df)
}

# ----------------- Season accumulators (data.frames) -------------
touch_all   <- data.table()
dribble_all <- data.table()
shotclk_all <- data.table()
cdef_all    <- data.table()

# --------------------------- MAIN -------------------------------
for (i in seq_along(game_dates)) {
  current_date <- game_dates[i]
  
  if (any(base_stats_player$game_date == current_date &
          base_stats_player$espn_game_id == "401705187")) {
    message(paste("Skipping date:", current_date, "- contains game 401705187")); next
  }
  
  print(sprintf("Processing date %s (%d/%d)", current_date, i, length(game_dates))); flush.console()
  
  pids <- base_stats_player %>%
    filter(game_date == current_date, !is.na(nba_player_id), nzchar(nba_player_id)) %>%
    distinct(nba_player_id) %>% pull(nba_player_id)
  
  if (!length(pids)) { message("  No players for this date."); next }
  
  # ---- per-player fetch -> transform -> append ----
  date_str <- format(current_date, "%Y-%m-%d")
  
  for (j in seq_along(pids)) {
    pid <- pids[j]
    print(sprintf("  → Player %d/%d | nba_player_id=%s | date=%s : fetching…", j, length(pids), pid, date_str))
    
    # Regular Season first (player_id + date window)
    resp <- tryCatch({
      nba_playerdashptshots(
        date_from = date_str, date_to = date_str,
        game_segment = "", last_n_games = 0,
        league_id = "00", location = "", month = 0, opponent_team_id = 0,
        outcome = "", per_mode = "Totals", period = 0,
        player_id = pid,
        season = season_token2, season_segment = "", season_type = "Regular Season",
        team_id = 0, vs_conference = "", vs_division = "",
        plus_minus = "", pace_adjust = "", rank = "",
        shot_clock_range = "", touch_time_range = "", dribble_range = "", general_range = ""
      )
    }, error = function(e) { message(sprintf("    RS error pid=%s : %s", pid, e$message)); NULL })
    
    # Fallback to Playoffs if everything empty/NULL
    if (is.null(resp) || (
      nrow(resp$TouchTimeShooting) == 0 &&
      nrow(resp$DribbleShooting)   == 0 &&
      nrow(resp$ShotClockShooting) == 0 &&
      nrow(resp$ClosestDefenderShooting) == 0
    )) {
      message(sprintf("    RS empty → Playoffs for pid=%s", pid))
      resp <- tryCatch({
        nba_playerdashptshots(
          date_from = date_str, date_to = date_str,
          game_segment = "", last_n_games = 0,
          league_id = "00", location = "", month = 0, opponent_team_id = 0,
          outcome = "", per_mode = "Totals", period = 0,
          player_id = pid,
          season = season_token2, season_segment = "", season_type = "Playoffs",
          team_id = 0, vs_conference = "", vs_division = "",
          plus_minus = "", pace_adjust = "", rank = "",
          shot_clock_range = "", touch_time_range = "", dribble_range = "", general_range = ""
        )
      }, error = function(e) { message(sprintf("    PO error pid=%s : %s", pid, e$message)); NULL })
    }
    
    # If we have data, transform & append IMMEDIATELY
    if (!is.null(resp)) {
      if (nrow(resp$TouchTimeShooting) > 0) {
        df_t <- resp$TouchTimeShooting %>%
          dplyr::select(PLAYER_ID, PLAYER_NAME_LAST_FIRST, TOUCH_TIME_RANGE,
                        FGM, FGA, FG_PCT, EFG_PCT, FG2M, FG2A, FG2_PCT, FG3M, FG3A, FG3_PCT) %>%
          append_context_fields(base_stats_player, current_date)
        df_t <- cleanup_transform(df_t, paste0("nba_touchtimetracked_", season_token, ".csv"),
                                  "TOUCH_TIME_RANGE", "touch_seconds_low", "touch_seconds_high")
        touch_all <- rbindlist(list(touch_all, df_t), use.names = TRUE, fill = TRUE)
        print(sprintf("    + TouchTime rows appended: %d", nrow(df_t)))
      }
      
      if (nrow(resp$DribbleShooting) > 0) {
        df_d <- resp$DribbleShooting %>%
          dplyr::select(PLAYER_ID, PLAYER_NAME_LAST_FIRST, DRIBBLE_RANGE,
                        FGM, FGA, FG_PCT, EFG_PCT, FG2M, FG2A, FG2_PCT, FG3M, FG3A, FG3_PCT) %>%
          append_context_fields(base_stats_player, current_date)
        df_d <- cleanup_transform(df_d, paste0("nba_dribblestracked_", season_token, ".csv"),
                                  "DRIBBLE_RANGE", "dribbles_low", "dribbles_high")
        dribble_all <- rbindlist(list(dribble_all, df_d), use.names = TRUE, fill = TRUE)
        print(sprintf("    + Dribbles rows appended: %d", nrow(df_d)))
      }
      
      if (nrow(resp$ShotClockShooting) > 0) {
        df_s <- resp$ShotClockShooting %>%
          dplyr::select(PLAYER_ID, PLAYER_NAME_LAST_FIRST, SHOT_CLOCK_RANGE,
                        FGM, FGA, FG_PCT, EFG_PCT, FG2M, FG2A, FG2_PCT, FG3M, FG3A, FG3_PCT) %>%
          append_context_fields(base_stats_player, current_date)
        df_s <- cleanup_transform(df_s, paste0("nba_shotclocktracked_", season_token, ".csv"),
                                  "SHOT_CLOCK_RANGE", "shot_clock_low", "shot_clock_high", "shot_clock_designation")
        shotclk_all <- rbindlist(list(shotclk_all, df_s), use.names = TRUE, fill = TRUE)
        print(sprintf("    + ShotClock rows appended: %d", nrow(df_s)))
      }
      
      if (nrow(resp$ClosestDefenderShooting) > 0) {
        df_c <- resp$ClosestDefenderShooting %>%
          dplyr::select(PLAYER_ID, PLAYER_NAME_LAST_FIRST, CLOSE_DEF_DIST_RANGE,
                        FGM, FGA, FG_PCT, EFG_PCT, FG2M, FG2A, FG2_PCT, FG3M, FG3A, FG3_PCT) %>%
          append_context_fields(base_stats_player, current_date)
        df_c <- cleanup_transform(df_c, paste0("nba_closetdefendertracked_", season_token, ".csv"),
                                  "CLOSE_DEF_DIST_RANGE", "defender_range_low", "defender_range_high", "defender_range_designation")
        cdef_all <- rbindlist(list(cdef_all, df_c), use.names = TRUE, fill = TRUE)
        print(sprintf("    + ClosestDefender rows appended: %d", nrow(df_c)))
      }
    }
    
    print(sprintf("    ✓ Finished pid=%s (%d/%d).", pid, j, length(pids)))
    Sys.sleep(1)  # <-- 50-second delay per player pull
  }
  
  Sys.sleep(1)
}

# -------------------- Write FINAL data.frames -------------------
if (nrow(touch_all))   
  fwrite(touch_all, file.path(output_dir, paste0("nba_touchtimetracked_", season_token, ".csv")), append = TRUE)

if (nrow(dribble_all)) 
  fwrite(dribble_all, file.path(output_dir, paste0("nba_dribblestracked_", season_token, ".csv")), append = TRUE)

if (nrow(shotclk_all)) 
  fwrite(shotclk_all, file.path(output_dir, paste0("nba_shotclocktracked_", season_token, ".csv")), append = TRUE)

if (nrow(cdef_all))    
  fwrite(cdef_all, file.path(output_dir, paste0("nba_closetdefendertracked_", season_token, ".csv")), append = TRUE)


print("✅ All shot-tracking data aggregated into data.frames and written to CSV.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Pull Shot Detail Chart from function for data aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀