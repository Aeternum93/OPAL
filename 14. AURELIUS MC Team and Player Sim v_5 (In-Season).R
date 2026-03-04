# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#           .o.       ooooo     ooo oooooooooooo ooooooooo.   oooooooooooo ooooo        ooooo ooooo     ooo  .oooooo..o      8      ooo        ooooo   .oooooo.                           oooooooo 
#          .888.      `888'     `8' `888'     `8 `888   `Y88. `888'     `8 `888'        `888' `888'     `8' d8P'    `Y8      8      `88.       .888'  d8P'  `Y8b                         dP""""""" 
#         .8"888.      888       8   888          888   .d88'  888          888          888   888       8  Y88bo.           8       888b     d'888  888               oooo    ooo      d88888b.   
#        .8' `888.     888       8   888oooo8     888ooo88P'   888oooo8     888          888   888       8   `"Y8888o.               8 Y88. .P  888  888                `88.  .8'           `Y88b  
#       .88ooo8888.    888       8   888    "     888`88b.     888    "     888          888   888       8       `"Y88b      8       8  `888'   888  888                 `88..8'              ]88  
#      .8'     `888.   `88.    .8'   888       o  888  `88b.   888       o  888       o  888   `88.    .8'  oo     .d8P      8       8    Y     888  `88b    ooo          `888'         o.   .88P  
#     o88o     o8888o    `YbodP'    o888ooooood8 o888o  o888o o888ooooood8 o888ooooood8 o888o    `YbodP'    8""88888P'       8      o8o        o888o  `Y8bood8P'           `8'          `8bd88P'   
#                                                                                                                                                                                             
#                                                                                                                                                                                             
#                                                                                                                                                                                             
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                                                               





# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 0. START: Load + Standardize Inputs (Mode-aware)
#   - mode = "today"    : loads today's schedule file (nba_schedule_YYYYMMDD.csv)
#   - mode = "backtest" : does NOT auto-load schedule; you will call load_schedule_for_date()
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

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

library(data.table)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(future)
library(future.apply)

plan(multisession, workers = 8)
options(future.rng.onMisuse = "ignore")


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#      .oooooo..o           oooo                          .        ooo        ooooo                 .o8            
#      d8P'    `Y8           `888                        .o8        `88.       .888'                "888            
#      Y88bo.       .ooooo.   888   .ooooo.   .ooooo.  .o888oo       888b     d'888   .ooooo.   .oooo888   .ooooo.  
#       `"Y8888o.  d88' `88b  888  d88' `88b d88' `"Y8   888         8 Y88. .P  888  d88' `88b d88' `888  d88' `88b 
#           `"Y88b 888ooo888  888  888ooo888 888         888         8  `888'   888  888   888 888   888  888ooo888 
#      oo     .d8P 888    .o  888  888    .o 888   .o8   888 .       8    Y     888  888   888 888   888  888    .o 
#      8""88888P'  `Y8bod8P' o888o `Y8bod8P' `Y8bod8P'   "888"      o8o        o888o `Y8bod8P' `Y8bod88P" `Y8bod8P' 
# 
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                             
                                                                                                             





# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠    
# START: Mode Selection 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠    


# -----------------------------
# MODE SELECTION
# -----------------------------
mode <- "backtest"   # "today" or "backtest"
load_player_odds <- FALSE

if (mode == "today") {
  as_of_date <- as.Date(current_date)
  
} else if (mode == "backtest") {
  as_of_date <- as.Date("2025-12-31")  # ← hard-coded test date for now
  
} else {
  stop("Invalid mode: ", mode)
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠    
# END: Mode Selection 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                             

# -----------------------------
# Base directories (centralize paths)
# -----------------------------
pbp_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data"
current_slate_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate"
sched_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule"
team_mc_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team"
player_mc_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player"
rot_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations"
odds_player_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds"
injury_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)"


# -----------------------------
# Helpers
# -----------------------------
standardize_bool <- function(x) {
  dplyr::case_when(
    x %in% c("TRUE", "True", "true", "1")    ~ TRUE,
    x %in% c("FALSE", "False", "false", "0") ~ FALSE,
    TRUE ~ FALSE
  )
}

coerce_date_any <- function(x) {
  # Returns Date where possible, else NA
  # Handles "YYYY-MM-DD", "YYYY/MM/DD", "YYYYMMDD"
  x <- as.character(x)
  out <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::ymd(gsub("/", "-", x)))
  }
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  }
  out
}

load_schedule_for_date <- function(date_ymd) {
  # date_ymd can be Date or "YYYY-MM-DD"
  dt <- as.Date(date_ymd)
  if (is.na(dt)) stop("load_schedule_for_date(): invalid date input.")
  ymd_str <- format(dt, "%Y%m%d")
  
  f <- file.path(current_slate_dir, paste0("nba_schedule_", ymd_str, ".csv"))
  if (!file.exists(f)) stop("Schedule file not found: ", f)
  
  df <- fread(f, colClasses = "character", encoding = "UTF-8")
  
  # standardize key cols if present
  if ("game_date" %in% names(df)) df$game_date <- as.character(df$game_date)
  if ("team_id"   %in% names(df)) df$team_id   <- as.character(df$team_id)
  if ("opp_id"    %in% names(df)) df$opp_id    <- as.character(df$opp_id)
  if ("game_id"   %in% names(df)) df$game_id   <- as.character(df$game_id)
  
  df
}

# -----------------------------
# Load pm_nbapbp MC file
# -----------------------------
pbp_path <- file.path(pbp_dir, paste0("pm_nbapbp_", season_token, ".csv"))
pbp_df <- fread(pbp_path, colClasses = "character", encoding = "UTF-8")

# Coerce key numeric fields used by MC profiling + standardize booleans
pbp_df <- pbp_df %>%
  mutate(
    team_id          = as.character(team_id),
    game_id          = as.character(game_id),
    season           = as.character(season),
    score_value      = suppressWarnings(as.numeric(score_value)),
    points_attempted = suppressWarnings(as.numeric(points_attempted)),
    scoring_play     = standardize_bool(scoring_play)
  ) %>%
  filter(!is.na(team_id), team_id != "")

# Ensure game_date exists and is usable (needed for rolling windows/backtests)
# If your pm_nbapbp already has game_date, this will just standardize it.
if ("game_date" %in% names(pbp_df)) {
  pbp_df <- pbp_df %>%
    mutate(
      game_date = as.character(game_date),
      game_date_dt = coerce_date_any(game_date)
    )
} else {
  pbp_df$game_date <- NA_character_
  pbp_df$game_date_dt <- as.Date(NA)
  warning("pbp_df is missing game_date. Rolling windows/backtests will require adding it.")
}

# -----------------------------
# Load BaseStats_Team_MC
# -----------------------------
basestatsT_mc_file <- file.path(team_mc_dir, paste0("BaseStats_Team_MC_", season_token, ".csv"))
BaseStats_Team_MC <- fread(basestatsT_mc_file, colClasses = "character", encoding = "UTF-8") %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID),
    season       = if ("season" %in% names(.)) as.character(season) else as.character(season_token)
  )

# -----------------------------
# Load BaseStats_Player_MC (optional for team MC; keep loaded for later)
# -----------------------------
basestatsP_mc_file <- file.path(player_mc_dir, paste0("BaseStats_Player_MC_", season_token, ".csv"))
BaseStats_Player_MC <- fread(basestatsP_mc_file, colClasses = "character", encoding = "UTF-8")

# -----------------------------
# Load rotations (optional for team MC; keep loaded for later)
# -----------------------------
nba_rotation_10M_file <- file.path(rot_dir, paste0("pm_nba_player_rotations_10M_", season_token, ".csv"))
nba_rotations_10M <- fread(nba_rotation_10M_file, colClasses = "character", encoding = "UTF-8")

nba_rotation_5M_file <- file.path(rot_dir, paste0("pm_nba_player_rotations_5M_", season_token, ".csv"))
nba_rotations_5M <- fread(nba_rotation_5M_file, colClasses = "character", encoding = "UTF-8")

player_rotations_file <- file.path(rot_dir, paste0("pm_nba_player_level_rotations_", season_token, ".csv"))
player_rotations <- fread(player_rotations_file, colClasses = "character", encoding = "UTF-8")

player_impact_delta_file <- file.path(rot_dir, paste0("player_impact_assessment_delta_", season_token, ".csv"))
player_impact_delta <- fread(player_impact_delta_file, colClasses = "character", encoding = "UTF-8")


# -----------------------------
# Load injury data (optional for team MC; keep loaded for later)
# -----------------------------
injury_data_file <- file.path(injury_dir, paste0("Injury_Database_", season_token, ".csv"))
injury_data <- fread(injury_data_file, colClasses = "character", encoding = "UTF-8")


# -----------------------------
# Load schedule data (optional for team MC; keep loaded for later)
# -----------------------------
sched_file <- file.path(sched_dir, paste0("nba_schedule_", season_token, ".csv"))
nba_schedule_season <- fread(sched_file, colClasses = "character", encoding = "UTF-8")


# -----------------------------
# Load player odds data (optional for team MC; keep loaded for later)
# -----------------------------
player_odds_enrich_file <- file.path(odds_player_dir, paste0("nba_player_odds_enrich_", season_token, ".csv"))
player_odds_enrich_df <- fread(player_odds_enrich_file, colClasses = "character", encoding = "UTF-8")


# -----------------------------
# Load schedule depending on mode
# -----------------------------
nba_schedule_today <- NULL

if (mode == "today") {
  # Use your formatted_date from master runner
  nba_schedule_today <- load_schedule_for_date(as.Date(current_date))
  
  # Safety check: your downstream assumes these exist for simulation
  req_cols <- c("game_id", "team_id", "opp_id")
  missing <- setdiff(req_cols, names(nba_schedule_today))
  if (length(missing) > 0) stop("Today's schedule missing columns: ", paste(missing, collapse = ", "))
}

# ----------------------------------------
# Attach NBA_Team_ID and NBA_Opp_ID
# ----------------------------------------

team_map_path <- file.path(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup",
  "team_id_mapping.csv"
)

team_id_map <- fread(
  team_map_path,
  colClasses = "character",
  encoding = "UTF-8"
)

# Expecting team_id / opp_id to be ESPN team IDs
nba_schedule_season <- nba_schedule_season %>%
  mutate(
    team_id = as.character(team_id),
    opp_id  = as.character(opp_id)
  ) %>%
  left_join(
    team_id_map %>%
      select(espn_team_id, nba_team_id) %>%
      rename(NBA_Team_ID = nba_team_id),
    by = c("team_id" = "espn_team_id")
  ) %>%
  left_join(
    team_id_map %>%
      select(espn_team_id, nba_team_id) %>%
      rename(NBA_Opp_ID = nba_team_id),
    by = c("opp_id" = "espn_team_id")
  )


rm("pbp_dir",
   "pbp_path",
   "sched_dir",
   "sched_file",
   "rot_dir",
   "injury_dir",
   "current_slate_dir",
   "team_map_path",
   "player_mc_dir",
   "team_mc_dir",
   "odds_player_dir",
   "player_impact_dela_file",
   "nba_rotation_5M_file",
   "nba_rotation_10M_file",
   "injury_data_file",
   "basestatsP_mc_file",
   "basestatsT_mc_file",
   "player_rotations_file"
)

# ============================================================
# STEP 0: Canonicalize games (1 row per game_id) for calibration
#   - Assumes nba_schedule_season has TWO rows per game_id:
#       is_static == TRUE  (original schedule row, true home/away)
#       is_static == FALSE (flipped row)
#   - Outputs:
#       nba_games_canon_season  (one row per game_id)
# ============================================================

library(dplyr)

nba_games_canon_season <- nba_schedule_season %>%
  mutate(
    game_id   = as.character(game_id),
    team_id   = as.character(team_id),
    opp_id    = as.character(opp_id),
    team      = as.character(team),
    opp       = as.character(opp),
    game_date = as.character(game_date),
    is_static = as.logical(is_static)
  ) %>%
  group_by(game_id) %>%
  summarise(
    game_date = dplyr::first(na.omit(game_date)),
    
    # --- HOME side (from the static/original row) ---
    home_team_id = team_id[is_static][1],
    home_team    = team[is_static][1],
    home_score   = suppressWarnings(as.numeric(team_score[is_static][1])),
    home_winner  = as.logical(team_winner[is_static][1]),
    
    # --- AWAY side (opponent fields from the SAME static row) ---
    away_team_id = opp_id[is_static][1],
    away_team    = opp[is_static][1],
    away_score   = suppressWarnings(as.numeric(opp_score[is_static][1])),
    away_winner  = as.logical(opp_winner[is_static][1]),
    
    # --- audits ---
    n_rows_in_game = dplyr::n(),
    n_static_rows  = sum(is_static, na.rm = TRUE),
    n_flip_rows    = sum(!is_static, na.rm = TRUE),
    
    # verify the two rows are truly flipped
    t1 = as.character(team_id[1]),
    o1 = as.character(opp_id[1]),
    t2 = as.character(team_id[2]),
    o2 = as.character(opp_id[2]),
    flip_ok = (t1 == o2 && t2 == o1),
    
    .groups = "drop"
  ) %>%
  mutate(
    canon_ok = (n_rows_in_game == 2L) & (n_static_rows == 1L) & flip_ok
  ) %>%
  select(-t1, -o1, -t2, -o2)

# ---- quick sanity audit ----
cat("Canonical games:", nrow(nba_games_canon_season), "\n")
cat("canon_ok == TRUE:", sum(nba_games_canon_season$canon_ok, na.rm = TRUE), "\n")
cat("canon_ok == FALSE:", sum(!nba_games_canon_season$canon_ok, na.rm = TRUE), "\n")

bad_games <- nba_games_canon_season %>% filter(!canon_ok)
if (nrow(bad_games) > 0) {
  print(bad_games %>% select(game_id, n_rows_in_game, n_static_rows, n_flip_rows, flip_ok) %>% head(25))
}


# ============================================================
# STEP 0: Canonicalize games (1 row per game_id) for calibration
#   - Assumes nba_schedule_season has TWO rows per game_id:
#       is_static == TRUE  (original schedule row, true home/away)
#       is_static == FALSE (flipped row)
#   - Outputs:
#       nba_games_canon_season  (one row per game_id)
# ============================================================

library(dplyr)

nba_games_canon_season <- nba_schedule_season %>%
  mutate(
    game_id   = as.character(game_id),
    team_id   = as.character(team_id),
    opp_id    = as.character(opp_id),
    team      = as.character(team),
    opp       = as.character(opp),
    game_date = as.character(game_date),
    is_static = as.logical(is_static)
  ) %>%
  group_by(game_id) %>%
  summarise(
    game_date = dplyr::first(na.omit(game_date)),
    
    # --- HOME side (from the static/original row) ---
    home_team_id = team_id[is_static][1],
    home_team    = team[is_static][1],
    home_score   = suppressWarnings(as.numeric(team_score[is_static][1])),
    home_winner  = as.logical(team_winner[is_static][1]),
    
    # --- AWAY side (opponent fields from the SAME static row) ---
    away_team_id = opp_id[is_static][1],
    away_team    = opp[is_static][1],
    away_score   = suppressWarnings(as.numeric(opp_score[is_static][1])),
    away_winner  = as.logical(opp_winner[is_static][1]),
    
    # --- audits ---
    n_rows_in_game = dplyr::n(),
    n_static_rows  = sum(is_static, na.rm = TRUE),
    n_flip_rows    = sum(!is_static, na.rm = TRUE),
    
    # verify the two rows are truly flipped
    t1 = as.character(team_id[1]),
    o1 = as.character(opp_id[1]),
    t2 = as.character(team_id[2]),
    o2 = as.character(opp_id[2]),
    flip_ok = (t1 == o2 && t2 == o1),
    
    .groups = "drop"
  ) %>%
  mutate(
    canon_ok = (n_rows_in_game == 2L) & (n_static_rows == 1L) & flip_ok
  ) %>%
  select(-t1, -o1, -t2, -o2)

# ---- quick sanity audit ----
cat("Canonical games:", nrow(nba_games_canon_season), "\n")
cat("canon_ok == TRUE:", sum(nba_games_canon_season$canon_ok, na.rm = TRUE), "\n")
cat("canon_ok == FALSE:", sum(!nba_games_canon_season$canon_ok, na.rm = TRUE), "\n")

bad_games <- nba_games_canon_season %>% filter(!canon_ok)
if (nrow(bad_games) > 0) {
  print(bad_games %>% select(game_id, n_rows_in_game, n_static_rows, n_flip_rows, flip_ok) %>% head(25))
}


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 0. END: Load + Standardize
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     oooooo     oooo                     o8o                                                   .oooooo.                             .                      oooo  
#      `888.     .8'                      `"'                                                  d8P'  `Y8b                          .o8                      `888  
#       `888.   .8'    .oooo.   oooo d8b oooo   .oooo.   ooo. .oo.    .ooooo.   .ooooo.       888           .ooooo.  ooo. .oo.   .o888oo oooo d8b  .ooooo.   888  
#        `888. .8'    `P  )88b  `888""8P `888  `P  )88b  `888P"Y88b  d88' `"Y8 d88' `88b      888          d88' `88b `888P"Y88b    888   `888""8P d88' `88b  888  
#         `888.8'      .oP"888   888      888   .oP"888   888   888  888       888ooo888      888          888   888  888   888    888    888     888   888  888  
#          `888'      d8(  888   888      888  d8(  888   888   888  888   .o8 888    .o      `88b    ooo  888   888  888   888    888 .  888     888   888  888  
#           `8'       `Y888""8o d888b    o888o `Y888""8o o888o o888o `Y8bod8P' `Y8bod8P'       `Y8bood8P'  `Y8bod8P' o888o o888o   "888" d888b    `Y8bod8P' o888o 
#
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                                                                            
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                                                                            





# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# START. VARIANCE CONTROLS (STD / randomness levers)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠


MC_VAR <- list(
  
  # 1. poss_sd_mult | This is a multiplier that scales the standard deviation of possessions. You can make it deviate more (>1.00) or less (<1.00) from the normal pace of play 
  # (more or less possessions)
  
  poss_sd_mult = 1.00,
  
  # 2.shoot_shrink | This is a shrink factor that pull the teams shooting away or closer to the anchor mean (league mean, baseline mean, or match up mean
  # shooting % shrink toward league-ish mean. Change the mean of the shooting percentage input (0 = none, 1 = full anchor)
  
  shoot_shrink = 0.20,
  
  # 3. pct jitter | This can add additional randomness to shooting probabilities on each simulation. Changes the per-simul;ation noise around the mean
  # previously set but shoot_shrink (adds randomness AFTER shrink; per-sim draw) Typical sane range: 0.005 to 0.020
  
  pct_jitter_sd = 0.00,
  
  # 4.tov_rand | This will determine turnover randomness strength. (0..1) -> blends team p_to with league p_to
  
  tov_rand = 0.25,
  
  # ---- Rebounds rate variance controls ----
  reb_sd_mult   = 1.00,
  reb_mean_mult = 1.00,
  reb_shrink    = 0.20,  # shrink reb/min toward anchor (0..1)
  reb_jitter_sd = 0.00,  # per-sim noise on reb/min (try 0.01–0.03 later)
  reb_rpm_lo    = 0.00,  # lower bound for rebounds per minute
  reb_rpm_hi    = 0.60,   # upper bound for rebounds per minute (prevents absurd tails)
  
  # ---- Assist rate variance controls ----
  ast_shrink    = 0.20,
  ast_jitter_sd = 0.00,
  ast_apm_lo    = 0.00,
  ast_apm_hi    = 0.35,   # sane cap (~16.8 ast / 48 min)
  
  # ---- Blocks rate variance controls ----
  blk_shrink    = 0.25,   # pull player BLK/min toward league BLK/min
  blk_jitter_sd = 0.01,   # per-sim randomness on block rate
  blk_bpm_lo    = 0.00,   # hard floor for blocks per minute
  blk_bpm_hi    = 0.20,  # hard cap for blocks per minute
  
  # ---- Steals rate variance controls ----
  stl_shrink    = 0.25,
  stl_jitter_sd = 0.01,
  stl_spm_lo    = 0.00,
  stl_spm_hi    = 0.20,
  
  # ---- Turnovers rate variance controls ----
  tov_shrink    = 0.30,
  tov_jitter_sd = 0.01,
  tov_tovpm_lo  = 0.00,
  tov_tovpm_hi  = 0.25,
  
  # ---- 3PM rate variance controls ----
  fg3m_shrink    = 0.25,
  fg3m_jitter_sd = 0.01,
  fg3m_pm_lo     = 0.00,
  fg3m_pm_hi     = 0.12,
  
  # ---- Fouls rate variance controls ----
  fouls_shrink    = 0.25,
  fouls_jitter_sd = 0.010,
  fouls_fpm_lo    = 0.00,
  fouls_fpm_hi    = 0.30
)
  

# ---- helpers (define ONCE) ----
clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  clamp01(x)
}

num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

shrink_to <- function(x, anchor, w) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(anchor)
  w <- clamp01(w)
  (1 - w) * x + w * anchor
}

apply_pct_jitter <- function(p, jitter_sd) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return(NA_real_)
  if (is.na(jitter_sd) || jitter_sd <= 0) return(clamp01(p))
  clamp01(rnorm(1, mean = p, sd = jitter_sd))
}

scale_reb_profile <- function(mu, sd, mean_mult = 1.0, sd_mult = 1.0) {
  mu <- num0(mu, 0); sd <- num0(sd, 0)
  list(mu = pmax(0, mu * mean_mult), sd = pmax(0, sd * sd_mult))
}


# ============================================================
# VARIANCE CONTROL: GENERIC RATE ADJUSTER (SIMULATION ONLY)
# ============================================================

apply_rate_variance_control <- function(
    rate,
    anchor,
    shrink_w,
    jitter_sd = 0,
    lower = 0,
    upper = Inf
) {
  rate   <- suppressWarnings(as.numeric(rate))
  anchor <- suppressWarnings(as.numeric(anchor))
  
  if (is.na(rate)) return(NA_real_)
  if (is.na(anchor)) anchor <- rate
  
  # --- shrink toward anchor ---
  w <- clamp01(shrink_w)
  adj <- (1 - w) * rate + w * anchor
  
  # --- optional jitter (per sim) ---
  if (!is.na(jitter_sd) && jitter_sd > 0) {
    adj <- rnorm(1, mean = adj, sd = jitter_sd)
  }
  
  # --- bounds ---
  adj <- pmin(upper, pmax(lower, adj))
  
  adj
}

# ============================================================
# VARIANCE CONTROL: REBOUNDS PER MIN (SIMULATION ONLY)
# ============================================================
apply_reb_rate_variance_control <- function(
    reb_per_min,
    anchor_rpm,
    shrink_w  = MC_VAR$reb_shrink,
    jitter_sd = MC_VAR$reb_jitter_sd,
    lower     = MC_VAR$reb_rpm_lo,
    upper     = MC_VAR$reb_rpm_hi
) {
  apply_rate_variance_control(
    rate      = reb_per_min,
    anchor    = anchor_rpm,
    shrink_w  = shrink_w,
    jitter_sd = jitter_sd,
    lower     = lower,
    upper     = upper
  )
}


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END VARIANCE CONTROLS
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#     ooooooooo.   oooo                                                ooooooooo.                       .o88o.  o8o  oooo                     
#     `888   `Y88. `888                                                `888   `Y88.                     888 `"  `"'  `888                     
#      888   .d88'  888   .oooo.   oooo    ooo  .ooooo.  oooo d8b       888   .d88' oooo d8b  .ooooo.  o888oo  oooo   888   .ooooo.   .oooo.o 
#      888ooo88P'   888  `P  )88b   `88.  .8'  d88' `88b `888""8P       888ooo88P'  `888""8P d88' `88b  888    `888   888  d88' `88b d88(  "8 
#      888          888   .oP"888    `88..8'   888ooo888  888           888          888     888   888  888     888   888  888ooo888 `"Y88b.  
#      888          888  d8(  888     `888'    888    .o  888           888          888     888   888  888     888   888  888    .o o.  )88b 
#     o888o        o888o `Y888""8o     .8'     `Y8bod8P' d888b         o888o        d888b    `Y8bod8P' o888o   o888o o888o `Y8bod8P' 8""888P' 
#                                  .o..P'                                                                                            
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠





# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧
# 💧💧💧 Injury / Availability Layer (MODE-SENSITIVE) 💧💧💧
#   - today    : builds roster availability for current_date
#   - backtest : builds roster availability for as_of_date (single date)
# Notes:
#   - NO daily_units / rolling windows in backtest (legacy removed)
#   - Outputs are always initialized to avoid downstream explosions
# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧

# 💧💧💧 Injury / Availability Layer (PATCHED, SAFE) 💧💧💧

stopifnot(
  exists("mode"),
  exists("BaseStats_Player_MC"),
  exists("injury_data"),
  exists("season_token2")
)

suppressWarnings({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(hoopR)
})

# -----------------------------
# 0) Standardize / bridge
# -----------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    NBA_PLAYER_ID  = as.character(NBA_PLAYER_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID)
  )

injury_data <- injury_data %>%
  mutate(
    ESPN_PLAYER_ID = as.character(if ("ESPN_PLAYER_ID" %in% names(.)) ESPN_PLAYER_ID else espn_player_id),
    ESPN_TEAM_ID   = as.character(if ("ESPN_TEAM_ID"   %in% names(.)) ESPN_TEAM_ID   else espn_team_id),
    status         = tolower(as.character(status))
  )

if ("date" %in% names(injury_data)) {
  injury_data <- injury_data %>% mutate(game_date_dt = as.Date(date))
} else if ("game_date" %in% names(injury_data)) {
  injury_data <- injury_data %>% mutate(game_date_dt = as.Date(game_date))
} else {
  stop("injury_data must contain `date` or `game_date`.")
}

bridge_unique <- BaseStats_Player_MC %>%
  select(NBA_PLAYER_ID, ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
  distinct(NBA_PLAYER_ID, ESPN_TEAM_ID, .keep_all = TRUE)

# -----------------------------
# 1) Roster cache
# -----------------------------
.roster_cache <- new.env(parent = emptyenv())

get_team_roster_cached <- function(nba_team_id, season_token2) {
  key <- paste0(season_token2, "::", nba_team_id)
  if (exists(key, envir = .roster_cache)) {
    return(get(key, envir = .roster_cache))
  }
  
  raw <- nba_commonteamroster(team_id = nba_team_id, season = season_token2)
  
  roster <- raw$CommonTeamRoster %>%
    rename(
      NBA_TEAM_ID   = TeamID,
      NBA_PLAYER_ID = PLAYER_ID
    ) %>%
    mutate(
      NBA_TEAM_ID   = as.character(NBA_TEAM_ID),
      NBA_PLAYER_ID = as.character(NBA_PLAYER_ID)
    )
  
  assign(key, roster, envir = .roster_cache)
  Sys.sleep(0.5)
  roster
}

# -----------------------------
# 2) Core builder (PATCHED)
# -----------------------------
build_availability_for_date <- function(schedule_df, game_date, date_key) {
  
  game_date <- as.Date(game_date)
  
  team_keys <- schedule_df %>%
    transmute(
      NBA_TEAM_ID  = as.character(NBA_Team_ID),
      ESPN_TEAM_ID = as.character(team_id),
      date_key     = as.character(date_key),
      GAME_DATE    = game_date
    ) %>%
    distinct()
  
  roster_pool <- purrr::map_dfr(unique(team_keys$NBA_TEAM_ID), function(nba_tid) {
    roster_i <- get_team_roster_cached(nba_tid, season_token2)
    espn_tid <- team_keys %>% filter(NBA_TEAM_ID == nba_tid) %>% slice(1) %>% pull(ESPN_TEAM_ID)
    
    roster_i %>%
      mutate(
        NBA_TEAM_ID  = nba_tid,
        ESPN_TEAM_ID = espn_tid,
        date_key     = date_key,
        GAME_DATE    = game_date
      )
  }) %>%
    left_join(bridge_unique, by = c("NBA_PLAYER_ID", "ESPN_TEAM_ID"))
  
  # ---- injury slice for this date ----
  inj_out <- injury_data %>%
    filter(game_date_dt == game_date, status == "out") %>%
    distinct(ESPN_TEAM_ID, ESPN_PLAYER_ID)
  
  # ---- PATCH STATUS (NO JOINS) ----
  roster_pool <- roster_pool %>%
    mutate(
      status = "active",
      is_out = as.integer(
        paste0(ESPN_TEAM_ID, "::", ESPN_PLAYER_ID) %in%
          paste0(inj_out$ESPN_TEAM_ID, "::", inj_out$ESPN_PLAYER_ID)
      )
    ) %>%
    mutate(
      status = ifelse(is_out == 1L, "out", "active")
    )
  
  roster_active <- roster_pool %>% filter(is_out == 0L)
  roster_out    <- roster_pool %>% filter(is_out == 1L)
  
  team_audit <- roster_pool %>%
    group_by(date_key, GAME_DATE, NBA_TEAM_ID, ESPN_TEAM_ID) %>%
    summarise(
      roster_pool_n = n(),
      out_n         = sum(is_out),
      active_n      = sum(1L - is_out),
      out_players   = paste(ESPN_PLAYER_ID[is_out == 1L], collapse = ","),
      .groups = "drop"
    )
  
  list(
    team_keys     = team_keys,
    roster_pool   = roster_pool,
    roster_active = roster_active,
    roster_out    = roster_out,
    team_audit    = team_audit
  )
}


# ===============================================================
# MODE ROUTER (SINGLE DATE + BACKTEST HISTORY)
# ===============================================================

# ---- Always initialize everything ----
roster_teamkeys_by_date <- list()
roster_pool_by_date     <- list()
roster_active_by_date   <- list()
roster_out_by_date      <- list()

# today-only convenience tables
nba_schedule_today           <- tibble()
roster_teamkeys_today        <- tibble()
roster_pool_today            <- tibble()
roster_active_today          <- tibble()
roster_out_today             <- tibble()
injury_availability_today_df <- tibble()
injury_team_summary_today_df <- tibble()

# single-date audit outputs (always = the run date)
injury_availability_audit_df <- tibble()
injury_team_summary_df       <- tibble()

# ---- NEW: backtest history containers (ALL DATES < run_date) ----
roster_teamkeys_backtest_by_date <- list()
roster_pool_backtest_by_date     <- list()
roster_active_backtest_by_date   <- list()
roster_out_backtest_by_date      <- list()

roster_teamkeys_backtest <- tibble()
roster_pool_backtest     <- tibble()
roster_active_backtest   <- tibble()
roster_out_backtest      <- tibble()

# -----------------------------
# Resolve run date (the "main" date)
# -----------------------------
run_date <- if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else {
  stop("Invalid mode: ", mode, " (expected 'today' or 'backtest').")
}

if (is.na(run_date)) stop("run_date is invalid.")

run_key <- format(run_date, "%Y%m%d")

stopifnot(exists("nba_schedule_season"))
stopifnot(all(c("game_date", "NBA_Team_ID", "team_id") %in% names(nba_schedule_season)))

sched_all <- nba_schedule_season %>%
  mutate(game_date_dt = as.Date(game_date)) %>%
  filter(!is.na(game_date_dt))

# ============================================================
# A) Build SINGLE-DATE availability for run_date
#     -> populate roster_*_by_date ONLY with run_key
# ============================================================
sched_run <- sched_all %>%
  filter(game_date_dt == run_date) %>%
  select(-game_date_dt)

if (nrow(sched_run) == 0) {
  stop("No games found for run_date: ", run_date)
}

run_avail <- build_availability_for_date(
  schedule_df = sched_run,
  game_date   = run_date,
  date_key    = run_key
)

roster_teamkeys_by_date[[run_key]] <- run_avail$team_keys
roster_pool_by_date[[run_key]]     <- run_avail$roster_pool
roster_active_by_date[[run_key]]   <- run_avail$roster_active
roster_out_by_date[[run_key]]      <- run_avail$roster_out

injury_availability_audit_df <- run_avail$avail_audit
injury_team_summary_df       <- run_avail$team_audit

# also fill *_today only if mode == "today"
if (mode == "today") {
  nba_schedule_today           <- sched_run
  roster_teamkeys_today        <- run_avail$team_keys
  roster_pool_today            <- run_avail$roster_pool
  roster_active_today          <- run_avail$roster_active
  roster_out_today             <- run_avail$roster_out
  injury_availability_today_df <- run_avail$avail_audit
  injury_team_summary_today_df <- run_avail$team_audit
}

message("RUN avail built ", run_key,
        " | pool=", nrow(run_avail$roster_pool),
        " | out=",  nrow(run_avail$roster_out),
        " | active=", nrow(run_avail$roster_active))

# ============================================================
# B) Build BACKTEST HISTORY for ALL slate dates STRICTLY < run_date
#     -> populate roster_*_backtest_by_date lists
# ============================================================
prior_dates <- sched_all %>%
  filter(game_date_dt < run_date) %>%
  distinct(game_date_dt) %>%
  arrange(game_date_dt) %>%
  pull(game_date_dt)

if (length(prior_dates) == 0) {
  message("No prior slate dates found before run_date=", run_date)
} else {
  
  for (dd in prior_dates) {
    kk <- format(as.Date(dd), "%Y%m%d")
    
    sched_dd <- sched_all %>%
      filter(game_date_dt == dd) %>%
      select(-game_date_dt)
    
    if (nrow(sched_dd) == 0) next
    
    out_i <- build_availability_for_date(
      schedule_df = sched_dd,
      game_date   = dd,
      date_key    = kk
    )
    
    roster_teamkeys_backtest_by_date[[kk]] <- out_i$team_keys
    roster_pool_backtest_by_date[[kk]]     <- out_i$roster_pool
    roster_active_backtest_by_date[[kk]]   <- out_i$roster_active
    roster_out_backtest_by_date[[kk]]      <- out_i$roster_out
  }
  
  # Optional: big DF versions (all prior dates only)
  hist_keys <- names(roster_active_backtest_by_date)
  
  roster_teamkeys_backtest <- dplyr::bind_rows(roster_teamkeys_backtest_by_date[hist_keys])
  roster_pool_backtest     <- dplyr::bind_rows(roster_pool_backtest_by_date[hist_keys])
  roster_active_backtest   <- dplyr::bind_rows(roster_active_backtest_by_date[hist_keys])
  roster_out_backtest      <- dplyr::bind_rows(roster_out_backtest_by_date[hist_keys])
  
  message("BACKTEST history built | dates=", length(hist_keys),
          " | active_rows=", nrow(roster_active_backtest),
          " | out_rows=", nrow(roster_out_backtest))
}

# ------------------------------------------------------------
# PATCH: Ensure ESPN_TEAM_ID exists via BaseStats_Team_MC
# ------------------------------------------------------------

team_id_bridge <- BaseStats_Team_MC %>%
  dplyr::select(NBA_TEAM_ID, ESPN_TEAM_ID) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    NBA_TEAM_ID  = as.character(NBA_TEAM_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

patch_team_ids <- function(df) {
  if (!("NBA_TEAM_ID" %in% names(df))) return(df)
  
  if (!("ESPN_TEAM_ID" %in% names(df))) {
    df <- df %>%
      dplyr::left_join(team_id_bridge, by = "NBA_TEAM_ID")
  }
  
  df
}

# ---- apply to ALL roster containers ----
roster_active_by_date <- lapply(roster_active_by_date, patch_team_ids)
roster_out_by_date    <- lapply(roster_out_by_date,    patch_team_ids)

roster_active_backtest_by_date <- lapply(roster_active_backtest_by_date, patch_team_ids)
roster_out_backtest_by_date    <- lapply(roster_out_backtest_by_date,    patch_team_ids)

# ------------------------------------------------------------
# PATCH: Ensure ESPN_TEAM_ID exists via BaseStats_Team_MC
# ------------------------------------------------------------

team_id_bridge <- BaseStats_Team_MC %>%
  dplyr::select(NBA_TEAM_ID, ESPN_TEAM_ID) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    NBA_TEAM_ID  = as.character(NBA_TEAM_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

patch_team_ids <- function(df) {
  if (!("NBA_TEAM_ID" %in% names(df))) return(df)
  
  if (!("ESPN_TEAM_ID" %in% names(df))) {
    df <- df %>%
      dplyr::left_join(team_id_bridge, by = "NBA_TEAM_ID")
  }
  
  df
}

# ---- APPLY TO ALL ROSTER CONTAINERS ----

# Live (run date)
roster_active_by_date <- lapply(roster_active_by_date, patch_team_ids)
roster_out_by_date    <- lapply(roster_out_by_date,    patch_team_ids)

# Backtest (historical)
roster_active_backtest_by_date <- lapply(roster_active_backtest_by_date, patch_team_ids)
roster_out_backtest_by_date    <- lapply(roster_out_backtest_by_date,    patch_team_ids)


# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧
# END INJURY PROTOYPE SECTION
# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     ooo        ooooo  o8o                              .                           ooooooooo.                          o8o                         .    o8o                                 
#     `88.       .888'  `"'                            .o8                           `888   `Y88.                        `"'                       .o8    `"'                                 
#      888b     d'888  oooo  ooo. .oo.   oooo  oooo  .o888oo  .ooooo.   .oooo.o       888   .d88' oooo d8b  .ooooo.     oooo  .ooooo.   .ooooo.  .o888oo oooo   .ooooo.  ooo. .oo.    .oooo.o 
#      8 Y88. .P  888  `888  `888P"Y88b  `888  `888    888   d88' `88b d88(  "8       888ooo88P'  `888""8P d88' `88b    `888 d88' `88b d88' `"Y8   888   `888  d88' `88b `888P"Y88b  d88(  "8 
#      8  `888'   888   888   888   888   888   888    888   888ooo888 `"Y88b.        888          888     888   888     888 888ooo888 888         888    888  888   888  888   888  `"Y88b.  
#      8    Y     888   888   888   888   888   888    888 . 888    .o o.  )88b       888          888     888   888     888 888    .o 888   .o8   888 .  888  888   888  888   888  o.  )88b 
#     o8o        o888o o888o o888o o888o  `V88V"V8P'   "888" `Y8bod8P' 8""888P'      o888o        d888b    `Y8bod8P'     888 `Y8bod8P' `Y8bod8P'   "888" o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                                        888                                                                  
#                                                                                                                    .o. 88P                                                                  
#                                                                                                                    `Y888P     
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




# 🏀🏀🏀 START: Minutes BASELINE PARAMETER BUILDER (NO SIMULATION) 🏀🏏🏏

stopifnot(
  exists("mode"),
  exists("roster_active_by_date"),
  exists("roster_out_by_date"),
  exists("BaseStats_Player_MC"),
  exists("num0")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

key_to_date <- function(k) {
  suppressWarnings(as.Date(gsub("[^0-9]", "", k), "%Y%m%d"))
}

# ------------------------------------------------------------
# Helper: build minutes PARAMETER TABLE for ONE DATE
# ------------------------------------------------------------
build_minutes_params_one_date <- function(
    proj_date,
    roster_active,
    roster_out,
    n_games_back = 10L
) {
  
  proj_date <- as.Date(proj_date)
  k <- format(proj_date, "%Y%m%d")
  
  # ----------------------------------------------------------
  # A) Historical per-quarter minute params (LEAKAGE-SAFE)
  # ----------------------------------------------------------
  build_q_params <- function(q) {
    col <- paste0("MINS_", q)
    if (!col %in% names(BaseStats_Player_MC)) return(NULL)
    
    BaseStats_Player_MC %>%
      dplyr::mutate(
        GAME_DATE = as.Date(GAME_DATE),
        val       = num0(.data[[col]], NA_real_)
      ) %>%
      dplyr::filter(GAME_DATE < proj_date) %>%
      dplyr::arrange(ESPN_PLAYER_ID, GAME_DATE) %>%
      dplyr::group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
      dplyr::slice_tail(n = n_games_back) %>%
      dplyr::summarise(
        !!paste0("mins_mean_", q) := mean(val, na.rm = TRUE),
        !!paste0("mins_sd_", q)   := sd(val, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  mins_params <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = c("ESPN_PLAYER_ID","ESPN_TEAM_ID")),
    lapply(quarters, build_q_params)
  )
  
  # ----------------------------------------------------------
  # B) Active players (BASE TABLE)
  # ----------------------------------------------------------
  active_df <- roster_active %>%
    dplyr::transmute(
      date_key       = k,
      GAME_DATE      = proj_date,
      ESPN_TEAM_ID,
      NBA_TEAM_ID,
      ESPN_PLAYER_ID,
      PLAYER
    ) %>%
    dplyr::distinct()
  
  # ----------------------------------------------------------
  # C) Out players (ENSURE INCLUDED EVEN IF NOT ACTIVE)
  # ----------------------------------------------------------
  has_out <- !is.null(roster_out) &&
    nrow(roster_out) > 0 &&
    all(c("ESPN_TEAM_ID","ESPN_PLAYER_ID") %in% names(roster_out))
  
  out_df <- if (!has_out) {
    tibble::tibble(
      ESPN_TEAM_ID   = character(),
      NBA_TEAM_ID    = character(),
      ESPN_PLAYER_ID = character(),
      PLAYER         = character()
    )
  } else {
    roster_out %>%
      dplyr::transmute(
        ESPN_TEAM_ID,
        NBA_TEAM_ID,
        ESPN_PLAYER_ID,
        PLAYER
      ) %>%
      dplyr::distinct()
  }
  
  out_missing <- out_df %>%
    dplyr::anti_join(active_df, by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
    dplyr::mutate(
      date_key  = k,
      GAME_DATE = proj_date
    )
  
  # ----------------------------------------------------------
  # D) PATCH AVAILABILITY (NO JOINS)
  # ----------------------------------------------------------
  out_keys <- paste0(
    roster_out$ESPN_TEAM_ID, "::", roster_out$ESPN_PLAYER_ID
  )
  
  df <- dplyr::bind_rows(active_df, out_missing) %>%
    dplyr::left_join(
      mins_params,
      by = c("ESPN_PLAYER_ID","ESPN_TEAM_ID")
    ) %>%
    dplyr::mutate(
      is_out = as.integer(
        paste0(ESPN_TEAM_ID, "::", ESPN_PLAYER_ID) %in% out_keys
      ),
      is_active = 1L - is_out,
      last_status = ifelse(is_out == 1L, "out", "active")

    ) %>%
    dplyr::select(
      date_key, GAME_DATE, ESPN_TEAM_ID, NBA_TEAM_ID, ESPN_PLAYER_ID, PLAYER,
      starts_with("mins_mean_"),
      starts_with("mins_sd_"),
      last_status, is_out, is_active
    )
  
  df
}

# ------------------------------------------------------------
# LIVE DATE
# ------------------------------------------------------------
proj_date <- if (mode == "backtest") as.Date(as_of_date) else as.Date(current_date)
k <- format(proj_date, "%Y%m%d")

player_minutes_params_by_date <- list()
player_minutes_params_by_date[[k]] <-
  build_minutes_params_one_date(
    proj_date,
    roster_active_by_date[[k]],
    roster_out_by_date[[k]]
  )

# ------------------------------------------------------------
# BACKTEST (STACKED TABLE)
# ------------------------------------------------------------
bt_keys  <- names(roster_active_backtest_by_date)
bt_dates <- key_to_date(bt_keys)
bt_keys  <- bt_keys[!is.na(bt_dates) & bt_dates < proj_date]

player_projections_backtest <- dplyr::bind_rows(
  lapply(bt_keys, function(kk) {
    build_minutes_params_one_date(
      proj_date     = key_to_date(kk),
      roster_active = roster_active_backtest_by_date[[kk]],
      roster_out    = roster_out_backtest_by_date[[kk]]
    )
  })
)

message(
  "Minutes params built | mode=", mode,
  " | live_rows=", nrow(player_minutes_params_by_date[[k]]),
  " | backtest_rows=", nrow(player_projections_backtest)
)

# 🏀🏀🏀 END: Minutes BASELINE PARAMETER BUILDER 🏀🏏🏏






# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Pull in Team Names and Opp Names
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# Add TEAM / OPP / HOME_AWAY fields onto player_projections_backtest
# (match ESPN_TEAM_ID -> schedule_df$team_id)
# ------------------------------------------------------------

stopifnot(exists("player_projections_backtest"), exists("nba_schedule_season"))
stopifnot(all(c("team_id","team","opp_id","opp","home_away_sym","home_away") %in% names(nba_schedule_season)))

player_projections_backtest <- player_projections_backtest %>%
  mutate(ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)) %>%
  left_join(
    nba_schedule_season %>%
      transmute(
        ESPN_GAME_ID   = as.character(game_id),
        ESPN_TEAM_ID   = as.character(team_id),
        TEAM           = as.character(team),
        ESPN_OPP_ID    = as.character(opp_id),
        OPP            = as.character(opp),
        HOME_AWAY_SYM  = as.character(home_away_sym),
        HOME_AWAY      = as.character(home_away)
      ) %>%
      distinct(ESPN_TEAM_ID, .keep_all = TRUE),
    by = "ESPN_TEAM_ID"
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Pull in Team Names and Opp Names
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     ooooooooo.              o8o                  .                 ooooooooo.                          o8o                         .    o8o                                 
#     `888   `Y88.            `"'                .o8                 `888   `Y88.                        `"'                       .o8    `"'                                 
#      888   .d88'  .ooooo.  oooo  ooo. .oo.   .o888oo  .oooo.o       888   .d88' oooo d8b  .ooooo.     oooo  .ooooo.   .ooooo.  .o888oo oooo   .ooooo.  ooo. .oo.    .oooo.o 
#      888ooo88P'  d88' `88b `888  `888P"Y88b    888   d88(  "8       888ooo88P'  `888""8P d88' `88b    `888 d88' `88b d88' `"Y8   888   `888  d88' `88b `888P"Y88b  d88(  "8 
#      888         888   888  888   888   888    888   `"Y88b.        888          888     888   888     888 888ooo888 888         888    888  888   888  888   888  `"Y88b.  
#      888         888   888  888   888   888    888 . o.  )88b       888          888     888   888     888 888    .o 888   .o8   888 .  888  888   888  888   888  o.  )88b 
#     o888o        `Y8bod8P' o888o o888o o888o   "888" 8""888P'      o888o        d888b    `Y8bod8P'     888 `Y8bod8P' `Y8bod8P'   "888" o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                        888                                                                  
#                                                                                                    .o. 88P                                                                  
#                                                                                                    `Y888P                                   
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Points Profile (BACKTEST ONLY)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control"),
  exists("num0")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# A) Build PPM baseline (LEAKAGE-SAFE, CGS)
# --------------------------------------------------
build_ppm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = as.Date(GAME_DATE),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      PTS_NUM        = num0(PTS_CGS,  NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      pts_per_min = ifelse(
        sum(MINS_NUM) > 0,
        sum(PTS_NUM) / sum(MINS_NUM),
        NA_real_
      ),
      .groups = "drop"
    )
}

# --------------------------------------------------
# BACKTEST ONLY (STRICTLY < GAME_DATE)
# --------------------------------------------------
player_projections_backtest <- player_projections_backtest %>%
  mutate(GAME_DATE = as.Date(GAME_DATE))

bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
bt_dates <- bt_dates[!is.na(bt_dates)]

ppm_cache <- new.env(parent = emptyenv())

get_ppm_df <- function(dd) {
  k <- format(dd, "%Y%m%d")
  if (exists(k, envir = ppm_cache)) return(get(k, envir = ppm_cache))
  df <- build_ppm_baseline(dd, 10L)
  assign(k, df, envir = ppm_cache)
  df
}

out <- vector("list", length(bt_dates))

for (i in seq_along(bt_dates)) {
  
  dd  <- bt_dates[i]
  sub <- player_projections_backtest %>%
    filter(GAME_DATE == dd)
  
  if (nrow(sub) == 0) next
  
  ppm_df <- get_ppm_df(dd)
  ppm_df$key <- paste0(ppm_df$ESPN_PLAYER_ID, "_", ppm_df$ESPN_TEAM_ID)
  ppm_map <- setNames(ppm_df$pts_per_min, ppm_df$key)
  
  league_ppm <- mean(ppm_df$pts_per_min, na.rm = TRUE)
  
  sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
  
  sub$pts_per_min <- unname(ppm_map[sub$key])
  
  sub$pts_per_min_sim <- vapply(
    sub$pts_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_ppm,
        shrink_w  = MC_VAR$shoot_shrink,
        jitter_sd = MC_VAR$pct_jitter_sd,
        lower     = 0.30,
        upper     = 1.60
      )
    },
    numeric(1)
  )
  
  sub$key <- NULL
  out[[i]] <- sub
}

player_projections_backtest <- dplyr::bind_rows(out)


message(
  "POINTS built (BACKTEST, quarter-based, variance-controlled) | rows=",
  nrow(player_projections_backtest)
)

# ======================================
# END: Points Profile (BACKTEST ONLY)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     ooooooooo.              .o8                                               .o8       ooooooooo.                          o8o                         .    o8o                                 
#     `888   `Y88.           "888                                              "888       `888   `Y88.                        `"'                       .o8    `"'                                 
#      888   .d88'  .ooooo.   888oooo.   .ooooo.  oooo  oooo  ooo. .oo.    .oooo888        888   .d88' oooo d8b  .ooooo.     oooo  .ooooo.   .ooooo.  .o888oo oooo   .ooooo.  ooo. .oo.    .oooo.o 
#      888ooo88P'  d88' `88b  d88' `88b d88' `88b `888  `888  `888P"Y88b  d88' `888        888ooo88P'  `888""8P d88' `88b    `888 d88' `88b d88' `"Y8   888   `888  d88' `88b `888P"Y88b  d88(  "8 
#      888`88b.    888ooo888  888   888 888   888  888   888   888   888  888   888        888          888     888   888     888 888ooo888 888         888    888  888   888  888   888  `"Y88b.  
#      888  `88b.  888    .o  888   888 888   888  888   888   888   888  888   888        888          888     888   888     888 888    .o 888   .o8   888 .  888  888   888  888   888  o.  )88b 
#     o888o  o888o `Y8bod8P'  `Y8bod8P' `Y8bod8P'  `V88V"V8P' o888o o888o `Y8bod88P"      o888o        d888b    `Y8bod8P'     888 `Y8bod8P' `Y8bod8P'   "888" o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                                             888                                                                  
#                                                                                                                         .o. 88P                                                                  
#                                                                                                                         `Y888P                         
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Rebounds Rate Profile (BACKTEST ONLY)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control"),
  exists("num0")
)

# --------------------------------------------------
# A) Build REB / MIN baseline (LEAKAGE-SAFE, CGS)
# --------------------------------------------------
build_rpm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = as.Date(GAME_DATE),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      REB_NUM        = num0(REB_CGS,  NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      reb_per_min = ifelse(
        sum(MINS_NUM) > 0,
        sum(REB_NUM) / sum(MINS_NUM),
        NA_real_
      ),
      .groups = "drop"
    )
}

# --------------------------------------------------
# BACKTEST ONLY (STRICTLY < GAME_DATE)
# --------------------------------------------------
player_projections_backtest <- player_projections_backtest %>%
  mutate(GAME_DATE = as.Date(GAME_DATE))

bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
bt_dates <- bt_dates[!is.na(bt_dates)]

rpm_cache <- new.env(parent = emptyenv())

get_rpm_df <- function(dd) {
  k <- format(dd, "%Y%m%d")
  if (exists(k, envir = rpm_cache)) return(get(k, envir = rpm_cache))
  df <- build_rpm_baseline(dd, 10L)
  assign(k, df, envir = rpm_cache)
  df
}

out <- vector("list", length(bt_dates))

for (i in seq_along(bt_dates)) {
  
  dd  <- bt_dates[i]
  sub <- player_projections_backtest %>%
    filter(GAME_DATE == dd)
  
  if (nrow(sub) == 0) next
  
  rpm_df <- get_rpm_df(dd)
  rpm_df$key <- paste0(rpm_df$ESPN_PLAYER_ID, "_", rpm_df$ESPN_TEAM_ID)
  rpm_map <- setNames(rpm_df$reb_per_min, rpm_df$key)
  
  league_rpm <- mean(rpm_df$reb_per_min, na.rm = TRUE)
  
  sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
  sub$reb_per_min <- unname(rpm_map[sub$key])
  
  # ---- variance-controlled REB/MIN ----
  sub$reb_per_min_adj <- vapply(
    sub$reb_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_rpm,
        shrink_w  = MC_VAR$reb_shrink,
        jitter_sd = MC_VAR$reb_jitter_sd,
        lower     = MC_VAR$reb_rpm_lo,
        upper     = MC_VAR$reb_rpm_hi
      )
    },
    numeric(1)
  )
  
  sub$key <- NULL
  out[[i]] <- sub
}

player_projections_backtest <- dplyr::bind_rows(out)

message(
  "REB rate built (BACKTEST ONLY) | rows=",
  nrow(player_projections_backtest)
)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Rebounds Rate Profile (BACKTEST ONLY)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooo.              .o8                                               .o8           
#     `888   `Y88.           "888                                              "888           
#      888   .d88'  .ooooo.   888oooo.   .ooooo.  oooo  oooo  ooo. .oo.    .oooo888   .oooo.o 
#      888ooo88P'  d88' `88b  d88' `88b d88' `88b `888  `888  `888P"Y88b  d88' `888  d88(  "8 
#      888`88b.    888ooo888  888   888 888   888  888   888   888   888  888   888  `"Y88b.  
#      888  `88b.  888    .o  888   888 888   888  888   888   888   888  888   888  o.  )88b 
#     o888o  o888o `Y8bod8P'  `Y8bod8P' `Y8bod8P'  `V88V"V8P' o888o o888o `Y8bod88P" 8""888P' 
#
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠


 
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# START: TEAM REBOUND PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS (GAME/DATE + QTR)
#   Creates:
#     - team_reb_profile_q_weaved (team_id x GAME_DATE x qtr)
#
#   Pulls from:
#     - player_projections_by_date[[k]]  (single date)
#     - player_projections_backtest      (all dates)
#
#   Requires in player projections DF (quarter-based):
#     ESPN_TEAM_ID
#     expected_minutes_adj_Q1..Q6
#     rebounds_proj_Q1..Q6
#     rebounds_sim_Q1..Q6
#
#   Optional (only if present; otherwise filled as 0):
#     orebounds_proj_Q1..Q6 / orebounds_sim_Q1..Q6
#     drebounds_proj_Q1..Q6 / drebounds_sim_Q1..Q6
#
#   Output columns:
#     team_id, GAME_DATE, qtr,
#     REB_mean, REB_sd, REB_sim,
#     OREB_mean, OREB_sd, OREB_sim,
#     DREB_mean, DREB_sd, DREB_sim,
#     OREB_share_mean
#
#   Notes:
#   - This is a per-game profile derived from your player projection layer.
#   - REB_sd/OREB_sd/DREB_sd are “single-draw implied” SDs (abs(sim - mean)).
#     If you later run multiple MC draws per game, we can replace this with true SD.
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Projection date + key (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in team reb profile section.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# helpers
# --------------------------------------------------
numN <- function(x) suppressWarnings(as.numeric(x))
clamp01 <- function(x) pmin(pmax(x, 0), 1)

# --------------------------------------------------
# A) Build TEAM REB profile from ONE player projection DF
# --------------------------------------------------
build_team_reb_profile_from_player_df <- function(proj_df, game_date_value) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(dplyr::tibble())
  
  stopifnot("ESPN_TEAM_ID" %in% names(proj_df))
  
  # required cols
  need_mins <- paste0("expected_minutes_adj_", quarters)
  need_reb_proj <- paste0("rebounds_proj_", quarters)
  need_reb_sim  <- paste0("rebounds_sim_",  quarters)
  
  if (!all(need_mins %in% names(proj_df))) {
    stop("Missing expected_minutes_adj_Q* columns in player projections DF.")
  }
  if (!all(need_reb_proj %in% names(proj_df)) || !all(need_reb_sim %in% names(proj_df))) {
    stop("Missing rebounds_proj_Q* or rebounds_sim_Q* columns in player projections DF.")
  }
  
  # optional cols (use if they exist, else fill 0s)
  need_oreb_proj <- paste0("orebounds_proj_", quarters)
  need_oreb_sim  <- paste0("orebounds_sim_",  quarters)
  need_dreb_proj <- paste0("drebounds_proj_", quarters)
  need_dreb_sim  <- paste0("drebounds_sim_",  quarters)
  
  has_oreb <- all(need_oreb_proj %in% names(proj_df)) && all(need_oreb_sim %in% names(proj_df))
  has_dreb <- all(need_dreb_proj %in% names(proj_df)) && all(need_dreb_sim %in% names(proj_df))
  
  proj_df <- proj_df %>%
    dplyr::mutate(
      team_id   = as.character(ESPN_TEAM_ID),
      GAME_DATE = as.Date(game_date_value)
    ) %>%
    dplyr::filter(!is.na(team_id), team_id != "")
  
  # --- team minutes by quarter (sum expected minutes) ---
  mins_long <- proj_df %>%
    dplyr::select(team_id, GAME_DATE, dplyr::all_of(need_mins)) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = "qtr",
      names_pattern = "^expected_minutes_adj_(Q[1-6])$",
      values_to = "team_minutes"
    ) %>%
    dplyr::mutate(
      team_minutes = numN(team_minutes),
      qtr = as.character(qtr)
    ) %>%
    dplyr::group_by(team_id, GAME_DATE, qtr) %>%
    dplyr::summarise(team_minutes = sum(team_minutes, na.rm = TRUE), .groups = "drop")
  
  # --- REB proj/sim by quarter (sum players) ---
  reb_long <- proj_df %>%
    dplyr::select(team_id, GAME_DATE,
                  dplyr::all_of(need_reb_proj),
                  dplyr::all_of(need_reb_sim)) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = c("kind","qtr"),
      names_pattern = "^(rebounds_proj|rebounds_sim)_(Q[1-6])$",
      values_to = "val"
    ) %>%
    dplyr::mutate(val = numN(val), qtr = as.character(qtr)) %>%
    dplyr::group_by(team_id, GAME_DATE, qtr, kind) %>%
    dplyr::summarise(val_sum = sum(val, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = kind, values_from = val_sum)
  
  # --- OREB proj/sim (optional) ---
  if (has_oreb) {
    oreb_long <- proj_df %>%
      dplyr::select(team_id, GAME_DATE,
                    dplyr::all_of(need_oreb_proj),
                    dplyr::all_of(need_oreb_sim)) %>%
      tidyr::pivot_longer(
        cols = -c(team_id, GAME_DATE),
        names_to = c("kind","qtr"),
        names_pattern = "^(orebounds_proj|orebounds_sim)_(Q[1-6])$",
        values_to = "val"
      ) %>%
      dplyr::mutate(val = numN(val), qtr = as.character(qtr)) %>%
      dplyr::group_by(team_id, GAME_DATE, qtr, kind) %>%
      dplyr::summarise(val_sum = sum(val, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = kind, values_from = val_sum)
  } else {
    oreb_long <- mins_long %>%
      dplyr::transmute(team_id, GAME_DATE, qtr,
                       orebounds_proj = 0, orebounds_sim = 0)
  }
  
  # --- DREB proj/sim (optional) ---
  if (has_dreb) {
    dreb_long <- proj_df %>%
      dplyr::select(team_id, GAME_DATE,
                    dplyr::all_of(need_dreb_proj),
                    dplyr::all_of(need_dreb_sim)) %>%
      tidyr::pivot_longer(
        cols = -c(team_id, GAME_DATE),
        names_to = c("kind","qtr"),
        names_pattern = "^(drebounds_proj|drebounds_sim)_(Q[1-6])$",
        values_to = "val"
      ) %>%
      dplyr::mutate(val = numN(val), qtr = as.character(qtr)) %>%
      dplyr::group_by(team_id, GAME_DATE, qtr, kind) %>%
      dplyr::summarise(val_sum = sum(val, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = kind, values_from = val_sum)
  } else {
    dreb_long <- mins_long %>%
      dplyr::transmute(team_id, GAME_DATE, qtr,
                       drebounds_proj = 0, drebounds_sim = 0)
  }
  
  # --- combine & compute “implied SD” from single sim draw ---
  out <- mins_long %>%
    dplyr::left_join(reb_long,  by = c("team_id","GAME_DATE","qtr")) %>%
    dplyr::left_join(oreb_long, by = c("team_id","GAME_DATE","qtr")) %>%
    dplyr::left_join(dreb_long, by = c("team_id","GAME_DATE","qtr")) %>%
    dplyr::mutate(
      REB_mean  = numN(rebounds_proj),
      REB_sim   = numN(rebounds_sim),
      REB_sd    = abs(REB_sim - REB_mean),
      
      OREB_mean = numN(orebounds_proj),
      OREB_sim  = numN(orebounds_sim),
      OREB_sd   = abs(OREB_sim - OREB_mean),
      
      DREB_mean = numN(drebounds_proj),
      DREB_sim  = numN(drebounds_sim),
      DREB_sd   = abs(DREB_sim - DREB_mean),
      
      OREB_share_mean = ifelse(!is.na(REB_mean) & REB_mean > 0, clamp01(OREB_mean / REB_mean), NA_real_),
      
      qtr = as.integer(sub("Q","", qtr))
    ) %>%
    dplyr::select(
      team_id, GAME_DATE, qtr,
      REB_mean, REB_sd, REB_sim,
      OREB_mean, OREB_sd, OREB_sim,
      DREB_mean, DREB_sd, DREB_sim,
      OREB_share_mean,
      team_minutes
    ) %>%
    dplyr::arrange(GAME_DATE, team_id, qtr)
  
  out
}

# --------------------------------------------------
# 1) SINGLE DATE (today): build from player_projections_by_date[[k]]
# --------------------------------------------------
team_reb_profile_today <- build_team_reb_profile_from_player_df(
  proj_df         = player_projections_by_date[[k]],
  game_date_value = proj_date
)

# --------------------------------------------------
# 2) BACKTEST (leakage-safe by construction): build per GAME_DATE
#     IMPORTANT: we do NOT create any “after date” version.
# --------------------------------------------------
team_reb_profile_backtest <- dplyr::tibble()

if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    dplyr::mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% dplyr::filter(GAME_DATE == dd)
    
    out_list[[i]] <- build_team_reb_profile_from_player_df(
      proj_df         = sub,
      game_date_value = dd
    )
  }
  
  team_reb_profile_backtest <- dplyr::bind_rows(out_list)
}

# --------------------------------------------------
# 3) Final object (one output only)
# --------------------------------------------------
team_reb_profile_q_weaved <- if (mode == "today") {
  team_reb_profile_today
} else {
  team_reb_profile_backtest
}

stopifnot(nrow(team_reb_profile_q_weaved) > 0)

message(
  "TEAM REB PROFILE (weaved) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | rows=", nrow(team_reb_profile_q_weaved),
  " | has_oreb=", ifelse(any(team_reb_profile_q_weaved$OREB_mean != 0, na.rm = TRUE), "yes", "no"),
  " | has_dreb=", ifelse(any(team_reb_profile_q_weaved$DREB_mean != 0, na.rm = TRUE), "yes", "no")
)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: TEAM REBOUND PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#          .o.                          o8o               .                 ooooooooo.                          o8o                         .    o8o                                 
#          .888.                         `"'             .o8                 `888   `Y88.                        `"'                       .o8    `"'                                 
#         .8"888.      .oooo.o  .oooo.o oooo   .oooo.o .o888oo  .oooo.o       888   .d88' oooo d8b  .ooooo.     oooo  .ooooo.   .ooooo.  .o888oo oooo   .ooooo.  ooo. .oo.    .oooo.o 
#        .8' `888.    d88(  "8 d88(  "8 `888  d88(  "8   888   d88(  "8       888ooo88P'  `888""8P d88' `88b    `888 d88' `88b d88' `"Y8   888   `888  d88' `88b `888P"Y88b  d88(  "8 
#       .88ooo8888.   `"Y88b.  `"Y88b.   888  `"Y88b.    888   `"Y88b.        888          888     888   888     888 888ooo888 888         888    888  888   888  888   888  `"Y88b.  
#      .8'     `888.  o.  )88b o.  )88b  888  o.  )88b   888 . o.  )88b       888          888     888   888     888 888    .o 888   .o8   888 .  888  888   888  888   888  o.  )88b 
#     o88o     o8888o 8""888P' 8""888P' o888o 8""888P'   "888" 8""888P'      o888o        d888b    `Y8bod8P'     888 `Y8bod8P' `Y8bod8P'   "888" o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                                888                                                                  
#                                                                                                            .o. 88P                                                                  
#                                                                                                            `Y888P             
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Assists Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("mode"),
  exists("num0"),
  exists("MC_VAR"),
  exists("shrink_to"),
  exists("apply_pct_jitter"),
  exists("clamp01")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Projection date + key
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in assists projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Rate variance control (ast/min)
#     - shrink to league anchor
#     - optional jitter
#     - clamp to sane bounds
# --------------------------------------------------
apply_rate_variance_control <- function(rate, anchor, shrink_w, jitter_sd, lower, upper) {
  r <- suppressWarnings(as.numeric(rate))
  if (is.na(r)) return(NA_real_)
  a <- suppressWarnings(as.numeric(anchor))
  if (is.na(a)) a <- r
  
  r2 <- shrink_to(r, a, shrink_w)
  r3 <- apply_pct_jitter(r2, jitter_sd)
  
  if (!is.na(lower)) r3 <- pmax(lower, r3)
  if (!is.na(upper)) r3 <- pmin(upper, r3)
  
  r3
}

# --------------------------------------------------
# B) Build AST/MIN baseline (LEAKAGE-SAFE, CGS)
#     + returns league_apm anchor for that cutoff_date
# --------------------------------------------------
build_apm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  hist <- BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      AST_NUM        = num0(AST_CGS, NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    select(GAME_DATE, ESPN_PLAYER_ID, ESPN_TEAM_ID, AST_NUM, MINS_NUM) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE)
  
  # league anchor ast/min (strictly < cutoff_date)
  league_apm <- with(hist, {
    tot_ast  <- sum(AST_NUM,  na.rm = TRUE)
    tot_mins <- sum(MINS_NUM, na.rm = TRUE)
    if (is.finite(tot_mins) && tot_mins > 0) tot_ast / tot_mins else NA_real_
  })
  
  player_apm <- hist %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      ast_lastN  = sum(AST_NUM,  na.rm = TRUE),
      mins_lastN = sum(MINS_NUM, na.rm = TRUE),
      games_used = dplyr::n(),
      ast_per_min = ifelse(mins_lastN > 0, ast_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
  
  list(player_apm = player_apm, league_apm = league_apm)
}

# --------------------------------------------------
# C) Helper: apply QUARTER assist projections to ONE DF
#     - uses expected_minutes_adj_Qx + minutes_sim_Qx
#     - adds assists_proj_Qx / assists_sim_Qx
#     - adds CGS totals
# --------------------------------------------------
apply_assists_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  bl <- build_apm_baseline(cutoff_date, n_games_back)
  apm_df     <- bl$player_apm
  league_apm <- bl$league_apm
  
  # map
  apm_df$ESPN_PLAYER_ID <- as.character(apm_df$ESPN_PLAYER_ID)
  apm_df$ESPN_TEAM_ID   <- as.character(apm_df$ESPN_TEAM_ID)
  apm_df$key <- paste0(apm_df$ESPN_PLAYER_ID, "_", apm_df$ESPN_TEAM_ID)
  apm_map <- setNames(apm_df$ast_per_min, apm_df$key)
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  # raw apm
  proj_df$ast_per_min <- unname(apm_map[proj_df$key])
  
  # variance-controlled apm (single apm applied across all quarters)
  proj_df$ast_per_min_adj <- vapply(
    proj_df$ast_per_min,
    function(r) apply_rate_variance_control(
      rate      = r,
      anchor    = league_apm,
      shrink_w  = MC_VAR$ast_shrink,
      jitter_sd = MC_VAR$ast_jitter_sd,
      lower     = MC_VAR$ast_apm_lo,
      upper     = MC_VAR$ast_apm_hi
    ),
    numeric(1)
  )
  
  # quarter outputs
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    # guard: if some quarter columns don't exist, skip
    if (!(exp_min %in% names(proj_df)) || !(sim_min %in% names(proj_df))) next
    
    proj_df[[paste0("assists_proj_", q)]] <- ifelse(
      !is.na(proj_df$ast_per_min_adj),
      num0(proj_df[[exp_min]], NA_real_) * proj_df$ast_per_min_adj,
      NA_real_
    )
    
    proj_df[[paste0("assists_sim_", q)]] <- ifelse(
      !is.na(proj_df$ast_per_min_adj),
      num0(proj_df[[sim_min]], NA_real_) * proj_df$ast_per_min_adj,
      NA_real_
    )
  }
  
  # CGS totals (derived)
  proj_df <- proj_df %>%
    mutate(
      assists_proj_CGS = rowSums(across(any_of(paste0("assists_proj_", quarters))), na.rm = TRUE),
      assists_sim_CGS  = rowSums(across(any_of(paste0("assists_sim_",  quarters))), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE: update player_projections_by_date[[k]]
# --------------------------------------------------
player_projections_by_date[[k]] <- apply_assists_quarter_df(
  proj_df      = player_projections_by_date[[k]],
  cutoff_date  = proj_date,
  n_games_back = 10L
)

# --------------------------------------------------
# 2) BACKTEST: per-date leakage-safe (cutoff = that GAME_DATE)
#     - caches per-date apm maps + league_apm
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  apm_cache <- new.env(parent = emptyenv())
  
  get_apm_bundle <- function(dd, n_games_back = 10L) {
    key <- format(as.Date(dd), "%Y%m%d")
    if (exists(key, envir = apm_cache, inherits = FALSE)) {
      return(get(key, envir = apm_cache, inherits = FALSE))
    }
    bl <- build_apm_baseline(as.Date(dd), n_games_back = n_games_back)
    
    df <- bl$player_apm
    df$key <- paste0(df$ESPN_PLAYER_ID, "_", df$ESPN_TEAM_ID)
    apm_map <- setNames(df$ast_per_min, df$key)
    
    bundle <- list(apm_map = apm_map, league_apm = bl$league_apm)
    assign(key, bundle, envir = apm_cache)
    bundle
  }
  
  out <- vector("list", length(bt_dates))
  
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    bun <- get_apm_bundle(dd, n_games_back = 10L)
    apm_map    <- bun$apm_map
    league_apm <- bun$league_apm
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$ast_per_min <- unname(apm_map[sub$key])
    
    sub$ast_per_min_adj <- vapply(
      sub$ast_per_min,
      function(r) apply_rate_variance_control(
        rate      = r,
        anchor    = league_apm,
        shrink_w  = MC_VAR$ast_shrink,
        jitter_sd = MC_VAR$ast_jitter_sd,
        lower     = MC_VAR$ast_apm_lo,
        upper     = MC_VAR$ast_apm_hi
      ),
      numeric(1)
    )
    
    for (q in quarters) {
      exp_min <- paste0("expected_minutes_adj_", q)
      sim_min <- paste0("minutes_sim_", q)
      if (!(exp_min %in% names(sub)) || !(sim_min %in% names(sub))) next
      
      sub[[paste0("assists_proj_", q)]] <- ifelse(
        !is.na(sub$ast_per_min_adj),
        num0(sub[[exp_min]], NA_real_) * sub$ast_per_min_adj,
        NA_real_
      )
      
      sub[[paste0("assists_sim_", q)]] <- ifelse(
        !is.na(sub$ast_per_min_adj),
        num0(sub[[sim_min]], NA_real_) * sub$ast_per_min_adj,
        NA_real_
      )
    }
    
    sub$assists_proj_CGS <- rowSums(sub[paste0("assists_proj_", quarters)], na.rm = TRUE)
    sub$assists_sim_CGS  <- rowSums(sub[paste0("assists_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out)
}

message(
  "ASSISTS (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists('player_projections_backtest'), nrow(player_projections_backtest), 0)
)

# ======================================
# END: Assists Projection (QUARTER-BASED)
# ======================================


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Assists Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Blocks Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ======================================
# START: Blocks Projection (QUARTER-BASED, DERIVED) (BY_DATE + BACKTEST)
# ======================================

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("num0"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Resolve projection date + key (MODE-SENSITIVE)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in blocks projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Build BLK PER MIN baseline (LEAKAGE-SAFE, CGS)
#   - returns list(map = bpm_map, anchor = league_bpm)
# --------------------------------------------------
build_bpm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  hist <- BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      BLK_NUM        = num0(BLK_CGS,  NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    select(GAME_DATE, ESPN_PLAYER_ID, ESPN_TEAM_ID, BLK_NUM, MINS_NUM) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE)
  
  # league anchor (pre-cutoff)
  league_bpm <- hist %>%
    summarise(
      league_bpm = ifelse(sum(MINS_NUM, na.rm = TRUE) > 0,
                          sum(BLK_NUM,  na.rm = TRUE) / sum(MINS_NUM, na.rm = TRUE),
                          NA_real_)
    ) %>%
    pull(league_bpm)
  
  # player bpm from last N games (pre-cutoff)
  per_player <- hist %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      blk_lastN  = sum(BLK_NUM,  na.rm = TRUE),
      mins_lastN = sum(MINS_NUM, na.rm = TRUE),
      blk_per_min = ifelse(mins_lastN > 0, blk_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
  
  per_player$key <- paste0(per_player$ESPN_PLAYER_ID, "_", per_player$ESPN_TEAM_ID)
  bpm_map <- setNames(per_player$blk_per_min, per_player$key)
  
  list(map = bpm_map, anchor = league_bpm)
}

# --------------------------------------------------
# Helper: apply quarter-based blocks to ONE DF
#   - cutoff_date drives leakage-safe bpm baseline
#   - variance control applied to bpm rate (adj_bpm)
# --------------------------------------------------
apply_blocks_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  base <- build_bpm_baseline(cutoff_date, n_games_back)
  bpm_map   <- base$map
  league_bpm <- base$anchor
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  # raw bpm
  proj_df$blk_per_min <- unname(bpm_map[proj_df$key])
  
  # variance-controlled bpm (rowwise to allow rnorm jitter per player)
  proj_df <- proj_df %>%
    rowwise() %>%
    mutate(
      adj_bpm = apply_rate_variance_control(
        rate      = blk_per_min,
        anchor    = league_bpm,
        shrink_w  = MC_VAR$blk_shrink,
        jitter_sd = MC_VAR$blk_jitter_sd,
        lower     = MC_VAR$blk_bpm_lo,
        upper     = MC_VAR$blk_bpm_hi
      )
    ) %>%
    ungroup()
  
  # ---- quarter outputs ----
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    proj_df[[paste0("blocks_proj_", q)]] <-
      ifelse(!is.na(proj_df$adj_bpm), num0(proj_df[[exp_min]], NA_real_) * proj_df$adj_bpm, NA_real_)
    
    proj_df[[paste0("blocks_sim_", q)]] <-
      ifelse(!is.na(proj_df$adj_bpm), num0(proj_df[[sim_min]], NA_real_) * proj_df$adj_bpm, NA_real_)
  }
  
  # ---- CGS totals (derived) ----
  proj_df <- proj_df %>%
    mutate(
      blocks_proj_CGS = rowSums(across(all_of(paste0("blocks_proj_", quarters))), na.rm = TRUE),
      blocks_sim_CGS  = rowSums(across(all_of(paste0("blocks_sim_",  quarters))), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE
# --------------------------------------------------
player_projections_by_date[[k]] <-
  apply_blocks_quarter_df(
    proj_df      = player_projections_by_date[[k]],
    cutoff_date  = proj_date,
    n_games_back = 10L
  )

# --------------------------------------------------
# 2) BACKTEST (STRICTLY < each GAME_DATE)
#   - cache bpm_map + league anchor per date
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  bpm_cache <- new.env(parent = emptyenv())
  
  get_bpm_pack <- function(dd, n_games_back = 10L) {
    kk <- format(as.Date(dd), "%Y%m%d")
    if (exists(kk, envir = bpm_cache, inherits = FALSE)) {
      return(get(kk, envir = bpm_cache, inherits = FALSE))
    }
    pack <- build_bpm_baseline(as.Date(dd), n_games_back = n_games_back)
    assign(kk, pack, envir = bpm_cache)
    pack
  }
  
  out <- vector("list", length(bt_dates))
  
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    pack <- get_bpm_pack(dd, n_games_back = 10L)
    bpm_map    <- pack$map
    league_bpm <- pack$anchor
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$blk_per_min <- unname(bpm_map[sub$key])
    
    sub <- sub %>%
      rowwise() %>%
      mutate(
        adj_bpm = apply_rate_variance_control(
          rate      = blk_per_min,
          anchor    = league_bpm,
          shrink_w  = MC_VAR$blk_shrink,
          jitter_sd = MC_VAR$blk_jitter_sd,
          lower     = MC_VAR$blk_bpm_lo,
          upper     = MC_VAR$blk_bpm_hi
        )
      ) %>%
      ungroup()
    
    for (q in quarters) {
      sub[[paste0("blocks_proj_", q)]] <-
        ifelse(!is.na(sub$adj_bpm),
               num0(sub[[paste0("expected_minutes_adj_", q)]], NA_real_) * sub$adj_bpm,
               NA_real_)
      
      sub[[paste0("blocks_sim_", q)]]  <-
        ifelse(!is.na(sub$adj_bpm),
               num0(sub[[paste0("minutes_sim_", q)]], NA_real_) * sub$adj_bpm,
               NA_real_)
    }
    
    sub$blocks_proj_CGS <- rowSums(sub[paste0("blocks_proj_", quarters)], na.rm = TRUE)
    sub$blocks_sim_CGS  <- rowSums(sub[paste0("blocks_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out)
}

message(
  "BLOCKS (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists("player_projections_backtest"), nrow(player_projections_backtest), 0)
)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Blocks Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Steals Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("num0"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control")  # <- from variance controls section
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Projection date + key (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in steals projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Build STEALS PER MIN baseline (LEAKAGE-SAFE, CGS)
#    Uses BaseStats_Player_MC only (no joins to rotations)
# --------------------------------------------------
build_spm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      STL_NUM        = num0(STL_CGS, NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      stl_lastN   = sum(STL_NUM,  na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      stl_per_min = ifelse(mins_lastN > 0, stl_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# --------------------------------------------------
# Helper: apply steals (quarter-based) to ONE DF
#   - cutoff_date drives leakage-safe spm baseline
#   - variance control applied to stl_per_min -> adj_spm
# --------------------------------------------------
apply_steals_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  cutoff_date <- as.Date(cutoff_date)
  
  spm_df <- build_spm_baseline(cutoff_date, n_games_back)
  spm_df$ESPN_PLAYER_ID <- as.character(spm_df$ESPN_PLAYER_ID)
  spm_df$ESPN_TEAM_ID   <- as.character(spm_df$ESPN_TEAM_ID)
  spm_df$key <- paste0(spm_df$ESPN_PLAYER_ID, "_", spm_df$ESPN_TEAM_ID)
  
  spm_map <- setNames(spm_df$stl_per_min, spm_df$key)
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  proj_df$stl_per_min <- unname(spm_map[proj_df$key])
  
  # ---- league anchor (computed from the same leakage-safe baseline window) ----
  league_spm <- mean(spm_df$stl_per_min, na.rm = TRUE)
  if (is.nan(league_spm) || is.na(league_spm)) league_spm <- 0.0
  
  # ---- variance-controlled per-player rate ----
  # NOTE: expects MC_VAR fields: stl_shrink, stl_jitter_sd, stl_spm_lo, stl_spm_hi
  proj_df$adj_spm <- vapply(
    proj_df$stl_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_spm,
        shrink_w  = MC_VAR$stl_shrink,
        jitter_sd = MC_VAR$stl_jitter_sd,
        lower     = MC_VAR$stl_spm_lo,
        upper     = MC_VAR$stl_spm_hi
      )
    },
    numeric(1)
  )
  
  # ---- quarter outputs ----
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    proj_df[[paste0("steals_proj_", q)]] <-
      ifelse(!is.na(proj_df$adj_spm), proj_df[[exp_min]] * proj_df$adj_spm, NA_real_)
    
    proj_df[[paste0("steals_sim_", q)]]  <-
      ifelse(!is.na(proj_df$adj_spm), proj_df[[sim_min]] * proj_df$adj_spm, NA_real_)
  }
  
  # ---- CGS totals ----
  proj_df <- proj_df %>%
    mutate(
      steals_proj_CGS = rowSums(across(paste0("steals_proj_", quarters)), na.rm = TRUE),
      steals_sim_CGS  = rowSums(across(paste0("steals_sim_",  quarters)), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE: update player_projections_by_date[[k]]
# --------------------------------------------------
player_projections_by_date[[k]] <- apply_steals_quarter_df(
  proj_df      = player_projections_by_date[[k]],
  cutoff_date  = proj_date,
  n_games_back = 10L
)

# --------------------------------------------------
# 2) BACKTEST: per-date leakage-safe (cutoff = that GAME_DATE)
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  # cache spm maps per date
  spm_cache <- new.env(parent = emptyenv())
  
  get_spm_pack_for_date <- function(dd, n_games_back = 10L) {
    key <- format(as.Date(dd), "%Y%m%d")
    if (exists(key, envir = spm_cache, inherits = FALSE)) {
      return(get(key, envir = spm_cache, inherits = FALSE))
    }
    df <- build_spm_baseline(as.Date(dd), n_games_back = n_games_back)
    df$key <- paste0(df$ESPN_PLAYER_ID, "_", df$ESPN_TEAM_ID)
    m <- setNames(df$stl_per_min, df$key)
    league_spm <- mean(df$stl_per_min, na.rm = TRUE)
    if (is.nan(league_spm) || is.na(league_spm)) league_spm <- 0.0
    pack <- list(map = m, league = league_spm)
    assign(key, pack, envir = spm_cache)
    pack
  }
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    pack <- get_spm_pack_for_date(dd, n_games_back = 10L)
    spm_map   <- pack$map
    league_spm <- pack$league
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$stl_per_min <- unname(spm_map[sub$key])
    
    sub$adj_spm <- vapply(
      sub$stl_per_min,
      function(r) {
        apply_rate_variance_control(
          rate      = r,
          anchor    = league_spm,
          shrink_w  = MC_VAR$stl_shrink,
          jitter_sd = MC_VAR$stl_jitter_sd,
          lower     = MC_VAR$stl_spm_lo,
          upper     = MC_VAR$stl_spm_hi
        )
      },
      numeric(1)
    )
    
    for (q in quarters) {
      sub[[paste0("steals_proj_", q)]] <-
        ifelse(!is.na(sub$adj_spm),
               sub[[paste0("expected_minutes_adj_", q)]] * sub$adj_spm,
               NA_real_)
      sub[[paste0("steals_sim_", q)]]  <-
        ifelse(!is.na(sub$adj_spm),
               sub[[paste0("minutes_sim_", q)]] * sub$adj_spm,
               NA_real_)
    }
    
    sub$steals_proj_CGS <- rowSums(sub[paste0("steals_proj_", quarters)], na.rm = TRUE)
    sub$steals_sim_CGS  <- rowSums(sub[paste0("steals_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out_list[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out_list)
}

message(
  "STEALS (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists("player_projections_backtest"), nrow(player_projections_backtest), 0)
)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Steals Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Turnovers Projection (PLAYER_ROTATIONS-Based)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("num0"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control") # <- variance controls section
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Resolve projection date + key (MODE-SENSITIVE)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in turnovers projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Build TOV PER MIN baseline (LEAKAGE-SAFE, CGS)
#    Uses BaseStats_Player_MC only (no joins to rotations)
# --------------------------------------------------
build_tovpm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      TOV_NUM        = num0(TOV_CGS, NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      tov_lastN   = sum(TOV_NUM,  na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      tov_per_min = ifelse(mins_lastN > 0, tov_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# --------------------------------------------------
# Helper: apply turnovers (quarter-based) to ONE DF
#   - cutoff_date drives leakage-safe tovpm baseline
#   - variance control applied to tov_per_min -> adj_tovpm
# --------------------------------------------------
apply_turnovers_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  cutoff_date <- as.Date(cutoff_date)
  
  tovpm_df <- build_tovpm_baseline(cutoff_date, n_games_back)
  tovpm_df$ESPN_PLAYER_ID <- as.character(tovpm_df$ESPN_PLAYER_ID)
  tovpm_df$ESPN_TEAM_ID   <- as.character(tovpm_df$ESPN_TEAM_ID)
  tovpm_df$key <- paste0(tovpm_df$ESPN_PLAYER_ID, "_", tovpm_df$ESPN_TEAM_ID)
  
  tovpm_map <- setNames(tovpm_df$tov_per_min, tovpm_df$key)
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  proj_df$tov_per_min <- unname(tovpm_map[proj_df$key])
  
  # ---- league anchor (computed from the same leakage-safe baseline window) ----
  league_tovpm <- mean(tovpm_df$tov_per_min, na.rm = TRUE)
  if (is.nan(league_tovpm) || is.na(league_tovpm)) league_tovpm <- 0.0
  
  # ---- variance-controlled per-player rate ----
  # NOTE: expects MC_VAR fields: tov_shrink, tov_jitter_sd, tov_tovpm_lo, tov_tovpm_hi
  proj_df$adj_tovpm <- vapply(
    proj_df$tov_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_tovpm,
        shrink_w  = MC_VAR$tov_shrink,
        jitter_sd = MC_VAR$tov_jitter_sd,
        lower     = MC_VAR$tov_tovpm_lo,
        upper     = MC_VAR$tov_tovpm_hi
      )
    },
    numeric(1)
  )
  
  # ---- quarter outputs ----
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    proj_df[[paste0("turnovers_proj_", q)]] <-
      ifelse(!is.na(proj_df$adj_tovpm), proj_df[[exp_min]] * proj_df$adj_tovpm, NA_real_)
    
    proj_df[[paste0("turnovers_sim_", q)]]  <-
      ifelse(!is.na(proj_df$adj_tovpm), proj_df[[sim_min]] * proj_df$adj_tovpm, NA_real_)
  }
  
  # ---- CGS totals ----
  proj_df <- proj_df %>%
    mutate(
      turnovers_proj_CGS = rowSums(across(paste0("turnovers_proj_", quarters)), na.rm = TRUE),
      turnovers_sim_CGS  = rowSums(across(paste0("turnovers_sim_",  quarters)), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE
# --------------------------------------------------
player_projections_by_date[[k]] <- apply_turnovers_quarter_df(
  proj_df      = player_projections_by_date[[k]],
  cutoff_date  = proj_date,
  n_games_back = 10L
)

# --------------------------------------------------
# 2) BACKTEST (STRICTLY < each GAME_DATE)
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  tovpm_cache <- new.env(parent = emptyenv())
  
  get_tovpm_pack_for_date <- function(dd, n_games_back = 10L) {
    key <- format(as.Date(dd), "%Y%m%d")
    if (exists(key, envir = tovpm_cache, inherits = FALSE)) {
      return(get(key, envir = tovpm_cache, inherits = FALSE))
    }
    df <- build_tovpm_baseline(as.Date(dd), n_games_back = n_games_back)
    df$key <- paste0(df$ESPN_PLAYER_ID, "_", df$ESPN_TEAM_ID)
    m <- setNames(df$tov_per_min, df$key)
    league_tovpm <- mean(df$tov_per_min, na.rm = TRUE)
    if (is.nan(league_tovpm) || is.na(league_tovpm)) league_tovpm <- 0.0
    pack <- list(map = m, league = league_tovpm)
    assign(key, pack, envir = tovpm_cache)
    pack
  }
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    pack <- get_tovpm_pack_for_date(dd, n_games_back = 10L)
    tovpm_map    <- pack$map
    league_tovpm <- pack$league
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$tov_per_min <- unname(tovpm_map[sub$key])
    
    sub$adj_tovpm <- vapply(
      sub$tov_per_min,
      function(r) {
        apply_rate_variance_control(
          rate      = r,
          anchor    = league_tovpm,
          shrink_w  = MC_VAR$tov_shrink,
          jitter_sd = MC_VAR$tov_jitter_sd,
          lower     = MC_VAR$tov_tovpm_lo,
          upper     = MC_VAR$tov_tovpm_hi
        )
      },
      numeric(1)
    )
    
    for (q in quarters) {
      sub[[paste0("turnovers_proj_", q)]] <-
        ifelse(!is.na(sub$adj_tovpm),
               sub[[paste0("expected_minutes_adj_", q)]] * sub$adj_tovpm,
               NA_real_)
      sub[[paste0("turnovers_sim_", q)]]  <-
        ifelse(!is.na(sub$adj_tovpm),
               sub[[paste0("minutes_sim_", q)]] * sub$adj_tovpm,
               NA_real_)
    }
    
    sub$turnovers_proj_CGS <- rowSums(sub[paste0("turnovers_proj_", quarters)], na.rm = TRUE)
    sub$turnovers_sim_CGS  <- rowSums(sub[paste0("turnovers_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out_list[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out_list)
}

message(
  "TURNOVERS (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists("player_projections_backtest"), nrow(player_projections_backtest), 0)
)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Turnovers Projection (PLAYER_ROTATIONS-Based)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooooooo                                                                                    
#     8'   888   `8                                                                                    
#          888      oooo  oooo  oooo d8b ooo. .oo.    .ooooo.  oooo    ooo  .ooooo.  oooo d8b  .oooo.o 
#          888      `888  `888  `888""8P `888P"Y88b  d88' `88b  `88.  .8'  d88' `88b `888""8P d88(  "8 
#          888       888   888   888      888   888  888   888   `88..8'   888ooo888  888     `"Y88b.  
#          888       888   888   888      888   888  888   888    `888'    888    .o  888     o.  )88b 
#         o888o      `V88V"V8P' d888b    o888o o888o `Y8bod8P'     `8'     `Y8bod8P' d888b    8""888P' 
#                                                                                                 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠




# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# START: TEAM TURNOVER PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS (GAME/DATE + QTR)
#   Creates:
#     - team_to_profile_q_weaved  (team_id x GAME_DATE x qtr)
#
#   Pulls from:
#     - player_projections_by_date[[k]]  (single date)
#     - player_projections_backtest      (all dates)
#
#   Requires in player projections DF (quarter-based):
#     ESPN_TEAM_ID
#     expected_minutes_adj_Q1..Q6
#     turnovers_proj_Q1..Q6
#     turnovers_sim_Q1..Q6
#
#   Output columns (required by TO engine / PTS engine):
#     team_id, GAME_DATE, qtr,
#     TOV_pct, TOV_live_share,
#     TO_per_poss (alias)
#
#   Notes:
#   - TOV_pct is interpreted as per-possession turnover probability.
#     Since we're deriving from players, we proxy possessions from team minutes:
#       poss_proxy ≈ (team_minutes / 48) * league_poss_per_game
#     Default league_poss_per_game = 99 (tweakable).
#   - TOV_live_share: if you don’t have live-ball turnover projections yet, we default to 0.60.
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Projection date + key (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in team TO profile section.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# helpers
# --------------------------------------------------
numN <- function(x) suppressWarnings(as.numeric(x))
clamp01 <- function(x) pmin(pmax(x, 0), 1)

# --------------------------------------------------
# A) Build TEAM TO profile from ONE player projection DF
# --------------------------------------------------
build_team_to_profile_from_player_df <- function(
    proj_df,
    game_date_value,
    league_poss_per_game = 99,
    team_minutes_per_game = 240
) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) {
    return(dplyr::tibble())
  }
  
  stopifnot("ESPN_TEAM_ID" %in% names(proj_df))
  
  # must have at least turnovers_proj_Q* present
  need_any <- c(paste0("turnovers_proj_", quarters), paste0("turnovers_sim_", quarters))
  if (!any(need_any %in% names(proj_df))) {
    stop("Missing turnovers_proj_Q*/turnovers_sim_Q* columns in player projections DF.")
  }
  
  # minutes columns must exist (you already have these in the minutes section)
  need_mins <- paste0("expected_minutes_adj_", quarters)
  if (!all(need_mins %in% names(proj_df))) {
    stop("Missing expected_minutes_adj_Q* columns in player projections DF.")
  }
  
  proj_df <- proj_df %>%
    dplyr::mutate(
      team_id   = as.character(ESPN_TEAM_ID),
      GAME_DATE = as.Date(game_date_value)
    ) %>%
    dplyr::filter(!is.na(team_id), team_id != "")
  
  # --- team minutes by quarter (sum expected minutes) ---
  mins_long <- proj_df %>%
    dplyr::select(team_id, GAME_DATE, dplyr::all_of(need_mins)) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = "qtr",
      names_pattern = "^expected_minutes_adj_(Q[1-6])$",
      values_to = "team_minutes"
    ) %>%
    dplyr::mutate(
      team_minutes = numN(team_minutes),
      qtr = as.character(qtr)
    ) %>%
    dplyr::group_by(team_id, GAME_DATE, qtr) %>%
    dplyr::summarise(team_minutes = sum(team_minutes, na.rm = TRUE), .groups = "drop")
  
  # --- turnovers by quarter (sum player turnovers) ---
  tov_long <- proj_df %>%
    dplyr::select(team_id, GAME_DATE,
                  dplyr::any_of(paste0("turnovers_proj_", quarters)),
                  dplyr::any_of(paste0("turnovers_sim_",  quarters)),
                  dplyr::any_of(paste0("live_turnovers_proj_", quarters)),
                  dplyr::any_of(paste0("live_turnovers_sim_",  quarters))) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = c("kind","qtr"),
      names_pattern = "^(turnovers_proj|turnovers_sim|live_turnovers_proj|live_turnovers_sim)_(Q[1-6])$",
      values_to = "val"
    ) %>%
    dplyr::mutate(
      val = numN(val),
      qtr = as.character(qtr)
    ) %>%
    dplyr::group_by(team_id, GAME_DATE, qtr, kind) %>%
    dplyr::summarise(val_sum = sum(val, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = kind, values_from = val_sum)
  
  # --- join minutes + turnovers and compute per-possession rate ---
  out <- mins_long %>%
    dplyr::left_join(tov_long, by = c("team_id","GAME_DATE","qtr")) %>%
    dplyr::mutate(
      # possessions proxy scaled by quarter minutes
      poss_proxy = (pmax(team_minutes, 0) / team_minutes_per_game) * league_poss_per_game,
      
      TOV_mean = numN(turnovers_proj),
      TOV_sim  = numN(turnovers_sim),
      
      # per-possession turnover probability (bounded)
      TOV_pct = ifelse(!is.na(poss_proxy) & poss_proxy > 0,
                       pmin(pmax(TOV_mean / poss_proxy, 0), 0.35),
                       0.13),
      
      TO_per_poss = TOV_pct,
      
      # live-ball share (if live turnovers exist; else default)
      live_mean = if ("live_turnovers_proj" %in% names(.)) numN(live_turnovers_proj) else NA_real_,
      TOV_live_share = ifelse(!is.na(TOV_mean) & TOV_mean > 0 & !is.na(live_mean),
                              clamp01(live_mean / TOV_mean),
                              0.60),
      
      qtr = as.integer(sub("Q","", qtr))
    ) %>%
    dplyr::select(
      team_id, GAME_DATE, qtr,
      TOV_pct, TOV_live_share, TO_per_poss,
      poss_proxy, team_minutes, TOV_mean, TOV_sim
    ) %>%
    dplyr::arrange(GAME_DATE, team_id, qtr)
  
  out
}

# --------------------------------------------------
# 1) SINGLE DATE: build team_to_profile_q_weaved_for_date
# --------------------------------------------------
team_to_profile_q_weaved_for_date <- build_team_to_profile_from_player_df(
  proj_df         = player_projections_by_date[[k]],
  game_date_value = proj_date
)

# --------------------------------------------------
# 2) BACKTEST: build team_to_profile_q_weaved (all dates)
# --------------------------------------------------
team_to_profile_q_weaved_backtest <- dplyr::tibble()

if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    dplyr::mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% dplyr::filter(GAME_DATE == dd)
    
    out_list[[i]] <- build_team_to_profile_from_player_df(
      proj_df         = sub,
      game_date_value = dd
    )
  }
  
  team_to_profile_q_weaved_backtest <- dplyr::bind_rows(out_list)
}

# --------------------------------------------------
# 3) Choose final object name (matches your pattern)
# --------------------------------------------------
team_to_profile_q_weaved <- if (mode == "today") {
  team_to_profile_q_weaved_for_date
} else {
  team_to_profile_q_weaved_backtest
}

stopifnot(nrow(team_to_profile_q_weaved) > 0)

message(
  "TEAM TO PROFILE (weaved) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | rows=", nrow(team_to_profile_q_weaved)
)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: TEAM TURNOVER PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ===============================================================
# TOV / LIVE-TOV SECTION (Quarter-level)
#   + Uses team_to_profile_q (team-specific TO% + live share)
#   + Blends TO% toward league anchor via MC_VAR$tov_rand (optional)
#   + Guarantees sane bounds + integer outputs
# ===============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# Utility helpers for probability safety and normalization.
# clamp01() bounds values to [0,1], while pct01() converts percentages to probabilities, applies defaults for NA, and enforces valid probability limits.
# ===============================================================
# TOV / LIVE-TOV SECTION (Quarter-level, PER GAME)
#   - Consumes team_to_profile_q_game
#   - Designed to plug directly into possessions-driven MC
# ===============================================================

clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  clamp01(x)
}

num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

shrink_to <- function(p, anchor, w) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) p <- anchor
  w <- clamp01(w)
  clamp01((1 - w) * p + w * anchor)
}

if (!exists("MC_ANCHOR")) {
  MC_ANCHOR <- list(to = 0.13)
}

# ---------------------------------------------------------------
# 1) Pull per-game quarter TOV inputs
# ---------------------------------------------------------------
get_team_tov_inputs_q_game <- function(team_id, game_date, qtr,
                                       team_to_profile_q_game,
                                       mc_anchor = MC_ANCHOR) {
  
  r <- team_to_profile_q_game %>%
    dplyr::filter(
      team_id == !!team_id,
      GAME_DATE == !!game_date,
      qtr == !!qtr
    ) %>%
    dplyr::slice(1)
  
  if (nrow(r) == 0) {
    return(list(
      p_to_team  = clamp01(num0(mc_anchor$to, 0.13)),
      live_share = 0.60
    ))
  }
  
  list(
    p_to_team  = pct01(r$TOV_pct, default = mc_anchor$to),
    live_share = clamp01(num0(r$TOV_live_share, 0.60))
  )
}

# ---------------------------------------------------------------
# 2) Final quarter-level TOV probabilities
# ---------------------------------------------------------------
derive_team_tov_probs_q_game <- function(team_id, game_date, qtr,
                                         team_to_profile_q_game,
                                         mc_var = MC_VAR,
                                         mc_anchor = MC_ANCHOR) {
  
  tov_in <- get_team_tov_inputs_q_game(
    team_id   = team_id,
    game_date = game_date,
    qtr       = qtr,
    team_to_profile_q_game = team_to_profile_q_game,
    mc_anchor = mc_anchor
  )
  
  p_to_raw <- clamp01(tov_in$p_to_team)
  live_raw <- clamp01(tov_in$live_share)
  
  p_to <- shrink_to(
    p_to_raw,
    mc_anchor$to,
    clamp01(num0(mc_var$tov_rand, 0))
  )
  
  list(
    p_to       = clamp01(p_to),
    live_share = clamp01(live_raw)
  )
}

# ---------------------------------------------------------------
# 3) Simulate quarter-level turnovers (PER GAME)
# ---------------------------------------------------------------
simulate_team_turnovers_q <- function(team_id, game_date, poss, qtr,
                                      team_to_profile_q_game,
                                      mc_var = MC_VAR,
                                      mc_anchor = MC_ANCHOR) {
  
  poss <- as.integer(poss)
  if (is.na(poss) || poss <= 0) {
    return(list(
      poss_total      = poss,
      turnovers       = 0L,
      live_turnovers  = 0L,
      dead_turnovers  = 0L,
      p_to_used       = 0,
      live_share_used = 0
    ))
  }
  
  probs <- derive_team_tov_probs_q_game(
    team_id   = team_id,
    game_date = game_date,
    qtr       = qtr,
    team_to_profile_q_game = team_to_profile_q_game,
    mc_var    = mc_var,
    mc_anchor = mc_anchor
  )
  
  total_tov <- rbinom(1, size = poss, prob = probs$p_to)
  live_tov  <- if (total_tov > 0) rbinom(1, total_tov, probs$live_share) else 0L
  
  list(
    poss_total      = poss,
    turnovers       = as.integer(total_tov),
    live_turnovers  = as.integer(live_tov),
    dead_turnovers  = as.integer(total_tov - live_tov),
    p_to_used       = probs$p_to,
    live_share_used = probs$live_share
  )
}



# ---------------------------------------------------------------
# 4) OPTIONAL: drop-in patch inside your offense sim
#     (replace your current TO block with this)
# ---------------------------------------------------------------
# Simulates a single team’s quarter-level offensive scoring outcome from a fixed number of possessions.
# This function is the core “points engine” that converts volume (possessions) into points using probabilistic basketball events.
#
# Flow overview:
# 1) Builds all quarter-specific probabilities via derive_team_probs_q(), pulling from turnover, points, and foul profiles,
#    and applying anchor shrinkage and Monte Carlo variance controls.
# 2) Simulates turnovers first using the dedicated turnover engine, reducing available possessions and tracking live/dead TOs.
# 3) Applies offensive fouls, which further consume possessions and are treated as additional turnovers.
# 4) Converts remaining possessions into shot attempts, splits them into 2PA vs 3PA, and simulates makes using stabilized FG%.
# 5) Simulates shooting fouls, and-1s, free-throw attempts, and made free throws using FT%.
#
# The output mirrors a mini box score (points, shots, fouls, turnovers) and also returns the exact probabilities used,
# making the engine debuggable, auditable, and suitable for repeated Monte Carlo iteration at the game or slate level.
simulate_team_offense_pts_q <- function(team_id, poss, qtr,
                                        team_to_profile_q,
                                        team_foul_profile_q,
                                        team_pts_profile_q,
                                        team_shoot_foul_profile_q = NULL,
                                        p_and1_given_made_foul = 1.0) {
  
  probs <- derive_team_probs_q(
    team_id = team_id, qtr = qtr,
    team_to_profile_q = team_to_profile_q,
    team_pts_profile_q = team_pts_profile_q,
    team_shoot_foul_profile_q = team_shoot_foul_profile_q,
    mc_var = MC_VAR,
    mc_anchor = MC_ANCHOR
  )
  
  # ---- TURNOVERS (UPDATED SECTION) ----
  tov_out <- simulate_team_turnovers_q(
    team_id = team_id, poss = poss, qtr = qtr,
    team_to_profile_q = team_to_profile_q,
    mc_var = MC_VAR,
    mc_anchor = MC_ANCHOR
  )
  
  total_tov <- tov_out$turnovers
  live_tov  <- tov_out$live_turnovers
  
  # possessions left for shot attempts
  shot_poss <- max(as.integer(poss) - total_tov, 0L)
  
  # offensive fouls consume shot possessions
  off_fouls <- draw_off_fouls_q(team_id, team_foul_profile_q, shot_poss, qtr)
  off_fouls <- as.integer(off_fouls)
  
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  # (rest of your PTS logic unchanged)
  p2_given_sh <- probs$p2_given_shot
  FG2_pct     <- probs$FG2_pct
  FG3_pct     <- probs$FG3_pct
  FT_pct      <- probs$FT_pct
  p_foul      <- probs$p_shoot_foul
  
  total_shots <- as.integer(shot_poss)
  
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  p_make2 <- if (FGA2 > 0) clamp01(FGM2 / FGA2) else 0
  p_make3 <- if (FGA3 > 0) clamp01(FGM3 / FGA3) else 0
  
  made_foul_2 <- if (foul_2 > 0) rbinom(1, size = foul_2, prob = p_make2) else 0L
  made_foul_3 <- if (foul_3 > 0) rbinom(1, size = foul_3, prob = p_make3) else 0L
  made_foul_2 <- as.integer(made_foul_2); made_foul_3 <- as.integer(made_foul_3)
  
  p_and1_given_made_foul <- clamp01(num0(p_and1_given_made_foul, 1.0))
  and1_2 <- if (made_foul_2 > 0) rbinom(1, size = made_foul_2, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3 > 0) rbinom(1, size = made_foul_3, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  missed_foul_2 <- as.integer(pmax(foul_2 - made_foul_2, 0L))
  missed_foul_3 <- as.integer(pmax(foul_3 - made_foul_3, 0L))
  
  FTA <- as.integer(2L * missed_foul_2 + 3L * missed_foul_3 + 1L * (and1_2 + and1_3))
  FTM <- if (FTA > 0) rbinom(1, size = FTA, prob = FT_pct) else 0L
  FTM <- as.integer(FTM)
  
  pts <- as.integer(2L * FGM2 + 3L * FGM3 + 1L * FTM)
  
  list(
    base_points    = pts,
    poss_total     = as.integer(poss),
    turnovers      = as.integer(total_tov),
    live_turnovers = as.integer(live_tov),
    off_fouls      = as.integer(off_fouls),
    FGA2 = FGA2, FGM2 = FGM2,
    FGA3 = FGA3, FGM3 = FGM3,
    FTA  = FTA,  FTM  = FTM,
    p_to_used      = tov_out$p_to_used,
    live_share_used= tov_out$live_share_used
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END. TOV / LIVE-TOV SECTION
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠


# ️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️
# ======================================
# START: Fouls Projection (QUARTER-BASED, DERIVED) + BY_DATE + BACKTEST
#   - Uses BaseStats_Player_MC only (LEAKAGE-SAFE)
#   - Builds FOULS PER MIN baseline (PF_CGS / MINS_CGS)
#   - Applies variance control -> adj_fpm
#   - Writes fouls_proj_Q* / fouls_sim_Q* + fouls_proj_CGS / fouls_sim_CGS
# ️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️


stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("num0"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control")  # <- from variance controls section
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Projection date + key (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in fouls projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Choose FOULS column (your file might use PF_CGS, FOULS_CGS, or PERSONAL_FOULS_CGS)
# --------------------------------------------------
fouls_col <- if ("PF_CGS" %in% names(BaseStats_Player_MC)) {
  "PF_CGS"
} else if ("FOULS_CGS" %in% names(BaseStats_Player_MC)) {
  "FOULS_CGS"
} else if ("PERSONAL_FOULS_CGS" %in% names(BaseStats_Player_MC)) {
  "PERSONAL_FOULS_CGS"
} else {
  "PF_CGS"  # fallback; stopifnot below will catch if missing
}
stopifnot(fouls_col %in% names(BaseStats_Player_MC))

# --------------------------------------------------
# B) Build FOULS PER MIN baseline (LEAKAGE-SAFE, CGS)
#    Uses BaseStats_Player_MC only (no joins)
# --------------------------------------------------
build_fpm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      FOUL_NUM       = num0(.data[[fouls_col]], NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      fouls_lastN = sum(FOUL_NUM, na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      fpm         = ifelse(mins_lastN > 0, fouls_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# --------------------------------------------------
# Helper: apply fouls (quarter-based) to ONE DF
#   - cutoff_date drives leakage-safe fpm baseline
#   - variance control applied to fpm -> adj_fpm
# --------------------------------------------------
apply_fouls_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  cutoff_date <- as.Date(cutoff_date)
  
  fpm_df <- build_fpm_baseline(cutoff_date, n_games_back)
  fpm_df$ESPN_PLAYER_ID <- as.character(fpm_df$ESPN_PLAYER_ID)
  fpm_df$ESPN_TEAM_ID   <- as.character(fpm_df$ESPN_TEAM_ID)
  fpm_df$key <- paste0(fpm_df$ESPN_PLAYER_ID, "_", fpm_df$ESPN_TEAM_ID)
  
  fpm_map <- setNames(fpm_df$fpm, fpm_df$key)
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  proj_df$fouls_per_min <- unname(fpm_map[proj_df$key])
  
  # ---- league anchor (same leakage-safe baseline window) ----
  league_fpm <- mean(fpm_df$fpm, na.rm = TRUE)
  if (is.nan(league_fpm) || is.na(league_fpm)) league_fpm <- 0.0
  
  # ---- variance-controlled per-player rate ----
  # NOTE: expects MC_VAR fields:
  #   fouls_shrink, fouls_jitter_sd, fouls_fpm_lo, fouls_fpm_hi
  proj_df$adj_fpm <- vapply(
    proj_df$fouls_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_fpm,
        shrink_w  = MC_VAR$fouls_shrink,
        jitter_sd = MC_VAR$fouls_jitter_sd,
        lower     = MC_VAR$fouls_fpm_lo,
        upper     = MC_VAR$fouls_fpm_hi
      )
    },
    numeric(1)
  )
  
  # ---- quarter outputs ----
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    proj_df[[paste0("fouls_proj_", q)]] <-
      ifelse(!is.na(proj_df$adj_fpm), proj_df[[exp_min]] * proj_df$adj_fpm, NA_real_)
    
    proj_df[[paste0("fouls_sim_", q)]]  <-
      ifelse(!is.na(proj_df$adj_fpm), proj_df[[sim_min]] * proj_df$adj_fpm, NA_real_)
  }
  
  # ---- CGS totals ----
  proj_df <- proj_df %>%
    mutate(
      fouls_proj_CGS = rowSums(across(paste0("fouls_proj_", quarters)), na.rm = TRUE),
      fouls_sim_CGS  = rowSums(across(paste0("fouls_sim_",  quarters)), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE: update player_projections_by_date[[k]]
# --------------------------------------------------
player_projections_by_date[[k]] <- apply_fouls_quarter_df(
  proj_df      = player_projections_by_date[[k]],
  cutoff_date  = proj_date,
  n_games_back = 10L
)

# --------------------------------------------------
# 2) BACKTEST: per-date leakage-safe (cutoff = that GAME_DATE)
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  fpm_cache <- new.env(parent = emptyenv())
  
  get_fpm_pack_for_date <- function(dd, n_games_back = 10L) {
    key <- format(as.Date(dd), "%Y%m%d")
    if (exists(key, envir = fpm_cache, inherits = FALSE)) {
      return(get(key, envir = fpm_cache, inherits = FALSE))
    }
    df <- build_fpm_baseline(as.Date(dd), n_games_back = n_games_back)
    df$key <- paste0(df$ESPN_PLAYER_ID, "_", df$ESPN_TEAM_ID)
    m <- setNames(df$fpm, df$key)
    league_fpm <- mean(df$fpm, na.rm = TRUE)
    if (is.nan(league_fpm) || is.na(league_fpm)) league_fpm <- 0.0
    pack <- list(map = m, league = league_fpm)
    assign(key, pack, envir = fpm_cache)
    pack
  }
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    pack <- get_fpm_pack_for_date(dd, n_games_back = 10L)
    fpm_map    <- pack$map
    league_fpm <- pack$league
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$fouls_per_min <- unname(fpm_map[sub$key])
    
    sub$adj_fpm <- vapply(
      sub$fouls_per_min,
      function(r) {
        apply_rate_variance_control(
          rate      = r,
          anchor    = league_fpm,
          shrink_w  = MC_VAR$fouls_shrink,
          jitter_sd = MC_VAR$fouls_jitter_sd,
          lower     = MC_VAR$fouls_fpm_lo,
          upper     = MC_VAR$fouls_fpm_hi
        )
      },
      numeric(1)
    )
    
    for (q in quarters) {
      sub[[paste0("fouls_proj_", q)]] <-
        ifelse(!is.na(sub$adj_fpm),
               sub[[paste0("expected_minutes_adj_", q)]] * sub$adj_fpm,
               NA_real_)
      sub[[paste0("fouls_sim_", q)]]  <-
        ifelse(!is.na(sub$adj_fpm),
               sub[[paste0("minutes_sim_", q)]] * sub$adj_fpm,
               NA_real_)
    }
    
    sub$fouls_proj_CGS <- rowSums(sub[paste0("fouls_proj_", quarters)], na.rm = TRUE)
    sub$fouls_sim_CGS  <- rowSums(sub[paste0("fouls_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out_list[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out_list)
}

message(
  "FOULS (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists("player_projections_backtest"), nrow(player_projections_backtest), 0),
  " | fouls_col=", fouls_col
)

# ️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️

# END: Fouls Projection (QUARTER-BASED) + BY_DATE + BACKTEST
# ======================================
# ️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️



# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# START: TEAM FOUL PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS (GAME/DATE + QTR)
#   Creates:
#     - team_foul_profile_q_weaved (team_id x GAME_DATE x qtr)
#
#   Pulls from:
#     - player_projections_by_date[[k]]  (single date)
#     - player_projections_backtest      (all dates)
#
#   Supports either foul column set in player projections:
#     A) fouls_proj_Q1..Q6 / fouls_sim_Q1..Q6
#     B) pf_proj_Q1..Q6    / pf_sim_Q1..Q6
#
#   Output columns (PTS engine consumption):
#     team_id, GAME_DATE, qtr, PF_mean, PF_sd, PF_sim, PF_per_poss
#
#   Notes:
#   - PF_per_poss requires possessions. If poss columns exist in player DF (team_poss_Qx),
#     we use them. Otherwise PF_per_poss will be NA and you can let the PTS engine default.
#   - PF_sd is “single-draw implied” SD = abs(PF_sim - PF_mean).
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")
numN <- function(x) suppressWarnings(as.numeric(x))

# --------------------------------------------------
# 0) Projection date + key (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in team foul profile section.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# A) Build TEAM FOUL profile from ONE player projection DF
# --------------------------------------------------
build_team_foul_profile_from_player_df <- function(proj_df, game_date_value) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(dplyr::tibble())
  
  stopifnot("ESPN_TEAM_ID" %in% names(proj_df))
  
  # ---- detect foul column set ----
  f_proj_A <- paste0("fouls_proj_", quarters)
  f_sim_A  <- paste0("fouls_sim_",  quarters)
  
  f_proj_B <- paste0("pf_proj_", quarters)
  f_sim_B  <- paste0("pf_sim_",  quarters)
  
  useA <- all(f_proj_A %in% names(proj_df)) && all(f_sim_A %in% names(proj_df))
  useB <- all(f_proj_B %in% names(proj_df)) && all(f_sim_B %in% names(proj_df))
  
  if (!useA && !useB) {
    stop("No foul columns found. Expected fouls_proj_Q* / fouls_sim_Q* or pf_proj_Q* / pf_sim_Q*.")
  }
  
  proj_cols <- if (useA) f_proj_A else f_proj_B
  sim_cols  <- if (useA) f_sim_A  else f_sim_B
  
  # ---- optional team possessions per quarter (if you already stamped them somewhere) ----
  poss_cols <- paste0("team_poss_", quarters)
  has_poss  <- all(poss_cols %in% names(proj_df))
  
  proj_df <- proj_df %>%
    dplyr::mutate(
      team_id   = as.character(ESPN_TEAM_ID),
      GAME_DATE = as.Date(game_date_value)
    ) %>%
    dplyr::filter(!is.na(team_id), team_id != "")
  
  # --- PF proj/sim by quarter (sum players) ---
  pf_long <- proj_df %>%
    dplyr::select(team_id, GAME_DATE,
                  dplyr::all_of(proj_cols),
                  dplyr::all_of(sim_cols)) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = c("kind","qtr"),
      names_pattern = "^(fouls_proj|fouls_sim|pf_proj|pf_sim)_(Q[1-6])$",
      values_to = "val"
    ) %>%
    dplyr::mutate(val = numN(val), qtr = as.character(qtr)) %>%
    dplyr::group_by(team_id, GAME_DATE, qtr, kind) %>%
    dplyr::summarise(val_sum = sum(val, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = kind, values_from = val_sum)
  
  # normalize names to PF_mean/PF_sim regardless of which set we used
  if ("fouls_proj" %in% names(pf_long)) pf_long$PF_mean <- numN(pf_long$fouls_proj)
  if ("pf_proj"    %in% names(pf_long)) pf_long$PF_mean <- numN(pf_long$pf_proj)
  
  if ("fouls_sim" %in% names(pf_long)) pf_long$PF_sim <- numN(pf_long$fouls_sim)
  if ("pf_sim"    %in% names(pf_long)) pf_long$PF_sim <- numN(pf_long$pf_sim)
  
  pf_long <- pf_long %>%
    dplyr::mutate(
      PF_sd = abs(numN(PF_sim) - numN(PF_mean)),
      qtr   = as.integer(sub("Q","", qtr))
    )
  
  # --- PF_per_poss (optional) ---
  if (has_poss) {
    poss_long <- proj_df %>%
      dplyr::select(team_id, GAME_DATE, dplyr::all_of(poss_cols)) %>%
      tidyr::pivot_longer(
        cols = -c(team_id, GAME_DATE),
        names_to = "qtr",
        names_pattern = "^team_poss_(Q[1-6])$",
        values_to = "poss"
      ) %>%
      dplyr::mutate(
        poss = numN(poss),
        qtr  = as.integer(sub("Q","", qtr))
      ) %>%
      dplyr::group_by(team_id, GAME_DATE, qtr) %>%
      dplyr::summarise(poss = max(poss, na.rm = TRUE), .groups = "drop")
  } else {
    poss_long <- pf_long %>%
      dplyr::transmute(team_id, GAME_DATE, qtr, poss = NA_real_)
  }
  
  out <- pf_long %>%
    dplyr::left_join(poss_long, by = c("team_id","GAME_DATE","qtr")) %>%
    dplyr::mutate(
      PF_per_poss = ifelse(!is.na(poss) & poss > 0, PF_mean / poss, NA_real_)
    ) %>%
    dplyr::select(
      team_id, GAME_DATE, qtr,
      PF_mean, PF_sd, PF_sim,
      PF_per_poss
    ) %>%
    dplyr::arrange(GAME_DATE, team_id, qtr)
  
  out
}

# --------------------------------------------------
# 1) SINGLE DATE (today): build from player_projections_by_date[[k]]
# --------------------------------------------------
team_foul_profile_today <- build_team_foul_profile_from_player_df(
  proj_df         = player_projections_by_date[[k]],
  game_date_value = proj_date
)

# --------------------------------------------------
# 2) BACKTEST (leakage-safe by construction): build per GAME_DATE
# --------------------------------------------------
team_foul_profile_backtest <- dplyr::tibble()

if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    dplyr::mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% dplyr::filter(GAME_DATE == dd)
    
    out_list[[i]] <- build_team_foul_profile_from_player_df(
      proj_df         = sub,
      game_date_value = dd
    )
  }
  
  team_foul_profile_backtest <- dplyr::bind_rows(out_list)
}

# --------------------------------------------------
# 3) Final object (one output only)
# --------------------------------------------------
team_foul_profile_q_weaved <- if (mode == "today") {
  team_foul_profile_today
} else {
  team_foul_profile_backtest
}

stopifnot(nrow(team_foul_profile_q_weaved) > 0)

message(
  "TEAM FOUL PROFILE (weaved) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | rows=", nrow(team_foul_profile_q_weaved),
  " | PF_per_poss_available_rows=", sum(!is.na(team_foul_profile_q_weaved$PF_per_poss))
)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: TEAM FOUL PROFILE (NO WINDOWS) — WEAVED FROM PLAYERS
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: 3-Point Makes Projection (PLAYER_ROTATIONS-Based)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: 3PM Projection (QUARTER-BASED, DERIVED) + BY_DATE + BACKTEST
#   - Baseline from BaseStats_Player_MC only (leakage-safe)
#   - Quarter projections use expected_minutes_adj_Qx / minutes_sim_Qx
#   - Variance control applied to fg3m_per_min -> adj_fg3m_pm
# ======================================

stopifnot(
  exists("mode"),
  exists("player_projections_by_date"),
  exists("player_projections_backtest"),
  exists("BaseStats_Player_MC"),
  exists("num0"),
  exists("MC_VAR"),
  exists("apply_rate_variance_control")
)

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# --------------------------------------------------
# 0) Resolve projection date + key (MODE-SENSITIVE)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in 3PM projection.")

k <- format(proj_date, "%Y%m%d")
stopifnot(k %in% names(player_projections_by_date))
stopifnot(nrow(player_projections_by_date[[k]]) > 0)

# --------------------------------------------------
# Helper: pick the correct 3PM source column
# --------------------------------------------------
fg3m_col <- if ("FG3M_CGS" %in% names(BaseStats_Player_MC)) {
  "FG3M_CGS"
} else if ("FG3M" %in% names(BaseStats_Player_MC)) {
  "FG3M"
} else if ("FG3M_CGS_NUM" %in% names(BaseStats_Player_MC)) {
  "FG3M_CGS_NUM"
} else if ("3PTM_CGS" %in% names(BaseStats_Player_MC)) {
  "3PTM_CGS"
} else {
  "FG3M_CGS"
}
stopifnot(fg3m_col %in% names(BaseStats_Player_MC))

# --------------------------------------------------
# A) Build 3PM PER MIN baseline (LEAKAGE-SAFE, CGS)
# --------------------------------------------------
build_fg3pm_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  cutoff_date <- as.Date(cutoff_date)
  
  BaseStats_Player_MC %>%
    mutate(
      GAME_DATE      = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      FG3M_NUM       = num0(.data[[fg3m_col]], NA_real_),
      MINS_NUM       = num0(MINS_CGS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE), GAME_DATE < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, GAME_DATE) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      fg3m_lastN   = sum(FG3M_NUM, na.rm = TRUE),
      mins_lastN   = sum(MINS_NUM, na.rm = TRUE),
      fg3m_per_min = ifelse(mins_lastN > 0, fg3m_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# --------------------------------------------------
# Helper: apply 3PM (quarter-based) to ONE DF
# --------------------------------------------------
apply_fg3m_quarter_df <- function(proj_df, cutoff_date, n_games_back = 10L) {
  
  if (is.null(proj_df) || nrow(proj_df) == 0) return(proj_df)
  
  cutoff_date <- as.Date(cutoff_date)
  
  fg3pm_df <- build_fg3pm_baseline(cutoff_date, n_games_back)
  fg3pm_df$ESPN_PLAYER_ID <- as.character(fg3pm_df$ESPN_PLAYER_ID)
  fg3pm_df$ESPN_TEAM_ID   <- as.character(fg3pm_df$ESPN_TEAM_ID)
  fg3pm_df$key <- paste0(fg3pm_df$ESPN_PLAYER_ID, "_", fg3pm_df$ESPN_TEAM_ID)
  
  fg3pm_map <- setNames(fg3pm_df$fg3m_per_min, fg3pm_df$key)
  
  proj_df$ESPN_PLAYER_ID <- as.character(proj_df$ESPN_PLAYER_ID)
  proj_df$ESPN_TEAM_ID   <- as.character(proj_df$ESPN_TEAM_ID)
  proj_df$key <- paste0(proj_df$ESPN_PLAYER_ID, "_", proj_df$ESPN_TEAM_ID)
  
  proj_df$fg3m_per_min <- unname(fg3pm_map[proj_df$key])
  
  # ---- league anchor from same leakage-safe baseline window ----
  league_fg3m_pm <- mean(fg3pm_df$fg3m_per_min, na.rm = TRUE)
  if (is.nan(league_fg3m_pm) || is.na(league_fg3m_pm)) league_fg3m_pm <- 0.0
  
  # ---- variance-controlled rate ----
  # NOTE: expects MC_VAR fields: fg3m_shrink, fg3m_jitter_sd, fg3m_pm_lo, fg3m_pm_hi
  proj_df$adj_fg3m_pm <- vapply(
    proj_df$fg3m_per_min,
    function(r) {
      apply_rate_variance_control(
        rate      = r,
        anchor    = league_fg3m_pm,
        shrink_w  = MC_VAR$fg3m_shrink,
        jitter_sd = MC_VAR$fg3m_jitter_sd,
        lower     = MC_VAR$fg3m_pm_lo,
        upper     = MC_VAR$fg3m_pm_hi
      )
    },
    numeric(1)
  )
  
  # ---- quarter outputs ----
  for (q in quarters) {
    exp_min <- paste0("expected_minutes_adj_", q)
    sim_min <- paste0("minutes_sim_", q)
    
    proj_df[[paste0("fg3m_proj_", q)]] <-
      ifelse(!is.na(proj_df$adj_fg3m_pm), proj_df[[exp_min]] * proj_df$adj_fg3m_pm, NA_real_)
    
    proj_df[[paste0("fg3m_sim_", q)]]  <-
      ifelse(!is.na(proj_df$adj_fg3m_pm), proj_df[[sim_min]] * proj_df$adj_fg3m_pm, NA_real_)
  }
  
  # ---- CGS totals ----
  proj_df <- proj_df %>%
    mutate(
      fg3m_proj_CGS = rowSums(across(paste0("fg3m_proj_", quarters)), na.rm = TRUE),
      fg3m_sim_CGS  = rowSums(across(paste0("fg3m_sim_",  quarters)), na.rm = TRUE)
    )
  
  proj_df$key <- NULL
  proj_df
}

# --------------------------------------------------
# 1) SINGLE DATE
# --------------------------------------------------
player_projections_by_date[[k]] <- apply_fg3m_quarter_df(
  proj_df      = player_projections_by_date[[k]],
  cutoff_date  = proj_date,
  n_games_back = 10L
)

# --------------------------------------------------
# 2) BACKTEST (STRICTLY < each GAME_DATE)
# --------------------------------------------------
if (exists("player_projections_backtest") && nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(GAME_DATE = as.Date(GAME_DATE))
  
  bt_dates <- sort(unique(player_projections_backtest$GAME_DATE))
  bt_dates <- bt_dates[!is.na(bt_dates)]
  
  fg3pm_cache <- new.env(parent = emptyenv())
  
  get_fg3pm_pack_for_date <- function(dd, n_games_back = 10L) {
    key <- format(as.Date(dd), "%Y%m%d")
    if (exists(key, envir = fg3pm_cache, inherits = FALSE)) {
      return(get(key, envir = fg3pm_cache, inherits = FALSE))
    }
    df <- build_fg3pm_baseline(as.Date(dd), n_games_back = n_games_back)
    df$key <- paste0(df$ESPN_PLAYER_ID, "_", df$ESPN_TEAM_ID)
    m <- setNames(df$fg3m_per_min, df$key)
    league_fg3m_pm <- mean(df$fg3m_per_min, na.rm = TRUE)
    if (is.nan(league_fg3m_pm) || is.na(league_fg3m_pm)) league_fg3m_pm <- 0.0
    pack <- list(map = m, league = league_fg3m_pm)
    assign(key, pack, envir = fg3pm_cache)
    pack
  }
  
  out_list <- vector("list", length(bt_dates))
  for (i in seq_along(bt_dates)) {
    dd  <- bt_dates[i]
    sub <- player_projections_backtest %>% filter(GAME_DATE == dd)
    
    pack <- get_fg3pm_pack_for_date(dd, n_games_back = 10L)
    fg3pm_map     <- pack$map
    league_fg3m_pm <- pack$league
    
    sub$ESPN_PLAYER_ID <- as.character(sub$ESPN_PLAYER_ID)
    sub$ESPN_TEAM_ID   <- as.character(sub$ESPN_TEAM_ID)
    sub$key <- paste0(sub$ESPN_PLAYER_ID, "_", sub$ESPN_TEAM_ID)
    
    sub$fg3m_per_min <- unname(fg3pm_map[sub$key])
    
    sub$adj_fg3m_pm <- vapply(
      sub$fg3m_per_min,
      function(r) {
        apply_rate_variance_control(
          rate      = r,
          anchor    = league_fg3m_pm,
          shrink_w  = MC_VAR$fg3m_shrink,
          jitter_sd = MC_VAR$fg3m_jitter_sd,
          lower     = MC_VAR$fg3m_pm_lo,
          upper     = MC_VAR$fg3m_pm_hi
        )
      },
      numeric(1)
    )
    
    for (q in quarters) {
      sub[[paste0("fg3m_proj_", q)]] <-
        ifelse(!is.na(sub$adj_fg3m_pm),
               sub[[paste0("expected_minutes_adj_", q)]] * sub$adj_fg3m_pm,
               NA_real_)
      sub[[paste0("fg3m_sim_", q)]]  <-
        ifelse(!is.na(sub$adj_fg3m_pm),
               sub[[paste0("minutes_sim_", q)]] * sub$adj_fg3m_pm,
               NA_real_)
    }
    
    sub$fg3m_proj_CGS <- rowSums(sub[paste0("fg3m_proj_", quarters)], na.rm = TRUE)
    sub$fg3m_sim_CGS  <- rowSums(sub[paste0("fg3m_sim_",  quarters)], na.rm = TRUE)
    
    sub$key <- NULL
    out_list[[i]] <- sub
  }
  
  player_projections_backtest <- dplyr::bind_rows(out_list)
}

message(
  "3PM (quarter-based) built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | single_date_rows=", nrow(player_projections_by_date[[k]]),
  " | backtest_rows=", ifelse(exists("player_projections_backtest"), nrow(player_projections_backtest), 0),
  " | fg3m_col=", fg3m_col
)

# ======================================
# END: 3PM Projection (QUARTER-BASED, DERIVED) + BY_DATE + BACKTEST
# ======================================


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: 3-Point Makes Projection (PLAYER_ROTATIONS-Based)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Join Actual Player Stats into Player Backtested Data
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# -----------------------------
# JOIN ACTUALS (CGS only) onto backtest rows
#   - Normalizes FG3M_CGS from BaseStats_Player_MC column `3PTM_CGS`
# -----------------------------
if (exists("BaseStats_Player_MC") && exists("player_projections_backtest") &&
    nrow(player_projections_backtest) > 0) {
  
  player_projections_backtest <- player_projections_backtest %>%
    mutate(
      GAME_DATE      = as.Date(GAME_DATE),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID)
    ) %>%
    left_join(
      BaseStats_Player_MC %>%
        transmute(
          GAME_DATE      = as.Date(GAME_DATE),
          ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
          ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
          
          MINS_CGS = as.numeric(MINS_CGS),
          PTS_CGS  = as.numeric(PTS_CGS),
          REB_CGS  = as.numeric(REB_CGS),
          AST_CGS  = as.numeric(AST_CGS),
          TOV_CGS  = as.numeric(TOV_CGS),
          BLK_CGS  = as.numeric(BLK_CGS),
          STL_CGS  = as.numeric(STL_CGS),
          
          # <-- your fix:
          FG3M_CGS = as.numeric(`3PTM_CGS`)
        ),
      by = c("GAME_DATE","ESPN_PLAYER_ID","ESPN_TEAM_ID")
    )
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Join Actual Player Stats into Player Backtested Data
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#
#     ooooooooo.                                                            o8o                                 
#     `888   `Y88.                                                          `"'                                 
#      888   .d88'  .ooooo.   .oooo.o  .oooo.o  .ooooo.   .oooo.o  .oooo.o oooo   .ooooo.  ooo. .oo.    .oooo.o 
#      888ooo88P'  d88' `88b d88(  "8 d88(  "8 d88' `88b d88(  "8 d88(  "8 `888  d88' `88b `888P"Y88b  d88(  "8 
#      888         888   888 `"Y88b.  `"Y88b.  888ooo888 `"Y88b.  `"Y88b.   888  888   888  888   888  `"Y88b.  
#      888         888   888 o.  )88b o.  )88b 888    .o o.  )88b o.  )88b  888  888   888  888   888  o.  )88b 
#     o888o        `Y8bod8P' 8""888P' 8""888P' `Y8bod8P' 8""888P' 8""888P' o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                          
#                                                                                                          
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                          


# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# PER-GAME: Build team_poss_profile_q for a slate (NO window profiles)
#   Output columns (required by poss engine, per game):
#     nba_game_id, game_date, team_id, season_token, qtr,
#     POSS_mean, POSS_sd
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

# ---- helpers (keep) ----
get_num_col <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

clamp_num <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x))
  x <- pmin(pmax(x, lo), hi)
  x
}

parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

# ============================================================
# build_team_poss_profile_q_games()
#   - BaseStats_Team_MC provides historical per-game rows
#   - slate_games provides the games we are predicting
#   - For each game/team/qtr, compute mu/sd from PRIOR games only
#   - No window_n column. n_games_back is internal only.
# ============================================================
build_team_poss_profile_q_games <- function(BaseStats_Team_MC,
                                            slate_games,
                                            season_token,
                                            qtrs = 1:6,
                                            n_games_back = 10L,
                                            sd_shrink = 1.0) {
  # Required inputs
  stopifnot("GAME_DATE" %in% names(BaseStats_Team_MC))
  stopifnot("ESPN_TEAM_ID" %in% names(BaseStats_Team_MC))
  
  # slate_games must include: nba_game_id, game_date, team_id
  req_slate <- c("nba_game_id", "game_date", "team_id")
  stopifnot(all(req_slate %in% names(slate_games)))
  
  # --- normalize BaseStats history ---
  hist <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      season_token = as.character(season_token),
      game_date_dt = parse_game_date(GAME_DATE)
    ) %>%
    filter(!is.na(team_id), team_id != "", !is.na(game_date_dt)) %>%
    select(team_id, season_token, game_date_dt, everything())
  
  # --- normalize slate games ---
  slate <- as.data.frame(slate_games) %>%
    mutate(
      nba_game_id  = as.character(nba_game_id),
      team_id      = as.character(team_id),
      game_date_dt = parse_game_date(game_date),
      season_token = as.character(season_token)
    ) %>%
    filter(!is.na(nba_game_id), nba_game_id != "",
           !is.na(team_id), team_id != "",
           !is.na(game_date_dt))
  
  # --- build long hist by quarter (poss_val) ---
  long_list <- lapply(qtrs, function(q) {
    poss_col <- paste0("T_POSS_Q", q)
    pace_col <- paste0("T_PACE_Q", q)
    
    poss_raw <- get_num_col(hist, poss_col)
    pace_raw <- get_num_col(hist, pace_col)
    
    use_poss <- poss_raw
    if (all(is.na(use_poss))) use_poss <- pace_raw
    
    data.frame(
      team_id      = hist$team_id,
      season_token = hist$season_token,
      game_date_dt = hist$game_date_dt,
      qtr          = as.integer(q),
      poss_val     = suppressWarnings(as.numeric(use_poss)),
      stringsAsFactors = FALSE
    )
  })
  
  hist_long <- dplyr::bind_rows(long_list) %>%
    filter(!is.na(team_id), team_id != "")
  
  hist_dt <- as.data.table(hist_long)
  setorder(hist_dt, team_id, qtr, game_date_dt)
  
  # --- expand slate to game/team/qtr grid ---
  grid <- as.data.table(slate)[, .(nba_game_id, game_date_dt, team_id, season_token)]
  grid <- unique(grid)
  grid <- grid[rep(seq_len(nrow(grid)), each = length(qtrs))]
  grid[, qtr := rep(as.integer(qtrs), times = nrow(unique(as.data.table(slate)[, .(nba_game_id, team_id, game_date_dt)])))]
  
  # --- compute profile per (nba_game_id, team_id, qtr) using PRIOR games only ---
  out <- grid[, {
    gdt <- game_date_dt[1]
    tid <- team_id[1]
    qq  <- qtr[1]
    
    h <- hist_dt[team_id == tid & qtr == qq & game_date_dt < gdt]
    v <- h$poss_val
    v <- v[!is.na(v) & v > 0]
    
    if (length(v) == 0) {
      mu <- NA_real_
      sd <- NA_real_
    } else {
      vN <- tail(v, min(as.integer(n_games_back), length(v)))
      mu <- mean(vN)
      sd <- if (length(vN) >= 2) stats::sd(vN) else 0
    }
    
    # defaults
    default_mu <- if (qq >= 5) 10 else 24
    default_sd <- if (qq >= 5) 2  else 3
    
    mu <- ifelse(is.na(mu) | mu <= 0, default_mu, mu)
    sd <- ifelse(is.na(sd) | sd < 0,  default_sd, sd)
    
    sd <- sd * sd_shrink
    
    # clamps
    mu <- clamp_num(mu, lo = if (qq >= 5) 4 else 12, hi = if (qq >= 5) 20 else 40)
    sd <- clamp_num(sd, lo = 0, hi = if (qq >= 5) 8 else 12)
    
    list(
      POSS_mean = as.numeric(mu),
      POSS_sd   = as.numeric(sd)
    )
  }, by = .(nba_game_id, game_date_dt, team_id, season_token, qtr)]
  
  out <- as_tibble(out) %>%
    mutate(
      nba_game_id  = as.character(nba_game_id),
      game_date    = as.character(game_date_dt),
      team_id      = as.character(team_id),
      season_token = as.character(season_token),
      qtr          = as.integer(qtr),
      POSS_mean    = as.numeric(POSS_mean),
      POSS_sd      = as.numeric(POSS_sd)
    ) %>%
    select(nba_game_id, game_date, team_id, season_token, qtr, POSS_mean, POSS_sd) %>%
    arrange(game_date, nba_game_id, team_id, qtr)
  
  stopifnot(all(c("nba_game_id","game_date","team_id","season_token","qtr","POSS_mean","POSS_sd") %in% names(out)))
  out
}


# ============================
# Build slate_games (PER TEAM, PER GAME)
# ============================
slate_games <- nba_schedule_season %>%
  transmute(
    nba_game_id = as.character(game_id),
    game_date   = as.character(game_date),
    team_id     = as.character(team_id)
  ) %>%
  distinct()

stopifnot(nrow(slate_games) %% 2 == 0)
stopifnot(all(c("nba_game_id","game_date","team_id") %in% names(slate_games)))

print(slate_games)

# ============================
# Build per-game possession profiles
# ============================
team_poss_profile_q <- build_team_poss_profile_q_games(
  BaseStats_Team_MC = BaseStats_Team_MC,
  slate_games       = slate_games,
  season_token      = season_token,
  qtrs              = 1:6,
  n_games_back      = 10L,
  sd_shrink         = 1.0
)

print(team_poss_profile_q %>% slice(1:20))


# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: Possession team_poss_profile_q  profile build 
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 2. START: MC predictions driven by POSSESSIONS as an INPUT DISTRIBUTION
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠


# Draw a single shared game-level possession count for a given quarter by combining both teams’ quarter-specific possession mean/SD, scaling variance via poss_sd_mult, 
# and clamping the result to a realistic min/max range.
draw_game_possessions_q <- function(home_team_id, away_team_id,
                                    team_poss_profile_q,
                                    qtr,
                                    mc_var = MC_VAR,
                                    min_poss = 15L,
                                    max_poss = 35L) {
  
  h <- team_poss_profile_q %>% dplyr::filter(team_id == home_team_id, qtr == !!qtr) %>% dplyr::slice(1)
  a <- team_poss_profile_q %>% dplyr::filter(team_id == away_team_id, qtr == !!qtr) %>% dplyr::slice(1)
  
  h_mu <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_mean)) else NA_real_
  a_mu <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_mean)) else NA_real_
  
  h_sd <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_sd)) else 0
  a_sd <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_sd)) else 0
  
  # fallback: OT should be lower than regulation
  if (is.na(h_mu) || h_mu <= 0) h_mu <- if (qtr >= 5) 10 else 24
  if (is.na(a_mu) || a_mu <= 0) a_mu <- if (qtr >= 5) 10 else 24
  if (is.na(h_sd) || h_sd < 0)  h_sd <- 0
  if (is.na(a_sd) || a_sd < 0)  a_sd <- 0
  
  game_mu <- (h_mu + a_mu) / 2
  game_sd <- (sqrt(h_sd^2 + a_sd^2) / 2) * num0(mc_var$poss_sd_mult, 0.75)
  
  val <- round(rnorm(1, mean = game_mu, sd = game_sd))
  val <- as.integer(pmin(pmax(val, min_poss), max_poss))
  val
}

# Split a shared game-level quarter possession total into home vs away possessions using each team’s quarter-specific expected possession mean as the allocation weight.
split_game_possessions_q <- function(game_poss, home_team_id, away_team_id,
                                     team_poss_profile_q, qtr) {
  
  h <- team_poss_profile_q %>% dplyr::filter(team_id == home_team_id, qtr == !!qtr) %>% dplyr::slice(1)
  a <- team_poss_profile_q %>% dplyr::filter(team_id == away_team_id, qtr == !!qtr) %>% dplyr::slice(1)
  
  h_mu <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_mean)) else if (qtr >= 5) 10 else 24
  a_mu <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_mean)) else if (qtr >= 5) 10 else 24
  if (is.na(h_mu) || h_mu <= 0) h_mu <- if (qtr >= 5) 10 else 24
  if (is.na(a_mu) || a_mu <= 0) a_mu <- if (qtr >= 5) 10 else 24
  
  total_mu <- h_mu + a_mu
  home_share <- if (total_mu > 0) h_mu / total_mu else 0.5
  
  home_poss <- as.integer(round(game_poss * home_share))
  away_poss <- as.integer(game_poss - home_poss)
  
  home_poss <- as.integer(pmax(home_poss, 0L))
  away_poss <- as.integer(pmax(away_poss, 0L))
  
  list(home = home_poss, away = away_poss)
}

# ------------------------------------------------------------
# simulate_game_once_poss_only()
# ------------------------------------------------------------
# PURPOSE
# - Simulate ONE complete game outcome (score + margin) by:
#   (1) drawing shared quarter-level possessions (pace),
#   (2) splitting those possessions between teams, and
#   (3) converting possessions into points using team offense profiles.
#
# KEY MODELING CHOICE
# - Possessions are treated as the primary input distribution (pace first),
#   and points are generated conditional on those possessions (efficiency second).
# - This creates more realistic score distributions than drawing points directly,
#   because it enforces a natural "volume x efficiency" structure.
#
# INPUTS (conceptual)
# - team_mc_profile: a sanity check table ensuring both teams have MC context rows
# - team_poss_profile_q: quarter-level possession mean/SD by team (drives pace)
# - team_to_profile_q: quarter-level turnover tendencies (used inside points sim)
# - team_foul_profile_q: quarter-level foul tendencies (used inside points sim)
# - team_pts_profile_q: quarter-level scoring/PPP tendencies (used inside points sim)
# - mc_var: variance knobs (here, poss_sd_mult affects pace volatility)
# - max_ot: cap on number of overtime periods to prevent infinite loops
#
# OUTPUT
# - A single-row tibble representing one simulated universe:
#   final points, margin, quarter possessions, and overtime count.
simulate_game_once_poss_only <- function(home_team_id, away_team_id,
                                         season_token, team_mc_profile,
                                         team_poss_profile_q,
                                         team_to_profile_q,
                                         team_foul_profile_q,
                                         team_pts_profile_q,
                                         max_ot = 2L,
                                         mc_var = MC_VAR) {
  
  # ------------------------------------------------------------
  # 1) Validate both teams have a usable MC profile row for this season
  # ------------------------------------------------------------
  # What:
  # - Pull one row per team from team_mc_profile.
  # Why:
  # - Ensures the team exists in the MC universe for the given season and avoids
  #   downstream simulations running with missing or mismatched team context.
  home_row <- team_mc_profile %>%
    dplyr::filter(team_id == home_team_id, season_token == !!season_token) %>%
    dplyr::slice(1)
  
  away_row <- team_mc_profile %>%
    dplyr::filter(team_id == away_team_id, season_token == !!season_token) %>%
    dplyr::slice(1)
  
  # Hard stop if either team is missing from the MC profile table
  if (nrow(home_row) == 0 || nrow(away_row) == 0) stop("Missing MC profile for one or both teams.")
  
  # ------------------------------------------------------------
  # 2) Pre-allocate quarter possession vectors (Q1–Q4)
  # ------------------------------------------------------------
  # Why:
  # - Keeps quarter-by-quarter pace explicitly tracked (useful for debugging,
  #   analysis, and later enhancements like clutch/late-game variance tweaks).
  home_q_poss <- integer(4)
  away_q_poss <- integer(4)
  
  # ------------------------------------------------------------
  # 3) REGULATION PACE: draw shared quarter possessions and split to each team
  # ------------------------------------------------------------
  # What:
  # - For each quarter:
  #   a) draw one shared game-level possession count (pace environment),
  #   b) allocate those possessions between home/away based on expected share.
  # Why:
  # - "Shared pace" prevents contradictions (home fast, away slow in same quarter)
  #   and keeps possession totals conserved between teams.
  for (q in 1:4) {
    # Draw total possessions for this quarter (shared pace)
    game_poss <- draw_game_possessions_q(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q,
      mc_var = mc_var
    )
    
    # Split the shared total into home vs away possessions (share-based)
    split <- split_game_possessions_q(
      game_poss = game_poss,
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q
    )
    
    # Store quarter possessions for downstream scoring simulation
    home_q_poss[q] <- split$home
    away_q_poss[q] <- split$away
  }
  
  # ------------------------------------------------------------
  # 4) REGULATION SCORING: convert possessions -> points (Q1–Q4)
  # ------------------------------------------------------------
  # What:
  # - For each quarter, simulate points for each team given its possessions.
  # How:
  # - Calls simulate_team_offense_pts_q() which uses:
  #   * turnover profile (lost possessions),
  #   * foul profile (free-throw effects / bonus dynamics),
  #   * points profile (efficiency / PPP behavior).
  # Why:
  # - Separates "how many chances" (possessions) from "how good are they" (efficiency),
  #   enabling clean tuning and more realistic score distributions.
  home_pts <- 0L
  away_pts <- 0L
  
  for (q in 1:4) {
    # Home offense points for quarter q
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss = as.integer(home_q_poss[q]),
      qtr = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    # Away offense points for quarter q
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss = as.integer(away_q_poss[q]),
      qtr = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    # Accumulate base points into regulation totals
    home_pts <- home_pts + as.integer(hb$base_points)
    away_pts <- away_pts + as.integer(ab$base_points)
  }
  
  # ------------------------------------------------------------
  # 5) OVERTIME LOGIC: play OT periods until not tied or max_ot reached
  # ------------------------------------------------------------
  # What:
  # - If regulation ends tied, simulate overtime(s) as additional quarters.
  # Why:
  # - Allows ties to resolve organically rather than forcing a winner.
  # - max_ot prevents infinite loops in rare repeated-tie scenarios.
  # How:
  # - OT quarters are indexed as Q5, Q6, ... (q_ot = 4 + ot_played).
  # - OT possessions are drawn with tighter guardrails (min/max lower than regulation),
  #   reflecting fewer possessions in a 5-minute OT period.
  ot_played <- 0L
  while (home_pts == away_pts && ot_played < max_ot) {
    ot_played <- ot_played + 1L
    q_ot <- 4L + ot_played
    
    # Draw OT possessions with OT-specific bounds
    game_poss_ot <- draw_game_possessions_q(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q_ot,
      mc_var = mc_var,
      min_poss = 6L,
      max_poss = 20L
    )
    
    # Split OT possessions to teams
    split_ot <- split_game_possessions_q(
      game_poss = game_poss_ot,
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q_ot
    )
    
    # Simulate OT scoring for home
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss = as.integer(split_ot$home),
      qtr = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    # Simulate OT scoring for away
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss = as.integer(split_ot$away),
      qtr = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    # Add OT points into totals
    home_pts <- home_pts + as.integer(hb$base_points)
    away_pts <- away_pts + as.integer(ab$base_points)
  }
  
  # ------------------------------------------------------------
  # 6) Return a single simulated game outcome
  # ------------------------------------------------------------
  # Includes:
  # - final points + margin
  # - quarter possession splits for transparency/debugging
  # - overtime count
  tibble::tibble(
    home_team_id = home_team_id,
    away_team_id = away_team_id,
    season       = season_token,
    home_points  = as.integer(home_pts),
    away_points  = as.integer(away_pts),
    margin       = as.integer(home_pts - away_pts),
    home_poss_q1 = as.integer(home_q_poss[1]),
    home_poss_q2 = as.integer(home_q_poss[2]),
    home_poss_q3 = as.integer(home_q_poss[3]),
    home_poss_q4 = as.integer(home_q_poss[4]),
    away_poss_q1 = as.integer(away_q_poss[1]),
    away_poss_q2 = as.integer(away_q_poss[2]),
    away_poss_q3 = as.integer(away_q_poss[3]),
    away_poss_q4 = as.integer(away_q_poss[4]),
    ot_played    = as.integer(ot_played)
  )
}


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 2. END: Possessions-driven MC (shared pace)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooo.              o8o                  .                 oooooooooooo                         o8o                             8            .o.       oooo             oooo                  
#     `888   `Y88.            `"'                .o8                 `888'     `8                         `"'                             8           .888.      `888             `888                  
#      888   .d88'  .ooooo.  oooo  ooo. .oo.   .o888oo  .oooo.o       888         ooo. .oo.    .oooooooo oooo  ooo. .oo.    .ooooo.       8          .8"888.      888  oo.ooooo.   888 .oo.    .oooo.   
#      888ooo88P'  d88' `88b `888  `888P"Y88b    888   d88(  "8       888oooo8    `888P"Y88b  888' `88b  `888  `888P"Y88b  d88' `88b                .8' `888.     888   888' `88b  888P"Y88b  `P  )88b  
#      888         888   888  888   888   888    888   `"Y88b.        888    "     888   888  888   888   888   888   888  888ooo888      8        .88ooo8888.    888   888   888  888   888   .oP"888  
#      888         888   888  888   888   888    888 . o.  )88b       888       o  888   888  `88bod8P'   888   888   888  888    .o      8       .8'     `888.   888   888   888  888   888  d8(  888  
#     o888o        `Y8bod8P' o888o o888o o888o   "888" 8""888P'      o888ooooood8 o888o o888o `8oooooo.  o888o o888o o888o `Y8bod8P'      8      o88o     o8888o o888o  888bod8P' o888o o888o `Y888""8o 
#                                                                                             d"     YD                                                                 888                             
#                                                                                             "Y88888P'                                                                o888o                                                                                                                                                                                                                                                                                                                           "Y88888P'
#                                                                      
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠





# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# START: TEAM PTS PROFILE (PER GAME × QUARTER) + WEAVE PLAYER ANCHORS
#   Output:
#     team_pts_profile_q_game   (team_id × GAME_DATE × qtr)  [rates]
#     team_pts_profile_q_weaved (team_id × GAME_DATE × qtr)  [rates + anchors]
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

stopifnot(exists("BaseStats_Team_MC"))
stopifnot(exists("mode"))
stopifnot(exists("season_token"))

# ---- helpers ----
get_num_col <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

clamp01 <- function(x) pmin(pmax(x, 0), 1)

# ✅ UPDATE: add ESPN_GAME_ID into the TEAM PTS PROFILE outputs
#   - Because BaseStats_Team_MC has ESPN_GAME_ID, we just carry it through
#   - Result: team_pts_profile_q_game and team_pts_profile_q_weaved now include ESPN_GAME_ID

# ============================================================
# 1) Build per-game quarter-level TEAM rate profile (NO WINDOWS)
#    One row per team_id × ESPN_GAME_ID × GAME_DATE × qtr
# ============================================================
build_team_pts_profile_q_per_game <- function(BaseStats_Team_MC,
                                              season_token,
                                              qtrs = 1:6) {
  
  stopifnot("ESPN_GAME_ID" %in% names(BaseStats_Team_MC))
  
  df <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      ESPN_GAME_ID = as.character(ESPN_GAME_ID),
      season_token = as.character(season_token),
      GAME_DATE    = parse_game_date(GAME_DATE)
    ) %>%
    filter(
      !is.na(team_id), team_id != "",
      !is.na(ESPN_GAME_ID), ESPN_GAME_ID != "",
      !is.na(GAME_DATE)
    )
  
  # Build long table: one row per team-game-quarter
  long_list <- lapply(qtrs, function(q) {
    
    fg2 <- get_num_col(df, paste0("T_2PT_PCT_Q", q))
    fg3 <- get_num_col(df, paste0("T_3PT_PCT_Q", q))
    ft  <- get_num_col(df, paste0("T_FT_PCT_Q",  q))
    ftr <- get_num_col(df, paste0("T_FTR_Q", q))
    
    # 3PA rate for shot mix
    three_par <- NA_real_
    if (paste0("T_3PAR_Q", q) %in% names(df)) {
      three_par <- get_num_col(df, paste0("T_3PAR_Q", q))
    } else if (all(c(paste0("T_3PA_Q", q), paste0("T_FGA_Q", q)) %in% names(df))) {
      three_pa <- get_num_col(df, paste0("T_3PA_Q", q))
      fga      <- get_num_col(df, paste0("T_FGA_Q", q))
      three_par <- ifelse(!is.na(fga) & fga > 0, three_pa / fga, NA_real_)
    }
    
    tibble::tibble(
      team_id      = df$team_id,
      ESPN_GAME_ID = df$ESPN_GAME_ID,
      season_token = df$season_token,
      GAME_DATE    = df$GAME_DATE,
      qtr          = as.integer(q),
      
      # raw
      FG2_pct_raw = fg2,
      FG3_pct_raw = fg3,
      FT_pct_raw  = ft,
      p3_raw      = three_par,
      FTR_raw     = ftr
    )
  })
  
  long_df <- dplyr::bind_rows(long_list) %>%
    mutate(
      # guardrails + defaults (league-ish)
      FG2_pct = clamp01(ifelse(is.na(FG2_pct_raw), 0.50, FG2_pct_raw)),
      FG3_pct = clamp01(ifelse(is.na(FG3_pct_raw), 0.36, FG3_pct_raw)),
      FT_pct  = clamp01(ifelse(is.na(FT_pct_raw),  0.78, FT_pct_raw)),
      
      p2_given_shot = clamp01(ifelse(is.na(p3_raw), 0.65, 1 - p3_raw)),
      
      FTA_per_FGA = ifelse(is.na(FTR_raw), 0.22, FTR_raw),
      FTA_per_FGA = pmax(0, FTA_per_FGA)
    ) %>%
    select(
      team_id, ESPN_GAME_ID, season_token, GAME_DATE, qtr,
      p2_given_shot, FG2_pct, FG3_pct, FT_pct, FTA_per_FGA
    ) %>%
    arrange(GAME_DATE, ESPN_GAME_ID, team_id, qtr)
  
  stopifnot(nrow(long_df) > 0)
  long_df
}

team_pts_profile_q_game <- build_team_pts_profile_q_per_game(
  BaseStats_Team_MC = BaseStats_Team_MC,
  season_token      = season_token,
  qtrs              = 1:6
)

print(team_pts_profile_q_game %>% dplyr::slice(1:20))

# ============================================================
# 2) Build PLAYER → TEAM anchors per game-quarter
#    - backtest: use player_projections_backtest (has GAME_DATE)
#    - today: use player_projections_by_date[[k]] (stamp proj_date)
# ============================================================
sum_if_exists <- function(df, col) {
  if (!col %in% names(df)) return(NA_real_)
  suppressWarnings(sum(as.numeric(df[[col]]), na.rm = TRUE))
}

build_team_player_anchors_q_game <- function(pp_df, game_date_value = NULL) {
  
  stopifnot(nrow(pp_df) > 0)
  stopifnot(all(c("ESPN_TEAM_ID") %in% names(pp_df)))
  
  out <- pp_df %>%
    mutate(
      team_id   = as.character(ESPN_TEAM_ID),
      GAME_DATE = if (!is.null(game_date_value)) as.Date(game_date_value) else as.Date(GAME_DATE)
    ) %>%
    group_by(team_id, GAME_DATE) %>%
    summarise(
      PTS_Q1  = sum_if_exists(cur_data(), "points_proj_Q1"),
      PTS_Q2  = sum_if_exists(cur_data(), "points_proj_Q2"),
      PTS_Q3  = sum_if_exists(cur_data(), "points_proj_Q3"),
      PTS_Q4  = sum_if_exists(cur_data(), "points_proj_Q4"),
      PTS_Q5  = sum_if_exists(cur_data(), "points_proj_Q5"),
      PTS_Q6  = sum_if_exists(cur_data(), "points_proj_Q6"),
      
      REB_Q1  = sum_if_exists(cur_data(), "rebounds_proj_Q1"),
      REB_Q2  = sum_if_exists(cur_data(), "rebounds_proj_Q2"),
      REB_Q3  = sum_if_exists(cur_data(), "rebounds_proj_Q3"),
      REB_Q4  = sum_if_exists(cur_data(), "rebounds_proj_Q4"),
      REB_Q5  = sum_if_exists(cur_data(), "rebounds_proj_Q5"),
      REB_Q6  = sum_if_exists(cur_data(), "rebounds_proj_Q6"),
      
      TOV_Q1  = sum_if_exists(cur_data(), "turnovers_proj_Q1"),
      TOV_Q2  = sum_if_exists(cur_data(), "turnovers_proj_Q2"),
      TOV_Q3  = sum_if_exists(cur_data(), "turnovers_proj_Q3"),
      TOV_Q4  = sum_if_exists(cur_data(), "turnovers_proj_Q4"),
      TOV_Q5  = sum_if_exists(cur_data(), "turnovers_proj_Q5"),
      TOV_Q6  = sum_if_exists(cur_data(), "turnovers_proj_Q6"),
      
      FOUL_Q1 = sum_if_exists(cur_data(), "fouls_proj_Q1"),
      FOUL_Q2 = sum_if_exists(cur_data(), "fouls_proj_Q2"),
      FOUL_Q3 = sum_if_exists(cur_data(), "fouls_proj_Q3"),
      FOUL_Q4 = sum_if_exists(cur_data(), "fouls_proj_Q4"),
      FOUL_Q5 = sum_if_exists(cur_data(), "fouls_proj_Q5"),
      FOUL_Q6 = sum_if_exists(cur_data(), "fouls_proj_Q6"),
      
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = -c(team_id, GAME_DATE),
      names_to = c("stat","qtr"),
      names_pattern = "^(PTS|REB|TOV|FOUL)_Q([1-6])$",
      values_to = "anchor"
    ) %>%
    mutate(qtr = as.integer(qtr)) %>%
    tidyr::pivot_wider(
      names_from  = stat,
      values_from = anchor
    ) %>%
    rename(
      PTS_anchor  = PTS,
      REB_anchor  = REB,
      TOV_anchor  = TOV,
      FOUL_anchor = FOUL
    ) %>%
    arrange(GAME_DATE, team_id, qtr)
  
  out
}

# Resolve proj_date + k if you need "today" anchors
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode")

k <- format(proj_date, "%Y%m%d")

team_player_anchors_q_game <- if (mode == "backtest") {
  stopifnot(exists("player_projections_backtest"))
  stopifnot(all(c("GAME_DATE","ESPN_TEAM_ID") %in% names(player_projections_backtest)))
  build_team_player_anchors_q_game(player_projections_backtest, game_date_value = NULL)
} else {
  stopifnot(exists("player_projections_by_date"))
  stopifnot(k %in% names(player_projections_by_date))
  build_team_player_anchors_q_game(player_projections_by_date[[k]], game_date_value = proj_date)
}

print(team_player_anchors_q_game %>% dplyr::slice(1:20))

# ============================================================
# 3) WEAVE: rates + anchors per team-game-quarter
#    Add ESPN_GAME_ID into the join by mapping from the rates table.
#    (anchors don’t naturally have ESPN_GAME_ID — this stamps it on safely)
# ============================================================
team_pts_profile_q_weaved <- team_pts_profile_q_game %>%
  left_join(
    team_player_anchors_q_game,
    by = c("team_id","GAME_DATE","qtr")
  ) %>%
  arrange(GAME_DATE, ESPN_GAME_ID, team_id, qtr)

print(team_pts_profile_q_weaved %>% dplyr::slice(1:20))

stopifnot(all(c(
  "team_id","ESPN_GAME_ID","season_token","GAME_DATE","qtr",
  "p2_given_shot","FG2_pct","FG3_pct","FT_pct","FTA_per_FGA"
) %in% names(team_pts_profile_q_weaved)))
stopifnot(all(c(
  "team_id","season_token","GAME_DATE","qtr",
  "p2_given_shot","FG2_pct","FG3_pct","FT_pct","FTA_per_FGA"
) %in% names(team_pts_profile_q_weaved)))

message(
  "TEAM per-game profile built | rows=",
  nrow(team_pts_profile_q_weaved),
  " | unique games=",
  length(unique(paste(team_pts_profile_q_weaved$GAME_DATE, team_pts_profile_q_weaved$team_id)))
)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: TEAM PTS PROFILE (PER GAME × QUARTER) + WEAVE PLAYER ANCHORS
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ===============================================================
# PTS ENGINE (NO WINDOWS) — GAME/DATE-AWARE
#   Uses: team_pts_profile_q_weaved (derived from PLAYER projections compression)
#
#   Required in team_pts_profile_q_weaved (one row per team_id x GAME_DATE x qtr):
#     team_id, GAME_DATE, qtr,
#     p2_given_shot, FG2_pct, FG3_pct, FT_pct, FTA_per_FGA,
#     (optional) p_shoot_foul  OR  P_SHOOT_FOUL
#
#   Turnovers + offensive fouls expected from team-level profiles you built from players:
#     team_to_profile_q_weaved:   team_id, GAME_DATE, qtr, p_to, p_live_to (optional)
#     team_foul_profile_q_weaved: team_id, GAME_DATE, qtr, p_off_foul (or off_fouls_per_poss)
#
#   Adds: shooting shrink + pct jitter (from MC_VAR)
# ===============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

stopifnot(exists("MC_VAR"))
stopifnot(exists("team_pts_profile_q_weaved"))
stopifnot(nrow(team_pts_profile_q_weaved) > 0)

# ----------------------------
# helpers
# ----------------------------
clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0) return(default)
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  clamp01(x)
}

num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

shrink_to <- function(p, anchor, w) {
  p <- suppressWarnings(as.numeric(p))
  if (length(p) == 0) p <- NA_real_
  if (is.na(p)) p <- anchor
  w <- clamp01(num0(w, 0))
  clamp01((1 - w) * p + w * anchor)
}

# league anchors
if (!exists("MC_ANCHOR")) {
  MC_ANCHOR <- list(
    fg2 = 0.52,
    fg3 = 0.36,
    ft  = 0.78,
    to  = 0.13,
    shoot_foul = 0.10,
    off_foul   = 0.03,  # offensive foul per poss proxy
    live_share = 0.60
  )
}

if (!exists("apply_pct_jitter")) {
  apply_pct_jitter <- function(p, jitter_sd) {
    p <- suppressWarnings(as.numeric(p))
    if (length(p) == 0 || is.na(p)) return(NA_real_)
    if (is.na(jitter_sd) || jitter_sd <= 0) return(clamp01(p))
    clamp01(rnorm(1, mean = p, sd = jitter_sd))
  }
}

# robust date parser for profiles
parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

# ----------------------------
# profile pickers (NO windows)
# ----------------------------
pick_profile_row_game_q <- function(df, team_id, game_date, qtr) {
  if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
  stopifnot(all(c("team_id","qtr") %in% names(df)))
  if (!("GAME_DATE" %in% names(df))) stop("Profile is missing GAME_DATE: ", deparse(substitute(df)))
  
  dd <- as.Date(game_date)
  tmp <- df %>%
    dplyr::mutate(
      team_id  = as.character(team_id),
      qtr      = as.integer(qtr),
      GAME_DATE = parse_game_date(GAME_DATE)
    ) %>%
    dplyr::filter(.data$team_id == as.character(team_id),
                  .data$qtr == as.integer(qtr),
                  .data$GAME_DATE == dd) %>%
    dplyr::slice(1)
  
  tmp
}

# ----------------------------
# pull shooting + FT inputs from TEAM PTS PROFILE (WEAVED)
# ----------------------------
get_team_pts_inputs_game_q <- function(team_id, game_date, qtr, team_pts_profile_q_weaved) {
  
  r <- pick_profile_row_game_q(team_pts_profile_q_weaved, team_id, game_date, qtr)
  
  if (nrow(r) == 0) {
    return(list(
      p2_given_shot = 0.65,
      FG2_pct = 0.50,
      FG3_pct = 0.36,
      FT_pct  = 0.78,
      FTA_per_FGA = 0.22,
      p_shoot_foul = NA_real_
    ))
  }
  
  # tolerate a couple naming conventions for shooting-foul prob
  p_shoot <- NA_real_
  if ("p_shoot_foul" %in% names(r))   p_shoot <- pct01(r$p_shoot_foul[1], default = NA_real_)
  if ("P_SHOOT_FOUL" %in% names(r))   p_shoot <- pct01(r$P_SHOOT_FOUL[1], default = p_shoot)
  
  list(
    p2_given_shot = clamp01(num0(r$p2_given_shot[1], 0.65)),
    FG2_pct       = clamp01(num0(r$FG2_pct[1], 0.50)),
    FG3_pct       = clamp01(num0(r$FG3_pct[1], 0.36)),
    FT_pct        = clamp01(num0(r$FT_pct[1], 0.78)),
    FTA_per_FGA   = pmax(0, suppressWarnings(as.numeric(r$FTA_per_FGA[1]))),
    p_shoot_foul  = p_shoot
  )
}

# ----------------------------
# pull turnover inputs from TEAM TO PROFILE (WEAVED)
# expected columns:
#   p_to (turnover per poss) OR TOV_pct
#   p_live_to (share of TO that are live) OR TOV_live_share
# ----------------------------
get_team_to_inputs_game_q <- function(team_id, game_date, qtr, team_to_profile_q_weaved) {
  
  r <- pick_profile_row_game_q(team_to_profile_q_weaved, team_id, game_date, qtr)
  
  if (nrow(r) == 0) {
    return(list(
      p_to = MC_ANCHOR$to,
      live_share = MC_ANCHOR$live_share
    ))
  }
  
  p_to <- NA_real_
  if ("p_to" %in% names(r))      p_to <- pct01(r$p_to[1], default = NA_real_)
  if ("TOV_pct" %in% names(r))   p_to <- pct01(r$TOV_pct[1], default = p_to)
  if (is.na(p_to)) p_to <- MC_ANCHOR$to
  
  live_share <- NA_real_
  if ("p_live_to" %in% names(r))      live_share <- pct01(r$p_live_to[1], default = NA_real_)
  if ("TOV_live_share" %in% names(r)) live_share <- pct01(r$TOV_live_share[1], default = live_share)
  if (is.na(live_share)) live_share <- MC_ANCHOR$live_share
  
  list(
    p_to = clamp01(p_to),
    live_share = clamp01(live_share)
  )
}

# ----------------------------
# pull offensive foul inputs from TEAM FOUL PROFILE (WEAVED)
# expected columns:
#   p_off_foul OR off_fouls_per_poss
# ----------------------------
get_team_off_foul_p_game_q <- function(team_id, game_date, qtr, team_foul_profile_q_weaved) {
  
  r <- pick_profile_row_game_q(team_foul_profile_q_weaved, team_id, game_date, qtr)
  
  if (nrow(r) == 0) return(MC_ANCHOR$off_foul)
  
  p_off <- NA_real_
  if ("p_off_foul" %in% names(r))         p_off <- pct01(r$p_off_foul[1], default = NA_real_)
  if ("off_fouls_per_poss" %in% names(r)) p_off <- pct01(r$off_fouls_per_poss[1], default = p_off)
  
  if (is.na(p_off)) p_off <- MC_ANCHOR$off_foul
  clamp01(p_off)
}

# ----------------------------
# main probability builder (NO windows, GAME/DATE aware)
# ----------------------------
derive_team_probs_game_q <- function(
    team_id, game_date, qtr,
    team_pts_profile_q_weaved,
    team_to_profile_q_weaved,
    team_foul_profile_q_weaved,
    mc_var    = MC_VAR,
    mc_anchor = MC_ANCHOR
) {
  
  # turnovers
  to_in <- get_team_to_inputs_game_q(team_id, game_date, qtr, team_to_profile_q_weaved)
  
  p_to_raw <- to_in$p_to
  live_share <- to_in$live_share
  
  # optional shrink for TO volatility (keep your old meaning)
  p_to <- shrink_to(
    p_to_raw,
    mc_anchor$to,
    clamp01(num0(mc_var$tov_rand, 0))
  )
  
  # shooting + FT
  pts_in <- get_team_pts_inputs_game_q(team_id, game_date, qtr, team_pts_profile_q_weaved)
  
  FG2_pct <- shrink_to(pts_in$FG2_pct, mc_anchor$fg2, num0(mc_var$shoot_shrink, 0))
  FG3_pct <- shrink_to(pts_in$FG3_pct, mc_anchor$fg3, num0(mc_var$shoot_shrink, 0))
  FT_pct  <- shrink_to(pts_in$FT_pct,  mc_anchor$ft,  num0(mc_var$shoot_shrink, 0))
  
  FG2_pct <- apply_pct_jitter(FG2_pct, num0(mc_var$pct_jitter_sd, 0))
  FG3_pct <- apply_pct_jitter(FG3_pct, num0(mc_var$pct_jitter_sd, 0))
  FT_pct  <- apply_pct_jitter(FT_pct,  num0(mc_var$pct_jitter_sd, 0))
  
  # shooting fouls
  p_shoot_foul <- pts_in$p_shoot_foul
  if (is.na(p_shoot_foul)) {
    avg_fts_per_shooting_foul <- 2.2
    p_shoot_foul <- ifelse(
      is.na(pts_in$FTA_per_FGA),
      num0(mc_anchor$shoot_foul, 0.10),
      pts_in$FTA_per_FGA / avg_fts_per_shooting_foul
    )
  }
  p_shoot_foul <- pmin(pmax(p_shoot_foul, 0), 0.18)
  
  # offensive fouls (separate from shooting fouls)
  p_off_foul <- get_team_off_foul_p_game_q(team_id, game_date, qtr, team_foul_profile_q_weaved)
  
  list(
    p_to         = p_to,
    live_share   = live_share,
    p_off_foul   = p_off_foul,
    p2_given_sh  = clamp01(num0(pts_in$p2_given_shot, 0.65)),
    FG2_pct      = FG2_pct,
    FG3_pct      = FG3_pct,
    FT_pct       = FT_pct,
    p_shoot_foul = p_shoot_foul
  )
}

# ----------------------------
# simulate team turnovers (GAME/DATE aware)
# ----------------------------
simulate_team_turnovers_game_q <- function(team_id, game_date, poss, qtr, team_to_profile_q_weaved) {
  
  to_in <- get_team_to_inputs_game_q(team_id, game_date, qtr, team_to_profile_q_weaved)
  
  p_to <- shrink_to(
    to_in$p_to,
    MC_ANCHOR$to,
    clamp01(num0(MC_VAR$tov_rand, 0))
  )
  
  poss_i <- as.integer(num0(poss, 0))
  to_ct  <- if (poss_i > 0) rbinom(1, size = poss_i, prob = clamp01(p_to)) else 0L
  to_ct  <- as.integer(to_ct)
  
  live_share <- clamp01(num0(to_in$live_share, MC_ANCHOR$live_share))
  live_ct <- if (to_ct > 0) rbinom(1, size = to_ct, prob = live_share) else 0L
  live_ct <- as.integer(live_ct)
  
  list(
    turnovers = to_ct,
    live_turnovers = live_ct,
    p_to_used = p_to,
    live_share_used = live_share
  )
}

# ----------------------------
# draw offensive fouls (GAME/DATE aware)
# ----------------------------
draw_off_fouls_game_q <- function(team_id, game_date, shot_poss, qtr, team_foul_profile_q_weaved) {
  p_off <- get_team_off_foul_p_game_q(team_id, game_date, qtr, team_foul_profile_q_weaved)
  sp <- as.integer(num0(shot_poss, 0))
  if (sp <= 0) return(0L)
  as.integer(rbinom(1, size = sp, prob = clamp01(p_off)))
}

# ----------------------------
# simulate ONE team’s quarter scoring given possessions (GAME/DATE aware)
# ----------------------------
simulate_team_offense_pts_game_q <- function(
    team_id, game_date, poss, qtr,
    team_pts_profile_q_weaved,
    team_to_profile_q_weaved,
    team_foul_profile_q_weaved,
    p_and1_given_made_foul = 0.08
) {
  
  probs <- derive_team_probs_game_q(
    team_id = team_id,
    game_date = game_date,
    qtr = qtr,
    team_pts_profile_q_weaved = team_pts_profile_q_weaved,
    team_to_profile_q_weaved  = team_to_profile_q_weaved,
    team_foul_profile_q_weaved = team_foul_profile_q_weaved,
    mc_var = MC_VAR,
    mc_anchor = MC_ANCHOR
  )
  
  poss_i <- as.integer(num0(poss, 0))
  
  tov_out <- simulate_team_turnovers_game_q(
    team_id = team_id,
    game_date = game_date,
    poss = poss_i,
    qtr  = qtr,
    team_to_profile_q_weaved = team_to_profile_q_weaved
  )
  
  total_tov <- as.integer(tov_out$turnovers)
  live_tov  <- as.integer(tov_out$live_turnovers)
  
  shot_poss <- max(poss_i - total_tov, 0L)
  
  # offensive fouls consume shot possessions and count as turnovers
  off_fouls <- draw_off_fouls_game_q(team_id, game_date, shot_poss, qtr, team_foul_profile_q_weaved)
  off_fouls <- as.integer(off_fouls)
  
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  total_shots <- as.integer(shot_poss)
  
  # 2PA / 3PA split
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = probs$p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  # makes
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = probs$FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = probs$FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  # shooting fouls (proxy)
  p_foul <- probs$p_shoot_foul
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  # and-1s on made shots (approx)
  made_foul_2_exp <- as.integer(round(FGM2 * p_foul))
  made_foul_3_exp <- as.integer(round(FGM3 * p_foul))
  
  and1_2 <- if (made_foul_2_exp > 0) rbinom(1, size = made_foul_2_exp, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3_exp > 0) rbinom(1, size = made_foul_3_exp, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  missed_foul_2 <- max(foul_2 - made_foul_2_exp, 0L)
  missed_foul_3 <- max(foul_3 - made_foul_3_exp, 0L)
  
  # free throws
  FTA <- as.integer(2L * missed_foul_2 + 3L * missed_foul_3 + 1L * (and1_2 + and1_3))
  FTM <- if (FTA > 0) rbinom(1, size = FTA, prob = probs$FT_pct) else 0L
  FTM <- as.integer(FTM)
  
  pts <- as.integer(2L * FGM2 + 3L * FGM3 + FTM)
  
  list(
    base_points    = pts,
    poss_total     = poss_i,
    turnovers      = total_tov,
    live_turnovers = live_tov,
    off_fouls      = off_fouls,
    FGA2 = FGA2, FGM2 = FGM2,
    FGA3 = FGA3, FGM3 = FGM3,
    FTA  = FTA,  FTM  = FTM,
    FG2_pct_used = probs$FG2_pct,
    FG3_pct_used = probs$FG3_pct,
    FT_pct_used  = probs$FT_pct,
    p_to_used    = tov_out$p_to_used,
    p_foul_used  = p_foul,
    p_off_foul_used = probs$p_off_foul
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END. PTS ENGINE (NO WINDOWS, GAME/DATE AWARE)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooo.              o8o                  .                 oooooooooooo                         o8o                             8      oooooooooo.                .             
#     `888   `Y88.            `"'                .o8                 `888'     `8                         `"'                             8      `888'   `Y8b             .o8             
#      888   .d88'  .ooooo.  oooo  ooo. .oo.   .o888oo  .oooo.o       888         ooo. .oo.    .oooooooo oooo  ooo. .oo.    .ooooo.       8       888     888  .ooooo.  .o888oo  .oooo.   
#      888ooo88P'  d88' `88b `888  `888P"Y88b    888   d88(  "8       888oooo8    `888P"Y88b  888' `88b  `888  `888P"Y88b  d88' `88b              888oooo888' d88' `88b   888   `P  )88b  
#      888         888   888  888   888   888    888   `"Y88b.        888    "     888   888  888   888   888   888   888  888ooo888      8       888    `88b 888ooo888   888    .oP"888  
#      888         888   888  888   888   888    888 . o.  )88b       888       o  888   888  `88bod8P'   888   888   888  888    .o      8       888    .88P 888    .o   888 . d8(  888  
#     o888o        `Y8bod8P' o888o o888o o888o   "888" 8""888P'      o888ooooood8 o888o o888o `8oooooo.  o888o o888o o888o `Y8bod8P'      8      o888bood8P'  `Y8bod8P'   "888" `Y888""8o 
#                                                                                             d"     YD                                                                                   
#                                                                                             "Y88888P'                                                                                   
#                                                                                                                                                                                    
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                                                                                                         


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ------------------------------------------------------------
# MODEL A FIX POINTS ENGINE BETA: Extra shots come ONLY from "miss -> OREB -> extra shot"
#   - NO additive SEC_CHN_FGA driver (avoids double-count risk)
#   - OREB draw is capped by misses (can't OREB a make)
# ------------------------------------------------------------
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
simulate_team_offense_modelA_q_game <- function(
    team_id, game_date, qtr, poss,
    team_pts_profile_q_weaved,
    team_to_profile_q_game,
    team_foul_profile_q_game,
    team_reb_profile_q_game,
    sd_shrink_fga = 0.75,
    p_and1_given_made_foul = 0.08
) {
  
  # ---------------------------
  # 1) Pull per-game shooting profile
  # ---------------------------
  pts_prof <- team_pts_profile_q_weaved %>%
    dplyr::filter(
      team_id == !!team_id,
      GAME_DATE == !!game_date,
      qtr == !!qtr
    ) %>%
    dplyr::slice(1)
  
  if (nrow(pts_prof) == 0) stop("Missing team_pts_profile_q_weaved row")
  
  FG2_pct <- apply_pct_jitter(pts_prof$FG2_pct, MC_VAR$pct_jitter_sd)
  FG3_pct <- apply_pct_jitter(pts_prof$FG3_pct, MC_VAR$pct_jitter_sd)
  FT_pct  <- apply_pct_jitter(pts_prof$FT_pct,  MC_VAR$pct_jitter_sd)
  
  p2_given_shot <- clamp01(pts_prof$p2_given_shot)
  FTA_per_FGA   <- num0(pts_prof$FTA_per_FGA, 0.22)
  
  # ---------------------------
  # 2) Turnovers (new TOV MC)
  # ---------------------------
  tov_out <- simulate_team_turnovers_q(
    team_id   = team_id,
    game_date = game_date,
    poss      = poss,
    qtr       = qtr,
    team_to_profile_q_game = team_to_profile_q_game
  )
  
  shot_poss <- as.integer(pmax(poss - tov_out$turnovers, 0L))
  
  # ---------------------------
  # 3) Offensive fouls
  # ---------------------------
  q_foul <- team_foul_profile_q_game %>%
    dplyr::filter(team_id == !!team_id, GAME_DATE == !!game_date, qtr == !!qtr) %>%
    dplyr::slice(1)
  
  off_fouls <- if (nrow(q_foul) > 0)
    rbinom(1, size = shot_poss, prob = clamp01(q_foul$PF_per_poss))
  else 0L
  
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(tov_out$turnovers + off_fouls)
  
  # ---------------------------
  # 4) BASE SHOTS
  # ---------------------------
  base_fga <- as.integer(shot_poss)
  
  # split 2 / 3
  FGA2 <- if (base_fga > 0) rbinom(1, base_fga, p2_given_shot) else 0L
  FGA3 <- as.integer(base_fga - FGA2)
  
  FGM2 <- if (FGA2 > 0) rbinom(1, FGA2, FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, FGA3, FG3_pct) else 0L
  
  misses_base <- as.integer((FGA2 - FGM2) + (FGA3 - FGM3))
  
  # ---------------------------
  # 5) MODEL A: OREB → EXTRA SHOTS
  # ---------------------------
  reb_prof <- team_reb_profile_q_game %>%
    dplyr::filter(team_id == !!team_id, GAME_DATE == !!game_date, qtr == !!qtr) %>%
    dplyr::slice(1)
  
  extra_shots <- if (nrow(reb_prof) > 0) {
    mu <- reb_prof$OREB_mean * MC_VAR$reb_mean_mult
    sd <- reb_prof$OREB_sd   * MC_VAR$reb_sd_mult
    pmin(draw_attempts(mu, sd), misses_base)
  } else 0L
  
  extra_shots <- as.integer(pmax(extra_shots, 0L))
  
  extra_FGA2 <- if (extra_shots > 0) rbinom(1, extra_shots, p2_given_shot) else 0L
  extra_FGA3 <- as.integer(extra_shots - extra_FGA2)
  
  extra_FGM2 <- if (extra_FGA2 > 0) rbinom(1, extra_FGA2, FG2_pct) else 0L
  extra_FGM3 <- if (extra_FGA3 > 0) rbinom(1, extra_FGA3, FG3_pct) else 0L
  
  # ---------------------------
  # 6) Free throws
  # ---------------------------
  p_shoot_foul <- pmin(FTA_per_FGA / 2.0, 0.16)
  
  total_made <- FGM2 + FGM3 + extra_FGM2 + extra_FGM3
  made_foul_exp <- round(total_made * p_shoot_foul)
  
  and1 <- if (made_foul_exp > 0)
    rbinom(1, made_foul_exp, clamp01(p_and1_given_made_foul))
  else 0L
  
  missed_foul <- rbinom(1, base_fga + extra_shots, p_shoot_foul)
  FTA <- as.integer(2L * missed_foul + and1)
  
  FTM <- if (FTA > 0) rbinom(1, FTA, FT_pct) else 0L
  
  # ---------------------------
  # 7) Points
  # ---------------------------
  pts <- as.integer(
    2L * (FGM2 + extra_FGM2) +
      3L * (FGM3 + extra_FGM3) +
      FTM
  )
  
  list(
    base_points    = pts,
    poss_total     = poss,
    shot_poss      = shot_poss,
    turnovers      = total_tov,
    live_turnovers = tov_out$live_turnovers,
    off_fouls      = off_fouls,
    FGA2 = FGA2 + extra_FGA2,
    FGM2 = FGM2 + extra_FGM2,
    FGA3 = FGA3 + extra_FGA3,
    FGM3 = FGM3 + extra_FGM3,
    FTA  = FTA,
    FTM  = FTM,
    misses_base = misses_base,
    extra_shots = extra_shots
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END MODEL A FIX POINTS ENGINE BETA | SECOND CHANCE AND POSSESSIONS MODEL
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooooooo                                            ooooooooo.                       .o88o.  o8o  oooo                     
#     8'   888   `8                                            `888   `Y88.                     888 `"  `"'  `888                     
#          888       .ooooo.   .oooo.   ooo. .oo.  .oo.         888   .d88' oooo d8b  .ooooo.  o888oo  oooo   888   .ooooo.   .oooo.o 
#          888      d88' `88b `P  )88b  `888P"Y88bP"Y88b        888ooo88P'  `888""8P d88' `88b  888    `888   888  d88' `88b d88(  "8 
#          888      888ooo888  .oP"888   888   888   888        888          888     888   888  888     888   888  888ooo888 `"Y88b.  
#          888      888    .o d8(  888   888   888   888        888          888     888   888  888     888   888  888    .o o.  )88b 
#         o888o     `Y8bod8P' `Y888""8o o888o o888o o888o      o888o        d888b    `Y8bod8P' o888o   o888o o888o `Y8bod8P' 8""888P' 
# 
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# --------------------------------------------------------------------------------
# TEAM MC PROFILE (v_4) — UPDATED (NO WINDOWS, WEAVED PER-GAME INPUT)
#
# WHAT:
# Builds a single, season-level baseline profile per team containing average
# FG2%, FG3%, and FT% collapsed across all quarters from team_pts_profile_q_weaved.
# One row per team_id per season_token.
#
# WHY:
# Stable shooting anchor for shrinkage / fallback / audit. Quarter dynamics remain
# in team_poss_profile_q, team_to_profile_q, team_foul_profile_q, team_pts_profile_q_weaved.
#
# HOW:
# Uses ONLY games strictly before proj_date (NO LEAKAGE). Averages across all
# available quarters and games for each team. Clamps + defaults for stability.
# --------------------------------------------------------------------------------

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# 3. START: Build single team_mc_profile (v_4) [WEAVED, NO WINDOWS]
#   - Purpose (v_4): one row per team_id per season_token
#   - Holds baseline shooting efficiencies + optional abbrev for audit
#   - Quarter dynamics come from:
#       team_poss_profile_q, team_to_profile_q, team_foul_profile_q, team_pts_profile_q_weaved
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

stopifnot(exists("mode"))
stopifnot(exists("season_token"))
stopifnot(exists("team_pts_profile_q_weaved"))
stopifnot(nrow(team_pts_profile_q_weaved) > 0)

# ---- helpers (local, safe) ----
clamp01 <- function(x) pmin(pmax(x, 0), 1)

# --------------------------------------------------
# 0) Projection date (single source of truth)
# --------------------------------------------------
proj_date <- if (mode == "today") {
  stopifnot(exists("current_date"))
  as.Date(current_date)
} else if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  as.Date(as_of_date)
} else stop("Invalid mode in team_mc_profile section.")

# --------------------------------------------------
# 1) Baseline shooting: collapse WEAVED quarter inputs across quarters + games
#     NO LEAKAGE: uses only GAME_DATE < proj_date
# --------------------------------------------------
team_mc_profile <- team_pts_profile_q_weaved %>%
  dplyr::mutate(
    GAME_DATE = as.Date(GAME_DATE),
    team_id   = as.character(team_id)
  ) %>%
  dplyr::filter(!is.na(team_id), team_id != "", !is.na(GAME_DATE)) %>%
  dplyr::filter(GAME_DATE < proj_date) %>%   # <-- NO LEAKAGE
  dplyr::group_by(team_id) %>%
  dplyr::summarise(
    FG2_pct = mean(FG2_pct, na.rm = TRUE),
    FG3_pct = mean(FG3_pct, na.rm = TRUE),
    FT_pct  = mean(FT_pct,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    season_token = as.character(season_token),
    FG2_pct = clamp01(ifelse(is.na(FG2_pct), 0.50, FG2_pct)),
    FG3_pct = clamp01(ifelse(is.na(FG3_pct), 0.36, FG3_pct)),
    FT_pct  = clamp01(ifelse(is.na(FT_pct),  0.78, FT_pct))
  ) %>%
  dplyr::select(team_id, season_token, FG2_pct, FG3_pct, FT_pct)

# --------------------------------------------------
# 2) Optional: team abbrev lookup for audit outputs (safe, no dependencies)
#    expects pbp_df contains home/away abbrevs and team_id columns
# --------------------------------------------------
if (exists("pbp_df")) {
  team_lookup <- pbp_df %>%
    dplyr::select(team_id, home_team_id, away_team_id, home_team_abbrev, away_team_abbrev) %>%
    dplyr::mutate(
      team_id = as.character(team_id),
      team_abbrev = dplyr::case_when(
        as.character(team_id) == as.character(home_team_id) ~ home_team_abbrev,
        as.character(team_id) == as.character(away_team_id) ~ away_team_abbrev,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(team_abbrev)) %>%
    dplyr::distinct(team_id, team_abbrev)
  
  team_mc_profile <- team_mc_profile %>%
    dplyr::left_join(team_lookup, by = "team_id")
}

# --------------------------------------------------
# 3) season_chosen for the MC calls (keep this var name)
# --------------------------------------------------
season_chosen <- as.character(season_token)

message(
  "team_mc_profile built | mode=", mode,
  " | proj_date=", format(proj_date),
  " | teams=", nrow(team_mc_profile)
)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# 3. END: team_mc_profile (v_4) [WEAVED, NO WINDOWS]
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#      .oooooo.                                                                         .              .o.             .o8      o8o                          .                                               .   
#      d8P'  `Y8b                                                                      .o8             .888.           "888      `"'                        .o8                                             .o8   
#     888      888 oo.ooooo.  oo.ooooo.   .ooooo.  ooo. .oo.    .ooooo.  ooo. .oo.   .o888oo          .8"888.      .oooo888     oooo oooo  oooo   .oooo.o .o888oo ooo. .oo.  .oo.    .ooooo.  ooo. .oo.   .o888oo 
#     888      888  888' `88b  888' `88b d88' `88b `888P"Y88b  d88' `88b `888P"Y88b    888           .8' `888.    d88' `888     `888 `888  `888  d88(  "8   888   `888P"Y88bP"Y88b  d88' `88b `888P"Y88b    888   
#     888      888  888   888  888   888 888   888  888   888  888ooo888  888   888    888          .88ooo8888.   888   888      888  888   888  `"Y88b.    888    888   888   888  888ooo888  888   888    888   
#     `88b    d88'  888   888  888   888 888   888  888   888  888    .o  888   888    888 .       .8'     `888.  888   888      888  888   888  o.  )88b   888 .  888   888   888  888    .o  888   888    888 . 
#      `Y8bood8P'   888bod8P'  888bod8P' `Y8bod8P' o888o o888o `Y8bod8P' o888o o888o   "888"      o88o     o8888o `Y8bod88P"     888  `V88V"V8P' 8""888P'   "888" o888o o888o o888o `Y8bod8P' o888o o888o   "888" 
#                   888        888                                                                                               888                                                                              
#                  o888o      o888o                                                                                          .o. 88P                                                                              
#                                                                                                                            `Y888P                                                                               
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# OPPONENT ADJUSTMENT LAYER (PER-GAME, PLAYER-DERIVED / WEAVED)
#   Purpose:
#     - Build "defense allowed" per-game, quarter-level rows by swapping opponent OFFENSE profiles
#     - NO WINDOWS, NO AGGREGATION
#
#   Input (expected):
#     team_pts_profile_q_weaved  (your player-derived team profile)
#       - one row per team_id x ESPN_GAME_ID (or game_id) x GAME_DATE x qtr
#       - must include: FG2_pct, FG3_pct, FT_pct, p2_given_shot, FTA_per_FGA
#
#   Outputs:
#     - def_allowed_long_game         (long, per-game, per-quarter)
#     - team_def_allowed_profile_q_game (wide, per-game, per-quarter)
#         (this is still "profile" shaped, but it's per-game, not windowed)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

stopifnot(exists("team_pts_profile_q_weaved"))
stopifnot(nrow(team_pts_profile_q_weaved) > 0)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(tibble)
})

# ----------------------------
# helpers
# ----------------------------
parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

# ----------------------------
# detect game id key (ESPN_GAME_ID vs game_id)
# ----------------------------
gid_col <- if ("ESPN_GAME_ID" %in% names(team_pts_profile_q_weaved)) {
  "ESPN_GAME_ID"
} else if ("game_id" %in% names(team_pts_profile_q_weaved)) {
  "game_id"
} else {
  stop("team_pts_profile_q_weaved must contain ESPN_GAME_ID or game_id.")
}

# ----------------------------
# schema prep
# ----------------------------
df <- team_pts_profile_q_weaved %>%
  mutate(
    team_id      = if ("team_id" %in% names(.)) as.character(team_id) else as.character(ESPN_TEAM_ID),
    season_token = if ("season_token" %in% names(.)) as.character(season_token) else as.character(season_token),
    GAME_DATE    = if ("GAME_DATE" %in% names(.)) GAME_DATE else if ("game_date_dt" %in% names(.)) game_date_dt else NA,
    game_date_dt = parse_game_date(GAME_DATE),
    qtr          = as.integer(qtr)
  ) %>%
  mutate(
    !!gid_col := as.character(.data[[gid_col]])
  ) %>%
  filter(!is.na(team_id), team_id != "", !is.na(.data[[gid_col]]), .data[[gid_col]] != "", !is.na(qtr))

# optional leakage cut (if slate_dt exists)
if (exists("slate_dt")) {
  slate_cut <- as.Date(slate_dt)
  df <- df %>% filter(is.na(game_date_dt) | game_date_dt < slate_cut)
}

# ----------------------------
# choose which OFFENSE profile fields we want as "allowed"
#   (these are from your weaved player-derived team profile)
# ----------------------------
off_fields <- c("p2_given_shot","FG2_pct","FG3_pct","FT_pct","FTA_per_FGA")
off_fields <- off_fields[off_fields %in% names(df)]
stopifnot(length(off_fields) > 0)

# enforce numeric for swap fields
df <- df %>%
  mutate(across(all_of(off_fields), ~ suppressWarnings(as.numeric(.))))

# ----------------------------
# ensure uniqueness per team/game/qtr (safety)
# ----------------------------
dup_check <- df %>%
  count(season_token, .data[[gid_col]], team_id, qtr) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  message("⚠️ Deduplicating team_pts_profile_q_weaved for opponent swap (multiple rows per team/game/qtr detected).")
}

df_min <- df %>%
  select(any_of(c("team_id", gid_col, "season_token", "game_date_dt", "qtr", off_fields))) %>%
  distinct(season_token, .data[[gid_col]], team_id, qtr, .keep_all = TRUE)

# ----------------------------
# opponent swap (same game_id, different team_id)
# ----------------------------
opp_swapped <- df_min %>%
  rename_with(~ paste0("OFF__", .x), all_of(off_fields)) %>%
  inner_join(
    df_min %>%
      select(-game_date_dt) %>%  # prevents .x/.y
      rename_with(~ paste0("OPP__", .x), all_of(off_fields)) %>%
      rename(opp_team_id = team_id),
    by = c("season_token", gid_col, "qtr")
  ) %>%
  filter(team_id != opp_team_id)

# ----------------------------
# long output (one row per team/game/qtr/stat)
# ----------------------------
def_allowed_long_game <- opp_swapped %>%
  pivot_longer(
    cols      = starts_with("OPP__"),
    names_to  = "stat",
    values_to = "opp_value"
  ) %>%
  mutate(
    stat = sub("^OPP__", "", stat)
  ) %>%
  select(
    team_id,
    opp_team_id,
    season_token,
    !!gid_col,
    game_date_dt,
    qtr,
    stat,
    opp_value
  ) %>%
  arrange(season_token, .data[[gid_col]], team_id, qtr, stat)

# ----------------------------
# wide output (per team/game/qtr) — easier to consume
# ----------------------------
team_def_allowed_profile_q_game <- def_allowed_long_game %>%
  pivot_wider(
    names_from  = stat,
    values_from = opp_value,
    names_prefix = "DEF_ALLOW__"
  ) %>%
  arrange(season_token, .data[[gid_col]], team_id, qtr)

# ----------------------------
# sanity checks
# ----------------------------
stopifnot(nrow(def_allowed_long_game) > 0)
stopifnot(nrow(team_def_allowed_profile_q_game) > 0)

message(
  "DEF ALLOWED (per-game) built | gid_col=", gid_col,
  " | long_rows=", nrow(def_allowed_long_game),
  " | wide_rows=", nrow(team_def_allowed_profile_q_game),
  " | stats=", length(unique(def_allowed_long_game$stat))
)

print(team_def_allowed_profile_q_game %>% dplyr::slice(1:20))

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: OPPONENT ADJUSTMENT LAYER (PER-GAME, PLAYER-DERIVED / WEAVED)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠\



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooo        ooooo                           .                    .oooooo.                      oooo                  .oooooo..o  o8o                                oooo                .    o8o                        
#     `88.       .888'                         .o8                   d8P'  `Y8b                     `888                 d8P'    `Y8  `"'                                `888              .o8    `"'                        
#      888b     d'888   .ooooo.  ooo. .oo.   .o888oo  .ooooo.       888           .oooo.   oooo d8b  888   .ooooo.       Y88bo.      oooo  ooo. .oo.  .oo.   oooo  oooo   888   .oooo.   .o888oo oooo   .ooooo.  ooo. .oo.   
#      8 Y88. .P  888  d88' `88b `888P"Y88b    888   d88' `88b      888          `P  )88b  `888""8P  888  d88' `88b       `"Y8888o.  `888  `888P"Y88bP"Y88b  `888  `888   888  `P  )88b    888   `888  d88' `88b `888P"Y88b  
#      8  `888'   888  888   888  888   888    888   888ooo888      888           .oP"888   888      888  888   888           `"Y88b  888   888   888   888   888   888   888   .oP"888    888    888  888   888  888   888  
#      8    Y     888  888   888  888   888    888 . 888    .o      `88b    ooo  d8(  888   888      888  888   888      oo     .d8P  888   888   888   888   888   888   888  d8(  888    888 .  888  888   888  888   888  
#     o8o        o888o `Y8bod8P' o888o o888o   "888" `Y8bod8P'       `Y8bood8P'  `Y888""8o d888b    o888o `Y8bod8P'      8""88888P'  o888o o888o o888o o888o  `V88V"V8P' o888o `Y888""8o   "888" o888o `Y8bod8P' o888o o888o 
#
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠       


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: AS_OF_DATE Minutes Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================

stopifnot(
  exists("mode"),
  exists("roster_active_by_date"),
  exists("roster_out_by_date"),
  exists("player_rotations")
)

# -----------------------------
# 1) Resolve projection date (MODE-SENSITIVE)
# -----------------------------
if (mode == "backtest") {
  stopifnot(exists("as_of_date"))
  proj_date <- as.Date(as_of_date)
} else {
  stopifnot(exists("current_date"))
  proj_date <- as.Date(current_date)
}

if (is.na(proj_date)) stop("Invalid projection date")

k <- format(proj_date, "%Y%m%d")

stopifnot(k %in% names(roster_active_by_date))
stopifnot(k %in% names(roster_out_by_date))

roster_active <- roster_active_by_date[[k]]
roster_out    <- roster_out_by_date[[k]]

stopifnot(nrow(roster_active) > 0)

# ---- helpers ----
num0 <- function(x, d = 0) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), d, x) }
clamp01 <- function(x) pmin(pmax(x, 0), 1)

role_bucket_from_min <- function(m) {
  m <- num0(m, 0)
  dplyr::case_when(
    m >= 28 ~ "starter",
    m >= 18 ~ "rotation",
    m >= 8  ~ "bench",
    TRUE    ~ "fringe"
  )
}

quarters <- c("Q1","Q2","Q3","Q4","Q5","Q6")

# -----------------------------
# 0) Normalize + DE-DUPE player_rotations
# -----------------------------
normalize_player_rotations <- function(pr) {
  
  if (!("GAME_DATE" %in% names(pr))) {
    if ("GAME_DATE.x" %in% names(pr)) pr <- pr %>% rename(GAME_DATE = `GAME_DATE.x`)
    if ("GAME_DATE.y" %in% names(pr)) pr <- pr %>% select(-`GAME_DATE.y`)
  }
  
  req <- c("GAME_DATE","ESPN_GAME_ID","ESPN_TEAM_ID","PLAYER_ID","MINS")
  if (!all(req %in% names(pr))) {
    stop("player_rotations missing: ", paste(setdiff(req, names(pr)), collapse = ", "))
  }
  
  pr %>%
    mutate(
      GAME_DATE      = as.Date(GAME_DATE),
      ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      ESPN_PLAYER_ID = as.character(PLAYER_ID),
      MINS_NUM       = num0(MINS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE)) %>%
    group_by(GAME_DATE, ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID) %>%
    summarise(
      MINS_NUM = max(MINS_NUM, na.rm = TRUE),
      STARTER_STATUS = if ("STARTER_STATUS" %in% names(pr)) {
        tmp <- pr$STARTER_STATUS[!is.na(pr$STARTER_STATUS)]
        if (length(tmp) == 0) NA_character_ else as.character(tmp[1])
      } else NA_character_,
      .groups = "drop"
    )
}

player_rotations_clean <- normalize_player_rotations(player_rotations)

player_rotations_clean <- player_rotations_clean %>%
  left_join(
    player_rotations %>% select(PLAYER_ID, PLAYER_NAME) %>% distinct(),
    by = "PLAYER_ID"
  )

# -----------------------------
# A) Minutes baselines (LEAKAGE-SAFE, PER QUARTER → COLUMNS)
# -----------------------------
build_minutes_baseline_q <- function(cutoff_date, q, n_games_back = 10L) {
  
  mins_col <- paste0("MINS_", q)
  if (!exists("BaseStats_Player_MC") || !(mins_col %in% names(BaseStats_Player_MC))) {
    return(NULL)
  }
  
  BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = as.Date(GAME_DATE),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      mins_val       = num0(.data[[mins_col]], NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, game_date_dt) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      !!paste0("mins_mean_", q) := mean(mins_val, na.rm = TRUE),
      !!paste0("mins_sd_", q)   := sd(mins_val, na.rm = TRUE),
      .groups = "drop"
    )
}

minutes_baseline_list <- lapply(quarters, function(q) {
  build_minutes_baseline_q(proj_date, q, 10L)
})

minutes_baseline <- Reduce(
  function(x, y) full_join(x, y, by = c("ESPN_PLAYER_ID","ESPN_TEAM_ID")),
  minutes_baseline_list
)

# -----------------------------
# B) OPTIONAL: usage / efficiency baseline (UNCHANGED)
# -----------------------------
build_usage_eff_baseline <- function(cutoff_date, n_games_back = 10L) {
  
  if (!exists("BaseStats_Player_MC")) return(tibble())
  
  BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = as.Date(GAME_DATE),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      MIN_NUM        = num0(MINS_CGS, 0),
      USG_NUM        = num0(USG_CGS, NA_real_),
      PTS_NUM        = num0(PTS_CGS, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < cutoff_date) %>%
    arrange(ESPN_PLAYER_ID, game_date_dt) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      baseline_usg = mean(USG_NUM, na.rm = TRUE),
      pts_per_min  = ifelse(sum(MIN_NUM) > 0, sum(PTS_NUM) / sum(MIN_NUM), NA_real_),
      .groups = "drop"
    )
}

usage_eff_base <- build_usage_eff_baseline(proj_date, 10L)

# -----------------------------
# 3) Availability table (NO ROW EXPANSION)
# -----------------------------
inj_out_slim <- roster_out %>%
  transmute(ESPN_TEAM_ID, ESPN_PLAYER_ID, last_status = "out") %>%
  distinct()

minutes_availability_df <- roster_active %>%
  transmute(
    date_key = k,
    GAME_DATE = proj_date,
    ESPN_TEAM_ID,
    NBA_TEAM_ID,
    ESPN_PLAYER_ID,
    PLAYER
  ) %>%
  left_join(minutes_baseline, by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  left_join(usage_eff_base, by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  left_join(inj_out_slim,   by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  mutate(
    last_status = ifelse(is.na(last_status), "active", last_status)
  )

# -----------------------------
# 4) Redistribution + MC draw (PER QUARTER, COLUMN-WISE)
# -----------------------------
for (q in quarters) {
  
  mean_col <- paste0("mins_mean_", q)
  sd_col   <- paste0("mins_sd_", q)
  exp_col  <- paste0("expected_minutes_adj_", q)
  sim_col  <- paste0("minutes_sim_", q)
  sim_sd   <- paste0("minutes_sd_", q)
  
  minutes_availability_df[[exp_col]] <- ifelse(
    minutes_availability_df$last_status == "out",
    0,
    num0(minutes_availability_df[[mean_col]], 0)
  )
  
  minutes_availability_df[[sim_sd]] <- pmax(
    1.5,
    num0(minutes_availability_df[[sd_col]],
         minutes_availability_df[[exp_col]] * 0.20)
  )
  
  minutes_availability_df[[sim_col]] <- pmin(
    12,
    pmax(
      0,
      rnorm(
        nrow(minutes_availability_df),
        minutes_availability_df[[exp_col]],
        minutes_availability_df[[sim_sd]]
      )
    )
  )
}


player_projections_by_date <- minutes_availability_df

# -----------------------------
# 7) CGS totals (DERIVED, NO RANDOMNESS)
# -----------------------------

q_exp_cols <- paste0("expected_minutes_adj_", quarters)
q_sim_cols <- paste0("minutes_sim_", quarters)

player_projections_by_date <- player_projections_by_date %>%
  mutate(
    expected_minutes_adj_CGS = rowSums(across(all_of(q_exp_cols)), na.rm = TRUE),
    minutes_sim_CGS          = rowSums(across(all_of(q_sim_cols)), na.rm = TRUE)
  )

# ======================================
# END: Minutes Projection
# ======================================


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: AS_OF_DATE Minutes Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
