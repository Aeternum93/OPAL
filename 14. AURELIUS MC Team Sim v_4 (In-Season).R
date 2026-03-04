# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#           .o.       ooooo     ooo ooooooooo.   oooooooooooo ooooo        ooooo ooooo     ooo  .oooooo..o      8      ooo        ooooo   .oooooo.   
#          .888.      `888'     `8' `888   `Y88. `888'     `8 `888'        `888' `888'     `8' d8P'    `Y8      8      `88.       .888'  d8P'  `Y8b  
#         .8"888.      888       8   888   .d88'  888          888          888   888       8  Y88bo.           8       888b     d'888  888          
#        .8' `888.     888       8   888ooo88P'   888oooo8     888          888   888       8   `"Y8888o.       8       8 Y88. .P  888  888          
#       .88ooo8888.    888       8   888`88b.     888    "     888          888   888       8       `"Y88b      8       8  `888'   888  888          
#      .8'     `888.   `88.    .8'   888  `88b.   888       o  888       o  888   `88.    .8'  oo     .d8P      8       8    Y     888  `88b    ooo  
#     o88o     o8888o    `YbodP'    o888o  o888o o888ooooood8 o888ooooood8 o888o    `YbodP'    8""88888P'       8      o8o        o888o  `Y8bood8P'  
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

# -----------------------------
# Mode switch
# -----------------------------
mode <- "today"   # "today" or "backtest"
load_player_odds <- FALSE  # keep FALSE until you actually use them in this workflow

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


# -----------------------------
# OPTIONAL: Load player odds (only if you set load_player_odds = TRUE)
# -----------------------------
nba_odds_points  <- NULL
nba_odds_assists <- NULL
nba_odds_rebounds <- NULL
nba_odds_threes  <- NULL
nba_odds_steals  <- NULL
nba_odds_blocks  <- NULL

if (isTRUE(load_player_odds)) {
  nba_odds_points_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_points_", season_token, ".csv"))
  nba_odds_assists_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_points_assists_", season_token, ".csv"))
  nba_odds_rebounds_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_rebounds_", season_token, ".csv"))
  nba_odds_threes_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_threes_", season_token, ".csv"))
  nba_odds_steals_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_steals_", season_token, ".csv"))
  nba_odds_blocks_file <- file.path(odds_player_dir, paste0("nba_historical_player_odds_player_blocks_", season_token, ".csv"))
  
  nba_odds_points  <- fread(nba_odds_points_file,  colClasses = "character", encoding = "UTF-8")
  nba_odds_assists <- fread(nba_odds_assists_file, colClasses = "character", encoding = "UTF-8")
  nba_odds_rebounds <- fread(nba_odds_rebounds_file, colClasses = "character", encoding = "UTF-8")
  nba_odds_threes  <- fread(nba_odds_threes_file,  colClasses = "character", encoding = "UTF-8")
  nba_odds_steals  <- fread(nba_odds_steals_file,  colClasses = "character", encoding = "UTF-8")
  nba_odds_blocks  <- fread(nba_odds_blocks_file,  colClasses = "character", encoding = "UTF-8")
}

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
  
  # 4) This will determine rebounding and second chance 2nd-chance. reb_sd_mult wll be for more or less randomness in the rebound and second chance points levers. reb_mean_mult will change the
  # mean  up and down. 
  reb_sd_mult   = 1.00,
  reb_mean_mult = 1.00
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


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END VARIANCE CONTROLS
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#       .oooooo.                                    .                  ooooooooo.             oooo  oooo   o8o                               .oooooo..o            oooo   o8o      .            
#      d8P'  `Y8b                                 .o8                  `888   `Y88.           `888  `888   `"'                              d8P'    `Y8            `888   `"'    .o8            
#     888          oooo d8b  .ooooo.   .oooo.   .o888oo  .ooooo.        888   .d88'  .ooooo.   888   888  oooo  ooo. .oo.    .oooooooo      Y88bo.      oo.ooooo.   888  oooo  .o888oo  .oooo.o 
#     888          `888""8P d88' `88b `P  )88b    888   d88' `88b       888ooo88P'  d88' `88b  888   888  `888  `888P"Y88b  888' `88b        `"Y8888o.   888' `88b  888  `888    888   d88(  "8 
#     888           888     888ooo888  .oP"888    888   888ooo888       888`88b.    888   888  888   888   888   888   888  888   888            `"Y88b  888   888  888   888    888   `"Y88b.  
#     `88b    ooo   888     888    .o d8(  888    888 . 888    .o       888  `88b.  888   888  888   888   888   888   888  `88bod8P'       oo     .d8P  888   888  888   888    888 . o.  )88b 
#      `Y8bood8P'  d888b    `Y8bod8P' `Y888""8o   "888" `Y8bod8P'      o888o  o888o `Y8bod8P' o888o o888o o888o o888o o888o `8oooooo.       8""88888P'   888bod8P' o888o o888o   "888" 8""888P' 
#                                                                                                                           d"     YD                    888                                    
#                                                                                                                           "Y88888P'                   o888o               
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠




# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 1. START: Build DAILY + ROLLING split UNITS (for MC backtests)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# - All backtest splits depend on comparing game dates to a test_date.
# - If game_date_dt is missing or all NA, we cannot safely filter
#   "games before test_date" and will risk leakage / invalid splits.
if (!"game_date_dt" %in% names(pbp_df)) stop("pbp_df missing game_date_dt. Fix Section 0 first.")
if (all(is.na(pbp_df$game_date_dt))) stop("pbp_df game_date_dt is all NA. Backtests need valid dates.")
stopifnot(exists("nba_schedule_season"))

# Ensures a single Date object representing "today" is used as the cutoff for separating historical training data from future games.
today_dt <- as.Date(current_date)

# ---- Normalize schedule date column to Date (fixed column name: game_date) ----
if (!"game_date" %in% names(nba_schedule_season)) {
  stop("nba_schedule_season missing game_date column.")
}

# Normalize the schedule game_date to a Date column (game_date_dt) and remove rows with invalid dates so date-based slate filtering is reliable.
nba_schedule_season_clean <- nba_schedule_season %>%
  dplyr::mutate(game_date_dt = suppressWarnings(as.Date(game_date))) %>%
  dplyr::filter(!is.na(game_date_dt))

# Build an ordered vector of past game dates (prior to today) that are eligible for backtesting and split construction.
test_dates <- nba_schedule_season_clean %>%
  dplyr::distinct(game_date_dt) %>%
  dplyr::filter(game_date_dt < today_dt) %>%
  dplyr::arrange(game_date_dt) %>%
  dplyr::pull(game_date_dt)

# Abort if no historical game dates exist prior to today, since backtesting requires at least one past slate to evaluate.
if (length(test_dates) == 0) stop("No eligible test_dates found in nba_schedule_season prior to today.")

# Determine the earliest game date in the schedule to define the season start boundary used for season-to-date training splits.
# Abort if the season start cannot be determined (e.g., all dates missing).
season_start_dt <- min(nba_schedule_season_clean$game_date_dt, na.rm = TRUE)
if (is.infinite(season_start_dt)) stop("Could not determine season_start_dt from nba_schedule_season_clean.")

# Define rolling window sizes (number of most recent games per team) used to build multiple recency-based training subsets.
rolling_n_vec <- c(5L, 7L, 10L, 15L, 20L, 25L, 35L)

# Build a rolling PBP dataset containing only the most recent N games for each team by identifying per-team game order and filtering PBP rows accordingly.
build_roll_pbp <- function(pbp_train_all, n_games) {
  team_games <- pbp_train_all %>%
    dplyr::distinct(team_id, game_id, game_date_dt) %>%
    dplyr::arrange(team_id, game_date_dt, game_id) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(game_num = dplyr::row_number(), max_num = max(game_num)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(game_num > (max_num - n_games)) %>%
    dplyr::select(team_id, game_id) %>%
    dplyr::distinct()
  
  pbp_train_all %>%
    dplyr::inner_join(team_games, by = c("team_id", "game_id"))
}

# Retrieve the game schedule for a specific test date, returning only the slate of matchups to be simulated for that day.
get_schedule_for_date <- function(test_date) {
  test_date <- as.Date(test_date)
  
  nba_schedule_season_clean %>%
    dplyr::filter(game_date_dt == test_date) %>%
    dplyr::select(-game_date_dt)
}

# Construct all training and slate inputs for a single test date, including full-history PBP, season-to-date PBP, rolling per-team PBP windows,
# team-level training data, and the scheduled games to simulate.
build_date_splits <- function(test_date) {
  test_date <- as.Date(test_date)
  date_key  <- format(test_date, "%Y%m%d")
  
  pbp_train_all <- pbp_df %>%
    dplyr::filter(!is.na(game_date_dt), game_date_dt < test_date)
  
  pbp_season <- pbp_df %>%
    dplyr::filter(!is.na(game_date_dt),
                  game_date_dt >= season_start_dt,
                  game_date_dt <  test_date)
  
  base_team_train <- BaseStats_Team_MC
  if ("game_date" %in% names(BaseStats_Team_MC)) {
    base_team_train <- BaseStats_Team_MC %>%
      dplyr::mutate(game_date_dt = suppressWarnings(as.Date(game_date))) %>%
      dplyr::filter(is.na(game_date_dt) | game_date_dt < test_date) %>%
      dplyr::select(-game_date_dt)
  }
  
  # ✅ FIX: schedule should be for *test_date* (not current_date)
  schedule_df <- get_schedule_for_date(test_date)
  
  rolling_list <- purrr::map(rolling_n_vec, function(n) {
    build_roll_pbp(pbp_train_all, n_games = n)
  })
  names(rolling_list) <- paste0("roll_", rolling_n_vec)
  rolling_list$season <- pbp_season
  
  list(
    date_key        = date_key,
    test_date       = test_date,
    schedule_df     = schedule_df,
    pbp_train_all   = pbp_train_all,
    pbp_season      = pbp_season,
    base_team_train = base_team_train,
    rolling_pbps    = rolling_list
  )
}

# Limit backtest construction to the most recent N past game dates to reduce runtime and focus evaluation on recent slates.
n_test_days_to_build <- 7L
test_dates_subset <- tail(test_dates, n_test_days_to_build)

# Initialize containers to store per-date backtest units, separating core daily inputs from rolling PBP subsets.
daily_units   <- list()
rolling_units <- list()

# Loop over selected backtest dates, build date-specific training splits, and store core inputs and rolling PBP windows keyed by date for later MC use
for (d in test_dates_subset) {
  obj <- build_date_splits(d)
  k <- obj$date_key
  
  daily_units[[k]] <- list(
    test_date       = obj$test_date,
    schedule_df     = obj$schedule_df,
    pbp_train_all   = obj$pbp_train_all,
    pbp_season      = obj$pbp_season,
    base_team_train = obj$base_team_train
  )
  
  rolling_units[[k]] <- obj$rolling_pbps
  
  message(
    "Built splits for: ", k,
    " | train_pbp_rows=", nrow(obj$pbp_train_all),
    " | season_pbp_rows=", nrow(obj$pbp_season),
    " | schedule_rows=", nrow(obj$schedule_df)
  )
}

# ---- TODAY unit ----
# Create a consistent string key for today's date to index the live (non-backtest) unit.
today_key <- format(today_dt, "%Y%m%d")

# Build the full historical PBP training set for the live run, including all games played prior to today.
today_pbp_train_all_df <- pbp_df %>%
  dplyr::filter(!is.na(game_date_dt), game_date_dt < today_dt)

# Build the season-to-date PBP training set for the live run, restricting data to the current season and excluding today’s games.
today_pbp_season_df <- pbp_df %>%
  dplyr::filter(!is.na(game_date_dt),
                game_date_dt >= season_start_dt,
                game_date_dt <  today_dt)

# Prepare team-level training data for the live run by filtering BaseStats_Team_MC to games played prior to today (if a game_date exists).
today_base_team_train_df <- BaseStats_Team_MC
if ("game_date" %in% names(BaseStats_Team_MC)) {
  today_base_team_train_df <- BaseStats_Team_MC %>%
    dplyr::mutate(game_date_dt = suppressWarnings(as.Date(game_date))) %>%
    dplyr::filter(is.na(game_date_dt) | game_date_dt < today_dt) %>%
    dplyr::select(-game_date_dt)
}

# Retrieve today’s scheduled games and attach date metadata for consistent downstream indexing and processing.
today_schedule_df <- get_schedule_for_date(today_dt) %>%
  dplyr::mutate(date_key = today_key, test_date = today_dt)

# Build rolling per-team PBP windows and season-to-date PBP for the live run, then bundle all required inputs into a single "today" unit for MC simulation.
today_roll_list <- purrr::map(rolling_n_vec, function(n) {
  build_roll_pbp(today_pbp_train_all_df, n_games = n)
})
names(today_roll_list) <- paste0("roll_", rolling_n_vec)
today_roll_list$season <- today_pbp_season_df

today_unit <- list(
  date_key        = today_key,
  test_date       = today_dt,
  schedule_df     = today_schedule_df,
  pbp_train_all   = today_pbp_train_all_df,
  pbp_season      = today_pbp_season_df,
  base_team_train = today_base_team_train_df,
  rolling_pbps    = today_roll_list
)

message(
  "Built TODAY unit: ", today_key,
  " | train_pbp_rows=", nrow(today_pbp_train_all_df),
  " | season_pbp_rows=", nrow(today_pbp_season_df),
  " | schedule_rows=", nrow(today_schedule_df)
)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 1. END: Splits (UNITS only; no bt_* materialization)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



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
slate_games <- nba_schedule_season_clean %>%
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
# REBOUNDING PROFILE SECTION: Build quarter-level rebounding profiles from BaseStats_Team_MC
#   Outputs:
#     - team_reb_profile_q (team_id x qtr):
#         REB_mean, REB_sd
#         OREB_mean, OREB_sd
#         DREB_mean, DREB_sd
#         OREB_share_mean  (OREB / REB)  [optional but useful]
#
# Notes:
#   - Uses T_REB_Qx / T_OREB_Qx / T_DREB_Qx when present
#   - Else falls back to CGS / 4 (and forces Q5/Q6=0 for CGS-only)
#   - All values are numeric; SDs NA -> 0
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# ===============================================================#
# This section builds quarter-level team rebounding profiles from BaseStats_Team_MC, producing stable mean and variance estimates
# for total rebounds (REB), offensive rebounds (OREB), and defensive rebounds (DREB).
#
# The logic first standardizes team identifiers, then pulls quarter-specific rebounding stats when available. If quarter
# splits are missing, it safely falls back to CGS totals divided evenly across Q1–Q4, with Q5/Q6 explicitly forced to zero to avoid
# overtime leakage.
#
# Rebounding data is reshaped into a long format and aggregated by team, season, and quarter to compute empirical means and standard
# deviations. Any NA standard deviations are coerced to zero to ensure numerical stability in downstream simulations.
#
# An optional OREB_share_mean (OREB / REB) is also computed, which is not required for base scoring but provides a clean input for
# second-chance or continuation models without contaminating the core possessions or points engine.
# ===============================================================
# ============================================================
# Build team_reb_profile_q with NO leakage + 7 windows
# ============================================================


has_col <- function(df, nm) nm %in% names(df)
clamp01 <- function(x) pmin(pmax(x, 0), 1)

parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

get_q_or_cgs_div4 <- function(df, q_col, cgs_col, q) {
  if (has_col(df, q_col)) return(suppressWarnings(as.numeric(df[[q_col]])))
  if (has_col(df, cgs_col)) {
    x <- suppressWarnings(as.numeric(df[[cgs_col]])) / 4
    if (q >= 5) x <- 0
    return(x)
  }
  rep(NA_real_, nrow(df))
}

build_team_reb_profile_q_windows <- function(BaseStats_Team_MC,
                                             season_token,
                                             slate_dt,
                                             windows = c(5L,7L,10L,15L,20L,25L,35L),
                                             qtrs = 1:6) {
  
  stopifnot("GAME_DATE" %in% names(BaseStats_Team_MC))
  stopifnot("ESPN_TEAM_ID" %in% names(BaseStats_Team_MC))
  
  df <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      season_token = as.character(season_token),
      game_date_dt = parse_game_date(GAME_DATE)
    ) %>%
    filter(!is.na(team_id), team_id != "", !is.na(game_date_dt))
  
  slate_dt <- as.Date(slate_dt)
  
  # ---- NO LEAKAGE ----
  df <- df %>% filter(game_date_dt < slate_dt)
  
  # ---- build long reb table ----
  long_list <- lapply(qtrs, function(q) {
    data.frame(
      team_id      = df$team_id,
      season_token = df$season_token,
      game_date_dt = df$game_date_dt,
      qtr          = as.integer(q),
      REB_raw  = get_q_or_cgs_div4(df, paste0("T_REB_Q",  q), "T_REB_CGS",  q),
      OREB_raw = get_q_or_cgs_div4(df, paste0("T_OREB_Q", q), "T_OREB_CGS", q),
      DREB_raw = get_q_or_cgs_div4(df, paste0("T_DREB_Q", q), "T_DREB_CGS", q),
      stringsAsFactors = FALSE
    )
  })
  
  long_df <- bind_rows(long_list) %>%
    filter(!(is.na(REB_raw) & is.na(OREB_raw) & is.na(DREB_raw)))
  
  dt <- as.data.table(long_df)
  setorder(dt, team_id, qtr, game_date_dt)
  
  # ---- window aggregation ----
  out <- rbindlist(lapply(windows, function(N) {
    dt[, {
      r  <- REB_raw [!is.na(REB_raw)]
      or <- OREB_raw[!is.na(OREB_raw)]
      dr <- DREB_raw[!is.na(DREB_raw)]
      
      rN  <- tail(r,  min(N, length(r)))
      orN <- tail(or, min(N, length(or)))
      drN <- tail(dr, min(N, length(dr)))
      
      REB_mean  <- if (length(rN)  > 0) mean(rN)  else NA_real_
      OREB_mean <- if (length(orN) > 0) mean(orN) else NA_real_
      DREB_mean <- if (length(drN) > 0) mean(drN) else NA_real_
      
      REB_sd  <- if (length(rN)  >= 2) sd(rN)  else 0
      OREB_sd <- if (length(orN) >= 2) sd(orN) else 0
      DREB_sd <- if (length(drN) >= 2) sd(drN) else 0
      
      OREB_share_mean <- if (!is.na(REB_mean) && REB_mean > 0) {
        clamp01(OREB_mean / REB_mean)
      } else {
        NA_real_
      }
      
      list(
        REB_mean,  REB_sd,
        OREB_mean, OREB_sd,
        DREB_mean, DREB_sd,
        OREB_share_mean
      )
    }, by = .(team_id, season_token, qtr)][, window_n := as.integer(N)][]
  }), fill = TRUE)
  
  as_tibble(out) %>%
    arrange(window_n, team_id, qtr)
}

# ============================================================
# RUN
# ============================================================

team_reb_profile_q <- build_team_reb_profile_q_windows(
  BaseStats_Team_MC = BaseStats_Team_MC,
  season_token      = season_token,
  slate_dt          = slate_dt
)

print(team_reb_profile_q %>% slice(1:20))

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: Rebounding profiles
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



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
# START: OPTION A: Build team_to_profile_q from BaseStats_Team_MC
#   Output columns (required by TO engine / PTS engine):
#     team_id, season_token, qtr,
#     TOV_pct, TOV_live_share
#
# Notes:
# - TOV_pct is a per-possession turnover probability (decimal 0–1). We’ll accept % (0–100) and convert.
# - TOV_live_share is the share of turnovers that are live-ball (decimal 0–1).
# - We keep TO_per_poss as a backward-compatible alias if any older code still references it.
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

# ============================================================
# Build team_to_profile_q with NO leakage + 7 windows
# ============================================================

# ---- helpers ----
clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- ifelse(is.na(x), default, x)
  x <- ifelse(x > 1 & x <= 100, x / 100, x)
  clamp01(x)
}

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

# ============================================================
# Main builder
# ============================================================

build_team_to_profile_q_windows <- function(BaseStats_Team_MC,
                                            season_token,
                                            slate_dt,
                                            windows = c(5L,7L,10L,15L,20L,25L,35L),
                                            qtrs = 1:6) {
  
  stopifnot("GAME_DATE" %in% names(BaseStats_Team_MC))
  stopifnot("ESPN_TEAM_ID" %in% names(BaseStats_Team_MC))
  
  df <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      season_token = as.character(season_token),
      game_date_dt = parse_game_date(GAME_DATE)
    ) %>%
    filter(!is.na(team_id), team_id != "", !is.na(game_date_dt))
  
  slate_dt <- as.Date(slate_dt)
  
  # ---- NO LEAKAGE ----
  df <- df %>% filter(game_date_dt < slate_dt)
  
  # ---- build long per-game TO observations ----
  long_list <- lapply(qtrs, function(q) {
    
    # ----- TOV_pct (per-poss) -----
    tov_pct_raw <- NA_real_
    
    if (paste0("TOV_pct_Q", q) %in% names(df)) {
      tov_pct_raw <- get_num_col(df, paste0("TOV_pct_Q", q))
    } else if (paste0("T_TOV_PCT_Q", q) %in% names(df)) {
      tov_pct_raw <- get_num_col(df, paste0("T_TOV_PCT_Q", q))
    } else if (paste0("T_TO_PER_POSS_Q", q) %in% names(df)) {
      tov_pct_raw <- get_num_col(df, paste0("T_TO_PER_POSS_Q", q))
    } else if (all(c(paste0("T_TOV_Q", q), paste0("T_POSS_Q", q)) %in% names(df))) {
      tov  <- get_num_col(df, paste0("T_TOV_Q", q))
      poss <- get_num_col(df, paste0("T_POSS_Q", q))
      tov_pct_raw <- ifelse(!is.na(poss) & poss > 0, tov / poss, NA_real_)
    } else if (all(c(paste0("T_TOV_Q", q), paste0("T_PACE_Q", q)) %in% names(df))) {
      tov  <- get_num_col(df, paste0("T_TOV_Q", q))
      poss <- get_num_col(df, paste0("T_PACE_Q", q))
      tov_pct_raw <- ifelse(!is.na(poss) & poss > 0, tov / poss, NA_real_)
    }
    
    tov_pct_raw <- pct01(tov_pct_raw, default = NA_real_)
    tov_pct_raw <- pmin(pmax(tov_pct_raw, 0), 0.35)
    
    # ----- live-ball share -----
    live_raw <- NA_real_
    
    if (paste0("TOV_live_share_Q", q) %in% names(df)) {
      live_raw <- get_num_col(df, paste0("TOV_live_share_Q", q))
    } else if (paste0("T_TOV_LIVE_SHARE_Q", q) %in% names(df)) {
      live_raw <- get_num_col(df, paste0("T_TOV_LIVE_SHARE_Q", q))
    } else if (all(c(paste0("T_LIVE_TOV_Q", q), paste0("T_TOV_Q", q)) %in% names(df))) {
      live_tov <- get_num_col(df, paste0("T_LIVE_TOV_Q", q))
      tov      <- get_num_col(df, paste0("T_TOV_Q", q))
      live_raw <- ifelse(!is.na(tov) & tov > 0, live_tov / tov, NA_real_)
    }
    
    live_raw <- pct01(live_raw, default = NA_real_)
    
    data.frame(
      team_id      = df$team_id,
      season_token = df$season_token,
      game_date_dt = df$game_date_dt,
      qtr          = as.integer(q),
      TOV_pct_raw  = tov_pct_raw,
      LIVE_raw     = live_raw,
      stringsAsFactors = FALSE
    )
  })
  
  long_df <- bind_rows(long_list)
  
  dt <- as.data.table(long_df)
  setorder(dt, team_id, qtr, game_date_dt)
  
  # ---- window aggregation ----
  out <- rbindlist(lapply(windows, function(N) {
    dt[, {
      tov_v  <- TOV_pct_raw[!is.na(TOV_pct_raw)]
      live_v <- LIVE_raw    [!is.na(LIVE_raw)]
      
      tov_N  <- tail(tov_v,  min(N, length(tov_v)))
      live_N <- tail(live_v, min(N, length(live_v)))
      
      TOV_pct        <- if (length(tov_N)  > 0) mean(tov_N)  else 0.13
      TOV_live_share <- if (length(live_N) > 0) mean(live_N) else 0.60
      
      list(
        TOV_pct        = clamp01(pmin(TOV_pct, 0.35)),
        TOV_live_share = clamp01(TOV_live_share),
        TO_per_poss    = clamp01(pmin(TOV_pct, 0.35))
      )
    }, by = .(team_id, season_token, qtr)][, window_n := as.integer(N)][]
  }), fill = TRUE)
  
  as_tibble(out) %>%
    arrange(window_n, team_id, qtr)
}

# ============================================================
# RUN
# ============================================================

team_to_profile_q <- build_team_to_profile_q_windows(
  BaseStats_Team_MC = BaseStats_Team_MC,
  season_token      = season_token,
  slate_dt          = slate_dt
)

print(team_to_profile_q %>% slice(1:20))


# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: OPTION A: Build team_to_profile_q from BaseStats_Team_MC
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠#
#
#
#     oooooooooooo                                   ooooooooooooo oooo                                                          8      oooooooooooo                       oooo           
#     `888'     `8                                   8'   888   `8 `888                                                          8      `888'     `8                       `888           
#      888         oooo d8b  .ooooo.   .ooooo.            888       888 .oo.   oooo d8b  .ooooo.  oooo oooo    ooo  .oooo.o      8       888          .ooooo.  oooo  oooo   888   .oooo.o 
#      888oooo8    `888""8P d88' `88b d88' `88b           888       888P"Y88b  `888""8P d88' `88b  `88. `88.  .8'  d88(  "8              888oooo8    d88' `88b `888  `888   888  d88(  "8 
#      888    "     888     888ooo888 888ooo888           888       888   888   888     888   888   `88..]88..8'   `"Y88b.       8       888    "    888   888  888   888   888  `"Y88b.  
#      888          888     888    .o 888    .o           888       888   888   888     888   888    `888'`888'    o.  )88b      8       888         888   888  888   888   888  o.  )88b 
#     o888o        d888b    `Y8bod8P' `Y8bod8P'          o888o     o888o o888o d888b    `Y8bod8P'     `8'  `8'     8""888P'      8      o888o        `Y8bod8P'  `V88V"V8P' o888o 8""888P' 
#                                                                                                                                                                                    
#                                                                                                                                                                                    
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠#                                                  
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠                                                  



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ================================================================================================
# FT% ONLY HOOK (NO DOUBLE COUNT)
#   - Use team_ft_profile_q ONLY to provide FT% by quarter (optionally shrunk/jittered)
#   - Keep ALL FTA generation inside your PTS engine (p_shoot_foul + and-1 logic)
#   - Do NOT compute baseline FTA from FTR * FGA anywhere if you use this
# ================================================================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

get_team_ft_pct_q <- function(team_id, season_token, qtr, window_n,
                              team_ft_profile_q,
                              default_ft = 0.78) {
  
  r <- team_ft_profile_q %>%
    dplyr::filter(
      team_id      == !!team_id,
      season_token == !!season_token,
      qtr          == !!qtr,
      window_n     == !!window_n
    ) %>%
    dplyr::slice(1)
  
  if (nrow(r) == 0) return(clamp01(default_ft))
  
  clamp01(num0(r$FT_pct_mean, default_ft))
}


# Use this inside derive_team_probs_q (or right after you call it) to override FT_pct ONLY.
# Example: inside derive_team_probs_q, after pts_in is created:
#
#   FT_pct <- get_team_ft_pct_q(team_id, season_token, qtr, team_ft_profile_q, default_ft = mc_anchor$ft)
#   FT_pct <- shrink_to(FT_pct, mc_anchor$ft, mc_var$shoot_shrink)
#   FT_pct <- apply_pct_jitter(FT_pct, mc_var$pct_jitter_sd)
#
# Then return FT_pct as usual.
#
# IMPORTANT:
#   - Do NOT compute FTA_base from FTR * FGA anywhere if you do this.
#   - Your PTS engine already produces FTA/FTM consistently.

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: FT% ONLY HOOK
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# START: OPTION A: Build team_foul_profile_q from BaseStats_Team_MC
#   Output columns (required by PTS engine):
#     team_id, season_token, qtr,
#     PF_per_poss
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# This block builds a quarter-level team foul profile (`team_foul_profile_q`) that estimates how often a team commits
# offensive fouls per possession, which are later treated as possession-ending events in the PTS engine.
# 
# For each quarter (Q1–Q6), the logic attempts to construct a foul rate using the most direct and reliable data available,
# preferring explicit per-possession foul rates when present. If those are unavailable, it progressively falls back to
# deriving rates from raw foul counts divided by possessions, then pace-based proxies, and finally a conservative proxy
# derived from free throw rate (FTR) when no direct foul information exists.
#
# All derived foul rates are stabilized with a reasonable default (~0.19 fouls per possession) and clamped to a safe
# upper bound to prevent unrealistic foul volumes in Monte Carlo simulations.
#
# The resulting profile does NOT simulate fouls on its own; instead, it acts as a rate table consumed by downstream
# logic (e.g., `draw_off_fouls_q()` inside the PTS engine) to determine how many possessions are lost to offensive fouls
# before shot attempts are generated.
#
# Output columns:
# - team_id, season_token, qtr: identifiers
# - PF_per_poss: estimated offensive fouls committed per possession for the given quarter
# ============================================================
# Build team_foul_profile_q with NO leakage + 7 windows
# ============================================================

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

# ============================================================
# Main builder
# ============================================================

build_team_foul_profile_q_windows <- function(BaseStats_Team_MC,
                                              season_token,
                                              slate_dt,
                                              windows = c(5L,7L,10L,15L,20L,25L,35L),
                                              qtrs = 1:6) {
  
  stopifnot("GAME_DATE" %in% names(BaseStats_Team_MC))
  stopifnot("ESPN_TEAM_ID" %in% names(BaseStats_Team_MC))
  
  df <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      season_token = as.character(season_token),
      game_date_dt = parse_game_date(GAME_DATE)
    ) %>%
    filter(!is.na(team_id), team_id != "", !is.na(game_date_dt))
  
  slate_dt <- as.Date(slate_dt)
  
  # ---- NO LEAKAGE ----
  df <- df %>% filter(game_date_dt < slate_dt)
  
  # ---- build long per-game foul observations ----
  long_list <- lapply(qtrs, function(q) {
    
    pf_rate <- NA_real_
    
    if (paste0("T_PF_PER_POSS_Q", q) %in% names(df)) {
      pf_rate <- get_num_col(df, paste0("T_PF_PER_POSS_Q", q))
    } else if (all(c(paste0("T_PF_Q", q), paste0("T_POSS_Q", q)) %in% names(df))) {
      pf   <- get_num_col(df, paste0("T_PF_Q", q))
      poss <- get_num_col(df, paste0("T_POSS_Q", q))
      pf_rate <- ifelse(!is.na(poss) & poss > 0, pf / poss, NA_real_)
    } else if (all(c(paste0("T_PF_Q", q), paste0("T_PACE_Q", q)) %in% names(df))) {
      pf   <- get_num_col(df, paste0("T_PF_Q", q))
      poss <- get_num_col(df, paste0("T_PACE_Q", q))
      pf_rate <- ifelse(!is.na(poss) & poss > 0, pf / poss, NA_real_)
    } else if (paste0("T_FTR_Q", q) %in% names(df)) {
      ftr <- get_num_col(df, paste0("T_FTR_Q", q))
      pf_rate <- ifelse(is.na(ftr), NA_real_, pmax(0, ftr) * 0.35)
    }
    
    data.frame(
      team_id      = df$team_id,
      season_token = df$season_token,
      game_date_dt = df$game_date_dt,
      qtr          = as.integer(q),
      PF_raw       = suppressWarnings(as.numeric(pf_rate)),
      stringsAsFactors = FALSE
    )
  })
  
  long_df <- bind_rows(long_list)
  
  dt <- as.data.table(long_df)
  setorder(dt, team_id, qtr, game_date_dt)
  
  # ---- window aggregation ----
  out <- rbindlist(lapply(windows, function(N) {
    dt[, {
      v <- PF_raw[!is.na(PF_raw)]
      vN <- tail(v, min(N, length(v)))
      
      PF_per_poss <- if (length(vN) > 0) mean(vN) else 0.19
      
      PF_per_poss <- pmin(pmax(PF_per_poss, 0), 0.45)
      
      list(PF_per_poss = PF_per_poss)
    }, by = .(team_id, season_token, qtr)][, window_n := as.integer(N)][]
  }), fill = TRUE)
  
  as_tibble(out) %>%
    arrange(window_n, team_id, qtr)
}

# ============================================================
# RUN
# ============================================================

team_foul_profile_q <- build_team_foul_profile_q_windows(
  BaseStats_Team_MC = BaseStats_Team_MC,
  season_token      = season_token,
  slate_dt          = slate_dt
)

print(team_foul_profile_q %>% slice(1:20))

stopifnot(
  all(c("team_id","season_token","qtr","window_n","PF_per_poss") %in% names(team_foul_profile_q))
)


# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: OPTION A: Build team_foul_profile_q from BaseStats_Team_MC
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
clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  clamp01(x)
}

# Numeric safety helper.
# num0() coerces inputs to numeric and replaces NA values with a specified default to prevent NA propagation in simulations.
num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

# Probability stabilization helper.
# shrink_to() blends a noisy probability toward a stable anchor using weight w (0 = fully team-driven, 1 = fully anchor-driven) and clamps the result to [0,1].
shrink_to <- function(p, anchor, w) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) p <- anchor
  w <- clamp01(w)
  clamp01((1 - w) * p + w * anchor)
}

# Defines league-average anchor values used to stabilize Monte Carlo probabilities.
# MC_ANCHOR provides safe fallback baselines (e.g., turnover rate) when team-level inputs are missing or intentionally shrunk toward league norms.
if (!exists("MC_ANCHOR")) {
  MC_ANCHOR <- list(to = 0.13)
}

# ---------------------------------------------------------------
# 1) Pull team turnover inputs for quarter
# ---------------------------------------------------------------
# Retrieves quarter-specific turnover inputs for a team from the turnover profile.
# Returns a stabilized turnover probability (p_to_team) and live-ball turnover share, supporting both percent and decimal formats.
# If no team/quarter row exists, the function safely falls back to league anchor values to keep the simulation stable.
# All outputs are clamped to valid probability bounds to prevent invalid Monte Carlo draws.

get_team_tov_inputs_q <- function(team_id,
                                  qtr,
                                  team_to_profile_q,
                                  season_token = NULL,
                                  window_n = NULL,
                                  mc_anchor = MC_ANCHOR) {
  
  r <- pick_profile_row(
    df           = team_to_profile_q,
    team_id      = team_id,
    season_token = season_token,
    qtr          = qtr,
    window_n     = window_n
  )
  
  if (nrow(r) == 0) {
    return(list(
      p_to_team  = clamp01(num0(mc_anchor$to, 0.13)),
      live_share = 0.60
    ))
  }
  
  list(
    # allow % or decimal
    p_to_team  = pct01(
      r$TOV_pct[1],
      default = clamp01(num0(mc_anchor$to, 0.13))
    ),
    
    # share of turnovers that are live-ball
    live_share = clamp01(num0(r$TOV_live_share[1], 0.60))
  )
}


# ---------------------------------------------------------------
# 2) Derive final TO probabilities (blend + bounds)
# ---------------------------------------------------------------
# Builds the final quarter-level turnover probabilities used by the simulation engine.
# Starts from team-specific turnover inputs and optionally blends them toward a league anchor using MC_VAR$tov_rand.
# This shrinkage controls volatility, allowing you to dial between pure team signal and stable league behavior.
# Outputs a clean, bounded turnover probability and live-ball turnover share for downstream simulation.
derive_team_tov_probs_q <- function(team_id,
                                    qtr,
                                    team_to_profile_q,
                                    season_token = NULL,
                                    window_n = NULL,
                                    mc_var    = MC_VAR,
                                    mc_anchor = MC_ANCHOR) {
  
  tov_in <- get_team_tov_inputs_q(
    team_id = team_id,
    qtr     = qtr,
    team_to_profile_q = team_to_profile_q,
    season_token = season_token,
    window_n     = window_n,
    mc_anchor    = mc_anchor
  )
  
  p_to_team  <- clamp01(tov_in$p_to_team)
  live_share <- clamp01(tov_in$live_share)
  
  # ---- BLEND TO% WITH ANCHOR (MC_VAR$tov_rand) ----
  # 0.0 = full team TO%, 1.0 = full anchor TO%
  tov_rand <- clamp01(num0(mc_var$tov_rand, 0))
  p_to <- shrink_to(
    p_to_team,
    clamp01(num0(mc_anchor$to, 0.13)),
    tov_rand
  )
  
  list(
    p_to       = clamp01(p_to),
    live_share = clamp01(live_share)
  )
}


# ---------------------------------------------------------------
# 3) Simulate turnovers for a quarter (given poss)
# ---------------------------------------------------------------
# Simulates quarter-level turnovers for a team given a fixed number of possessions.
# Uses the derived turnover probability and live-ball share to draw total turnovers, then splits them into live and dead-ball events.
# Ensures all outputs are integers and safely handles edge cases like zero or missing possessions.
# This converts abstract turnover rates into concrete possession losses consumed by the points engine.
# Also returns the exact probabilities used, which is useful for debugging, calibration, and variance control.
simulate_team_turnovers_q <- function(team_id, poss, qtr,
                                      team_to_profile_q,
                                      season_token = NULL,
                                      window_n = NULL,
                                      mc_var    = MC_VAR,
                                      mc_anchor = MC_ANCHOR) {
  
  poss <- as.integer(poss)
  if (is.na(poss) || poss <= 0) {
    return(list(
      poss_total      = as.integer(pmax(poss, 0)),
      turnovers       = 0L,
      live_turnovers  = 0L,
      dead_turnovers  = 0L,
      p_to_used       = 0,
      live_share_used = 0
    ))
  }
  
  probs <- derive_team_tov_probs_q(
    team_id = team_id,
    qtr = qtr,
    team_to_profile_q = team_to_profile_q,
    season_token = season_token,
    window_n = window_n,
    mc_var = mc_var,
    mc_anchor = mc_anchor
  )
  
  p_to       <- probs$p_to
  live_share <- probs$live_share
  
  total_tov <- if (p_to > 0) rbinom(1, size = poss, prob = p_to) else 0L
  total_tov <- as.integer(total_tov)
  
  live_tov <- if (total_tov > 0) rbinom(1, size = total_tov, prob = live_share) else 0L
  live_tov <- as.integer(live_tov)
  
  dead_tov <- as.integer(pmax(total_tov - live_tov, 0L))
  
  list(
    poss_total      = as.integer(poss),
    turnovers       = total_tov,
    live_turnovers  = live_tov,
    dead_turnovers  = dead_tov,
    p_to_used       = p_to,
    live_share_used = live_share
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
# START: Build team_pts_profile_q from BaseStats_Team_MC
#   Purpose (v_4):
#     - Quarter-level shooting + foul inputs for PTS engine
#     - One row per team_id × season_token × qtr
#
#   Required output columns:
#     team_id, season_token, qtr,
#     p2_given_shot, FG2_pct, FG3_pct, FT_pct, FTA_per_FGA
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

# Safely extracts a numeric column from a data frame.
# If the column does not exist, returns an NA vector of matching length instead of erroring.
get_num_col <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

# Build a quarter-level team shooting + free-throw profile table from BaseStats_Team_MC.
# Output is one row per team_id × season_token × quarter (Q1–Q6, where Q5/Q6 are OT slots),
# and it supplies the core efficiency inputs consumed by the PTS simulation engine.
#
# What this produces (per team per quarter):
# - FG2_pct: 2PT field goal percentage (bounded to [0,1], default 0.50 if missing)
# - FG3_pct: 3PT field goal percentage (bounded to [0,1], default 0.36 if missing)
# - FT_pct:  free throw percentage (bounded to [0,1], default 0.78 if missing)
# - p2_given_shot: probability a shot attempt is a 2PT (vs 3PT), derived from 3PA rate if available
# - FTA_per_FGA: free throw pressure proxy derived from free-throw rate (FTR), with a safe default if missing
#
# How it works:
# 1) Loop quarters q = 1..6 and pull quarter-specific shooting inputs from BaseStats_Team_MC using consistent naming:
#    - T_2PT_PCT_Q{q}, T_3PT_PCT_Q{q}, T_FT_PCT_Q{q}
#    - T_FTR_Q{q} as a proxy for “FTA per FGA” pressure
#
# 2) Determine 3PA rate (three_par) to build shot mix:
#    - Prefer a direct quarter-level 3PA rate column if present (T_3PAR_Q{q})
#    - Otherwise, derive it from raw attempts if available: three_par = T_3PA_Q{q} / T_FGA_Q{q}
#    - If neither exists, leave as NA and rely on defaults
#
# 3) Construct the profile row:
#    - team_id comes from ESPN_TEAM_ID (this must match the IDs used in your sim functions)
#    - season_token is stamped in (so multi-season support is easy later)
#    - qtr is the quarter index
#
# 4) Apply stability guardrails:
#    - clamp01() forces all % values to valid probability space [0,1]
#    - ifelse(is.na(...), default, ...) provides safe league-ish defaults when stats are missing
#    - p2_given_shot is computed as 1 - 3PA_rate, with a fallback of 0.65 when 3PA_rate is unknown
#    - FTA_per_FGA falls back to 0.22 if missing and is constrained to be non-negative
#
# 5) Clean + validate:
#    - drop missing/blank team_id rows
#    - print the first few rows for a quick visual sanity check
#    - stopifnot() asserts the table is non-empty and contains all required columns for the PTS engine
# ============================================================
# Build team_pts_profile_q with NO leakage + 7 windows
# ============================================================


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

build_team_pts_profile_q_windows <- function(BaseStats_Team_MC,
                                             season_token,
                                             slate_dt,
                                             windows = c(5L,7L,10L,15L,20L,25L,35L),
                                             qtrs = 1:6) {
  
  df <- BaseStats_Team_MC %>%
    mutate(
      team_id      = as.character(ESPN_TEAM_ID),
      season_token = as.character(season_token),
      game_date_dt = parse_game_date(GAME_DATE)
    ) %>%
    filter(!is.na(team_id), team_id != "", !is.na(game_date_dt)) %>%
    filter(game_date_dt < as.Date(slate_dt))   # NO LEAKAGE
  
  # ---- per-game long table ----
  long_list <- lapply(qtrs, function(q) {
    
    fg2 <- get_num_col(df, paste0("T_2PT_PCT_Q", q))
    fg3 <- get_num_col(df, paste0("T_3PT_PCT_Q", q))
    ft  <- get_num_col(df, paste0("T_FT_PCT_Q",  q))
    ftr <- get_num_col(df, paste0("T_FTR_Q", q))
    
    three_par <- NA_real_
    if (paste0("T_3PAR_Q", q) %in% names(df)) {
      three_par <- get_num_col(df, paste0("T_3PAR_Q", q))
    } else if (all(c(paste0("T_3PA_Q", q), paste0("T_FGA_Q", q)) %in% names(df))) {
      three_pa <- get_num_col(df, paste0("T_3PA_Q", q))
      fga      <- get_num_col(df, paste0("T_FGA_Q", q))
      three_par <- ifelse(!is.na(fga) & fga > 0, three_pa / fga, NA_real_)
    }
    
    data.frame(
      team_id      = df$team_id,
      season_token = df$season_token,
      game_date_dt = df$game_date_dt,
      qtr          = as.integer(q),
      
      FG2_pct_raw = fg2,
      FG3_pct_raw = fg3,
      FT_pct_raw  = ft,
      p3_raw      = three_par,
      FTR_raw     = ftr,
      stringsAsFactors = FALSE
    )
  })
  
  long_df <- bind_rows(long_list)
  dt <- as.data.table(long_df)
  setorder(dt, team_id, qtr, game_date_dt)
  
  # ---- window aggregation ----
  out <- rbindlist(lapply(windows, function(N) {
    
    dt[, {
      v <- tail(.SD, min(.N, N))
      
      FG2_pct <- clamp01(mean(v$FG2_pct_raw, na.rm = TRUE))
      FG3_pct <- clamp01(mean(v$FG3_pct_raw, na.rm = TRUE))
      FT_pct  <- clamp01(mean(v$FT_pct_raw,  na.rm = TRUE))
      
      p3 <- mean(v$p3_raw, na.rm = TRUE)
      p2_given_shot <- clamp01(ifelse(is.na(p3), 0.65, 1 - p3))
      
      FTA_per_FGA <- mean(v$FTR_raw, na.rm = TRUE)
      if (is.na(FTA_per_FGA)) FTA_per_FGA <- 0.22
      FTA_per_FGA <- pmax(0, FTA_per_FGA)
      
      list(
        FG2_pct        = FG2_pct,
        FG3_pct        = FG3_pct,
        FT_pct         = FT_pct,
        p2_given_shot  = p2_given_shot,
        FTA_per_FGA    = FTA_per_FGA
      )
      
    }, by = .(team_id, season_token, qtr)][, window_n := as.integer(N)][]
    
  }), fill = TRUE)
  
  as_tibble(out) %>%
    arrange(window_n, team_id, qtr)
}

# ============================================================
# RUN
# ============================================================

team_pts_profile_q <- build_team_pts_profile_q_windows(
  BaseStats_Team_MC = BaseStats_Team_MC,
  season_token      = season_token,
  slate_dt          = slate_dt
)

print(team_pts_profile_q %>% slice(1:20))

stopifnot(
  all(c(
    "team_id","season_token","qtr","window_n",
    "p2_given_shot","FG2_pct","FG3_pct","FT_pct","FTA_per_FGA"
  ) %in% names(team_pts_profile_q))
)



# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# END: Build team_pts_profile_q from BaseStats_Team_MCbuild 
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ===============================================================
# PTS ENGINE UPGRADE (uses team_pts_profile_q as inputs)
#   + Adds: shooting shrink + pct jitter (from MC_VAR)
#   + Uses team_shoot_foul_profile_q when available (else FTA/FGA proxy)
#   + Adds TO randomness blend via MC_VAR$tov_rand (optional, stable)
# ===============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# Clamp numeric inputs to the valid probability range [0, 1] to prevent invalid Monte Carlo draws.
# pct01() additionally converts 0–100 percentages to proportions, applies a fallback default for NA values, and enforces bounds.
clamp01 <- function(x) pmin(pmax(x, 0), 1)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  clamp01(x)
}

# Safely coerce inputs to numeric and replace NA values with a specified default.
# Used throughout the MC engine to prevent NA propagation during random draws and arithmetic.
num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

# Shrinks a probability toward a stable anchor value using weight w (0 = no shrink, 1 = full anchor).
# This stabilizes noisy team estimates while retaining controllable team-specific signal.
shrink_to <- function(p, anchor, w) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) p <- anchor
  w <- clamp01(w)
  clamp01((1 - w) * p + w * anchor)
}

# Defines league-average anchor probabilities used as stabilization targets in Monte Carlo simulations.
# These anchors prevent extreme or missing team inputs from producing unrealistic outcomes.
if (!exists("MC_ANCHOR")) {
  MC_ANCHOR <- list(fg2 = 0.52, fg3 = 0.36, ft = 0.78, to = 0.13, shoot_foul = 0.10)
}

# Apply optional Monte Carlo noise to a probability after shrinkage.
# Adds controlled randomness around the mean (p) using a normal draw,
# while safely clamping the result to [0,1] to prevent invalid probabilities.
if (!exists("apply_pct_jitter")) {
  apply_pct_jitter <- function(p, jitter_sd) {
    p <- suppressWarnings(as.numeric(p))
    if (is.na(p)) return(NA_real_)
    if (is.na(jitter_sd) || jitter_sd <= 0) return(clamp01(p))
    clamp01(rnorm(1, mean = p, sd = jitter_sd))
  }
}

# Extracts quarter-specific offensive scoring inputs for a team from team_pts_profile_q.
# These values describe shot mix (2PT vs 3PT), shooting efficiency, and free-throw rate,
# and are the core efficiency inputs for the points simulation engine.
#
# If no profile row exists for the team/quarter, league-average fallback values are returned
# to keep the simulation stable and prevent missing-profile failures.
#
# All probability-like values are clamped to valid ranges to avoid invalid or explosive outcomes.
get_team_pts_inputs_q <- function(team_id, qtr,
                                  team_pts_profile_q,
                                  season_token = NULL,
                                  window_n = NULL) {
  
  r <- team_pts_profile_q %>%
    dplyr::filter(team_id == !!team_id, qtr == !!qtr)
  
  # optional season filter
  if (!is.null(season_token) && "season_token" %in% names(r)) {
    r <- r %>% dplyr::filter(season_token == !!season_token)
  }
  
  # optional window filter
  if (!is.null(window_n) && "window_n" %in% names(r)) {
    r <- r %>% dplyr::filter(window_n == !!window_n)
  }
  
  r <- r %>% dplyr::slice(1)
  
  if (nrow(r) == 0) {
    return(list(
      p2_given_shot = 0.65,
      FG2_pct = 0.50,
      FG3_pct = 0.36,
      FT_pct  = 0.78,
      FTA_per_FGA = 0.22
    ))
  }
  
  list(
    p2_given_shot = clamp01(r$p2_given_shot[1]),
    FG2_pct       = clamp01(r$FG2_pct[1]),
    FG3_pct       = clamp01(r$FG3_pct[1]),
    FT_pct        = clamp01(r$FT_pct[1]),
    FTA_per_FGA   = pmax(0, suppressWarnings(as.numeric(r$FTA_per_FGA[1])))
  )
}


# Retrieves the quarter-specific probability that a shot attempt results in a shooting foul,
# using a pre-built team foul profile when available.
#
# Supports multiple column naming conventions and safely returns NA when no reliable estimate
# exists, allowing the points engine to fall back to FTA-based proxies instead of failing.
get_team_shoot_foul_p_q <- function(team_id,
                                    season_token,
                                    qtr,
                                    team_foul_profile_q = NULL,
                                    window_n = NULL) {
  
  if (is.null(team_foul_profile_q) || nrow(team_foul_profile_q) == 0) {
    return(NA_real_)
  }
  
  r <- team_foul_profile_q %>%
    dplyr::filter(
      team_id == !!team_id,
      season_token == !!season_token,
      qtr == !!qtr
    )
  
  if (!is.null(window_n) && "window_n" %in% names(r)) {
    r <- r %>% dplyr::filter(window_n == !!window_n)
  }
  
  r <- r %>% dplyr::slice(1)
  
  if (nrow(r) == 0) return(NA_real_)
  
  if ("P_SHOOT_FOUL" %in% names(r)) {
    return(clamp01(num0(r$P_SHOOT_FOUL[1], NA_real_)))
  }
  if ("p_shoot_foul" %in% names(r)) {
    return(clamp01(num0(r$p_shoot_foul[1], NA_real_)))
  }
  
  NA_real_
}


# Combines team quarter-level turnover inputs + shooting inputs into a single set of usable simulation probabilities.
# This is the “probability builder” that turns profile tables into clean, bounded, MC-ready parameters.
#
# Turnovers:
# - Reads team TO% and live-ball share from team_to_profile_q (with safe defaults).
# - Optionally shrinks TO% toward a league anchor using mc_var$tov_rand to control volatility/noise.
#
# Shooting:
# - Pulls shot mix (p2_given_shot), FG2/FG3/FT % and FT pressure (FTA_per_FGA) from team_pts_profile_q.
# - Applies shrinkage (mc_var$shoot_shrink) toward league anchors to avoid extreme team inputs.
# - Applies optional per-simulation jitter (mc_var$pct_jitter_sd) after shrink to add controlled randomness.
#
# Fouls:
# - Prefers an explicit shooting-foul probability from a foul profile when available.
# - Otherwise derives a proxy from FTA_per_FGA, then clamps to a reasonable upper bound to prevent runaway FT volume.
#
# Output:
# - Returns a list of probabilities that the points engine consumes (turnovers, shot mix, make rates, FT%, foul rates).
derive_team_probs_q <- function(
    team_id, qtr,
    team_to_profile_q,
    team_pts_profile_q,
    team_shoot_foul_profile_q = NULL,
    season_token = NULL,
    window_n = NULL,
    mc_var    = MC_VAR,
    mc_anchor = MC_ANCHOR
) {
  
  # -------------------------------
  # Turnovers (WINDOW-AWARE)
  # -------------------------------
  q_to <- pick_profile_row(
    df = team_to_profile_q,
    team_id = team_id,
    season_token = season_token,
    qtr = qtr,
    window_n = window_n
  )
  
  p_to_raw <- if (nrow(q_to) > 0)
    pct01(q_to$TOV_pct, default = mc_anchor$to)
  else
    mc_anchor$to
  
  live_share <- if (nrow(q_to) > 0)
    clamp01(num0(q_to$TOV_live_share, 0.6))
  else
    0.6
  
  p_to <- shrink_to(
    p_to_raw,
    mc_anchor$to,
    clamp01(num0(mc_var$tov_rand, 0))
  )
  
  # -------------------------------
  # Shooting inputs (WINDOW-AWARE)
  # -------------------------------
  pts_in <- get_team_pts_inputs_q(
    team_id = team_id,
    qtr = qtr,
    team_pts_profile_q = team_pts_profile_q,
    season_token = season_token,
    window_n = window_n
  )
  
  FG2_pct <- shrink_to(pts_in$FG2_pct, mc_anchor$fg2, mc_var$shoot_shrink)
  FG3_pct <- shrink_to(pts_in$FG3_pct, mc_anchor$fg3, mc_var$shoot_shrink)
  FT_pct  <- shrink_to(pts_in$FT_pct,  mc_anchor$ft,  mc_var$shoot_shrink)
  
  FG2_pct <- apply_pct_jitter(FG2_pct, mc_var$pct_jitter_sd)
  FG3_pct <- apply_pct_jitter(FG3_pct, mc_var$pct_jitter_sd)
  FT_pct  <- apply_pct_jitter(FT_pct,  mc_var$pct_jitter_sd)
  
  # -------------------------------
  # Shooting fouls (WINDOW-AWARE)
  # -------------------------------
  p_shoot_foul <- get_team_shoot_foul_p_q(
    team_id = team_id,
    season_token = season_token,
    qtr = qtr,
    team_foul_profile_q = team_shoot_foul_profile_q,
    window_n = window_n
  )
  
  if (is.na(p_shoot_foul)) {
    avg_fts_per_shooting_foul <- 2.2
    p_shoot_foul <- ifelse(
      is.na(pts_in$FTA_per_FGA),
      num0(mc_anchor$shoot_foul, 0.10),
      pts_in$FTA_per_FGA / avg_fts_per_shooting_foul
    )
  }
  
  p_shoot_foul <- pmin(pmax(p_shoot_foul, 0), 0.18)
  
  # -------------------------------
  # Output
  # -------------------------------
  list(
    p_to = p_to,
    live_share = live_share,
    p2_given_shot = pts_in$p2_given_shot,
    FG2_pct = FG2_pct,
    FG3_pct = FG3_pct,
    FT_pct  = FT_pct,
    p_shoot_foul = p_shoot_foul
  )
}

# Simulate ONE team’s quarter-level scoring outcome given a fixed number of possessions.
# This function converts "volume" (possessions) into concrete box-score-like outputs by stochastically modeling:
#   turnovers → remaining shot possessions → 2PA/3PA mix → makes → shooting fouls/and-1s → free throws → total points.
#
# Steps (high level):
# 1) Build quarter-specific probabilities (TO%, live TO share, shot mix, FG/FT%, shooting-foul rate) via derive_team_probs_q().
# 2) Draw turnovers from a binomial process; subtract them from possessions to get shot possessions.
# 3) Draw offensive fouls (from team_foul_profile_q) which also consume shot possessions and are treated as extra turnovers.
# 4) Treat each remaining shot possession as one shot attempt; split attempts into 2PA vs 3PA using p2_given_shot.
# 5) Draw made shots for 2s and 3s using FG2_pct / FG3_pct (already stabilized by shrink + optional jitter).
# 6) Draw shooting fouls on attempts and approximate and-1s on made shots; convert fouls into free-throw attempts.
# 7) Draw made free throws using FT_pct and compute total points = 2*FGM2 + 3*FGM3 + FTM.
#
# Output:
# - Returns points plus supporting components (turnovers, fouls, shot/make counts, FT counts, and the exact probabilities used),
#   which is useful for debugging, calibration, and later extensions (rebounds, transition points, bonus logic, etc.).
simulate_team_offense_pts_q <- function(
    team_id, poss, qtr,
    team_to_profile_q,
    team_foul_profile_q,
    team_pts_profile_q,
    season_token = NULL,
    window_n = NULL,
    team_shoot_foul_profile_q = NULL,
    p_and1_given_made_foul = 0.08
) {
  
  
  probs <- derive_team_probs_q(
    team_id = team_id,
    qtr = qtr,
    team_to_profile_q = team_to_profile_q,
    team_pts_profile_q = team_pts_profile_q,
    team_shoot_foul_profile_q = team_shoot_foul_profile_q,
    season_token = season_token,
    window_n = window_n,
    mc_var = MC_VAR,
    mc_anchor = MC_ANCHOR
  )
  
  
  p_to        <- probs$p_to
  live_share  <- probs$live_share
  p2_given_sh <- probs$p2_given_shot
  FG2_pct     <- probs$FG2_pct
  FG3_pct     <- probs$FG3_pct
  FT_pct      <- probs$FT_pct
  p_foul      <- probs$p_shoot_foul
  
  poss_i <- as.integer(num0(poss, 0))
  
  tov_out <- simulate_team_turnovers_q(
    team_id = team_id,
    poss    = poss,
    qtr     = qtr,
    team_to_profile_q = team_to_profile_q,
    mc_var    = MC_VAR,
    mc_anchor = MC_ANCHOR
  )
  
  total_tov <- as.integer(tov_out$turnovers)
  live_tov  <- as.integer(tov_out$live_turnovers)
  
  shot_poss <- max(as.integer(poss) - total_tov, 0L)
  
  
  # offensive fouls (these consume shot possessions)
  off_fouls <- draw_off_fouls_q(team_id, team_foul_profile_q, shot_poss, qtr)
  off_fouls <- as.integer(off_fouls)
  
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  # each remaining shot_poss becomes one shot attempt
  total_shots <- as.integer(shot_poss)
  
  # split 2PA / 3PA
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  # makes (reflect shrink + jitter)
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  # shooting fouls on attempts (simple proxy)
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  # and-1s on makes (approx)
  made_foul_2_exp <- as.integer(round(FGM2 * p_foul))
  made_foul_3_exp <- as.integer(round(FGM3 * p_foul))
  
  and1_2 <- if (made_foul_2_exp > 0) rbinom(1, size = made_foul_2_exp, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3_exp > 0) rbinom(1, size = made_foul_3_exp, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  missed_foul_2 <- max(foul_2 - made_foul_2_exp, 0L)
  missed_foul_3 <- max(foul_3 - made_foul_3_exp, 0L)
  
  # free throws (reflect shrink + jitter via FT_pct)
  FTA <- as.integer(2L * missed_foul_2 + 3L * missed_foul_3 + 1L * (and1_2 + and1_3))
  FTM <- if (FTA > 0) rbinom(1, size = FTA, prob = FT_pct) else 0L
  FTM <- as.integer(FTM)
  
  pts <- as.integer(2L * FGM2 + 3L * FGM3 + 1L * FTM)
  
  list(
    base_points    = pts,
    poss_total     = as.integer(poss),
    turnovers      = total_tov,
    live_turnovers = live_tov,
    off_fouls      = off_fouls,
    FGA2 = FGA2, FGM2 = FGM2,
    FGA3 = FGA3, FGM3 = FGM3,
    FTA  = FTA,  FTM  = FTM,
    FG2_pct_used = FG2_pct,
    FG3_pct_used = FG3_pct,
    FT_pct_used  = FT_pct,
    p_to_used    = p_to,
    p_foul_used  = p_foul
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END. PTS ENGINE UPGRADE
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

simulate_team_offense_B_q_v3_modelA <- function(team_id, season_token, qtr,
                                                poss,
                                                team_pts_profile_q,
                                                team_to_profile_q,
                                                team_foul_profile_q,
                                                team_reb_profile_q,
                                                sd_shrink_fga = 0.75,
                                                p_and1_given_made_foul = 0.08) {
  
  # ---- base shooting profile for this qtr (from team_pts_profile_q) ----
  pts_prof <- get_pts_prof_q(team_id, season_token, qtr, team_pts_profile_q)
  
  pts_prof$FG2_pct <- apply_pct_jitter(pts_prof$FG2_pct, MC_VAR$pct_jitter_sd)
  pts_prof$FG3_pct <- apply_pct_jitter(pts_prof$FG3_pct, MC_VAR$pct_jitter_sd)
  pts_prof$FT_pct  <- apply_pct_jitter(pts_prof$FT_pct,  MC_VAR$pct_jitter_sd)
  
  
  # ---- turnover inputs ----
  q_to <- team_to_profile_q %>% dplyr::filter(team_id == !!team_id, qtr == !!qtr) %>% dplyr::slice(1)
  p_to <- if (nrow(q_to) > 0) pct01(q_to$TOV_pct, default = 0.13) else 0.13
  live_share <- if (nrow(q_to) > 0) clamp01(num0(q_to$TOV_live_share, 0.6)) else 0.6
  
  total_tov <- if (p_to > 0) rbinom(1, size = poss, prob = p_to) else 0L
  total_tov <- as.integer(total_tov)
  live_tov  <- if (total_tov > 0) rbinom(1, size = total_tov, prob = live_share) else 0L
  live_tov  <- as.integer(live_tov)
  
  shot_poss <- max(poss - total_tov, 0L)
  
  # ---- offensive fouls reduce shot possessions (kept as you had it) ----
  q_foul <- team_foul_profile_q %>% dplyr::filter(team_id == !!team_id, qtr == !!qtr) %>% dplyr::slice(1)
  of_mu <- if (nrow(q_foul) > 0) num0(q_foul$OFF_FOULS_mean, 0) else 0
  of_sd <- if (nrow(q_foul) > 0) num0(q_foul$OFF_FOULS_sd,   0) else 0
  off_fouls <- draw_attempts(of_mu, of_sd)
  off_fouls <- as.integer(pmin(pmax(off_fouls, 0L), shot_poss))
  
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  # ---- BASE shots: draw around expected quarter FGA, cap to shot_poss ----
  fga_mu <- pts_prof$FGA_mean
  fga_sd <- pts_prof$FGA_sd * sd_shrink_fga
  base_fga <- draw_attempts(fga_mu, fga_sd)
  base_fga <- as.integer(pmin(base_fga, shot_poss))
  
  # ------------------------------------------------------------
  # TRACK + PATCH: forced shots ("missing possessions -> shots")
  # If base_fga < shot_poss, then (shot_poss - base_fga) shot possessions
  # would have no shot outcome unless we force them to become generic shots.
  # ------------------------------------------------------------
  forced_shots <- as.integer(pmax(shot_poss - base_fga, 0L))
  
  # If you want the accounting patch ON (recommended for consistency):
  base_fga <- as.integer(base_fga + forced_shots)
  
  # rate for monitoring
  forced_shots_rate <- ifelse(shot_poss > 0, forced_shots / shot_poss, 0)
  
  
  # split 2/3 mix
  FGA2 <- if (base_fga > 0) rbinom(1, size = base_fga, prob = pts_prof$p2_given_shot) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(base_fga - FGA2)
  
  # makes
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = pts_prof$FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = pts_prof$FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  # misses
  misses_base <- as.integer((FGA2 - FGM2) + (FGA3 - FGM3))
  misses_base <- as.integer(pmax(misses_base, 0L))
  
  # ------------------------------------------------------------
  # MODEL A: Continuations via OREB ONLY
  #   - draw OREB count from team_reb_profile_q
  #   - cap by misses
  #   - each OREB becomes an extra shot attempt
  # ------------------------------------------------------------
  oreb_prof <- get_oreb_prof_q(team_id, season_token, qtr, team_reb_profile_q)
  
  oreb_mu <- oreb_prof$OREB_mean * MC_VAR$reb_mean_mult
  oreb_sd <- oreb_prof$OREB_sd   * MC_VAR$reb_sd_mult
  
  oreb_draw <- draw_attempts(oreb_mu, oreb_sd)
  extra_shots <- as.integer(pmin(oreb_draw, misses_base))
  
  # simulate scoring on extra shots using same shot mix and %s (simple + stable)
  extra_FGA2 <- if (extra_shots > 0) rbinom(1, size = extra_shots, prob = pts_prof$p2_given_shot) else 0L
  extra_FGA2 <- as.integer(extra_FGA2)
  extra_FGA3 <- as.integer(extra_shots - extra_FGA2)
  
  extra_FGM2 <- if (extra_FGA2 > 0) rbinom(1, size = extra_FGA2, prob = pts_prof$FG2_pct) else 0L
  extra_FGM3 <- if (extra_FGA3 > 0) rbinom(1, size = extra_FGA3, prob = pts_prof$FG3_pct) else 0L
  extra_FGM2 <- as.integer(extra_FGM2); extra_FGM3 <- as.integer(extra_FGM3)
  
  # ---- FT logic (kept consistent with your approach) ----
  avg_fts_per_foul <- 2.0
  p_shoot_foul <- pts_prof$FTA_per_FGA / avg_fts_per_foul
  p_shoot_foul <- pmin(pmax(p_shoot_foul, 0), 0.16)
  
  foul_2    <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_shoot_foul) else 0L
  foul_3    <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_shoot_foul) else 0L
  foul_2_sc <- if (extra_FGA2 > 0) rbinom(1, size = extra_FGA2, prob = p_shoot_foul) else 0L
  foul_3_sc <- if (extra_FGA3 > 0) rbinom(1, size = extra_FGA3, prob = p_shoot_foul) else 0L
  
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  foul_2_sc <- as.integer(foul_2_sc); foul_3_sc <- as.integer(foul_3_sc)
  
  made_foul_2_exp <- round((FGM2 + extra_FGM2) * p_shoot_foul)
  made_foul_3_exp <- round((FGM3 + extra_FGM3) * p_shoot_foul)
  
  and1_2 <- if (made_foul_2_exp > 0) rbinom(1, size = made_foul_2_exp, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3_exp > 0) rbinom(1, size = made_foul_3_exp, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  missed_foul_2 <- max((foul_2 + foul_2_sc) - made_foul_2_exp, 0L)
  missed_foul_3 <- max((foul_3 + foul_3_sc) - made_foul_3_exp, 0L)
  
  FTA <- as.integer(2L * missed_foul_2 + 3L * missed_foul_3 + 1L * (and1_2 + and1_3))
  FTM <- if (FTA > 0) rbinom(1, size = FTA, prob = pts_prof$FT_pct) else 0L
  FTM <- as.integer(FTM)
  
  pts <- as.integer(
    2L * (FGM2 + extra_FGM2) +
      3L * (FGM3 + extra_FGM3) +
      1L * FTM
  )
  
  list(
    base_points    = pts,
    
    poss_total     = as.integer(poss),
    shot_poss      = as.integer(shot_poss),
    
    turnovers      = as.integer(total_tov),
    live_turnovers = as.integer(live_tov),
    off_fouls      = as.integer(off_fouls),
    
    # base + extra shot accounting
    FGA2 = as.integer(FGA2 + extra_FGA2),
    FGM2 = as.integer(FGM2 + extra_FGM2),
    FGA3 = as.integer(FGA3 + extra_FGA3),
    FGM3 = as.integer(FGM3 + extra_FGM3),
    
    FTA  = as.integer(FTA),
    FTM  = as.integer(FTM),
    
    misses_base = as.integer(misses_base),
    extra_shots = as.integer(extra_shots),
    forced_shots      = as.integer(forced_shots),
    forced_shots_rate = forced_shots_rate
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
# TEAM MC PROFILE (v_4) — WHAT / WHY / HOW
#
# WHAT:
# Builds a single, season-level baseline profile per team containing average
# FG2%, FG3%, and FT% collapsed across all quarters from team_pts_profile_q.
# This table has exactly one row per team_id per season_token and may optionally
# include a team abbreviation for audit/debug outputs.
#
# WHY:
# This profile acts as a stable anchor for the Monte Carlo system. It provides
# sane season-level shooting baselines used for calibration, shrinkage, fallback
# defaults, and reporting, while quarter-level profiles handle all in-game
# dynamics and variance. Keeping this layer prevents over-reliance on noisy
# quarter splits and simplifies debugging and validation.
#
# HOW:
# Quarter-level shooting efficiencies are averaged across quarters, clamped to
# realistic bounds, and defaulted when missing. No possession, turnover, foul,
# or rebounding logic is included here—those remain in their respective profiles.
# This profile is read-only for the MC and does not simulate outcomes itself.
# --------------------------------------------------------------------------------

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# 3. START: Build single team_mc_profile (v_4)
#   - Purpose (v_4): one row per team_id per season_token
#   - Holds baseline shooting efficiencies + optional abbrev for audit
#   - Quarter dynamics come from:
#       team_poss_profile_q, team_to_profile_q, team_foul_profile_q, team_pts_profile_q
# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬

# --- 1) baseline shooting: collapse team_pts_profile_q across quarters ---
team_mc_profile <- team_pts_profile_q %>%
  dplyr::filter(window_n == 35) %>%   # anchor window
  dplyr::group_by(team_id, season_token) %>%
  dplyr::summarise(
    FG2_pct = mean(FG2_pct, na.rm = TRUE),
    FG3_pct = mean(FG3_pct, na.rm = TRUE),
    FT_pct  = mean(FT_pct,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    FG2_pct = clamp01(ifelse(is.na(FG2_pct), 0.50, FG2_pct)),
    FG3_pct = clamp01(ifelse(is.na(FG3_pct), 0.36, FG3_pct)),
    FT_pct  = clamp01(ifelse(is.na(FT_pct),  0.78, FT_pct))
  )


# --- 2) optional: team abbrev lookup for audit outputs (safe, no dependencies) ---
# expects pbp_df contains home/away abbrevs and team_id columns
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

# --- 3) season_chosen for the MC calls (keep this var name) ---
season_chosen <- as.character(season_token)

# 🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬🧬
# 3. END: team_mc_profile (v_4)
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
# OPPONENT ADJUSTMENT LAYER:
#   Build "defense allowed" profiles by swapping opponent offensive stats within ESPN_GAME_ID
#   Outputs:
#     - def_allowed_long
#     - team_def_allowed_profile_q
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(tibble)

# ----------------------------
# helpers
# ----------------------------
parse_game_date <- function(x) {
  d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x))
  d
}

has_col <- function(df, nm) nm %in% names(df)

# ----------------------------
# prep base (schema-safe)
# ----------------------------
# ensure season_token column exists (or coerce it)
if (!"season_token" %in% names(BaseStats_Team_MC)) {
  BaseStats_Team_MC$season_token <- as.character(season_token)
} else {
  BaseStats_Team_MC$season_token <- as.character(BaseStats_Team_MC$season_token)
}

# ensure game_date_dt exists (or build it)
if (!"game_date_dt" %in% names(BaseStats_Team_MC)) {
  BaseStats_Team_MC <- BaseStats_Team_MC %>%
    mutate(game_date_dt = parse_game_date(GAME_DATE))
}

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  ) %>%
  filter(!is.na(team_id), team_id != "", !is.na(game_date_dt))

# ----------------------------
# NO LEAKAGE (training-only)
# ----------------------------
slate_cut <- as.Date(slate_dt)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  filter(game_date_dt < slate_cut)

# ----------------------------
# choose stats to swap
# ----------------------------
q_cols <- c(
  "T_2PT_PCT_Q", "T_3PT_PCT_Q", "T_FT_PCT_Q", "T_FG_PCT_Q",
  "T_TOV_PCT_Q", "T_OFF_FOULS_Q",
  "T_OREB_Q", "T_DREB_Q", "T_REB_Q",
  "T_FTR_Q"
)

cgs_cols <- c(
  "T_2PT_PCT_CGS", "T_3PT_PCT_CGS", "T_FT_PCT_CGS", "T_FG_PCT_CGS",
  "T_TOV_PCT_CGS", "T_OFF_FOULS_CGS",
  "T_OREB_CGS", "T_DREB_CGS", "T_REB_CGS",
  "T_FTR_CGS"
)

expand_q_cols <- function(prefix) paste0(prefix, 1:6)

q_cols_exist <- q_cols[
  sapply(q_cols, function(x) any(expand_q_cols(x) %in% names(BaseStats_Team_MC)))
]

swap_cols <- c(
  unlist(lapply(q_cols_exist, expand_q_cols)),
  cgs_cols[cgs_cols %in% names(BaseStats_Team_MC)]
)
swap_cols <- swap_cols[swap_cols %in% names(BaseStats_Team_MC)]

use_cols <- c("team_id", "ESPN_GAME_ID", "season_token", "game_date_dt")

# ----------------------------
# build minimal table (and force uniqueness per team/game)
# ----------------------------
dup_check <- BaseStats_Team_MC %>%
  count(season_token, ESPN_GAME_ID, team_id) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  message("⚠️ Deduplicating BaseStats_Team_MC for opponent swap (multiple rows per team/game detected).")
}

bs_min <- BaseStats_Team_MC %>%
  select(any_of(c(use_cols, swap_cols))) %>%
  mutate(across(all_of(swap_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  distinct(season_token, ESPN_GAME_ID, team_id, .keep_all = TRUE)

# ----------------------------
# opponent swap
# ----------------------------
opp_swapped <- bs_min %>%
  rename_with(~ paste0("OFF__", .x), all_of(swap_cols)) %>%
  inner_join(
    bs_min %>%
      select(-game_date_dt) %>%                  # prevents .x/.y
      rename_with(~ paste0("OPP__", .x), all_of(swap_cols)) %>%
      rename(opp_team_id = team_id),
    by = c("season_token", "ESPN_GAME_ID")
  ) %>%
  filter(team_id != opp_team_id)


# ----------------------------
# long format (quarter + CGS)
# ----------------------------
def_allowed_long_q <- opp_swapped %>%
  pivot_longer(
    cols = starts_with("OPP__") & ends_with(paste0("_Q", 1:6)),
    names_to = "stat_q",
    values_to = "opp_value"
  ) %>%
  mutate(
    qtr  = as.integer(sub(".*_Q([1-6])$", "\\1", stat_q)),
    stat = sub("^OPP__", "", sub("_Q[1-6]$", "", stat_q))
  ) %>%
  select(team_id, season_token, game_date_dt, qtr, stat, opp_value)

def_allowed_long_cgs <- opp_swapped %>%
  pivot_longer(
    cols = starts_with("OPP__") & ends_with("_CGS"),
    names_to = "stat_cgs",
    values_to = "opp_value"
  ) %>%
  mutate(
    qtr  = 0L,
    stat = sub("^OPP__", "", sub("_CGS$", "", stat_cgs))
  ) %>%
  select(team_id, season_token, game_date_dt, qtr, stat, opp_value)

def_allowed_long <- bind_rows(def_allowed_long_q, def_allowed_long_cgs)

dt <- as.data.table(def_allowed_long)
setorder(dt, team_id, stat, qtr, game_date_dt)

# ----------------------------
# WINDOWED PROFILE (7 windows)
# ----------------------------
windows <- c(5L, 7L, 10L, 15L, 20L, 25L, 35L)

team_def_allowed_profile_q <- rbindlist(lapply(windows, function(N) {
  
  dt[, {
    v  <- opp_value[!is.na(opp_value)]
    vN <- tail(v, min(N, length(v)))
    
    list(
      DEF_ALLOW_mean = if (length(vN) > 0) mean(vN) else NA_real_,
      DEF_ALLOW_sd   = if (length(vN) > 1) sd(vN)   else 0,
      n_games        = length(vN)
    )
  }, by = .(team_id, season_token, qtr, stat)][
    , window_n := as.integer(N)
  ][]
}), fill = TRUE)

team_def_allowed_profile_q <- as_tibble(team_def_allowed_profile_q)

# ----------------------------
# sanity checks
# ----------------------------
stopifnot(
  all(c(
    "team_id","season_token","qtr","stat",
    "window_n","DEF_ALLOW_mean","DEF_ALLOW_sd","n_games"
  ) %in% names(team_def_allowed_profile_q))
)

print(team_def_allowed_profile_q %>% slice(1:20))

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END OPPONENT ADJUMENT SECTION
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠


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
                                                                                                                                                                                                                       



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 4. START: Core helpers + quarter sim functions (WINDOW-AWARE)
#   NOTE:
#     - This section does NOT "run all 7 windows"
#     - It only makes MC functions accept window_n and use it in profile lookups
#     - Running + calibrating all 7 windows happens in the backtest loop / runner
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)

# --- window-aware row picker (strict by default) ---
pick_profile_row <- function(df, team_id, season_token = NULL, qtr = NULL, window_n = NULL,
                             strict_window = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
  
  out <- df %>% dplyr::filter(team_id == !!team_id)
  
  if (!is.null(season_token) && has_col(out, "season_token")) {
    out <- out %>% dplyr::filter(season_token == !!season_token)
  }
  if (!is.null(qtr) && has_col(out, "qtr")) {
    out <- out %>% dplyr::filter(qtr == !!qtr)
  }
  
  # apply window filter only if present and requested
  if (!is.null(window_n) && has_col(out, "window_n")) {
    out_w <- out %>% dplyr::filter(as.integer(window_n) == as.integer(!!window_n))
    
    if (nrow(out_w) > 0) {
      return(out_w %>% dplyr::slice(1))
    }
    
    # STRICT mode: if requested window not present, return 0-row slice (forces defaults upstream)
    if (isTRUE(strict_window)) {
      return(out[0, , drop = FALSE])
    }
    
    # NON-STRICT fallback: take largest available window for that slice (use sparingly)
    out <- out %>%
      dplyr::arrange(dplyr::desc(as.integer(window_n))) %>%
      dplyr::slice(1)
    return(out)
  }
  
  out %>% dplyr::slice(1)
}

safe_div <- function(a, b) {
  if (length(b) == 0) return(NA_real_)
  ifelse(is.na(b) | b == 0, NA_real_, a / b)
}

tag_from_z <- function(z, lo = -1, hi = 1) {
  if (length(z) == 0) return(NA_character_)
  z <- suppressWarnings(as.numeric(z))
  if (length(z) == 0 || is.na(z)) return(NA_character_)
  if (z <= lo)  return("low")
  if (z >= hi)  return("high")
  "normal"
}

clamp01 <- function(x) {
  if (length(x) == 0) return(NA_real_)
  pmin(pmax(x, 0), 1)
}

draw_attempts <- function(mean_val, sd_val) {
  if (length(mean_val) == 0) return(0L)
  mean_val <- suppressWarnings(as.numeric(mean_val))
  if (length(mean_val) == 0 || is.na(mean_val) || mean_val <= 0) return(0L)
  
  if (length(sd_val) == 0) sd_val <- 0
  sd_val <- suppressWarnings(as.numeric(sd_val))
  if (length(sd_val) == 0 || is.na(sd_val)) sd_val <- 0
  
  val <- round(rnorm(1, mean = mean_val, sd = sd_val))
  val <- max(val, 0L)
  as.integer(val)
}

pct01 <- function(x, default = 0) {
  if (length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  pmin(pmax(x, 0), 1)
}

num0 <- function(x, default = 0) {
  if (length(x) == 0) return(default)
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0) return(default)
  out[is.na(out)] <- default
  out
}

apply_pct_jitter <- function(p, sd = 0.01, default = 0.5) {
  p <- pct01(p, default = default)
  if (is.na(sd) || sd <= 0) return(clamp01(p))
  clamp01(p + rnorm(1, mean = 0, sd = sd))
}

get_team_shoot_foul_p_q <- function(team_id, season_token, qtr,
                                    team_foul_profile_q = NULL,
                                    default_p = 0.06,
                                    window_n = NULL) {
  
  if (is.null(team_foul_profile_q) || nrow(team_foul_profile_q) == 0) {
    return(default_p)
  }
  
  r <- pick_profile_row(team_foul_profile_q, team_id, season_token, qtr, window_n, strict_window = TRUE)
  if (nrow(r) == 0) return(default_p)
  
  if (!("p_shoot_foul" %in% names(r))) return(default_p)
  clamp01(num0(r$p_shoot_foul, default_p))
}

draw_team_possessions_q <- function(team_id, team_poss_profile_q,
                                    sd_shrink = 0.75,
                                    qtr,
                                    min_q = 1L,
                                    default_mu = 25,
                                    season_token = NULL,
                                    window_n = NULL) {
  
  prof <- pick_profile_row(team_poss_profile_q, team_id, season_token, qtr, window_n, strict_window = TRUE)
  
  mu <- if (nrow(prof) > 0 && has_col(prof, "POSS_mean")) suppressWarnings(as.numeric(prof$POSS_mean[1])) else NA_real_
  sd <- if (nrow(prof) > 0 && has_col(prof, "POSS_sd"))   suppressWarnings(as.numeric(prof$POSS_sd[1]))   else 0
  
  if (is.na(mu) || mu <= 0) mu <- default_mu
  if (is.na(sd) || sd < 0)  sd <- 0
  
  sd   <- sd * sd_shrink
  poss <- draw_attempts(mu, sd)
  as.integer(pmax(poss, min_q))
}

draw_off_fouls_q <- function(team_id, team_foul_profile_q, shot_poss, qtr,
                             season_token = NULL,
                             window_n = NULL) {
  
  prof <- pick_profile_row(team_foul_profile_q, team_id, season_token, qtr, window_n, strict_window = TRUE)
  
  mu <- if (nrow(prof) > 0 && has_col(prof, "OFF_FOULS_mean")) num0(prof$OFF_FOULS_mean[1], 0) else 0
  sd <- if (nrow(prof) > 0 && has_col(prof, "OFF_FOULS_sd"))   num0(prof$OFF_FOULS_sd[1],   0) else 0
  
  of <- draw_attempts(mu, sd)
  as.integer(pmin(pmax(of, 0L), shot_poss))
}

apply_blowout_drag <- function(q_poss, margin_3q,
                               blowout_margin = 18L,
                               min_mult = 0.75,
                               max_mult = 0.92) {
  if (is.na(margin_3q) || abs(margin_3q) < blowout_margin) return(as.integer(q_poss))
  
  m <- pmin(abs(margin_3q), 30)
  t <- (m - blowout_margin) / (30 - blowout_margin)
  mult <- max_mult - t * (max_mult - min_mult)
  as.integer(pmax(1L, round(q_poss * mult)))
}

apply_endgame_intentional_fouls_simple <- function(home_pts, away_pts,
                                                   home_FT_pct, away_FT_pct,
                                                   close_margin = 6L,
                                                   max_foul_trips = 6L,
                                                   foul_ft_per_trip = 2L,
                                                   lever = 1.0) {
  margin <- home_pts - away_pts
  if (is.na(margin) || abs(margin) > close_margin) {
    return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,trips=0L))
  }
  
  lever <- clamp01(num0(lever, 1.0))
  trips <- as.integer(round((sample.int(max_foul_trips + 1L, 1) - 1L) * lever))
  trips <- as.integer(pmin(pmax(trips, 0L), max_foul_trips))
  if (trips <= 0) return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,trips=0L))
  
  home_trailing <- (margin < 0)
  
  if (home_trailing) {
    a_fta <- as.integer(trips * foul_ft_per_trip)
    a_ftm <- as.integer(if (a_fta > 0) rbinom(1, a_fta, clamp01(away_FT_pct)) else 0L)
    return(list(h_fta=0L,h_ftm=0L,a_fta=a_fta,a_ftm=a_ftm,trips=trips))
  } else {
    h_fta <- as.integer(trips * foul_ft_per_trip)
    h_ftm <- as.integer(if (h_fta > 0) rbinom(1, h_fta, clamp01(home_FT_pct)) else 0L)
    return(list(h_fta=h_fta,h_ftm=h_ftm,a_fta=0L,a_ftm=0L,trips=trips))
  }
}

simulate_game_once <- function(home_team_id, away_team_id,
                               season_token,
                               team_poss_profile_q,
                               team_to_profile_q,
                               team_foul_profile_q,
                               team_pts_profile_q,
                               max_ot = 2L,
                               clutch_close_margin = 6L,
                               clutch_lever = 1.0,
                               window_n = NULL) {
  
  home_q_poss <- sapply(1:4, function(q) {
    draw_team_possessions_q(home_team_id, team_poss_profile_q, qtr = q,
                            season_token = season_token, window_n = window_n)
  })
  away_q_poss <- sapply(1:4, function(q) {
    draw_team_possessions_q(away_team_id, team_poss_profile_q, qtr = q,
                            season_token = season_token, window_n = window_n)
  })
  
  home_pts <- 0L; away_pts <- 0L
  
  acc <- function() list(tov=0L, live=0L, off=0L, fga2=0L, fgm2=0L, fga3=0L, fgm3=0L, fta=0L, ftm=0L)
  H <- acc(); A <- acc()
  
  home_pts_q1 <- 0L; away_pts_q1 <- 0L
  home_pts_q2 <- 0L; away_pts_q2 <- 0L
  home_pts_q3 <- 0L; away_pts_q3 <- 0L
  home_pts_q4 <- 0L; away_pts_q4 <- 0L
  home_pts_q5 <- 0L; away_pts_q5 <- 0L
  home_pts_q6 <- 0L; away_pts_q6 <- 0L
  
  for (q in 1:3) {
    
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss    = as.integer(home_q_poss[q]),
      qtr     = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q,
      season_token = season_token,
      window_n = window_n
    )
    
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss    = as.integer(away_q_poss[q]),
      qtr     = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q,
      season_token = season_token,
      window_n = window_n
    )
    
    hq_pts <- as.integer(hb$base_points)
    aq_pts <- as.integer(ab$base_points)
    
    if (q == 1) { home_pts_q1 <- home_pts_q1 + hq_pts; away_pts_q1 <- away_pts_q1 + aq_pts }
    if (q == 2) { home_pts_q2 <- home_pts_q2 + hq_pts; away_pts_q2 <- away_pts_q2 + aq_pts }
    if (q == 3) { home_pts_q3 <- home_pts_q3 + hq_pts; away_pts_q3 <- away_pts_q3 + aq_pts }
    
    home_pts <- home_pts + hq_pts
    away_pts <- away_pts + aq_pts
    
    H$tov <- H$tov + hb$turnovers;      H$live <- H$live + hb$live_turnovers; H$off <- H$off + hb$off_fouls
    H$fga2 <- H$fga2 + hb$FGA2;         H$fgm2 <- H$fgm2 + hb$FGM2
    H$fga3 <- H$fga3 + hb$FGA3;         H$fgm3 <- H$fgm3 + hb$FGM3
    H$fta  <- H$fta  + hb$FTA;          H$ftm  <- H$ftm  + hb$FTM
    
    A$tov <- A$tov + ab$turnovers;      A$live <- A$live + ab$live_turnovers; A$off <- A$off + ab$off_fouls
    A$fga2 <- A$fga2 + ab$FGA2;         A$fgm2 <- A$fgm2 + ab$FGM2
    A$fga3 <- A$fga3 + ab$FGA3;         A$fgm3 <- A$fgm3 + ab$FGM3
    A$fta  <- A$fta  + ab$FTA;          A$ftm  <- A$ftm  + ab$FTM
  }
  
  margin_3q <- as.integer(home_pts - away_pts)
  
  home_q_poss[4] <- apply_blowout_drag(home_q_poss[4], margin_3q)
  away_q_poss[4] <- apply_blowout_drag(away_q_poss[4], margin_3q)
  
  hb4 <- simulate_team_offense_pts_q(
    team_id = home_team_id,
    poss    = as.integer(home_q_poss[4]),
    qtr     = 4,
    team_to_profile_q   = team_to_profile_q,
    team_foul_profile_q = team_foul_profile_q,
    team_pts_profile_q  = team_pts_profile_q,
    season_token = season_token,
    window_n = window_n
  )
  
  ab4 <- simulate_team_offense_pts_q(
    team_id = away_team_id,
    poss    = as.integer(away_q_poss[4]),
    qtr     = 4,
    team_to_profile_q   = team_to_profile_q,
    team_foul_profile_q = team_foul_profile_q,
    team_pts_profile_q  = team_pts_profile_q,
    season_token = season_token,
    window_n = window_n
  )
  
  home_pts_q4 <- home_pts_q4 + as.integer(hb4$base_points)
  away_pts_q4 <- away_pts_q4 + as.integer(ab4$base_points)
  
  home_pts <- home_pts + as.integer(hb4$base_points)
  away_pts <- away_pts + as.integer(ab4$base_points)
  
  H$tov <- H$tov + hb4$turnovers;      H$live <- H$live + hb4$live_turnovers; H$off <- H$off + hb4$off_fouls
  H$fga2 <- H$fga2 + hb4$FGA2;         H$fgm2 <- H$fgm2 + hb4$FGM2
  H$fga3 <- H$fga3 + hb4$FGA3;         H$fgm3 <- H$fgm3 + hb4$FGM3
  H$fta  <- H$fta  + hb4$FTA;          H$ftm  <- H$ftm  + hb4$FTM
  
  A$tov <- A$tov + ab4$turnovers;      A$live <- A$live + ab4$live_turnovers; A$off <- A$off + ab4$off_fouls
  A$fga2 <- A$fga2 + ab4$FGA2;         A$fgm2 <- A$fgm2 + ab4$FGM2
  A$fga3 <- A$fga3 + ab4$FGA3;         A$fgm3 <- A$fgm3 + ab4$FGM3
  A$fta  <- A$fta  + ab4$FTA;          A$ftm  <- A$ftm  + ab4$FTM
  
  # clutch FT% pull must also be window-aware
  h_probs <- derive_team_probs_q(
    team_id = home_team_id,
    qtr = 4,
    team_to_profile_q  = team_to_profile_q,
    team_pts_profile_q = team_pts_profile_q,
    season_token = season_token,
    window_n = window_n
  )
  
  a_probs <- derive_team_probs_q(
    team_id = away_team_id,
    qtr = 4,
    team_to_profile_q  = team_to_profile_q,
    team_pts_profile_q = team_pts_profile_q,
    season_token = season_token,
    window_n = window_n
  )
  
  clutch <- apply_endgame_intentional_fouls_simple(
    home_pts = home_pts, away_pts = away_pts,
    home_FT_pct = h_probs$FT_pct,
    away_FT_pct = a_probs$FT_pct,
    close_margin = as.integer(clutch_close_margin),
    max_foul_trips = 6L,
    foul_ft_per_trip = 2L,
    lever = clutch_lever
  )
  
  home_pts <- as.integer(home_pts + clutch$h_ftm)
  away_pts <- as.integer(away_pts + clutch$a_ftm)
  H$fta <- as.integer(H$fta + clutch$h_fta); H$ftm <- as.integer(H$ftm + clutch$h_ftm)
  A$fta <- as.integer(A$fta + clutch$a_fta); A$ftm <- as.integer(A$ftm + clutch$a_ftm)
  
  ot_played <- 0L
  while (home_pts == away_pts && ot_played < max_ot) {
    
    ot_played <- ot_played + 1L
    q_ot <- 4L + ot_played
    
    hposs <- draw_team_possessions_q(home_team_id, team_poss_profile_q, qtr = q_ot,
                                     min_q = 1L, default_mu = 12,
                                     season_token = season_token, window_n = window_n)
    aposs <- draw_team_possessions_q(away_team_id, team_poss_profile_q, qtr = q_ot,
                                     min_q = 1L, default_mu = 12,
                                     season_token = season_token, window_n = window_n)
    
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss    = as.integer(hposs),
      qtr     = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q,
      season_token = season_token,
      window_n = window_n
    )
    
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss    = as.integer(aposs),
      qtr     = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q,
      season_token = season_token,
      window_n = window_n
    )
    
    if (q_ot == 5) {
      home_pts_q5 <- home_pts_q5 + as.integer(hb$base_points)
      away_pts_q5 <- away_pts_q5 + as.integer(ab$base_points)
    }
    if (q_ot == 6) {
      home_pts_q6 <- home_pts_q6 + as.integer(hb$base_points)
      away_pts_q6 <- away_pts_q6 + as.integer(ab$base_points)
    }
    
    home_pts <- as.integer(home_pts + hb$base_points)
    away_pts <- as.integer(away_pts + ab$base_points)
    
    H$tov <- H$tov + hb$turnovers;      H$live <- H$live + hb$live_turnovers; H$off <- H$off + hb$off_fouls
    H$fga2 <- H$fga2 + hb$FGA2;         H$fgm2 <- H$fgm2 + hb$FGM2
    H$fga3 <- H$fga3 + hb$FGA3;         H$fgm3 <- H$fgm3 + hb$FGM3
    H$fta  <- H$fta  + hb$FTA;          H$ftm  <- H$ftm  + hb$FTM
    
    A$tov <- A$tov + ab$turnovers;      A$live <- A$live + ab$live_turnovers; A$off <- A$off + ab$off_fouls
    A$fga2 <- A$fga2 + ab$FGA2;         A$fgm2 <- A$fgm2 + ab$FGM2
    A$fga3 <- A$fga3 + ab$FGA3;         A$fgm3 <- A$fgm3 + ab$FGM3
    A$fta  <- A$fta  + ab$FTA;          A$ftm  <- A$ftm  + ab$FTM
  }
  
  tibble::tibble(
    home_team_id = as.character(home_team_id),
    away_team_id = as.character(away_team_id),
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
    ot_played    = as.integer(ot_played),
    
    home_pts_q1 = as.integer(home_pts_q1),
    home_pts_q2 = as.integer(home_pts_q2),
    home_pts_q3 = as.integer(home_pts_q3),
    home_pts_q4 = as.integer(home_pts_q4),
    home_pts_q5 = as.integer(home_pts_q5),
    home_pts_q6 = as.integer(home_pts_q6),
    
    away_pts_q1 = as.integer(away_pts_q1),
    away_pts_q2 = as.integer(away_pts_q2),
    away_pts_q3 = as.integer(away_pts_q3),
    away_pts_q4 = as.integer(away_pts_q4),
    away_pts_q5 = as.integer(away_pts_q5),
    away_pts_q6 = as.integer(away_pts_q6),
    
    home_turnovers = as.integer(H$tov),
    away_turnovers = as.integer(A$tov),
    home_live_to   = as.integer(H$live),
    away_live_to   = as.integer(A$live),
    home_off_fouls = as.integer(H$off),
    away_off_fouls = as.integer(A$off),
    
    home_FGA2 = as.integer(H$fga2), home_FGM2 = as.integer(H$fgm2),
    away_FGA2 = as.integer(A$fga2), away_FGM2 = as.integer(A$fgm2),
    home_FGA3 = as.integer(H$fga3), home_FGM3 = as.integer(H$fgm3),
    away_FGA3 = as.integer(A$fga3), away_FGM3 = as.integer(A$fgm3),
    home_FTA  = as.integer(H$fta),  home_FTM  = as.integer(H$ftm),
    away_FTA  = as.integer(A$fta),  away_FTM  = as.integer(A$ftm),
    
    margin_3q          = as.integer(margin_3q),
    clutch_trips       = as.integer(clutch$trips),
    clutch_home_fta    = as.integer(clutch$h_fta),
    clutch_home_ftm    = as.integer(clutch$h_ftm),
    clutch_away_fta    = as.integer(clutch$a_fta),
    clutch_away_ftm    = as.integer(clutch$a_ftm),
    
    window_n = if (is.null(window_n)) NA_integer_ else as.integer(window_n)
  )
}

simulate_matchup <- function(home_team_id, away_team_id,
                             n_sims = 100L,
                             season_token,
                             team_poss_profile_q,
                             team_to_profile_q,
                             team_foul_profile_q,
                             team_pts_profile_q,
                             max_ot = 2L,
                             window_n = NULL) {
  
  sims <- purrr::map_dfr(seq_len(n_sims), function(i) {
    simulate_game_once(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      season_token = season_token,
      team_poss_profile_q = team_poss_profile_q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q,
      max_ot = max_ot,
      window_n = window_n
    )
  })
  
  sims
}


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 4. END: Core MC
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. START: Run MC for each game in today's slate (parallel) [WINDOW-AWARE]
#   - IMPORTANT: simulate 1 row per game_id (avoid double-simming flipped rows)
#   - Runs ALL windows in window_vec
#   - Keeps audit-friendly sim rows in mc_sims_all_games
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

set.seed(123)

if (!all(c("game_id", "team_id", "opp_id") %in% names(nba_schedule_today))) {
  stop("Expected columns game_id, team_id, opp_id not all present in nba_schedule_today.")
}

# ------------------------------------------------------------
# 0) Ensure we run ONE simulation per game_id (not both flipped rows)
# Prefer: is_static == TRUE if you have it; otherwise keep distinct(game_id)
# ------------------------------------------------------------
nba_schedule_today_games <- nba_schedule_today %>%
  mutate(
    game_id = as.character(game_id),
    team_id = as.character(team_id),
    opp_id  = as.character(opp_id)
  )

if ("is_static" %in% names(nba_schedule_today_games)) {
  nba_schedule_today_games <- nba_schedule_today_games %>%
    filter(is_static == TRUE)
} else {
  nba_schedule_today_games <- nba_schedule_today_games %>%
    distinct(game_id, .keep_all = TRUE)
}

if (nrow(nba_schedule_today_games) == 0) stop("nba_schedule_today_games is empty after de-dupe.")

# ------------------------------------------------------------
# 1) Window vector (your 7 windows)
#   NOTE: keep in one place so backtest + today runner use the same list
# ------------------------------------------------------------
window_vec <- c(5L, 10L, 15L, 20L, 25L, 30L, 35L)

game_indices <- seq_len(nrow(nba_schedule_today_games))

mc_sims_list <- future.apply::future_lapply(
  game_indices,
  function(i) {
    
    this_row  <- nba_schedule_today_games[i, ]
    home_id   <- this_row$team_id
    away_id   <- this_row$opp_id
    this_game <- this_row$game_id
    
    # run all windows for this game, bind together
    sims_all_w <- purrr::map_dfr(window_vec, function(w) {
      
      sims <- simulate_matchup(
        home_team_id = home_id,
        away_team_id = away_id,
        season_token = season_token,
        team_poss_profile_q = team_poss_profile_q,
        team_to_profile_q   = team_to_profile_q,
        team_foul_profile_q = team_foul_profile_q,
        team_pts_profile_q  = team_pts_profile_q,
        n_sims = 100L,
        max_ot = 2L,
        window_n = as.integer(w)
      )
      
      dplyr::mutate(
        sims,
        game_id      = this_game,
        home_team_id = as.character(home_id),
        away_team_id = as.character(away_id),
        window_n     = as.integer(w),
        sim_index    = dplyr::row_number()
      )
    })
    
    sims_all_w
  },
  future.seed = TRUE,
  future.globals = list(
    # --- core data ---
    nba_schedule_today_games = nba_schedule_today_games,
    season_token = season_token,
    window_vec   = window_vec,
    MC_VAR = MC_VAR,
    MC_ANCHOR = MC_ANCHOR,
    
    team_poss_profile_q = team_poss_profile_q,
    team_to_profile_q   = team_to_profile_q,
    team_foul_profile_q = team_foul_profile_q,
    team_pts_profile_q  = team_pts_profile_q,
    
    # --- helpers ---
    has_col = has_col,
    pick_profile_row = pick_profile_row,
    apply_pct_jitter = apply_pct_jitter,
    num0 = num0,
    pct01 = pct01,
    clamp01 = clamp01,
    safe_div = safe_div,
    draw_attempts = draw_attempts,
    tag_from_z = tag_from_z,
    shrink_to = shrink_to,
    
    # --- MC internals (ACTUALLY USED) ---
    draw_team_possessions_q = draw_team_possessions_q,
    draw_off_fouls_q = draw_off_fouls_q,
    get_team_shoot_foul_p_q = get_team_shoot_foul_p_q,
    get_team_pts_inputs_q  = get_team_pts_inputs_q,
    get_team_tov_inputs_q = get_team_tov_inputs_q,
    derive_team_tov_probs_q = derive_team_tov_probs_q,
    derive_team_probs_q     = derive_team_probs_q,
    simulate_team_offense_pts_q = simulate_team_offense_pts_q,
    simulate_game_once = simulate_game_once,

    simulate_team_turnovers_q = simulate_team_turnovers_q, 
    simulate_matchup   = simulate_matchup,
    
    # --- MISC Game Flow Functions ---
    apply_blowout_drag = apply_blowout_drag, 
    apply_endgame_intentional_fouls_simple = apply_endgame_intentional_fouls_simple,
    
    `%>%` = magrittr::`%>%`,
    
    # --- Safe Number Functions ---
    num0 = num0,
    pct01 = pct01,
    clamp01 = clamp01,
    safe_div = safe_div,
    draw_attempts = draw_attempts,
    tag_from_z = tag_from_z,
    pick_profile_row = pick_profile_row
  )
)

mc_sims_all_games <- data.table::rbindlist(mc_sims_list, use.names = TRUE, fill = TRUE)

# ------------------------------------------------------------
# 2) Summaries by game + window
# ------------------------------------------------------------
mc_summary_by_game <- mc_sims_all_games %>%
  dplyr::group_by(game_id, home_team_id, away_team_id, window_n) %>%
  dplyr::summarise(
    MC_HomePts     = mean(home_points),
    MC_AwayPts     = mean(away_points),
    MC_Total       = mean(home_points + away_points),
    MC_Spread      = mean(home_points - away_points),
    MC_HomeWinProb = mean(margin > 0),
    MC_AwayWinProb = mean(margin < 0),
    MC_PushProb    = mean(margin == 0),
    .groups = "drop"
  ) %>%
  dplyr::rename(
    team_id = home_team_id,
    opp_id  = away_team_id
  )

# ------------------------------------------------------------
# 3) Join back into nba_schedule_today (both flipped rows ok)
#   - We join by game_id + team_id + opp_id, AND window_n
#   - If nba_schedule_today has both flipped rows, both will get their correct orientation
# ------------------------------------------------------------
nba_schedule_today <- nba_schedule_today %>%
  mutate(
    game_id = as.character(game_id),
    team_id = as.character(team_id),
    opp_id  = as.character(opp_id)
  ) %>%
  tidyr::crossing(window_n = window_vec) %>%     # gives you one row per (game row x window)
  dplyr::left_join(mc_summary_by_game, by = c("game_id", "team_id", "opp_id", "window_n")) %>%
  dplyr::relocate(window_n, dplyr::starts_with("MC_"), .before = 1)

# ------------------------------------------------------------
# 4) Abbrev joins + relocate (fix column names)
# ------------------------------------------------------------
nba_schedule_today <- nba_schedule_today %>%
  dplyr::left_join(
    team_id_map %>%
      dplyr::select(espn_team_id, team_short_name) %>%
      dplyr::rename(team_abv = team_short_name),
    by = c("team_id" = "espn_team_id")
  ) %>%
  dplyr::left_join(
    team_id_map %>%
      dplyr::select(espn_team_id, team_short_name) %>%
      dplyr::rename(opp_abv = team_short_name),
    by = c("opp_id" = "espn_team_id")
  ) %>%
  dplyr::relocate(team_abv, opp_abv, .after = opp_id)

mc_summary_by_game <- mc_summary_by_game %>%
  dplyr::left_join(
    team_id_map %>%
      dplyr::select(espn_team_id, team_short_name) %>%
      dplyr::rename(team_abv = team_short_name),
    by = c("team_id" = "espn_team_id")
  ) %>%
  dplyr::left_join(
    team_id_map %>%
      dplyr::select(espn_team_id, team_short_name) %>%
      dplyr::rename(opp_abv = team_short_name),
    by = c("opp_id" = "espn_team_id")
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. END: Run MC
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooo        ooooo                 .o8            oooo         .oooooo.             oooo   o8o   .o8                              .    o8o                        
#     `88.       .888'                "888            `888        d8P'  `Y8b            `888   `"'  "888                            .o8    `"'                        
#      888b     d'888   .ooooo.   .oooo888   .ooooo.   888       888           .oooo.    888  oooo   888oooo.  oooo d8b  .oooo.   .o888oo oooo   .ooooo.  ooo. .oo.   
#      8 Y88. .P  888  d88' `88b d88' `888  d88' `88b  888       888          `P  )88b   888  `888   d88' `88b `888""8P `P  )88b    888   `888  d88' `88b `888P"Y88b  
#      8  `888'   888  888   888 888   888  888ooo888  888       888           .oP"888   888   888   888   888  888      .oP"888    888    888  888   888  888   888  
#      8    Y     888  888   888 888   888  888    .o  888       `88b    ooo  d8(  888   888   888   888   888  888     d8(  888    888 .  888  888   888  888   888  
#     o8o        o888o `Y8bod8P' `Y8bod88P" `Y8bod8P' o888o       `Y8bood8P'  `Y888""8o o888o o888o  `Y8bod8P' d888b    `Y888""8o   "888" o888o `Y8bod8P' o888o o888o 
#                                                                                                                                                                
#
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠#
                                                                                                                                                                

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 6. START: Walk-Forward Backtest (WINDOW-AWARE) — FIXED
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# ------------------------------------------------------------
# 0) Canonicalize required columns
# ------------------------------------------------------------
stopifnot(exists("nba_games_canon_season"))

bt_df <- nba_games_canon_season %>%
  dplyr::mutate(
    game_id       = as.character(game_id),
    home_team_id  = as.character(home_team_id),
    away_team_id  = as.character(away_team_id)
  )

# Ensure DATE column called game_date_dt
if (!("game_date_dt" %in% names(bt_df))) {
  if ("game_date" %in% names(bt_df)) {
    bt_df <- bt_df %>% dplyr::mutate(game_date_dt = as.Date(game_date))
  } else if ("game_date" %in% toupper(names(bt_df))) {
    nm <- names(bt_df)[toupper(names(bt_df)) == "GAME_DATE"][1]
    bt_df <- bt_df %>% dplyr::mutate(game_date_dt = as.Date(.data[[nm]]))
  } else {
    stop("No usable game date column found. Expected game_date_dt or game_date.")
  }
} else {
  bt_df <- bt_df %>% dplyr::mutate(game_date_dt = as.Date(game_date_dt))
}

# One row per game (handle flipped rows)
if ("is_static" %in% names(bt_df)) {
  bt_df <- bt_df %>% dplyr::filter(is_static == TRUE)
} else {
  bt_df <- bt_df %>% dplyr::distinct(game_id, .keep_all = TRUE)
}

# Detect actual score columns
score_team_col <- dplyr::case_when(
  "home_score" %in% names(bt_df) ~ "home_score",
  TRUE ~ NA_character_
)
score_opp_col <- dplyr::case_when(
  "away_score" %in% names(bt_df) ~ "away_score",
  TRUE ~ NA_character_
)

if (is.na(score_team_col) || is.na(score_opp_col)) {
  stop("Could not find actual score cols. Need home_score + away_score in bt_df.")
}

# ------------------------------------------------------------
# 1) Backtest config
# ------------------------------------------------------------
backtest_dates <- sort(unique(bt_df$game_date_dt))
window_vec     <- c(5L, 10L, 15L, 20L, 25L, 30L, 35L)
n_sims_bt      <- 100L
max_ot         <- 2L

bt_results <- list()

# ------------------------------------------------------------
# 2) Walk-forward loop
# ------------------------------------------------------------
for (as_of_date in backtest_dates) {
  
  message("Backtest date: ", as.character(as_of_date))
  
  slate_today <- bt_df %>%
    dplyr::filter(game_date_dt == as_of_date)
  
  if (nrow(slate_today) == 0) next
  
  hist_data <- bt_df %>%
    dplyr::filter(game_date_dt < as_of_date)
  
  if (nrow(hist_data) < 50) next
  
  # ----------------------------------------------------------
  # Rebuild WINDOW-AWARE profiles AS OF DATE
  # (Your environment has *_windows builders, not legacy names)
  # ----------------------------------------------------------
  team_poss_profile_q <- build_team_poss_profile_q_windows(hist_data, window_vec = window_vec)
  team_to_profile_q   <- build_team_to_profile_q_windows(hist_data,   window_vec = window_vec)
  team_foul_profile_q <- build_team_foul_profile_q_windows(hist_data, window_vec = window_vec)
  team_pts_profile_q  <- build_team_pts_profile_q_windows(hist_data,  window_vec = window_vec)
  
  for (w in window_vec) {
    
    sims_today <- purrr::map_dfr(seq_len(nrow(slate_today)), function(i) {
      
      r <- slate_today[i, ]
      
      simulate_matchup(
        home_team_id = r$home_team_id,
        away_team_id = r$away_team_id,
        season_token = r$season_token,
        team_poss_profile_q = team_poss_profile_q,
        team_to_profile_q   = team_to_profile_q,
        team_foul_profile_q = team_foul_profile_q,
        team_pts_profile_q  = team_pts_profile_q,
        n_sims   = n_sims_bt,
        max_ot   = max_ot,
        window_n = as.integer(w)
      ) %>%
        dplyr::mutate(
          game_id   = r$game_id,
          game_date = as_of_date,
          window_n  = as.integer(w),
          home_team = r$home_team_id,
          away_team = r$away_team_id,
          actual_margin = suppressWarnings(
            as.numeric(r[[score_team_col]]) - as.numeric(r[[score_opp_col]])
          ),
          actual_total  = suppressWarnings(
            as.numeric(r[[score_team_col]]) + as.numeric(r[[score_opp_col]])
          )
        )
    })
    
    if (nrow(sims_today) > 0) {
      bt_results[[paste(as.character(as_of_date), w, sep = "_")]] <- sims_today
    }
  }
}

if (length(bt_results) == 0) {
  stop("Backtest produced 0 results. Likely causes: no historical rows before dates or profile builders returning empty.")
}

# ------------------------------------------------------------
# 3) Bind + calibration-ready summary
# ------------------------------------------------------------
bt_sims_all <- dplyr::bind_rows(bt_results)

bt_summary <- bt_sims_all %>%
  dplyr::group_by(game_id, game_date, window_n) %>%
  dplyr::summarise(
    MC_spread   = mean(margin, na.rm = TRUE),
    MC_total    = mean(home_points + away_points, na.rm = TRUE),
    MC_win_prob = mean(margin > 0, na.rm = TRUE),
    actual_margin = dplyr::first(actual_margin),
    actual_total  = dplyr::first(actual_total),
    .groups = "drop"
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 6. END: Walk-Forward Backtest
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠




# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
#
#
#     ooooooooo.   oooo                                                ooooooooo.                          o8o                         .    o8o                                        .o ooo        ooooo   .oooooo.   o.   
#     `888   `Y88. `888                                                `888   `Y88.                        `"'                       .o8    `"'                                       .8' `88.       .888'  d8P'  `Y8b  `8.  
#      888   .d88'  888   .oooo.   oooo    ooo  .ooooo.  oooo d8b       888   .d88' oooo d8b  .ooooo.     oooo  .ooooo.   .ooooo.  .o888oo oooo   .ooooo.  ooo. .oo.    .oooo.o      .8'   888b     d'888  888           `8. 
#      888ooo88P'   888  `P  )88b   `88.  .8'  d88' `88b `888""8P       888ooo88P'  `888""8P d88' `88b    `888 d88' `88b d88' `"Y8   888   `888  d88' `88b `888P"Y88b  d88(  "8      88    8 Y88. .P  888  888            88 
#      888          888   .oP"888    `88..8'   888ooo888  888           888          888     888   888     888 888ooo888 888         888    888  888   888  888   888  `"Y88b.       88    8  `888'   888  888            88 
#      888          888  d8(  888     `888'    888    .o  888           888          888     888   888     888 888    .o 888   .o8   888 .  888  888   888  888   888  o.  )88b      `8.   8    Y     888  `88b    ooo   .8' 
#     o888o        o888o `Y888""8o     .8'     `Y8bod8P' d888b         o888o        d888b    `Y8bod8P'     888 `Y8bod8P' `Y8bod8P'   "888" o888o `Y8bod8P' o888o o888o 8""888P'       `8. o8o        o888o  `Y8bood8P'  .8'  
#                                  .o..P'                                                                  888                                                                         `"                               "'   
#                                  `Y8P'                                                               .o. 88P                                                                                                               
#                                                                                                      `Y888P                                                                                                             
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧
# ===============================================================
# Injury / Availability Layer (organized)
#   Bucket A: Rolling windows (date_keys from daily_units)
#   Bucket B: Today (current_date filtered from nba_schedule_season)
#
# Requires in memory:
#   - daily_units[[k]]$schedule_df  (rolling) with: NBA_Team_ID, team_id
#   - nba_schedule_season           (season)  with: game_date, NBA_Team_ID, team_id
#   - current_date                  (Date or date-like)
#   - BaseStats_Player_MC           with: NBA_PLAYER_ID, ESPN_PLAYER_ID, ESPN_TEAM_ID
#   - injury_data                   with: (ESPN_PLAYER_ID or espn_player_id),
#                                         (ESPN_TEAM_ID or espn_team_id),
#                                         status, (date or game_date)
#   - season_token2                 (ex "2025-26")
# ===============================================================

stopifnot(
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

# normalize injury columns (supports both uppercase and lowercase field names)
injury_data <- injury_data %>%
  mutate(
    ESPN_PLAYER_ID = as.character(if ("ESPN_PLAYER_ID" %in% names(.)) ESPN_PLAYER_ID else espn_player_id),
    ESPN_TEAM_ID   = as.character(if ("ESPN_TEAM_ID"   %in% names(.)) ESPN_TEAM_ID   else espn_team_id),
    status         = as.character(status)
  )

# normalize injury date -> game_date_dt
if ("date" %in% names(injury_data)) {
  injury_data <- injury_data %>% mutate(game_date_dt = as.Date(date))
} else if ("game_date" %in% names(injury_data)) {
  injury_data <- injury_data %>% mutate(game_date_dt = as.Date(game_date))
} else {
  stop("injury_data must have `date` (preferred) or `game_date` for slate matching.")
}

# unique bridge for roster -> ESPN_PLAYER_ID
bridge_unique <- BaseStats_Player_MC %>%
  select(NBA_PLAYER_ID, ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
  filter(!is.na(NBA_PLAYER_ID), !is.na(ESPN_PLAYER_ID), !is.na(ESPN_TEAM_ID)) %>%
  distinct(NBA_PLAYER_ID, ESPN_TEAM_ID, .keep_all = TRUE)

# -----------------------------
# 1) Roster cache
# -----------------------------
.roster_cache <- new.env(parent = emptyenv())

get_team_roster_cached <- function(nba_team_id, season_token2) {
  key <- paste0(season_token2, "::", nba_team_id)
  if (exists(key, envir = .roster_cache, inherits = FALSE)) {
    return(get(key, envir = .roster_cache, inherits = FALSE))
  }
  
  raw <- nba_commonteamroster(team_id = nba_team_id, season = season_token2)
  
  roster <- raw$CommonTeamRoster %>%
    select(-LeagueID, -NICKNAME, -PLAYER_SLUG, -BIRTH_DATE, -SCHOOL, -HOW_ACQUIRED) %>%
    rename(
      NBA_TEAM_ID   = TeamID,
      NBA_PLAYER_ID = PLAYER_ID
    ) %>%
    mutate(
      NBA_TEAM_ID   = as.character(NBA_TEAM_ID),
      NBA_PLAYER_ID = as.character(NBA_PLAYER_ID)
    )
  
  assign(key, roster, envir = .roster_cache)
  Sys.sleep(0.50)
  roster
}

# -----------------------------
# 2) Core builder (one date)
# -----------------------------
build_availability_for_date <- function(schedule_df, game_date, date_key = NA_character_) {
  stopifnot(!is.null(schedule_df), nrow(schedule_df) > 0)
  stopifnot(all(c("NBA_Team_ID", "team_id") %in% names(schedule_df)))
  
  game_date <- as.Date(game_date)
  if (is.na(game_date)) stop("build_availability_for_date(): invalid game_date.")
  
  # teams for this slate
  team_keys <- schedule_df %>%
    transmute(
      NBA_TEAM_ID  = as.character(NBA_Team_ID),
      ESPN_TEAM_ID = as.character(team_id),
      date_key     = as.character(date_key),
      GAME_DATE    = game_date
    ) %>%
    distinct() %>%
    filter(!is.na(NBA_TEAM_ID), NBA_TEAM_ID != "")
  
  # roster pool (NBA rosters) + attach ESPN team + bridge to ESPN player
  roster_pool <- purrr::map_dfr(unique(team_keys$NBA_TEAM_ID), function(nba_tid) {
    roster_i <- get_team_roster_cached(nba_tid, season_token2)
    espn_tid <- team_keys %>% filter(NBA_TEAM_ID == nba_tid) %>% slice(1) %>% pull(ESPN_TEAM_ID)
    if (length(espn_tid) == 0) espn_tid <- NA_character_
    
    roster_i %>%
      mutate(
        date_key     = as.character(date_key),
        GAME_DATE    = game_date,
        NBA_TEAM_ID  = as.character(nba_tid),
        ESPN_TEAM_ID = as.character(espn_tid)
      )
  }) %>%
    mutate(
      NBA_PLAYER_ID = as.character(NBA_PLAYER_ID),
      ESPN_TEAM_ID  = as.character(ESPN_TEAM_ID)
    ) %>%
    left_join(
      bridge_unique,
      by = c("NBA_PLAYER_ID" = "NBA_PLAYER_ID", "ESPN_TEAM_ID" = "ESPN_TEAM_ID")
    )
  
  # OUT list for this date
  inj_out <- injury_data %>%
    filter(!is.na(game_date_dt), game_date_dt == game_date) %>%
    filter(tolower(status) == "out") %>%
    select(ESPN_TEAM_ID, ESPN_PLAYER_ID, status, game_date_dt, everything()) %>%
    distinct(ESPN_TEAM_ID, ESPN_PLAYER_ID, .keep_all = TRUE)
  
  roster_out <- roster_pool %>%
    inner_join(inj_out, by = c("ESPN_TEAM_ID", "ESPN_PLAYER_ID"))
  
  roster_active <- roster_pool %>%
    anti_join(inj_out, by = c("ESPN_TEAM_ID", "ESPN_PLAYER_ID"))
  
  # audits (per-date)
  avail_audit <- roster_pool %>%
    mutate(
      is_out = ifelse(
        paste0(ESPN_TEAM_ID, "::", ESPN_PLAYER_ID) %in%
          paste0(inj_out$ESPN_TEAM_ID, "::", inj_out$ESPN_PLAYER_ID),
        1L, 0L
      )
    ) %>%
    left_join(
      inj_out %>%
        transmute(ESPN_TEAM_ID, ESPN_PLAYER_ID, injury_status = status),
      by = c("ESPN_TEAM_ID", "ESPN_PLAYER_ID")
    ) %>%
    mutate(injury_status = ifelse(is.na(injury_status), "active", injury_status))
  
  team_audit <- avail_audit %>%
    group_by(date_key, GAME_DATE, NBA_TEAM_ID, ESPN_TEAM_ID) %>%
    summarise(
      roster_pool_n = n(),
      out_n         = sum(is_out, na.rm = TRUE),
      active_n      = sum(1L - is_out, na.rm = TRUE),
      out_players   = paste0(ESPN_PLAYER_ID[is_out == 1L], collapse = ","),
      .groups = "drop"
    )
  
  list(
    team_keys     = team_keys,
    roster_pool   = roster_pool,
    roster_active = roster_active,
    roster_out    = roster_out,
    avail_audit   = avail_audit,
    team_audit    = team_audit
  )
}

# ===============================================================
# Bucket A: Rolling windows (date_keys)
# ===============================================================
stopifnot(exists("daily_units"))
date_keys <- names(daily_units)
stopifnot(length(date_keys) > 0)

rolling_availability_by_date <- list()

for (k in date_keys) {
  schedule_df <- daily_units[[k]]$schedule_df
  if (is.null(schedule_df) || nrow(schedule_df) == 0) {
    message("Skipping ", k, " (empty schedule_df).")
    next
  }
  
  window_date <- as.Date(k, format = "%Y%m%d")
  if (is.na(window_date)) {
    message("Skipping ", k, " (invalid date_key).")
    next
  }
  
  out <- build_availability_for_date(
    schedule_df = schedule_df,
    game_date   = window_date,
    date_key    = k
  )
  
  rolling_availability_by_date[[k]] <- out
  
  message(
    "Rolling avail built ", k,
    " | teams=", nrow(out$team_keys),
    " | pool=", nrow(out$roster_pool),
    " | out=", nrow(out$roster_out),
    " | active=", nrow(out$roster_active)
  )
}

# convenience lists (match your old naming)
roster_teamkeys_by_date <- purrr::map(rolling_availability_by_date, "team_keys")
roster_pool_by_date     <- purrr::map(rolling_availability_by_date, "roster_pool")
roster_active_by_date   <- purrr::map(rolling_availability_by_date, "roster_active")
roster_out_by_date      <- purrr::map(rolling_availability_by_date, "roster_out")

injury_availability_audit_list <- purrr::map(rolling_availability_by_date, "avail_audit")
injury_team_summary_list       <- purrr::map(rolling_availability_by_date, "team_audit")

injury_availability_audit_df <- dplyr::bind_rows(injury_availability_audit_list)
injury_team_summary_df       <- dplyr::bind_rows(injury_team_summary_list)

# ===============================================================
# Bucket B: Today (current_date) from nba_schedule_season
# ===============================================================
stopifnot(exists("current_date"))
stopifnot(exists("nba_schedule_season"))
stopifnot(all(c("game_date", "NBA_Team_ID", "team_id") %in% names(nba_schedule_season)))

today_date <- as.Date(current_date)
today_date_key <- format(today_date, "%Y%m%d")

nba_schedule_today <- nba_schedule_season %>%
  mutate(game_date_dt = as.Date(game_date)) %>%
  filter(game_date_dt == today_date)

if (nrow(nba_schedule_today) == 0) {
  message("No games found for current_date = ", today_date, ". Skipping today bucket.")
  
  roster_teamkeys_today <- tibble()
  roster_pool_today     <- tibble()
  roster_active_today   <- tibble()
  roster_out_today      <- tibble()
  
  injury_availability_today_df <- tibble()
  injury_team_summary_today_df <- tibble()
  
} else {
  
  today_availability <- build_availability_for_date(
    schedule_df = nba_schedule_today,
    game_date   = today_date,
    date_key    = today_date_key
  )
  
  roster_teamkeys_today <- today_availability$team_keys
  roster_pool_today     <- today_availability$roster_pool
  roster_active_today   <- today_availability$roster_active   # <-- base player pool for player_minutes_mc
  roster_out_today      <- today_availability$roster_out
  
  injury_availability_today_df <- today_availability$avail_audit
  injury_team_summary_today_df <- today_availability$team_audit
  
  message(
    "TODAY avail built ", today_date_key,
    " | teams=", nrow(roster_teamkeys_today),
    " | pool=", nrow(roster_pool_today),
    " | out=", nrow(roster_out_today),
    " | active=", nrow(roster_active_today)
  )
}

# Outputs you now have:
#   Rolling:
#     roster_teamkeys_by_date[[k]]
#     roster_pool_by_date[[k]]
#     roster_active_by_date[[k]]
#     roster_out_by_date[[k]]
#     injury_availability_audit_df
#     injury_team_summary_df
#
#   Today:
#     nba_schedule_today
#     roster_teamkeys_today
#     roster_pool_today
#     roster_active_today
#     roster_out_today
#     injury_availability_today_df
#     injury_team_summary_today_df

# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧
# END INJURY PROTOYPE SECTION
# 💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧💧



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Minutes Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# Answers:
#   (1) expected in/out, minutes expectation, role bucket, last known status
#   (2) how much minutes/usage/efficiency was redistributed
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

suppressWarnings({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

stopifnot(
  exists("roster_active_by_date"),
  exists("roster_out_by_date"),
  exists("player_rotations")
)

# ---- helpers ----
num0 <- function(x, d=0) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), d, x) }
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

# -----------------------------
# 0) Normalize + DE-DUPE player_rotations
#    Unit of truth: (GAME_DATE, ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID)
#    This prevents any teammate-pair / multi-row inflation.
# -----------------------------
normalize_player_rotations <- function(pr) {
  # normalize GAME_DATE column if joins created suffixes
  if (!("GAME_DATE" %in% names(pr))) {
    if ("GAME_DATE.x" %in% names(pr)) pr <- pr %>% rename(GAME_DATE = `GAME_DATE.x`)
    if ("GAME_DATE.y" %in% names(pr)) pr <- pr %>% select(-`GAME_DATE.y`)
  }
  
  req <- c("GAME_DATE", "ESPN_GAME_ID", "ESPN_TEAM_ID", "PLAYER_ID", "MINS")
  if (!all(req %in% names(pr))) {
    missing <- setdiff(req, names(pr))
    stop("player_rotations is missing required columns: ", paste(missing, collapse = ", "))
  }
  
  pr %>%
    mutate(
      GAME_DATE    = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_GAME_ID = as.character(ESPN_GAME_ID),
      ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
      ESPN_PLAYER_ID    = as.character(PLAYER_ID),
      MINS_NUM     = num0(MINS, NA_real_)
    ) %>%
    filter(!is.na(GAME_DATE)) %>%
    group_by(GAME_DATE, ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID) %>%
    summarise(
      MINS_NUM = suppressWarnings(max(MINS_NUM, na.rm = TRUE)),  # safest if duplicates exist
      STARTER_STATUS = if ("STARTER_STATUS" %in% names(pr)) {
        # keep first non-NA if present
        tmp <- pr$STARTER_STATUS
        tmp <- tmp[!is.na(tmp)]
        if (length(tmp) == 0) NA_character_ else as.character(tmp[1])
      } else NA_character_,
      .groups = "drop"
    )
}

player_rotations_clean <- normalize_player_rotations(player_rotations)

# -----------------------------
# A) Build MINUTES baseline from player_rotations (anchor)
#   HARD-MAPPED to your schema:
#     GAME_DATE, PLAYER_ID, ESPN_TEAM_ID, MINS_NUM, STARTER_STATUS
# -----------------------------
build_minutes_baseline_from_rotations <- function(current_date, n_games_back = 10L) {
  
  pr <- player_rotations_clean %>%
    mutate(
      ESPN_PLAYER_ID = PLAYER_ID,
      game_date_dt   = GAME_DATE
    ) %>%
    filter(game_date_dt < current_date) %>%
    arrange(ESPN_PLAYER_ID, game_date_dt)
  
  # starter flag (optional)
  pr <- pr %>%
    mutate(
      starter_flag = ifelse(
        tolower(as.character(STARTER_STATUS)) %in% c("starter","start","1","true","yes","y"),
        1L, 0L
      )
    )
  
  pr %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      mins_mean    = mean(MINS_NUM, na.rm = TRUE),
      mins_sd      = sd(MINS_NUM, na.rm = TRUE),
      games_used   = sum(!is.na(MINS_NUM)),
      starter_rate = ifelse(all(is.na(starter_flag)), NA_real_, mean(starter_flag, na.rm = TRUE)),
      .groups = "drop"
    )
}

player_rotations_clean <- player_rotations_clean %>%
  left_join(
    player_rotations %>%
      select(PLAYER_ID, PLAYER_NAME) %>%
      distinct(),
    by = "PLAYER_ID"
  )


# -----------------------------
# B) OPTIONAL: usage/efficiency proxies from BaseStats_Player_MC (soft)
# -----------------------------
pick_first <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) NA_character_ else hit[1]
}

build_usage_eff_baseline <- function(current_date, n_games_back = 10L) {
  if (!exists("BaseStats_Player_MC")) return(tibble())
  
  bs <- BaseStats_Player_MC
  
  date_col <- pick_first(bs, c("game_date_dt","game_date","GAME_DATE","date"))
  pid      <- pick_first(bs, c("ESPN_PLAYER_ID","espn_player_id"))
  tid      <- pick_first(bs, c("ESPN_TEAM_ID","espn_team_id"))
  gid      <- pick_first(bs, c("ESPN_GAME_ID","espn_game_id","game_id"))
  
  if (is.na(date_col) || is.na(pid) || is.na(tid)) return(tibble())
  
  min_col <- pick_first(bs, c("P_MIN_CGS","MIN_CGS","MIN","minutes","P_MIN"))
  usg_col <- pick_first(bs, c("P_USG_CGS","USG_CGS","USG","usage"))
  pts_col <- pick_first(bs, c("P_PTS_CGS","PTS_CGS","PTS"))
  reb_col <- pick_first(bs, c("P_REB_CGS","REB_CGS","REB"))
  ast_col <- pick_first(bs, c("P_AST_CGS","AST_CGS","AST"))
  
  bs <- bs %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(.data[[date_col]])),
      ESPN_PLAYER_ID = as.character(.data[[pid]]),
      ESPN_TEAM_ID   = as.character(.data[[tid]]),
      ESPN_GAME_ID   = if (!is.na(gid)) as.character(.data[[gid]]) else NA_character_,
      MIN_NUM        = if (!is.na(min_col)) num0(.data[[min_col]], 0) else NA_real_,
      USG_NUM        = if (!is.na(usg_col)) num0(.data[[usg_col]], NA_real_) else NA_real_,
      PTS_NUM        = if (!is.na(pts_col)) num0(.data[[pts_col]], NA_real_) else NA_real_,
      REB_NUM        = if (!is.na(reb_col)) num0(.data[[reb_col]], NA_real_) else NA_real_,
      AST_NUM        = if (!is.na(ast_col)) num0(.data[[ast_col]], NA_real_) else NA_real_
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    arrange(ESPN_PLAYER_ID, game_date_dt, ESPN_GAME_ID)
  
  bs %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      baseline_usg = mean(USG_NUM, na.rm = TRUE),
      pts_per_min  = ifelse(sum(MIN_NUM, na.rm = TRUE) > 0, sum(PTS_NUM, na.rm = TRUE) / sum(MIN_NUM, na.rm = TRUE), NA_real_),
      reb_per_min  = ifelse(sum(MIN_NUM, na.rm = TRUE) > 0, sum(REB_NUM, na.rm = TRUE) / sum(MIN_NUM, na.rm = TRUE), NA_real_),
      ast_per_min  = ifelse(sum(MIN_NUM, na.rm = TRUE) > 0, sum(AST_NUM, na.rm = TRUE) / sum(MIN_NUM, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
}

# -----------------------------
# 1) Select slate context
# -----------------------------

# -----------------------------
# 1) Select slate context (PROJECTIONS = TODAY SLATE)
# -----------------------------
stopifnot(exists("current_date"))
stopifnot(exists("roster_active_today"))
stopifnot(exists("roster_out_today"))

current_date <- as.Date(current_date)               # keep master current_date (today)
k            <- format(current_date, "%Y%m%d")      # label only (date_key)

roster_active <- roster_active_today
roster_out    <- roster_out_today

stopifnot(!is.null(roster_active), nrow(roster_active) > 0)

# -----------------------------
# 2) Build baselines
# -----------------------------
minutes_baseline <- build_minutes_baseline_from_rotations(current_date, n_games_back = 10L)
usage_eff_base   <- build_usage_eff_baseline(current_date, n_games_back = 10L)

# -----------------------------
# 3) Availability audit table (player-level)
# -----------------------------
inj_out_slim <- roster_out %>%
  transmute(
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    last_status    = "out"
  ) %>%
  distinct()

minutes_availability_df <- roster_active %>%
  transmute(
    date_key       = k,
    GAME_DATE      = current_date,
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    NBA_TEAM_ID    = as.character(NBA_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    PLAYER         = as.character(PLAYER)
  ) %>%
  distinct() %>%
  left_join(minutes_baseline, by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  left_join(usage_eff_base,   by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  left_join(inj_out_slim,     by = c("ESPN_TEAM_ID","ESPN_PLAYER_ID")) %>%
  mutate(
    last_status = ifelse(is.na(last_status), "active", last_status),
    expected_in = as.integer(last_status != "out"),
    expected_minutes = ifelse(is.na(mins_mean), 0, pmax(0, mins_mean)),
    role_bucket      = role_bucket_from_min(expected_minutes)
  )

# -----------------------------
# 4) Redistribution (minutes) — remove OUT minutes, reallocate to actives by team
# -----------------------------
out_pool <- minutes_availability_df %>%
  filter(last_status == "out") %>%
  mutate(
    out_minutes_est = num0(expected_minutes, 0),
    out_usg_est     = baseline_usg,
    out_pts_pm      = pts_per_min
  ) %>%
  select(date_key, GAME_DATE, ESPN_TEAM_ID, NBA_TEAM_ID, ESPN_PLAYER_ID, PLAYER,
         out_minutes_est, out_usg_est, out_pts_pm)

out_team_totals <- out_pool %>%
  group_by(date_key, GAME_DATE, ESPN_TEAM_ID, NBA_TEAM_ID) %>%
  summarise(
    out_players_n     = n(),
    out_minutes_total = sum(out_minutes_est, na.rm = TRUE),
    out_usg_avg       = mean(out_usg_est, na.rm = TRUE),
    out_pts_pm_avg    = mean(out_pts_pm, na.rm = TRUE),
    out_players       = paste0(ESPN_PLAYER_ID, collapse = ","),
    .groups = "drop"
  )

alloc_df <- minutes_availability_df %>%
  filter(last_status == "active") %>%
  mutate(
    role_w = case_when(
      role_bucket == "starter"  ~ 1.25,
      role_bucket == "rotation" ~ 1.00,
      role_bucket == "bench"    ~ 0.70,
      TRUE                      ~ 0.40
    ),
    usg_w  = ifelse(is.na(baseline_usg), 1.0, 1.0 + clamp01(baseline_usg / 40)),
    alloc_weight = pmax(0.01, role_w * usg_w)
  ) %>%
  left_join(out_team_totals, by = c("date_key","GAME_DATE","ESPN_TEAM_ID","NBA_TEAM_ID")) %>%
  mutate(out_minutes_total = num0(out_minutes_total, 0)) %>%
  group_by(date_key, GAME_DATE, ESPN_TEAM_ID, NBA_TEAM_ID) %>%
  mutate(
    wsum = sum(alloc_weight, na.rm = TRUE),
    add_minutes = ifelse(wsum > 0, out_minutes_total * (alloc_weight / wsum), 0),
    expected_minutes_adj = pmax(0, expected_minutes + add_minutes)
  ) %>%
  ungroup() %>%
  select(-wsum)

# -----------------------------
# 5) Team-level redistribution audit summary
# -----------------------------
minutes_redistribution_team_df <- alloc_df %>%
  group_by(date_key, GAME_DATE, ESPN_TEAM_ID, NBA_TEAM_ID) %>%
  summarise(
    out_players_n       = max(num0(out_players_n, 0)),
    out_minutes_total   = max(num0(out_minutes_total, 0)),
    active_players_n    = n(),
    active_minutes_base = sum(expected_minutes, na.rm = TRUE),
    active_minutes_adj  = sum(expected_minutes_adj, na.rm = TRUE),
    minutes_added_back  = sum(add_minutes, na.rm = TRUE),
    
    baseline_usg_team_avg = mean(baseline_usg, na.rm = TRUE),
    baseline_pts_pm_avg   = mean(pts_per_min, na.rm = TRUE),
    
    top_receivers = paste0(
      ESPN_PLAYER_ID[order(add_minutes, decreasing = TRUE)][1:min(5, n())],
      collapse = ","
    ),
    .groups = "drop"
  )

# -----------------------------
# 6) Monte Carlo minutes draw (per-player) on adjusted expectation
#    NOTE: minutes_sim is ONE random draw (one scenario) around expected_minutes_adj.
# -----------------------------
set.seed(123)

player_minutes_mc <- alloc_df %>%
  rowwise() %>%
  mutate(
    minutes_sd  = pmax(2, ifelse(is.na(mins_sd), expected_minutes_adj * 0.12, mins_sd)),
    minutes_sim = rnorm(1, expected_minutes_adj, minutes_sd),
    minutes_sim = pmin(48, pmax(0, minutes_sim))  # keep sane for now
  ) %>%
  ungroup()

# Outputs you can View():
# View(minutes_availability_df)
# View(minutes_redistribution_team_df)
# View(player_minutes_mc)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Minutes Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Pull in Team Names and Opp Names
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ------------------------------------------------------------
# Add TEAM / OPP / HOME_AWAY fields onto player_minutes_mc
# (match ESPN_TEAM_ID -> schedule_df$team_id)
# ------------------------------------------------------------

stopifnot(exists("player_minutes_mc"), exists("schedule_df"))
stopifnot(all(c("ESPN_TEAM_ID") %in% names(player_minutes_mc)))
stopifnot(all(c("team_id","team","opp_id","opp","home_away_sym","home_away") %in% names(schedule_df)))

player_minutes_mc <- player_minutes_mc %>%
  mutate(ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)) %>%
  left_join(
    schedule_df %>%
      transmute(
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
# ======================================
# START: Points Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# -----------------------------
# C) Build POINTS PER MINUTE (PPM) baseline
#     PTS from BaseStats_Player_MC (PTS_CGS)
#     MINS from player_rotations_clean (MINS_NUM)
# -----------------------------
build_ppm_baseline <- function(current_date, n_games_back = 10L) {
  
  pts_hist <- BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      PTS_NUM        = num0(PTS_CGS, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, PTS_NUM)
  
  mins_hist <- player_rotations_clean %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      MINS_NUM       = num0(MINS_NUM, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, MINS_NUM)
  
  joined <- pts_hist %>%
    inner_join(mins_hist, by = c("game_date_dt", "ESPN_PLAYER_ID", "ESPN_TEAM_ID")) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, game_date_dt)
  
  joined %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      pts_lastN   = sum(PTS_NUM,  na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      games_used  = dplyr::n(),
      pts_per_min = ifelse(mins_lastN > 0, pts_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# -----------------------------
# D) PATCH pts_per_min + points projections into player_minutes_mc (NO JOINS)
# -----------------------------
ppm_baseline_df <- build_ppm_baseline(current_date, n_games_back = 10L)

# --- ensure character keys ---
player_minutes_mc$ESPN_PLAYER_ID <- as.character(player_minutes_mc$ESPN_PLAYER_ID)
player_minutes_mc$ESPN_TEAM_ID   <- as.character(player_minutes_mc$ESPN_TEAM_ID)
ppm_baseline_df$ESPN_PLAYER_ID   <- as.character(ppm_baseline_df$ESPN_PLAYER_ID)
ppm_baseline_df$ESPN_TEAM_ID     <- as.character(ppm_baseline_df$ESPN_TEAM_ID)

# --- build fast lookup key ---
ppm_baseline_df$key <- paste0(ppm_baseline_df$ESPN_PLAYER_ID, "_", ppm_baseline_df$ESPN_TEAM_ID)
ppm_map <- setNames(ppm_baseline_df$pts_per_min, ppm_baseline_df$key)

player_minutes_mc$key <- paste0(player_minutes_mc$ESPN_PLAYER_ID, "_", player_minutes_mc$ESPN_TEAM_ID)

# --- PATCH: overwrite/assign pts_per_min safely ---
player_minutes_mc$pts_per_min <- unname(ppm_map[player_minutes_mc$key])

# --- projections ---
player_minutes_mc$points_proj <- ifelse(
  !is.na(player_minutes_mc$pts_per_min),
  player_minutes_mc$expected_minutes_adj * player_minutes_mc$pts_per_min,
  NA_real_
)

player_minutes_mc$points_sim <- ifelse(
  !is.na(player_minutes_mc$pts_per_min),
  player_minutes_mc$minutes_sim * player_minutes_mc$pts_per_min,
  NA_real_
)

# --- cleanup temp key ---
player_minutes_mc$key <- NULL
ppm_baseline_df$key   <- NULL
rm(ppm_map)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Points Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Rebounds Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# -----------------------------
# C) Build REBOUNDS PER MINUTE (RPM) baseline
#     REB from BaseStats_Player_MC (REB_CGS)   <-- change to your exact rebounds column name if different
#     MINS from player_rotations_clean (MINS_NUM)
# -----------------------------
build_rpm_baseline <- function(current_date, n_games_back = 10L) {
  
  reb_hist <- BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      REB_NUM        = num0(REB_CGS, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, REB_NUM)
  
  mins_hist <- player_rotations_clean %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      MINS_NUM       = num0(MINS_NUM, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, MINS_NUM)
  
  joined <- reb_hist %>%
    inner_join(mins_hist, by = c("game_date_dt", "ESPN_PLAYER_ID", "ESPN_TEAM_ID")) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, game_date_dt)
  
  joined %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      reb_lastN   = sum(REB_NUM,  na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      games_used  = dplyr::n(),
      reb_per_min = ifelse(mins_lastN > 0, reb_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# -----------------------------
# D) PATCH reb_per_min + rebound projections into player_minutes_mc (NO JOINS)
# -----------------------------
rpm_baseline_df <- build_rpm_baseline(current_date, n_games_back = 10L)

# --- ensure character keys ---
player_minutes_mc$ESPN_PLAYER_ID <- as.character(player_minutes_mc$ESPN_PLAYER_ID)
player_minutes_mc$ESPN_TEAM_ID   <- as.character(player_minutes_mc$ESPN_TEAM_ID)
rpm_baseline_df$ESPN_PLAYER_ID   <- as.character(rpm_baseline_df$ESPN_PLAYER_ID)
rpm_baseline_df$ESPN_TEAM_ID     <- as.character(rpm_baseline_df$ESPN_TEAM_ID)

# --- build fast lookup key ---
rpm_baseline_df$key <- paste0(rpm_baseline_df$ESPN_PLAYER_ID, "_", rpm_baseline_df$ESPN_TEAM_ID)
rpm_map <- setNames(rpm_baseline_df$reb_per_min, rpm_baseline_df$key)

player_minutes_mc$key <- paste0(player_minutes_mc$ESPN_PLAYER_ID, "_", player_minutes_mc$ESPN_TEAM_ID)

# --- PATCH: overwrite/assign reb_per_min safely ---
player_minutes_mc$reb_per_min <- unname(rpm_map[player_minutes_mc$key])

# --- projections ---
player_minutes_mc$rebounds_proj <- ifelse(
  !is.na(player_minutes_mc$reb_per_min),
  player_minutes_mc$expected_minutes_adj * player_minutes_mc$reb_per_min,
  NA_real_
)

player_minutes_mc$rebounds_sim <- ifelse(
  !is.na(player_minutes_mc$reb_per_min),
  player_minutes_mc$minutes_sim * player_minutes_mc$reb_per_min,
  NA_real_
)

# --- cleanup temp key ---
player_minutes_mc$key <- NULL
rpm_baseline_df$key   <- NULL
rm(rpm_map)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Rebounds Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Assists Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# -----------------------------
# C) Build ASSISTS PER MINUTE (APM) baseline
#     AST from BaseStats_Player_MC (AST_CGS)   <-- change to your exact assists column name if different
#     MINS from player_rotations_clean (MINS_NUM)
# -----------------------------
build_apm_baseline <- function(current_date, n_games_back = 10L) {
  
  ast_hist <- BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      AST_NUM        = num0(AST_CGS, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, AST_NUM)
  
  mins_hist <- player_rotations_clean %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      MINS_NUM       = num0(MINS_NUM, NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < current_date) %>%
    select(game_date_dt, ESPN_PLAYER_ID, ESPN_TEAM_ID, MINS_NUM)
  
  joined <- ast_hist %>%
    inner_join(mins_hist, by = c("game_date_dt", "ESPN_PLAYER_ID", "ESPN_TEAM_ID")) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, game_date_dt)
  
  joined %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      ast_lastN   = sum(AST_NUM,  na.rm = TRUE),
      mins_lastN  = sum(MINS_NUM, na.rm = TRUE),
      games_used  = dplyr::n(),
      ast_per_min = ifelse(mins_lastN > 0, ast_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
}

# -----------------------------
# D) PATCH ast_per_min + assist projections into player_minutes_mc (NO JOINS)
# -----------------------------
apm_baseline_df <- build_apm_baseline(current_date, n_games_back = 10L)

# --- ensure character keys ---
player_minutes_mc$ESPN_PLAYER_ID <- as.character(player_minutes_mc$ESPN_PLAYER_ID)
player_minutes_mc$ESPN_TEAM_ID   <- as.character(player_minutes_mc$ESPN_TEAM_ID)
apm_baseline_df$ESPN_PLAYER_ID   <- as.character(apm_baseline_df$ESPN_PLAYER_ID)
apm_baseline_df$ESPN_TEAM_ID     <- as.character(apm_baseline_df$ESPN_TEAM_ID)

# --- build fast lookup key ---
apm_baseline_df$key <- paste0(apm_baseline_df$ESPN_PLAYER_ID, "_", apm_baseline_df$ESPN_TEAM_ID)
apm_map <- setNames(apm_baseline_df$ast_per_min, apm_baseline_df$key)

player_minutes_mc$key <- paste0(player_minutes_mc$ESPN_PLAYER_ID, "_", player_minutes_mc$ESPN_TEAM_ID)

# --- PATCH: overwrite/assign ast_per_min safely ---
player_minutes_mc$ast_per_min <- unname(apm_map[player_minutes_mc$key])

# --- projections ---
player_minutes_mc$assists_proj <- ifelse(
  !is.na(player_minutes_mc$ast_per_min),
  player_minutes_mc$expected_minutes_adj * player_minutes_mc$ast_per_min,
  NA_real_
)

player_minutes_mc$assists_sim <- ifelse(
  !is.na(player_minutes_mc$ast_per_min),
  player_minutes_mc$minutes_sim * player_minutes_mc$ast_per_min,
  NA_real_
)

# --- cleanup temp key ---
player_minutes_mc$key <- NULL
apm_baseline_df$key   <- NULL
rm(apm_map)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Assists Projection (PLAYER_ROTATIONS-Based) + AUDIT + REDISTRIBUTION
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# START: Build OUT impact columns from injury_data (team-level) + PATCH (NO JOINS)
#   Fills:
#     out_players_n, out_minutes_total, out_usg_avg, out_pts_pm_avg, out_players, add_minutes
#   Uses:
#     PRIMARY: injury_team_summary_df (canonical, slate-aligned)
#     FALLBACK: injury_data + BaseStats_Player_MC (last-N baseline)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(exists("player_minutes_mc"))
stopifnot(exists("injury_data"))
stopifnot(exists("BaseStats_Player_MC"))

# -----------------------------
# A) Build player baselines (mins_per_game, usg, pts_per_min) from BaseStats_Player_MC (FALLBACK support)
#   NOTE: adjust these column names ONLY if yours differ:
#     - minutes: MINS_CGS
#     - usage:   USG_CGS
#     - points:  PTS_CGS
# -----------------------------
build_player_out_baselines <- function(current_date, n_games_back = 10L) {
  
  bs <- BaseStats_Player_MC %>%
    mutate(
      game_date_dt   = suppressWarnings(as.Date(GAME_DATE)),
      ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
      ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
      MINS_NUM       = num0(MINS_CGS, NA_real_),
      USG_NUM        = num0(USG_CGS,  NA_real_),
      PTS_NUM        = num0(PTS_CGS,  NA_real_)
    ) %>%
    filter(!is.na(game_date_dt), game_date_dt < as.Date(current_date)) %>%
    arrange(ESPN_PLAYER_ID, ESPN_TEAM_ID, game_date_dt) %>%
    group_by(ESPN_PLAYER_ID, ESPN_TEAM_ID) %>%
    slice_tail(n = n_games_back) %>%
    summarise(
      mins_lastN   = sum(MINS_NUM, na.rm = TRUE),
      pts_lastN    = sum(PTS_NUM,  na.rm = TRUE),
      usg_mean     = ifelse(all(is.na(USG_NUM)), NA_real_, mean(USG_NUM, na.rm = TRUE)),
      games_used   = dplyr::n(),
      mins_per_g   = ifelse(games_used > 0, mins_lastN / games_used, NA_real_),
      pts_per_min  = ifelse(mins_lastN > 0, pts_lastN / mins_lastN, NA_real_),
      .groups = "drop"
    )
  
  bs$key <- paste0(bs$ESPN_PLAYER_ID, "_", bs$ESPN_TEAM_ID)
  bs
}

# -----------------------------
# B) Build team-level OUT impact
#   PRIMARY: injury_team_summary_df (canonical team OUT state)
#   FALLBACK: injury_data date/status filter + BaseStats_Player_MC baselines
# -----------------------------
build_team_out_impact <- function(current_date, n_games_back = 10L, verbose = FALSE) {
  
  # ---- PRIMARY PATH: injury_team_summary_df ----
  if (exists("injury_team_summary_df")) {
    
    # expected columns: espn_team_id, out_players_n, out_minutes_total, out_usg_avg, out_pts_pm_avg, out_players
    needed <- c("espn_team_id","out_players_n","out_minutes_total","out_usg_avg","out_pts_pm_avg","out_players")
    have   <- names(injury_team_summary_df)
    
    if (all(needed %in% have)) {
      
      out_df <- injury_team_summary_df %>%
        mutate(
          ESPN_TEAM_ID      = as.character(espn_team_id),
          out_players_n     = as.integer(num0(out_players_n, 0)),
          out_minutes_total = num0(out_minutes_total, 0),
          out_usg_avg       = num0(out_usg_avg, NA_real_),
          out_pts_pm_avg    = num0(out_pts_pm_avg, NA_real_),
          out_players       = as.character(out_players),
          add_minutes       = num0(out_minutes_total, 0)
        ) %>%
        select(
          ESPN_TEAM_ID,
          out_players_n,
          out_minutes_total,
          out_usg_avg,
          out_pts_pm_avg,
          out_players,
          add_minutes
        )
      
      return(out_df)
    }
    
    if (verbose) {
      cat("[OUT IMPACT] injury_team_summary_df exists but missing needed cols:\n")
      print(setdiff(needed, have))
      cat("[OUT IMPACT] Falling back to injury_data method.\n")
    }
  }
  
  # ---- FALLBACK PATH: injury_data + BaseStats_Player_MC ----
  slate_date <- suppressWarnings(as.Date(current_date))
  
  inj <- injury_data %>%
    mutate(
      date_dt        = suppressWarnings(as.Date(date)),
      status_clean   = toupper(trimws(as.character(status))),
      ESPN_TEAM_ID   = as.character(espn_team_id),
      ESPN_PLAYER_ID = as.character(espn_player_id),
      player_clean   = as.character(player_clean)
    )
  
  out_raw <- inj %>%
    filter(!is.na(date_dt), date_dt == slate_date) %>%
    filter(!is.na(status_clean) & status_clean == "OUT") %>%
    select(ESPN_TEAM_ID, ESPN_PLAYER_ID, player_clean) %>%
    distinct()
  
  if (verbose) {
    cat("\n[OUT IMPACT] FALLBACK MODE\n")
    cat("[OUT IMPACT] slate_date:", as.character(slate_date), "\n")
    cat("[OUT IMPACT] rows on slate_date:", sum(inj$date_dt == slate_date, na.rm = TRUE), "\n")
    cat("[OUT IMPACT] OUT rows on slate_date:", nrow(out_raw), "\n")
  }
  
  if (nrow(out_raw) == 0L) {
    return(
      tibble::tibble(
        ESPN_TEAM_ID      = character(),
        out_players_n     = integer(),
        out_minutes_total = numeric(),
        out_usg_avg       = numeric(),
        out_pts_pm_avg    = numeric(),
        out_players       = character(),
        add_minutes       = numeric()
      )
    )
  }
  
  base <- build_player_out_baselines(slate_date, n_games_back = n_games_back)
  
  out_raw$key <- paste0(out_raw$ESPN_PLAYER_ID, "_", out_raw$ESPN_TEAM_ID)
  
  mins_map <- setNames(base$mins_per_g,  base$key)
  usg_map  <- setNames(base$usg_mean,    base$key)
  ppm_map  <- setNames(base$pts_per_min, base$key)
  
  out_raw$mins_per_g  <- unname(mins_map[out_raw$key])
  out_raw$usg_mean    <- unname(usg_map[out_raw$key])
  out_raw$pts_per_min <- unname(ppm_map[out_raw$key])
  
  out_raw$key <- NULL
  rm(mins_map, usg_map, ppm_map, base)
  
  out_raw %>%
    group_by(ESPN_TEAM_ID) %>%
    summarise(
      out_players_n     = dplyr::n(),
      out_minutes_total = sum(mins_per_g, na.rm = TRUE),
      out_usg_avg = {
        w <- mins_per_g
        ifelse(sum(w, na.rm = TRUE) > 0,
               sum(usg_mean * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
               NA_real_)
      },
      out_pts_pm_avg = {
        w <- mins_per_g
        ifelse(sum(w, na.rm = TRUE) > 0,
               sum(pts_per_min * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
               NA_real_)
      },
      out_players = paste0(unique(na.omit(player_clean)), collapse = ", "),
      add_minutes = sum(mins_per_g, na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------
# C) PATCH team OUT impact into player_minutes_mc (NO JOINS)
# -----------------------------
out_impact_df <- build_team_out_impact(current_date, n_games_back = 10L, verbose = FALSE)

player_minutes_mc$ESPN_TEAM_ID <- as.character(player_minutes_mc$ESPN_TEAM_ID)
out_impact_df$ESPN_TEAM_ID     <- as.character(out_impact_df$ESPN_TEAM_ID)

out_n_map    <- setNames(out_impact_df$out_players_n,     out_impact_df$ESPN_TEAM_ID)
out_min_map  <- setNames(out_impact_df$out_minutes_total, out_impact_df$ESPN_TEAM_ID)
out_usg_map  <- setNames(out_impact_df$out_usg_avg,       out_impact_df$ESPN_TEAM_ID)
out_ppm_map  <- setNames(out_impact_df$out_pts_pm_avg,    out_impact_df$ESPN_TEAM_ID)
out_list_map <- setNames(out_impact_df$out_players,       out_impact_df$ESPN_TEAM_ID)
add_min_map  <- setNames(out_impact_df$add_minutes,       out_impact_df$ESPN_TEAM_ID)

player_minutes_mc$out_players_n     <- unname(out_n_map[player_minutes_mc$ESPN_TEAM_ID])
player_minutes_mc$out_minutes_total <- unname(out_min_map[player_minutes_mc$ESPN_TEAM_ID])
player_minutes_mc$out_usg_avg       <- unname(out_usg_map[player_minutes_mc$ESPN_TEAM_ID])
player_minutes_mc$out_pts_pm_avg    <- unname(out_ppm_map[player_minutes_mc$ESPN_TEAM_ID])
player_minutes_mc$out_players       <- unname(out_list_map[player_minutes_mc$ESPN_TEAM_ID])
player_minutes_mc$add_minutes       <- unname(add_min_map[player_minutes_mc$ESPN_TEAM_ID])

# defaults for teams with no OUTs
player_minutes_mc$out_players_n     <- ifelse(is.na(player_minutes_mc$out_players_n), 0L, player_minutes_mc$out_players_n)
player_minutes_mc$out_minutes_total <- ifelse(is.na(player_minutes_mc$out_minutes_total), 0,  player_minutes_mc$out_minutes_total)
player_minutes_mc$add_minutes       <- ifelse(is.na(player_minutes_mc$add_minutes), 0,  player_minutes_mc$add_minutes)
# leave out_usg_avg / out_pts_pm_avg as NA when no outs (correct)
# leave out_players as NA when no outs (fine for audit)

rm(out_impact_df, out_n_map, out_min_map, out_usg_map, out_ppm_map, out_list_map, add_min_map)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ======================================
# END: Build OUT impact columns from injury_data (team-level) + PATCH (NO JOINS)
# ======================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# Enrich player_minutes_mc with FanDuel (FD) player odds + clean names
# Match keys:
#   player_minutes_mc$GAME_DATE  <-> player_odds_enrich_df$GAME_DATE_CTR
#   player_minutes_mc$ESPN_PLAYER_ID <-> player_odds_enrich_df$ESPN_PLAYER_ID
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# Enrich player_minutes_mc with FanDuel (FD) player odds + clean names
# Join keys (robust):
#   ODDS_API_MAP + ESPN_PLAYER_ID  (both normalized)
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 🏀============================================================
# Enrich player_minutes_mc with FanDuel odds via ODDS_API_MAP
# Join keys:
#   ODDS_API_MAP + ESPN_PLAYER_ID
# Assumes player_odds_enrich_df$GAME_DATE_CTR is already "yyyy-MM-dd"
# 🏀============================================================

stopifnot(exists("player_minutes_mc"), exists("player_odds_enrich_df"))

# --- projections side key ---
player_minutes_mc <- player_minutes_mc %>%
  mutate(
    GAME_DATE      = as.character(GAME_DATE),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    TEAM           = as.character(TEAM),
    OPP            = as.character(OPP),
    ODDS_API_MAP   = paste0(TEAM, "_", OPP, "_", GAME_DATE)
  )

# --- odds side key (NO date conversion) ---
odds_fd_slim <- player_odds_enrich_df %>%
  mutate(
    GAME_DATE      = as.character(GAME_DATE_CTR),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    TEAM           = as.character(TEAM),
    OPP            = as.character(OPP),
    ODDS_API_MAP   = paste0(TEAM, "_", OPP, "_", GAME_DATE)
  ) %>%
  select(
    ODDS_API_MAP,
    GAME_DATE,
    ESPN_PLAYER_ID,
    PLAYER,
    NUM,
    HEADSHOT,
    TEAM_LOGO,
    OPP_LOGO,
    PTS_FD_LINE_O,
    PTS_FD_ODDS_O,
    REB_FD_LINE_O,
    REB_FD_ODDS_O,
    AST_FD_LINE_O,
    AST_FD_ODDS_O,
    `3PM_FD_LINE_O`,
    `3PM_FD_ODDS_O`
  ) %>%
  distinct(ODDS_API_MAP, ESPN_PLAYER_ID, .keep_all = TRUE)

# --- join + overwrite PLAYER with clean odds name ---
player_minutes_mc <- player_minutes_mc %>%
  left_join(
    odds_fd_slim,
    by = c("ODDS_API_MAP", "ESPN_PLAYER_ID"),
    suffix = c("", "_odds")
  ) %>%
  mutate(
    PLAYER = ifelse(!is.na(PLAYER_odds) & PLAYER_odds != "", PLAYER_odds, PLAYER)
  ) %>%
  select(-PLAYER_odds)

# 🏀============================================================
# END
# 🏀============================================================

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Enrich player_minutes_mc with FanDuel (FD) player odds + clean names
# Match keys:
#   player_minutes_mc$GAME_DATE  <-> player_odds_enrich_df$GAME_DATE_CTR
#   player_minutes_mc$ESPN_PLAYER_ID <-> player_odds_enrich_df$ESPN_PLAYER_ID
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

dir.create("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/0. Prediction Data/player_predictions", 
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(
  player_minutes_mc,
  paste0(
    "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/0. Prediction Data/player_predictions/player_minutes_mc_",
    formatted_date,
    ".csv"
  )
)



