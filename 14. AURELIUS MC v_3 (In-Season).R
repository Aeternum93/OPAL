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

plan(multisession, workers = 5)
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
# ============================================================
# START. VARIANCE CONTROLS (STD / randomness levers)
# Keep these limited so you don't "fit the sim"
# ============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# ---------------------------
# Variance controls (add-ons)
# ---------------------------

MC_VAR <- list(
  # 1) possessions SD multiplier (lower = tighter pace)
  poss_sd_mult = 0.75,

  # 2) shooting % shrink toward league-ish mean (0 = no shrink, 1 = fully league mean)
  shoot_shrink = 0.20,

  # 2b) pct jitter (adds randomness AFTER shrink; per-sim draw)
  #     Typical sane range: 0.005 to 0.020
  pct_jitter_sd = 0.00,

  # 3) turnover randomness strength (0..1). Higher = more random TO outcomes.
  to_rand = 0.25,

  # 4) rebounding / 2nd-chance levers
  #    reb_sd_mult: increases/decreases volatility of OREB / 2nd chance draws
  #    reb_mean_mult: shifts level of OREB / 2nd chance volume (optional)
  reb_sd_mult   = 1.00,
  reb_mean_mult = 1.00
)

clamp01 <- function(x) pmin(pmax(x, 0), 1)

# pct jitter helper: apply to probabilities (FG%, FT%) after shrink
apply_pct_jitter <- function(p, jitter_sd) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return(NA_real_)
  if (is.na(jitter_sd) || jitter_sd <= 0) return(clamp01(p))
  clamp01(rnorm(1, mean = p, sd = jitter_sd))
}


# reb/2ch scaling helper: apply to mean/sd inputs before draw_attempts()
scale_reb_profile <- function(mu, sd, mean_mult = 1.0, sd_mult = 1.0) {
  mu <- suppressWarnings(as.numeric(mu))
  sd <- suppressWarnings(as.numeric(sd))
  if (is.na(mu)) mu <- 0
  if (is.na(sd)) sd <- 0
  list(
    mu = pmax(0, mu * mean_mult),
    sd = pmax(0, sd * sd_mult)
  )
}


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ============================================================
# END VARIANCE CONTROLS (STD / randomness levers)
# Keep these limited so you don't "fit the sim"
# ============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 1. START: Build DAILY + ROLLING split UNITS (for MC backtests)
#   - NO materialized bt_* dataframes (keep as lists for backend processing)
#   - Adds FULL SEASON PBP window (season-to-date) alongside rolling windows
#   - Uses nba_schedule_season for date windows + slate filtering
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

if (!"game_date_dt" %in% names(pbp_df)) stop("pbp_df missing game_date_dt. Fix Section 0 first.")
if (all(is.na(pbp_df$game_date_dt))) stop("pbp_df game_date_dt is all NA. Backtests need valid dates.")
stopifnot(exists("nba_schedule_season"))

today_dt <- as.Date(current_date)

# ---- Identify schedule date column & normalize to Date ----
sched_date_col <- if ("GAME_DATE" %in% names(nba_schedule_season)) {
  "GAME_DATE"
} else if ("game_date" %in% names(nba_schedule_season)) {
  "game_date"
} else if ("date" %in% names(nba_schedule_season)) {
  "date"
} else {
  stop("nba_schedule_season missing a schedule date column (expected GAME_DATE or game_date or date).")
}

nba_schedule_season_clean <- nba_schedule_season %>%
  dplyr::mutate(
    game_date_dt = suppressWarnings(as.Date(.data[[sched_date_col]]))
  ) %>%
  dplyr::filter(!is.na(game_date_dt))

# ---- test_dates now comes from nba_schedule_season ----
test_dates <- nba_schedule_season_clean %>%
  dplyr::distinct(game_date_dt) %>%
  dplyr::filter(game_date_dt < today_dt) %>%
  dplyr::arrange(game_date_dt) %>%
  dplyr::pull(game_date_dt)

if (length(test_dates) == 0) stop("No eligible test_dates found in nba_schedule_season prior to today.")

# ---- FULL SEASON START (season-to-date anchor) ----
season_start_dt <- min(nba_schedule_season_clean$game_date_dt, na.rm = TRUE)
if (is.infinite(season_start_dt)) stop("Could not determine season_start_dt from nba_schedule_season_clean.")

# ---- Rolling windows (per-team last N games) ----
rolling_n_vec <- c(5L, 7L, 10L, 15L, 25L, 35L)

build_roll_pbp <- function(pbp_train_all, n_games) {
  team_games <- pbp_train_all %>%
    dplyr::distinct(team_id, game_id, game_date_dt) %>%
    dplyr::arrange(team_id, game_date_dt, game_id) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(
      game_num = dplyr::row_number(),
      max_num  = max(game_num)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(game_num > (max_num - n_games)) %>%
    dplyr::select(team_id, game_id) %>%
    dplyr::distinct()
  
  pbp_train_all %>%
    dplyr::inner_join(team_games, by = c("team_id", "game_id"))
}

# ---- schedule fetch is a filter on nba_schedule_season ----
get_schedule_for_date <- function(test_date) {
  test_date <- as.Date(test_date)
  
  nba_schedule_season_clean %>%
    dplyr::filter(game_date_dt == test_date) %>%
    dplyr::select(-game_date_dt)
}

build_date_splits <- function(test_date) {
  test_date <- as.Date(test_date)
  date_key  <- format(test_date, "%Y%m%d")
  
  # -----------------------------
  # Train pools (strictly before test_date)
  # -----------------------------
  pbp_train_all <- pbp_df %>%
    dplyr::filter(!is.na(game_date_dt), game_date_dt < test_date)
  
  # FULL SEASON (season-to-date) = same as "all before test_date" but anchored to season_start_dt
  # (kept separate so you can treat it as a named window in the MC)
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
  
  # -----------------------------
  # Slate to predict (THIS test_date)
  # -----------------------------
  schedule_df <- get_schedule_for_date(current_date)
  
  # -----------------------------
  # Rolling pbps (built off pbp_train_all)
  # -----------------------------
  rolling_list <- purrr::map(rolling_n_vec, function(n) {
    build_roll_pbp(pbp_train_all, n_games = n)
  })
  names(rolling_list) <- paste0("roll_", rolling_n_vec)
  
  # Optional: include season as another "window" in the same list for MC iteration
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

# -----------------------------
# build only the last N days
# -----------------------------
n_test_days_to_build <- 7L
test_dates_subset <- tail(test_dates, n_test_days_to_build)

daily_units   <- list()
rolling_units <- list()

for (d in test_dates_subset) {
  obj <- build_date_splits(d)
  k <- obj$date_key
  
  # Daily unit (expanding/all-history + season + slate + base team)
  daily_units[[k]] <- list(
    test_date       = obj$test_date,
    schedule_df     = obj$schedule_df,
    pbp_train_all   = obj$pbp_train_all,
    pbp_season      = obj$pbp_season,
    base_team_train = obj$base_team_train
  )
  
  # Rolling unit (roll_5...roll_35 + season)
  rolling_units[[k]] <- obj$rolling_pbps
  
  message(
    "Built splits for: ", k,
    " | train_pbp_rows=", nrow(obj$pbp_train_all),
    " | season_pbp_rows=", nrow(obj$pbp_season),
    " | schedule_rows=", nrow(obj$schedule_df)
  )
}

# ============================================================
# TODAY SLATE UNIT (unseen games) — schedule from nba_schedule_season
# ============================================================

today_key <- format(today_dt, "%Y%m%d")

today_pbp_train_all_df <- pbp_df %>%
  dplyr::filter(!is.na(game_date_dt), game_date_dt < today_dt)

today_pbp_season_df <- pbp_df %>%
  dplyr::filter(!is.na(game_date_dt),
                game_date_dt >= season_start_dt,
                game_date_dt <  today_dt)

today_base_team_train_df <- BaseStats_Team_MC
if ("game_date" %in% names(BaseStats_Team_MC)) {
  today_base_team_train_df <- BaseStats_Team_MC %>%
    dplyr::mutate(game_date_dt = suppressWarnings(as.Date(game_date))) %>%
    dplyr::filter(is.na(game_date_dt) | game_date_dt < today_dt) %>%
    dplyr::select(-game_date_dt)
}

today_schedule_df <- get_schedule_for_date(today_dt) %>%
  dplyr::mutate(date_key = today_key, test_date = today_dt)

today_roll_list <- purrr::map(rolling_n_vec, function(n) {
  build_roll_pbp(today_pbp_train_all_df, n_games = n)
})
names(today_roll_list) <- paste0("roll_", rolling_n_vec)

# add season window into the same list for MC iteration
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
# 2. START: MC predictions driven by POSSESSIONS as an INPUT DISTRIBUTION
#   - Shared pace draw per quarter (one game pace), then split between teams
#   - Simulate points given possessions using team shooting/TO/foul probabilities
#   - OT only if tied (uses Q5/Q6 if present)
#   NOTE: This is not a separate "possessions model"—it's an input distribution inside the sim.
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# ---- helper: draw ONE shared game possessions number for the quarter ----
draw_game_possessions_q <- function(home_team_id, away_team_id,
                                    team_poss_profile_q,
                                    qtr,
                                    sd_shrink = 0.75,
                                    min_poss = 15L,
                                    max_poss = 35L) {
  
  h <- team_poss_profile_q %>% filter(team_id == home_team_id, qtr == !!qtr) %>% slice(1)
  a <- team_poss_profile_q %>% filter(team_id == away_team_id, qtr == !!qtr) %>% slice(1)
  
  h_mu <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_mean)) else NA_real_
  a_mu <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_mean)) else NA_real_
  
  h_sd <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_sd)) else 0
  a_sd <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_sd)) else 0
  
  if (is.na(h_mu) || h_mu <= 0) h_mu <- 24
  if (is.na(a_mu) || a_mu <= 0) a_mu <- 24
  if (is.na(h_sd) || h_sd < 0)  h_sd <- 0
  if (is.na(a_sd) || a_sd < 0)  a_sd <- 0
  
  game_mu <- (h_mu + a_mu) / 2
  game_sd <- (sqrt(h_sd^2 + a_sd^2) / 2) * sd_shrink
  
  val <- round(rnorm(1, mean = game_mu, sd = game_sd))
  val <- as.integer(pmin(pmax(val, min_poss), max_poss))
  val
}

# ---- helper: split the shared quarter possessions between teams ----
split_game_possessions_q <- function(game_poss, home_team_id, away_team_id,
                                     team_poss_profile_q, qtr) {
  
  h <- team_poss_profile_q %>% filter(team_id == home_team_id, qtr == !!qtr) %>% slice(1)
  a <- team_poss_profile_q %>% filter(team_id == away_team_id, qtr == !!qtr) %>% slice(1)
  
  h_mu <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$POSS_mean)) else 24
  a_mu <- if (nrow(a) > 0) suppressWarnings(as.numeric(a$POSS_mean)) else 24
  
  if (is.na(h_mu) || h_mu <= 0) h_mu <- 24
  if (is.na(a_mu) || a_mu <= 0) a_mu <- 24
  
  total_mu <- h_mu + a_mu
  home_share <- if (total_mu > 0) h_mu / total_mu else 0.5
  
  home_poss <- as.integer(round(game_poss * home_share))
  away_poss <- as.integer(game_poss - home_poss)
  
  # guardrails (no negatives)
  if (home_poss < 0) home_poss <- 0L
  if (away_poss < 0) away_poss <- 0L
  
  list(home = home_poss, away = away_poss)
}

# ---- possessions-driven game sim (shared-pace version) ----
simulate_game_once_poss_only <- function(home_team_id, away_team_id,
                                         season_chosen, team_mc_profile,
                                         team_poss_profile_q,
                                         team_to_profile_q,
                                         team_foul_profile_q,
                                         team_pts_profile_q,
                                         max_ot = 2L) {
  
  home_row <- team_mc_profile %>% filter(team_id == home_team_id, season == season_chosen) %>% slice(1)
  away_row <- team_mc_profile %>% filter(team_id == away_team_id, season == season_chosen) %>% slice(1)
  if (nrow(home_row) == 0 || nrow(away_row) == 0) stop("Missing MC profile for one or both teams.")
  
  # ---- Step 1: draw SHARED quarter possessions, then split to teams ----
  home_q_poss <- integer(4)
  away_q_poss <- integer(4)
  
  for (q in 1:4) {
    game_poss <- draw_game_possessions_q(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q
    )
    
    split <- split_game_possessions_q(
      game_poss = game_poss,
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q
    )
    
    home_q_poss[q] <- split$home
    away_q_poss[q] <- split$away
  }
  
  # ---- Step 2: simulate scoring given those possessions ----
  home_pts <- 0L
  away_pts <- 0L
  
  for (q in 1:4) {
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss = as.integer(home_q_poss[q]),
      qtr = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss = as.integer(away_q_poss[q]),
      qtr = q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    home_pts <- home_pts + as.integer(hb$base_points)
    away_pts <- away_pts + as.integer(ab$base_points)
  }
  
  # ---- Step 3: OT only if tied (uses Q5/Q6 possession profiles if present) ----
  ot_played <- 0L
  while (home_pts == away_pts && ot_played < max_ot) {
    ot_played <- ot_played + 1L
    q_ot <- 4L + ot_played  # 5, then 6
    
    # shared OT pace draw (smaller range is fine, but keep consistent)
    game_poss_ot <- draw_game_possessions_q(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q_ot,
      min_poss = 6L,
      max_poss = 20L
    )
    
    split_ot <- split_game_possessions_q(
      game_poss = game_poss_ot,
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      team_poss_profile_q = team_poss_profile_q,
      qtr = q_ot
    )
    
    hb <- simulate_team_offense_pts_q(
      team_id = home_team_id,
      poss = as.integer(split_ot$home),
      qtr = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    ab <- simulate_team_offense_pts_q(
      team_id = away_team_id,
      poss = as.integer(split_ot$away),
      qtr = q_ot,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
    
    home_pts <- home_pts + as.integer(hb$base_points)
    away_pts <- away_pts + as.integer(ab$base_points)
  }
  
  tibble::tibble(
    home_team_id = home_team_id,
    away_team_id = away_team_id,
    season       = season_chosen,
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

simulate_matchup_poss_only <- function(home_team_id, away_team_id,
                                       season_chosen, team_mc_profile,
                                       team_poss_profile_q,
                                       team_to_profile_q,
                                       team_foul_profile_q,
                                       team_pts_profile_q,
                                       n_sims = 10000L) {
  
  purrr::map_dfr(
    seq_len(n_sims),
    ~ simulate_game_once_poss_only(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      season_chosen = season_chosen,
      team_mc_profile = team_mc_profile,
      team_poss_profile_q = team_poss_profile_q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      team_pts_profile_q  = team_pts_profile_q
    )
  )
}

summarise_mc_poss_only <- function(sims_df) {
  sims_df %>%
    dplyr::summarise(
      MC_HomePts     = mean(home_points),
      MC_AwayPts     = mean(away_points),
      MC_Total       = mean(home_points + away_points),
      MC_Spread      = mean(margin),
      MC_HomeWinProb = mean(margin > 0),
      MC_AwayWinProb = mean(margin < 0),
      MC_PushProb    = mean(margin == 0),
      MC_HomePts_SD  = sd(home_points),
      MC_AwayPts_SD  = sd(away_points),
      MC_Total_SD    = sd(home_points + away_points),
      MC_Spread_SD   = sd(margin)
    )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 2. END: Possessions-driven MC (shared pace)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ===============================================================
# PTS ENGINE UPGRADE (uses team_pts_profile_q as inputs)
#   + Adds: shooting shrink + pct jitter (from MC_VAR)
# ===============================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

clamp01 <- function(x) pmin(pmax(x, 0), 1)

get_team_pts_inputs_q <- function(team_id, qtr, team_pts_profile_q) {
  r <- team_pts_profile_q %>% filter(team_id == !!team_id, qtr == !!qtr) %>% slice(1)
  
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

derive_team_probs_q <- function(team_id, qtr,
                                team_to_profile_q,
                                team_pts_profile_q,
                                mc_var    = MC_VAR,
                                mc_anchor = MC_ANCHOR) {
  
  # turnovers
  q_to <- team_to_profile_q %>%
    filter(team_id == !!team_id, qtr == !!qtr) %>%
    slice(1)
  
  p_to <- if (nrow(q_to) > 0) pct01(q_to$TOV_pct, default = 0.13) else 0.13
  live_share <- if (nrow(q_to) > 0) clamp01(q_to$TOV_live_share) else 0.6
  
  # shooting inputs from BaseStats-driven profile
  pts_in <- get_team_pts_inputs_q(team_id, qtr, team_pts_profile_q)
  
  # ---- APPLY SHRINK (MC_VAR$shoot_shrink) ----
  FG2_pct <- shrink_to(pts_in$FG2_pct, mc_anchor$fg2, mc_var$shoot_shrink)
  FG3_pct <- shrink_to(pts_in$FG3_pct, mc_anchor$fg3, mc_var$shoot_shrink)
  FT_pct  <- shrink_to(pts_in$FT_pct,  mc_anchor$ft,  mc_var$shoot_shrink)
  
  # ---- APPLY PCT JITTER (MC_VAR$pct_jitter_sd) AFTER SHRINK ----
  FG2_pct <- apply_pct_jitter(FG2_pct, mc_var$pct_jitter_sd)
  FG3_pct <- apply_pct_jitter(FG3_pct, mc_var$pct_jitter_sd)
  FT_pct  <- apply_pct_jitter(FT_pct,  mc_var$pct_jitter_sd)
  
  # convert FTA/FGA into a shooting-foul probability proxy
  avg_fts_per_shooting_foul <- 2.2
  p_shoot_foul <- ifelse(is.na(pts_in$FTA_per_FGA),
                         0.10,
                         pts_in$FTA_per_FGA / avg_fts_per_shooting_foul)
  p_shoot_foul <- pmin(pmax(p_shoot_foul, 0), 0.18)
  
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

simulate_team_offense_pts_q <- function(team_id, poss, qtr,
                                        team_to_profile_q,
                                        team_foul_profile_q,
                                        team_pts_profile_q,
                                        p_and1_given_made_foul = 0.08) {
  
  probs <- derive_team_probs_q(
    team_id = team_id, qtr = qtr,
    team_to_profile_q = team_to_profile_q,
    team_pts_profile_q = team_pts_profile_q,
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
  
  # turnovers
  total_tov <- if (p_to > 0) rbinom(1, size = poss, prob = p_to) else 0L
  total_tov <- as.integer(total_tov)
  
  live_tov <- if (total_tov > 0) rbinom(1, size = total_tov, prob = live_share) else 0L
  live_tov <- as.integer(live_tov)
  
  # possessions left for shot attempts
  shot_poss <- max(poss - total_tov, 0L)
  
  # offensive fouls (these consume shot possessions)
  off_fouls <- draw_off_fouls_q(team_id, team_foul_profile_q, shot_poss, qtr)
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  # each remaining shot_poss becomes one shot attempt
  total_shots <- as.integer(shot_poss)
  
  # split 2PA / 3PA
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  # makes (NOW reflect shrink + jitter)
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
  
  # free throws (NOW reflect shrink + jitter via FT_pct)
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
    FTA  = FTA,  FTM  = FTM
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END. PTS ENGINE UPGRADE
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# TOV SECTION: Build quarter-level turnover profiles from BaseStats_Team_MC
#   Output:
#     - team_to_profile_q  (team_id x qtr)
#       cols: TOV_pct, TOV_mean, TOV_sd, TOV_live_share
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)
pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  pmin(pmax(x, 0), 1)
}
num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}
clamp01 <- function(x) pmin(pmax(x, 0), 1)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# helper: pull Q col with CGS fallback (and /4 for quarter if needed)
get_stat_q <- function(df, q_col, cgs_col = NULL, q = 1L, div_if_cgs = 4) {
  if (has_col(df, q_col)) return(suppressWarnings(as.numeric(df[[q_col]])))
  if (!is.null(cgs_col) && has_col(df, cgs_col)) {
    x <- suppressWarnings(as.numeric(df[[cgs_col]]))
    out <- x / div_if_cgs
    if (q >= 5) out <- 0
    return(out)
  }
  rep(NA_real_, nrow(df))
}

to_long <- purrr::map_dfr(1:6, function(q) {
  
  pct_q  <- paste0("T_TOV_PCT_Q", q)
  tov_q  <- paste0("T_TOV_Q", q)
  live_q <- paste0("T_TOV_LIVEB_Q", q)
  
  pct_c  <- "T_TOV_PCT_CGS"
  tov_c  <- "T_TOV_CGS"
  live_c <- "T_TOV_LIVEB_CGS"
  
  # %: don't divide by 4 (it's a rate)
  tov_pct_raw <- if (has_col(BaseStats_Team_MC, pct_q)) {
    suppressWarnings(as.numeric(BaseStats_Team_MC[[pct_q]]))
  } else if (has_col(BaseStats_Team_MC, pct_c)) {
    suppressWarnings(as.numeric(BaseStats_Team_MC[[pct_c]]))
  } else {
    rep(NA_real_, nrow(BaseStats_Team_MC))
  }
  
  tibble::tibble(
    team_id = BaseStats_Team_MC$team_id,
    season  = BaseStats_Team_MC$season,
    qtr     = q,
    
    TOV_pct_raw = tov_pct_raw,
    TOV_raw     = get_stat_q(BaseStats_Team_MC, tov_q,  tov_c,  q = q),
    LIVE_raw    = get_stat_q(BaseStats_Team_MC, live_q, live_c, q = q)
  )
})

team_to_profile_q <- to_long %>%
  mutate(
    # normalize TOV%
    TOV_pct = dplyr::case_when(
      is.na(TOV_pct_raw) ~ NA_real_,
      TOV_pct_raw > 1 & TOV_pct_raw <= 100 ~ TOV_pct_raw / 100,
      TOV_pct_raw < 0 ~ 0,
      TOV_pct_raw > 1 ~ 1,
      TRUE ~ TOV_pct_raw
    ),
    TOV_pct = clamp01(TOV_pct)
  ) %>%
  group_by(team_id, season, qtr) %>%
  summarise(
    TOV_pct = mean(TOV_pct, na.rm = TRUE),
    
    TOV_mean = mean(TOV_raw,  na.rm = TRUE),
    TOV_sd   = sd(TOV_raw,    na.rm = TRUE),
    
    LIVE_mean = mean(LIVE_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TOV_sd = ifelse(is.na(TOV_sd), 0, TOV_sd),
    
    # live-ball share (fallback to 0.6 if missing/unstable)
    TOV_live_share = dplyr::case_when(
      !is.na(TOV_mean) & TOV_mean > 0 & !is.na(LIVE_mean) ~ clamp01(LIVE_mean / TOV_mean),
      TRUE ~ 0.6
    ),
    
    # defaults so sim doesn't explode early season
    TOV_pct = clamp01(ifelse(is.na(TOV_pct), 0.13, TOV_pct)),
    TOV_mean = ifelse(is.na(TOV_mean), 0, TOV_mean)
  )

# quick sanity check (optional)
# team_to_profile_q %>% filter(team_id == "1610612747") %>% arrange(qtr)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: TOV profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# FOULS SECTION: Build quarter-level offensive foul + shooting-foul-to-FT profiles from BaseStats_Team_MC
#   Outputs:
#     - team_off_foul_profile_q   (team_id x qtr): OFF_FOULS_mean, OFF_FOULS_sd
#     - team_shoot_foul_profile_q (team_id x qtr): P_SHOOT_FOUL (prob a shot attempt draws shooting FTs)
#
# Notes:
#   - OFF FOULS: uses T_OFF_FOULS_Qx else CGS/4 fallback (Q5/Q6 forced 0 if only CGS exists)
#   - P_SHOOT_FOUL: derived from (FTA / FGA) scaled by avg_fts_per_trip (≈2.0)
#       p_shoot_foul ≈ (FTA/FGA) / 2.0 , then clamped
#   - If you later want DEF fouls separately, we can add those as another profile.
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)
num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}
clamp01 <- function(x) pmin(pmax(x, 0), 1)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# helper: quarter value with CGS fallback (/4), Q5/Q6 forced 0 if CGS-only
get_q_or_cgs_div4 <- function(df, q_col, cgs_col, q) {
  if (has_col(df, q_col)) return(suppressWarnings(as.numeric(df[[q_col]])))
  if (has_col(df, cgs_col)) {
    x <- suppressWarnings(as.numeric(df[[cgs_col]])) / 4
    if (q >= 5) x <- 0
    return(x)
  }
  rep(NA_real_, nrow(df))
}

# ---------------------------------------------------------------
# A) OFFENSIVE FOULS profile (counts)
# ---------------------------------------------------------------
off_foul_long <- purrr::map_dfr(1:6, function(q) {
  q_nm  <- paste0("T_OFF_FOULS_Q", q)
  cgs_nm <- "T_OFF_FOULS_CGS"
  
  tibble::tibble(
    team_id = BaseStats_Team_MC$team_id,
    season  = BaseStats_Team_MC$season,
    qtr     = q,
    off_fouls_raw = get_q_or_cgs_div4(BaseStats_Team_MC, q_nm, cgs_nm, q)
  )
})

team_off_foul_profile_q <- off_foul_long %>%
  filter(!is.na(off_fouls_raw), off_fouls_raw >= 0) %>%
  group_by(team_id, season, qtr) %>%
  summarise(
    OFF_FOULS_mean = mean(off_fouls_raw, na.rm = TRUE),
    OFF_FOULS_sd   = sd(off_fouls_raw,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    OFF_FOULS_mean = ifelse(is.na(OFF_FOULS_mean), 0, OFF_FOULS_mean),
    OFF_FOULS_sd   = ifelse(is.na(OFF_FOULS_sd),   0, OFF_FOULS_sd)
  )

# ---------------------------------------------------------------
# B) SHOOTING FOUL proxy profile from FTA/FGA (rate)
#    Uses BaseStats_Team_MC:
#      - FGA: T_FGA_Qx (or T_FGA_CGS/4)
#      - FTA: T_FTA_Qx (or T_FTA_CGS/4)  <-- if you don't have these, tell me what your FT attempt columns are
# ---------------------------------------------------------------
fta_exists <- any(grepl("^T_FTA_(Q[1-6]|CGS)$", names(BaseStats_Team_MC)))
if (!fta_exists) {
  message("NOTE: No T_FTA_Qx / T_FTA_CGS columns found. ",
          "team_shoot_foul_profile_q will be skipped. ",
          "If your FTA column name differs, map it here.")
  team_shoot_foul_profile_q <- tibble::tibble()
} else {
  shoot_foul_long <- purrr::map_dfr(1:6, function(q) {
    fga_q  <- paste0("T_FGA_Q", q)
    fga_c  <- "T_FGA_CGS"
    fta_q  <- paste0("T_FTA_Q", q)
    fta_c  <- "T_FTA_CGS"
    
    tibble::tibble(
      team_id = BaseStats_Team_MC$team_id,
      season  = BaseStats_Team_MC$season,
      qtr     = q,
      FGA_raw = get_q_or_cgs_div4(BaseStats_Team_MC, fga_q, fga_c, q),
      FTA_raw = get_q_or_cgs_div4(BaseStats_Team_MC, fta_q, fta_c, q)
    )
  })
  
  avg_fts_per_trip <- 2.0  # you can tune this later
  
  team_shoot_foul_profile_q <- shoot_foul_long %>%
    group_by(team_id, season, qtr) %>%
    summarise(
      FGA_mean = mean(FGA_raw, na.rm = TRUE),
      FTA_mean = mean(FTA_raw, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      fta_per_fga = ifelse(!is.na(FGA_mean) & FGA_mean > 0, FTA_mean / FGA_mean, NA_real_),
      
      # p_shoot_foul is "per shot attempt" probability of triggering a shooting-foul FT trip
      P_SHOOT_FOUL = ifelse(is.na(fta_per_fga), NA_real_, fta_per_fga / avg_fts_per_trip),
      
      # clamps + reasonable defaults
      P_SHOOT_FOUL = clamp01(ifelse(is.na(P_SHOOT_FOUL), 0.08, P_SHOOT_FOUL)),
      P_SHOOT_FOUL = pmin(P_SHOOT_FOUL, 0.18)
    )
}

# quick sanity checks (optional)
# team_off_foul_profile_q %>% filter(team_id == "1610612747") %>% arrange(qtr)
# team_shoot_foul_profile_q %>% filter(team_id == "1610612747") %>% arrange(qtr)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: FOUL profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# REBOUNDING SECTION: Build quarter-level rebounding profiles from BaseStats_Team_MC
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
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)
clamp01 <- function(x) pmin(pmax(x, 0), 1)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# helper: quarter value with CGS fallback (/4), Q5/Q6 forced 0 if CGS-only
get_q_or_cgs_div4 <- function(df, q_col, cgs_col, q) {
  if (has_col(df, q_col)) return(suppressWarnings(as.numeric(df[[q_col]])))
  if (has_col(df, cgs_col)) {
    x <- suppressWarnings(as.numeric(df[[cgs_col]])) / 4
    if (q >= 5) x <- 0
    return(x)
  }
  rep(NA_real_, nrow(df))
}

# ---- build long table for REB / OREB / DREB ----
reb_long <- purrr::map_dfr(1:6, function(q) {
  tibble::tibble(
    team_id = BaseStats_Team_MC$team_id,
    season  = BaseStats_Team_MC$season,
    qtr     = q,
    
    REB_raw  = get_q_or_cgs_div4(BaseStats_Team_MC, paste0("T_REB_Q",  q), "T_REB_CGS",  q),
    OREB_raw = get_q_or_cgs_div4(BaseStats_Team_MC, paste0("T_OREB_Q", q), "T_OREB_CGS", q),
    DREB_raw = get_q_or_cgs_div4(BaseStats_Team_MC, paste0("T_DREB_Q", q), "T_DREB_CGS", q)
  )
})

team_reb_profile_q <- reb_long %>%
  # keep rows where at least one reb metric exists
  filter(!(is.na(REB_raw) & is.na(OREB_raw) & is.na(DREB_raw))) %>%
  group_by(team_id, season, qtr) %>%
  summarise(
    REB_mean  = mean(REB_raw,  na.rm = TRUE),
    REB_sd    = sd(REB_raw,    na.rm = TRUE),
    
    OREB_mean = mean(OREB_raw, na.rm = TRUE),
    OREB_sd   = sd(OREB_raw,   na.rm = TRUE),
    
    DREB_mean = mean(DREB_raw, na.rm = TRUE),
    DREB_sd   = sd(DREB_raw,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(ends_with("_sd"), ~ ifelse(is.na(.), 0, .)),
    
    # optional: share of rebounds that are offensive (helps drive 2nd-chance points later)
    OREB_share_mean = ifelse(!is.na(REB_mean) & REB_mean > 0,
                             clamp01(OREB_mean / REB_mean),
                             NA_real_)
  )

# quick sanity checks (optional)
# team_reb_profile_q %>% filter(team_id == "1610612747") %>% arrange(qtr)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: Rebounding profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ------------------------------------------------------------
# MODEL A FIX: Extra shots come ONLY from "miss -> OREB -> extra shot"
#   - NO additive SEC_CHN_FGA driver (avoids double-count risk)
#   - OREB draw is capped by misses (can't OREB a make)
# ------------------------------------------------------------
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

simulate_team_offense_B_q_v3_modelA <- function(team_id, season_chosen, qtr,
                                                poss,
                                                team_pts_profile_q,
                                                team_to_profile_q,
                                                team_foul_profile_q,
                                                team_reb_profile_q,
                                                sd_shrink_fga = 0.75,
                                                p_and1_given_made_foul = 0.08) {
  
  # ---- base shooting profile for this qtr (from team_pts_profile_q) ----
  pts_prof <- get_pts_prof_q(team_id, season_chosen, qtr, team_pts_profile_q)
  
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
  oreb_prof <- get_oreb_prof_q(team_id, season_chosen, qtr, team_reb_profile_q)
  
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
# END SECOND CHANCE POINTS and POSSESSIONS 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# FREE THROW MODULE (BaseStats_Team_MC)
#   Outputs:
#     - team_ft_profile_q (team_id x qtr):
#         FTM_mean, FTM_sd
#         FT_pct_mean
#         FTR_mean, FTR_sd   (FTR ≈ FTA / FGA)
#
#   Notes:
#     - Uses T_FTM_Qx, T_FT_PCT_Qx, T_FTR_Qx where present
#     - Fallbacks:
#         * FTM: uses T_FTM_CGS/4 for Q1–Q4, and 0 for Q5–Q6 if no Qx
#         * FT% / FTR: uses CGS for all quarters if no Qx
#     - This is an ENHANCEMENT module (does not replace PTS section)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)

pct01 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  pmin(pmax(x, 0), 1)
}
num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}
clamp01 <- function(x) pmin(pmax(x, 0), 1)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# ---- helpers ----
get_FTM_q <- function(df, q, q_col, cgs_col) {
  if (has_col(df, q_col)) return(suppressWarnings(as.numeric(df[[q_col]])))
  if (has_col(df, cgs_col)) {
    x <- suppressWarnings(as.numeric(df[[cgs_col]])) / 4
    if (q >= 5) x <- 0
    return(x)
  }
  rep(NA_real_, nrow(df))
}

get_rate_q <- function(df, q, q_col, cgs_col) {
  # for rates, DO NOT /4; if q_col missing, just reuse CGS across quarters
  if (has_col(df, q_col)) return(vapply(df[[q_col]], pct01, numeric(1), default = NA_real_))
  if (has_col(df, cgs_col)) return(vapply(df[[cgs_col]], pct01, numeric(1), default = NA_real_))
  rep(NA_real_, nrow(df))
}

ft_long <- purrr::map_dfr(1:6, function(q) {
  tibble::tibble(
    team_id = BaseStats_Team_MC$team_id,
    season  = BaseStats_Team_MC$season,
    qtr     = q,
    
    FTM_raw    = get_FTM_q(BaseStats_Team_MC, q, paste0("T_FTM_Q", q), "T_FTM_CGS"),
    FT_pct_raw = get_rate_q(BaseStats_Team_MC, q, paste0("T_FT_PCT_Q", q), "T_FT_PCT_CGS"),
    FTR_raw    = get_rate_q(BaseStats_Team_MC, q, paste0("T_FTR_Q", q), "T_FTR_CGS")
  )
})

team_ft_profile_q <- ft_long %>%
  group_by(team_id, season, qtr) %>%
  summarise(
    FTM_mean    = mean(FTM_raw,    na.rm = TRUE),
    FTM_sd      = sd(FTM_raw,      na.rm = TRUE),
    FT_pct_mean = mean(FT_pct_raw, na.rm = TRUE),
    FTR_mean    = mean(FTR_raw,    na.rm = TRUE),
    FTR_sd      = sd(FTR_raw,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FTM_sd = ifelse(is.na(FTM_sd), 0, FTM_sd),
    FTR_sd = ifelse(is.na(FTR_sd), 0, FTR_sd),
    
    # stability clamps / defaults
    FT_pct_mean = clamp01(ifelse(is.na(FT_pct_mean), 0.78, FT_pct_mean)),
    FTR_mean    = pmax(0, ifelse(is.na(FTR_mean), 0.22, FTR_mean))
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END: FT profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# ================================================================================================
# FT GENERATION + ENDGAME INTENTIONAL FOUL MODULE (hooks into your existing quarter sim)
#   - baseline FTAs drawn from FTR * FGA within the quarter
#   - clutch lever scales FTR only when you tell it to (Q4 close-game)
#   - intentional foul add-on is a separate toggle (endgame only)
# ================================================================================================
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

draw_attempts <- function(mean_val, sd_val) {
  if (is.na(mean_val) || mean_val <= 0) return(0L)
  if (is.na(sd_val)) sd_val <- 0
  val <- round(rnorm(1, mean = mean_val, sd = sd_val))
  val <- max(val, 0L)
  as.integer(val)
}

# pull team FT params for a quarter (with sane fallbacks)
get_team_ft_params_q <- function(team_id, season_chosen, qtr, team_ft_profile_q) {
  r <- team_ft_profile_q %>%
    filter(team_id == !!team_id, season == season_chosen, qtr == !!qtr) %>%
    slice(1)
  
  if (nrow(r) == 0) {
    return(list(FT_pct = 0.78, FTR_mu = 0.22, FTR_sd = 0))
  }
  
  list(
    FT_pct = clamp01(num0(r$FT_pct_mean, 0.78)),
    FTR_mu = pmax(0, num0(r$FTR_mean, 0.22)),
    FTR_sd = pmax(0, num0(r$FTR_sd, 0))
  )
}

# baseline FTAs from FTR * FGA
draw_baseline_fta <- function(FGA, FTR_mu, FTR_sd, clutch_mult = 1.0) {
  if (is.na(FGA) || FGA <= 0) return(0L)
  mu <- (FTR_mu * clutch_mult) * FGA
  sd <- (FTR_sd * clutch_mult) * FGA
  as.integer(draw_attempts(mu, sd))
}

# endgame intentional fouls (simple, clean toggle)
apply_endgame_intentional_fouls <- function(home_pts, away_pts,
                                            home_FT_pct, away_FT_pct,
                                            close_margin = 6L,
                                            max_foul_trips = 6L,
                                            foul_ft_per_trip = 2L,
                                            lever = 1.0) {
  margin <- home_pts - away_pts
  if (is.na(margin) || abs(margin) > close_margin) {
    return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,extra_trips=0L))
  }
  
  # number of intentional foul trips (0..max), scaled by lever
  trips <- as.integer(round((sample.int(max_foul_trips + 1L, 1) - 1L) * lever))
  trips <- as.integer(pmin(pmax(trips, 0L), max_foul_trips))
  if (trips <= 0) return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,extra_trips=0L))
  
  home_trailing <- (margin < 0)
  
  if (home_trailing) {
    h_fta <- as.integer(trips * foul_ft_per_trip)
    h_ftm <- as.integer(if (h_fta > 0) rbinom(1, h_fta, clamp01(home_FT_pct)) else 0L)
    return(list(h_fta=h_fta,h_ftm=h_ftm,a_fta=0L,a_ftm=0L,extra_trips=trips))
  } else {
    a_fta <- as.integer(trips * foul_ft_per_trip)
    a_ftm <- as.integer(if (a_fta > 0) rbinom(1, a_fta, clamp01(away_FT_pct)) else 0L)
    return(list(h_fta=0L,h_ftm=0L,a_fta=a_fta,a_ftm=a_ftm,extra_trips=trips))
  }
}

# --------------------------------------------------------------------------------
# DROP-IN REPLACEMENT: simulate_team_offense_B_q (adds FT generation via team_ft_profile_q)
#   - keeps your TO / off-foul / 2p/3p structure
#   - changes ONLY how baseline FTA/FTM are generated:
#       * baseline FTA from FTR * FGA (quarter-specific)
#       * FT% from team_ft_profile_q (quarter-specific)
#   - clutch_mult_ftr is the "lever" input (default 1.0)
# --------------------------------------------------------------------------------
simulate_team_offense_B_q <- function(team_row, poss, qtr,
                                      season_chosen,
                                      team_to_profile_q,
                                      team_foul_profile_q,
                                      team_ft_profile_q,
                                      clutch_mult_ftr = 1.0,
                                      p_and1_given_made_foul = 0.08) {
  
  tid <- team_row$team_id[1]
  
  # shot make rates (still fine to source from your PTS profile row)
  FG2_pct <- pct01(team_row$FG2_pct, default = 0.50)
  FG3_pct <- pct01(team_row$FG3_pct, default = 0.36)
  
  # turnovers + foul probability you already derive
  probs <- derive_team_probs_q(team_row, team_to_profile_q, qtr)
  p_to        <- probs$p_to
  live_share  <- probs$live_share
  p2_given_sh <- probs$p_2_given_shot
  p_foul      <- probs$p_shoot_foul
  
  total_tov <- if (p_to > 0) rbinom(1, size = poss, prob = p_to) else 0L
  total_tov <- as.integer(total_tov)
  
  live_tov <- if (total_tov > 0) rbinom(1, size = total_tov, prob = live_share) else 0L
  live_tov <- as.integer(live_tov)
  
  shot_poss <- max(poss - total_tov, 0L)
  
  off_fouls <- draw_off_fouls_q(tid, team_foul_profile_q, shot_poss, qtr)
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  total_tov <- as.integer(total_tov + off_fouls)
  
  total_shots <- shot_poss
  
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  # keep your foul logic (used for and-1 estimation)
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  made_foul_2_exp <- round(FGM2 * p_foul)
  made_foul_3_exp <- round(FGM3 * p_foul)
  
  and1_2 <- if (made_foul_2_exp > 0) rbinom(1, size = made_foul_2_exp, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3_exp > 0) rbinom(1, size = made_foul_3_exp, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  # ---- NEW: baseline FTs from team_ft_profile_q using FTR ----
  ftp <- get_team_ft_params_q(tid, season_chosen, qtr, team_ft_profile_q)
  FT_pct <- ftp$FT_pct
  
  FGA_total <- as.integer(FGA2 + FGA3)
  FTA_base  <- draw_baseline_fta(FGA_total, ftp$FTR_mu, ftp$FTR_sd, clutch_mult = clutch_mult_ftr)
  
  # add +1 FT for each and-1
  FTA <- as.integer(pmax(FTA_base + (and1_2 + and1_3), 0L))
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
    FT_pct_used = FT_pct,
    FTR_used_mu = ftp$FTR_mu
  )
}

# --------------------------------------------------------------------------------
# HOW TO USE INSIDE simulate_game_once (minimal guidance, no overwrite)
#   - Pass team_ft_profile_q into the offense sim
#   - For clutch lever: set clutch_mult_ftr > 1 ONLY in Q4 when abs(margin_3q) <= X
#   - Then keep your intentional foul block, but swap to apply_endgame_intentional_fouls()
# --------------------------------------------------------------------------------
# Example logic you can drop into your existing Q4 call site:
#   is_close <- (abs(margin_3q) <= 6)
#   h_clutch_mult <- if (is_close && margin_3q < 0) 1.10 else 1.00   # home trailing => more FTs
#   a_clutch_mult <- if (is_close && margin_3q > 0) 1.10 else 1.00   # away trailing => more FTs
#
#   hb4 <- simulate_team_offense_B_q(..., team_ft_profile_q = team_ft_profile_q, clutch_mult_ftr = h_clutch_mult)
#   ab4 <- simulate_team_offense_B_q(..., team_ft_profile_q = team_ft_profile_q, clutch_mult_ftr = a_clutch_mult)
#
# Then replace your clutch add-on with:
#   clutch <- apply_endgame_intentional_fouls(home_pts, away_pts,
#                                            home_FT_pct = hb4$FT_pct_used,
#                                            away_FT_pct = ab4$FT_pct_used,
#                                            close_margin = 6L,
#                                            max_foul_trips = 6L,
#                                            foul_ft_per_trip = 2L,
#                                            lever = 1.0)
#   home_pts <- home_pts + clutch$h_ftm
#   away_pts <- away_pts + clutch$a_ftm
#   H$fta <- H$fta + clutch$h_fta; H$ftm <- H$ftm + clutch$h_ftm
#   A$fta <- A$fta + clutch$a_fta; A$ftm <- A$ftm + clutch$a_ftm


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END FT ENGINE AND ENDGAME FOUL MODULE
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# OPPONENT ADJUSTMENT LAYER:
#   Build "defense allowed" profiles by swapping opponent offensive stats within ESPN_GAME_ID
#   Outputs:
#     - def_allowed_long
#     - team_def_allowed_profile_q
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

has_col <- function(df, nm) nm %in% names(df)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# ---- Choose which opponent-adjusted columns you want to build "allowed" versions for ----
# Add/remove columns here as needed. These are typical "rate/pct" levers + key volume levers.
q_cols <- c(
  # shooting %
  "T_2PT_PCT_Q", "T_3PT_PCT_Q", "T_FT_PCT_Q", "T_FG_PCT_Q",
  # turnovers / fouls / rebounding
  "T_TOV_PCT_Q", "T_OFF_FOULS_Q", "T_OREB_Q", "T_DREB_Q", "T_REB_Q",
  # free throw rate (already a rate)
  "T_FTR_Q"
)

cgs_cols <- c(
  "T_2PT_PCT_CGS", "T_3PT_PCT_CGS", "T_FT_PCT_CGS", "T_FG_PCT_CGS",
  "T_TOV_PCT_CGS", "T_OFF_FOULS_CGS", "T_OREB_CGS", "T_DREB_CGS", "T_REB_CGS",
  "T_FTR_CGS"
)

# keep only columns that actually exist
q_cols_exist <- q_cols[paste0(q_cols, "1") %in% names(BaseStats_Team_MC) | paste0(q_cols, "2") %in% names(BaseStats_Team_MC) |
                         paste0(q_cols, "3") %in% names(BaseStats_Team_MC) | paste0(q_cols, "4") %in% names(BaseStats_Team_MC) |
                         paste0(q_cols, "5") %in% names(BaseStats_Team_MC) | paste0(q_cols, "6") %in% names(BaseStats_Team_MC)]
cgs_cols_exist <- cgs_cols[cgs_cols %in% names(BaseStats_Team_MC)]

# helper to build actual column names for Q1..Q6 from a "T_X_Q" prefix
expand_q_cols <- function(prefix_q) paste0(prefix_q, 1:6)

use_cols <- c("season", "ESPN_GAME_ID", "team_id")

# build list of actual columns to swap
swap_cols <- c(
  unlist(map(q_cols_exist, expand_q_cols)),
  cgs_cols_exist
)
swap_cols <- swap_cols[swap_cols %in% names(BaseStats_Team_MC)]

bs_min <- BaseStats_Team_MC %>%
  select(any_of(c(use_cols, swap_cols))) %>%
  mutate(across(all_of(swap_cols), ~ suppressWarnings(as.numeric(.))))

# ---- Swap opponent stats inside each game ----
# For each team row, attach opponent's values as "DEF_ALLOW_*"
opp_swapped <- bs_min %>%
  rename_with(~ paste0("OFF__", .x), all_of(swap_cols)) %>%
  inner_join(
    bs_min %>%
      rename_with(~ paste0("OPP__", .x), all_of(swap_cols)) %>%
      rename(opp_team_id = team_id),
    by = c("season", "ESPN_GAME_ID")
  ) %>%
  filter(team_id != opp_team_id)

# long format by quarter for easier profiling
def_allowed_long_q <- opp_swapped %>%
  pivot_longer(
    cols = starts_with("OPP__") & ends_with(paste0("_Q", 1:6)),
    names_to = "stat_q",
    values_to = "opp_value"
  ) %>%
  mutate(
    qtr = as.integer(sub(".*_Q([1-6])$", "\\1", stat_q)),
    stat = sub("^OPP__", "", sub("_Q[1-6]$", "", stat_q))
  ) %>%
  select(team_id, season, ESPN_GAME_ID, opp_team_id, qtr, stat, opp_value)

def_allowed_long_cgs <- opp_swapped %>%
  pivot_longer(
    cols = starts_with("OPP__") & ends_with("_CGS"),
    names_to = "stat_cgs",
    values_to = "opp_value"
  ) %>%
  mutate(
    qtr = 0L,  # use 0 as CGS bucket
    stat = sub("^OPP__", "", sub("_CGS$", "", stat_cgs))
  ) %>%
  select(team_id, season, ESPN_GAME_ID, opp_team_id, qtr, stat, opp_value)

def_allowed_long <- bind_rows(def_allowed_long_q, def_allowed_long_cgs)

# ---- Profile: per team x qtr x stat mean/sd of what they allow ----
team_def_allowed_profile_q <- def_allowed_long %>%
  group_by(team_id, season, qtr, stat) %>%
  summarise(
    DEF_ALLOW_mean = mean(opp_value, na.rm = TRUE),
    DEF_ALLOW_sd   = sd(opp_value,   na.rm = TRUE),
    n_games        = sum(!is.na(opp_value)),
    .groups = "drop"
  ) %>%
  mutate(
    DEF_ALLOW_sd = ifelse(is.na(DEF_ALLOW_sd), 0, DEF_ALLOW_sd)
  )

# Example:
# team_def_allowed_profile_q %>% filter(team_id=="1610612747", qtr==1) %>% arrange(stat)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# END OPPONENT ADJUMENT SECTION 
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
  Sys.sleep(0.35)
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

