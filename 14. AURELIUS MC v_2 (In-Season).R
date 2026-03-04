# ===============================================================
# AURELIUS MC — QUARTER ENGINE VERSION (Q1–Q6)
#   - Uses BaseStats_Team_MC T_POSS_Q1..Q6 if present
#   - Uses OT only if tied after Q4
#   - Uses quarter TOV% / OFF FOULS if present, else _CGS fallback
# ===============================================================


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 0. START: Load pm_nbapbp, BaseStats_Team_MC, BaseStats_Player_MC, rotations, odds files
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
library(future)
library(future.apply)

plan(multisession, workers = 5)
options(future.rng.onMisuse = "ignore")

# --- Load pm_nbapbp MC file ------------------------------------
pbp_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/pm_nbapbp_",
  season_token,
  ".csv"
)
pbp_df <- fread(pbp_path, colClasses = "character", encoding = "UTF-8")

# --- Load CURRENT SLATE ----------------------------------------
current_slate_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate/nba_schedule_",
  formatted_date,
  ".csv"
)
nba_schedule_formatted_date <- fread(current_slate_file, colClasses = "character", encoding = "UTF-8")

# --- Load BaseStats_Team_MC -------------------------------------
basestatsT_mc_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_",
  season_token,
  ".csv"
)
BaseStats_Team_MC <- fread(basestatsT_mc_file, colClasses = "character", encoding = "UTF-8")

# --- Load BaseStats_Player_MC (ready) ---------------------------
basestatsP_mc_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_MC_",
  season_token,
  ".csv"
)
BaseStats_Player_MC <- fread(basestatsP_mc_file, colClasses = "character", encoding = "UTF-8")

# --- Load rotations (ready) ------------------------------------
nba_rotation_10M_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/pm_nba_player_rotations_10M_",
  season_token,
  ".csv"
)
nba_rotations_10M <- fread(nba_rotation_10M_file, colClasses = "character", encoding = "UTF-8")

nba_rotation_5M_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/pm_nba_player_rotations_5M_",
  season_token,
  ".csv"
)
nba_rotations_5M <- fread(nba_rotation_5M_file, colClasses = "character", encoding = "UTF-8")

# --- Load Odds files (ready) -----------------------------------
nba_odds_points_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_points_",
  season_token,
  ".csv"
)
nba_odds_points <- fread(nba_odds_points_file, colClasses = "character", encoding = "UTF-8")

nba_odds_assists_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_points_assists_",
  season_token,
  ".csv"
)
nba_odds_assists <- fread(nba_odds_assists_file, colClasses = "character", encoding = "UTF-8")

nba_odds_rebounds_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_rebounds_",
  season_token,
  ".csv"
)
nba_odds_rebounds <- fread(nba_odds_rebounds_file, colClasses = "character", encoding = "UTF-8")

nba_odds_threes_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_threes_",
  season_token,
  ".csv"
)
nba_odds_threes <- fread(nba_odds_threes_file, colClasses = "character", encoding = "UTF-8")

nba_odds_steals_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_steals_",
  season_token,
  ".csv"
)
nba_odds_steals <- fread(nba_odds_steals_file, colClasses = "character", encoding = "UTF-8")

nba_odds_blocks_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_blocks_",
  season_token,
  ".csv"
)
nba_odds_blocks <- fread(nba_odds_blocks_file, colClasses = "character", encoding = "UTF-8")

# --- Coerce key numeric fields in PBP ---------------------------
pbp_df <- pbp_df %>%
  mutate(
    score_value      = suppressWarnings(as.numeric(score_value)),
    points_attempted = suppressWarnings(as.numeric(points_attempted)),
    scoring_play     = case_when(
      scoring_play %in% c("TRUE", "True", "true", "1")    ~ TRUE,
      scoring_play %in% c("FALSE", "False", "false", "0") ~ FALSE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!is.na(team_id))

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 0. END: Load section
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 1. START: Build team shooting + volume profiles from PBP
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

shot_df <- pbp_df %>%
  filter(!is.na(points_attempted) & points_attempted > 0) %>%
  mutate(
    shot_type_mc = case_when(
      points_attempted == 3 ~ "3P",
      points_attempted == 2 ~ "2P",
      points_attempted == 1 ~ "FT",
      TRUE ~ "OTHER"
    )
  )

pbp_df <- pbp_df %>%
  mutate(
    is_turnover_mc = if_else(
      grepl("Turnover", type_text, ignore.case = TRUE),
      1L,
      0L
    )
  )

team_shooting_profile <- shot_df %>%
  group_by(team_id, season) %>%
  summarise(
    FGA2 = sum(shot_type_mc == "2P"),
    FGM2 = sum(shot_type_mc == "2P" & score_value == 2),
    FGA3 = sum(shot_type_mc == "3P"),
    FGM3 = sum(shot_type_mc == "3P" & score_value == 3),
    FTA  = sum(shot_type_mc == "FT"),
    FTM  = sum(shot_type_mc == "FT" & score_value == 1),
    .groups = "drop"
  ) %>%
  mutate(
    FG2_pct = if_else(FGA2 > 0, FGM2 / FGA2, 0),
    FG3_pct = if_else(FGA3 > 0, FGM3 / FGA3, 0),
    FT_pct  = if_else(FTA  > 0, FTM  / FTA,  0)
  )

team_game_counts <- pbp_df %>%
  group_by(season, game_id, team_id) %>%
  summarise(
    FGA2      = sum(points_attempted == 2, na.rm = TRUE),
    FGA3      = sum(points_attempted == 3, na.rm = TRUE),
    FTA       = sum(points_attempted == 1, na.rm = TRUE),
    Turnovers = sum(is_turnover_mc,        na.rm = TRUE),
    .groups   = "drop"
  )

team_volume_profile <- team_game_counts %>%
  group_by(team_id, season) %>%
  summarise(
    games_played = n(),
    FGA2_mean    = mean(FGA2),
    FGA2_sd      = sd(FGA2),
    FGA3_mean    = mean(FGA3),
    FGA3_sd      = sd(FGA3),
    FTA_mean     = mean(FTA),
    FTA_sd       = sd(FTA),
    TO_mean      = mean(Turnovers),
    TO_sd        = sd(Turnovers),
    .groups      = "drop"
  ) %>%
  mutate(across(ends_with("_sd"), ~ if_else(is.na(.), 0, .)))

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 1. END: PBP profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 2. START: Quarter profiles from BaseStats_Team_MC (POSS, TOV%, OFF FOULS)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

has_col <- function(df, nm) nm %in% names(df)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    team_id      = as.character(ESPN_TEAM_ID)
  )

# ---- pace profile: per team per qtr mean/sd (T_POSS_Q1..Q6) ----
pace_long <- purrr::map_dfr(1:6, function(q) {
  q_col  <- paste0("T_POSS_Q", q)
  fb_col <- "T_POSS_CGS"
  
  if (has_col(BaseStats_Team_MC, q_col)) {
    BaseStats_Team_MC %>%
      transmute(team_id = team_id, qtr = q, poss = suppressWarnings(as.numeric(.data[[q_col]])))
  } else if (has_col(BaseStats_Team_MC, fb_col)) {
    BaseStats_Team_MC %>%
      transmute(team_id = team_id, qtr = q, poss = suppressWarnings(as.numeric(.data[[fb_col]])) / 4) %>%
      mutate(poss = ifelse(q <= 4, poss, 0))
  } else {
    tibble::tibble(team_id = character(), qtr = integer(), poss = numeric())
  }
})

team_poss_profile_q <- pace_long %>%
  filter(!is.na(poss), poss >= 0) %>%
  group_by(team_id, qtr) %>%
  summarise(
    POSS_mean = mean(poss, na.rm = TRUE),
    POSS_sd   = sd(poss,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(POSS_sd = if_else(is.na(POSS_sd), 0, POSS_sd))

# ---- turnovers profile: per team per qtr TOV% + live share -----
to_long <- purrr::map_dfr(1:6, function(q) {
  pct_nm  <- paste0("T_TOV_PCT_Q", q)
  tov_nm  <- paste0("T_TOV_Q", q)
  live_nm <- paste0("T_TOV_LIVEB_Q", q)
  
  pct_fb  <- "T_TOV_PCT_CGS"
  tov_fb  <- "T_TOV_CGS"
  live_fb <- "T_TOV_LIVEB_CGS"
  
  BaseStats_Team_MC %>%
    transmute(
      team_id = team_id,
      qtr     = q,
      TOV_pct_raw = dplyr::case_when(
        has_col(BaseStats_Team_MC, pct_nm) ~ suppressWarnings(as.numeric(.data[[pct_nm]])),
        has_col(BaseStats_Team_MC, pct_fb) ~ suppressWarnings(as.numeric(.data[[pct_fb]])),
        TRUE ~ NA_real_
      ),
      TOV_raw = dplyr::case_when(
        has_col(BaseStats_Team_MC, tov_nm) ~ suppressWarnings(as.numeric(.data[[tov_nm]])),
        has_col(BaseStats_Team_MC, tov_fb) ~ suppressWarnings(as.numeric(.data[[tov_fb]])) / 4,
        TRUE ~ NA_real_
      ),
      LIVE_raw = dplyr::case_when(
        has_col(BaseStats_Team_MC, live_nm) ~ suppressWarnings(as.numeric(.data[[live_nm]])),
        has_col(BaseStats_Team_MC, live_fb) ~ suppressWarnings(as.numeric(.data[[live_fb]])) / 4,
        TRUE ~ NA_real_
      )
    )
})

team_to_profile_q <- to_long %>%
  group_by(team_id, qtr) %>%
  summarise(
    TOV_pct_raw = mean(TOV_pct_raw, na.rm = TRUE),
    TOV_mean    = mean(TOV_raw,     na.rm = TRUE),
    TOV_live    = mean(LIVE_raw,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TOV_pct = case_when(
      is.na(TOV_pct_raw) ~ NA_real_,
      TOV_pct_raw > 1 & TOV_pct_raw <= 100 ~ TOV_pct_raw / 100,
      TOV_pct_raw < 0 ~ 0,
      TOV_pct_raw > 1 ~ 1,
      TRUE ~ TOV_pct_raw
    ),
    TOV_pct = pmin(pmax(TOV_pct, 0), 1),
    TOV_live_share = case_when(
      !is.na(TOV_mean) & TOV_mean > 0 & !is.na(TOV_live) ~ pmin(pmax(TOV_live / TOV_mean, 0), 1),
      TRUE ~ 0.6
    )
  )

# ---- offensive fouls profile: per team per qtr OFF_FOULS mean/sd
foul_long <- purrr::map_dfr(1:6, function(q) {
  q_nm  <- paste0("T_OFF_FOULS_Q", q)
  fb_nm <- "T_OFF_FOULS_CGS"
  
  if (has_col(BaseStats_Team_MC, q_nm)) {
    BaseStats_Team_MC %>%
      transmute(team_id = team_id, qtr = q,
                off_fouls = suppressWarnings(as.numeric(.data[[q_nm]])))
  } else if (has_col(BaseStats_Team_MC, fb_nm)) {
    BaseStats_Team_MC %>%
      transmute(team_id = team_id, qtr = q,
                off_fouls = suppressWarnings(as.numeric(.data[[fb_nm]])) / 4) %>%
      mutate(off_fouls = ifelse(q <= 4, off_fouls, 0))
  } else {
    tibble::tibble(team_id = character(), qtr = integer(), off_fouls = numeric())
  }
})

team_foul_profile_q <- foul_long %>%
  filter(!is.na(off_fouls), off_fouls >= 0) %>%
  group_by(team_id, qtr) %>%
  summarise(
    OFF_FOULS_mean = mean(off_fouls, na.rm = TRUE),
    OFF_FOULS_sd   = sd(off_fouls,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    OFF_FOULS_sd   = if_else(is.na(OFF_FOULS_sd), 0, OFF_FOULS_sd),
    OFF_FOULS_mean = if_else(is.na(OFF_FOULS_mean), 0, OFF_FOULS_mean)
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 2. END: Quarter profiles
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 3. START: Build single team_mc_profile (season-level shooting/volume + derived rates)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

team_mc_profile <- team_shooting_profile %>%
  inner_join(team_volume_profile, by = c("team_id", "season")) %>%
  mutate(
    SCORING_POSS_mean = if_else(
      games_played > 0,
      (FGA2_mean + FGA3_mean),   # simple proxy; will normalize below anyway
      0
    )
  )

# safety: team lookup abbrev
team_lookup <- pbp_df %>%
  select(team_id, home_team_id, away_team_id, home_team_abbrev, away_team_abbrev) %>%
  mutate(
    team_abbrev = case_when(
      team_id == home_team_id ~ home_team_abbrev,
      team_id == away_team_id ~ away_team_abbrev,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(team_abbrev)) %>%
  distinct(team_id, team_abbrev)

team_mc_profile <- team_mc_profile %>%
  left_join(team_lookup, by = "team_id") %>%
  mutate(
    # mean scoring possessions (possessions that did NOT end in TO) — uses TO_mean from PBP
    TOV_pct = if_else(
      (FGA2_mean + FGA3_mean + TO_mean) > 0,
      TO_mean / (FGA2_mean + FGA3_mean + TO_mean),
      0.13
    ),
    SCORING_POSS_mean = if_else(
      (FGA2_mean + FGA3_mean + TO_mean) > 0,
      (FGA2_mean + FGA3_mean),   # shots proxy
      1
    ),
    # rates per scoring poss
    FGA2_per_scoring_poss = if_else(SCORING_POSS_mean > 0, FGA2_mean / SCORING_POSS_mean, 0),
    FGA3_per_scoring_poss = if_else(SCORING_POSS_mean > 0, FGA3_mean / SCORING_POSS_mean, 0),
    FTA_per_scoring_poss  = if_else(SCORING_POSS_mean > 0, FTA_mean  / SCORING_POSS_mean, 0)
  )

season_chosen <- unique(team_mc_profile$season)[1]

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 3. END: team_mc_profile
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 4. START: Core helpers + quarter sim functions
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

safe_div <- function(a, b) ifelse(is.na(b) | b == 0, NA_real_, a / b)

tag_from_z <- function(z, lo = -1, hi = 1) {
  if (is.na(z)) return(NA_character_)
  if (z <= lo)  return("low")
  if (z >= hi)  return("high")
  "normal"
}

clamp01 <- function(x) pmin(pmax(x, 0), 1)

draw_attempts <- function(mean_val, sd_val) {
  if (is.na(mean_val) || mean_val <= 0) return(0L)
  if (is.na(sd_val)) sd_val <- 0
  val <- round(rnorm(1, mean = mean_val, sd = sd_val))
  val <- max(val, 0L)
  as.integer(val)
}

pct01 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(default)
  if (x > 1 && x <= 100) x <- x / 100
  pmin(pmax(x, 0), 1)
}

num0 <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), default, x)
}

draw_team_possessions_q <- function(team_id, team_poss_profile_q,
                                    sd_shrink = 0.75,
                                    qtr,
                                    min_q = 1L) {
  prof <- team_poss_profile_q %>%
    filter(team_id == !!team_id, qtr == !!qtr) %>%
    slice(1)
  
  mu <- if (nrow(prof) > 0) prof$POSS_mean else NA_real_
  sd <- if (nrow(prof) > 0) prof$POSS_sd   else 0
  
  if (is.na(mu) || mu <= 0) mu <- 25
  if (is.na(sd)) sd <- 0
  
  sd <- sd * sd_shrink
  poss <- draw_attempts(mu, sd)
  as.integer(pmax(poss, min_q))
}

derive_team_probs_q <- function(team_row, team_to_profile_q, qtr) {
  tid <- team_row$team_id[1]
  
  q_to <- team_to_profile_q %>%
    filter(team_id == tid, qtr == !!qtr) %>%
    slice(1)
  
  p_to <- if (nrow(q_to) > 0) pct01(q_to$TOV_pct, default = 0.13) else pct01(team_row$TOV_pct, default = 0.13)
  live_share <- if (nrow(q_to) > 0) clamp01(q_to$TOV_live_share) else 0.6
  
  fga2_rate <- pmax(0, num0(team_row$FGA2_per_scoring_poss, 0))
  fga3_rate <- pmax(0, num0(team_row$FGA3_per_scoring_poss, 0))
  shot_rate <- fga2_rate + fga3_rate
  
  p_2_given_shot <- if (shot_rate > 0) pct01(fga2_rate / shot_rate, default = 0.65) else 0.65
  
  ft_per_scoring_poss <- pmax(0, num0(team_row$FTA_per_scoring_poss, 0))
  ft_per_shot <- if (shot_rate > 0) ft_per_scoring_poss / shot_rate else 0
  
  avg_fts_per_foul <- 2.0
  p_shoot_foul <- pct01(ft_per_shot / avg_fts_per_foul, default = 0)
  p_shoot_foul <- pmin(p_shoot_foul, 0.16)
  
  list(
    p_to = p_to,
    live_share = live_share,
    p_2_given_shot = p_2_given_shot,
    p_shoot_foul = p_shoot_foul
  )
}

draw_off_fouls_q <- function(team_id, team_foul_profile_q, shot_poss, qtr) {
  prof <- team_foul_profile_q %>%
    filter(team_id == !!team_id, qtr == !!qtr) %>%
    slice(1)
  
  mu <- if (nrow(prof) > 0) num0(prof$OFF_FOULS_mean, 0) else 0
  sd <- if (nrow(prof) > 0) num0(prof$OFF_FOULS_sd,   0) else 0
  
  of <- draw_attempts(mu, sd)
  as.integer(pmin(pmax(of, 0L), shot_poss))
}

simulate_team_offense_B_q <- function(team_row, poss, qtr,
                                      team_to_profile_q,
                                      team_foul_profile_q,
                                      p_and1_given_made_foul = 0.08) {
  
  tid <- team_row$team_id[1]
  
  FG2_pct <- pct01(team_row$FG2_pct, default = 0.50)
  FG3_pct <- pct01(team_row$FG3_pct, default = 0.36)
  FT_pct  <- pct01(team_row$FT_pct,  default = 0.78)
  
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
  
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  made_foul_2_exp <- round(FGM2 * p_foul)
  made_foul_3_exp <- round(FGM3 * p_foul)
  
  and1_2 <- if (made_foul_2_exp > 0) rbinom(1, size = made_foul_2_exp, prob = p_and1_given_made_foul) else 0L
  and1_3 <- if (made_foul_3_exp > 0) rbinom(1, size = made_foul_3_exp, prob = p_and1_given_made_foul) else 0L
  and1_2 <- as.integer(and1_2); and1_3 <- as.integer(and1_3)
  
  missed_foul_2 <- max(foul_2 - made_foul_2_exp, 0L)
  missed_foul_3 <- max(foul_3 - made_foul_3_exp, 0L)
  
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

# ===============================================================
# 7. GAME SCRIPT & QUARTER ENGINE (Option A)
#    - Uses quarter-specific POSS + TOV% + OFF_FOULS profiles
#    - Blowout drag (late pace suppression)
#    - Close-game clutch extension + intentional foul FTs
#    - OT via Q5 / Q6 only when tied after Q4 / OT1
# ===============================================================

# --- helper: get quarter value from a row by suffix ---
get_q <- function(row, prefix, q, default = NA_real_) {
  nm <- paste0(prefix, "_Q", q)
  if (!nm %in% names(row)) return(default)
  out <- suppressWarnings(as.numeric(row[[nm]][1]))
  if (is.na(out)) default else out
}

# --- draw quarter possessions from quarter profile (mean/sd) ---
draw_poss_q <- function(poss_row, q, min_poss = 8L, max_poss = 35L) {
  mu <- get_q(poss_row, "POSS_mean", q, default = NA_real_)
  sd <- get_q(poss_row, "POSS_sd",   q, default = 0)
  
  if (is.na(mu) || mu <= 0) mu <- 24
  if (is.na(sd) || sd < 0) sd <- 0
  
  val <- round(rnorm(1, mean = mu, sd = sd))
  val <- as.integer(pmin(pmax(val, min_poss), max_poss))
  val
}

# --- blowout drag applies only to Q4 possessions (pace drops) ---
apply_blowout_drag <- function(q4_poss, margin_3q, blowout_margin = 18,
                               min_mult = 0.75, max_mult = 0.92) {
  if (is.na(margin_3q)) return(q4_poss)
  if (abs(margin_3q) < blowout_margin) return(q4_poss)
  
  m <- pmin(abs(margin_3q), 30)
  t <- (m - blowout_margin) / (30 - blowout_margin)  # 0..1
  mult <- max_mult - t * (max_mult - min_mult)
  as.integer(pmax(1L, round(q4_poss * mult)))
}

# --- clutch extension: add a few endgame possessions + intentional foul FTs ---
apply_clutch_extension <- function(home_row, away_row, home_pts, away_pts,
                                   close_margin = 6,
                                   max_extra_poss = 6L,
                                   foul_ft_per_trip = 2L,
                                   ft_make_boost = 0.04) {
  margin <- home_pts - away_pts
  if (is.na(margin) || abs(margin) > close_margin) {
    return(list(extra_poss = 0L,
                clutch_fta_home = 0L, clutch_ftm_home = 0L,
                clutch_fta_away = 0L, clutch_ftm_away = 0L))
  }
  
  extra_poss <- as.integer(sample.int(max_extra_poss + 1L, 1) - 1L)  # 0..max
  if (extra_poss <= 0) {
    return(list(extra_poss = 0L,
                clutch_fta_home = 0L, clutch_ftm_home = 0L,
                clutch_fta_away = 0L, clutch_ftm_away = 0L))
  }
  
  home_trailing <- (margin < 0)
  
  FT_home <- clamp01(pct01(home_row$FT_pct, default = 0.78) + ft_make_boost)
  FT_away <- clamp01(pct01(away_row$FT_pct, default = 0.78) + ft_make_boost)
  
  # Not every “extra possession” becomes an intentional foul trip; approximate.
  foul_trips <- as.integer(round(extra_poss * 0.65))
  foul_trips <- pmin(foul_trips, extra_poss)
  
  clutch_fta_home <- 0L
  clutch_fta_away <- 0L
  if (home_trailing) clutch_fta_home <- as.integer(foul_trips * foul_ft_per_trip) else clutch_fta_away <- as.integer(foul_trips * foul_ft_per_trip)
  
  clutch_ftm_home <- if (clutch_fta_home > 0) rbinom(1, clutch_fta_home, FT_home) else 0L
  clutch_ftm_away <- if (clutch_fta_away > 0) rbinom(1, clutch_fta_away, FT_away) else 0L
  
  list(
    extra_poss       = extra_poss,
    clutch_fta_home  = as.integer(clutch_fta_home),
    clutch_ftm_home  = as.integer(clutch_ftm_home),
    clutch_fta_away  = as.integer(clutch_fta_away),
    clutch_ftm_away  = as.integer(clutch_ftm_away)
  )
}

# ---- simulate one game (Option A: quarter-aware + script INSIDE) ----
simulate_game_once <- function(home_team_id, away_team_id,
                               season_chosen, team_mc_profile,
                               team_poss_profile_q,
                               team_to_profile_q,
                               team_foul_profile_q,
                               max_ot = 2L) {
  
  home_row <- team_mc_profile %>% filter(team_id == home_team_id, season == season_chosen) %>% slice(1)
  away_row <- team_mc_profile %>% filter(team_id == away_team_id, season == season_chosen) %>% slice(1)
  if (nrow(home_row) == 0 || nrow(away_row) == 0) stop("Missing MC profile for one or both teams.")
  
  # ---- Q1–Q4 possessions (quarter engine) ----
  home_q_poss <- sapply(1:4, function(q) draw_team_possessions_q(home_team_id, team_poss_profile_q, qtr = q))
  away_q_poss <- sapply(1:4, function(q) draw_team_possessions_q(away_team_id, team_poss_profile_q, qtr = q))
  
  home_pts <- 0L; away_pts <- 0L
  acc <- function() list(tov=0L, live=0L, off=0L, fga2=0L, fgm2=0L, fga3=0L, fgm3=0L, fta=0L, ftm=0L)
  H <- acc(); A <- acc()
  
  # --- quarter points buckets ---
  home_pts_q1 <- 0L; away_pts_q1 <- 0L
  home_pts_q2 <- 0L; away_pts_q2 <- 0L
  home_pts_q3 <- 0L; away_pts_q3 <- 0L
  home_pts_q4 <- 0L; away_pts_q4 <- 0L
  home_pts_q5 <- 0L; away_pts_q5 <- 0L  # OT1
  home_pts_q6 <- 0L; away_pts_q6 <- 0L  # OT2
  
  # ---- Q1–Q3 normal ----
  for (q in 1:3) {
    hb <- simulate_team_offense_B_q(home_row, poss = as.integer(home_q_poss[q]), qtr = q,
                                    team_to_profile_q = team_to_profile_q,
                                    team_foul_profile_q = team_foul_profile_q)
    ab <- simulate_team_offense_B_q(away_row, poss = as.integer(away_q_poss[q]), qtr = q,
                                    team_to_profile_q = team_to_profile_q,
                                    team_foul_profile_q = team_foul_profile_q)
    
    # --- store this quarter's points ---
    hq_pts <- as.integer(hb$base_points)
    aq_pts <- as.integer(ab$base_points)
    
    if (q == 1) { home_pts_q1 <- home_pts_q1 + hq_pts; away_pts_q1 <- away_pts_q1 + aq_pts }
    if (q == 2) { home_pts_q2 <- home_pts_q2 + hq_pts; away_pts_q2 <- away_pts_q2 + aq_pts }
    if (q == 3) { home_pts_q3 <- home_pts_q3 + hq_pts; away_pts_q3 <- away_pts_q3 + aq_pts }
    
    home_pts <- home_pts + hb$base_points
    away_pts <- away_pts + ab$base_points
    
    H$tov <- H$tov + hb$turnovers; H$live <- H$live + hb$live_turnovers; H$off <- H$off + hb$off_fouls
    H$fga2 <- H$fga2 + hb$FGA2; H$fgm2 <- H$fgm2 + hb$FGM2; H$fga3 <- H$fga3 + hb$FGA3; H$fgm3 <- H$fgm3 + hb$FGM3
    H$fta  <- H$fta  + hb$FTA;  H$ftm  <- H$ftm  + hb$FTM
    
    A$tov <- A$tov + ab$turnovers; A$live <- A$live + ab$live_turnovers; A$off <- A$off + ab$off_fouls
    A$fga2 <- A$fga2 + ab$FGA2; A$fgm2 <- A$fgm2 + ab$FGM2; A$fga3 <- A$fga3 + ab$FGA3; A$fgm3 <- A$fgm3 + ab$FGM3
    A$fta  <- A$fta  + ab$FTA;  A$ftm  <- A$ftm  + ab$FTM
  }
  
  # ---- Q4 game script: blowout drag on possessions ----
  margin_3q <- home_pts - away_pts
  
  apply_blowout_drag_simple <- function(poss, margin_3q, blowout_margin = 18L, min_mult = 0.75, max_mult = 0.92) {
    if (is.na(margin_3q) || abs(margin_3q) < blowout_margin) return(as.integer(poss))
    m <- pmin(abs(margin_3q), 30)
    t <- (m - blowout_margin) / (30 - blowout_margin)
    mult <- max_mult - t * (max_mult - min_mult)
    as.integer(pmax(1L, round(poss * mult)))
  }
  
  home_q_poss[4] <- apply_blowout_drag_simple(home_q_poss[4], margin_3q)
  away_q_poss[4] <- apply_blowout_drag_simple(away_q_poss[4], margin_3q)
  
  hb4 <- simulate_team_offense_B_q(home_row, poss = as.integer(home_q_poss[4]), qtr = 4,
                                   team_to_profile_q = team_to_profile_q,
                                   team_foul_profile_q = team_foul_profile_q)
  ab4 <- simulate_team_offense_B_q(away_row, poss = as.integer(away_q_poss[4]), qtr = 4,
                                   team_to_profile_q = team_to_profile_q,
                                   team_foul_profile_q = team_foul_profile_q)
  
  home_pts_q4 <- home_pts_q4 + as.integer(hb4$base_points)
  away_pts_q4 <- away_pts_q4 + as.integer(ab4$base_points)
  
  home_pts <- home_pts + hb4$base_points
  away_pts <- away_pts + ab4$base_points
  
  H$tov <- H$tov + hb4$turnovers; H$live <- H$live + hb4$live_turnovers; H$off <- H$off + hb4$off_fouls
  H$fga2 <- H$fga2 + hb4$FGA2; H$fgm2 <- H$fgm2 + hb4$FGM2; H$fga3 <- H$fga3 + hb4$FGA3; H$fgm3 <- H$fgm3 + hb4$FGM3
  H$fta  <- H$fta  + hb4$FTA;  H$ftm  <- H$ftm  + hb4$FTM
  
  A$tov <- A$tov + ab4$turnovers; A$live <- A$live + ab4$live_turnovers; A$off <- A$off + ab4$off_fouls
  A$fga2 <- A$fga2 + ab4$FGA2; A$fgm2 <- A$fgm2 + ab4$FGM2; A$fga3 <- A$fga3 + ab4$FGA3; A$fgm3 <- A$fgm3 + ab4$FGM3
  A$fta  <- A$fta  + ab4$FTA;  A$ftm  <- A$ftm  + ab4$FTM
  
  # ---- clutch add-on: intentional foul FTs (simple) ----
  add_clutch_fts <- function(home_pts, away_pts, home_row, away_row, close_margin = 6L) {
    margin <- home_pts - away_pts
    if (abs(margin) > close_margin) return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,extra_poss=0L))
    
    extra_poss <- sample.int(7, 1) - 1L  # 0..6
    if (extra_poss <= 0) return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,extra_poss=0L))
    
    foul_trips <- as.integer(round(extra_poss * 0.65))
    if (foul_trips <= 0) return(list(h_fta=0L,h_ftm=0L,a_fta=0L,a_ftm=0L,extra_poss=extra_poss))
    
    home_trailing <- (margin < 0)
    FT_home <- clamp01(pct01(home_row$FT_pct, default = 0.78) + 0.04)
    FT_away <- clamp01(pct01(away_row$FT_pct, default = 0.78) + 0.04)
    
    if (home_trailing) {
      h_fta <- as.integer(2L * foul_trips)
      h_ftm <- as.integer(rbinom(1, h_fta, FT_home))
      return(list(h_fta=h_fta,h_ftm=h_ftm,a_fta=0L,a_ftm=0L,extra_poss=extra_poss))
    } else {
      a_fta <- as.integer(2L * foul_trips)
      a_ftm <- as.integer(rbinom(1, a_fta, FT_away))
      return(list(h_fta=0L,h_ftm=0L,a_fta=a_fta,a_ftm=a_ftm,extra_poss=extra_poss))
    }
  }
  
  clutch <- add_clutch_fts(home_pts, away_pts, home_row, away_row)
  home_pts <- home_pts + clutch$h_ftm
  away_pts <- away_pts + clutch$a_ftm
  H$fta <- H$fta + clutch$h_fta; H$ftm <- H$ftm + clutch$h_ftm
  A$fta <- A$fta + clutch$a_fta; A$ftm <- A$ftm + clutch$a_ftm
  
  # ---- OT only if tied (Q5/Q6) ----
  ot_played <- 0L
  while (home_pts == away_pts && ot_played < max_ot) {
    ot_played <- ot_played + 1L
    q_ot <- 4L + ot_played
    
    hposs <- draw_team_possessions_q(home_team_id, team_poss_profile_q, qtr = q_ot, min_q = 1L)
    aposs <- draw_team_possessions_q(away_team_id, team_poss_profile_q, qtr = q_ot, min_q = 1L)
    
    hb <- simulate_team_offense_B_q(home_row, poss = hposs, qtr = q_ot,
                                    team_to_profile_q = team_to_profile_q,
                                    team_foul_profile_q = team_foul_profile_q)
    ab <- simulate_team_offense_B_q(away_row, poss = aposs, qtr = q_ot,
                                    team_to_profile_q = team_to_profile_q,
                                    team_foul_profile_q = team_foul_profile_q)
    
    # OT quarter points buckets
    if (q_ot == 5) { home_pts_q5 <- home_pts_q5 + as.integer(hb$base_points); away_pts_q5 <- away_pts_q5 + as.integer(ab$base_points) }
    if (q_ot == 6) { home_pts_q6 <- home_pts_q6 + as.integer(hb$base_points); away_pts_q6 <- away_pts_q6 + as.integer(ab$base_points) }
    
    home_pts <- home_pts + hb$base_points
    away_pts <- away_pts + ab$base_points
    
    H$tov <- H$tov + hb$turnovers; H$live <- H$live + hb$live_turnovers; H$off <- H$off + hb$off_fouls
    H$fga2 <- H$fga2 + hb$FGA2; H$fgm2 <- H$fgm2 + hb$FGM2; H$fga3 <- H$fga3 + hb$FGA3; H$fgm3 <- H$fgm3 + hb$FGM3
    H$fta  <- H$fta  + hb$FTA;  H$ftm  <- H$ftm  + hb$FTM
    
    A$tov <- A$tov + ab$turnovers; A$live <- A$live + ab$live_turnovers; A$off <- A$off + ab$off_fouls
    A$fga2 <- A$fga2 + ab$FGA2; A$fgm2 <- A$fgm2 + ab$FGM2; A$fga3 <- A$fga3 + ab$FGA3; A$fgm3 <- A$fgm3 + ab$FGM3
    A$fta  <- A$fta  + ab$FTA;  A$ftm  <- A$ftm  + ab$FTM
  }
  
  tibble(
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
    clutch_extra_poss  = as.integer(clutch$extra_poss),
    clutch_home_fta    = as.integer(clutch$h_fta),
    clutch_home_ftm    = as.integer(clutch$h_ftm),
    clutch_away_fta    = as.integer(clutch$a_fta),
    clutch_away_ftm    = as.integer(clutch$a_ftm)
  )
}



simulate_matchup <- function(home_team_id, away_team_id,
                             season_chosen, team_mc_profile,
                             team_poss_profile_q,
                             team_to_profile_q,
                             team_foul_profile_q,
                             n_sims = 10000L) {
  
  purrr::map_dfr(
    seq_len(n_sims),
    ~ simulate_game_once(
      home_team_id = home_team_id,
      away_team_id = away_team_id,
      season_chosen = season_chosen,
      team_mc_profile = team_mc_profile,
      team_poss_profile_q = team_poss_profile_q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q
    )
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 4. END: Core MC
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. START: Run MC for each game in today's slate (parallel)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

set.seed(123)

if (!all(c("game_id", "team_id", "opp_id") %in% names(nba_schedule_formatted_date))) {
  stop("Expected columns game_id, team_id, opp_id not all present in nba_schedule_formatted_date.")
}

game_indices <- seq_len(nrow(nba_schedule_formatted_date))

mc_sims_list <- future_lapply(
  game_indices,
  function(i) {
    this_row  <- nba_schedule_formatted_date[i, ]
    home_id   <- this_row$team_id
    away_id   <- this_row$opp_id
    this_game <- this_row$game_id
    
    sims <- simulate_matchup(
      home_team_id = home_id,
      away_team_id = away_id,
      season_chosen = season_chosen,
      team_mc_profile = team_mc_profile,
      team_poss_profile_q = team_poss_profile_q,
      team_to_profile_q   = team_to_profile_q,
      team_foul_profile_q = team_foul_profile_q,
      n_sims = 10000L
    )
    
    sims %>%
      mutate(
        game_id      = this_game,
        home_team_id = home_id,
        away_team_id = away_id,
        sim_index    = row_number()
      )
  },
  future.seed = TRUE
)

mc_sims_all_games <- data.table::rbindlist(mc_sims_list, use.names = TRUE, fill = TRUE)

mc_summary_by_game <- mc_sims_all_games %>%
  group_by(game_id, home_team_id, away_team_id) %>%
  summarise(
    MC_HomePts     = mean(home_points),
    MC_AwayPts     = mean(away_points),
    MC_Total       = mean(home_points + away_points),
    MC_Spread      = mean(home_points - away_points),
    MC_HomeWinProb = mean(margin > 0),
    MC_AwayWinProb = mean(margin < 0),
    MC_PushProb    = mean(margin == 0),
    .groups = "drop"
  ) %>%
  rename(
    team_id = home_team_id,
    opp_id  = away_team_id
  )

nba_schedule_formatted_date <- nba_schedule_formatted_date %>%
  left_join(mc_summary_by_game, by = c("game_id", "team_id", "opp_id")) %>%
  relocate(starts_with("MC_"), .before = 1)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. END: Run MC
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 6. START: Save updated schedule (with MC output) to Current Slate
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

save_schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate/nba_schedule_",
  formatted_date,
  ".csv"
)

fwrite(
  nba_schedule_formatted_date,
  save_schedule_path,
  quote = TRUE,
  na = "",
  sep = ","
)

message("Saved updated schedule with MC output to: ", save_schedule_path)

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 6. END: Save
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
