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
# START: Load pm_nbapbp, BaseStats_Team_MC, BaseStats_Player_MC, nba_player_rotations, nba_odds_ file
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     

# 💠💠💠 0. Load pm_nbapbp, BaseStats_Team_MC and BaseStats_Player_MC 💠💠💠

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

# --- Load CURRENT SLATE (nba_schedule_formatted_date) ----------
current_slate_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/0. Current Slate/nba_schedule_",
  formatted_date,
  ".csv"
)
nba_schedule_formatted_date <- fread(current_slate_file, colClasses = "character", encoding = "UTF-8")

# --- Load BaseStats_Team_MC (not used yet, but ready) ----------
basestatsT_mc_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_",
  season_token,
  ".csv"
)
BaseStats_Team_MC <- fread(basestatsT_mc_file, colClasses = "character", encoding = "UTF-8")

# --- Load BaseStats_Player_MC (not used yet, but ready) --------
basestatsP_mc_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_MC_",
  season_token,
  ".csv"
)
BaseStats_Player_MC <- fread(basestatsP_mc_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_rotation_10M (not used yet, but ready) ----------
nba_rotation_10M_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/pm_nba_player_rotations_10M_",
  season_token,
  ".csv"
)
nba_rotations_10M <- fread(nba_rotation_10M_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_rotation_5M (not used yet, but ready) ----------
nba_rotation_5M_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/pm_nba_player_rotations_5M_",
  season_token,
  ".csv"
)
nba_rotations_5M <- fread(nba_rotation_5M_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_points (not used yet, but ready) ----------
nba_odds_points_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_points_",
  season_token,
  ".csv"
)
nba_odds_points <- fread(nba_odds_points_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_assists (not used yet, but ready) ----------
nba_odds_assists_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_points_assists_",
  season_token,
  ".csv"
)
nba_odds_assists <- fread(nba_odds_assists_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_rebounds (not used yet, but ready) ----------
nba_odds_rebounds_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_rebounds_",
  season_token,
  ".csv"
)
nba_odds_rebounds <- fread(nba_odds_rebounds_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_threes (not used yet, but ready) ----------
nba_odds_threes_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_threes_",
  season_token,
  ".csv"
)
nba_odds_threes <- fread(nba_odds_threes_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_steals (not used yet, but ready) ----------
nba_odds_steals_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_steals_",
  season_token,
  ".csv"
)
nba_odds_steals <- fread(nba_odds_steals_file, colClasses = "character", encoding = "UTF-8")

# --- Load nba_odds_blocks (not used yet, but ready) ----------
nba_odds_blocks_file <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_blocks_",
  season_token,
  ".csv"
)
nba_odds_blocks <- fread(nba_odds_blocks_file, colClasses = "character", encoding = "UTF-8")
 
# ---------------------------------------------------------------
# Coerce key numeric fields in PBP
# ---------------------------------------------------------------
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
# END: Load pm_nbapbp, BaseStats_Team_MC, BaseStats_Player_MC, nba_player_rotations, nba_odds_ file
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠    



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     
# 1. START: Build team shooting + scoring profiles from PBP establishes baseline
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     


# --- Identify basic shot events --------------------------------
shot_df <- pbp_df %>%
  filter(!is.na(points_attempted) & points_attempted > 0)

shot_df <- shot_df %>%
  mutate(
    shot_type_mc = case_when(
      points_attempted == 3 ~ "3P",
      points_attempted == 2 ~ "2P",
      points_attempted == 1 ~ "FT",
      TRUE ~ "OTHER"
    )
  )

# --- Simple turnover flag (can be upgraded later) --------------
pbp_df <- pbp_df %>%
  mutate(
    is_turnover_mc = if_else(
      grepl("Turnover", type_text, ignore.case = TRUE),
      1L,
      0L
    )
  )

# --- Per-team shooting profile (FG%, 3P%, FT%) -----------------
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

# --- Per-team per-game volume ----------------------------------
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
# 1. END: Build team shooting + scoring profiles from PBP establishes baseline
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     
# 2. START: Build Baseline for POSSESSIONS & PACE 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠   


# Make sure key fields are numeric / usable
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    T_POSS_CGS = suppressWarnings(as.numeric(T_POSS_CGS)),
    team_id    = as.character(ESPN_TEAM_ID)
  )

# Build per-team possession profile from BaseStats_Team_MC
team_poss_profile <- BaseStats_Team_MC %>%
  filter(!is.na(T_POSS_CGS)) %>%
  group_by(team_id) %>%   # <— only team_id
  summarise(
    POSS_mean = mean(T_POSS_CGS, na.rm = TRUE),
    POSS_sd   = sd(T_POSS_CGS,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    POSS_sd = if_else(is.na(POSS_sd), 0, POSS_sd)
  )


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     
# 2. END: Build Baseline for POSSESSIONS & PACE 
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     
# 3. START: REBOUNDING MODEL (OREB / DREB)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  

# Ensure IDs + key rebounding fields are usable
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID           = as.character(ESPN_TEAM_ID),
    T_OREB_PCT_CGS         = suppressWarnings(as.numeric(T_OREB_PCT_CGS)),
    T_DREB_PCT_CGS         = suppressWarnings(as.numeric(T_DREB_PCT_CGS)),
    OPP_OREB_PCT_ALLO_CGS  = suppressWarnings(as.numeric(OPP_OREB_PCT_ALLO_CGS)),
    OPP_DREB_PCT_ALLO_CGS  = suppressWarnings(as.numeric(OPP_DREB_PCT_ALLO_CGS)),
    T_SEC_CHN_PPP_CGS      = suppressWarnings(as.numeric(T_SEC_CHN_PPP_CGS))
  ) 

# Per-team rebounding profile (season-level)
team_reb_profile <- BaseStats_Team_MC %>%
  group_by(team_id) %>%
  summarise(
    OREB_PCT      = mean(T_OREB_PCT_CGS,        na.rm = TRUE),
    DREB_PCT      = mean(T_DREB_PCT_CGS,        na.rm = TRUE),
    OREB_PCT_ALLO = mean(OPP_OREB_PCT_ALLO_CGS, na.rm = TRUE),
    DREB_PCT_ALLO = mean(OPP_DREB_PCT_ALLO_CGS, na.rm = TRUE),
    SEC_PPP       = mean(T_SEC_CHN_PPP_CGS,     na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(
    OREB_PCT      = pmin(pmax(OREB_PCT,      0), 1),
    DREB_PCT      = pmin(pmax(DREB_PCT,      0), 1),
    OREB_PCT_ALLO = pmin(pmax(OREB_PCT_ALLO, 0), 1),
    DREB_PCT_ALLO = pmin(pmax(DREB_PCT_ALLO, 0), 1),
    SEC_PPP       = if_else(is.na(SEC_PPP) | SEC_PPP < 0, 1, SEC_PPP)
  ) %>%
  rename(ESPN_TEAM_ID = team_id)


# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠     
# 3. END: REBOUNDING MODEL (OREB / DREB)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  
# 4. START: TURNOVERS MODEL
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# Ensure key turnover fields are usable
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID        = as.character(ESPN_TEAM_ID),
    T_TOV_CGS           = suppressWarnings(as.numeric(T_TOV_CGS)),
    T_TOV_LIVEB_CGS     = suppressWarnings(as.numeric(T_TOV_LIVEB_CGS)),
    T_TOV_DEADB_CGS     = suppressWarnings(as.numeric(T_TOV_DEADB_CGS)),
    T_TOV_PCT_CGS       = suppressWarnings(as.numeric(T_TOV_PCT_CGS))
  )

team_to_profile <- BaseStats_Team_MC %>%
  group_by(ESPN_TEAM_ID) %>%
  summarise(
    TOV_mean       = mean(T_TOV_CGS,       na.rm = TRUE),
    TOV_live_mean  = mean(T_TOV_LIVEB_CGS, na.rm = TRUE),
    TOV_pct_raw    = mean(T_TOV_PCT_CGS,   na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(
    # If turnover pct is in 0–100, convert to 0–1
    TOV_pct = case_when(
      is.na(TOV_pct_raw)            ~ 0,
      TOV_pct_raw > 1 & TOV_pct_raw <= 100 ~ TOV_pct_raw / 100,
      TOV_pct_raw < 0               ~ 0,
      TOV_pct_raw > 1               ~ 1,
      TRUE                          ~ TOV_pct_raw
    ),
    # live-ball share of turnovers
    TOV_live_share = dplyr::case_when(
      !is.na(TOV_mean) & TOV_mean > 0 ~ pmin(pmax(TOV_live_mean / TOV_mean, 0), 1),
      TRUE                            ~ 0.6  # fallback default
    )
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  
# 4. END: TURNOVERS MODEL
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠 



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. START: FOULS MODEL (possession killers only)
#     - Use OFFENSIVE FOULS as "dead-ball turnover" events
#     - Do NOT subtract possessions for total fouls (T_FOULS_CGS)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  mutate(
    ESPN_TEAM_ID     = as.character(ESPN_TEAM_ID),
    team_id          = as.character(ESPN_TEAM_ID),
    T_OFF_FOULS_CGS  = suppressWarnings(as.numeric(T_OFF_FOULS_CGS)),
    T_CHRG_DRWN_CGS  = suppressWarnings(as.numeric(T_CHRG_DRWN_CGS)) # optional (defense-side later)
  )

team_foul_profile <- BaseStats_Team_MC %>%
  group_by(team_id) %>%
  summarise(
    OFF_FOULS_mean = mean(T_OFF_FOULS_CGS, na.rm = TRUE),
    OFF_FOULS_sd   = sd(T_OFF_FOULS_CGS,   na.rm = TRUE),
    CHRG_DRWN_mean = mean(T_CHRG_DRWN_CGS, na.rm = TRUE),  # optional, not used in offense sim yet
    CHRG_DRWN_sd   = sd(T_CHRG_DRWN_CGS,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    OFF_FOULS_sd = if_else(is.na(OFF_FOULS_sd), 0, OFF_FOULS_sd),
    CHRG_DRWN_sd = if_else(is.na(CHRG_DRWN_sd), 0, CHRG_DRWN_sd),
    OFF_FOULS_mean = if_else(is.na(OFF_FOULS_mean) | OFF_FOULS_mean < 0, 0, OFF_FOULS_mean),
    CHRG_DRWN_mean = if_else(is.na(CHRG_DRWN_mean) | CHRG_DRWN_mean < 0, 0, CHRG_DRWN_mean)
  )

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 5. END: FOULS MODEL (possession killers only)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  
# 6. START: SHOT QUALITY / SHOT ZONES
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠  
# 6. END: SHOT QUALITY / SHOT ZONES
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 7. START: GAME SCRIPT & LATE-GAME LOGIC
#    - Quarter chunking (Q1–Q4)
#    - Blowout slows pace / suppresses late scoring variance
#    - Close games add possessions + intentional fouls (FT-driven)
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠

# ---- helper: split total possessions into 4 quarters (simple) ---
split_poss_into_quarters <- function(poss_team) {
  # base split (roughly even)
  q <- rep(floor(poss_team / 4), 4)
  rem <- poss_team - sum(q)
  if (rem > 0) q[1:rem] <- q[1:rem] + 1L
  as.integer(q)
}

# ---- helper: apply blowout pace drag in Q4 ----------------------
apply_blowout_drag <- function(q4_poss, margin, blowout_margin = 18,
                               min_mult = 0.75, max_mult = 0.92) {
  if (is.na(margin)) return(q4_poss)
  if (abs(margin) < blowout_margin) return(q4_poss)
  
  # bigger blowout => stronger drag (but never ridiculous)
  # scale margin 18..30 into multiplier range
  m <- pmin(abs(margin), 30)
  t <- (m - blowout_margin) / (30 - blowout_margin)   # 0..1
  mult <- max_mult - t * (max_mult - min_mult)
  as.integer(pmax(1L, round(q4_poss * mult)))
}

# ---- helper: close-game extension + intentional fouls -----------
apply_clutch_extension <- function(home_row, away_row,
                                   home_pts, away_pts,
                                   q4_poss,
                                   close_margin = 6,
                                   max_extra_poss = 6L,
                                   foul_ft_per_poss = 2L,
                                   ft_make_boost = 0.04) {
  margin <- home_pts - away_pts
  if (is.na(margin) || abs(margin) > close_margin) {
    return(list(
      q4_poss_adj = q4_poss,
      extra_poss_home = 0L,
      extra_poss_away = 0L,
      clutch_fta_home = 0L,
      clutch_ftm_home = 0L,
      clutch_fta_away = 0L,
      clutch_ftm_away = 0L
    ))
  }
  
  # In close games, you often see: timeouts + intentional fouls + more possessions.
  # We approximate that as a few extra possessions for BOTH teams.
  extra_poss <- sample.int(max_extra_poss + 1L, 1)  # 0..max
  extra_poss <- as.integer(extra_poss)
  
  # Trailing team gets most of the intentional foul FT benefit.
  # Convert a portion of those “extra possessions” into FT trips.
  home_trailing <- (margin < 0)
  
  FT_home_base <- pct01(home_row$FT_pct, default = 0.78)
  FT_away_base <- pct01(away_row$FT_pct, default = 0.78)
  
  # slightly higher make rate late (set plays, best shooters, etc.) – small boost only
  FT_home <- clamp01(FT_home_base + ft_make_boost)
  FT_away <- clamp01(FT_away_base + ft_make_boost)
  
  # allocate more FT trips to trailing team
  foul_trips <- as.integer(round(extra_poss * 0.65))   # not every extra poss becomes FTs
  foul_trips <- pmin(foul_trips, extra_poss)
  
  clutch_fta_home <- 0L; clutch_fta_away <- 0L
  if (home_trailing) {
    clutch_fta_home <- as.integer(foul_trips * foul_ft_per_poss)
  } else {
    clutch_fta_away <- as.integer(foul_trips * foul_ft_per_poss)
  }
  
  clutch_ftm_home <- if (clutch_fta_home > 0) rbinom(1, clutch_fta_home, FT_home) else 0L
  clutch_ftm_away <- if (clutch_fta_away > 0) rbinom(1, clutch_fta_away, FT_away) else 0L
  
  list(
    q4_poss_adj = as.integer(q4_poss + extra_poss),
    extra_poss_home = extra_poss,
    extra_poss_away = extra_poss,
    clutch_fta_home = as.integer(clutch_fta_home),
    clutch_ftm_home = as.integer(clutch_ftm_home),
    clutch_fta_away = as.integer(clutch_fta_away),
    clutch_ftm_away = as.integer(clutch_ftm_away)
  )
}

# ---- replace simulate_game_once with quarter + script version ---
simulate_game_once <- function(home_team_id, away_team_id, season_chosen, team_mc_profile,
                               min_team, max_team) {
  
  home_row <- team_mc_profile %>%
    dplyr::filter(team_id == home_team_id, season == season_chosen) %>%
    dplyr::slice(1)
  
  away_row <- team_mc_profile %>%
    dplyr::filter(team_id == away_team_id, season == season_chosen) %>%
    dplyr::slice(1)
  
  if (nrow(home_row) == 0 || nrow(away_row) == 0) {
    stop("Missing MC profile for one or both teams.")
  }
  
  # draw per-team possessions for the game
  poss_team <- draw_game_possessions(
    home_row, away_row,
    sd_shrink = 0.75,
    min_team  = min_team,
    max_team  = max_team
  )
  
  # split into quarters
  q_poss <- split_poss_into_quarters(poss_team)
  
  home_pts <- 0L; away_pts <- 0L
  home_off_fouls <- 0L; away_off_fouls <- 0L
  home_tov <- 0L; away_tov <- 0L
  home_live_to <- 0L; away_live_to <- 0L
  home_FGA2 <- 0L; home_FGM2 <- 0L; home_FGA3 <- 0L; home_FGM3 <- 0L; home_FTA <- 0L; home_FTM <- 0L
  away_FGA2 <- 0L; away_FGM2 <- 0L; away_FGA3 <- 0L; away_FGM3 <- 0L; away_FTA <- 0L; away_FTM <- 0L
  
  # Q1–Q3 straightforward
  for (qq in 1:3) {
    hb <- simulate_team_offense_B(home_row, poss = q_poss[qq])
    ab <- simulate_team_offense_B(away_row, poss = q_poss[qq])
    
    home_pts <- home_pts + hb$base_points
    away_pts <- away_pts + ab$base_points
    
    home_off_fouls <- home_off_fouls + num0(hb$off_fouls, 0)
    away_off_fouls <- away_off_fouls + num0(ab$off_fouls, 0)
    
    home_tov <- home_tov + hb$turnovers
    away_tov <- away_tov + ab$turnovers
    home_live_to <- home_live_to + hb$live_turnovers
    away_live_to <- away_live_to + ab$live_turnovers
    
    home_FGA2 <- home_FGA2 + hb$FGA2; home_FGM2 <- home_FGM2 + hb$FGM2
    home_FGA3 <- home_FGA3 + hb$FGA3; home_FGM3 <- home_FGM3 + hb$FGM3
    home_FTA  <- home_FTA  + hb$FTA;  home_FTM  <- home_FTM  + hb$FTM
    
    away_FGA2 <- away_FGA2 + ab$FGA2; away_FGM2 <- away_FGM2 + ab$FGM2
    away_FGA3 <- away_FGA3 + ab$FGA3; away_FGM3 <- away_FGM3 + ab$FGM3
    away_FTA  <- away_FTA  + ab$FTA;  away_FTM  <- away_FTM  + ab$FTM
  }
  
  # Q4: apply blowout drag first
  margin_3q <- home_pts - away_pts
  q4_poss_adj <- apply_blowout_drag(q_poss[4], margin_3q)
  
  # simulate “base” Q4
  hb4 <- simulate_team_offense_B(home_row, poss = q4_poss_adj)
  ab4 <- simulate_team_offense_B(away_row, poss = q4_poss_adj)
  
  home_pts <- home_pts + hb4$base_points
  away_pts <- away_pts + ab4$base_points
  
  home_off_fouls <- home_off_fouls + num0(hb4$off_fouls, 0)
  away_off_fouls <- away_off_fouls + num0(ab4$off_fouls, 0)
  
  home_tov <- home_tov + hb4$turnovers
  away_tov <- away_tov + ab4$turnovers
  home_live_to <- home_live_to + hb4$live_turnovers
  away_live_to <- away_live_to + ab4$live_turnovers
  
  home_FGA2 <- home_FGA2 + hb4$FGA2; home_FGM2 <- home_FGM2 + hb4$FGM2
  home_FGA3 <- home_FGA3 + hb4$FGA3; home_FGM3 <- home_FGM3 + hb4$FGM3
  home_FTA  <- home_FTA  + hb4$FTA;  home_FTM  <- home_FTM  + hb4$FTM
  
  away_FGA2 <- away_FGA2 + ab4$FGA2; away_FGM2 <- away_FGM2 + ab4$FGM2
  away_FGA3 <- away_FGA3 + ab4$FGA3; away_FGM3 <- away_FGM3 + ab4$FGM3
  away_FTA  <- away_FTA  + ab4$FTA;  away_FTM  <- away_FTM  + ab4$FTM
  
  # If close late: add clutch extension + intentional foul FTs
  clutch <- apply_clutch_extension(
    home_row, away_row,
    home_pts, away_pts,
    q4_poss_adj
  )
  
  # Add FT points (FTM) directly (these are “late-game intentional foul” freebies)
  home_pts <- home_pts + clutch$clutch_ftm_home
  away_pts <- away_pts + clutch$clutch_ftm_away
  
  # Track them as FTA/FTM too (so totals reconcile)
  home_FTA <- home_FTA + clutch$clutch_fta_home
  home_FTM <- home_FTM + clutch$clutch_ftm_home
  away_FTA <- away_FTA + clutch$clutch_fta_away
  away_FTM <- away_FTM + clutch$clutch_ftm_away
  
  # realized 3P rates for tags
  home_fg3_real <- safe_div(home_FGM3, home_FGA3)
  away_fg3_real <- safe_div(away_FGM3, away_FGA3)
  
  # pace tags (game-level: compare poss_team vs season mean)
  home_poss_z <- safe_div(poss_team - home_row$POSS_mean, home_row$POSS_sd)
  away_poss_z <- safe_div(poss_team - away_row$POSS_mean, away_row$POSS_sd)
  
  tibble::tibble(
    home_team_id     = home_team_id,
    away_team_id     = away_team_id,
    season           = season_chosen,
    
    home_points      = as.integer(home_pts),
    away_points      = as.integer(away_pts),
    margin           = as.integer(home_pts - away_pts),
    
    poss_team        = as.integer(poss_team),
    poss_game_comb   = as.integer(poss_team * 2L),
    
    home_turnovers   = as.integer(home_tov),
    away_turnovers   = as.integer(away_tov),
    home_live_to     = as.integer(home_live_to),
    away_live_to     = as.integer(away_live_to),
    
    home_off_fouls   = as.integer(home_off_fouls),
    away_off_fouls   = as.integer(away_off_fouls),
    
    home_FGA2        = as.integer(home_FGA2), home_FGM2 = as.integer(home_FGM2),
    away_FGA2        = as.integer(away_FGA2), away_FGM2 = as.integer(away_FGM2),
    
    home_FGA3        = as.integer(home_FGA3), home_FGM3 = as.integer(home_FGM3),
    away_FGA3        = as.integer(away_FGA3), away_FGM3 = as.integer(away_FGM3),
    
    home_FTA         = as.integer(home_FTA),  home_FTM  = as.integer(home_FTM),
    away_FTA         = as.integer(away_FTA),  away_FTM  = as.integer(away_FTM),
    
    home_FG3_real    = home_fg3_real,
    away_FG3_real    = away_fg3_real,
    
    # script visibility (debug drivers)
    margin_3q        = as.integer(margin_3q),
    q4_poss_base     = as.integer(q_poss[4]),
    q4_poss_blowout  = as.integer(q4_poss_adj),
    clutch_fta_home  = as.integer(clutch$clutch_fta_home),
    clutch_ftm_home  = as.integer(clutch$clutch_ftm_home),
    clutch_fta_away  = as.integer(clutch$clutch_fta_away),
    clutch_ftm_away  = as.integer(clutch$clutch_ftm_away),
    
    home_pace_tag    = tag_from_z(home_poss_z),
    away_pace_tag    = tag_from_z(away_poss_z)
  )
}

# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠
# 7. END: GAME SCRIPT & LATE-GAME LOGIC
# 💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠💠



# 4. FOULS & FREE THROWS / BONUS
# 5. SHOT QUALITY / SHOT ZONES
# 6. OPPONENT DEFENSE INTERACTION
# 7. GAME SCRIPT & LATE-GAME LOGIC
# 8. LINEUPS & USAGE

#Separate each MC module into its own section (shooting → possessions → rebounding → turnovers → fouls, etc.).
#After each addition, re-run the Monte Carlo engine and measure the deltas in predicted win probability, spread, and totals.
#This isolates the predictive impact of each module.



# ===============================================================
# 2. Merge into a single team MC profile
# ===============================================================
team_mc_profile <- team_shooting_profile %>%
  inner_join(team_volume_profile, by = c("team_id", "season")) %>%
  left_join(team_poss_profile,    by = "team_id") %>%                         # possessions
  left_join(team_reb_profile,     by = c("team_id" = "ESPN_TEAM_ID")) %>%     # rebounding
  left_join(team_to_profile,      by = c("team_id" = "ESPN_TEAM_ID")) %>%
  left_join(team_foul_profile,    by = "team_id") %>% # turnovers
  mutate(
    # mean scoring possessions (possessions that did NOT end in TO)
    SCORING_POSS_mean = if_else(
      POSS_mean > 0,
      POSS_mean * (1 - TOV_pct),
      POSS_mean
    ),
    SCORING_POSS_mean = if_else(
      is.na(SCORING_POSS_mean) | SCORING_POSS_mean <= 0,
      POSS_mean,  # fallback
      SCORING_POSS_mean
    ),
    
    # shot + FT rates per *scoring* possession
    FGA2_per_scoring_poss = if_else(SCORING_POSS_mean > 0, FGA2_mean / SCORING_POSS_mean, 0),
    FGA3_per_scoring_poss = if_else(SCORING_POSS_mean > 0, FGA3_mean / SCORING_POSS_mean, 0),
    FTA_per_scoring_poss  = if_else(SCORING_POSS_mean > 0, FTA_mean  / SCORING_POSS_mean, 0)
  )

team_lookup <- pbp_df %>%
  select(team_id, home_team_id, away_team_id,
         home_team_abbrev, away_team_abbrev) %>%
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
  left_join(team_lookup, by = "team_id")

season_chosen <- unique(team_mc_profile$season)[1]



# ===============================================================
# 3. Core MC functions (Option B: shot-first possession model)
#   FIX: T_POSS_CGS is BOTH-TEAMS COMBINED possessions.
#        So per-team possessions = combined / 2.
#   FIX: future_lapply workers need team_mc_profile passed in.
#   UPDATE: remove arbitrary poss caps; instead cap to observed
#           min/max of T_POSS_CGS (combined scale).
# ===============================================================

# ---- helpers ---------------------------------------------------
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

# ---- pct + numeric sanitizers (prevents 0–100 % fields from breaking sims) ---
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


# ---- observed min/max (combined both teams) from BaseStats_Team_MC ----
poss_bounds <- BaseStats_Team_MC %>%
  mutate(T_POSS_CGS = suppressWarnings(as.numeric(T_POSS_CGS))) %>%
  filter(!is.na(T_POSS_CGS), T_POSS_CGS > 0) %>%
  summarise(
    min_team = floor(min(T_POSS_CGS)),
    max_team = ceiling(max(T_POSS_CGS)),
    .groups = "drop"
  )

min_team <- poss_bounds$min_team
max_team <- poss_bounds$max_team

cat("Observed T_POSS_CGS bounds (PER-TEAM):", min_team, "to", max_team, "\n")
cat("Implied per-team bounds:", floor(min_team/2), "to", ceiling(max_team/2), "\n")

# ---- shared COMBINED pace draw, then split to per-team ---------
draw_game_possessions <- function(home_row, away_row,
                                  sd_shrink = 0.75,
                                  min_team,
                                  max_team) {
  
  # POSS_mean/POSS_sd are now per-team pace
  mu_team <- mean(c(home_row$POSS_mean, away_row$POSS_mean), na.rm = TRUE)
  sd_team <- mean(c(home_row$POSS_sd,   away_row$POSS_sd),   na.rm = TRUE)
  
  if (is.na(mu_team) || mu_team <= 0) mu_team <- mean(c(min_team, max_team), na.rm = TRUE)
  if (is.na(sd_team)) sd_team <- 0
  
  sd_team <- sd_team * sd_shrink
  
  poss_team <- draw_attempts(mu_team, sd_team)
  poss_team <- max(poss_team, 1L)
  
  # cap only to observed per-team extremes
  poss_team <- as.integer(pmin(pmax(poss_team, min_team), max_team))
  
  poss_team
}

# ---- convert your per-scoring-poss rates into usable probs -----
derive_team_probs <- function(team_row) {
  
  # FIX: TOV_pct may be 0–100, scale to 0–1 safely
  p_to <- pct01(team_row$TOV_pct, default = 0.13)
  
  # FIX: rate fields may be character/NA
  fga2_rate <- pmax(0, num0(team_row$FGA2_per_scoring_poss, 0))
  fga3_rate <- pmax(0, num0(team_row$FGA3_per_scoring_poss, 0))
  shot_rate <- fga2_rate + fga3_rate
  
  p_2_given_shot <- if (shot_rate > 0) {
    pct01(fga2_rate / shot_rate, default = 0.65)
  } else {
    0.65
  }
  p_3_given_shot <- 1 - p_2_given_shot
  
  ft_per_scoring_poss <- pmax(0, num0(team_row$FTA_per_scoring_poss, 0))
  ft_per_shot <- if (shot_rate > 0) ft_per_scoring_poss / shot_rate else 0
  
  # Convert to shooting-foul probability (approx). Cap to keep sane.
  avg_fts_per_foul <- 2.0
  p_shoot_foul <- pct01(ft_per_shot / avg_fts_per_foul, default = 0)
  p_shoot_foul <- pmin(p_shoot_foul, 0.16)   # guardrail (tunable)
  
  list(
    p_to = p_to,
    p_2_given_shot = p_2_given_shot,
    p_3_given_shot = p_3_given_shot,
    p_shoot_foul = p_shoot_foul
  )
}


# ---- simulate one team's offense over N possessions ------------
simulate_team_offense_B <- function(team_row, poss,
                                    p_and1_given_made_foul = 0.08) {
  
  # FIX: pct fields may be 0–100 (or character)
  FG2_pct <- pct01(team_row$FG2_pct, default = 0.50)
  FG3_pct <- pct01(team_row$FG3_pct, default = 0.36)
  FT_pct  <- pct01(team_row$FT_pct,  default = 0.78)
  
  
  probs <- derive_team_probs(team_row)
  p_to        <- probs$p_to
  p2_given_sh <- probs$p_2_given_shot
  p_foul      <- probs$p_shoot_foul
  
  # 1) Turnovers
  total_tov <- if (p_to > 0) rbinom(1, size = poss, prob = p_to) else 0L
  total_tov <- as.integer(total_tov)
  
  live_share <- ifelse(is.na(team_row$TOV_live_share), 0.6, team_row$TOV_live_share)
  live_share <- clamp01(live_share)
  
  live_tov <- if (total_tov > 0) rbinom(1, size = total_tov, prob = live_share) else 0L
  live_tov <- as.integer(live_tov)
  
  # 2) Shots on non-TO possessions (1 shot per shot-possession for now)
  # 2) Shots on non-TO possessions (1 shot per shot-possession for now)
  shot_poss <- max(poss - total_tov, 0L)
  
  # --- Offensive fouls as extra dead-ball turnovers (possession killers) ---
  off_mu <- num0(team_row$OFF_FOULS_mean, 0)
  off_sd <- num0(team_row$OFF_FOULS_sd,   0)
  
  off_fouls <- draw_attempts(off_mu, off_sd)
  off_fouls <- as.integer(pmin(pmax(off_fouls, 0L), shot_poss))
  
  # remove those possessions from shot chances
  shot_poss <- as.integer(pmax(shot_poss - off_fouls, 0L))
  
  # count them as additional turnovers (dead-ball, so NOT added to live_tov)
  total_tov <- as.integer(total_tov + off_fouls)
  
  total_shots <- shot_poss
  
  
  # 3) Split shots into 2PA / 3PA
  FGA2 <- if (total_shots > 0) rbinom(1, size = total_shots, prob = p2_given_sh) else 0L
  FGA2 <- as.integer(FGA2)
  FGA3 <- as.integer(total_shots - FGA2)
  
  # 4) Makes
  FGM2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = FG2_pct) else 0L
  FGM3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = FG3_pct) else 0L
  FGM2 <- as.integer(FGM2); FGM3 <- as.integer(FGM3)
  
  # 5) Shooting fouls (attached to shots)
  foul_2 <- if (FGA2 > 0) rbinom(1, size = FGA2, prob = p_foul) else 0L
  foul_3 <- if (FGA3 > 0) rbinom(1, size = FGA3, prob = p_foul) else 0L
  foul_2 <- as.integer(foul_2); foul_3 <- as.integer(foul_3)
  
  # 6) And-1 logic (approx)
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
  
  base_pts <- as.integer(2L * FGM2 + 3L * FGM3 + 1L * FTM)
  
  list(
    base_points     = base_pts,
    poss_total      = as.integer(poss),
    turnovers       = total_tov,
    live_turnovers  = live_tov,
    off_fouls       = off_fouls,   # <--- add this
    FGA2            = FGA2, FGM2 = FGM2,
    FGA3            = FGA3, FGM3 = FGM3,
    FTA             = FTA,  FTM  = FTM
  )
}

# ---- simulate one game (NOTE: team_mc_profile is passed in) ----
simulate_game_once <- function(home_team_id, away_team_id, season_chosen, team_mc_profile,
                               min_team, max_team) {
  
  home_row <- team_mc_profile %>%
    dplyr::filter(team_id == home_team_id, season == season_chosen) %>%
    dplyr::slice(1)
  
  away_row <- team_mc_profile %>%
    dplyr::filter(team_id == away_team_id, season == season_chosen) %>%
    dplyr::slice(1)
  
  if (nrow(home_row) == 0 || nrow(away_row) == 0) {
    stop("Missing MC profile for one or both teams.")
  }
  
  # Corrected: draw COMBINED pace (capped to observed min/max), then split to per-team
  poss_team <- draw_game_possessions(
    home_row, away_row,
    sd_shrink = 0.75,
    min_team  = min_team,
    max_team  = max_team
  )
  
  
  home_base <- simulate_team_offense_B(home_row, poss = poss_team)
  away_base <- simulate_team_offense_B(away_row, poss = poss_team)
  
  home_points <- home_base$base_points
  away_points <- away_base$base_points
  
  # ---- tags / realized rates ----
  home_fg3_real <- safe_div(home_base$FGM3, home_base$FGA3)
  away_fg3_real <- safe_div(away_base$FGM3, away_base$FGA3)
  
  home_poss_z <- safe_div(home_base$poss_total - home_row$POSS_mean, home_row$POSS_sd)
  away_poss_z <- safe_div(away_base$poss_total - away_row$POSS_mean, away_row$POSS_sd)
  
  home_to_exp <- home_base$poss_total * ifelse(is.na(home_row$TOV_pct), 0, home_row$TOV_pct)
  away_to_exp <- away_base$poss_total * ifelse(is.na(away_row$TOV_pct), 0, away_row$TOV_pct)
  
  home_to_z <- safe_div(home_base$turnovers - home_to_exp, sqrt(pmax(home_to_exp, 1)))
  away_to_z <- safe_div(away_base$turnovers - away_to_exp, sqrt(pmax(away_to_exp, 1)))
  
  home_fg3_se <- sqrt(pmax(home_row$FG3_pct * (1 - home_row$FG3_pct) / pmax(home_base$FGA3, 1), 1e-6))
  away_fg3_se <- sqrt(pmax(away_row$FG3_pct * (1 - away_row$FG3_pct) / pmax(away_base$FGA3, 1), 1e-6))
  
  home_fg3_z <- safe_div(home_fg3_real - home_row$FG3_pct, home_fg3_se)
  away_fg3_z <- safe_div(away_fg3_real - away_row$FG3_pct, away_fg3_se)
  
  tibble::tibble(
    home_team_id     = home_team_id,
    away_team_id     = away_team_id,
    season           = season_chosen,
    
    home_points      = home_points,
    away_points      = away_points,
    margin           = home_points - away_points,
    
    # drivers
    poss_team        = poss_team,
    poss_game_comb   = poss_team * 2L,
    
    home_poss        = home_base$poss_total,
    away_poss        = away_base$poss_total,
    
    home_turnovers   = home_base$turnovers,
    away_turnovers   = away_base$turnovers,
    home_live_to     = home_base$live_turnovers,
    away_live_to     = away_base$live_turnovers,
    
    home_FGA2        = home_base$FGA2, home_FGM2 = home_base$FGM2,
    away_FGA2        = away_base$FGA2, away_FGM2 = away_base$FGM2,
    
    home_FGA3        = home_base$FGA3, home_FGM3 = home_base$FGM3,
    away_FGA3        = away_base$FGA3, away_FGM3 = away_base$FGM3,
    
    home_FTA         = home_base$FTA,  home_FTM  = home_base$FTM,
    away_FTA         = away_base$FTA,  away_FTM  = away_base$FTM,
    
    home_FG3_real    = home_fg3_real,
    away_FG3_real    = away_fg3_real,
    
    # regime tags
    home_pace_tag    = tag_from_z(home_poss_z),
    away_pace_tag    = tag_from_z(away_poss_z),
    home_to_tag      = tag_from_z(home_to_z),
    away_to_tag      = tag_from_z(away_to_z),
    home_3p_tag      = tag_from_z(home_fg3_z),
    away_3p_tag      = tag_from_z(away_fg3_z)
  )
}

simulate_matchup <- function(home_team_id, away_team_id,
                             season_chosen, team_mc_profile,
                             min_team, max_team,
                             n_sims = 10000L) {
  purrr::map_dfr(
    seq_len(n_sims),
    ~ simulate_game_once(home_team_id, away_team_id, season_chosen, team_mc_profile, min_team, max_team)
  )
}





# ===============================================================
# 4. Run MC for each game in today's slate (parallel)
# ===============================================================

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
      home_team_id    = home_id,
      away_team_id    = away_id,
      season_chosen   = season_chosen,
      team_mc_profile = team_mc_profile,
      min_team        = min_team,
      max_team        = max_team,
      n_sims          = 10000L
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

# Build per-game summary and join back to schedule
mc_summary_by_game <- mc_sims_all_games %>%
  group_by(game_id, home_team_id, away_team_id) %>%
  summarise(
    MC_HomePts      = mean(home_points),
    MC_AwayPts      = mean(away_points),
    MC_Total        = mean(home_points + away_points),
    MC_Spread       = mean(home_points - away_points),  # home - away
    MC_HomeWinProb  = mean(margin > 0),
    MC_AwayWinProb  = mean(margin < 0),
    MC_PushProb     = mean(margin == 0),
    .groups = "drop"
  ) %>%
  rename(
    team_id = home_team_id,
    opp_id  = away_team_id
  )

nba_schedule_formatted_date <- nba_schedule_formatted_date %>%
  left_join(mc_summary_by_game,
            by = c("game_id", "team_id", "opp_id"))

# At this point:
# - nba_schedule_formatted_date has MC_* prediction columns per game
# - mc_sims_all_games contains every one of the 10k sim results per game

nba_schedule_formatted_date <- nba_schedule_formatted_date %>%
  dplyr::relocate(starts_with("MC_"), .before = 1)



# ============================================================
# 6. SAVE UPDATED SCHEDULE (WITH MC OUTPUT) TO CURRENT SLATE
# ============================================================

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


