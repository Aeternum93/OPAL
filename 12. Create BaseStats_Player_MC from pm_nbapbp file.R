# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     oooooooooo.               o8o  oooo        .o8       oooooooooo.                                .oooooo..o     .                 .                        ooooooooo.   oooo                                                       ooo        ooooo   .oooooo.   
#     `888'   `Y8b              `"'  `888       "888       `888'   `Y8b                              d8P'    `Y8   .o8               .o8                        `888   `Y88. `888                                                       `88.       .888'  d8P'  `Y8b  
#      888     888 oooo  oooo  oooo   888   .oooo888        888     888  .oooo.    .oooo.o  .ooooo.  Y88bo.      .o888oo  .oooo.   .o888oo  .oooo.o              888   .d88'  888   .oooo.   oooo    ooo  .ooooo.  oooo d8b              888b     d'888  888          
#      888oooo888' `888  `888  `888   888  d88' `888        888oooo888' `P  )88b  d88(  "8 d88' `88b  `"Y8888o.    888   `P  )88b    888   d88(  "8              888ooo88P'   888  `P  )88b   `88.  .8'  d88' `88b `888""8P              8 Y88. .P  888  888          
#      888    `88b  888   888   888   888  888   888        888    `88b  .oP"888  `"Y88b.  888ooo888      `"Y88b   888    .oP"888    888   `"Y88b.               888          888   .oP"888    `88..8'   888ooo888  888                  8  `888'   888  888          
#      888    .88P  888   888   888   888  888   888        888    .88P d8(  888  o.  )88b 888    .o oo     .d8P   888 . d8(  888    888 . o.  )88b              888          888  d8(  888     `888'    888    .o  888                  8    Y     888  `88b    ooo  
#     o888bood8P'   `V88V"V8P' o888o o888o `Y8bod88P"      o888bood8P'  `Y888""8o 8""888P' `Y8bod8P' 8""88888P'    "888" `Y888""8o   "888" 8""888P' ooooooooooo o888o        o888o `Y888""8o     .8'     `Y8bod8P' d888b    ooooooooooo o8o        o888o  `Y8bood8P'  
#                                                                                                                                                       
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Intial BaseStats_Player_MC Build Logic ====
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
  "run_batch",
  
  #BaseStats_Team_MC save
  "BaseStats_Team_MC",
  "BaseStats_Team",
  "BaseStats_Player"
  
)))



gc()
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(readr)




team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_",
  season_token, ".csv"
)

# 2.2 Load BaseStats_Team (if exists)
if (file.exists(team_output_path) && file.info(team_output_path)$size > 0) {
  BaseStats_Team_MC <- read.csv(team_output_path, stringsAsFactors = FALSE)
  if ("NBA_GAME_ID" %in% names(BaseStats_Team_MC)) BaseStats_Team_MC$NBA_GAME_ID <- as.character(BaseStats_Team_MC$NBA_GAME_ID)
  if ("NBA_TEAM_ID" %in% names(BaseStats_Team_MC)) BaseStats_Team_MC$NBA_TEAM_ID <- as.character(BaseStats_Team_MC$NBA_TEAM_ID)
  if ("ESPN_GAME_ID" %in% names(BaseStats_Team_MC)) BaseStats_Team_MC$ESPN_GAME_ID <- as.character(BaseStats_Team_MC$ESPN_GAME_ID)
  if ("ESPN_TEAM_ID" %in% names(BaseStats_Team_MC)) BaseStats_Team_MC$ESPN_TEAM_ID <- as.character(BaseStats_Team_MC$ESPN_TEAM_ID)
  if ("GAME_DATE"  %in% names(BaseStats_Team_MC)) BaseStats_Team_MC$GAME_DATE  <- as.Date(BaseStats_Team_MC$GAME_DATE)
} else {
  BaseStats_Team_MC <- data.frame()
}


# --- CONFIG ---
base_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/"
pm_path <- file.path(base_dir, paste0("pm_nbapbp_", season_token, ".csv"))
stopifnot(file.exists(pm_path))


cat("\n[BaseStats_Player_MC] Loading pm file and seeding player rows...\n")

# --- LOAD AS CHARACTER (same as Team_MC) ---
pm_df <- fread(pm_path, colClasses = "character") %>% as_tibble()

# Optional score diff like Team_MC (kept for parity; not used directly here)
pm_df <- pm_df %>%
  mutate(score_diff = abs(as.integer(away_score) - as.integer(home_score)))

# --- MARKER & SCORE COLS (same method) ---
marker_col <- case_when(
  "text" %in% names(pm_df) ~ "text",
  "desc" %in% names(pm_df) ~ "desc",
  TRUE ~ NA_character_
)
stopifnot(!is.na(marker_col))

home_score_col <- if ("home_score" %in% names(pm_df)) "home_score" else NA_character_
away_score_col <- if ("away_score" %in% names(pm_df)) "away_score" else NA_character_
stopifnot(!is.na(home_score_col), !is.na(away_score_col))

# --- DECISION PER GAME (same logic as Team_MC) ---
decisions <-
  pm_df %>%
  filter(.data[[marker_col]] %in% c("End of the 4th Quarter",
                                    "End of the 1st Overtime",
                                    "End of the 2nd Overtime")) %>%
  transmute(
    game_id,
    hs = suppressWarnings(as.integer(.data[[home_score_col]])),
    `as` = suppressWarnings(as.integer(.data[[away_score_col]])),
    level = .data[[marker_col]],
    order_level = case_when(
      level == "End of the 4th Quarter"  ~ 1L,
      level == "End of the 1st Overtime" ~ 2L,
      level == "End of the 2nd Overtime" ~ 3L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(game_id, order_level) %>%
  group_by(game_id) %>%
  filter(hs != `as`) %>%             # first non-tied checkpoint
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    game_id,
    WINNER_SIDE = if_else(hs > `as`, "home", "away"),
    DECIDED_AT  = case_when(
      order_level == 1L ~ "OT0",
      order_level == 2L ~ "OT1",
      order_level == 3L ~ "OT2",
      TRUE ~ NA_character_
    )
  )

# --- PER-GAME META (carry ids exactly like Team_MC) ---
games <-
  pm_df %>%
  group_by(game_id) %>%
  summarise(
    nba_game_id       = first(nba_game_id),
    nba_team_id       = first(nba_team_id),
    home_team_id      = first(home_team_id),
    away_team_id      = first(away_team_id),
    home_team_abbrev  = coalesce(first(home_team_abbrev), NA_character_),
    away_team_abbrev  = coalesce(first(away_team_abbrev), NA_character_),
    game_date         = first(game_date),
    .groups = "drop"
  )

# --- BUILD PLAYER ROWS FROM EVENTS (athlete_id_1) ---
# Map each event's team_id to HOME_AWAY using the same game-local home/away ids,
# then reduce to unique (game, team, player) combos.
players_seed <-
  pm_df %>%
  select(
    game_id, nba_game_id, nba_team_id, team_id,
    home_team_id, away_team_id,
    home_team_abbrev, away_team_abbrev,
    game_date, athlete_id_1
  ) %>%
  mutate(
    HOME_AWAY = case_when(
      team_id == home_team_id ~ "home",
      team_id == away_team_id ~ "away",
      TRUE ~ NA_character_
    ),
    TEAM = case_when(
      HOME_AWAY == "home" ~ home_team_abbrev,
      HOME_AWAY == "away" ~ away_team_abbrev,
      TRUE ~ NA_character_
    ),
    ESPN_PLAYER_ID = athlete_id_1
  ) %>%
  filter(!is.na(ESPN_PLAYER_ID), ESPN_PLAYER_ID != "", !is.na(HOME_AWAY)) %>%
  distinct(game_id, team_id, ESPN_PLAYER_ID, .keep_all = TRUE)

# --- ATTACH DECISIONS & FINAL ADMIN FIELDS (mirror Team_MC fields) ---
BaseStats_Player_MC <-
  players_seed %>%
  left_join(decisions, by = "game_id") %>%
  mutate(
    ESPN_GAME_ID = as.character(game_id),
    NBA_GAME_ID  = as.character(nba_game_id),
    ESPN_TEAM_ID = as.character(team_id),
    NBA_TEAM_ID  = as.character(nba_team_id),
    GAME_DATE    = as.character(game_date),
    IS_HOME      = if_else(HOME_AWAY == "home", 1L, 0L),
    IS_AWAY      = if_else(HOME_AWAY == "away", 1L, 0L),
    TEAM_WINNER  = if_else(!is.na(WINNER_SIDE) & HOME_AWAY == WINNER_SIDE, 1L, 0L),
    OT_1         = if_else(DECIDED_AT == "OT1", 1L, 0L),
    OT_2         = if_else(DECIDED_AT == "OT2", 1L, 0L)
  ) %>%
  select(
    ESPN_GAME_ID, NBA_GAME_ID, ESPN_TEAM_ID, NBA_TEAM_ID,
    GAME_DATE, TEAM_WINNER, OT_1, OT_2,
    HOME_AWAY, IS_AWAY, IS_HOME, TEAM,
    ESPN_PLAYER_ID
  ) %>%
  arrange(ESPN_GAME_ID, desc(IS_HOME), ESPN_TEAM_ID, ESPN_PLAYER_ID)


# Map each (ESPN_GAME_ID, ESPN_TEAM_ID) to its opponent TEAM once
opp_map <- BaseStats_Player_MC %>%
  distinct(ESPN_GAME_ID, ESPN_TEAM_ID, TEAM) %>%
  inner_join(
    distinct(BaseStats_Player_MC, ESPN_GAME_ID, ESPN_TEAM_ID, TEAM),
    by = "ESPN_GAME_ID",
    suffix = c("", "_OPP")
  ) %>%
  filter(ESPN_TEAM_ID != ESPN_TEAM_ID_OPP) %>%
  transmute(ESPN_GAME_ID, ESPN_TEAM_ID, OPP = TEAM_OPP) %>%
  distinct()

# Attach OPP to every player row
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  left_join(opp_map, by = c("ESPN_GAME_ID","ESPN_TEAM_ID"))

cat(sprintf("[BaseStats_Player_MC] Seeded %s player-game-team rows.\n",
            format(nrow(BaseStats_Player_MC), big.mark = ",")))


pm_df$def_team_id<- ifelse(pm_df$team_id == pm_df$home_team_id,
                           pm_df$away_team_id,
                           pm_df$home_team_id)
# cleanup
rm(players_seed, games, decisions)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Intial BaseStats_Player_MC Build Logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Add Additional Columns from BaseStats_Player to BaseStats_Player_MC Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- PATH: hoopr_name_mapping.csv ---
# Located in your Data Cleanup folder
name_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"
stopifnot(file.exists(name_map_path))


hoopr_df <- fread(name_map_path, colClasses = "character", encoding = "UTF-8") %>% as_tibble()

# sanity: expected cols
nm_id_col   <- if ("espn_player_id" %in% names(hoopr_df)) "espn_player_id" else NA_character_
nm_name_col <- if ("clean_player_name"    %in% names(hoopr_df)) "clean_player_name"    else NA_character_
stopifnot(!is.na(nm_id_col), !is.na(nm_name_col))

# de-dup: one name per espn_player_id
hoopr_map <- hoopr_df %>%
  transmute(
    espn_player_id = .data[[nm_id_col]],
    player_name    = .data[[nm_name_col]]
  ) %>%
  mutate(
    espn_player_id = stringr::str_trim(espn_player_id),
    player_name    = stringr::str_trim(player_name)
  ) %>%
  filter(!is.na(espn_player_id), espn_player_id != "") %>%
  group_by(espn_player_id) %>%
  summarise(player_name = dplyr::coalesce(dplyr::first(player_name[!is.na(player_name) & player_name != ""]), dplyr::first(player_name)), .groups = "drop")

# attach to BaseStats_Player_MC and name column PLAYER_NAME
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  left_join(hoopr_map, by = c("ESPN_PLAYER_ID" = "espn_player_id")) %>%
  mutate(PLAYER_NAME = as.character(player_name)) %>%
  select(-player_name) %>%
  relocate(PLAYER_NAME, .after = ESPN_PLAYER_ID)




# --- Fix TEAM abbreviations in BaseStats_Player_MC -----------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    TEAM = case_when(
      TEAM == "GS"   ~ "GSW",
      TEAM == "NY"   ~ "NYK",
      TEAM == "WSH"  ~ "WAS",
      TEAM == "NO"   ~ "NOP",
      TEAM == "SA"   ~ "SAS",
      TEAM == "UTAH" ~ "UTA",
      TRUE ~ TEAM
    )
  )


BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    OPP = case_when(
      OPP == "GS"   ~ "GSW",
      OPP == "NY"   ~ "NYK",
      OPP == "WSH"  ~ "WAS",
      OPP == "NO"   ~ "NOP",
      OPP == "SA"   ~ "SAS",
      OPP == "UTAH" ~ "UTA",
      TRUE ~ OPP
    )
  )



# --- Load BaseStats_Player for this season -------------------------------------
bsp_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/",
  "BaseStats_Player_", season_token, ".csv"
)

BaseStats_Player <- fread(bsp_path, colClasses = "character", encoding = "UTF-8")

#
# --- Build combined keys --------------------------------------------------------
key_mc  <- paste0(BaseStats_Player_MC$ESPN_PLAYER_ID, "_", BaseStats_Player_MC$ESPN_GAME_ID)
key_bsp <- paste0(BaseStats_Player$espn_player_id, "_", BaseStats_Player$espn_game_id)

idx <- match(key_mc, key_bsp)


# --- Append requested fields ----------------------------------------------------
BaseStats_Player_MC$NBA_PLAYER_ID <- BaseStats_Player$nba_player_id[idx]
BaseStats_Player_MC$HEADSHOT <- BaseStats_Player$headshot[idx]
BaseStats_Player_MC$OPP_LOGO      <- BaseStats_Player$opp_logo[idx]
BaseStats_Player_MC$TEAM_LOGO     <- BaseStats_Player$team_logo[idx]
BaseStats_Player_MC$OPP           <- BaseStats_Player$opp[idx]
BaseStats_Player_MC$STARTER_STATUS <- BaseStats_Player$starter_status[idx]
BaseStats_Player_MC$POS <- BaseStats_Player$position[idx]
BaseStats_Player_MC$EJECTED <- BaseStats_Player$ejected[idx]
BaseStats_Player_MC$REASON <- BaseStats_Player$reason[idx]


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Add Additional Columns from BaseStats_Player to BaseStats_Player_MC Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: IS_B2B and TOT_RST_DYS ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ensure date typed for diff
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(GAME_DATE_DT = as.Date(GAME_DATE)) %>%
  group_by(ESPN_TEAM_ID) %>%
  arrange(GAME_DATE_DT, ESPN_GAME_ID, .by_group = TRUE) %>%
  mutate(
    TOT_RST_DYS = as.integer(GAME_DATE_DT - dplyr::lag(GAME_DATE_DT)),
    IS_B2B      = if_else(!is.na(TOT_RST_DYS) & TOT_RST_DYS == 1L, 1L, 0L)
  ) %>%
  ungroup() %>%
  mutate(
    TOT_RST_DYS = dplyr::coalesce(TOT_RST_DYS, 0L),
    IS_B2B      = as.integer(IS_B2B)
  ) %>%
  select(-GAME_DATE_DT)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: IS_B2B and TOT_RST_DYS ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: OFFTEAM and DEFTEAM SUCCESS Play Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- Row-level flags on pm_df: OFFTEAM_SUCCESS / DEFTEAM_SUCCESS (1 per play) ---


# 1) Load success criteria file
crit_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/success_criteria_nba.csv"
stopifnot(file.exists(crit_path))
succ_crit_df <- fread(crit_path, colClasses = "character") %>% as_tibble()

# 2) Extract lookup values
off_vec <- succ_crit_df$offteam_success %>% na.omit() %>% trimws()
def_vec <- succ_crit_df$defteam_success %>% na.omit() %>% trimws()

# 3) Validate that pm_df has type_text column
stopifnot("type_text" %in% names(pm_df))

# 4) For each play in pm_df, flag 1 if type_text matches an entry from criteria
pm_df <- pm_df %>%
  mutate(
    OFFTEAM_SUCCESS = if_else(type_text %in% off_vec, 1L, 0L),
    DEFTEAM_SUCCESS = if_else(type_text %in% def_vec, 1L, 0L)
  )

rm(succ_crit_df, tie_by_qtr, tie_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: OFFTEAM and DEFTEAM SUCCESS Play Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: ============================================================
# === PROTOTYPE: Player Minutes by Quarter from lineup cols ===
#     Uses home_P1_espn_id..home_P5_espn_id and away_P1..P5
#     Credits elapsed seconds between events to those 10 players
# ============================================================
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
library(dplyr)
library(tidyr)

stopifnot(exists("pm_df"), exists("BaseStats_Player_MC"))

# ---- 0) detect the "seconds remaining" column (handles your typo) ----
sec_col <- dplyr::case_when(
  "end_quarter_seconds_remaining" %in% names(pm_df) ~ "end_quarter_seconds_remaining",
  "end_quarter_seconds_reaming"   %in% names(pm_df) ~ "end_quarter_seconds_reaming",
  TRUE ~ NA_character_
)

if (is.na(sec_col)) {
  stop("Could not find end_quarter_seconds_remaining (or end_quarter_seconds_reaming) in pm_df.")
}

# ---- 1) define player-id lineup columns we will scan ----
home_cols <- paste0("home_P", 1:5, "_espn_id")
away_cols <- paste0("away_P", 1:5, "_espn_id")
player_cols <- c(home_cols, away_cols)

missing_cols <- setdiff(player_cols, names(pm_df))
if (length(missing_cols) > 0) {
  stop("Missing lineup cols in pm_df: ", paste(missing_cols, collapse = ", "))
}

# ---- 2) prep: coerce needed fields & order within (game_id, qtr) ----
pbp_for_mins <- pm_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(coalesce(as.numeric(qtr), as.numeric(period)))),
    SEC_REM = suppressWarnings(as.numeric(.data[[sec_col]])),
    seq = suppressWarnings(as.integer(sequence_number))
  ) %>%
  filter(!is.na(game_id), !is.na(qtr), qtr %in% 1:6, !is.na(SEC_REM)) %>%
  group_by(game_id, qtr) %>%
  arrange(desc(SEC_REM), seq, .by_group = TRUE) %>%
  # elapsed seconds to next row (SEC_REM decreases as time passes)
  mutate(
    SEC_NEXT = dplyr::lead(SEC_REM),
    # if next is NA => last row of qtr, run clock to 0
    SEG_SECONDS = dplyr::if_else(!is.na(SEC_NEXT), SEC_REM - SEC_NEXT, SEC_REM),
    SEG_SECONDS = dplyr::if_else(is.na(SEG_SECONDS) | SEG_SECONDS < 0, 0, SEG_SECONDS)
  ) %>%
  ungroup()

# ---- 3) explode 10 players-on-court into long and sum minutes ----
mins_long <- pbp_for_mins %>%
  select(game_id, qtr, SEG_SECONDS, all_of(player_cols)) %>%
  pivot_longer(
    cols = all_of(player_cols),
    names_to = "slot",
    values_to = "ESPN_PLAYER_ID"
  ) %>%
  mutate(
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  filter(!is.na(ESPN_PLAYER_ID), ESPN_PLAYER_ID != "") %>%
  group_by(game_id, qtr, ESPN_PLAYER_ID) %>%
  summarise(
    SECONDS_ON = sum(SEG_SECONDS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(MINS_ON = SECONDS_ON / 60)

# ---- 4) pivot to Q1..Q6 + CGS ----
mins_wide <- mins_long %>%
  mutate(qtr = as.integer(qtr)) %>%
  select(game_id, ESPN_PLAYER_ID, qtr, MINS_ON) %>%
  pivot_wider(
    id_cols = c(game_id, ESPN_PLAYER_ID),
    names_from = qtr,
    values_from = MINS_ON,
    names_prefix = "MINS_Q",
    values_fill = 0
  ) %>%
  mutate(
    MINS_CGS = rowSums(across(starts_with("MINS_Q")), na.rm = TRUE)
  )

# ---- 5) join back into BaseStats_Player_MC ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  left_join(
    mins_wide,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_PLAYER_ID" = "ESPN_PLAYER_ID")
  ) %>%
  mutate(
    across(matches("^MINS_Q[1-6]$|^MINS_CGS$"), ~ coalesce(., 0))
  )

rm(pbp_for_mins, mins_long, mins_wide, sec_col, missing_cols, home_cols, away_cols, player_cols)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Player Minutes by Quarter from lineup cols ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# === START: TOP-LEVEL Scoring Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#      .oooooo..o                               o8o                         
#     d8P'    `Y8                               `"'                         
#     Y88bo.       .ooooo.   .ooooo.  oooo d8b oooo  ooo. .oo.    .oooooooo 
#      `"Y8888o.  d88' `"Y8 d88' `88b `888""8P `888  `888P"Y88b  888' `88b  
#          `"Y88b 888       888   888  888      888   888   888  888   888  
#     oo     .d8P 888   .o8 888   888  888      888   888   888  `88bod8P'  
#     8""88888P'  `Y8bod8P' `Y8bod8P' d888b    o888o o888o o888o `8oooooo.  
#                                                                d"     YD  
#                                                                "Y88888P'  
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀       



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Scoring Overall Points Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Safety check for pm_df columns we need
stopifnot(all(c("game_id", "qtr", "athlete_id_1", "score_value") %in% names(pm_df)))

# ----------------------------------------
# A) Per-player points by quarter (Q1–Q6)
# ----------------------------------------

player_pts_by_qtr <- pm_df %>%
  dplyr::mutate(
    qtr         = suppressWarnings(as.integer(qtr)),
    score_value = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(
    !is.na(game_id),
    !is.na(athlete_id_1),
    !is.na(qtr),
    qtr %in% 1:6,
    !is.na(score_value),
    score_value != 0L
  ) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(PTS_Q = sum(score_value), .groups = "drop") %>%
  tidyr::pivot_wider(
    id_cols     = c(game_id, athlete_id_1),
    names_from  = qtr,
    values_from = PTS_Q,
    names_prefix = "PTS_Q",
    values_fill  = 0L
  )

# ----------------------------------------
# B) Full-game total per player
# ----------------------------------------

player_pts_cgs <- player_pts_by_qtr %>%
  dplyr::mutate(
    PTS_CGS = rowSums(dplyr::across(dplyr::starts_with("PTS_Q")))
  ) %>%
  dplyr::select(game_id, athlete_id_1, PTS_CGS)

# ----------------------------------------
# C) Join into BaseStats_Player_MC and apply OT logic
#    (assumes BaseStats_Player_MC has ESPN_GAME_ID, ESPN_PLAYER_ID, OT_1, OT_2)
# ----------------------------------------

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    player_pts_by_qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_pts_cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    PTS_Q1 = dplyr::coalesce(PTS_Q1, 0L),
    PTS_Q2 = dplyr::coalesce(PTS_Q2, 0L),
    PTS_Q3 = dplyr::coalesce(PTS_Q3, 0L),
    PTS_Q4 = dplyr::coalesce(PTS_Q4, 0L),
    PTS_Q5 = dplyr::coalesce(PTS_Q5, 0L),
    PTS_Q6 = dplyr::coalesce(PTS_Q6, 0L)
  )


# ============================================
# Bring team T_PTS_Q1–Q6 onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

# Which T_PTS_* columns actually exist on the team table?
team_pts_cols <- intersect(
  names(BaseStats_Team_MC),
  c("T_PTS_Q1", "T_PTS_Q2", "T_PTS_Q3", "T_PTS_Q4", "T_PTS_Q5", "T_PTS_Q6")
)

# Minimal team-level frame for join
team_pts_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_pts_cols)
  ) %>%
  dplyr::distinct()

# Join onto player MC: each player now carries team T_PTS_Q* for that game
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_pts_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )

# ======================================
# PLAYER BONUS POINTS (per quarter + CGS)
# ======================================

# BONUS PREP: build pb_bonus + foul_threshold_qtr
stopifnot(all(c("game_id","qtr","team_id","home_team_id","away_team_id",
                "clock_minutes","clock_seconds","type_text","score_value",
                "athlete_id_1") %in% names(pm_df)))

pb_bonus <- pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds)),
    foul_flag = type_text %in% c("Personal Foul","Shooting Foul","Loose Ball Foul"),
    opp_id = dplyr::case_when(
      team_id == home_team_id ~ away_team_id,
      team_id == away_team_id ~ home_team_id,
      TRUE ~ NA_character_
    ),
    score_value_int = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::group_by(game_id, qtr) %>%
  dplyr::arrange(dplyr::desc(t_sec), .by_group = TRUE) %>%
  dplyr::mutate(evt = dplyr::row_number()) %>%
  dplyr::ungroup()

foul_threshold_qtr <- pb_bonus %>%
  dplyr::filter(qtr %in% 1:6, foul_flag) %>%
  dplyr::group_by(game_id, qtr, team_id) %>%
  dplyr::mutate(cum_fouls = cumsum(foul_flag)) %>%
  dplyr::filter(cum_fouls >= 5L) %>%
  dplyr::summarise(threshold_evt = min(evt), .groups = "drop")

# --- 1) Identify *bonus free throws* at the play level ---

bonus_shots <- pb_bonus %>%
  dplyr::filter(qtr %in% 1:6) %>%  # Q1–Q4 + OT1/OT2
  dplyr::left_join(
    foul_threshold_qtr %>% dplyr::rename(opp_id = team_id),
    by = c("game_id", "qtr", "opp_id")
  ) %>%
  dplyr::filter(
    !is.na(threshold_evt),        # bonus has been reached
    evt > threshold_evt,          # after the 5th foul
    score_value_int == 1L,        # one-point free throws
    !is.na(athlete_id_1)          # must have a shooter
  )

# --- 2) Per-player bonus points by quarter (Q1–Q6) ---

player_bonus_qtr <- bonus_shots %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    BON_PTS = dplyr::n(),         # each row is 1 bonus point
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    id_cols     = c(game_id, athlete_id_1),
    names_from  = qtr,
    values_from = BON_PTS,
    values_fill = 0L,
    names_prefix = "BON_PTS_Q"
  )

# --- 3) Full-game bonus points per player (CGS) ---

player_bonus_cgs <- player_bonus_qtr %>%
  dplyr::mutate(
    BON_PTS_CGS = rowSums(dplyr::across(dplyr::starts_with("BON_PTS_Q")), na.rm = TRUE)
  ) %>%
  dplyr::select(game_id, athlete_id_1, BON_PTS_CGS)

# --- 4) Join into BaseStats_Player_MC and apply OT gating ---

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    player_bonus_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_bonus_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    BON_PTS_Q1 = dplyr::coalesce(BON_PTS_Q1, 0L),
    BON_PTS_Q2 = dplyr::coalesce(BON_PTS_Q2, 0L),
    BON_PTS_Q3 = dplyr::coalesce(BON_PTS_Q3, 0L),
    BON_PTS_Q4 = dplyr::coalesce(BON_PTS_Q4, 0L),
    
    BON_PTS_Q5 = dplyr::if_else(
      OT_1 == 1L,
      dplyr::coalesce(BON_PTS_Q5, 0L),
      0L
    ),
    BON_PTS_Q6 = dplyr::if_else(
      OT_2 == 1L,
      dplyr::coalesce(BON_PTS_Q6, 0L),
      0L
    ),
    
    BON_PTS_CGS = dplyr::coalesce(BON_PTS_CGS, 0L),
    
    # Single total bonus-points field
    BON_PTS = BON_PTS_CGS
  )


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Scoring Overall Points Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Raw Field Goal Data Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ============================================
# Bring team T_FGA_* and T_FGM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_fgx_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    "T_FGA_Q1","T_FGA_Q2","T_FGA_Q3","T_FGA_Q4","T_FGA_Q5","T_FGA_Q6","T_FGA_CGS",
    "T_FGM_Q1","T_FGM_Q2","T_FGM_Q3","T_FGM_Q4","T_FGM_Q5","T_FGM_Q6","T_FGM_CGS"
  )
)

team_fgx_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_fgx_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_fgx_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )


# ============================================
# PLAYER FIELD GOALS: FGA, FGM, FG% (Q1–Q6 + CGS)
# ============================================

# safety: required columns
stopifnot(all(c("game_id","qtr","athlete_id_1","shooting_play",
                "scoring_play","type_text") %in% names(pm_df)))

# reuse the same logical-normalizer as team section
to_bool <- function(x) {
  xv <- tolower(as.character(x))
  xv %in% c("true","t","1","yes","y")
}

# -------------------------------
# A) Field Goal Attempts (FGA) per player
# -------------------------------
fga_base_player <- pm_df %>%
  dplyr::mutate(
    qtr     = suppressWarnings(as.integer(qtr)),
    is_shot = to_bool(shooting_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  ) %>%
  dplyr::filter(!is.na(athlete_id_1))

# quarter counts (Q1–Q6; ensure Q5/Q6 exist)
player_fga_qtr <- fga_base_player %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(FGA = sum(is_shot, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(qtr = 1:6, fill = list(FGA = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = FGA,
    values_fill = 0L,
    names_prefix = "FGA_Q"
  )

# complete-game (include OT)
player_fga_cgs <- fga_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(FGA_CGS = sum(is_shot, na.rm = TRUE), .groups = "drop")

# -------------------------------
# B) Field Goals Made (FGM) per player
# -------------------------------
fgm_base_player <- pm_df %>%
  dplyr::mutate(
    qtr     = suppressWarnings(as.integer(qtr)),
    is_make = to_bool(scoring_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  ) %>%
  dplyr::filter(!is.na(athlete_id_1))

player_fgm_qtr <- fgm_base_player %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(FGM = sum(is_make, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(qtr = 1:6, fill = list(FGM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = FGM,
    values_fill = 0L,
    names_prefix = "FGM_Q"
  )

player_fgm_cgs <- fgm_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(FGM_CGS = sum(is_make, na.rm = TRUE), .groups = "drop")

# -------------------------------
# C) Join into BaseStats_Player_MC and compute FG%
# -------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    player_fga_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_fga_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_fgm_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_fgm_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    FGA_Q1 = dplyr::coalesce(FGA_Q1, 0L),
    FGA_Q2 = dplyr::coalesce(FGA_Q2, 0L),
    FGA_Q3 = dplyr::coalesce(FGA_Q3, 0L),
    FGA_Q4 = dplyr::coalesce(FGA_Q4, 0L),
    FGM_Q1 = dplyr::coalesce(FGM_Q1, 0L),
    FGM_Q2 = dplyr::coalesce(FGM_Q2, 0L),
    FGM_Q3 = dplyr::coalesce(FGM_Q3, 0L),
    FGM_Q4 = dplyr::coalesce(FGM_Q4, 0L),
    
    # OT stats only if OT happened
    FGA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGA_Q5, 0L), 0L),
    FGA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGA_Q6, 0L), 0L),
    FGM_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGM_Q5, 0L), 0L),
    FGM_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGM_Q6, 0L), 0L),
    
    FGA_CGS = dplyr::coalesce(FGA_CGS, 0L),
    FGM_CGS = dplyr::coalesce(FGM_CGS, 0L)
  ) %>%
  dplyr::mutate(
    FG_PCT_Q1 = ifelse(FGA_Q1 > 0, FGM_Q1 / FGA_Q1, NA_real_),
    FG_PCT_Q2 = ifelse(FGA_Q2 > 0, FGM_Q2 / FGA_Q2, NA_real_),
    FG_PCT_Q3 = ifelse(FGA_Q3 > 0, FGM_Q3 / FGA_Q3, NA_real_),
    FG_PCT_Q4 = ifelse(FGA_Q4 > 0, FGM_Q4 / FGA_Q4, NA_real_),
    
    FG_PCT_Q5 = dplyr::if_else(
      OT_1 == 1L & FGA_Q5 > 0,
      FGM_Q5 / FGA_Q5,
      NA_real_
    ),
    FG_PCT_Q6 = dplyr::if_else(
      OT_2 == 1L & FGA_Q6 > 0,
      FGM_Q6 / FGA_Q6,
      NA_real_
    ),
    
    FG_PCT_CGS = ifelse(FGA_CGS > 0, FGM_CGS / FGA_CGS, NA_real_)
  )


# ============================================
# Player FG% relative to TEAM FGA/FGM (Q1–Q6 + CGS)
# Produces FG_PCT_T2P_Q1..Q6 and FG_PCT_T2P_CGS
# ============================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_T2P_Q1 = ifelse(T_FGA_Q1 > 0, FGM_Q1 / T_FGA_Q1, NA_real_),
    FG_PCT_T2P_Q2 = ifelse(T_FGA_Q2 > 0, FGM_Q2 / T_FGA_Q2, NA_real_),
    FG_PCT_T2P_Q3 = ifelse(T_FGA_Q3 > 0, FGM_Q3 / T_FGA_Q3, NA_real_),
    FG_PCT_T2P_Q4 = ifelse(T_FGA_Q4 > 0, FGM_Q4 / T_FGA_Q4, NA_real_),
    
    # OT periods only if game actually had OT
    FG_PCT_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_FGA_Q5 > 0,
      FGM_Q5 / T_FGA_Q5,
      NA_real_
    ),
    FG_PCT_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_FGA_Q6 > 0,
      FGM_Q6 / T_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_T2P_CGS = ifelse(T_FGA_CGS > 0, FGM_CGS / T_FGA_CGS, NA_real_)
  )

# (optional) cleanup
rm(fga_base_player, fgm_base_player,
   player_fga_qtr, player_fga_cgs,
   player_fgm_qtr, player_fgm_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Raw Field Goal Data Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Free Throw Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================
# Bring team T_FTA_* and T_FTM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_ftx_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    "T_FTA_Q1","T_FTA_Q2","T_FTA_Q3","T_FTA_Q4","T_FTA_Q5","T_FTA_Q6","T_FTA_CGS",
    "T_FTM_Q1","T_FTM_Q2","T_FTM_Q3","T_FTM_Q4","T_FTM_Q5","T_FTM_Q6","T_FTM_CGS"
  )
)

team_ftx_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_ftx_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_ftx_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )

# ============================================
# PLAYER FREE THROWS: FTA, FTM, FT%, FTR
# FTA_Q1..Q6, FTA_CGS
# FTM_Q1..Q6, FTM_CGS
# FT_PCT_Q1..Q6, FT_PCT_CGS
# FTR_Q1..Q6, FTR_CGS
# ============================================

stopifnot(all(c("game_id","qtr","athlete_id_1","team_id",
                "shooting_play","scoring_play") %in% names(pm_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

has_type_id <- "type_id" %in% names(pm_df) || "play_type_id" %in% names(pm_df)
type_id_col <- if ("type_id" %in% names(pm_df)) {
  "type_id"
} else if ("play_type_id" %in% names(pm_df)) {
  "play_type_id"
} else {
  NA_character_
}

ft_attempt_base_player <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    any_text = paste0(
      if ("type_text" %in% names(.)) type_text else "",
      " ",
      if ("text" %in% names(.)) text else "",
      " ",
      if ("desc" %in% names(.)) desc else ""
    ),
    ft_flag = if (!is.na(type_id_col)) {
      get(type_id_col) %in% 97:102
    } else {
      stringr::str_detect(any_text, stringr::regex("\\bFree Throw\\b", ignore_case = TRUE))
    },
    is_fta = ft_flag & to_bool(shooting_play),
    is_ftm = ft_flag & to_bool(scoring_play)
  ) %>%
  dplyr::filter(!is.na(athlete_id_1))

# ---------- FTA: quarter (Q1–Q6) + complete game ----------
player_fta_qtr <- ft_attempt_base_player %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(FTA = sum(is_fta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(qtr = 1:6, fill = list(FTA = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = FTA,
    values_fill = 0L,
    names_prefix = "FTA_Q"
  )

player_fta_cgs <- ft_attempt_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(FTA_CGS = sum(is_fta, na.rm = TRUE), .groups = "drop")

# ---------- FTM: quarter (Q1–Q6) + complete game ----------
player_ftm_qtr <- ft_attempt_base_player %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(FTM = sum(is_ftm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(qtr = 1:6, fill = list(FTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = FTM,
    values_fill = 0L,
    names_prefix = "FTM_Q"
  )

player_ftm_cgs <- ft_attempt_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(FTM_CGS = sum(is_ftm, na.rm = TRUE), .groups = "drop")

# ---------- Join & compute FT% and FTR (uses player FGA_* already added) ----------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    player_fta_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_fta_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_ftm_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_ftm_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    FTA_Q1 = dplyr::coalesce(FTA_Q1, 0L),
    FTA_Q2 = dplyr::coalesce(FTA_Q2, 0L),
    FTA_Q3 = dplyr::coalesce(FTA_Q3, 0L),
    FTA_Q4 = dplyr::coalesce(FTA_Q4, 0L),
    FTM_Q1 = dplyr::coalesce(FTM_Q1, 0L),
    FTM_Q2 = dplyr::coalesce(FTM_Q2, 0L),
    FTM_Q3 = dplyr::coalesce(FTM_Q3, 0L),
    FTM_Q4 = dplyr::coalesce(FTM_Q4, 0L),
    
    # OT (only if occurred)
    FTA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FTA_Q5, 0L), 0L),
    FTA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FTA_Q6, 0L), 0L),
    FTM_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FTM_Q5, 0L), 0L),
    FTM_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FTM_Q6, 0L), 0L),
    
    FTA_CGS = dplyr::coalesce(FTA_CGS, 0L),
    FTM_CGS = dplyr::coalesce(FTM_CGS, 0L)
  ) %>%
  dplyr::mutate(
    # FT%
    FT_PCT_Q1 = ifelse(FTA_Q1 > 0, FTM_Q1 / FTA_Q1, NA_real_),
    FT_PCT_Q2 = ifelse(FTA_Q2 > 0, FTM_Q2 / FTA_Q2, NA_real_),
    FT_PCT_Q3 = ifelse(FTA_Q3 > 0, FTM_Q3 / FTA_Q3, NA_real_),
    FT_PCT_Q4 = ifelse(FTA_Q4 > 0, FTM_Q4 / FTA_Q4, NA_real_),
    FT_PCT_Q5 = dplyr::if_else(OT_1 == 1L & FTA_Q5 > 0, FTM_Q5 / FTA_Q5, NA_real_),
    FT_PCT_Q6 = dplyr::if_else(OT_2 == 1L & FTA_Q6 > 0, FTM_Q6 / FTA_Q6, NA_real_),
    FT_PCT_CGS = ifelse(FTA_CGS > 0, FTM_CGS / FTA_CGS, NA_real_),
    
    # FTR = FTA / FGA  (uses player FGA_* from FG section)
    FTR_Q1 = ifelse(FGA_Q1 > 0, FTA_Q1 / FGA_Q1, NA_real_),
    FTR_Q2 = ifelse(FGA_Q2 > 0, FTA_Q2 / FGA_Q2, NA_real_),
    FTR_Q3 = ifelse(FGA_Q3 > 0, FTA_Q3 / FGA_Q3, NA_real_),
    FTR_Q4 = ifelse(FGA_Q4 > 0, FTA_Q4 / FGA_Q4, NA_real_),
    FTR_Q5 = dplyr::if_else(OT_1 == 1L & FGA_Q5 > 0, FTA_Q5 / FGA_Q5, NA_real_),
    FTR_Q6 = dplyr::if_else(OT_2 == 1L & FGA_Q6 > 0, FTA_Q6 / FGA_Q6, NA_real_),
    FTR_CGS = ifelse(FGA_CGS > 0, FTA_CGS / FGA_CGS, NA_real_)
  )

# ============================================
# Player FT% relative to TEAM FTA (Q1–Q6 + CGS)
# Produces FT_PCT_T2P_Q1..Q6 and FT_PCT_T2P_CGS
# ============================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FT_PCT_T2P_Q1 = ifelse(T_FTA_Q1 > 0, FTM_Q1 / T_FTA_Q1, NA_real_),
    FT_PCT_T2P_Q2 = ifelse(T_FTA_Q2 > 0, FTM_Q2 / T_FTA_Q2, NA_real_),
    FT_PCT_T2P_Q3 = ifelse(T_FTA_Q3 > 0, FTM_Q3 / T_FTA_Q3, NA_real_),
    FT_PCT_T2P_Q4 = ifelse(T_FTA_Q4 > 0, FTM_Q4 / T_FTA_Q4, NA_real_),
    
    # OT periods only if game actually had OT
    FT_PCT_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_FTA_Q5 > 0,
      FTM_Q5 / T_FTA_Q5,
      NA_real_
    ),
    FT_PCT_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_FTA_Q6 > 0,
      FTM_Q6 / T_FTA_Q6,
      NA_real_
    ),
    
    # Complete game
    FT_PCT_T2P_CGS = ifelse(T_FTA_CGS > 0, FTM_CGS / T_FTA_CGS, NA_real_)
  )

# optional cleanup
rm(ft_attempt_base_player,
   player_fta_qtr, player_fta_cgs,
   player_ftm_qtr, player_ftm_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Free Throw Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ============================================
# Bring team T_2PTA_* and T_2PTM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_2ptx_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    "T_2PTA_Q1","T_2PTA_Q2","T_2PTA_Q3","T_2PTA_Q4","T_2PTA_Q5","T_2PTA_Q6","T_2PTA_CGS",
    "T_2PTM_Q1","T_2PTM_Q2","T_2PTM_Q3","T_2PTM_Q4","T_2PTM_Q5","T_2PTM_Q6","T_2PTM_CGS"
  )
)

team_2ptx_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_2ptx_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_2ptx_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )


# ============================================
# PLAYER 2PT ATTEMPTS / MAKES / % / eFG / PTS / PPP / RATE / PTS SHARE
# 2PTA_Q1..Q6, 2PTA_CGS
# 2PTM_Q1..Q6, 2PTM_CGS
# 2PT_PCT_*, 2PT_EFG_PCT_*, 2PT_PTS_*, 2PT_PPP_*, 2PT_RATE_*, 2PT_PTSHR_*
# ============================================

stopifnot(all(c("game_id","qtr","team_id","athlete_id_1",
                "type_text","scoring_play","score_value") %in% names(pm_df)))

zone_col <- dplyr::case_when(
  "shot_zone_basic" %in% names(pm_df) ~ "shot_zone_basic",
  "SHOT_ZONE_BASIC" %in% names(pm_df) ~ "SHOT_ZONE_BASIC",
  TRUE ~ NA_character_
)
stopifnot(!is.na(zone_col))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
zf <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# --- flag *player* 2PT attempts and makes ---
twopt_base_player <- pm_df %>%
  dplyr::mutate(
    qtr     = suppressWarnings(as.integer(qtr)),
    is_2pta = to_bool(shooting_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE)) &
      !(.data[[zone_col]] %in% c("Above the Break 3","Right Corner 3","Left Corner 3")),
    is_2ptm = is_2pta &
      to_bool(scoring_play) &
      suppressWarnings(as.integer(score_value)) == 2L
  ) %>%
  dplyr::filter(!is.na(athlete_id_1))

# --- per-quarter player counts (Q1–Q6) ---
twopt_player_qtr <- twopt_base_player %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    `2PTA` = sum(is_2pta, na.rm = TRUE),
    `2PTM` = sum(is_2ptm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(`2PTA` = 0L, `2PTM` = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols     = c(game_id, athlete_id_1),
    names_from  = qtr,
    values_from = c(`2PTA`, `2PTM`),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# --- complete-game (CGS) player counts ---
twopt_player_cgs <- twopt_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(
    `2PTA_CGS` = sum(is_2pta, na.rm = TRUE),
    `2PTM_CGS` = sum(is_2ptm, na.rm = TRUE),
    .groups = "drop"
  )

# --- join into BaseStats_Player_MC & mask OT; make sure FGA / PTS_SCORED are ready ---
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    twopt_player_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    twopt_player_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    # coalesce 2PT attempts/makes
    `2PTA_Q1` = dplyr::coalesce(`2PTA_Q1`, 0L),
    `2PTA_Q2` = dplyr::coalesce(`2PTA_Q2`, 0L),
    `2PTA_Q3` = dplyr::coalesce(`2PTA_Q3`, 0L),
    `2PTA_Q4` = dplyr::coalesce(`2PTA_Q4`, 0L),
    `2PTM_Q1` = dplyr::coalesce(`2PTM_Q1`, 0L),
    `2PTM_Q2` = dplyr::coalesce(`2PTM_Q2`, 0L),
    `2PTM_Q3` = dplyr::coalesce(`2PTM_Q3`, 0L),
    `2PTM_Q4` = dplyr::coalesce(`2PTM_Q4`, 0L),
    
    `2PTA_Q5` = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(`2PTA_Q5`, 0L), 0L),
    `2PTA_Q6` = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(`2PTA_Q6`, 0L), 0L),
    `2PTM_Q5` = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(`2PTM_Q5`, 0L), 0L),
    `2PTM_Q6` = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(`2PTM_Q6`, 0L), 0L),
    
    `2PTA_CGS` = dplyr::coalesce(`2PTA_CGS`, 0L),
    `2PTM_CGS` = dplyr::coalesce(`2PTM_CGS`, 0L),
    
    # ensure player-level FGA and PTS_SCORED exist for RATE / PTS share
    FGA_Q1 = dplyr::coalesce(FGA_Q1, 0),
    FGA_Q2 = dplyr::coalesce(FGA_Q2, 0),
    FGA_Q3 = dplyr::coalesce(FGA_Q3, 0),
    FGA_Q4 = dplyr::coalesce(FGA_Q4, 0),
    FGA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGA_Q5, 0), 0),
    FGA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGA_Q6, 0), 0),
    FGA_CGS = dplyr::coalesce(FGA_CGS, 0),
    
    PTS_Q1 = dplyr::coalesce(PTS_Q1, 0),
    PTS_Q2 = dplyr::coalesce(PTS_Q2, 0),
    PTS_Q3 = dplyr::coalesce(PTS_Q3, 0),
    PTS_Q4 = dplyr::coalesce(PTS_Q4, 0),
    PTS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(PTS_Q5, 0), 0),
    PTS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(PTS_Q6, 0), 0),
    PTS_CGS = dplyr::coalesce(PTS_CGS, 0)
  ) %>%
  dplyr::mutate(
    # 2PT%
    `2PT_PCT_Q1` = zf(`2PTM_Q1`, `2PTA_Q1`),
    `2PT_PCT_Q2` = zf(`2PTM_Q2`, `2PTA_Q2`),
    `2PT_PCT_Q3` = zf(`2PTM_Q3`, `2PTA_Q3`),
    `2PT_PCT_Q4` = zf(`2PTM_Q4`, `2PTA_Q4`),
    `2PT_PCT_Q5` = dplyr::if_else(OT_1 == 1L, zf(`2PTM_Q5`, `2PTA_Q5`), NA_real_),
    `2PT_PCT_Q6` = dplyr::if_else(OT_2 == 1L, zf(`2PTM_Q6`, `2PTA_Q6`), NA_real_),
    `2PT_PCT_CGS` = zf(`2PTM_CGS`, `2PTA_CGS`),
    
    # eFG for 2s = FG%
    `2PT_EFG_PCT_Q1` = `2PT_PCT_Q1`,
    `2PT_EFG_PCT_Q2` = `2PT_PCT_Q2`,
    `2PT_EFG_PCT_Q3` = `2PT_PCT_Q3`,
    `2PT_EFG_PCT_Q4` = `2PT_PCT_Q4`,
    `2PT_EFG_PCT_Q5` = `2PT_PCT_Q5`,
    `2PT_EFG_PCT_Q6` = `2PT_PCT_Q6`,
    `2PT_EFG_PCT_CGS` = `2PT_PCT_CGS`,
    
    # Points from 2s
    `2PT_PTS_Q1` = 2 * `2PTM_Q1`,
    `2PT_PTS_Q2` = 2 * `2PTM_Q2`,
    `2PT_PTS_Q3` = 2 * `2PTM_Q3`,
    `2PT_PTS_Q4` = 2 * `2PTM_Q4`,
    `2PT_PTS_Q5` = 2 * `2PTM_Q5`,
    `2PT_PTS_Q6` = 2 * `2PTM_Q6`,
    `2PT_PTS_CGS` = 2 * `2PTM_CGS`,
    
    # "Possessions" proxy = attempts
    `2PT_POSS_Q1` = `2PTA_Q1`,
    `2PT_POSS_Q2` = `2PTA_Q2`,
    `2PT_POSS_Q3` = `2PTA_Q3`,
    `2PT_POSS_Q4` = `2PTA_Q4`,
    `2PT_POSS_Q5` = `2PTA_Q5`,
    `2PT_POSS_Q6` = `2PTA_Q6`,
    `2PT_POSS_CGS` = `2PTA_CGS`,
    
    # PPP
    `2PT_PPP_Q1` = zf(`2PT_PTS_Q1`, `2PT_POSS_Q1`),
    `2PT_PPP_Q2` = zf(`2PT_PTS_Q2`, `2PT_POSS_Q2`),
    `2PT_PPP_Q3` = zf(`2PT_PTS_Q3`, `2PT_POSS_Q3`),
    `2PT_PPP_Q4` = zf(`2PT_PTS_Q4`, `2PT_POSS_Q4`),
    `2PT_PPP_Q5` = dplyr::if_else(OT_1 == 1L, zf(`2PT_PTS_Q5`, `2PT_POSS_Q5`), NA_real_),
    `2PT_PPP_Q6` = dplyr::if_else(OT_2 == 1L, zf(`2PT_PTS_Q6`, `2PT_POSS_Q6`), NA_real_),
    `2PT_PPP_CGS` = zf(`2PT_PTS_CGS`, `2PT_POSS_CGS`),
    
    # RATE = 2PTA / FGA
    `2PT_RATE_Q1` = zf(`2PTA_Q1`, FGA_Q1),
    `2PT_RATE_Q2` = zf(`2PTA_Q2`, FGA_Q2),
    `2PT_RATE_Q3` = zf(`2PTA_Q3`, FGA_Q3),
    `2PT_RATE_Q4` = zf(`2PTA_Q4`, FGA_Q4),
    `2PT_RATE_Q5` = dplyr::if_else(OT_1 == 1L, zf(`2PTA_Q5`, FGA_Q5), NA_real_),
    `2PT_RATE_Q6` = dplyr::if_else(OT_2 == 1L, zf(`2PTA_Q6`, FGA_Q6), NA_real_),
    `2PT_RATE_CGS` = zf(`2PTA_CGS`, FGA_CGS),
    
    # PTS share = (2*2PTM) / player's total points
    `2PT_PTSHR_Q1` = zf(`2PT_PTS_Q1`, PTS_Q1),
    `2PT_PTSHR_Q2` = zf(`2PT_PTS_Q2`, PTS_Q2),
    `2PT_PTSHR_Q3` = zf(`2PT_PTS_Q3`, PTS_Q3),
    `2PT_PTSHR_Q4` = zf(`2PT_PTS_Q4`, PTS_Q4),
    `2PT_PTSHR_Q5` = dplyr::if_else(OT_1 == 1L, zf(`2PT_PTS_Q5`, PTS_Q5), NA_real_),
    `2PT_PTSHR_Q6` = dplyr::if_else(OT_2 == 1L, zf(`2PT_PTS_Q6`, PTS_Q6), NA_real_),
    `2PT_PTSHR_CGS` = zf(`2PT_PTS_CGS`, PTS_CGS)
  )


# ============================================
# Player 2PT% relative to TEAM 2PTA (Q1–Q6 + CGS)
# Produces P_2PT_PCT_T2P_Q1..Q6 and P_2PT_PCT_T2P_CGS
# ============================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    P_2PT_PCT_T2P_Q1 = ifelse(T_2PTA_Q1 > 0, `2PTM_Q1` / T_2PTA_Q1, NA_real_),
    P_2PT_PCT_T2P_Q2 = ifelse(T_2PTA_Q2 > 0, `2PTM_Q2` / T_2PTA_Q2, NA_real_),
    P_2PT_PCT_T2P_Q3 = ifelse(T_2PTA_Q3 > 0, `2PTM_Q3` / T_2PTA_Q3, NA_real_),
    P_2PT_PCT_T2P_Q4 = ifelse(T_2PTA_Q4 > 0, `2PTM_Q4` / T_2PTA_Q4, NA_real_),
    
    # OT periods only if the game actually had OT
    P_2PT_PCT_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_2PTA_Q5 > 0,
      `2PTM_Q5` / T_2PTA_Q5,
      NA_real_
    ),
    P_2PT_PCT_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_2PTA_Q6 > 0,
      `2PTM_Q6` / T_2PTA_Q6,
      NA_real_
    ),
    
    # Complete game
    P_2PT_PCT_T2P_CGS = ifelse(T_2PTA_CGS > 0, `2PTM_CGS` / T_2PTA_CGS, NA_real_)
  )

rm(twopt_base_player, twopt_player_qtr, twopt_player_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Pull-Up 2PT  Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================
# Bring team T_PU_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_pu_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    "T_PU_FGA_Q1","T_PU_FGA_Q2","T_PU_FGA_Q3","T_PU_FGA_Q4","T_PU_FGA_Q5","T_PU_FGA_Q6","T_PU_FGA_CGS",
    "T_PU_FGM_Q1","T_PU_FGM_Q2","T_PU_FGM_Q3","T_PU_FGM_Q4","T_PU_FGM_Q5","T_PU_FGM_Q6","T_PU_FGM_CGS"
  )
)

team_pu_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_pu_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_pu_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )


# ======================================
# PLAYER PULL-UP FGA / FGM / PTS (Q1–Q6 + CGS)
# ======================================

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "shooting_play","scoring_play","score_value",
  "athlete_id_1"
) %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build Pull-Up base from pm_df (detect only "Pullup") ----
pu_base_player <- pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play),
    pts   = suppressWarnings(as.integer(score_value)),
    is_pu = stringr::str_detect(type_text, stringr::regex("pullup", ignore_case = TRUE))
  ) %>%
  dplyr::filter(
    is_pu,
    shot,
    !is.na(game_id),
    !is.na(athlete_id_1),
    !is.na(qtr),
    qtr %in% 1:6
  ) %>%
  dplyr::transmute(
    game_id,
    team_id,
    athlete_id_1,
    qtr,
    pu_fga = TRUE,
    pu_fgm = make & !is.na(pts) & pts > 0L,
    pu_pts = dplyr::if_else(make & !is.na(pts) & pts > 0L, pts, 0L)
  )

# ---- Quarter-level tallies (Q1–Q6) per player ----
pu_player_qtr <- pu_base_player %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    PU_FGA = sum(pu_fga, na.rm = TRUE),
    PU_FGM = sum(pu_fgm, na.rm = TRUE),
    PU_PTS = sum(pu_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(qtr = 1:6, fill = list(PU_FGA = 0L, PU_FGM = 0L, PU_PTS = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols     = c(game_id, athlete_id_1),
    names_from  = qtr,
    values_from = c(PU_FGA, PU_FGM, PU_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies per player ----
pu_player_cgs <- pu_base_player %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(
    PU_FGA_CGS = sum(pu_fga, na.rm = TRUE),
    PU_FGM_CGS = sum(pu_fgm, na.rm = TRUE),
    PU_PTS_CGS = sum(pu_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Player_MC & apply OT gating ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    pu_player_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    pu_player_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    # Coalesce Q1–Q4
    PU_FGA_Q1 = dplyr::coalesce(PU_FGA_Q1, 0L),
    PU_FGA_Q2 = dplyr::coalesce(PU_FGA_Q2, 0L),
    PU_FGA_Q3 = dplyr::coalesce(PU_FGA_Q3, 0L),
    PU_FGA_Q4 = dplyr::coalesce(PU_FGA_Q4, 0L),
    
    PU_FGM_Q1 = dplyr::coalesce(PU_FGM_Q1, 0L),
    PU_FGM_Q2 = dplyr::coalesce(PU_FGM_Q2, 0L),
    PU_FGM_Q3 = dplyr::coalesce(PU_FGM_Q3, 0L),
    PU_FGM_Q4 = dplyr::coalesce(PU_FGM_Q4, 0L),
    
    PU_PTS_Q1 = dplyr::coalesce(PU_PTS_Q1, 0L),
    PU_PTS_Q2 = dplyr::coalesce(PU_PTS_Q2, 0L),
    PU_PTS_Q3 = dplyr::coalesce(PU_PTS_Q3, 0L),
    PU_PTS_Q4 = dplyr::coalesce(PU_PTS_Q4, 0L),
    
    # OT: only if those periods exist
    PU_FGA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(PU_FGA_Q5, 0L), 0L),
    PU_FGA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(PU_FGA_Q6, 0L), 0L),
    PU_FGM_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(PU_FGM_Q5, 0L), 0L),
    PU_FGM_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(PU_FGM_Q6, 0L), 0L),
    PU_PTS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(PU_PTS_Q5, 0L), 0L),
    PU_PTS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(PU_PTS_Q6, 0L), 0L),
    
    PU_FGA_CGS = dplyr::coalesce(PU_FGA_CGS, 0L),
    PU_FGM_CGS = dplyr::coalesce(PU_FGM_CGS, 0L),
    PU_PTS_CGS = dplyr::coalesce(PU_PTS_CGS, 0L)
  )

# ==========================================================
# Player Pull-Up FG% relative to TEAM Pull-Up FGA (Q1–Q6+CGS)
# Produces PU_FG_PCT_T2P_Q1..Q6 and PU_FG_PCT_T2P_CGS
# ==========================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    PU_FG_PCT_T2P_Q1 = ifelse(T_PU_FGA_Q1 > 0, PU_FGM_Q1 / T_PU_FGA_Q1, NA_real_),
    PU_FG_PCT_T2P_Q2 = ifelse(T_PU_FGA_Q2 > 0, PU_FGM_Q2 / T_PU_FGA_Q2, NA_real_),
    PU_FG_PCT_T2P_Q3 = ifelse(T_PU_FGA_Q3 > 0, PU_FGM_Q3 / T_PU_FGA_Q3, NA_real_),
    PU_FG_PCT_T2P_Q4 = ifelse(T_PU_FGA_Q4 > 0, PU_FGM_Q4 / T_PU_FGA_Q4, NA_real_),
    
    # OT only if those periods occurred
    PU_FG_PCT_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_PU_FGA_Q5 > 0,
      PU_FGM_Q5 / T_PU_FGA_Q5,
      NA_real_
    ),
    PU_FG_PCT_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_PU_FGA_Q6 > 0,
      PU_FGM_Q6 / T_PU_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    PU_FG_PCT_T2P_CGS = ifelse(T_PU_FGA_CGS > 0, PU_FGM_CGS / T_PU_FGA_CGS, NA_real_)
  )


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Pull-Up 2PT  Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀 PLAYER 3PT Field Goal Data Aggregation (Q1–Q6 + CGS)

# ============================================
# Bring team T_3PTA_* and T_3PTM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_3ptx_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    "T_3PTA_Q1","T_3PTA_Q2","T_3PTA_Q3","T_3PTA_Q4","T_3PTA_Q5","T_3PTA_Q6","T_3PTA_CGS",
    "T_3PTM_Q1","T_3PTM_Q2","T_3PTM_Q3","T_3PTM_Q4","T_3PTM_Q5","T_3PTM_Q6","T_3PTM_CGS"
  )
)

team_3ptx_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_3ptx_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_3ptx_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )

# Uses athlete_id_1 as the shooter id

stopifnot(all(c(
  "game_id","team_id","qtr","shooting_play","scoring_play","score_value",
  "home_team_id","away_team_id","SHOT_ZONE_BASIC","athlete_id_1"
) %in% names(pm_df)))

zone_col <- "SHOT_ZONE_BASIC"
to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(num, den) ifelse(den > 0 & !is.na(den), num / den, NA_real_)

three_labels <- c("Above the Break 3", "Right Corner 3", "Left Corner 3")
three_rx     <- stringr::regex(paste(three_labels, collapse = "|"), ignore_case = TRUE)

# --- Row-level flags per PLAYER (athlete_id_1) ---------------------------------------
player_three_base <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    is_3pa = to_bool(shooting_play) &
      stringr::str_detect(.data[[zone_col]], three_rx),
    is_3pm = to_bool(scoring_play) &
      suppressWarnings(as.integer(score_value)) == 3L &
      stringr::str_detect(.data[[zone_col]], three_rx)
  ) %>%
  dplyr::filter(!is.na(athlete_id_1)) %>%
  dplyr::select(game_id, team_id, qtr, athlete_id_1, is_3pa, is_3pm)

# --- Per-quarter tallies by player (Q1–Q6) -------------------------------------------
player_t3_qtr <- player_three_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    `3PTA`   = sum(is_3pa, na.rm = TRUE),
    `3PTM`   = sum(is_3pm, na.rm = TRUE),
    `3PT_PTS` = 3L * sum(is_3pm, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  tidyr::complete(
    qtr  = 1:6,
    fill = list(`3PTA` = 0L, `3PTM` = 0L, `3PT_PTS` = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols     = c(game_id, athlete_id_1),
    names_from  = qtr,
    values_from = c(`3PTA`, `3PTM`, `3PT_PTS`),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# --- Complete-game totals by player (CGS) -------------------------------------------
player_t3_cgs <- player_three_base %>%
  dplyr::group_by(game_id, athlete_id_1) %>%
  dplyr::summarise(
    `3PTA_CGS`   = sum(is_3pa, na.rm = TRUE),
    `3PTM_CGS`   = sum(is_3pm, na.rm = TRUE),
    `3PT_PTS_CGS` = 3L * sum(is_3pm, na.rm = TRUE),
    .groups      = "drop"
  )

# --- Join into BaseStats_Player_MC and apply OT gating ------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    player_t3_qtr,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::left_join(
    player_t3_cgs,
    by = c("ESPN_GAME_ID"   = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    `3PTA_Q1` = dplyr::coalesce(`3PTA_Q1`, 0L),
    `3PTA_Q2` = dplyr::coalesce(`3PTA_Q2`, 0L),
    `3PTA_Q3` = dplyr::coalesce(`3PTA_Q3`, 0L),
    `3PTA_Q4` = dplyr::coalesce(`3PTA_Q4`, 0L),
    
    `3PTM_Q1` = dplyr::coalesce(`3PTM_Q1`, 0L),
    `3PTM_Q2` = dplyr::coalesce(`3PTM_Q2`, 0L),
    `3PTM_Q3` = dplyr::coalesce(`3PTM_Q3`, 0L),
    `3PTM_Q4` = dplyr::coalesce(`3PTM_Q4`, 0L),
    
    `3PT_PTS_Q1` = dplyr::coalesce(`3PT_PTS_Q1`, 0L),
    `3PT_PTS_Q2` = dplyr::coalesce(`3PT_PTS_Q2`, 0L),
    `3PT_PTS_Q3` = dplyr::coalesce(`3PT_PTS_Q3`, 0L),
    `3PT_PTS_Q4` = dplyr::coalesce(`3PT_PTS_Q4`, 0L),
    
    # OT periods only if the game had OT
    `3PTA_Q5` = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(`3PTA_Q5`, 0L), 0L),
    `3PTA_Q6` = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(`3PTA_Q6`, 0L), 0L),
    `3PTM_Q5` = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(`3PTM_Q5`, 0L), 0L),
    `3PTM_Q6` = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(`3PTM_Q6`, 0L), 0L),
    `3PT_PTS_Q5` = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(`3PT_PTS_Q5`, 0L), 0L),
    `3PT_PTS_Q6` = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(`3PT_PTS_Q6`, 0L), 0L),
    
    `3PTA_CGS`   = dplyr::coalesce(`3PTA_CGS`, 0L),
    `3PTM_CGS`   = dplyr::coalesce(`3PTM_CGS`, 0L),
    `3PT_PTS_CGS` = dplyr::coalesce(`3PT_PTS_CGS`, 0L)
  )

# --- Derived player 3PT% / eFG% / PPP / share / rate -------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # 3PT%
    `3PT_PCT_Q1`  = safe_div(`3PTM_Q1`, `3PTA_Q1`),
    `3PT_PCT_Q2`  = safe_div(`3PTM_Q2`, `3PTA_Q2`),
    `3PT_PCT_Q3`  = safe_div(`3PTM_Q3`, `3PTA_Q3`),
    `3PT_PCT_Q4`  = safe_div(`3PTM_Q4`, `3PTA_Q4`),
    `3PT_PCT_Q5`  = dplyr::if_else(OT_1 == 1L, safe_div(`3PTM_Q5`, `3PTA_Q5`), NA_real_),
    `3PT_PCT_Q6`  = dplyr::if_else(OT_2 == 1L, safe_div(`3PTM_Q6`, `3PTA_Q6`), NA_real_),
    `3PT_PCT_CGS` = safe_div(`3PTM_CGS`, `3PTA_CGS`),
    
    # 3PT eFG%
    `3PT_EFG_PCT_Q1`  = safe_div(1.5 * `3PTM_Q1`, `3PTA_Q1`),
    `3PT_EFG_PCT_Q2`  = safe_div(1.5 * `3PTM_Q2`, `3PTA_Q2`),
    `3PT_EFG_PCT_Q3`  = safe_div(1.5 * `3PTM_Q3`, `3PTA_Q3`),
    `3PT_EFG_PCT_Q4`  = safe_div(1.5 * `3PTM_Q4`, `3PTA_Q4`),
    `3PT_EFG_PCT_Q5`  = dplyr::if_else(OT_1 == 1L, safe_div(1.5 * `3PTM_Q5`, `3PTA_Q5`), NA_real_),
    `3PT_EFG_PCT_Q6`  = dplyr::if_else(OT_2 == 1L, safe_div(1.5 * `3PTM_Q6`, `3PTA_Q6`), NA_real_),
    `3PT_EFG_PCT_CGS` = safe_div(1.5 * `3PTM_CGS`, `3PTA_CGS`),
    
    # PPP (points per 3PA)
    `3PT_PPP_Q1`  = safe_div(`3PT_PTS_Q1`, `3PTA_Q1`),
    `3PT_PPP_Q2`  = safe_div(`3PT_PTS_Q2`, `3PTA_Q2`),
    `3PT_PPP_Q3`  = safe_div(`3PT_PTS_Q3`, `3PTA_Q3`),
    `3PT_PPP_Q4`  = safe_div(`3PT_PTS_Q4`, `3PTA_Q4`),
    `3PT_PPP_Q5`  = dplyr::if_else(OT_1 == 1L, safe_div(`3PT_PTS_Q5`, `3PTA_Q5`), NA_real_),
    `3PT_PPP_Q6`  = dplyr::if_else(OT_2 == 1L, safe_div(`3PT_PTS_Q6`, `3PTA_Q6`), NA_real_),
    `3PT_PPP_CGS` = safe_div(`3PT_PTS_CGS`, `3PTA_CGS`),
    
    # Share of player's total points (you already have PTS_SCORED_Q* / PTS_SCORED_CGS)
    `3PT_PTSHR_Q1`  = safe_div(`3PT_PTS_Q1`, PTS_Q1),
    `3PT_PTSHR_Q2`  = safe_div(`3PT_PTS_Q2`, PTS_Q2),
    `3PT_PTSHR_Q3`  = safe_div(`3PT_PTS_Q3`, PTS_Q3),
    `3PT_PTSHR_Q4`  = safe_div(`3PT_PTS_Q4`, PTS_Q4),
    `3PT_PTSHR_Q5`  = dplyr::if_else(OT_1 == 1L, safe_div(`3PT_PTS_Q5`, PTS_Q5), NA_real_),
    `3PT_PTSHR_Q6`  = dplyr::if_else(OT_2 == 1L, safe_div(`3PT_PTS_Q6`, PTS_Q6), NA_real_),
    `3PT_PTSHR_CGS` = safe_div(`3PT_PTS_CGS`, PTS_CGS),
    
    # Rate: player's 3PA as share of player's FGA (FGA_Q* already built)
    `3PT_RATE_Q1`  = safe_div(`3PTA_Q1`, FGA_Q1),
    `3PT_RATE_Q2`  = safe_div(`3PTA_Q2`, FGA_Q2),
    `3PT_RATE_Q3`  = safe_div(`3PTA_Q3`, FGA_Q3),
    `3PT_RATE_Q4`  = safe_div(`3PTA_Q4`, FGA_Q4),
    `3PT_RATE_Q5`  = dplyr::if_else(OT_1 == 1L, safe_div(`3PTA_Q5`, FGA_Q5), NA_real_),
    `3PT_RATE_Q6`  = dplyr::if_else(OT_2 == 1L, safe_div(`3PTA_Q6`, FGA_Q6), NA_real_),
    `3PT_RATE_CGS` = safe_div(`3PTA_CGS`, FGA_CGS)
  )

# ============================================
# Player 3PT% relative to TEAM 3PTA (Q1–Q6 + CGS)
# Produces P_3PT_PCT_T3P_Q1..Q6 and P_3PT_PCT_T3P_CGS
# ============================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    P_3PT_PCT_T2P_Q1 = ifelse(T_3PTA_Q1 > 0, T_3PTM_Q1 / T_3PTA_Q1, NA_real_),
    P_3PT_PCT_T2P_Q2 = ifelse(T_3PTA_Q2 > 0, T_3PTM_Q2 / T_3PTA_Q2, NA_real_),
    P_3PT_PCT_T2P_Q3 = ifelse(T_3PTA_Q3 > 0, T_3PTM_Q3 / T_3PTA_Q3, NA_real_),
    P_3PT_PCT_T2P_Q4 = ifelse(T_3PTA_Q4 > 0, T_3PTM_Q4 / T_3PTA_Q4, NA_real_),
    
    # OT periods only if the game actually had OT
    P_3PT_PCT_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_3PTA_Q5 > 0,
      T_3PTM_Q5 / T_3PTA_Q5,
      NA_real_
    ),
    P_3PT_PCT_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_3PTA_Q6 > 0,
      T_3PTM_Q6 / T_3PTA_Q6,
      NA_real_
    ),
    
    # Complete game
    P_3PT_PCT_T2P_CGS = ifelse(
      T_3PTA_CGS > 0,
      T_3PTM_CGS / T_3PTA_CGS,
      NA_real_
    )
  )

rm(player_three_base, player_t3_qtr, player_t3_cgs)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: eFG%, TS%  Field Goal Data Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  
  # =====================================================
# Ensure base shooting / scoring / possession columns
# =====================================================
dplyr::mutate(
  # FGA
  FGA_Q1  = coalesce(FGA_Q1,0),
  FGA_Q2  = coalesce(FGA_Q2,0),
  FGA_Q3  = coalesce(FGA_Q3,0),
  FGA_Q4  = coalesce(FGA_Q4,0),
  FGA_Q5  = if_else(OT_1 == 1L, coalesce(FGA_Q5,0), 0),
  FGA_Q6  = if_else(OT_2 == 1L, coalesce(FGA_Q6,0), 0),
  FGA_CGS = coalesce(FGA_CGS,0),
  
  # FGM
  FGM_Q1  = coalesce(FGM_Q1,0),
  FGM_Q2  = coalesce(FGM_Q2,0),
  FGM_Q3  = coalesce(FGM_Q3,0),
  FGM_Q4  = coalesce(FGM_Q4,0),
  FGM_Q5  = if_else(OT_1 == 1L, coalesce(FGM_Q5,0), 0),
  FGM_Q6  = if_else(OT_2 == 1L, coalesce(FGM_Q6,0), 0),
  FGM_CGS = coalesce(FGM_CGS,0),
  
  # 3PTM  (NO PREFIX — correct names)
  `3PTM_Q1`  = coalesce(`3PTM_Q1`,0L),
  `3PTM_Q2`  = coalesce(`3PTM_Q2`,0L),
  `3PTM_Q3`  = coalesce(`3PTM_Q3`,0L),
  `3PTM_Q4`  = coalesce(`3PTM_Q4`,0L),
  `3PTM_Q5`  = if_else(OT_1 == 1L, coalesce(`3PTM_Q5`,0L), 0L),
  `3PTM_Q6`  = if_else(OT_2 == 1L, coalesce(`3PTM_Q6`,0L), 0L),
  `3PTM_CGS` = coalesce(`3PTM_CGS`,0L),
  
  # FTA
  FTA_Q1  = coalesce(FTA_Q1,0L),
  FTA_Q2  = coalesce(FTA_Q2,0L),
  FTA_Q3  = coalesce(FTA_Q3,0L),
  FTA_Q4  = coalesce(FTA_Q4,0L),
  FTA_Q5  = if_else(OT_1 == 1L, coalesce(FTA_Q5,0L), 0L),
  FTA_Q6  = if_else(OT_2 == 1L, coalesce(FTA_Q6,0L), 0L),
  FTA_CGS = coalesce(FTA_CGS,0L),
  
  # POINTS
  PTS_Q1  = coalesce(PTS_Q1,0),
  PTS_Q2  = coalesce(PTS_Q2,0),
  PTS_Q3  = coalesce(PTS_Q3,0),
  PTS_Q4  = coalesce(PTS_Q4,0),
  PTS_Q5  = if_else(OT_1 == 1L, coalesce(PTS_Q5,0), 0),
  PTS_Q6  = if_else(OT_2 == 1L, coalesce(PTS_Q6,0), 0),
  PTS_CGS = coalesce(PTS_CGS,0)
) %>%
  
  # =====================================================
# eFG% = (FGM + 0.5 * 3PTM) / FGA
# =====================================================
dplyr::mutate(
  EFG_PCT_Q1  = safe_div(FGM_Q1  + 0.5 * `3PTM_Q1`, FGA_Q1),
  EFG_PCT_Q2  = safe_div(FGM_Q2  + 0.5 * `3PTM_Q2`, FGA_Q2),
  EFG_PCT_Q3  = safe_div(FGM_Q3  + 0.5 * `3PTM_Q3`, FGA_Q3),
  EFG_PCT_Q4  = safe_div(FGM_Q4  + 0.5 * `3PTM_Q4`, FGA_Q4),
  EFG_PCT_Q5  = if_else(OT_1 == 1L, safe_div(FGM_Q5 + 0.5 * `3PTM_Q5`, FGA_Q5), NA_real_),
  EFG_PCT_Q6  = if_else(OT_2 == 1L, safe_div(FGM_Q6 + 0.5 * `3PTM_Q6`, FGA_Q6), NA_real_),
  EFG_PCT_CGS = safe_div(FGM_CGS + 0.5 * `3PTM_CGS`, FGA_CGS)
) %>%
  
  # =====================================================
# TS% = PTS / (2 * (FGA + 0.44 * FTA))
# =====================================================
dplyr::mutate(
  TS_PCT_Q1 = { d <- (FGA_Q1 + 0.44 * FTA_Q1); ifelse(d > 0, PTS_Q1 / (2 * d), NA_real_) },
  TS_PCT_Q2 = { d <- (FGA_Q2 + 0.44 * FTA_Q2); ifelse(d > 0, PTS_Q2 / (2 * d), NA_real_) },
  TS_PCT_Q3 = { d <- (FGA_Q3 + 0.44 * FTA_Q3); ifelse(d > 0, PTS_Q3 / (2 * d), NA_real_) },
  TS_PCT_Q4 = { d <- (FGA_Q4 + 0.44 * FTA_Q4); ifelse(d > 0, PTS_Q4 / (2 * d), NA_real_) },
  
  TS_PCT_Q5 = if_else(
    OT_1 == 1L,
    { d <- (FGA_Q5 + 0.44 * FTA_Q5); ifelse(d > 0, PTS_Q5 / (2 * d), NA_real_) },
    NA_real_
  ),
  
  TS_PCT_Q6 = if_else(
    OT_2 == 1L,
    { d <- (FGA_Q6 + 0.44 * FTA_Q6); ifelse(d > 0, PTS_Q6 / (2 * d), NA_real_) },
    NA_real_
  ),
  
  TS_PCT_CGS = {
    d <- (FGA_CGS + 0.44 * FTA_CGS)
    ifelse(d > 0, PTS_CGS / (2 * d), NA_real_)
  }
)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: eFG%, TS%  Field Goal Data Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Second Chance Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ============================================================
# Bring team second-chance totals onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================================

team_sec_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA / FGM
    "T_FGA_SEC_CHN_Q1","T_FGA_SEC_CHN_Q2","T_FGA_SEC_CHN_Q3","T_FGA_SEC_CHN_Q4",
    "T_FGA_SEC_CHN_Q5","T_FGA_SEC_CHN_Q6","T_FGA_SEC_CHN_CGS",
    "T_FGM_SEC_CHN_Q1","T_FGM_SEC_CHN_Q2","T_FGM_SEC_CHN_Q3","T_FGM_SEC_CHN_Q4",
    "T_FGM_SEC_CHN_Q5","T_FGM_SEC_CHN_Q6","T_FGM_SEC_CHN_CGS"
  )
)

team_sec_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_sec_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_sec_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )

# Assumes pm_df has: game_id, team_id, qtr, type_id, shooting_play, scoring_play,
#                    score_value, athlete_id_1
# and that BaseStats_Player_MC has: ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID, OT_1, OT_2,
#                                   POSS_Q*, PTS_Q* already set up.

safe_div <- function(num, den) ifelse(den > 0 & !is.na(den), num / den, NA_real_)

# ---- Build player second-chance base (credit to shooter on next play) ----
sec_player_base <- pm_df %>%
  dplyr::mutate(
    qtr      = suppressWarnings(as.integer(qtr)),
    idx      = dplyr::row_number(),                          # preserve order
    is_orb   = suppressWarnings(as.integer(type_id)) == 156L,# offensive rebound
    sh       = tolower(as.character(shooting_play)) %in% c("true","t","1","yes","y"),
    sc       = tolower(as.character(scoring_play)) %in% c("true","t","1","yes","y"),
    val      = suppressWarnings(as.integer(score_value)),
    shooter_next = dplyr::lead(athlete_id_1)                 # shooter on next play
  ) %>%
  dplyr::arrange(idx) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::mutate(
    next_shoot = dplyr::lead(sh),
    next_score = dplyr::lead(sc),
    next_val   = dplyr::lead(val)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is_orb, !is.na(shooter_next)) %>%            # keep ORBs w/ identified shooter
  dplyr::transmute(
    game_id,
    team_id,
    qtr,
    athlete_id_1 = as.character(shooter_next),               # key for player
    next_shoot,
    next_score,
    next_val
  )

# ---- Quarter splits (Q1–Q6) ----
sec_player_qtr <- sec_player_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    FGA_SEC_CHN  = sum(next_shoot, na.rm = TRUE),
    FGM_SEC_CHN  = sum(next_score, na.rm = TRUE),
    SEC_CHN_PTS  = sum(
      dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L),
      na.rm = TRUE
    ),
    SEC_CHN_POSS = dplyr::n(),   # each ORB is one second-chance possession
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      FGA_SEC_CHN  = 0L,
      FGM_SEC_CHN  = 0L,
      SEC_CHN_PTS  = 0L,
      SEC_CHN_POSS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(FGA_SEC_CHN, FGM_SEC_CHN, SEC_CHN_PTS, SEC_CHN_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
sec_player_cgs <- sec_player_base %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  dplyr::summarise(
    FGA_SEC_CHN_CGS  = sum(next_shoot, na.rm = TRUE),
    FGM_SEC_CHN_CGS  = sum(next_score, na.rm = TRUE),
    SEC_CHN_PTS_CGS  = sum(
      dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L),
      na.rm = TRUE
    ),
    SEC_CHN_POSS_CGS = dplyr::n(),
    .groups = "drop"
  )

# ---- Join onto BaseStats_Player_MC ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    sec_player_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::left_join(
    sec_player_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always; OT only if game had OT
    FGA_SEC_CHN_Q1  = dplyr::coalesce(FGA_SEC_CHN_Q1, 0L),
    FGA_SEC_CHN_Q2  = dplyr::coalesce(FGA_SEC_CHN_Q2, 0L),
    FGA_SEC_CHN_Q3  = dplyr::coalesce(FGA_SEC_CHN_Q3, 0L),
    FGA_SEC_CHN_Q4  = dplyr::coalesce(FGA_SEC_CHN_Q4, 0L),
    FGA_SEC_CHN_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGA_SEC_CHN_Q5, 0L), 0L),
    FGA_SEC_CHN_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGA_SEC_CHN_Q6, 0L), 0L),
    FGA_SEC_CHN_CGS = dplyr::coalesce(FGA_SEC_CHN_CGS, 0L),
    
    FGM_SEC_CHN_Q1  = dplyr::coalesce(FGM_SEC_CHN_Q1, 0L),
    FGM_SEC_CHN_Q2  = dplyr::coalesce(FGM_SEC_CHN_Q2, 0L),
    FGM_SEC_CHN_Q3  = dplyr::coalesce(FGM_SEC_CHN_Q3, 0L),
    FGM_SEC_CHN_Q4  = dplyr::coalesce(FGM_SEC_CHN_Q4, 0L),
    FGM_SEC_CHN_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGM_SEC_CHN_Q5, 0L), 0L),
    FGM_SEC_CHN_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGM_SEC_CHN_Q6, 0L), 0L),
    FGM_SEC_CHN_CGS = dplyr::coalesce(FGM_SEC_CHN_CGS, 0L),
    
    SEC_CHN_PTS_Q1  = dplyr::coalesce(SEC_CHN_PTS_Q1, 0L),
    SEC_CHN_PTS_Q2  = dplyr::coalesce(SEC_CHN_PTS_Q2, 0L),
    SEC_CHN_PTS_Q3  = dplyr::coalesce(SEC_CHN_PTS_Q3, 0L),
    SEC_CHN_PTS_Q4  = dplyr::coalesce(SEC_CHN_PTS_Q4, 0L),
    SEC_CHN_PTS_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(SEC_CHN_PTS_Q5, 0L), 0L),
    SEC_CHN_PTS_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(SEC_CHN_PTS_Q6, 0L), 0L),
    SEC_CHN_PTS_CGS = dplyr::coalesce(SEC_CHN_PTS_CGS, 0L),
    
    SEC_CHN_POSS_Q1  = dplyr::coalesce(SEC_CHN_POSS_Q1, 0L),
    SEC_CHN_POSS_Q2  = dplyr::coalesce(SEC_CHN_POSS_Q2, 0L),
    SEC_CHN_POSS_Q3  = dplyr::coalesce(SEC_CHN_POSS_Q3, 0L),
    SEC_CHN_POSS_Q4  = dplyr::coalesce(SEC_CHN_POSS_Q4, 0L),
    SEC_CHN_POSS_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(SEC_CHN_POSS_Q5, 0L), 0L),
    SEC_CHN_POSS_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(SEC_CHN_POSS_Q6, 0L), 0L),
    SEC_CHN_POSS_CGS = dplyr::coalesce(SEC_CHN_POSS_CGS, 0L)
  ) %>%
  
  # ---- Player second-chance FG%, PPP, Rate, Point Share ----
dplyr::mutate(
  # FG%
  FG_PCT_SEC_CHN_Q1  = safe_div(FGM_SEC_CHN_Q1 , FGA_SEC_CHN_Q1),
  FG_PCT_SEC_CHN_Q2  = safe_div(FGM_SEC_CHN_Q2 , FGA_SEC_CHN_Q2),
  FG_PCT_SEC_CHN_Q3  = safe_div(FGM_SEC_CHN_Q3 , FGA_SEC_CHN_Q3),
  FG_PCT_SEC_CHN_Q4  = safe_div(FGM_SEC_CHN_Q4 , FGA_SEC_CHN_Q4),
  FG_PCT_SEC_CHN_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(FGM_SEC_CHN_Q5 , FGA_SEC_CHN_Q5), NA_real_),
  FG_PCT_SEC_CHN_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(FGM_SEC_CHN_Q6 , FGA_SEC_CHN_Q6), NA_real_),
  FG_PCT_SEC_CHN_CGS = safe_div(FGM_SEC_CHN_CGS, FGA_SEC_CHN_CGS),
  
  # PPP (points per second-chance possession)
  SEC_CHN_PPP_Q1  = safe_div(SEC_CHN_PTS_Q1 , SEC_CHN_POSS_Q1),
  SEC_CHN_PPP_Q2  = safe_div(SEC_CHN_PTS_Q2 , SEC_CHN_POSS_Q2),
  SEC_CHN_PPP_Q3  = safe_div(SEC_CHN_PTS_Q3 , SEC_CHN_POSS_Q3),
  SEC_CHN_PPP_Q4  = safe_div(SEC_CHN_PTS_Q4 , SEC_CHN_POSS_Q4),
  SEC_CHN_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(SEC_CHN_PTS_Q5 , SEC_CHN_POSS_Q5), NA_real_),
  SEC_CHN_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(SEC_CHN_PTS_Q6 , SEC_CHN_POSS_Q6), NA_real_),
  SEC_CHN_PPP_CGS = safe_div(SEC_CHN_PTS_CGS, SEC_CHN_POSS_CGS),
  

  # Point share: share of player total points coming from second chances
  SEC_CHN_PTSHR_Q1  = safe_div(SEC_CHN_PTS_Q1 , PTS_Q1),
  SEC_CHN_PTSHR_Q2  = safe_div(SEC_CHN_PTS_Q2 , PTS_Q2),
  SEC_CHN_PTSHR_Q3  = safe_div(SEC_CHN_PTS_Q3 , PTS_Q3),
  SEC_CHN_PTSHR_Q4  = safe_div(SEC_CHN_PTS_Q4 , PTS_Q4),
  SEC_CHN_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(SEC_CHN_PTS_Q5 , PTS_Q5), NA_real_),
  SEC_CHN_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(SEC_CHN_PTS_Q6 , PTS_Q6), NA_real_),
  SEC_CHN_PTSHR_CGS = safe_div(SEC_CHN_PTS_CGS, PTS_CGS)
)

# ============================================================
# Player Second-Chance FG% relative to TEAM second-chance FGA
# Produces: FG_PCT_SEC_CHN_T2P_Q1..Q6 and FG_PCT_SEC_CHN_T2P_CGS
# ============================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_SEC_CHN_T2P_Q1 = ifelse(T_FGA_SEC_CHN_Q1 > 0,
                                   FGM_SEC_CHN_Q1 / T_FGA_SEC_CHN_Q1, NA_real_),
    FG_PCT_SEC_CHN_T2P_Q2 = ifelse(T_FGA_SEC_CHN_Q2 > 0,
                                   FGM_SEC_CHN_Q2 / T_FGA_SEC_CHN_Q2, NA_real_),
    FG_PCT_SEC_CHN_T2P_Q3 = ifelse(T_FGA_SEC_CHN_Q3 > 0,
                                   FGM_SEC_CHN_Q3 / T_FGA_SEC_CHN_Q3, NA_real_),
    FG_PCT_SEC_CHN_T2P_Q4 = ifelse(T_FGA_SEC_CHN_Q4 > 0,
                                   FGM_SEC_CHN_Q4 / T_FGA_SEC_CHN_Q4, NA_real_),
    
    # OT periods only if game actually had OT
    FG_PCT_SEC_CHN_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_FGA_SEC_CHN_Q5 > 0,
      FGM_SEC_CHN_Q5 / T_FGA_SEC_CHN_Q5,
      NA_real_
    ),
    FG_PCT_SEC_CHN_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_FGA_SEC_CHN_Q6 > 0,
      FGM_SEC_CHN_Q6 / T_FGA_SEC_CHN_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_SEC_CHN_T2P_CGS = ifelse(
      T_FGA_SEC_CHN_CGS > 0,
      FGM_SEC_CHN_CGS / T_FGA_SEC_CHN_CGS,
      NA_real_
    )
  )

rm(sec_player_base, sec_player_qtr, sec_player_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Second Chance Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fast Break Data Aggregation Section — PLAYER (athlete_id_1) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ============================================
# Bring team T_FBRK_FGA_* and T_FBRK_FGM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ============================================

team_fbrk_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_FBRK_FGA_Q1","T_FBRK_FGA_Q2","T_FBRK_FGA_Q3","T_FBRK_FGA_Q4",
    "T_FBRK_FGA_Q5","T_FBRK_FGA_Q6","T_FBRK_FGA_CGS",
    # FGM
    "T_FBRK_FGM_Q1","T_FBRK_FGM_Q2","T_FBRK_FGM_Q3","T_FBRK_FGM_Q4",
    "T_FBRK_FGM_Q5","T_FBRK_FGM_Q6","T_FBRK_FGM_CGS"
  )
)

team_fbrk_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_fbrk_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_fbrk_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )

# ============================================
## === START: Fast Break Data Aggregation Section — PLAYER (athlete_id_1) ====
# ============================================

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "shooting_play","scoring_play","score_value",
  "clock_minutes","clock_seconds","athlete_id_1"
) %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build "next play" context, attributing to next shooter (athlete_id_1) ----
fb_player_base <- pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds)),
    trig  = stringr::str_detect(
      type_text,
      stringr::regex("Turnover|Steal|Defensive Rebound", ignore_case = TRUE)
    )
  ) %>%
  dplyr::arrange(game_id, qtr, dplyr::desc(t_sec)) %>%   # next row = smaller clock
  dplyr::group_by(game_id, qtr) %>%
  dplyr::mutate(
    next_team   = dplyr::lead(team_id),
    next_t_sec  = dplyr::lead(t_sec),
    next_shoot  = to_bool(dplyr::lead(shooting_play)),
    next_scoreF = to_bool(dplyr::lead(scoring_play)),
    next_pts    = suppressWarnings(as.integer(dplyr::lead(score_value))),
    next_player = dplyr::lead(athlete_id_1),
    dt          = as.integer(t_sec - next_t_sec)          # clock counts down
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    game_id,
    team_id      = next_team,      # offensive team on the break
    athlete_id_1 = next_player,    # shooter on the fast break
    qtr,
    fb_poss = trig & dt >= 0L & dt <= 6L &
      !is.na(next_team) & !is.na(next_player),
    fb_fga  = fb_poss & next_shoot,
    fb_fgm  = fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L,
    fb_pts  = dplyr::if_else(
      fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L,
      next_pts,
      0L
    )
  ) %>%
  dplyr::filter(fb_poss)

# ---- Per-quarter (Q1–Q6) player tallies ----
fbrk_player_qtr <- fb_player_base %>%
  dplyr::mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    FBRK_FGA  = sum(fb_fga,  na.rm = TRUE),
    FBRK_FGM  = sum(fb_fgm,  na.rm = TRUE),
    FBRK_PTS  = sum(fb_pts,  na.rm = TRUE),
    FBRK_POSS = sum(fb_poss, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(FBRK_FGA = 0L, FBRK_FGM = 0L, FBRK_PTS = 0L, FBRK_POSS = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(FBRK_FGA, FBRK_FGM, FBRK_PTS, FBRK_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) player tallies ----
fbrk_player_cgs <- fb_player_base %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  dplyr::summarise(
    FBRK_FGA_CGS  = sum(fb_fga,  na.rm = TRUE),
    FBRK_FGM_CGS  = sum(fb_fgm,  na.rm = TRUE),
    FBRK_PTS_CGS  = sum(fb_pts,  na.rm = TRUE),
    FBRK_POSS_CGS = sum(fb_poss, na.rm = TRUE),
    .groups       = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking / coalesce ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    fbrk_player_qtr,
    by = c(
      "ESPN_GAME_ID"    = "game_id",
      "ESPN_TEAM_ID"    = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::left_join(
    fbrk_player_cgs,
    by = c(
      "ESPN_GAME_ID"    = "game_id",
      "ESPN_TEAM_ID"    = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always
    FBRK_FGA_Q1 = dplyr::coalesce(FBRK_FGA_Q1, 0L),
    FBRK_FGA_Q2 = dplyr::coalesce(FBRK_FGA_Q2, 0L),
    FBRK_FGA_Q3 = dplyr::coalesce(FBRK_FGA_Q3, 0L),
    FBRK_FGA_Q4 = dplyr::coalesce(FBRK_FGA_Q4, 0L),
    FBRK_FGM_Q1 = dplyr::coalesce(FBRK_FGM_Q1, 0L),
    FBRK_FGM_Q2 = dplyr::coalesce(FBRK_FGM_Q2, 0L),
    FBRK_FGM_Q3 = dplyr::coalesce(FBRK_FGM_Q3, 0L),
    FBRK_FGM_Q4 = dplyr::coalesce(FBRK_FGM_Q4, 0L),
    FBRK_PTS_Q1 = dplyr::coalesce(FBRK_PTS_Q1, 0L),
    FBRK_PTS_Q2 = dplyr::coalesce(FBRK_PTS_Q2, 0L),
    FBRK_PTS_Q3 = dplyr::coalesce(FBRK_PTS_Q3, 0L),
    FBRK_PTS_Q4 = dplyr::coalesce(FBRK_PTS_Q4, 0L),
    FBRK_POSS_Q1 = dplyr::coalesce(FBRK_POSS_Q1, 0L),
    FBRK_POSS_Q2 = dplyr::coalesce(FBRK_POSS_Q2, 0L),
    FBRK_POSS_Q3 = dplyr::coalesce(FBRK_POSS_Q3, 0L),
    FBRK_POSS_Q4 = dplyr::coalesce(FBRK_POSS_Q4, 0L),
    
    # OT only if OT_1 / OT_2 active
    FBRK_FGA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FBRK_FGA_Q5, 0L), 0L),
    FBRK_FGA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FBRK_FGA_Q6, 0L), 0L),
    FBRK_FGM_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FBRK_FGM_Q5, 0L), 0L),
    FBRK_FGM_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FBRK_FGM_Q6, 0L), 0L),
    FBRK_PTS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FBRK_PTS_Q5, 0L), 0L),
    FBRK_PTS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FBRK_PTS_Q6, 0L), 0L),
    FBRK_POSS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FBRK_POSS_Q5, 0L), 0L),
    FBRK_POSS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FBRK_POSS_Q6, 0L), 0L),
    
    FBRK_FGA_CGS  = dplyr::coalesce(FBRK_FGA_CGS, 0L),
    FBRK_FGM_CGS  = dplyr::coalesce(FBRK_FGM_CGS, 0L),
    FBRK_PTS_CGS  = dplyr::coalesce(FBRK_PTS_CGS, 0L),
    FBRK_POSS_CGS = dplyr::coalesce(FBRK_POSS_CGS, 0L)
  )

# ============================================
# Player Fast-Break FG% relative to TEAM FBRK FGA
# Produces FG_PCT_FBRK_T2P_Q1..Q6 and FG_PCT_FBRK_T2P_CGS
# ============================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_FBRK_T2P_Q1 = ifelse(T_FBRK_FGA_Q1 > 0, FBRK_FGM_Q1 / T_FBRK_FGA_Q1, NA_real_),
    FG_PCT_FBRK_T2P_Q2 = ifelse(T_FBRK_FGA_Q2 > 0, FBRK_FGM_Q2 / T_FBRK_FGA_Q2, NA_real_),
    FG_PCT_FBRK_T2P_Q3 = ifelse(T_FBRK_FGA_Q3 > 0, FBRK_FGM_Q3 / T_FBRK_FGA_Q3, NA_real_),
    FG_PCT_FBRK_T2P_Q4 = ifelse(T_FBRK_FGA_Q4 > 0, FBRK_FGM_Q4 / T_FBRK_FGA_Q4, NA_real_),
    
    # OT periods only if the game actually had OT
    FG_PCT_FBRK_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_FBRK_FGA_Q5 > 0,
      FBRK_FGM_Q5 / T_FBRK_FGA_Q5,
      NA_real_
    ),
    FG_PCT_FBRK_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_FBRK_FGA_Q6 > 0,
      FBRK_FGM_Q6 / T_FBRK_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_FBRK_T2P_CGS = ifelse(
      T_FBRK_FGA_CGS > 0,
      FBRK_FGM_CGS / T_FBRK_FGA_CGS,
      NA_real_
    )
  )
rm(fb_player_base, fbrk_player_qtr, fbrk_player_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fast Break Data Aggregation Section — PLAYER (athlete_id_1) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: AND1 Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: AND1 Player Data Aggregation Section (athlete_id_1 → ESPN_PLAYER_ID) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ================================================================
# Bring team AND1 totals onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_and1_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_AND1_FGA_Q1","T_AND1_FGA_Q2","T_AND1_FGA_Q3","T_AND1_FGA_Q4",
    "T_AND1_FGA_Q5","T_AND1_FGA_Q6","T_AND1_FGA_CGS",
    # FGM
    "T_AND1_FGM_Q1","T_AND1_FGM_Q2","T_AND1_FGM_Q3","T_AND1_FGM_Q4",
    "T_AND1_FGM_Q5","T_AND1_FGM_Q6","T_AND1_FGM_CGS"
  )
)

team_and1_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_and1_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_and1_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )



stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value",
                "athlete_id_1") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---------- Build AND-1 base at *player* level ----------
and1_player_base <- pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(type_text == "Free Throw - 1 of 1") %>%
  dplyr::transmute(
    game_id,
    team_id,
    athlete_id_1,
    qtr,
    and1_fga = shot,                                  # attempt flag
    and1_fgm = make & !is.na(pts) & pts > 0L,         # made flag
    and1_pts = dplyr::if_else(make & !is.na(pts) & pts > 0L, pts, 0L)
  )

# ---------- Per-quarter (Q1–Q6) ----------
and1_player_qtr <- and1_player_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
  dplyr::summarise(
    AND1_FGA = sum(and1_fga, na.rm = TRUE),
    AND1_FGM = sum(and1_fgm, na.rm = TRUE),
    AND1_PTS = sum(and1_pts, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(AND1_FGA = 0L,
                AND1_FGM = 0L,
                AND1_PTS = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(AND1_FGA, AND1_FGM, AND1_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---------- Complete game (CGS) ----------
and1_player_cgs <- and1_player_base %>%
  dplyr::group_by(game_id, team_id, athlete_id_1) %>%
  dplyr::summarise(
    AND1_FGA_CGS = sum(and1_fga, na.rm = TRUE),
    AND1_FGM_CGS = sum(and1_fgm, na.rm = TRUE),
    AND1_PTS_CGS = sum(and1_pts, na.rm = TRUE),
    .groups        = "drop"
  )

# ---------- Join onto BaseStats_Player_MC (ESPN_PLAYER_ID) ----------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    and1_player_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::left_join(
    and1_player_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "athlete_id_1"
    )
  ) %>%
  dplyr::mutate(
    # coalesce + OT masking
    AND1_FGA_Q1  = dplyr::coalesce(AND1_FGA_Q1, 0L),
    AND1_FGA_Q2  = dplyr::coalesce(AND1_FGA_Q2, 0L),
    AND1_FGA_Q3  = dplyr::coalesce(AND1_FGA_Q3, 0L),
    AND1_FGA_Q4  = dplyr::coalesce(AND1_FGA_Q4, 0L),
    AND1_FGA_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(AND1_FGA_Q5, 0L), 0L),
    AND1_FGA_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(AND1_FGA_Q6, 0L), 0L),
    AND1_FGA_CGS = dplyr::coalesce(AND1_FGA_CGS, 0L),
    
    AND1_FGM_Q1  = dplyr::coalesce(AND1_FGM_Q1, 0L),
    AND1_FGM_Q2  = dplyr::coalesce(AND1_FGM_Q2, 0L),
    AND1_FGM_Q3  = dplyr::coalesce(AND1_FGM_Q3, 0L),
    AND1_FGM_Q4  = dplyr::coalesce(AND1_FGM_Q4, 0L),
    AND1_FGM_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(AND1_FGM_Q5, 0L), 0L),
    AND1_FGM_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(AND1_FGM_Q6, 0L), 0L),
    AND1_FGM_CGS = dplyr::coalesce(AND1_FGM_CGS, 0L),
    
    AND1_PTS_Q1  = dplyr::coalesce(AND1_PTS_Q1, 0L),
    AND1_PTS_Q2  = dplyr::coalesce(AND1_PTS_Q2, 0L),
    AND1_PTS_Q3  = dplyr::coalesce(AND1_PTS_Q3, 0L),
    AND1_PTS_Q4  = dplyr::coalesce(AND1_PTS_Q4, 0L),
    AND1_PTS_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(AND1_PTS_Q5, 0L), 0L),
    AND1_PTS_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(AND1_PTS_Q6, 0L), 0L),
    AND1_PTS_CGS = dplyr::coalesce(AND1_PTS_CGS, 0L)
  )

# ---------- Derived: FG%, PPP, Rate vs player FGA ----------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # FG%
    AND1_FG_PCT_Q1  = safe_div(AND1_FGM_Q1 , AND1_FGA_Q1),
    AND1_FG_PCT_Q2  = safe_div(AND1_FGM_Q2 , AND1_FGA_Q2),
    AND1_FG_PCT_Q3  = safe_div(AND1_FGM_Q3 , AND1_FGA_Q3),
    AND1_FG_PCT_Q4  = safe_div(AND1_FGM_Q4 , AND1_FGA_Q4),
    AND1_FG_PCT_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(AND1_FGM_Q5 , AND1_FGA_Q5), NA_real_),
    AND1_FG_PCT_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(AND1_FGM_Q6 , AND1_FGA_Q6), NA_real_),
    AND1_FG_PCT_CGS = safe_div(AND1_FGM_CGS, AND1_FGA_CGS),
    
    # PPP (points per AND-1 attempt)
    AND1_PPP_Q1  = safe_div(AND1_PTS_Q1 , AND1_FGA_Q1),
    AND1_PPP_Q2  = safe_div(AND1_PTS_Q2 , AND1_FGA_Q2),
    AND1_PPP_Q3  = safe_div(AND1_PTS_Q3 , AND1_FGA_Q3),
    AND1_PPP_Q4  = safe_div(AND1_PTS_Q4 , AND1_FGA_Q4),
    AND1_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(AND1_PTS_Q5 , AND1_FGA_Q5), NA_real_),
    AND1_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(AND1_PTS_Q6 , AND1_FGA_Q6), NA_real_),
    AND1_PPP_CGS = safe_div(AND1_PTS_CGS, AND1_FGA_CGS),
    
    # RATE = AND-1 FGA as share of player's total FGA
    FGA_Q1  = dplyr::coalesce(FGA_Q1,0),
    FGA_Q2  = dplyr::coalesce(FGA_Q2,0),
    FGA_Q3  = dplyr::coalesce(FGA_Q3,0),
    FGA_Q4  = dplyr::coalesce(FGA_Q4,0),
    FGA_Q5  = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(FGA_Q5,0), 0),
    FGA_Q6  = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(FGA_Q6,0), 0),
    FGA_CGS = dplyr::coalesce(FGA_CGS,0),
    
    AND1_RATE_Q1  = safe_div(AND1_FGA_Q1 , FGA_Q1),
    AND1_RATE_Q2  = safe_div(AND1_FGA_Q2 , FGA_Q2),
    AND1_RATE_Q3  = safe_div(AND1_FGA_Q3 , FGA_Q3),
    AND1_RATE_Q4  = safe_div(AND1_FGA_Q4 , FGA_Q4),
    AND1_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(AND1_FGA_Q5 , FGA_Q5), NA_real_),
    AND1_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(AND1_FGA_Q6 , FGA_Q6), NA_real_),
    AND1_RATE_CGS = safe_div(AND1_FGA_CGS, FGA_CGS)
  )


# ================================================================
# Player AND1 FG% relative to TEAM AND1 FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_AND1_T2P_Q1..Q6 and FG_PCT_AND1_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_AND1_T2P_Q1 = ifelse(
      T_AND1_FGA_Q1 > 0,
      AND1_FGM_Q1 / T_AND1_FGA_Q1,
      NA_real_
    ),
    FG_PCT_AND1_T2P_Q2 = ifelse(
      T_AND1_FGA_Q2 > 0,
      AND1_FGM_Q2 / T_AND1_FGA_Q2,
      NA_real_
    ),
    FG_PCT_AND1_T2P_Q3 = ifelse(
      T_AND1_FGA_Q3 > 0,
      AND1_FGM_Q3 / T_AND1_FGA_Q3,
      NA_real_
    ),
    FG_PCT_AND1_T2P_Q4 = ifelse(
      T_AND1_FGA_Q4 > 0,
      AND1_FGM_Q4 / T_AND1_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if the game actually had OT
    FG_PCT_AND1_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_AND1_FGA_Q5 > 0,
      AND1_FGM_Q5 / T_AND1_FGA_Q5,
      NA_real_
    ),
    FG_PCT_AND1_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_AND1_FGA_Q6 > 0,
      AND1_FGM_Q6 / T_AND1_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_AND1_T2P_CGS = ifelse(
      T_AND1_FGA_CGS > 0,
      AND1_FGM_CGS / T_AND1_FGA_CGS,
      NA_real_
    )
  )
rm(and1_player_base, and1_player_qtr, and1_player_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: AND1 Player Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Mid-Range Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ================================================================
# Bring team MIDR_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_midr_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_MIDR_FGA_Q1","T_MIDR_FGA_Q2","T_MIDR_FGA_Q3","T_MIDR_FGA_Q4",
    "T_MIDR_FGA_Q5","T_MIDR_FGA_Q6","T_MIDR_FGA_CGS",
    # FGM
    "T_MIDR_FGM_Q1","T_MIDR_FGM_Q2","T_MIDR_FGM_Q3","T_MIDR_FGM_Q4",
    "T_MIDR_FGM_Q5","T_MIDR_FGM_Q6","T_MIDR_FGM_CGS"
  )
)

team_midr_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_midr_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_midr_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )


# ================================================================
# Player Mid-Range (2PT) FGA / FGM / PTS by athlete_id_1
# Creates: MIDR_FGA_Q1..Q6 + MIDR_FGA_CGS,
#          MIDR_FGM_Q1..Q6 + MIDR_FGM_CGS,
#          MIDR_PTS_Q1..Q6 + MIDR_PTS_CGS
# ================================================================

stopifnot("athlete_id_1" %in% names(pm_df))
stopifnot(exists("BaseStats_Player_MC"))

# Helper to pick shot-zone column & derive sz_col for this block
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(pm_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

player_midr_base <- pm_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shooter_id = as.character(athlete_id_1),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    is_midr    = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) == "mid-range"
    } else {
      stringr::str_detect(type_text, stringr::regex("mid[- ]?range", ignore_case = TRUE))
    }
  ) %>%
  dplyr::filter(
    !is.na(shooter_id),
    shooter_id != "",
    is_midr,
    shot
  ) %>%
  dplyr::transmute(
    game_id, team_id, shooter_id, qtr,
    MIDR_FGA = TRUE,
    MIDR_FGM = make & !is.na(pts) & pts > 0L,
    MIDR_PTS = dplyr::if_else(MIDR_FGM, pts, 0L)
  )

# ---- Player quarter tallies (Q1–Q6) ----
player_midr_qtr <- player_midr_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, shooter_id, qtr) %>%
  dplyr::summarise(
    MIDR_FGA = sum(MIDR_FGA, na.rm = TRUE),
    MIDR_FGM = sum(MIDR_FGM, na.rm = TRUE),
    MIDR_PTS = sum(MIDR_PTS, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(MIDR_FGA, MIDR_FGM, MIDR_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Player complete-game (CGS) tallies ----
player_midr_cgs <- player_midr_base %>%
  dplyr::group_by(game_id, team_id, shooter_id) %>%
  dplyr::summarise(
    MIDR_FGA_CGS = sum(MIDR_FGA, na.rm = TRUE),
    MIDR_FGM_CGS = sum(MIDR_FGM, na.rm = TRUE),
    MIDR_PTS_CGS = sum(MIDR_PTS, na.rm = TRUE),
    .groups      = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    player_midr_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::left_join(
    player_midr_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        MIDR_FGA_Q1, MIDR_FGA_Q2, MIDR_FGA_Q3, MIDR_FGA_Q4, MIDR_FGA_Q5, MIDR_FGA_Q6, MIDR_FGA_CGS,
        MIDR_FGM_Q1, MIDR_FGM_Q2, MIDR_FGM_Q3, MIDR_FGM_Q4, MIDR_FGM_Q5, MIDR_FGM_Q6, MIDR_FGM_CGS,
        MIDR_PTS_Q1, MIDR_PTS_Q2, MIDR_PTS_Q3, MIDR_PTS_Q4, MIDR_PTS_Q5, MIDR_PTS_Q6, MIDR_PTS_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking at player level (only expose if OT occurred)
    MIDR_FGA_Q5 = dplyr::if_else(OT_1 == 1L, MIDR_FGA_Q5, 0L),
    MIDR_FGM_Q5 = dplyr::if_else(OT_1 == 1L, MIDR_FGM_Q5, 0L),
    MIDR_PTS_Q5 = dplyr::if_else(OT_1 == 1L, MIDR_PTS_Q5, 0L),
    
    MIDR_FGA_Q6 = dplyr::if_else(OT_2 == 1L, MIDR_FGA_Q6, 0L),
    MIDR_FGM_Q6 = dplyr::if_else(OT_2 == 1L, MIDR_FGM_Q6, 0L),
    MIDR_PTS_Q6 = dplyr::if_else(OT_2 == 1L, MIDR_PTS_Q6, 0L)
  )


# ================================================================
# Player MIDR FG% relative to TEAM MIDR FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_MIDR_T2P_Q1..Q6 and FG_PCT_MIDR_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_MIDR_T2P_Q1 = ifelse(
      T_MIDR_FGA_Q1 > 0,
      MIDR_FGM_Q1 / T_MIDR_FGA_Q1,
      NA_real_
    ),
    FG_PCT_MIDR_T2P_Q2 = ifelse(
      T_MIDR_FGA_Q2 > 0,
      MIDR_FGM_Q2 / T_MIDR_FGA_Q2,
      NA_real_
    ),
    FG_PCT_MIDR_T2P_Q3 = ifelse(
      T_MIDR_FGA_Q3 > 0,
      MIDR_FGM_Q3 / T_MIDR_FGA_Q3,
      NA_real_
    ),
    FG_PCT_MIDR_T2P_Q4 = ifelse(
      T_MIDR_FGA_Q4 > 0,
      MIDR_FGM_Q4 / T_MIDR_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if game actually had OT
    FG_PCT_MIDR_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_MIDR_FGA_Q5 > 0,
      MIDR_FGM_Q5 / T_MIDR_FGA_Q5,
      NA_real_
    ),
    FG_PCT_MIDR_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_MIDR_FGA_Q6 > 0,
      MIDR_FGM_Q6 / T_MIDR_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_MIDR_T2P_CGS = ifelse(
      T_MIDR_FGA_CGS > 0,
      MIDR_FGM_CGS / T_MIDR_FGA_CGS,
      NA_real_
    )
  )


rm(player_midr_base, player_midr_qtr, player_midr_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Mid-Range Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Rim (Restricted Area) Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ================================================================
# Bring team RIM_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_rim_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_RIM_FGA_Q1","T_RIM_FGA_Q2","T_RIM_FGA_Q3","T_RIM_FGA_Q4",
    "T_RIM_FGA_Q5","T_RIM_FGA_Q6","T_RIM_FGA_CGS",
    # FGM
    "T_RIM_FGM_Q1","T_RIM_FGM_Q2","T_RIM_FGM_Q3","T_RIM_FGM_Q4",
    "T_RIM_FGM_Q5","T_RIM_FGM_Q6","T_RIM_FGM_CGS"
  )
)

team_rim_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_rim_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_rim_df,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  )


# ================================================================
# Player Rim (Restricted Area, 2PT) FGA / FGM / PTS by athlete_id_1
# Creates: RIM_FGA_Q1..Q6 + RIM_FGA_CGS,
#          RIM_FGM_Q1..Q6 + RIM_FGM_CGS,
#          RIM_PTS_Q1..Q6 + RIM_PTS_CGS
# ================================================================

stopifnot("athlete_id_1" %in% names(pm_df))
stopifnot(exists("BaseStats_Player_MC"))

player_rim_base <- pm_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shooter_id = as.character(athlete_id_1),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    is_rim     = if (!is.na(sz_col)) {
      tolower(trimws(.data[[sz_col]])) == "restricted area"
    } else {
      stringr::str_detect(
        type_text,
        stringr::regex("restricted area|at the rim|at rim", ignore_case = TRUE)
      )
    }
  ) %>%
  dplyr::filter(
    !is.na(shooter_id),
    shooter_id != "",
    is_rim,
    shot
  ) %>%
  dplyr::transmute(
    game_id, team_id, shooter_id, qtr,
    RIM_FGA = TRUE,
    RIM_FGM = make & !is.na(pts) & pts > 0L,
    RIM_PTS = dplyr::if_else(RIM_FGM, pts, 0L)
  )

# ---- Player quarter tallies (Q1–Q6) ----
player_rim_qtr <- player_rim_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, shooter_id, qtr) %>%
  dplyr::summarise(
    RIM_FGA = sum(RIM_FGA, na.rm = TRUE),
    RIM_FGM = sum(RIM_FGM, na.rm = TRUE),
    RIM_PTS = sum(RIM_PTS, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(RIM_FGA, RIM_FGM, RIM_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Player complete-game (CGS) tallies ----
player_rim_cgs <- player_rim_base %>%
  dplyr::group_by(game_id, team_id, shooter_id) %>%
  dplyr::summarise(
    RIM_FGA_CGS = sum(RIM_FGA, na.rm = TRUE),
    RIM_FGM_CGS = sum(RIM_FGM, na.rm = TRUE),
    RIM_PTS_CGS = sum(RIM_PTS, na.rm = TRUE),
    .groups     = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    player_rim_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::left_join(
    player_rim_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        RIM_FGA_Q1, RIM_FGA_Q2, RIM_FGA_Q3, RIM_FGA_Q4, RIM_FGA_Q5, RIM_FGA_Q6, RIM_FGA_CGS,
        RIM_FGM_Q1, RIM_FGM_Q2, RIM_FGM_Q3, RIM_FGM_Q4, RIM_FGM_Q5, RIM_FGM_Q6, RIM_FGM_CGS,
        RIM_PTS_Q1, RIM_PTS_Q2, RIM_PTS_Q3, RIM_PTS_Q4, RIM_PTS_Q5, RIM_PTS_Q6, RIM_PTS_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking at player level
    RIM_FGA_Q5 = dplyr::if_else(OT_1 == 1L, RIM_FGA_Q5, 0L),
    RIM_FGM_Q5 = dplyr::if_else(OT_1 == 1L, RIM_FGM_Q5, 0L),
    RIM_PTS_Q5 = dplyr::if_else(OT_1 == 1L, RIM_PTS_Q5, 0L),
    
    RIM_FGA_Q6 = dplyr::if_else(OT_2 == 1L, RIM_FGA_Q6, 0L),
    RIM_FGM_Q6 = dplyr::if_else(OT_2 == 1L, RIM_FGM_Q6, 0L),
    RIM_PTS_Q6 = dplyr::if_else(OT_2 == 1L, RIM_PTS_Q6, 0L)
  )



# ================================================================
# Player RIM FG% relative to TEAM RIM FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_RIM_T2P_Q1..Q6 and FG_PCT_RIM_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_RIM_T2P_Q1 = ifelse(
      T_RIM_FGA_Q1 > 0,
      RIM_FGM_Q1 / T_RIM_FGA_Q1,
      NA_real_
    ),
    FG_PCT_RIM_T2P_Q2 = ifelse(
      T_RIM_FGA_Q2 > 0,
      RIM_FGM_Q2 / T_RIM_FGA_Q2,
      NA_real_
    ),
    FG_PCT_RIM_T2P_Q3 = ifelse(
      T_RIM_FGA_Q3 > 0,
      RIM_FGM_Q3 / T_RIM_FGA_Q3,
      NA_real_
    ),
    FG_PCT_RIM_T2P_Q4 = ifelse(
      T_RIM_FGA_Q4 > 0,
      RIM_FGM_Q4 / T_RIM_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if game actually had OT
    FG_PCT_RIM_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_RIM_FGA_Q5 > 0,
      RIM_FGM_Q5 / T_RIM_FGA_Q5,
      NA_real_
    ),
    FG_PCT_RIM_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_RIM_FGA_Q6 > 0,
      RIM_FGM_Q6 / T_RIM_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_RIM_T2P_CGS = ifelse(
      T_RIM_FGA_CGS > 0,
      RIM_FGM_CGS / T_RIM_FGA_CGS,
      NA_real_
    )
  )

rm(player_rim_base, player_rim_qtr, player_rim_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === EMD: Rim (Restricted Area) Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Putback Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ================================================================
# Bring team PUTB_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_putb_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_PUTB_FGA_Q1","T_PUTB_FGA_Q2","T_PUTB_FGA_Q3","T_PUTB_FGA_Q4",
    "T_PUTB_FGA_Q5","T_PUTB_FGA_Q6","T_PUTB_FGA_CGS",
    # FGM
    "T_PUTB_FGM_Q1","T_PUTB_FGM_Q2","T_PUTB_FGM_Q3","T_PUTB_FGM_Q4",
    "T_PUTB_FGM_Q5","T_PUTB_FGM_Q6","T_PUTB_FGM_CGS"
  )
)

team_putb_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_putb_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_putb_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )

# ================================================================
# Player Putback (2PT) FGA / FGM / PTS by athlete_id_1
# Creates on BaseStats_Player_MC:
#   PUTB_FGA_Q1..Q6 + PUTB_FGA_CGS
#   PUTB_FGM_Q1..Q6 + PUTB_FGM_CGS
#   PUTB_PTS_Q1..Q6 + PUTB_PTS_CGS
# ================================================================

stopifnot("athlete_id_1" %in% names(pm_df))
stopifnot(exists("BaseStats_Player_MC"))

player_putb_base <- pm_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shooter_id = as.character(athlete_id_1),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    is_putb    = stringr::str_detect(
      type_text,
      stringr::regex("\\bputback\\b", ignore_case = TRUE)
    )
  ) %>%
  dplyr::filter(
    !is.na(shooter_id),
    shooter_id != "",
    is_putb,
    shot
  ) %>%
  dplyr::transmute(
    game_id, team_id, shooter_id, qtr,
    PUTB_FGA = TRUE,
    PUTB_FGM = make & !is.na(pts) & pts > 0L,
    PUTB_PTS = dplyr::if_else(PUTB_FGM, pts, 0L)
  )

# ---- Player quarter tallies (Q1–Q6) ----
player_putb_qtr <- player_putb_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, shooter_id, qtr) %>%
  dplyr::summarise(
    PUTB_FGA = sum(PUTB_FGA, na.rm = TRUE),
    PUTB_FGM = sum(PUTB_FGM, na.rm = TRUE),
    PUTB_PTS = sum(PUTB_PTS, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(PUTB_FGA, PUTB_FGM, PUTB_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Player complete-game (CGS) tallies ----
player_putb_cgs <- player_putb_base %>%
  dplyr::group_by(game_id, team_id, shooter_id) %>%
  dplyr::summarise(
    PUTB_FGA_CGS = sum(PUTB_FGA, na.rm = TRUE),
    PUTB_FGM_CGS = sum(PUTB_FGM, na.rm = TRUE),
    PUTB_PTS_CGS = sum(PUTB_PTS, na.rm = TRUE),
    .groups      = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    player_putb_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::left_join(
    player_putb_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        PUTB_FGA_Q1, PUTB_FGA_Q2, PUTB_FGA_Q3, PUTB_FGA_Q4, PUTB_FGA_Q5, PUTB_FGA_Q6, PUTB_FGA_CGS,
        PUTB_FGM_Q1, PUTB_FGM_Q2, PUTB_FGM_Q3, PUTB_FGM_Q4, PUTB_FGM_Q5, PUTB_FGM_Q6, PUTB_FGM_CGS,
        PUTB_PTS_Q1, PUTB_PTS_Q2, PUTB_PTS_Q3, PUTB_PTS_Q4, PUTB_PTS_Q5, PUTB_PTS_Q6, PUTB_PTS_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking at player level
    PUTB_FGA_Q5 = dplyr::if_else(OT_1 == 1L, PUTB_FGA_Q5, 0L),
    PUTB_FGM_Q5 = dplyr::if_else(OT_1 == 1L, PUTB_FGM_Q5, 0L),
    PUTB_PTS_Q5 = dplyr::if_else(OT_1 == 1L, PUTB_PTS_Q5, 0L),
    
    PUTB_FGA_Q6 = dplyr::if_else(OT_2 == 1L, PUTB_FGA_Q6, 0L),
    PUTB_FGM_Q6 = dplyr::if_else(OT_2 == 1L, PUTB_FGM_Q6, 0L),
    PUTB_PTS_Q6 = dplyr::if_else(OT_2 == 1L, PUTB_PTS_Q6, 0L)
  )


# ================================================================
# Player PUTBACK FG% relative to TEAM PUTBACK FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_PUTB_T2P_Q1..Q6 and FG_PCT_PUTB_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_PUTB_T2P_Q1 = ifelse(
      T_PUTB_FGA_Q1 > 0,
      PUTB_FGM_Q1 / T_PUTB_FGA_Q1,
      NA_real_
    ),
    FG_PCT_PUTB_T2P_Q2 = ifelse(
      T_PUTB_FGA_Q2 > 0,
      PUTB_FGM_Q2 / T_PUTB_FGA_Q2,
      NA_real_
    ),
    FG_PCT_PUTB_T2P_Q3 = ifelse(
      T_PUTB_FGA_Q3 > 0,
      PUTB_FGM_Q3 / T_PUTB_FGA_Q3,
      NA_real_
    ),
    FG_PCT_PUTB_T2P_Q4 = ifelse(
      T_PUTB_FGA_Q4 > 0,
      PUTB_FGM_Q4 / T_PUTB_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if game actually had OT
    FG_PCT_PUTB_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_PUTB_FGA_Q5 > 0,
      PUTB_FGM_Q5 / T_PUTB_FGA_Q5,
      NA_real_
    ),
    FG_PCT_PUTB_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_PUTB_FGA_Q6 > 0,
      PUTB_FGM_Q6 / T_PUTB_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_PUTB_T2P_CGS = ifelse(
      T_PUTB_FGA_CGS > 0,
      PUTB_FGM_CGS / T_PUTB_FGA_CGS,
      NA_real_
    )
  )

rm(player_putb_base, player_putb_qtr, player_putb_cgs)



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Putback Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Corner 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ================================================================
# Bring team CNR3_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_cnr3_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_CNR_3PT_FGA_Q1","T_CNR_3PT_FGA_Q2","T_CNR_3PT_FGA_Q3","T_CNR_3PT_FGA_Q4",
    "T_CNR_3PT_FGA_Q5","T_CNR_3PT_FGA_Q6","T_CNR_3PT_FGA_CGS",
    # FGM
    "T_CNR_3PT_FGM_Q1","T_CNR_3PT_FGM_Q2","T_CNR_3PT_FGM_Q3","T_CNR_3PT_FGM_Q4",
    "T_CNR_3PT_FGM_Q5","T_CNR_3PT_FGM_Q6","T_CNR_3PT_FGM_CGS"
  )
)

team_cnr3_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_cnr3_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_cnr3_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )


# ================================================================
# Player Corner 3PT FGA / FGM / PTS by athlete_id_1
# Creates on BaseStats_Player_MC:
#   CNR3_FGA_Q1..Q6 + CNR3_FGA_CGS
#   CNR3_FGM_Q1..Q6 + CNR3_FGM_CGS
#   CNR3_PTS_Q1..Q6 + CNR3_PTS_CGS
# ================================================================

stopifnot("athlete_id_1" %in% names(pm_df))
stopifnot(exists("BaseStats_Player_MC"))

player_cnr3_base <- pm_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shooter_id = as.character(athlete_id_1),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    is_corner3 = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("right corner 3","left corner 3")
    } else {
      stringr::str_detect(type_text, stringr::regex("corner\\s*3", ignore_case = TRUE))
    }
  ) %>%
  dplyr::filter(
    !is.na(shooter_id),
    shooter_id != "",
    is_corner3,
    shot
  ) %>%
  dplyr::transmute(
    game_id, team_id, shooter_id, qtr,
    CNR3_FGA = TRUE,
    CNR3_FGM = make & !is.na(pts) & pts > 0L,
    CNR3_PTS = dplyr::if_else(CNR3_FGM, pts, 0L)
  )

# ---- Player quarter tallies (Q1–Q6) ----
player_cnr3_qtr <- player_cnr3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, shooter_id, qtr) %>%
  dplyr::summarise(
    CNR3_FGA = sum(CNR3_FGA, na.rm = TRUE),
    CNR3_FGM = sum(CNR3_FGM, na.rm = TRUE),
    CNR3_PTS = sum(CNR3_PTS, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(CNR3_FGA, CNR3_FGM, CNR3_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Player complete-game (CGS) tallies ----
player_cnr3_cgs <- player_cnr3_base %>%
  dplyr::group_by(game_id, team_id, shooter_id) %>%
  dplyr::summarise(
    CNR3_FGA_CGS = sum(CNR3_FGA, na.rm = TRUE),
    CNR3_FGM_CGS = sum(CNR3_FGM, na.rm = TRUE),
    CNR3_PTS_CGS = sum(CNR3_PTS, na.rm = TRUE),
    .groups      = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    player_cnr3_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::left_join(
    player_cnr3_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        CNR3_FGA_Q1, CNR3_FGA_Q2, CNR3_FGA_Q3, CNR3_FGA_Q4, CNR3_FGA_Q5, CNR3_FGA_Q6, CNR3_FGA_CGS,
        CNR3_FGM_Q1, CNR3_FGM_Q2, CNR3_FGM_Q3, CNR3_FGM_Q4, CNR3_FGM_Q5, CNR3_FGM_Q6, CNR3_FGM_CGS,
        CNR3_PTS_Q1, CNR3_PTS_Q2, CNR3_PTS_Q3, CNR3_PTS_Q4, CNR3_PTS_Q5, CNR3_PTS_Q6, CNR3_PTS_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking at player level
    CNR3_FGA_Q5 = dplyr::if_else(OT_1 == 1L, CNR3_FGA_Q5, 0L),
    CNR3_FGM_Q5 = dplyr::if_else(OT_1 == 1L, CNR3_FGM_Q5, 0L),
    CNR3_PTS_Q5 = dplyr::if_else(OT_1 == 1L, CNR3_PTS_Q5, 0L),
    
    CNR3_FGA_Q6 = dplyr::if_else(OT_2 == 1L, CNR3_FGA_Q6, 0L),
    CNR3_FGM_Q6 = dplyr::if_else(OT_2 == 1L, CNR3_FGM_Q6, 0L),
    CNR3_PTS_Q6 = dplyr::if_else(OT_2 == 1L, CNR3_PTS_Q6, 0L)
  )

# ================================================================
# Player CORNER-3 FG% relative to TEAM CORNER-3 FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_CNR3_T2P_Q1..Q6 and FG_PCT_CNR3_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_CNR3_T2P_Q1 = ifelse(
      T_CNR_3PT_FGA_Q1 > 0,
      CNR3_FGM_Q1 / T_CNR_3PT_FGA_Q1,
      NA_real_
    ),
    FG_PCT_CNR3_T2P_Q2 = ifelse(
      T_CNR_3PT_FGA_Q2 > 0,
      CNR3_FGM_Q2 / T_CNR_3PT_FGA_Q2,
      NA_real_
    ),
    FG_PCT_CNR3_T2P_Q3 = ifelse(
      T_CNR_3PT_FGA_Q3 > 0,
      CNR3_FGM_Q3 / T_CNR_3PT_FGA_Q3,
      NA_real_
    ),
    FG_PCT_CNR3_T2P_Q4 = ifelse(
      T_CNR_3PT_FGA_Q4 > 0,
      CNR3_FGM_Q4 / T_CNR_3PT_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if the game actually had OT
    FG_PCT_CNR3_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_CNR_3PT_FGA_Q5 > 0,
      CNR3_FGM_Q5 / T_CNR_3PT_FGA_Q5,
      NA_real_
    ),
    FG_PCT_CNR3_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_CNR_3PT_FGA_Q6 > 0,
      CNR3_FGM_Q6 / T_CNR_3PT_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_CNR3_T2P_CGS = ifelse(
      T_CNR_3PT_FGA_CGS > 0,
      CNR3_FGM_CGS / T_CNR_3PT_FGA_CGS,
      NA_real_
    )
  )


rm(player_cnr3_base, player_cnr3_qtr, player_cnr3_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Corner 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Above the Break 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ================================================================
# Bring team ATB3_* onto BaseStats_Player_MC
# via (ESPN_GAME_ID, ESPN_TEAM_ID)
# ================================================================

team_atb3_cols <- intersect(
  names(BaseStats_Team_MC),
  c(
    # FGA
    "T_ATB_3PT_FGA_Q1","T_ATB_3PT_FGA_Q2","T_ATB_3PT_FGA_Q3","T_ATB_3PT_FGA_Q4",
    "T_ATB_3PT_FGA_Q5","T_ATB_3PT_FGA_Q6","T_ATB_3PT_FGA_CGS",
    # FGM
    "T_ATB_3PT_FGM_Q1","T_ATB_3PT_FGM_Q2","T_ATB_3PT_FGM_Q3","T_ATB_3PT_FGM_Q4",
    "T_ATB_3PT_FGM_Q5","T_ATB_3PT_FGM_Q6","T_ATB_3PT_FGM_CGS"
  )
)

team_atb3_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    dplyr::all_of(team_atb3_cols)
  ) %>%
  dplyr::distinct()

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::left_join(
    team_atb3_df,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID")
  )


# ================================================================
# Player Above-the-Break 3PT FGA / FGM / PTS by athlete_id_1
# Creates on BaseStats_Player_MC:
#   ATB3_FGA_Q1..Q6 + ATB3_FGA_CGS
#   ATB3_FGM_Q1..Q6 + ATB3_FGM_CGS
#   ATB3_PTS_Q1..Q6 + ATB3_PTS_CGS
# ================================================================

stopifnot("athlete_id_1" %in% names(pm_df))
stopifnot(exists("BaseStats_Player_MC"))

player_atb3_base <- pm_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shooter_id = as.character(athlete_id_1),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    is_atb3    = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("above the break 3")
    } else {
      stringr::str_detect(
        type_text,
        stringr::regex("above\\s*the\\s*break", ignore_case = TRUE)
      )
    }
  ) %>%
  dplyr::filter(
    !is.na(shooter_id),
    shooter_id != "",
    is_atb3,
    shot
  ) %>%
  dplyr::transmute(
    game_id, team_id, shooter_id, qtr,
    ATB3_FGA = TRUE,
    ATB3_FGM = make & !is.na(pts) & pts > 0L,
    ATB3_PTS = dplyr::if_else(ATB3_FGM, pts, 0L)
  )

# ---- Player quarter tallies (Q1–Q6) ----
player_atb3_qtr <- player_atb3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, shooter_id, qtr) %>%
  dplyr::summarise(
    ATB3_FGA = sum(ATB3_FGA, na.rm = TRUE),
    ATB3_FGM = sum(ATB3_FGM, na.rm = TRUE),
    ATB3_PTS = sum(ATB3_PTS, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(ATB3_FGA, ATB3_FGM, ATB3_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Player complete-game (CGS) tallies ----
player_atb3_cgs <- player_atb3_base %>%
  dplyr::group_by(game_id, team_id, shooter_id) %>%
  dplyr::summarise(
    ATB3_FGA_CGS = sum(ATB3_FGA, na.rm = TRUE),
    ATB3_FGM_CGS = sum(ATB3_FGM, na.rm = TRUE),
    ATB3_PTS_CGS = sum(ATB3_PTS, na.rm = TRUE),
    .groups      = "drop"
  )

# ---- Join into BaseStats_Player_MC and apply OT masking ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    player_atb3_qtr,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::left_join(
    player_atb3_cgs,
    by = c(
      "ESPN_GAME_ID"   = "game_id",
      "ESPN_TEAM_ID"   = "team_id",
      "ESPN_PLAYER_ID" = "shooter_id"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        ATB3_FGA_Q1, ATB3_FGA_Q2, ATB3_FGA_Q3, ATB3_FGA_Q4, ATB3_FGA_Q5, ATB3_FGA_Q6, ATB3_FGA_CGS,
        ATB3_FGM_Q1, ATB3_FGM_Q2, ATB3_FGM_Q3, ATB3_FGM_Q4, ATB3_FGM_Q5, ATB3_FGM_Q6, ATB3_FGM_CGS,
        ATB3_PTS_Q1, ATB3_PTS_Q2, ATB3_PTS_Q3, ATB3_PTS_Q4, ATB3_PTS_Q5, ATB3_PTS_Q6, ATB3_PTS_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking at player level
    ATB3_FGA_Q5 = dplyr::if_else(OT_1 == 1L, ATB3_FGA_Q5, 0L),
    ATB3_FGM_Q5 = dplyr::if_else(OT_1 == 1L, ATB3_FGM_Q5, 0L),
    ATB3_PTS_Q5 = dplyr::if_else(OT_1 == 1L, ATB3_PTS_Q5, 0L),
    
    ATB3_FGA_Q6 = dplyr::if_else(OT_2 == 1L, ATB3_FGA_Q6, 0L),
    ATB3_FGM_Q6 = dplyr::if_else(OT_2 == 1L, ATB3_FGM_Q6, 0L),
    ATB3_PTS_Q6 = dplyr::if_else(OT_2 == 1L, ATB3_PTS_Q6, 0L)
  )

# ================================================================
# Player ATB-3 FG% relative to TEAM ATB-3 FGA (Q1–Q6 + CGS)
# Produces: FG_PCT_ATB3_T2P_Q1..Q6 and FG_PCT_ATB3_T2P_CGS
# ================================================================

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # Q1–Q4 always
    FG_PCT_ATB3_T2P_Q1 = ifelse(
      T_ATB_3PT_FGA_Q1 > 0,
      ATB3_FGM_Q1 / T_ATB_3PT_FGA_Q1,
      NA_real_
    ),
    FG_PCT_ATB3_T2P_Q2 = ifelse(
      T_ATB_3PT_FGA_Q2 > 0,
      ATB3_FGM_Q2 / T_ATB_3PT_FGA_Q2,
      NA_real_
    ),
    FG_PCT_ATB3_T2P_Q3 = ifelse(
      T_ATB_3PT_FGA_Q3 > 0,
      ATB3_FGM_Q3 / T_ATB_3PT_FGA_Q3,
      NA_real_
    ),
    FG_PCT_ATB3_T2P_Q4 = ifelse(
      T_ATB_3PT_FGA_Q4 > 0,
      ATB3_FGM_Q4 / T_ATB_3PT_FGA_Q4,
      NA_real_
    ),
    
    # OT periods only if the game actually had OT
    FG_PCT_ATB3_T2P_Q5 = dplyr::if_else(
      OT_1 == 1L & T_ATB_3PT_FGA_Q5 > 0,
      ATB3_FGM_Q5 / T_ATB_3PT_FGA_Q5,
      NA_real_
    ),
    FG_PCT_ATB3_T2P_Q6 = dplyr::if_else(
      OT_2 == 1L & T_ATB_3PT_FGA_Q6 > 0,
      ATB3_FGM_Q6 / T_ATB_3PT_FGA_Q6,
      NA_real_
    ),
    
    # Complete game
    FG_PCT_ATB3_T2P_CGS = ifelse(
      T_ATB_3PT_FGA_CGS > 0,
      ATB3_FGM_CGS / T_ATB_3PT_FGA_CGS,
      NA_real_
    )
  )


rm(player_atb3_base, player_atb3_qtr, player_atb3_cgs)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Above the Break 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Shot Clock Tracking Distance FG Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Shot Clock Tracking (CGS only, PLAYER level)                                       #
# Uses: shot-clock tracked CSV                                                       #
# Adds cols to BaseStats_Player_MC                                                   #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID):
#   SHOT_CLK_EE_FGA_CGS,  SHOT_CLK_EE_FGM_CGS,  SHOT_CLK_EE_FG_PCT_CGS
#   SHOT_CLK_VE_FGA_CGS,  SHOT_CLK_VE_FGM_CGS,  SHOT_CLK_VE_FG_PCT_CGS
#   SHOT_CLK_E_FGA_CGS,   SHOT_CLK_E_FGM_CGS,   SHOT_CLK_E_FG_PCT_CGS
#   SHOT_CLK_AVG_FGA_CGS, SHOT_CLK_AVG_FGM_CGS, SHOT_CLK_AVG_FG_PCT_CGS
#   SHOT_CLK_L_FGA_CGS,   SHOT_CLK_L_FGM_CGS,   SHOT_CLK_L_FG_PCT_CGS
#   SHOT_CLK_VL_FGA_CGS,  SHOT_CLK_VL_FGM_CGS,  SHOT_CLK_VL_FG_PCT_CGS
# Notes:
# - "Extremely Early" = rows with shot_clock_high == 24 (NOT designation).
# - We KEEP blank shot_clock_designation rows.
# - Join on (ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID) as character.
#====================================================================================#

stopifnot(exists("BaseStats_Player_MC"))
stopifnot(is.data.frame(pm_df))

.safe_pct <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# -- 1) Load file (reuse sc_raw if present) -----------------------------------------
sc_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_shotclocktracked_",
  season_token, ".csv")
)

if (!exists("sc_raw") || !is.data.frame(sc_raw)) {
  if (!file.exists(sc_path)) stop("Shot-clock file not found: ", sc_path)
  sc_raw <- read.csv(sc_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) Normalize (NO filtering by designation; keep blanks) ------------------------
if (!"espn_game_id"   %in% names(sc_raw) && "game_id"   %in% names(sc_raw)) sc_raw$espn_game_id   <- sc_raw$game_id
if (!"espn_team_id"   %in% names(sc_raw) && "team_id"   %in% names(sc_raw)) sc_raw$espn_team_id   <- sc_raw$team_id
if (!"espn_player_id" %in% names(sc_raw) && "player_id" %in% names(sc_raw)) sc_raw$espn_player_id <- sc_raw$player_id

num_safely <- function(x) suppressWarnings(as.numeric(x))

sc_raw <- sc_raw |>
  dplyr::mutate(
    espn_game_id   = as.character(espn_game_id),
    espn_team_id   = as.character(espn_team_id),
    espn_player_id = as.character(espn_player_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM),
    shot_clock_high        = num_safely(shot_clock_high),
    shot_clock_designation = trimws(as.character(shot_clock_designation))
  )
# IMPORTANT: no filter here; blanks are preserved so EE via high==24 is not lost.

# -- 3) Robust ESPN ID detection from pm_df ----------------------------------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID",
            "nba_game_id","NBA_GAME_ID","game_id_alt")

.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  ids <- as.character(df[[col]])
  ids[!is.na(ids) & nzchar(ids)]
}

sc_ids_all <- unique(as.character(sc_raw$espn_game_id))

overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df)) {
    dplyr::filter(pm_df, isTRUE(shooting_play))
  } else {
    pm_df
  }
  ids <- .get_ids(pmf, cn)
  list(col = cn, ids = unique(ids), inter = length(intersect(ids, sc_ids_all)))
})

best_idx <- which.max(vapply(overlaps, `[[`, numeric(1), "inter"))
best     <- overlaps[[best_idx]]

message(sprintf("ShotClock CGS (PLAYER): file games=%s | pm_df id col=%s | intersection=%s",
                length(sc_ids_all), best$col, best$inter))

pm_ids <- if (length(best$ids)) unique(best$ids) else character()

sc_use <- if (length(pm_ids) && best$inter > 0) {
  dplyr::filter(sc_raw, espn_game_id %in% pm_ids)
} else {
  message("ShotClock CGS (PLAYER): No ID overlap—using all shot-clock rows as fallback.")
  sc_raw
}

# -- 4) Aggregate (CGS per game/team/player) ----------------------------------------
sc_player_agg <- sc_use |>
  dplyr::group_by(espn_game_id, espn_team_id, espn_player_id) |>
  dplyr::summarise(
    # Extremely Early: strictly by shot_clock_high == 24
    SHOT_CLK_EE_FGA_CGS  = sum(ifelse(shot_clock_high == 24, FGA, 0), na.rm = TRUE),
    SHOT_CLK_EE_FGM_CGS  = sum(ifelse(shot_clock_high == 24, FGM, 0), na.rm = TRUE),
    
    # Others by designation (blanks won't count toward any)
    SHOT_CLK_VE_FGA_CGS  = sum(ifelse(shot_clock_designation == "Very Early", FGA, 0), na.rm = TRUE),
    SHOT_CLK_VE_FGM_CGS  = sum(ifelse(shot_clock_designation == "Very Early", FGM, 0), na.rm = TRUE),
    
    SHOT_CLK_E_FGA_CGS   = sum(ifelse(shot_clock_designation == "Early", FGA, 0), na.rm = TRUE),
    SHOT_CLK_E_FGM_CGS   = sum(ifelse(shot_clock_designation == "Early", FGM, 0), na.rm = TRUE),
    
    SHOT_CLK_AVG_FGA_CGS = sum(ifelse(shot_clock_designation == "Average", FGA, 0), na.rm = TRUE),
    SHOT_CLK_AVG_FGM_CGS = sum(ifelse(shot_clock_designation == "Average", FGM, 0), na.rm = TRUE),
    
    SHOT_CLK_L_FGA_CGS   = sum(ifelse(shot_clock_designation == "Late", FGA, 0), na.rm = TRUE),
    SHOT_CLK_L_FGM_CGS   = sum(ifelse(shot_clock_designation == "Late", FGM, 0), na.rm = TRUE),
    
    SHOT_CLK_VL_FGA_CGS  = sum(ifelse(shot_clock_designation == "Very Late", FGA, 0), na.rm = TRUE),
    SHOT_CLK_VL_FGM_CGS  = sum(ifelse(shot_clock_designation == "Very Late", FGM, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    SHOT_CLK_EE_FG_PCT_CGS  = .safe_pct(SHOT_CLK_EE_FGM_CGS,  SHOT_CLK_EE_FGA_CGS),
    SHOT_CLK_VE_FG_PCT_CGS  = .safe_pct(SHOT_CLK_VE_FGM_CGS,  SHOT_CLK_VE_FGA_CGS),
    SHOT_CLK_E_FG_PCT_CGS   = .safe_pct(SHOT_CLK_E_FGM_CGS,   SHOT_CLK_E_FGA_CGS),
    SHOT_CLK_AVG_FG_PCT_CGS = .safe_pct(SHOT_CLK_AVG_FGM_CGS, SHOT_CLK_AVG_FGA_CGS),
    SHOT_CLK_L_FG_PCT_CGS   = .safe_pct(SHOT_CLK_L_FGM_CGS,   SHOT_CLK_L_FGA_CGS),
    SHOT_CLK_VL_FG_PCT_CGS  = .safe_pct(SHOT_CLK_VL_FGM_CGS,  SHOT_CLK_VL_FGA_CGS)
  ) |>
  dplyr::rename(
    ESPN_GAME_ID   = espn_game_id,
    ESPN_TEAM_ID   = espn_team_id,
    ESPN_PLAYER_ID = espn_player_id
  ) |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  )

# -- 5) Join into BaseStats_Player_MC -----------------------------------------------
.req_cols <- c("ESPN_GAME_ID", "ESPN_TEAM_ID", "ESPN_PLAYER_ID")
.miss_bsp <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Player_MC)))
if (length(.miss_bsp)) {
  stop("BaseStats_Player_MC missing join keys: ", paste(.miss_bsp, collapse = ", "))
}

BaseStats_Player_MC <- BaseStats_Player_MC |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) |>
  dplyr::left_join(
    sc_player_agg,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","ESPN_PLAYER_ID")
  )

# Fill NA counts with 0 (leave *_PCT_* as NA when denom==0)
count_cols <- c(
  "SHOT_CLK_EE_FGA_CGS","SHOT_CLK_EE_FGM_CGS",
  "SHOT_CLK_VE_FGA_CGS","SHOT_CLK_VE_FGM_CGS",
  "SHOT_CLK_E_FGA_CGS","SHOT_CLK_E_FGM_CGS",
  "SHOT_CLK_AVG_FGA_CGS","SHOT_CLK_AVG_FGM_CGS",
  "SHOT_CLK_L_FGA_CGS","SHOT_CLK_L_FGM_CGS",
  "SHOT_CLK_VL_FGA_CGS","SHOT_CLK_VL_FGM_CGS"
)
for (cc in intersect(count_cols, names(BaseStats_Player_MC))) {
  BaseStats_Player_MC[[cc]] <- ifelse(is.na(BaseStats_Player_MC[[cc]]), 0, BaseStats_Player_MC[[cc]])
}

rm(best, overlaps, sc_ids_all, pm_ids, sc_use, sc_player_agg)
message("Shot Clock Tracking (CGS, PLAYER level) columns added to BaseStats_Player_MC (EE uses high==24; blanks kept).")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Shot Clock Tracking Distance FG Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT & 3PT Shot Clock Tracking Distance Field Goal Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Shot Clock — 2PT / 3PT Splits (CGS only, PLAYER level)                            #
# Uses: shot-clock tracked CSV                                                      #
# Adds cols to BaseStats_Player_MC                                                  #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID), for buckets:
#   EE, VE, E, AVG, L, VL
#   SHOT_CLK_<bucket>_2PT_FGM_CGS / FGA_CGS
#   SHOT_CLK_<bucket>_3PT_FGM_CGS / FGA_CGS
#====================================================================================#

stopifnot(exists("BaseStats_Player_MC"))
stopifnot(is.data.frame(pm_df))

# -- 1) Load shot-clock file (re-use sc_raw if it exists) ---------------------------
sc_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_shotclocktracked_",
  season_token, ".csv")
)

if (!exists("sc_raw") || !is.data.frame(sc_raw)) {
  if (!file.exists(sc_path)) stop("Shot-clock file not found: ", sc_path)
  sc_raw <- read.csv(sc_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) Normalize (keep blanks) -----------------------------------------------------
if (!"espn_game_id"   %in% names(sc_raw) && "game_id"   %in% names(sc_raw)) sc_raw$espn_game_id   <- sc_raw$game_id
if (!"espn_team_id"   %in% names(sc_raw) && "team_id"   %in% names(sc_raw)) sc_raw$espn_team_id   <- sc_raw$team_id
if (!"espn_player_id" %in% names(sc_raw) && "player_id" %in% names(sc_raw)) sc_raw$espn_player_id <- sc_raw$player_id

num_safely <- function(x) suppressWarnings(as.numeric(x))

sc_raw <- sc_raw |>
  dplyr::mutate(
    espn_game_id   = as.character(espn_game_id),
    espn_team_id   = as.character(espn_team_id),
    espn_player_id = as.character(espn_player_id),
    FGA  = num_safely(FGA),
    FGM  = num_safely(FGM),
    FG2M = if ("FG2M" %in% names(sc_raw)) num_safely(FG2M) else NA_real_,
    FG2A = if ("FG2A" %in% names(sc_raw)) num_safely(FG2A) else NA_real_,
    FG3M = if ("FG3M" %in% names(sc_raw)) num_safely(FG3M) else
      if ("FGM3" %in% names(sc_raw)) num_safely(FGM3) else NA_real_,
    FG3A = if ("FG3A" %in% names(sc_raw)) num_safely(FG3A) else
      if ("FGA3" %in% names(sc_raw)) num_safely(FGA3) else NA_real_,
    shot_clock_high        = num_safely(shot_clock_high),
    shot_clock_designation = trimws(as.character(shot_clock_designation))
  )

# Fallback for FG2 if missing
if (any(is.na(sc_raw$FG2M))) {
  sc_raw$FG2M <- ifelse(
    is.na(sc_raw$FG2M),
    pmax(sc_raw$FGM - sc_raw$FG3M, 0),
    sc_raw$FG2M
  )
}
if (any(is.na(sc_raw$FG2A))) {
  sc_raw$FG2A <- ifelse(
    is.na(sc_raw$FG2A),
    pmax(sc_raw$FGA - sc_raw$FG3A, 0),
    sc_raw$FG2A
  )
}

# -- 3) Robust ESPN GAME ID detection from pm_df -----------------------------------
.cands   <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID",
              "nba_game_id","NBA_GAME_ID","game_id_alt")
.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  x <- as.character(df[[col]])
  x[!is.na(x) & nzchar(x)]
}

sc_ids_all <- unique(as.character(sc_raw$espn_game_id))

overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df))
    dplyr::filter(pm_df, isTRUE(shooting_play))
  else
    pm_df
  ids <- .get_ids(pmf, cn)
  list(col = cn, ids = unique(ids), inter = length(intersect(ids, sc_ids_all)))
})

best_idx <- which.max(vapply(overlaps, `[[`, numeric(1), "inter"))
best     <- overlaps[[best_idx]]

message(sprintf("ShotClk 2/3PT CGS (PLAYER): file games=%s | pm_df id col=%s | intersection=%s",
                length(sc_ids_all), best$col, best$inter))

pm_ids <- if (length(best$ids)) unique(best$ids) else character()
sc_use <- if (length(pm_ids) && best$inter > 0) {
  dplyr::filter(sc_raw, espn_game_id %in% pm_ids)
} else {
  message("No ID overlap—using all shot-clock rows.")
  sc_raw
}

# -- 4) Aggregate 2PT/3PT by shot-clock bucket *per player* -------------------------
sc_player_agg <- sc_use |>
  dplyr::group_by(espn_game_id, espn_team_id, espn_player_id) |>
  dplyr::summarise(
    # Extremely Early: shot_clock_high == 24
    SHOT_CLK_EE_2PT_FGM_CGS = sum(ifelse(shot_clock_high == 24, FG2M, 0), na.rm = TRUE),
    SHOT_CLK_EE_2PT_FGA_CGS = sum(ifelse(shot_clock_high == 24, FG2A, 0), na.rm = TRUE),
    SHOT_CLK_EE_3PT_FGM_CGS = sum(ifelse(shot_clock_high == 24, FG3M, 0), na.rm = TRUE),
    SHOT_CLK_EE_3PT_FGA_CGS = sum(ifelse(shot_clock_high == 24, FG3A, 0), na.rm = TRUE),
    
    # Very Early (designation)
    SHOT_CLK_VE_2PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Very Early", FG2M, 0), na.rm = TRUE),
    SHOT_CLK_VE_2PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Very Early", FG2A, 0), na.rm = TRUE),
    SHOT_CLK_VE_3PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Very Early", FG3M, 0), na.rm = TRUE),
    SHOT_CLK_VE_3PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Very Early", FG3A, 0), na.rm = TRUE),
    
    # Early
    SHOT_CLK_E_2PT_FGM_CGS  = sum(ifelse(shot_clock_designation == "Early", FG2M, 0), na.rm = TRUE),
    SHOT_CLK_E_2PT_FGA_CGS  = sum(ifelse(shot_clock_designation == "Early", FG2A, 0), na.rm = TRUE),
    SHOT_CLK_E_3PT_FGM_CGS  = sum(ifelse(shot_clock_designation == "Early", FG3M, 0), na.rm = TRUE),
    SHOT_CLK_E_3PT_FGA_CGS  = sum(ifelse(shot_clock_designation == "Early", FG3A, 0), na.rm = TRUE),
    
    # Average
    SHOT_CLK_AVG_2PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Average", FG2M, 0), na.rm = TRUE),
    SHOT_CLK_AVG_2PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Average", FG2A, 0), na.rm = TRUE),
    SHOT_CLK_AVG_3PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Average", FG3M, 0), na.rm = TRUE),
    SHOT_CLK_AVG_3PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Average", FG3A, 0), na.rm = TRUE),
    
    # Late
    SHOT_CLK_L_2PT_FGM_CGS  = sum(ifelse(shot_clock_designation == "Late", FG2M, 0), na.rm = TRUE),
    SHOT_CLK_L_2PT_FGA_CGS  = sum(ifelse(shot_clock_designation == "Late", FG2A, 0), na.rm = TRUE),
    SHOT_CLK_L_3PT_FGM_CGS  = sum(ifelse(shot_clock_designation == "Late", FG3M, 0), na.rm = TRUE),
    SHOT_CLK_L_3PT_FGA_CGS  = sum(ifelse(shot_clock_designation == "Late", FG3A, 0), na.rm = TRUE),
    
    # Very Late
    SHOT_CLK_VL_2PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Very Late", FG2M, 0), na.rm = TRUE),
    SHOT_CLK_VL_2PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Very Late", FG2A, 0), na.rm = TRUE),
    SHOT_CLK_VL_3PT_FGM_CGS = sum(ifelse(shot_clock_designation == "Very Late", FG3M, 0), na.rm = TRUE),
    SHOT_CLK_VL_3PT_FGA_CGS = sum(ifelse(shot_clock_designation == "Very Late", FG3A, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(
    ESPN_GAME_ID   = espn_game_id,
    ESPN_TEAM_ID   = espn_team_id,
    ESPN_PLAYER_ID = espn_player_id
  ) |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  )

# -- 5) Join into BaseStats_Player_MC; fill NA counts with 0 ------------------------
.req_cols <- c("ESPN_GAME_ID","ESPN_TEAM_ID","ESPN_PLAYER_ID")
.miss_bsp <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Player_MC)))
if (length(.miss_bsp)) {
  stop("BaseStats_Player_MC missing join keys: ", paste(.miss_bsp, collapse = ", "))
}

BaseStats_Player_MC <- BaseStats_Player_MC |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) |>
  dplyr::left_join(
    sc_player_agg,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","ESPN_PLAYER_ID")
  )

sc_counts <- grep("^SHOT_CLK_.*_(2PT|3PT)_FG[MA]_CGS$", names(BaseStats_Player_MC), value = TRUE)
for (cc in sc_counts) {
  BaseStats_Player_MC[[cc]] <- ifelse(is.na(BaseStats_Player_MC[[cc]]), 0, BaseStats_Player_MC[[cc]])
}

rm(best, overlaps, sc_ids_all, pm_ids, sc_use, sc_player_agg)
message("Shot Clock (2PT/3PT CGS, PLAYER level) columns added to BaseStats_Player_MC.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT & 3PT Shot Clock Tracking Distance Field Goal Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Defender Distance FG Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Closest Defender Distance (CGS only, PLAYER level)                                  #
# Uses: closest-defender tracked CSV                                                  #
# Adds cols to BaseStats_Player_MC                                                    #
#------------------------------------------------------------------------------------#
# New columns created (per ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID):
#   DEF_DIST_VT_FGA_CGS, DEF_DIST_VT_FGM_CGS
#   DEF_DIST_T_FGA_CGS,  DEF_DIST_T_FGM_CGS
#   DEF_DIST_O_FGA_CGS,  DEF_DIST_O_FGM_CGS
#   DEF_DIST_WO_FGA_CGS, DEF_DIST_WO_FGM_CGS
#   DEF_DIST_T_FG_PCT_CGS   # (Very Tight + Tight) FG%
#   DEF_DIST_O_FG_PCT_CGS   # (Open + Wide Open) FG%
# Notes:
# - Joins on (ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID) as character.
# - Percentages are NA when denominator == 0.
#====================================================================================#

stopifnot(exists("BaseStats_Player_MC"))

.safe_pct <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# -- 1) Locate & load defender-tracked file (reuse def_raw if present) --------------
def_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_closetdefendertracked_",
  season_token, ".csv")
)

if (!exists("def_raw") || !is.data.frame(def_raw)) {
  if (!file.exists(def_path)) {
    stop("Closest-defender file not found at: ", def_path)
  }
  def_raw <- read.csv(def_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) ID normalization & basic hygiene -------------------------------------------
# Expect: espn_game_id, espn_team_id, espn_player_id, defender_range_designation, FGA, FGM
if (!"espn_game_id"   %in% names(def_raw) && "game_id"   %in% names(def_raw)) {
  def_raw$espn_game_id <- def_raw$game_id
}
if (!"espn_team_id"   %in% names(def_raw) && "team_id"   %in% names(def_raw)) {
  def_raw$espn_team_id <- def_raw$team_id
}
if (!"espn_player_id" %in% names(def_raw) && "player_id" %in% names(def_raw)) {
  def_raw$espn_player_id <- def_raw$player_id
}

num_safely <- function(x) suppressWarnings(as.numeric(x))

def_raw <- def_raw %>%
  dplyr::mutate(
    espn_game_id   = as.character(espn_game_id),
    espn_team_id   = as.character(espn_team_id),
    espn_player_id = as.character(espn_player_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM)
  )

# Keep only the four known buckets
valid_designations <- c("Very Tight", "Tight", "Open", "Wide Open")
def_raw <- def_raw %>%
  dplyr::filter(defender_range_designation %in% valid_designations)

# -- 3) Determine the game set from pm_df (robust ESPN ID detection) ---------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID",
            "nba_game_id","NBA_GAME_ID","game_id_alt")

.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  ids <- as.character(df[[col]])
  ids[!is.na(ids) & nzchar(ids)]
}

def_ids <- unique(as.character(def_raw$espn_game_id))

overlaps <- lapply(.cands, function(cn) {
  pm_filtered <- if ("shooting_play" %in% names(pm_df)) {
    dplyr::filter(pm_df, isTRUE(shooting_play))
  } else pm_df
  
  ids   <- .get_ids(pm_filtered, cn)
  inter <- length(intersect(ids, def_ids))
  list(col = cn, ids = unique(ids), inter = inter)
})

best_idx <- which.max(vapply(overlaps, `[[`, numeric(1), "inter"))
best     <- overlaps[[best_idx]]

message(sprintf("ClosestDef CGS (PLAYER): defender games=%s | pm_df id col=%s | intersection=%s",
                length(def_ids), best$col, best$inter))

pm_ids <- if (length(best$ids)) unique(best$ids) else character()

def_use <- if (length(pm_ids) && best$inter > 0) {
  dplyr::filter(def_raw, espn_game_id %in% pm_ids)
} else {
  message("ClosestDef CGS (PLAYER): No ID overlap—using all defender rows as fallback.")
  def_raw
}

# -- 4) Aggregate to CGS per (game, team, player) -----------------------------------
def_player_agg <- def_use %>%
  dplyr::group_by(espn_game_id, espn_team_id, espn_player_id) %>%
  dplyr::summarise(
    # Very Tight (0–2 ft)
    DEF_DIST_VT_FGA_CGS = sum(ifelse(defender_range_designation == "Very Tight", FGA, 0), na.rm = TRUE),
    DEF_DIST_VT_FGM_CGS = sum(ifelse(defender_range_designation == "Very Tight", FGM, 0), na.rm = TRUE),
    
    # Tight (2–4 ft)
    DEF_DIST_T_FGA_CGS  = sum(ifelse(defender_range_designation == "Tight", FGA, 0), na.rm = TRUE),
    DEF_DIST_T_FGM_CGS  = sum(ifelse(defender_range_designation == "Tight", FGM, 0), na.rm = TRUE),
    
    # Open (4–6 ft)
    DEF_DIST_O_FGA_CGS  = sum(ifelse(defender_range_designation == "Open", FGA, 0), na.rm = TRUE),
    DEF_DIST_O_FGM_CGS  = sum(ifelse(defender_range_designation == "Open", FGM, 0), na.rm = TRUE),
    
    # Wide Open (6+ ft)
    DEF_DIST_WO_FGA_CGS = sum(ifelse(defender_range_designation == "Wide Open", FGA, 0), na.rm = TRUE),
    DEF_DIST_WO_FGM_CGS = sum(ifelse(defender_range_designation == "Wide Open", FGM, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    # Percentages (combined buckets)
    DEF_DIST_T_FG_PCT_CGS = .safe_pct(
      DEF_DIST_VT_FGM_CGS + DEF_DIST_T_FGM_CGS,
      DEF_DIST_VT_FGA_CGS + DEF_DIST_T_FGA_CGS
    ),
    DEF_DIST_O_FG_PCT_CGS = .safe_pct(
      DEF_DIST_O_FGM_CGS + DEF_DIST_WO_FGM_CGS,
      DEF_DIST_O_FGA_CGS + DEF_DIST_WO_FGA_CGS
    )
  ) %>%
  dplyr::rename(
    ESPN_GAME_ID   = espn_game_id,
    ESPN_TEAM_ID   = espn_team_id,
    ESPN_PLAYER_ID = espn_player_id
  ) %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  )

# -- 5) Join back into BaseStats_Player_MC ------------------------------------------
.req_cols <- c("ESPN_GAME_ID", "ESPN_TEAM_ID", "ESPN_PLAYER_ID")
.miss_bsp <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Player_MC)))
if (length(.miss_bsp)) {
  stop("BaseStats_Player_MC is missing required join keys: ", paste(.miss_bsp, collapse = ", "))
}

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    def_player_agg,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID", "ESPN_PLAYER_ID")
  )

# Replace NA counts with 0 (percentages remain NA)
count_cols <- c(
  "DEF_DIST_VT_FGA_CGS","DEF_DIST_VT_FGM_CGS",
  "DEF_DIST_T_FGA_CGS","DEF_DIST_T_FGM_CGS",
  "DEF_DIST_O_FGA_CGS","DEF_DIST_O_FGM_CGS",
  "DEF_DIST_WO_FGA_CGS","DEF_DIST_WO_FGM_CGS"
)
for (cc in intersect(count_cols, names(BaseStats_Player_MC))) {
  BaseStats_Player_MC[[cc]] <- ifelse(is.na(BaseStats_Player_MC[[cc]]), 0, BaseStats_Player_MC[[cc]])
}

rm(best, def_ids, pm_ids, overlaps, def_use, def_player_agg)
message("Closest Defender Distance (CGS, PLAYER level) columns added to BaseStats_Player_MC.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Defender Distance FG Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT & 3PT Defender Distance Field Goal Data Aggregation Section (Player) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Closest Defender Distance — 2PT / 3PT Splits (CGS only, PLAYER level)             #
# Uses: pm_df, closest-defender tracked CSV                                         #
# Adds cols to BaseStats_Player_MC                                                  #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID), for buckets:
#   DEF_DIST_<VT/T/O/WO>_2PT_FGM_CGS / DEF_DIST_<VT/T/O/WO>_2PT_FGA_CGS
#   DEF_DIST_<VT/T/O/WO>_3PT_FGM_CGS / DEF_DIST_<VT/T/O/WO>_3PT_FGA_CGS
# Joins on (ESPN_GAME_ID, ESPN_TEAM_ID, ESPN_PLAYER_ID) as character.
#====================================================================================#

stopifnot(exists("BaseStats_Player_MC"))
stopifnot(is.data.frame(pm_df))

# -- 1) Load defender-distance file (re-use def_raw if it exists) -------------------
def_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_closetdefendertracked_",
  season_token, ".csv")
)

if (!exists("def_raw") || !is.data.frame(def_raw)) {
  if (!file.exists(def_path)) stop("Closest-defender file not found at: ", def_path)
  def_raw <- read.csv(def_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) Normalize columns & hygiene -------------------------------------------------
if (!"espn_game_id"  %in% names(def_raw) && "game_id"  %in% names(def_raw)) def_raw$espn_game_id  <- def_raw$game_id
if (!"espn_team_id"  %in% names(def_raw) && "team_id"  %in% names(def_raw)) def_raw$espn_team_id  <- def_raw$team_id
if (!"espn_player_id"%in% names(def_raw) && "player_id"%in% names(def_raw)) def_raw$espn_player_id <- def_raw$player_id

num_safely <- function(x) suppressWarnings(as.numeric(x))

def_raw <- def_raw |>
  dplyr::mutate(
    espn_game_id   = as.character(espn_game_id),
    espn_team_id   = as.character(espn_team_id),
    espn_player_id = as.character(espn_player_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM),
    FG2M = if ("FG2M" %in% names(def_raw)) num_safely(FG2M) else NA_real_,
    FG2A = if ("FG2A" %in% names(def_raw)) num_safely(FG2A) else NA_real_,
    FG3M = if ("FG3M" %in% names(def_raw)) num_safely(FG3M) else
      if ("FGM3" %in% names(def_raw)) num_safely(FGM3) else NA_real_,
    FG3A = if ("FG3A" %in% names(def_raw)) num_safely(FG3A) else
      if ("FGA3" %in% names(def_raw)) num_safely(FGA3) else NA_real_,
    defender_range_designation = as.character(defender_range_designation)
  )

# Fallback for FG2M/FG2A if missing
if (any(is.na(def_raw$FG2M))) {
  def_raw$FG2M <- ifelse(
    is.na(def_raw$FG2M),
    pmax(def_raw$FGM - def_raw$FG3M, 0),
    def_raw$FG2M
  )
}
if (any(is.na(def_raw$FG2A))) {
  def_raw$FG2A <- ifelse(
    is.na(def_raw$FG2A),
    pmax(def_raw$FGA - def_raw$FG3A, 0),
    def_raw$FG2A
  )
}

# Keep only valid distance buckets
valid_dd <- c("Very Tight","Tight","Open","Wide Open")
def_raw <- def_raw |> dplyr::filter(defender_range_designation %in% valid_dd)

# -- 3) Robust ESPN GAME ID detection from pm_df -----------------------------------
.cands  <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID",
             "nba_game_id","NBA_GAME_ID","game_id_alt")
.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  x <- as.character(df[[col]])
  x[!is.na(x) & nzchar(x)]
}

def_ids <- unique(as.character(def_raw$espn_game_id))

overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df))
    dplyr::filter(pm_df, isTRUE(shooting_play))
  else
    pm_df
  ids <- .get_ids(pmf, cn)
  list(col = cn, ids = unique(ids), inter = length(intersect(ids, def_ids)))
})

best_idx <- which.max(vapply(overlaps, `[[`, numeric(1), "inter"))
best     <- overlaps[[best_idx]]

message(sprintf(
  "DefDist 2/3PT CGS (PLAYER): file games=%s | pm_df id col=%s | intersection=%s",
  length(def_ids), best$col, best$inter
))

pm_ids  <- if (length(best$ids)) unique(best$ids) else character()
def_use <- if (length(pm_ids) && best$inter > 0) {
  dplyr::filter(def_raw, espn_game_id %in% pm_ids)
} else {
  message("No ID overlap—using all defender-distance rows.")
  def_raw
}

# -- 4) Aggregate 2PT/3PT by bucket *per player* ------------------------------------
dd_player_agg <- def_use |>
  dplyr::group_by(espn_game_id, espn_team_id, espn_player_id) |>
  dplyr::summarise(
    # Very Tight
    DEF_DIST_VT_2PT_FGM_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG2M, 0), na.rm=TRUE),
    DEF_DIST_VT_2PT_FGA_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG2A, 0), na.rm=TRUE),
    DEF_DIST_VT_3PT_FGM_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG3M, 0), na.rm=TRUE),
    DEF_DIST_VT_3PT_FGA_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG3A, 0), na.rm=TRUE),
    
    # Tight
    DEF_DIST_T_2PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Tight", FG2M, 0), na.rm=TRUE),
    DEF_DIST_T_2PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Tight", FG2A, 0), na.rm=TRUE),
    DEF_DIST_T_3PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Tight", FG3M, 0), na.rm=TRUE),
    DEF_DIST_T_3PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Tight", FG3A, 0), na.rm=TRUE),
    
    # Open
    DEF_DIST_O_2PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Open", FG2M, 0), na.rm=TRUE),
    DEF_DIST_O_2PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Open", FG2A, 0), na.rm=TRUE),
    DEF_DIST_O_3PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Open", FG3M, 0), na.rm=TRUE),
    DEF_DIST_O_3PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Open", FG3A, 0), na.rm=TRUE),
    
    # Wide Open
    DEF_DIST_WO_2PT_FGM_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG2M, 0), na.rm=TRUE),
    DEF_DIST_WO_2PT_FGA_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG2A, 0), na.rm=TRUE),
    DEF_DIST_WO_3PT_FGM_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG3M, 0), na.rm=TRUE),
    DEF_DIST_WO_3PT_FGA_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG3A, 0), na.rm=TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(
    ESPN_GAME_ID   = espn_game_id,
    ESPN_TEAM_ID   = espn_team_id,
    ESPN_PLAYER_ID = espn_player_id
  ) |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  )

# -- 5) Join into BaseStats_Player_MC; fill NA counts with 0 ------------------------
.req_cols <- c("ESPN_GAME_ID","ESPN_TEAM_ID","ESPN_PLAYER_ID")
.miss_bsp <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Player_MC)))
if (length(.miss_bsp)) {
  stop("BaseStats_Player_MC missing join keys: ", paste(.miss_bsp, collapse = ", "))
}

BaseStats_Player_MC <- BaseStats_Player_MC |>
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) |>
  dplyr::left_join(
    dd_player_agg,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","ESPN_PLAYER_ID")
  )

dd_counts <- grep("^DEF_DIST_.*_(2PT|3PT)_FG[MA]_CGS$", names(BaseStats_Player_MC), value = TRUE)
for (cc in dd_counts) {
  BaseStats_Player_MC[[cc]] <- ifelse(is.na(BaseStats_Player_MC[[cc]]), 0, BaseStats_Player_MC[[cc]])
}

rm(best, def_ids, overlaps, pm_ids, def_use, dd_player_agg)
message("Defender Distance (2PT/3PT CGS, PLAYER level) columns added to BaseStats_Player_MC.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT & 3PT Defender Distance Field Goal Data Aggregation Section (Player) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# === START: TOP-LEVEL Detriment Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#    oooooooooo.                 .             o8o                                              .   
#     `888'   `Y8b              .o8             `"'                                            .o8   
#      888      888  .ooooo.  .o888oo oooo d8b oooo  ooo. .oo.  .oo.    .ooooo.  ooo. .oo.   .o888oo 
#      888      888 d88' `88b   888   `888""8P `888  `888P"Y88bP"Y88b  d88' `88b `888P"Y88b    888   
#      888      888 888ooo888   888    888      888   888   888   888  888ooo888  888   888    888   
#      888     d88' 888    .o   888 .  888      888   888   888   888  888    .o  888   888    888 . 
#     o888bood8P'   `Y8bod8P'   "888" d888b    o888o o888o o888o o888o `Y8bod8P' o888o o888o   "888" 
#                                                                                               
#                                                                                               
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀  



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Turnovers Data Aggregation Section (PLAYER, athlete_id_1) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id","team_id","qtr","type_text","athlete_id_1") %in% names(pm_df)))
stopifnot(exists("BaseStats_Player_MC"))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Common base (Q1–Q6), *committing* player/team via athlete_id_1 ----------------
tov_base_player <- pm_df %>%
  dplyr::mutate(
    qtr          = to_int(qtr),
    team_id      = as.character(team_id),
    athlete_id_1 = as.character(athlete_id_1),
    txt          = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(athlete_id_1),
    nzchar(athlete_id_1)
  )

# Patterns (case-insensitive)
pat_liveb <- stringr::regex(
  "lost ball turnover|bad pass turnover|offensive foul turnover",
  ignore_case = TRUE
)
pat_badp  <- stringr::regex("bad pass turnover", ignore_case = TRUE)
pat_any   <- stringr::regex("turnover", ignore_case = TRUE)

# ---- Separate populations ----------------------------------------------------------
tov_liveb_df <- tov_base_player %>%
  dplyr::filter(stringr::str_detect(txt, pat_liveb))

tov_all_df <- tov_base_player %>%
  dplyr::filter(stringr::str_detect(txt, pat_any))

tov_deadb_df <- tov_base_player %>%
  dplyr::filter(stringr::str_detect(txt, pat_any) &
                  !stringr::str_detect(txt, pat_liveb))

tov_badp_df <- tov_base_player %>%
  dplyr::filter(stringr::str_detect(txt, pat_badp))

# ---- Helper: tally by quarter + CGS for (game, team, player) -----------------------
tally_tov_player <- function(df, name_prefix) {
  # Q1–Q6
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1
    ) %>%
    dplyr::rename_with(
      ~ paste0(name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1,
      !!paste0(name_prefix, "_CGS") := CGS
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build pieces (player-level)
res_liveb_p <- tally_tov_player(tov_liveb_df, "TOV_LIVEB")
res_deadb_p <- tally_tov_player(tov_deadb_df, "TOV_DEADB")
res_badp_p  <- tally_tov_player(tov_badp_df,  "TOV_BADP")
res_all_p   <- tally_tov_player(tov_all_df,   "TOV")

# ---- Join into BaseStats_Player_MC -------------------------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  # live-ball
  dplyr::left_join(
    res_liveb_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_liveb_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # dead-ball
  dplyr::left_join(
    res_deadb_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_deadb_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # bad pass
  dplyr::left_join(
    res_badp_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_badp_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # all turnovers
  dplyr::left_join(
    res_all_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_all_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # fill NAs for all P_TOV* count columns
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^P_TOV(_(LIVEB|DEADB|BADP))?_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating: if no OT_1/OT_2, force Q5/Q6 to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^P_TOV(_(LIVEB|DEADB|BADP))?_Q5$"),
      ~ dplyr::if_else(OT_1 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^P_TOV(_(LIVEB|DEADB|BADP))?_Q6$"),
      ~ dplyr::if_else(OT_2 == 1L, ., 0L)
    )
  )

rm(
  res_all_p, res_badp_p, res_deadb_p, res_liveb_p,
  tov_all_df, tov_badp_df, tov_base_player, tov_deadb_df, tov_liveb_df
)


# ============================================================
# Pull Team Turnovers (Q1–Q6) from BaseStats_Team_MC
# ============================================================

team_tov_qtr <- BaseStats_Team_MC %>%
  select(
    ESPN_GAME_ID,
    TEAM,
    T_TOV_Q1,
    T_TOV_Q2,
    T_TOV_Q3,
    T_TOV_Q4,
    T_TOV_Q5,
    T_TOV_Q6
  )

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  left_join(
    team_tov_qtr,
    by = c("ESPN_GAME_ID", "TEAM")
  )

rm(team_tov_qtr)

message("[✓] Player turnovers (live, dead, bad pass, all) added to BaseStats_Player_MC (athlete_id_1 based, OT-aware).")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Turnovers Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fouls Data Aggregation Section (PLAYER, athlete_id_1) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id","team_id","qtr","type_text","athlete_id_1") %in% names(pm_df)))
stopifnot(exists("BaseStats_Player_MC"))

to_int <- function(x) suppressWarnings(as.integer(x))

# ---- Base with committing player/team & normalized text (Q1–Q6) --------------------
foul_base_p <- pm_df %>%
  dplyr::mutate(
    qtr          = to_int(qtr),
    team_id      = as.character(team_id),
    athlete_id_1 = as.character(athlete_id_1),
    txt          = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(athlete_id_1),
    nzchar(athlete_id_1)
  )

# ---- Patterns (same as team section, operate on `txt`) -----------------------------
pat_any_foul   <- stringr::regex("foul", ignore_case = TRUE)

pat_shoot_foul <- stringr::regex("shooting foul", ignore_case = TRUE)
pat_tech_foul  <- stringr::regex("technical foul", ignore_case = TRUE)
pat_flag_foul  <- stringr::regex("flagrant foul type 1|flagrant foul type 2", ignore_case = TRUE)

pat_off_foul   <- stringr::regex(
  "offensive foul|offensive charge|offensive foul turnover|loose ball foul",
  ignore_case = TRUE
)

pat_def_foul   <- stringr::regex(
  paste(
    "personal foul",
    "personal take foul",
    "away from play foul",
    "transition take foul",
    "flagrant foul type 1",
    "flagrant foul type 2",
    "clear path foul",
    sep = "|"
  ),
  ignore_case = TRUE
)

pat_charge_drawn <- stringr::regex("offensive charge", ignore_case = TRUE)

# ---- Separate populations (player-level) -------------------------------------------
fouls_all_df_p   <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_any_foul))
fouls_shot_df_p  <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_shoot_foul))
fouls_tech_df_p  <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_tech_foul))
fouls_flag_df_p  <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_flag_foul))
fouls_off_df_p   <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_off_foul))
fouls_def_df_p   <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_def_foul))
charges_df_p     <- foul_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_charge_drawn))

# ---- Helper: per-quarter (Q1–Q6) + CGS, by (game, team, player) --------------------
tally_foul_player <- function(df, name_prefix) {
  # Q1–Q6 with complete quarters
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1
    ) %>%
    dplyr::rename_with(
      ~ paste0(name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1,
      !!paste0(name_prefix, "_CGS") := CGS
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Build tallies (player-level) --------------------------------------------------
res_fouls_all_p  <- tally_foul_player(fouls_all_df_p,  "FOULS")
res_fouls_shot_p <- tally_foul_player(fouls_shot_df_p, "SHOT_FOULS")
res_fouls_tech_p <- tally_foul_player(fouls_tech_df_p, "TECH_FOULS")
res_fouls_flag_p <- tally_foul_player(fouls_flag_df_p, "FLAG_FOULS")
res_fouls_off_p  <- tally_foul_player(fouls_off_df_p,  "OFF_FOULS")
res_fouls_def_p  <- tally_foul_player(fouls_def_df_p,  "DEF_FOULS")
res_chrg_draw_p  <- tally_foul_player(charges_df_p,    "CHRG_DRWN")

# ---- Join into BaseStats_Player_MC -------------------------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  # Total fouls
  dplyr::left_join(
    res_fouls_all_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_all_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Shooting
  dplyr::left_join(
    res_fouls_shot_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_shot_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Technical
  dplyr::left_join(
    res_fouls_tech_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_tech_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Flagrant
  dplyr::left_join(
    res_fouls_flag_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_flag_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Offensive
  dplyr::left_join(
    res_fouls_off_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_off_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Defensive
  dplyr::left_join(
    res_fouls_def_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_fouls_def_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Charges drawn
  dplyr::left_join(
    res_chrg_draw_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_chrg_draw_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Fill any newly added NA counts with 0 (Q1–Q6 + CGS)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^P_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating: if no OT_1/OT_2, force Q5/Q6 foul counts to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^P_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q5$"),
      ~ dplyr::if_else(OT_1 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^P_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q6$"),
      ~ dplyr::if_else(OT_2 == 1L, ., 0L)
    )
  )

rm(
  fouls_all_df_p, fouls_shot_df_p, fouls_tech_df_p, fouls_flag_df_p,
  fouls_off_df_p, fouls_def_df_p, charges_df_p, foul_base_p,
  res_fouls_all_p, res_fouls_shot_p, res_fouls_tech_p, res_fouls_flag_p,
  res_fouls_off_p, res_fouls_def_p, res_chrg_draw_p
)

message("[✓] Player fouls (total, shooting, technical, flagrant, offensive, defensive, charges drawn) tallied Q1–Q6 (OT-aware) + CGS and joined to BaseStats_Player_MC.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fouls Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





# === START: TOP-LEVEL Peripheral Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     ooooooooo.                       o8o             oooo                                     oooo  
#     `888   `Y88.                     `"'             `888                                     `888  
#      888   .d88'  .ooooo.  oooo d8b oooo  oo.ooooo.   888 .oo.    .ooooo.  oooo d8b  .oooo.    888  
#      888ooo88P'  d88' `88b `888""8P `888   888' `88b  888P"Y88b  d88' `88b `888""8P `P  )88b   888  
#      888         888ooo888  888      888   888   888  888   888  888ooo888  888      .oP"888   888  
#      888         888    .o  888      888   888   888  888   888  888    .o  888     d8(  888   888  
#     o888o        `Y8bod8P' d888b    o888o  888bod8P' o888o o888o `Y8bod8P' d888b    `Y888""8o o888o 
#                                            888                                                      
#                                           o888o 
#                                      
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Assists Data Aggregation Section (PLAYER, athlete_id_2) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id","team_id","qtr","type_text","score_value","athlete_id_2") %in% names(pm_df)))
stopifnot(exists("BaseStats_Player_MC"))

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
to_int   <- function(x) suppressWarnings(as.integer(x))

# ---- Build assist base from pm_df (assister = athlete_id_2) ------------------------
ast_base_p <-
  pm_df %>%
  dplyr::mutate(
    qtr          = to_int(qtr),
    team_id      = as.character(team_id),
    athlete_id_2 = as.character(athlete_id_2),
    is_ast       = stringr::str_detect(text, stringr::regex("assists", ignore_case = TRUE)),
    pts          = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(
    is_ast,
    qtr %in% 1:6,
    !is.na(athlete_id_2),
    nzchar(athlete_id_2)
  )

# ---- Per-quarter tallies (Q1–Q6) ---------------------------------------------------
ast_qtr_p <-
  ast_base_p %>%
  dplyr::group_by(game_id, team_id, athlete_id_2, qtr) %>%
  dplyr::summarise(
    AST     = dplyr::n(),
    AST_PTS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, athlete_id_2) %>%
  tidyr::complete(qtr = 1:6, fill = list(AST = 0L, AST_PTS = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(AST, AST_PTS),
    names_sep   = "_Q",
    values_fill = 0L
  ) %>%
  # ID columns for join into BaseStats_Player_MC
  dplyr::rename(
    espn_game_id   = game_id,
    espn_team_id   = team_id,
    espn_player_id = athlete_id_2
  ) %>%
  # Rename stat columns -> AST_*, AST_PTS_*
  dplyr::rename_with(~ gsub("^AST_", "AST_", .x)) %>%
  dplyr::rename_with(~ gsub("^AST_PTS_", "AST_PTS_", .x))

# ---- Complete-game tallies (CGS) ---------------------------------------------------
ast_cgs_p <-
  ast_base_p %>%
  dplyr::group_by(game_id, team_id, athlete_id_2) %>%
  dplyr::summarise(
    AST_CGS     = dplyr::n(),
    AST_PTS_CGS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(
    espn_game_id   = game_id,
    espn_team_id   = team_id,
    espn_player_id = athlete_id_2
  )

# ---- Join into BaseStats_Player_MC -------------------------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  dplyr::left_join(
    ast_qtr_p,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    ast_cgs_p,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Fill all Q1–Q6 + CGS assist counts/points
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^AST(_PTS)?_Q[1-6]$|^AST(_PTS)?_CGS$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # ---- OT gating: zero out Q5/Q6 when OT_1 / OT_2 did not happen -------------------
dplyr::mutate(
  dplyr::across(
    tidyselect::matches("^AST(_PTS)?_Q5$"),
    ~ dplyr::if_else(OT_1 == 1L, ., 0L)
  ),
  dplyr::across(
    tidyselect::matches("^AST(_PTS)?_Q6$"),
    ~ dplyr::if_else(OT_2 == 1L, ., 0L)
  )
) %>%
  # ---- Derived: Assist % and AST:TOV (player-level) --------------------------------
dplyr::mutate(
  # Assist % (uses player FGM buckets if present in BaseStats_Player_MC)
  AST_PCT_Q1 = safe_div(AST_Q1, (T_FGM_Q1 - FGM_Q1)),
  AST_PCT_Q2 = safe_div(AST_Q2, (T_FGM_Q2 - FGM_Q2)),
  AST_PCT_Q3 = safe_div(AST_Q3, (T_FGM_Q3 - FGM_Q3)),
  AST_PCT_Q4 = safe_div(AST_Q4, (T_FGM_Q4 - FGM_Q4)),
  AST_PCT_Q5 = safe_div(AST_Q5, (T_FGM_Q5 - FGM_Q5)),
  AST_PCT_Q6 = safe_div(AST_Q6, (T_FGM_Q6 - FGM_Q6)),
  AST_PCT_CGS = safe_div(AST_CGS, (T_FGM_CGS - FGM_CGS)),
  
  # AST:TOV ratios (player AST vs player TOV)
  AST_TOV_Q1  = safe_div(AST_Q1 , TOV_Q1 ),
  AST_TOV_Q2  = safe_div(AST_Q2 , TOV_Q2 ),
  AST_TOV_Q3  = safe_div(AST_Q3 , TOV_Q3 ),
  AST_TOV_Q4  = safe_div(AST_Q4 , TOV_Q4 ),
  AST_TOV_Q5  = safe_div(AST_Q5 , TOV_Q5 ),   # OT1
  AST_TOV_Q6  = safe_div(AST_Q6 , TOV_Q6 ),   # OT2
  AST_TOV_CGS = safe_div(AST_CGS, TOV_CGS)
)

rm(ast_base_p, ast_qtr_p, ast_cgs_p)
message("[✓] Player assists (Q1–Q6 OT-aware) + AST_PTS + AST% + AST:TOV joined into BaseStats_Player_MC (assister = athlete_id_2).")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Assists Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Rebounds Data Aggregation Section (PLAYER, athlete_id_1) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id","team_id","qtr","type_text","athlete_id_1") %in% names(pm_df)))
stopifnot(exists("BaseStats_Player_MC"))

to_int <- function(x) suppressWarnings(as.integer(x))

# ---- Base with normalized labels (Q1–Q6), rebounder = athlete_id_1 -----------------
reb_base_p <- pm_df %>%
  dplyr::mutate(
    qtr          = to_int(qtr),
    team_id      = as.character(team_id),
    athlete_id_1 = as.character(athlete_id_1),
    lbl          = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(athlete_id_1),
    nzchar(athlete_id_1)
  )

# Patterns
pat_oreb <- stringr::regex("^offensive rebound$", ignore_case = TRUE)
pat_dreb <- stringr::regex("^defensive rebound$", ignore_case = TRUE)

# ---- Helper: tally by quarter + CGS, credited to same player/team -------------------
tally_reb_player <- function(df, name_prefix) {
  # Q1–Q6
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1
    ) %>%
    dplyr::rename_with(
      ~ paste0(name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id, athlete_id_1) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = team_id,
      espn_player_id = athlete_id_1,
      !!paste0(name_prefix, "_CGS") := CGS
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build pieces (player-level)
res_oreb_p <- reb_base_p %>%
  dplyr::filter(stringr::str_detect(lbl, pat_oreb)) %>%
  tally_reb_player("OREB")

res_dreb_p <- reb_base_p %>%
  dplyr::filter(stringr::str_detect(lbl, pat_dreb)) %>%
  tally_reb_player("DREB")

# ---- Join into BaseStats_Player_MC --------------------------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  # Offensive rebounds
  dplyr::left_join(
    res_oreb_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_oreb_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Defensive rebounds
  dplyr::left_join(
    res_dreb_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_dreb_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Fill new NAs with 0; Q1–Q6 + CGS
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^(O|D)REB_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    ),
    # Totals = OREB + DREB (Q1–Q6 + CGS)
    REB_Q1  = OREB_Q1  + DREB_Q1,
    REB_Q2  = OREB_Q2  + DREB_Q2,
    REB_Q3  = OREB_Q3  + DREB_Q3,
    REB_Q4  = OREB_Q4  + DREB_Q4,
    REB_Q5  = OREB_Q5  + DREB_Q5,
    REB_Q6  = OREB_Q6  + DREB_Q6,
    REB_CGS = OREB_CGS + DREB_CGS
  )

rm(reb_base_p, res_oreb_p, res_dreb_p)
message("[✓] Player rebounds (OREB, DREB, REB) Q1–Q6 + CGS added to BaseStats_Player_MC (rebounder = athlete_id_1).")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Rebounds Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Usage Rate (Player) — CGS ONLY (NO *100, NA => 0) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# USG share (not %):
# USG_CGS = ( (FGA + 0.44*FTA + TOV) * (48*5) ) / ( MINS * (T_FGA + 0.44*T_FTA + T_TOV) )
# Notes:
# - Uses MINS_Q1..MINS_Q6 and team totals T_FGA_Q1..Q6, T_FTA_Q1..Q6, T_TOV_Q1..Q6
# - All NAs treated as 0

# ---- helpers (keep local) ----
num0 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::coalesce(x, 0)
}


# ---- compute USG_CGS only ----
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    # player mins
    mins_cgs = num0(MINS_Q1) + num0(MINS_Q2) + num0(MINS_Q3) + num0(MINS_Q4) + num0(MINS_Q5) + num0(MINS_Q6),
    
    # player "usage events"
    p_poss_cgs =
      (num0(FGA_Q1) + num0(FGA_Q2) + num0(FGA_Q3) + num0(FGA_Q4) + num0(FGA_Q5) + num0(FGA_Q6)) +
      0.44 * (num0(FTA_Q1) + num0(FTA_Q2) + num0(FTA_Q3) + num0(FTA_Q4) + num0(FTA_Q5) + num0(FTA_Q6)) +
      (num0(TOV_Q1) + num0(TOV_Q2) + num0(TOV_Q3) + num0(TOV_Q4) + num0(TOV_Q5) + num0(TOV_Q6)),
    
    # team "usage events"
    t_poss_cgs =
      (num0(T_FGA_Q1) + num0(T_FGA_Q2) + num0(T_FGA_Q3) + num0(T_FGA_Q4) + num0(T_FGA_Q5) + num0(T_FGA_Q6)) +
      0.44 * (num0(T_FTA_Q1) + num0(T_FTA_Q2) + num0(T_FTA_Q3) + num0(T_FTA_Q4) + num0(T_FTA_Q5) + num0(T_FTA_Q6)) +
      (num0(T_TOV_Q1) + num0(T_TOV_Q2) + num0(T_TOV_Q3) + num0(T_TOV_Q4) + num0(T_TOV_Q5) + num0(T_TOV_Q6)),
    
    # usage share (NO *100)
    USG_CGS = dplyr::if_else(
      mins_cgs > 0 & t_poss_cgs > 0,
      (p_poss_cgs * (48)) / (mins_cgs * t_poss_cgs),
      NA_real_
    )
  ) %>%
  # cleanup temp cols
  dplyr::select(-mins_cgs, -p_poss_cgs, -t_poss_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Usage Rate (Player) — CGS ONLY ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: TOP-LEVEL Peripheral Data Aggregation Section ====



# === START: TOP-LEVEL Defense Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     oooooooooo.              .o88o.                                          
#     `888'   `Y8b             888 `"                                          
#      888      888  .ooooo.  o888oo   .ooooo.  ooo. .oo.    .oooo.o  .ooooo.  
#      888      888 d88' `88b  888    d88' `88b `888P"Y88b  d88(  "8 d88' `88b 
#      888      888 888ooo888  888    888ooo888  888   888  `"Y88b.  888ooo888 
#      888     d88' 888    .o  888    888    .o  888   888  o.  )88b 888    .o 
#     o888bood8P'   `Y8bod8P' o888o   `Y8bod8P' o888o o888o 8""888P' `Y8bod8P' 
#                                                                         
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀              


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Steals and Blocks Data Aggregation Section (PLAYER, athlete_id_2) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# We expect these columns in pm_df
stopifnot(all(c("game_id","team_id","qtr","text","athlete_id_2","def_team_id") %in% names(pm_df)))
stopifnot(exists("BaseStats_Player_MC"))

to_int <- function(x) suppressWarnings(as.integer(x))

# ---- Player hustle base: defender = athlete_id_2, Q1–Q6 ----------------------------
# def_team_id = defender's team (inverse of offensive team_id)
hustle_base_p <- pm_df %>%
  dplyr::mutate(
    qtr          = to_int(qtr),
    def_team_id  = as.character(def_team_id),
    athlete_id_2 = as.character(athlete_id_2),
    txt          = stringr::str_to_lower(stringr::str_trim(text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(athlete_id_2),
    nzchar(athlete_id_2)
  )

# Patterns (same as team section)
pat_steal <- stringr::regex("steal", ignore_case = TRUE)
pat_block <- stringr::regex("block", ignore_case = TRUE)

stl_df_p <- hustle_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_steal))
blk_df_p <- hustle_base_p %>% dplyr::filter(stringr::str_detect(txt, pat_block))

# ---- Helper: tally by quarter + CGS for players (defender) -------------------------
tally_hustle_player <- function(df, name_prefix) {
  # Q1–Q6 (AST-style: group, then complete within each game/def_team_id/player)
  qtr_df <- df %>%
    dplyr::group_by(game_id, def_team_id, athlete_id_2, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop_last") %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      names_prefix = "Q",
      values_fill = 0L
    ) %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = def_team_id,
      espn_player_id = athlete_id_2
    ) %>%
    dplyr::rename_with(
      ~ paste0(name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, def_team_id, athlete_id_2) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(
      espn_game_id   = game_id,
      espn_team_id   = def_team_id,
      espn_player_id = athlete_id_2,
      !!paste0(name_prefix, "_CGS") := CGS
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build player-level pieces
res_stl_p <- tally_hustle_player(stl_df_p, "STL")
res_blk_p <- tally_hustle_player(blk_df_p, "BLK")

# ---- Join into BaseStats_Player_MC --------------------------------------------------
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  # Steals
  dplyr::left_join(
    res_stl_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_stl_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Blocks
  dplyr::left_join(
    res_blk_p$qtr,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  dplyr::left_join(
    res_blk_p$cgs,
    by = c("ESPN_GAME_ID"   = "espn_game_id",
           "ESPN_TEAM_ID"   = "espn_team_id",
           "ESPN_PLAYER_ID" = "espn_player_id")
  ) %>%
  # Fill new NA counts with 0 (Q1–Q6 + CGS)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^(STL|BLK)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  )

message("[✓] Player steals & blocks (athlete_id_2, grouped by def_team_id) Q1–Q6 + CGS added to BaseStats_Player_MC.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Steals and Blocks Data Aggregation Section (PLAYER) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀






# === START: TOP-LEVEL Runs Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     ooooooooo.                                    
#     `888   `Y88.                                  
#      888   .d88' oooo  oooo  ooo. .oo.    .oooo.o 
#      888ooo88P'  `888  `888  `888P"Y88b  d88(  "8 
#      888`88b.     888   888   888   888  `"Y88b.  
#      888  `88b.   888   888   888   888  o.  )88b 
#     o888o  o888o  `V88V"V8P' o888o o888o 8""888P' 
#                                              
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                              
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: (Player Level) Runs Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ── 1) Prepare PBP (order events, normalize types) ───────────────────────────────
pbp_prepped <- pm_df %>%
  mutate(
    # seconds left in the (quarter/period)
    clock_seconds_total = suppressWarnings(as.numeric(clock_minutes)) * 60 +
      suppressWarnings(as.numeric(clock_seconds)),
    # prefer qtr if present; otherwise use period
    qtr = suppressWarnings(as.integer(coalesce(as.numeric(qtr), as.numeric(period)))),
    period = suppressWarnings(as.integer(period)),
    sequence_number = suppressWarnings(as.integer(sequence_number))
  ) %>%
  arrange(game_id, qtr, desc(clock_seconds_total), sequence_number)

# ── 2) Keep only scoring plays and give each scoring row a per-game row_id ───────
scoring_events <- pbp_prepped %>%
  filter(tolower(scoring_play) %in% c("true","t","1","yes","y")) %>%
  mutate(points = suppressWarnings(as.numeric(score_value))) %>%
  group_by(game_id) %>%
  arrange(qtr, desc(clock_seconds_total), sequence_number, .by_group = TRUE) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  transmute(
    game_id,
    row_id,
    scoring_team = team_id,
    points,
    qtr,
    period,
    clock_seconds_total
  )

# ── 3) Detect contiguous runs (same team scoring on consecutive scoring events) ──
detect_runs_vec <- function(df) {
  # df is scoring_events for a single game_id (ordered & with row_id)
  if (nrow(df) == 0) return(df[0, c()])
  
  grp <- cumsum(df$scoring_team != dplyr::lag(df$scoring_team, default = df$scoring_team[1]))
  
  df %>%
    mutate(run_grp = grp) %>%
    group_by(game_id, run_grp) %>%
    summarise(
      run_team      = first(scoring_team),
      run_start_row = first(row_id),
      run_end_row   = last(row_id),
      run_points    = sum(points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(-run_grp)
}

run_stats <- scoring_events %>%
  group_by(game_id) %>%
  group_split() %>%
  lapply(detect_runs_vec) %>%
  bind_rows()

# ── 4) Enrich each run with start/end quarter & clock (join back by row_id) ─────
# helper lookups for start/end rows within each game
se_start <- scoring_events %>%
  select(game_id, row_id,
         run_start_qtr  = qtr,
         run_start_period = period,
         run_start_clock  = clock_seconds_total)

se_end <- scoring_events %>%
  select(game_id, row_id,
         run_end_qtr    = qtr,
         run_end_period = period,
         run_end_clock  = clock_seconds_total)

run_stats <- run_stats %>%
  left_join(se_start, by = c("game_id", "run_start_row" = "row_id")) %>%
  left_join(se_end,   by = c("game_id", "run_end_row"   = "row_id")) %>%
  mutate(
    run_start_clock_minutes = floor(run_start_clock / 60),
    run_start_clock_seconds = round(run_start_clock %% 60),
    run_end_clock_minutes   = floor(run_end_clock   / 60),
    run_end_clock_seconds   = round(run_end_clock   %% 60),
    # NEW: duration of the run within the period (clock counts down)
    run_duration_sec        = pmax(0, run_start_clock - run_end_clock)
  )


# (Optional) If you also want a prefiltered version, e.g., only 6+ point runs:
# run_stats_6plus <- run_stats %>% filter(run_points >= 6)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: (Player Level) Runs Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: (Player Level) Runs Contribution Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================================
# === PLAYER RUN CONTRIBUTION: RUN_CONT_Qx / RUN_PTS_CONT_Qx ==
# ============================================================

library(dplyr)
library(tidyr)

stopifnot(exists("pm_df"), exists("run_stats"), exists("BaseStats_Player_MC"))

# --- parameter: minimum points for a run to "count" for player stats ----
BIG_RUN_MIN_POINTS <- 7L   # change to 10L / 15L later if you want

# 1) Rebuild scoring-events table WITH player id (athlete_id_1)
#    using the same ordering logic as the runs section
scoring_events_player <- pm_df %>%
  mutate(
    clock_seconds_total = suppressWarnings(as.numeric(clock_minutes)) * 60 +
      suppressWarnings(as.numeric(clock_seconds)),
    qtr     = suppressWarnings(as.integer(coalesce(as.numeric(qtr), as.numeric(period)))),
    period  = suppressWarnings(as.integer(period)),
    sequence_number = suppressWarnings(as.integer(sequence_number))
  ) %>%
  arrange(game_id, qtr, desc(clock_seconds_total), sequence_number) %>%
  # keep only scoring plays
  filter(tolower(scoring_play) %in% c("true","t","1","yes","y")) %>%
  mutate(
    points = suppressWarnings(as.numeric(score_value))
  ) %>%
  group_by(game_id) %>%
  arrange(qtr, desc(clock_seconds_total), sequence_number, .by_group = TRUE) %>%
  mutate(row_id = dplyr::row_number()) %>%
  ungroup() %>%
  transmute(
    game_id,
    row_id,
    scoring_team = team_id,
    points,
    qtr,
    period,
    clock_seconds_total,
    athlete_id_1 = as.character(athlete_id_1)
  )

# 2) Filter run_stats down to "big runs" (team scored >= BIG_RUN_MIN_POINTS)
#    Optionally add a duration filter, e.g. run_duration_sec <= 120
big_runs <- run_stats %>%
  filter(
    !is.na(run_start_qtr),
    run_start_qtr %in% 1:4,
    run_points >= BIG_RUN_MIN_POINTS
    # , run_duration_sec <= 120   # uncomment if you want "7+ in <= 2 min" etc.
  )

# 3) Attach big runs to their constituent scoring events + players
#    (each row = one scoring play that occurs inside a qualifying run)
run_scoring_player <- big_runs %>%
  select(
    game_id,
    run_team,
    run_start_row,
    run_end_row,
    run_start_qtr
  ) %>%
  inner_join(
    scoring_events_player,
    by = c("game_id")
  ) %>%
  # keep only scoring rows that belong to this run and team
  filter(
    scoring_team == run_team,
    row_id >= run_start_row,
    row_id <= run_end_row
  )

# 4) Collapse to per-player-per-run contribution
#    (one row per player per qualifying run)
player_run_level <- run_scoring_player %>%
  group_by(
    game_id,
    run_team,
    run_start_qtr,
    run_start_row,
    run_end_row,
    athlete_id_1
  ) %>%
  summarise(
    RUN_CONT = 1L,                           # player contributed to this run
    RUN_PTS  = sum(points, na.rm = TRUE),    # points scored in this run
    .groups  = "drop"
  )

# 5) Aggregate to per-player-per-game per quarter
player_runs_qtr <- player_run_level %>%
  group_by(game_id, athlete_id_1, run_start_qtr) %>%
  summarise(
    RUN_CONT_Q = sum(RUN_CONT, na.rm = TRUE),  # how many big runs they contributed to
    RUN_PTS_Q  = sum(RUN_PTS,  na.rm = TRUE),  # points in those big runs
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    id_cols      = c(game_id, athlete_id_1),
    names_from   = run_start_qtr,
    values_from  = c(RUN_CONT_Q, RUN_PTS_Q),
    names_glue   = "{.value}_Q{run_start_qtr}"
  ) %>%
  # rename to final field names
  rename_with(
    ~ gsub("^RUN_CONT_Q_", "RUN_CONT_Q", .x),
    dplyr::starts_with("RUN_CONT_Q_")
  ) %>%
  rename_with(
    ~ gsub("^RUN_PTS_Q_", "RUN_PTS_CONT_Q", .x),
    dplyr::starts_with("RUN_PTS_Q_")
  )

# 6) Aggregate CGS (full-game) versions
player_runs_cgs <- player_run_level %>%
  group_by(game_id, athlete_id_1) %>%
  summarise(
    RUN_CONT_CGS     = sum(RUN_CONT, na.rm = TRUE),
    RUN_PTS_CONT_CGS = sum(RUN_PTS,  na.rm = TRUE),
    .groups = "drop"
  )

# 7) Join into BaseStats_Player_MC
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID)
  ) %>%
  left_join(
    player_runs_qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  left_join(
    player_runs_cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_PLAYER_ID" = "athlete_id_1")
  ) %>%
  # coalesce missing values (no contributions => 0)
  mutate(
    dplyr::across(
      tidyselect::matches("^RUN_CONT_Q[1-4]$|^RUN_CONT_CGS$"),
      ~ dplyr::coalesce(., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^RUN_PTS_CONT_Q[1-4]$|^RUN_PTS_CONT_CGS$"),
      ~ dplyr::coalesce(., 0)
    )
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: (Player Level) Runs Contribution Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Join Player Odds into BaseStats_Player_MC ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    # Make a copy so original never changes
    GAME_DATE_ORIG = GAME_DATE,
    
    # Try parsing — failures will be NA but original stays intact
    GAME_DATE_PARSED = suppressWarnings(as.Date(GAME_DATE_ORIG, format = "%m/%d/%Y")),
    
    # Use parsed version ONLY if valid, otherwise fall back to original string
    GAME_DATE_STR = ifelse(
      !is.na(GAME_DATE_PARSED),
      format(GAME_DATE_PARSED, "%Y-%m-%d"),
      GAME_DATE_ORIG  # fallback if it couldn't parse
    ),
    
    # Build ODDSAPI_MAP from safe formatted field
    ODDSAPI_MAP = paste0(TEAM, "_", OPP, "_", GAME_DATE_STR)
  ) %>%
  select(-GAME_DATE_PARSED)   # remove temp helper column



library(dplyr)
library(data.table)

stopifnot(exists("BaseStats_Player_MC"), exists("season_token"))

# 1) Load season-level player odds file
player_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/",
  "nba_player_odds_enrich_", season_token, ".csv"
)

player_odds <- fread(player_odds_path, encoding = "UTF-8", colClasses = "character") %>%
  as_tibble() %>%
  mutate(
    # normalize date to yyyy-MM-dd to match BaseStats_Player_MC
    GAME_DATE = as.Date(GAME_DATE, format = "%m/%d/%Y"),
    GAME_DATE = format(GAME_DATE, "%Y-%m-%d"),
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP)
  )

# 2) Keep only the ID + prop columns we want to bring over
player_odds_sel <- player_odds %>%
  select(
    ESPN_PLAYER_ID,
    ODDSAPI_MAP,
    PTS_DK_LINE_O, PTS_DK_ODDS_O, PTS_DK_LINE_U, PTS_DK_ODDS_U,
    PTS_FD_LINE_O, PTS_FD_ODDS_O, PTS_FD_LINE_U, PTS_FD_ODDS_U,
    PTS_FNA_LINE_O, PTS_FNA_ODDS_O, PTS_FNA_LINE_U, PTS_FNA_ODDS_U,
    PTS_BMGM_LINE_O, PTS_BMGM_ODDS_O, PTS_BMGM_LINE_U, PTS_BMGM_ODDS_U,
    AST_DK_LINE_O, AST_DK_ODDS_O, AST_DK_LINE_U, AST_DK_ODDS_U,
    AST_FD_LINE_O, AST_FD_ODDS_O, AST_FD_LINE_U, AST_FD_ODDS_U,
    AST_FNA_LINE_O, AST_FNA_ODDS_O, AST_FNA_LINE_U, AST_FNA_ODDS_U,
    AST_BMGM_LINE_O, AST_BMGM_ODDS_O, AST_BMGM_LINE_U, AST_BMGM_ODDS_U,
    REB_DK_LINE_O, REB_DK_ODDS_O, REB_DK_LINE_U, REB_DK_ODDS_U,
    REB_FD_LINE_O, REB_FD_ODDS_O, REB_FD_LINE_U, REB_FD_ODDS_U,
    REB_FNA_LINE_O, REB_FNA_ODDS_O, REB_FNA_LINE_U, REB_FNA_ODDS_U,
    REB_BMGM_LINE_O, REB_BMGM_ODDS_O, REB_BMGM_LINE_U, REB_BMGM_ODDS_U,
    `3PM_DK_LINE_O`, `3PM_DK_ODDS_O`, `3PM_DK_LINE_U`, `3PM_DK_ODDS_U`,
    `3PM_FD_LINE_O`, `3PM_FD_ODDS_O`, `3PM_FD_LINE_U`, `3PM_FD_ODDS_U`,
    `3PM_FNA_LINE_O`, `3PM_FNA_ODDS_O`, `3PM_FNA_LINE_U`, `3PM_FNA_ODDS_U`,
    `3PM_BMGM_LINE_O`, `3PM_BMGM_ODDS_O`, `3PM_BMGM_LINE_U`, `3PM_BMGM_ODDS_U`,
    PTS_COVER, AST_COVER, REB_COVER, `3PM_COVER`
  )

# 3) Prepare BaseStats_Player_MC keys & join odds in
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP)
  ) %>%
  left_join(
    player_odds_sel,
    by = c("ESPN_PLAYER_ID", "ODDSAPI_MAP")
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Join Player Odds into BaseStats_Player_MC ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Join Schedule/Odds into BaseStats_Player_MC ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(data.table)

stopifnot(exists("BaseStats_Player_MC"), exists("season_token"))

# 1) Load season schedule file (already enriched with totals/spreads/moneyline)
nba_sched_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/",
  "nba_schedule_", season_token, ".csv"
)

nba_schedule <- fread(nba_sched_path, encoding = "UTF-8", colClasses = "character") %>%
  as_tibble() %>%
  mutate(
    game_id = as.character(game_id),
    team_id = as.character(team_id)
  )

# 2) Keep only the ID + schedule/odds columns we care about
nba_schedule_sel <- nba_schedule %>%
  select(
    game_id,
    team_id,
    DK_total_O, FD_total_O, FNT_total_O, BMGM_total_O,
    DK_total_odds_O, DK_total_odds_U,
    FD_total_odds_O, FD_total_odds_U,
    FNT_total_odds_O, FNT_total_odds_U,
    BMGM_total_odds_O, BMGM_total_odds_U,
    DK_spread, FD_spread, FNT_spread, BMGM_spread,
    DK_h2h, FD_h2h, FNT_h2h, BMGM_h2h,
    team_color, home_alternate_color,
    opp_color, opp_alternate_color,
    team_score, opp_score,
    team_winner, opp_winner
  )

# 3) Join onto BaseStats_Player_MC by game + team
BaseStats_Player_MC <- BaseStats_Player_MC %>%
  mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  left_join(
    nba_schedule_sel,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id"
    )
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Join Schedule/Odds into BaseStats_Player_MC ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START ==== Write BaseStats Player for Monte Carlo and Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 9.6 Export the updated BaseStats_Team data frame
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_MC_",
  season_token,
  ".csv"
)
write.csv(BaseStats_Player_MC, file = team_output_path, row.names = FALSE)

print(paste("BaseStats_Player for Monte Carlo has been exported to:", team_output_path))

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END ==== Write BaseStats Player for Monte Carlo and Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START ==== Create .rds version of all files ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


folders <- c(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/8. Leauge Standings",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/14. Bet History",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)",
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)"
)

total <- 0

for (folder in folders) {
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(csv_files) == 0) {
    message("No CSVs found in: ", basename(folder))
    next
  }
  
  for (f in csv_files) {
    tryCatch({
      dat <- read_csv(f, show_col_progress = FALSE, col_types = cols())
      rds_path <- sub("\\.csv$", ".rds", f, ignore.case = TRUE)
      saveRDS(dat, rds_path)
      total <- total + 1
      message("Converted: ", basename(f))
    }, error = function(e) {
      message("ERROR on ", basename(f), ": ", e$message)
    })
  }
}

message("\nDone! Converted ", total, " files.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END ==== Create .rds version of all files ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

