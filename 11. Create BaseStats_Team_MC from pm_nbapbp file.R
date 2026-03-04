# ==== START: Build BaseStats_Team_MC Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     oooooooooo.               o8o  oooo        .o8       oooooooooo.                                .oooooo..o     .                 .                        ooooooooooooo                                                   ooo        ooooo   .oooooo.   
#     `888'   `Y8b              `"'  `888       "888       `888'   `Y8b                              d8P'    `Y8   .o8               .o8                        8'   888   `8                                                   `88.       .888'  d8P'  `Y8b  
#      888     888 oooo  oooo  oooo   888   .oooo888        888     888  .oooo.    .oooo.o  .ooooo.  Y88bo.      .o888oo  .oooo.   .o888oo  .oooo.o                  888       .ooooo.   .oooo.   ooo. .oo.  .oo.                888b     d'888  888          
#      888oooo888' `888  `888  `888   888  d88' `888        888oooo888' `P  )88b  d88(  "8 d88' `88b  `"Y8888o.    888   `P  )88b    888   d88(  "8                  888      d88' `88b `P  )88b  `888P"Y88bP"Y88b               8 Y88. .P  888  888          
#      888    `88b  888   888   888   888  888   888        888    `88b  .oP"888  `"Y88b.  888ooo888      `"Y88b   888    .oP"888    888   `"Y88b.                   888      888ooo888  .oP"888   888   888   888               8  `888'   888  888          
#      888    .88P  888   888   888   888  888   888        888    .88P d8(  888  o.  )88b 888    .o oo     .d8P   888 . d8(  888    888 . o.  )88b                  888      888    .o d8(  888   888   888   888               8    Y     888  `88b    ooo  
#     o888bood8P'   `V88V"V8P' o888o o888o `Y8bod88P"      o888bood8P'  `Y888""8o 8""888P' `Y8bod8P' 8""88888P'    "888" `Y888""8o   "888" 8""888P' ooooooooooo     o888o     `Y8bod8P' `Y888""8o o888o o888o o888o ooooooooooo o8o        o888o  `Y8bood8P'  
#                                                                                                                                                                                                                                                        
#                                                                                                                                                                                                                                                        
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Intial BaseStats_Team_MC Build Logic ====
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
library(data.table)
library(stringr)
library(tidyr)
library(readr)


# --- CONFIG ---
base_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/"
pm_path <- file.path(base_dir, paste0("pm_nbapbp_", season_token, ".csv"))
stopifnot(file.exists(pm_path))

# --- LOAD AS CHARACTER ---
pm_df <- fread(pm_path, colClasses = "character") %>% as_tibble()

# Format home_score and away_score as integers
pm_df <- pm_df %>%
  dplyr::mutate(
    score_diff = abs(as.integer(away_score) - as.integer(home_score))
  )

# --- MARKER & SCORE COLS ---
marker_col <- dplyr::case_when(
  "text" %in% names(pm_df) ~ "text",
  "desc" %in% names(pm_df) ~ "desc",
  TRUE ~ NA_character_
)
stopifnot(!is.na(marker_col))

home_score_col <- if ("home_score" %in% names(pm_df)) "home_score" else NA_character_
away_score_col <- if ("away_score" %in% names(pm_df)) "away_score" else NA_character_
stopifnot(!is.na(home_score_col), !is.na(away_score_col))

# --- DECISION PER GAME (Q4 -> OT1 -> OT2) ---
decisions <-
  pm_df %>%
  filter(.data[[marker_col]] %in% c("End of the 4th Quarter",
                                    "End of the 1st Overtime",
                                    "End of the 2nd Overtime")) %>%
  transmute(
    game_id,
    hs = suppressWarnings(as.integer(.data[[home_score_col]])),
    as = suppressWarnings(as.integer(.data[[away_score_col]])),
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
  filter(hs != as) %>%                 # first non-tied checkpoint
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    game_id,
    WINNER_SIDE = if_else(hs > as, "home", "away"),
    DECIDED_AT  = case_when(
      order_level == 1L ~ "OT0",
      order_level == 2L ~ "OT1",
      order_level == 3L ~ "OT2",
      TRUE ~ NA_character_
    )
  )

# --- PER-GAME META (carry nba_game_id forward here) ---
games <-
  pm_df %>%
  group_by(game_id) %>%
  summarise(
    nba_game_id        = first(nba_game_id),
    nba_team_id        = first(nba_team_id),
    home_team_id       = first(home_team_id),
    away_team_id       = first(away_team_id),
    home_team_abbrev   = coalesce(first(home_team_abbrev), NA_character_),
    away_team_abbrev   = coalesce(first(away_team_abbrev), NA_character_),
    game_date          = first(game_date),
    .groups = "drop"
  )

# --- EXPAND TO TWO ROWS PER GAME (HOME/AWAY) ---
home_rows <- games %>%
  transmute(
    game_id,
    nba_game_id,
    nba_team_id,
    team_id   = home_team_id,
    HOME_AWAY = "home",
    TEAM      = home_team_abbrev,
    game_date
  )

away_rows <- games %>%
  transmute(
    game_id,
    nba_game_id,
    nba_team_id,
    team_id   = away_team_id,
    HOME_AWAY = "away",
    TEAM      = away_team_abbrev,
    game_date
  )

BaseStats_Team_MC <- bind_rows(home_rows, away_rows)

# --- ATTACH DECISIONS & FINAL ADMIN FIELDS ---
BaseStats_Team_MC <-
  BaseStats_Team_MC %>%
  left_join(decisions, by = "game_id") %>%
  mutate(
    ESPN_GAME_ID = as.character(game_id),
    NBA_GAME_ID  = as.character(nba_game_id),     # <-- from pm_df via `games`
    ESPN_TEAM_ID = as.character(team_id),
    NBA_TEAM_ID  = as.character(nba_team_id),                # create the column so select() succeeds
    GAME_DATE    = as.character(game_date),
    IS_HOME      = if_else(HOME_AWAY == "home", 1L, 0L),
    IS_AWAY      = if_else(HOME_AWAY == "away", 1L, 0L),
    TEAM_WINNER  = if_else(!is.na(WINNER_SIDE) & HOME_AWAY == WINNER_SIDE, 1L, 0L),
    OT_1 = as.integer(game_id %in% pm_df$game_id[suppressWarnings(as.integer(pm_df$qtr)) == 5]),
    OT_2 = as.integer(game_id %in% pm_df$game_id[suppressWarnings(as.integer(pm_df$qtr)) == 6]),
  ) %>%
  select(
    ESPN_GAME_ID, NBA_GAME_ID, ESPN_TEAM_ID, NBA_TEAM_ID,
    GAME_DATE, TEAM_WINNER, OT_1, OT_2,
    HOME_AWAY, IS_AWAY, IS_HOME, TEAM
  ) %>%
  arrange(ESPN_GAME_ID, desc(IS_HOME))

# Map each (ESPN_GAME_ID, ESPN_TEAM_ID) to its opponent TEAM once
opp_map <- BaseStats_Team_MC %>%
  distinct(ESPN_GAME_ID, ESPN_TEAM_ID, TEAM) %>%
  inner_join(
    distinct(BaseStats_Team_MC, ESPN_GAME_ID, ESPN_TEAM_ID, TEAM),
    by = "ESPN_GAME_ID",
    suffix = c("", "_OPP")
  ) %>%
  filter(ESPN_TEAM_ID != ESPN_TEAM_ID_OPP) %>%
  transmute(ESPN_GAME_ID, ESPN_TEAM_ID, OPP = TEAM_OPP) %>%
  distinct()

# Attach OPP to every player row
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  left_join(opp_map, by = c("ESPN_GAME_ID","ESPN_TEAM_ID"))

cat(sprintf("[BaseStats_Player_MC] Seeded %s player-game-team rows.\n",
            format(nrow(BaseStats_Team_MC), big.mark = ",")))


pm_df$def_team_id<- ifelse(pm_df$team_id == pm_df$home_team_id,
                           pm_df$away_team_id,
                           pm_df$home_team_id)
# ensure GAME_DATE is in true Date format

# --- Fix TEAM abbreviations in BaseStats_Player_MC -----------------------------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
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


BaseStats_Team_MC <- BaseStats_Team_MC %>%
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


rm(away_rows, home_rows, games, decisions)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Intial BaseStats_Team_MC Build Logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: IS_B2B and TOT_RST_DYS ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ensure date typed for diff
BaseStats_Team_MC <- BaseStats_Team_MC %>%
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
# === START: BaseStats_Team column pull in section  ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(data.table)
library(dplyr)

#===============================================================================
# 10.x  Pull logo / side / totals info from BaseStats_Team_<season_token>
#===============================================================================

bst_team_path <- file.path(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)",
  "1. hoopR", "1. BaseStats_Team",
  paste0("BaseStats_Team_", season_token, ".csv")
)

BaseStats_Team_season <- fread(
  bst_team_path,
  colClasses = "character",
  encoding   = "UTF-8"
)

bst_lookup <- BaseStats_Team_season %>%
  select(
    espn_game_id,
    team, opp,
    team_logo,
    opp_logo,
    home_away,
    dog_win, fav_win, dog_loss, fav_loss,
    home_win, home_loss, away_win, away_loss,
    home_dog_win, home_fav_loss, away_dog_win, away_fav_loss,
    DK_total_O,  DK_total_U,
    FD_total_O,  FD_total_U,
    BMGM_total_O, BMGM_total_U,
    FNT_total_O,  FNT_total_U,
    DK_total_odds_O,  DK_total_odds_U,
    FD_total_odds_O,  FD_total_odds_U,
    BMGM_total_odds_O, BMGM_total_odds_U,
    FNT_total_odds_O,  FNT_total_odds_U
  ) %>%
  distinct(espn_game_id, team, .keep_all = TRUE)   # <-- KEEP BOTH SIDES PER GAME

# join into MC df by espn_game_id + team
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  left_join(
    bst_lookup,
    by = c(
      "ESPN_GAME_ID" = "espn_game_id",
      "TEAM"         = "team"
    )
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: BaseStats_Team column pull in section  ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Calculate Tie Changes Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# --- ADD: T_TIE_CHNG_Q1..Q6 and T_TIE_CHNG_CGS (times the game transitions INTO a tie) ---

# build per-quarter tie-enter counts (Q1-Q4 + OT1(qtr=5) + OT2(qtr=6))
tie_by_qtr <-
  pm_df %>%
  mutate(
    hs  = suppressWarnings(as.integer(.data[["home_score"]])),
    as  = suppressWarnings(as.integer(.data[["away_score"]])),
    qtr = suppressWarnings(as.integer(qtr))
  ) %>%
  filter(qtr %in% 1:6) %>%                                   # include OT1/OT2
  arrange(game_id, qtr) %>%
  group_by(game_id, qtr) %>%
  mutate(
    diff    = hs - as,
    is_tie  = !is.na(diff) & diff == 0L,
    tie_in  = is_tie & dplyr::lag(!is_tie, default = FALSE)  # transition into a tie
  ) %>%
  summarise(ties = sum(tie_in, na.rm = TRUE), .groups = "drop") %>%
  group_by(game_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(ties = 0L)) %>%     # ensure all qtrs exist
  ungroup() %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = ties, values_fill = 0L,
    names_prefix = "T_TIE_CHNG_Q"
  )

# build full-game (CGS) tie-enter counts (includes any OTs present)
tie_cgs <-
  pm_df %>%
  mutate(
    hs = suppressWarnings(as.integer(.data[["home_score"]])),
    as = suppressWarnings(as.integer(.data[["away_score"]]))
  ) %>%
  arrange(game_id) %>%
  group_by(game_id) %>%
  mutate(
    diff   = hs - as,
    is_tie = !is.na(diff) & diff == 0L,
    tie_in = is_tie & dplyr::lag(!is_tie, default = FALSE)
  ) %>%
  summarise(T_TIE_CHNG_CGS = sum(tie_in, na.rm = TRUE), .groups = "drop")

# join into BaseStats_Team_MC (2 rows per game)
BaseStats_Team_MC <-
  BaseStats_Team_MC %>%
  left_join(tie_by_qtr, by = c("ESPN_GAME_ID" = "game_id")) %>%
  left_join(tie_cgs,   by = c("ESPN_GAME_ID" = "game_id")) %>%
  mutate(
    T_TIE_CHNG_Q1  = dplyr::coalesce(`T_TIE_CHNG_Q1`, 0L),
    T_TIE_CHNG_Q2  = dplyr::coalesce(`T_TIE_CHNG_Q2`, 0L),
    T_TIE_CHNG_Q3  = dplyr::coalesce(`T_TIE_CHNG_Q3`, 0L),
    T_TIE_CHNG_Q4  = dplyr::coalesce(`T_TIE_CHNG_Q4`, 0L),
    # Only expose OT columns when the game actually had those OTs
    T_TIE_CHNG_Q5  = if_else(OT_1 == 1L, dplyr::coalesce(`T_TIE_CHNG_Q5`, 0L), 0L),
    T_TIE_CHNG_Q6  = if_else(OT_2 == 1L, dplyr::coalesce(`T_TIE_CHNG_Q6`, 0L), 0L),
    T_TIE_CHNG_CGS = dplyr::coalesce(T_TIE_CHNG_CGS, 0L)
  )


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Calculate Tie Changes Data Aggregation ====
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



# === START: TOP-LEVEL Possessions Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#     ooooooooo.                                                            o8o                                 
#     `888   `Y88.                                                          `"'                                 
#      888   .d88'  .ooooo.   .oooo.o  .oooo.o  .ooooo.   .oooo.o  .oooo.o oooo   .ooooo.  ooo. .oo.    .oooo.o 
#      888ooo88P'  d88' `88b d88(  "8 d88(  "8 d88' `88b d88(  "8 d88(  "8 `888  d88' `88b `888P"Y88b  d88(  "8 
#      888         888   888 `"Y88b.  `"Y88b.  888ooo888 `"Y88b.  `"Y88b.   888  888   888  888   888  `"Y88b.  
#      888         888   888 o.  )88b o.  )88b 888    .o o.  )88b o.  )88b  888  888   888  888   888  o.  )88b 
#     o888o        `Y8bod8P' 8""888P' 8""888P' `Y8bod8P' 8""888P' 8""888P' o888o `Y8bod8P' o888o o888o 8""888P' 
#                                                                                                          
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                                                                                          
                                                                                                          

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Total Possessions Logic Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- Team Possessions by Quarter (Q1–Q6) and Complete Game (CGS) ---
# Per your spec: count how many times each team_id appears in pm_df (by quarter / full game)

# 🏀🏀🏀🏀 START: Total Possessions Logic Identification 🏀🏀🏀🏀

# list of type_ids to exclude from possession counts
exclude_types <- c(
  # --- Free Throws ---
  97,98,99,100,101,102,103,
  104,105,106,107,108,
  157,165,166,
  
  # --- Substitutions ---
  584,
  
  # --- End of period/game (do NOT count as possession events) ---
  412,402,
  
  # --- Reviews / Challenges ---
  213,214,215,216,277,278,279,280,
  
  # --- Fouls (do NOT end possessions except offensive, but we keep TO events separately) ---
  22,24,32,31,36,37,
  40,43,44,45,
  
  # --- Technicals / Delay / Admin ---
  8,25,28,29,30,33,35,47,48,517,
  
  # --- Rebounds (never possessions) ---
  155,156,
  
  # --- Timeouts ---
  16,
  
  # --- Violations that don't end possession directly ---
  10,11,12,13,
  
  # --- Misc non-possession events ---
  70,71,72,73,74  # note: some ARE turnovers but PBP feeds separate them cleanly already
)


# ensure qtr numeric
pm_df$qtr <- suppressWarnings(as.integer(pm_df$qtr))

# --- Team Possessions by Quarter (Q1–Q6) and Complete Game (CGS) ---
# Filter out excluded event types BEFORE counting
poss_by_qtr <-
  pm_df %>%
  filter(qtr %in% 1:6) %>%
  filter(!type_id %in% exclude_types) %>%        # <–– NEW LINE
  group_by(game_id, team_id, qtr) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(cnt = 0L)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = qtr, values_from = cnt, values_fill = 0L,
    names_prefix = "T_POSS_Q"
  )

# --- Complete Game Possessions (same filter applied)
poss_cgs <-
  pm_df %>%
  filter(!type_id %in% exclude_types) %>%        # <–– NEW LINE
  group_by(game_id, team_id) %>%
  summarise(T_POSS_CGS = n(), .groups = "drop")

# join into BaseStats_Team_MC exactly as before
BaseStats_Team_MC <-
  BaseStats_Team_MC %>%
  left_join(poss_by_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(poss_cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  mutate(
    T_POSS_Q1  = dplyr::coalesce(`T_POSS_Q1`, 0L),
    T_POSS_Q2  = dplyr::coalesce(`T_POSS_Q2`, 0L),
    T_POSS_Q3  = dplyr::coalesce(`T_POSS_Q3`, 0L),
    T_POSS_Q4  = dplyr::coalesce(`T_POSS_Q4`, 0L),
    T_POSS_Q5  = if_else(OT_1 == 1L, dplyr::coalesce(`T_POSS_Q5`, 0L), 0L),
    T_POSS_Q6  = if_else(OT_2 == 1L, dplyr::coalesce(`T_POSS_Q6`, 0L), 0L),
    T_POSS_CGS = dplyr::coalesce(T_POSS_CGS, 0L)
  )

# 🏀🏀🏀🏀 END: Total Possessions Logic Identification 🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Total Possessions Logic Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Possessions Lead/Neut/Trail Logic Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# TEAM POSSESSIONS: LEAD / TRAIL / NEUTRAL (Q1–Q4 & CGS)
# and OFF/DEF POSSESSION SUCCESS (Q1–Q4 & CGS)
# Appends to existing pm_df (which already has OFFTEAM_SUCCESS / DEFTEAM_SUCCESS)
# and joins into BaseStats_Team_MC (2 rows per game).
# ===============================

# --- Prep: coerce types & per-row team-relative diff ---
pm_df <- pm_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    hs  = suppressWarnings(as.integer(home_score)),
    as  = suppressWarnings(as.integer(away_score)),
    team_side = dplyr::case_when(
      team_id == home_team_id ~ "home",
      team_id == away_team_id ~ "away",
      TRUE ~ NA_character_
    ),
    # team-relative score difference at this play
    team_diff = dplyr::case_when(
      team_side == "home" ~ hs - as,
      team_side == "away" ~ as - hs,
      TRUE ~ NA_integer_
    ),
    lead_flag  = as.integer(!is.na(team_diff) & team_diff > 0L),
    trail_flag = as.integer(!is.na(team_diff) & team_diff < 0L),
    neut_flag  = as.integer(!is.na(team_diff) & team_diff == 0L),
    # opponent team_id for this play (used for DEF success)
    opp_team_id = dplyr::case_when(
      team_side == "home" ~ away_team_id,
      team_side == "away" ~ home_team_id,
      TRUE ~ NA_character_
    )
  )

# ---------- LEAD / TRAIL / NEUTRAL COUNTS BY QUARTER (Q1–Q6) ----------
leadtrail_qtr <-
  pm_df %>%
  filter(qtr %in% 1:6) %>%
  group_by(game_id, team_id, qtr) %>%
  summarise(
    T_POSS_LEAD  = sum(lead_flag,  na.rm = TRUE),
    T_POSS_TRAIL = sum(trail_flag, na.rm = TRUE),
    T_POSS_NEUT  = sum(neut_flag,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6,
                  fill = list(T_POSS_LEAD = 0L, T_POSS_TRAIL = 0L, T_POSS_NEUT = 0L)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = starts_with("T_POSS_"), names_to = "kind", values_to = "val") %>%
  tidyr::unite("stat", kind, qtr, sep = "_Q") %>%
  tidyr::pivot_wider(names_from = stat, values_from = val, values_fill = 0L)

# ---------- LEAD / TRAIL / NEUTRAL COUNTS (COMPLETE GAME) ----------
leadtrail_cgs <-
  pm_df %>%
  group_by(game_id, team_id) %>%
  summarise(
    T_POSS_LEAD_CGS  = sum(lead_flag,  na.rm = TRUE),
    T_POSS_TRAIL_CGS = sum(trail_flag, na.rm = TRUE),
    T_POSS_NEUT_CGS  = sum(neut_flag,  na.rm = TRUE),
    .groups = "drop"
  )

# ---------- JOIN INTO BaseStats_Team_MC & MASK OT COLUMNS ----------
BaseStats_Team_MC <-
  BaseStats_Team_MC %>%
  left_join(leadtrail_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(leadtrail_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  mutate(
    # Q1–Q4 always present
    T_POSS_LEAD_Q1  = dplyr::coalesce(T_POSS_LEAD_Q1,  0L),
    T_POSS_LEAD_Q2  = dplyr::coalesce(T_POSS_LEAD_Q2,  0L),
    T_POSS_LEAD_Q3  = dplyr::coalesce(T_POSS_LEAD_Q3,  0L),
    T_POSS_LEAD_Q4  = dplyr::coalesce(T_POSS_LEAD_Q4,  0L),
    T_POSS_TRAIL_Q1 = dplyr::coalesce(T_POSS_TRAIL_Q1, 0L),
    T_POSS_TRAIL_Q2 = dplyr::coalesce(T_POSS_TRAIL_Q2, 0L),
    T_POSS_TRAIL_Q3 = dplyr::coalesce(T_POSS_TRAIL_Q3, 0L),
    T_POSS_TRAIL_Q4 = dplyr::coalesce(T_POSS_TRAIL_Q4, 0L),
    T_POSS_NEUT_Q1  = dplyr::coalesce(T_POSS_NEUT_Q1,  0L),
    T_POSS_NEUT_Q2  = dplyr::coalesce(T_POSS_NEUT_Q2,  0L),
    T_POSS_NEUT_Q3  = dplyr::coalesce(T_POSS_NEUT_Q3,  0L),
    T_POSS_NEUT_Q4  = dplyr::coalesce(T_POSS_NEUT_Q4,  0L),
    
    # OT columns only if game had those OTs
    T_POSS_LEAD_Q5  = if_else(OT_1 == 1L, dplyr::coalesce(T_POSS_LEAD_Q5,  0L), 0L),
    T_POSS_TRAIL_Q5 = if_else(OT_1 == 1L, dplyr::coalesce(T_POSS_TRAIL_Q5, 0L), 0L),
    T_POSS_NEUT_Q5  = if_else(OT_1 == 1L, dplyr::coalesce(T_POSS_NEUT_Q5,  0L), 0L),
    
    T_POSS_LEAD_Q6  = if_else(OT_2 == 1L, dplyr::coalesce(T_POSS_LEAD_Q6,  0L), 0L),
    T_POSS_TRAIL_Q6 = if_else(OT_2 == 1L, dplyr::coalesce(T_POSS_TRAIL_Q6, 0L), 0L),
    T_POSS_NEUT_Q6  = if_else(OT_2 == 1L, dplyr::coalesce(T_POSS_NEUT_Q6,  0L), 0L),
    
    # CGS always present
    T_POSS_LEAD_CGS  = dplyr::coalesce(T_POSS_LEAD_CGS,  0L),
    T_POSS_TRAIL_CGS = dplyr::coalesce(T_POSS_TRAIL_CGS, 0L),
    T_POSS_NEUT_CGS  = dplyr::coalesce(T_POSS_NEUT_CGS,  0L)
  )


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Possessions Lead/Neut/Trail Logic Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Possessions OFF and DEF POSS SUCCESS Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ---------- OFFENSIVE / DEFENSIVE POSSESSION SUCCESS BY QUARTER ----------
# Offensive success credited to the acting team (team_id)
off_succ_qtr <-
  pm_df %>%
  filter(qtr %in% 1:4) %>%
  group_by(game_id, team_id, qtr) %>%
  summarise(val = sum(OFFTEAM_SUCCESS, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = val, values_fill = 0L,
    names_prefix = "T_POSS_OFF_SUCC_Q"
  )

# Defensive success credited to the opponent (opp_team_id)
def_succ_qtr <-
  pm_df %>%
  filter(qtr %in% 1:4) %>%
  group_by(game_id, team_id = opp_team_id, qtr) %>%
  summarise(val = sum(DEFTEAM_SUCCESS, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = val, values_fill = 0L,
    names_prefix = "T_POSS_DEF_SUCC_Q"
  )

# ---------- OFFENSIVE / DEFENSIVE POSSESSION SUCCESS (COMPLETE GAME) ----------
off_succ_cgs <-
  pm_df %>%
  group_by(game_id, team_id) %>%
  summarise(T_POSS_OFF_SUCC_CGS = sum(OFFTEAM_SUCCESS, na.rm = TRUE), .groups = "drop")

def_succ_cgs <-
  pm_df %>%
  group_by(game_id, team_id = opp_team_id) %>%
  summarise(T_POSS_DEF_SUCC_CGS = sum(DEFTEAM_SUCCESS, na.rm = TRUE), .groups = "drop")

# ---------- JOIN EVERYTHING INTO BaseStats_Team_MC ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  left_join(off_succ_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(def_succ_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(off_succ_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(def_succ_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  mutate(
    across(
      c(starts_with("T_POSS_OFF_SUCC_Q"), starts_with("T_POSS_DEF_SUCC_Q"),
        T_POSS_OFF_SUCC_CGS, T_POSS_DEF_SUCC_CGS),
      ~ dplyr::coalesce(., 0L)
    )
  )


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Possessions OFF and DEF POSS SUCCESS Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Possessions TOT and AVG Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# TEAM POSSESSION TIME (TOTAL & AVERAGE)
# T_TOT_POSS_TIME_Q1..Q4, T_TOT_POSS_TIME_CGS (seconds)
# T_AVG_POSS_TIME_Q1..Q4, T_AVG_POSS_TIME_CGS (seconds)
# ===============================

# ---- sanity: required columns ----
# ---- sanity: required columns ----
stopifnot(all(c("game_id","qtr","team_id","clock_minutes","clock_seconds") %in% names(pm_df)))

# ---- helper: build possession segments for ONE (game_id, qtr) slice ----
# RETURN: only team_id, seg_sec   (group_modify will reattach game_id,qtr)
build_poss_segments <- function(.x, quarter_start_sec = 12L * 60L) {
  df <- .x
  if (nrow(df) == 0L) return(tibble(team_id = character(), seg_sec = integer()))
  
  tmp <- df %>%
    mutate(
      t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
        suppressWarnings(as.integer(clock_seconds))
    ) %>%
    arrange(desc(t_sec)) %>%                 # quarter clock counts down
    mutate(
      prev_team = dplyr::lag(team_id),
      prev_t    = dplyr::lag(t_sec, default = quarter_start_sec),
      seg_sec   = as.integer(prev_t - t_sec),
      assign_to = prev_team
    ) %>%
    filter(!is.na(assign_to), seg_sec > 0L) %>%
    transmute(team_id = assign_to, seg_sec)
  
  # tail segment from last event to 0 for last team's possession
  last_row <- df %>%
    mutate(t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
             suppressWarnings(as.integer(clock_seconds))) %>%
    arrange(desc(t_sec)) %>% slice_tail(n = 1)
  
  tail_team <- last_row$team_id
  tail_seg  <- as.integer(last_row$t_sec - 0L)
  if (!is.na(tail_team) && tail_seg > 0L) {
    tmp <- bind_rows(tmp, tibble(team_id = tail_team, seg_sec = tail_seg))
  }
  
  tmp
}

# ---- build segments by quarter (Q1–Q6; OT1=5 min, OT2=5 min) ----
segments_qtr <-
  pm_df %>%
  mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  filter(qtr %in% 1:6) %>%
  group_by(game_id, qtr) %>%
  group_modify(~ build_poss_segments(.x, quarter_start_sec = ifelse(.y$qtr <= 4L, 12L * 60L, 5L * 60L))) %>%
  ungroup()

# ---- totals and averages by quarter ----
poss_time_qtr <-
  segments_qtr %>%
  group_by(game_id, team_id, qtr) %>%
  summarise(
    T_TOT_POSS_TIME = sum(seg_sec, na.rm = TRUE),
    T_AVG_POSS_TIME = ifelse(n() > 0, mean(seg_sec, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6,
                  fill = list(T_TOT_POSS_TIME = 0, T_AVG_POSS_TIME = 0)) %>%
  ungroup() %>%
  tidyr::pivot_longer(c(T_TOT_POSS_TIME, T_AVG_POSS_TIME), names_to = "metric", values_to = "val") %>%
  tidyr::unite("stat", metric, qtr, sep = "_Q") %>%
  tidyr::pivot_wider(names_from = stat, values_from = val, values_fill = 0)

# ---- build segments for complete game (all periods incl. OT) ----
segments_cgs <-
  pm_df %>%
  mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds))
  ) %>%
  arrange(game_id, qtr, desc(t_sec)) %>%
  group_by(game_id) %>%
  mutate(
    prev_team = dplyr::lag(team_id),
    prev_t    = dplyr::lag(t_sec, default = 12L * 60L),  # start of Q1
    seg_sec   = as.integer(prev_t - t_sec),
    assign_to = prev_team
  ) %>%
  filter(!is.na(assign_to), seg_sec > 0L) %>%
  select(game_id, team_id = assign_to, seg_sec) %>%
  ungroup()

# tail-from-final-event-to-0 per game
tail_cgs <-
  pm_df %>%
  mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds))
  ) %>%
  arrange(game_id, qtr, desc(t_sec)) %>%
  group_by(game_id) %>%
  slice_tail(n = 1) %>%
  transmute(game_id, team_id, seg_sec = as.integer(t_sec - 0L)) %>%
  ungroup()

segments_cgs <- bind_rows(segments_cgs, tail_cgs)

poss_time_cgs <-
  segments_cgs %>%
  group_by(game_id, team_id) %>%
  summarise(
    T_TOT_POSS_TIME_CGS = sum(seg_sec, na.rm = TRUE),
    T_AVG_POSS_TIME_CGS = ifelse(n() > 0, mean(seg_sec, na.rm = TRUE), 0),
    .groups = "drop"
  )

# ---- join into BaseStats_Team_MC ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  left_join(poss_time_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(poss_time_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  mutate(
    # Q1–Q4 always present
    T_TOT_POSS_TIME_Q1 = dplyr::coalesce(T_TOT_POSS_TIME_Q1, 0),
    T_TOT_POSS_TIME_Q2 = dplyr::coalesce(T_TOT_POSS_TIME_Q2, 0),
    T_TOT_POSS_TIME_Q3 = dplyr::coalesce(T_TOT_POSS_TIME_Q3, 0),
    T_TOT_POSS_TIME_Q4 = dplyr::coalesce(T_TOT_POSS_TIME_Q4, 0),
    T_AVG_POSS_TIME_Q1 = dplyr::coalesce(T_AVG_POSS_TIME_Q1, 0),
    T_AVG_POSS_TIME_Q2 = dplyr::coalesce(T_AVG_POSS_TIME_Q2, 0),
    T_AVG_POSS_TIME_Q3 = dplyr::coalesce(T_AVG_POSS_TIME_Q3, 0),
    T_AVG_POSS_TIME_Q4 = dplyr::coalesce(T_AVG_POSS_TIME_Q4, 0),
    
    # OT columns only if game had those OTs
    T_TOT_POSS_TIME_Q5 = if_else(OT_1 == 1L, dplyr::coalesce(T_TOT_POSS_TIME_Q5, 0), 0),
    T_AVG_POSS_TIME_Q5 = if_else(OT_1 == 1L, dplyr::coalesce(T_AVG_POSS_TIME_Q5, 0), 0),
    T_TOT_POSS_TIME_Q6 = if_else(OT_2 == 1L, dplyr::coalesce(T_TOT_POSS_TIME_Q6, 0), 0),
    T_AVG_POSS_TIME_Q6 = if_else(OT_2 == 1L, dplyr::coalesce(T_AVG_POSS_TIME_Q6, 0), 0),
    
    # CGS always present
    T_TOT_POSS_TIME_CGS = dplyr::coalesce(T_TOT_POSS_TIME_CGS, 0),
    T_AVG_POSS_TIME_CGS = dplyr::coalesce(T_AVG_POSS_TIME_CGS, 0)
  )


rm(poss_time_qtr, def_succ_cgs, def_succ_qtr, leadtrail_cgs, leadtrail_qtr, off_succ_cgs, off_succ_qtr, poss_by_qtr, poss_cgs, poss_time_cgs, poss_by_qtr, segments_cgs, segments_qtr, tail_cgs)
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Possessions TOT and AVG Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Pace Identification (per 48 possessions, using poss time) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- safe divide helper (prevents Inf, NaN, errors) ---
safe_div <- function(num, denom) {
  ifelse(is.na(denom) | denom == 0, 0, num / denom)
}


stopifnot(all(c("ESPN_GAME_ID","ESPN_TEAM_ID") %in% names(BaseStats_Team_MC)))

# 1) Clean possession-time inputs (seconds)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_TOT_POSS_TIME_Q[1-6]$|^T_AVG_POSS_TIME_Q[1-6]$|
                           ^T_TOT_POSS_TIME_CGS$|^T_AVG_POSS_TIME_CGS$"),
      ~ dplyr::coalesce(suppressWarnings(as.numeric(.)), 0)
    )
  )

# 2) Estimate # of possessions from total / average possession time
for (q in 1:6) {
  tot_col <- paste0("T_TOT_POSS_TIME_Q", q)
  avg_col <- paste0("T_AVG_POSS_TIME_Q", q)
  est_col <- paste0("T_EST_POSS_Q",  q)
  
  BaseStats_Team_MC[[est_col]] <-
    safe_div(BaseStats_Team_MC[[tot_col]], BaseStats_Team_MC[[avg_col]])
}

BaseStats_Team_MC[["T_EST_POSS_CGS"]] <-
  safe_div(BaseStats_Team_MC[["T_TOT_POSS_TIME_CGS"]],
           BaseStats_Team_MC[["T_AVG_POSS_TIME_CGS"]])

# 3) Build opponent possession snapshot (same table, flipped team)
opp_poss_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    OPPONENT_TEAM_ID = ESPN_TEAM_ID,
    dplyr::starts_with("T_EST_POSS_Q"),
    T_EST_POSS_CGS
  ) %>%
  dplyr::rename_with(
    ~ gsub("^T_EST_POSS_Q", "OPP_EST_POSS_Q", .x),
    .cols = dplyr::starts_with("T_EST_POSS_Q")
  ) %>%
  dplyr::rename(OPP_EST_POSS_CGS = T_EST_POSS_CGS)

# 4) Join opponent estimated possessions onto each team row
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::inner_join(opp_poss_df, by = "ESPN_GAME_ID") %>%
  dplyr::filter(ESPN_TEAM_ID != OPPONENT_TEAM_ID)

# 5) Compute pace per 48 for each period (Q1–Q6)
for (q in 1:6) {
  team_col <- paste0("T_EST_POSS_Q",  q)
  opp_col  <- paste0("OPP_EST_POSS_Q", q)
  pace_col <- paste0("T_PACE_Q",       q)
  
  # 12 min for Q1–Q4, 5 min for OT1/OT2
  period_minutes <- ifelse(q <= 4, 12, 5)
  
  BaseStats_Team_MC[[pace_col]] <-
    48 * (BaseStats_Team_MC[[team_col]] + BaseStats_Team_MC[[opp_col]]) /
    (2 * period_minutes)
}

# 6) Game-level pace per 48 (CGS) with OT adjustment
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    GAME_MINUTES =
      48 +
      5 * dplyr::coalesce(OT_1, 0L) +
      5 * dplyr::coalesce(OT_2, 0L),
    T_PACE_CGS =
      48 * (T_EST_POSS_CGS + OPP_EST_POSS_CGS) / (2 * GAME_MINUTES)
  )

# 7) Optional cleanup: drop helper cols
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::select(
    -OPPONENT_TEAM_ID,
    -dplyr::starts_with("T_EST_POSS_Q"),
    -T_EST_POSS_CGS,
    -dplyr::starts_with("OPP_EST_POSS_Q"),
    -OPP_EST_POSS_CGS,
    -GAME_MINUTES
  )

rm(opp_poss_df)

message("[✓] Pace (T_PACE_Q1–Q6, T_PACE_CGS) computed from possession time.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Pace Identification (per 48 possessions, using poss time) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: TOP-LEVEL Possessions Data Aggregation Section ====





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

# ===============================
# TEAM POINTS & BONUS POINTS
# T_PTS_Q1..Q4, T_PTS_CGS
# T_BON_PTS_Q1..Q4, T_BON_PTS_CGS
# ===============================

# ------- Safety: needed columns -------
stopifnot(all(c("game_id","qtr","team_id","home_team_id","away_team_id",
                "home_score","away_score","type_text","clock_minutes","clock_seconds",
                "score_value") %in% names(pm_df)))

# ======================================
# A) TEAM POINTS BY QUARTER + CGS (Q1–Q6) via type_text == "End Period"
# ======================================

# End-of-period snapshots (Q1–Q4 + OT1/OT2) from type_text
eoq <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    home_score = suppressWarnings(as.integer(home_score)),
    away_score = suppressWarnings(as.integer(away_score))
  ) %>%
  dplyr::filter(type_text == "End Period", qtr %in% 1:6) %>%
  dplyr::arrange(game_id, qtr, clock_minutes, clock_seconds) %>%
  dplyr::group_by(game_id, qtr) %>%
  dplyr::slice_tail(n = 1) %>%  # in case multiple "End Period" lines exist
  dplyr::ungroup() %>%
  dplyr::transmute(game_id, qtr, home_team_id, away_team_id, home_score, away_score)

# Points at end of each period for each team (Q1–Q6)
pts_by_qtr <- dplyr::bind_rows(
  eoq %>% dplyr::transmute(game_id, team_id = home_team_id, qtr, pts = home_score),
  eoq %>% dplyr::transmute(game_id, team_id = away_team_id, qtr, pts = away_score)
) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(pts = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = pts, values_fill = 0L,
    names_prefix = "T_PTS_Q"
  )


# Final game points (CGS) = snapshot from highest period present (Q4, or OT1, or OT2)
finals <- eoq %>%
  dplyr::group_by(game_id) %>%
  dplyr::slice_max(qtr, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

pts_cgs <- dplyr::bind_rows(
  finals %>% dplyr::transmute(game_id, team_id = home_team_id, T_PTS_CGS = home_score),
  finals %>% dplyr::transmute(game_id, team_id = away_team_id, T_PTS_CGS = away_score)
) %>%
  dplyr::distinct() %>%
  dplyr::mutate(T_PTS_CGS = dplyr::coalesce(T_PTS_CGS, 0L))

# Join into BaseStats_Team_MC and gate OT columns by OT flags
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(pts_by_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(pts_cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_PTS_Q1  = dplyr::coalesce(T_PTS_Q1, 0L),
    T_PTS_Q2  = dplyr::coalesce(T_PTS_Q2, 0L),
    T_PTS_Q3  = dplyr::coalesce(T_PTS_Q3, 0L),
    T_PTS_Q4  = dplyr::coalesce(T_PTS_Q4, 0L),
    # OT periods only when flags indicate OT happened
    T_PTS_Q5  = if_else(OT_1 == 1L, dplyr::coalesce(T_PTS_Q5, 0L), 0L),
    T_PTS_Q6  = if_else(OT_2 == 1L, dplyr::coalesce(T_PTS_Q6, 0L), 0L),
    # CGS
    T_PTS_CGS = dplyr::coalesce(T_PTS_CGS, 0L)
  )

# Add right after your PTS-by-quarter join/mutate
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # ensure NAs are 0 before diffs
    T_PTS_Q1 = dplyr::coalesce(T_PTS_Q1, 0L),
    T_PTS_Q2 = dplyr::coalesce(T_PTS_Q2, 0L),
    T_PTS_Q3 = dplyr::coalesce(T_PTS_Q3, 0L),
    T_PTS_Q4 = dplyr::coalesce(T_PTS_Q4, 0L),
    T_PTS_Q5 = dplyr::coalesce(T_PTS_Q5, 0L),
    T_PTS_Q6 = dplyr::coalesce(T_PTS_Q6, 0L),
    
    T_PTS_SCORED_Q1 = T_PTS_Q1,
    T_PTS_SCORED_Q2 = pmax(T_PTS_Q2 - T_PTS_Q1, 0L),
    T_PTS_SCORED_Q3 = pmax(T_PTS_Q3 - T_PTS_Q2, 0L),
    T_PTS_SCORED_Q4 = pmax(T_PTS_Q4 - T_PTS_Q3, 0L),
    
    # OT quarters only if the game had them
    T_PTS_SCORED_Q5 = dplyr::if_else(OT_1 == 1L, pmax(T_PTS_Q5 - T_PTS_Q4, 0L), 0L),
    T_PTS_SCORED_Q6 = dplyr::if_else(OT_2 == 1L, pmax(T_PTS_Q6 - T_PTS_Q5, 0L), 0L),
    
    # Full game scored = final total (or rowSums of Q-scored if you prefer)
    T_PTS_SCORED_CGS = dplyr::coalesce(T_PTS_CGS, 0L)
  )


# ======================================
# B) TEAM BONUS POINTS
# ======================================

# ------- Bonus PTS (add OT handling via qtr 5/6 + gate by OT_1/OT_2) -------

pb_bonus <- pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds))
  ) %>%
  dplyr::group_by(game_id, qtr) %>%
  dplyr::arrange(dplyr::desc(t_sec), .by_group = TRUE) %>%
  dplyr::mutate(evt = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    foul_flag = type_text %in% c("Personal Foul","Shooting Foul","Loose Ball Foul"),
    opp_id = dplyr::case_when(
      team_id == home_team_id ~ away_team_id,
      team_id == away_team_id ~ home_team_id,
      TRUE ~ NA_character_
    ),
    score_value_int = suppressWarnings(as.integer(score_value))
  )

foul_threshold_qtr <- pb_bonus %>%
  dplyr::filter(qtr %in% 1:6, foul_flag) %>%                            # <- include OT1/OT2
  dplyr::group_by(game_id, qtr, team_id) %>%
  dplyr::mutate(cum_fouls = cumsum(foul_flag)) %>%
  dplyr::filter(cum_fouls >= 5L) %>%
  dplyr::summarise(threshold_evt = min(evt), .groups = "drop")

bonus_pts_qtr <- pb_bonus %>%
  dplyr::filter(qtr %in% 1:6) %>%                                       # <- include OT1/OT2
  dplyr::left_join(
    foul_threshold_qtr %>% dplyr::rename(opp_id = team_id, threshold_evt = threshold_evt),
    by = c("game_id", "qtr", "opp_id")
  ) %>%
  dplyr::group_by(game_id, qtr, team_id) %>%
  dplyr::summarise(
    bon = sum(!is.na(threshold_evt) & evt > threshold_evt & score_value_int == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(bon = 0L)) %>%                 # <- ensure Q1..Q6 exist
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = bon, values_fill = 0L,
    names_prefix = "T_BON_PTS_Q"
  )

bonus_pts_cgs <- bonus_pts_qtr %>%
  dplyr::mutate(T_BON_PTS_CGS = rowSums(dplyr::across(starts_with("T_BON_PTS_Q")), na.rm = TRUE)) %>%
  dplyr::select(game_id, team_id, T_BON_PTS_CGS)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(bonus_pts_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(bonus_pts_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    T_BON_PTS_Q1 = dplyr::coalesce(T_BON_PTS_Q1, 0L),
    T_BON_PTS_Q2 = dplyr::coalesce(T_BON_PTS_Q2, 0L),
    T_BON_PTS_Q3 = dplyr::coalesce(T_BON_PTS_Q3, 0L),
    T_BON_PTS_Q4 = dplyr::coalesce(T_BON_PTS_Q4, 0L),
    T_BON_PTS_Q5 = if_else(OT_1 == 1L, dplyr::coalesce(T_BON_PTS_Q5, 0L), 0L),  # <- gate by OT_1
    T_BON_PTS_Q6 = if_else(OT_2 == 1L, dplyr::coalesce(T_BON_PTS_Q6, 0L), 0L),  # <- gate by OT_2
    T_BON_PTS_CGS = dplyr::coalesce(T_BON_PTS_CGS, 0L)
  )

rm(bonus_pts_cgs, bonus_pts_qtr, foul_threshold_qtr, pts_by_qtr, pts_cgs, eoq, pb_bonus)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Scoring Overall Points Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Raw Field Goal Data Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ===============================
# TEAM FIELD GOALS: ATTEMPTS, MAKES, PERCENTAGE
# T_FGA_Q1..Q6, T_FGA_CGS
# T_FGM_Q1..Q6, T_FGM_CGS
# T_FG_PCT_Q1..Q6, T_FG_PCT_CGS
# ===============================

# safety: required columns
stopifnot(all(c("game_id","qtr","team_id","shooting_play","scoring_play","type_text") %in% names(pm_df)))

# normalize logical-like flags that may be "TRUE"/"FALSE"/1/0
to_bool <- function(x) {
  xv <- tolower(as.character(x))
  xv %in% c("true","t","1","yes","y")
}

# -------------------------------
# A) Field Goal Attempts (FGA)
# -------------------------------
fga_base <- pm_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    is_shot = to_bool(shooting_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  )


# quarter counts (Q1–Q6; ensure Q5/Q6 exist)
fga_qtr <- fga_base %>%
  filter(qtr %in% 1:6) %>%
  group_by(game_id, team_id, qtr) %>%
  summarise(T_FGA = sum(is_shot, na.rm = TRUE), .groups = "drop") %>%
  group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_FGA = 0L)) %>%
  ungroup() %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = T_FGA, values_fill = 0L,
    names_prefix = "T_FGA_Q"
  )

# complete-game (include OT)
fga_cgs <- fga_base %>%
  group_by(game_id, team_id) %>%
  summarise(T_FGA_CGS = sum(is_shot, na.rm = TRUE), .groups = "drop")

# -------------------------------
# B) Field Goals Made (FGM)
#   - exclude Free Throws
#   - use scoring_play == TRUE
# -------------------------------
fgm_base <- pm_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    is_make = to_bool(scoring_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  )

# quarter counts (Q1–Q6; ensure Q5/Q6 exist)
fgm_qtr <- fgm_base %>%
  filter(qtr %in% 1:6) %>%
  group_by(game_id, team_id, qtr) %>%
  summarise(T_FGM = sum(is_make, na.rm = TRUE), .groups = "drop") %>%
  group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_FGM = 0L)) %>%
  ungroup() %>%
  tidyr::pivot_wider(
    names_from = qtr, values_from = T_FGM, values_fill = 0L,
    names_prefix = "T_FGM_Q"
  )

# complete-game (include OT)
fgm_cgs <- fgm_base %>%
  group_by(game_id, team_id) %>%
  summarise(T_FGM_CGS = sum(is_make, na.rm = TRUE), .groups = "drop")

# -------------------------------
# C) Join into BaseStats_Team_MC and compute FG%
# -------------------------------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  left_join(fga_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(fga_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(fgm_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  left_join(fgm_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  mutate(
    # Q1–Q4 always present
    T_FGA_Q1 = coalesce(T_FGA_Q1,0L), T_FGA_Q2 = coalesce(T_FGA_Q2,0L),
    T_FGA_Q3 = coalesce(T_FGA_Q3,0L), T_FGA_Q4 = coalesce(T_FGA_Q4,0L),
    T_FGM_Q1 = coalesce(T_FGM_Q1,0L), T_FGM_Q2 = coalesce(T_FGM_Q2,0L),
    T_FGM_Q3 = coalesce(T_FGM_Q3,0L), T_FGM_Q4 = coalesce(T_FGM_Q4,0L),
    
    # OT stats appear only if OT happened; otherwise force 0
    T_FGA_Q5 = if_else(OT_1 == 1L, coalesce(T_FGA_Q5,0L), 0L),
    T_FGA_Q6 = if_else(OT_2 == 1L, coalesce(T_FGA_Q6,0L), 0L),
    T_FGM_Q5 = if_else(OT_1 == 1L, coalesce(T_FGM_Q5,0L), 0L),
    T_FGM_Q6 = if_else(OT_2 == 1L, coalesce(T_FGM_Q6,0L), 0L),
    
    T_FGA_CGS = coalesce(T_FGA_CGS,0L),
    T_FGM_CGS = coalesce(T_FGM_CGS,0L)
  ) %>%
  mutate(
    T_FG_PCT_Q1  = ifelse(T_FGA_Q1  > 0, T_FGM_Q1  / T_FGA_Q1 , NA_real_),
    T_FG_PCT_Q2  = ifelse(T_FGA_Q2  > 0, T_FGM_Q2  / T_FGA_Q2 , NA_real_),
    T_FG_PCT_Q3  = ifelse(T_FGA_Q3  > 0, T_FGM_Q3  / T_FGA_Q3 , NA_real_),
    T_FG_PCT_Q4  = ifelse(T_FGA_Q4  > 0, T_FGM_Q4  / T_FGA_Q4 , NA_real_),
    
    # For OT periods: show % only if that OT occurred; otherwise NA
    T_FG_PCT_Q5  = if_else(OT_1 == 1L & T_FGA_Q5 > 0, T_FGM_Q5 / T_FGA_Q5, NA_real_),
    T_FG_PCT_Q6  = if_else(OT_2 == 1L & T_FGA_Q6 > 0, T_FGM_Q6 / T_FGA_Q6, NA_real_),
    
    T_FG_PCT_CGS = ifelse(T_FGA_CGS > 0, T_FGM_CGS / T_FGA_CGS, NA_real_)
  )

rm(fga_base, fga_cgs, fga_qtr, fgm_base, fgm_cgs, fgm_qtr)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Raw Field Goal Data Section Complete ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Free Throw Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ===============================
# FREE THROWS: ATTEMPTS, MAKES, FT%, FTR  (with OT support)
# T_FTA_Q1..Q6, T_FTA_CGS
# T_FTM_Q1..Q6, T_FTM_CGS
# T_FT_PCT_Q1..Q6, T_FT_PCT_CGS
# T_FTR_Q1..Q6, T_FTR_CGS
# ===============================

stopifnot(all(c("game_id","qtr","team_id","shooting_play","scoring_play") %in% names(pm_df)))
to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

has_type_id <- "type_id" %in% names(pm_df) || "play_type_id" %in% names(pm_df)
type_id_col <- if ("type_id" %in% names(pm_df)) "type_id" else if ("play_type_id" %in% names(pm_df)) "play_type_id" else NA_character_

ft_attempt_base <- pm_df %>%
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
  )

# ---------- FTA: quarter (Q1–Q6) + complete game ----------
fta_qtr <- ft_attempt_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(T_FTA = sum(is_fta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_FTA = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = qtr, values_from = T_FTA, values_fill = 0L,
                     names_prefix = "T_FTA_Q")

fta_cgs <- ft_attempt_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(T_FTA_CGS = sum(is_fta, na.rm = TRUE), .groups = "drop")

# ---------- FTM: quarter (Q1–Q6) + complete game ----------
ftm_qtr <- ft_attempt_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(T_FTM = sum(is_ftm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_FTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = qtr, values_from = T_FTM, values_fill = 0L,
                     names_prefix = "T_FTM_Q")

ftm_cgs <- ft_attempt_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(T_FTM_CGS = sum(is_ftm, na.rm = TRUE), .groups = "drop")

# ---------- Join & compute FT% and FTR (FTA/FGA) ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(fta_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(fta_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(ftm_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(ftm_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_FTA_Q1 = coalesce(T_FTA_Q1,0L), T_FTA_Q2 = coalesce(T_FTA_Q2,0L),
    T_FTA_Q3 = coalesce(T_FTA_Q3,0L), T_FTA_Q4 = coalesce(T_FTA_Q4,0L),
    T_FTM_Q1 = coalesce(T_FTM_Q1,0L), T_FTM_Q2 = coalesce(T_FTM_Q2,0L),
    T_FTM_Q3 = coalesce(T_FTM_Q3,0L), T_FTM_Q4 = coalesce(T_FTM_Q4,0L),
    
    # OT (only if occurred)
    T_FTA_Q5 = if_else(OT_1 == 1L, coalesce(T_FTA_Q5,0L), 0L),
    T_FTA_Q6 = if_else(OT_2 == 1L, coalesce(T_FTA_Q6,0L), 0L),
    T_FTM_Q5 = if_else(OT_1 == 1L, coalesce(T_FTM_Q5,0L), 0L),
    T_FTM_Q6 = if_else(OT_2 == 1L, coalesce(T_FTM_Q6,0L), 0L),
    
    T_FTA_CGS = coalesce(T_FTA_CGS,0L),
    T_FTM_CGS = coalesce(T_FTM_CGS,0L)
  ) %>%
  dplyr::mutate(
    # FT%
    T_FT_PCT_Q1 = ifelse(T_FTA_Q1>0, T_FTM_Q1/T_FTA_Q1, NA_real_),
    T_FT_PCT_Q2 = ifelse(T_FTA_Q2>0, T_FTM_Q2/T_FTA_Q2, NA_real_),
    T_FT_PCT_Q3 = ifelse(T_FTA_Q3>0, T_FTM_Q3/T_FTA_Q3, NA_real_),
    T_FT_PCT_Q4 = ifelse(T_FTA_Q4>0, T_FTM_Q4/T_FTA_Q4, NA_real_),
    T_FT_PCT_Q5 = if_else(OT_1==1L & T_FTA_Q5>0, T_FTM_Q5/T_FTA_Q5, NA_real_),
    T_FT_PCT_Q6 = if_else(OT_2==1L & T_FTA_Q6>0, T_FTM_Q6/T_FTA_Q6, NA_real_),
    T_FT_PCT_CGS = ifelse(T_FTA_CGS>0, T_FTM_CGS/T_FTA_CGS, NA_real_),
    
    # FTR = FTA/FGA  (assumes T_FGA_* already present)
    T_FTR_Q1 = ifelse(T_FGA_Q1>0, T_FTA_Q1/T_FGA_Q1, NA_real_),
    T_FTR_Q2 = ifelse(T_FGA_Q2>0, T_FTA_Q2/T_FGA_Q2, NA_real_),
    T_FTR_Q3 = ifelse(T_FGA_Q3>0, T_FTA_Q3/T_FGA_Q3, NA_real_),
    T_FTR_Q4 = ifelse(T_FGA_Q4>0, T_FTA_Q4/T_FGA_Q4, NA_real_),
    T_FTR_Q5 = if_else(OT_1==1L & T_FGA_Q5>0, T_FTA_Q5/T_FGA_Q5, NA_real_),
    T_FTR_Q6 = if_else(OT_2==1L & T_FGA_Q6>0, T_FTA_Q6/T_FGA_Q6, NA_real_),
    T_FTR_CGS = ifelse(T_FGA_CGS>0, T_FTA_CGS/T_FGA_CGS, NA_real_)
  )

rm(ft_attempt_base, fta_cgs, fta_qtr, ftm_cgs, ftm_qtr)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Free Throw Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# 2PT ATTEMPTS / MAKES / % / eFG / PTS / PPP / RATE / PTS SHARE  (with OT support)
# ===============================

stopifnot(all(c("game_id","qtr","team_id","type_text","scoring_play","score_value") %in% names(pm_df)))

zone_col <- dplyr::case_when(
  "shot_zone_basic" %in% names(pm_df) ~ "shot_zone_basic",
  "SHOT_ZONE_BASIC" %in% names(pm_df) ~ "SHOT_ZONE_BASIC",
  TRUE ~ NA_character_
)
stopifnot(!is.na(zone_col))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
zf <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# --- flag 2PT attempts and makes ---
twopt_base <- pm_df %>%
  dplyr::mutate(
    qtr     = suppressWarnings(as.integer(qtr)),
    is_2pta = to_bool(shooting_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE)) &
      !(.data[[zone_col]] %in% c("Above the Break 3","Right Corner 3","Left Corner 3")),
    is_2ptm = is_2pta & to_bool(scoring_play) & suppressWarnings(as.integer(score_value)) == 2L
  )

# --- per-quarter counts (Q1–Q6) ---
twopt_qtr <- twopt_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(T_2PTA = sum(is_2pta, na.rm = TRUE),
                   T_2PTM = sum(is_2ptm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_2PTA = 0L, T_2PTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_2PTA, T_2PTM),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# --- complete-game (CGS) counts ---
twopt_cgs <- twopt_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(T_2PTA_CGS = sum(is_2pta, na.rm = TRUE),
                   T_2PTM_CGS = sum(is_2ptm, na.rm = TRUE), .groups = "drop")

# --- join & mask OT, ensure supporting team totals exist ---
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(twopt_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(twopt_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_2PTA_Q1 = coalesce(T_2PTA_Q1,0L), T_2PTA_Q2 = coalesce(T_2PTA_Q2,0L),
    T_2PTA_Q3 = coalesce(T_2PTA_Q3,0L), T_2PTA_Q4 = coalesce(T_2PTA_Q4,0L),
    T_2PTM_Q1 = coalesce(T_2PTM_Q1,0L), T_2PTM_Q2 = coalesce(T_2PTM_Q2,0L),
    T_2PTM_Q3 = coalesce(T_2PTM_Q3,0L), T_2PTM_Q4 = coalesce(T_2PTM_Q4,0L),
    
    # OT masked by flags
    T_2PTA_Q5 = if_else(OT_1==1L, coalesce(T_2PTA_Q5,0L), 0L),
    T_2PTA_Q6 = if_else(OT_2==1L, coalesce(T_2PTA_Q6,0L), 0L),
    T_2PTM_Q5 = if_else(OT_1==1L, coalesce(T_2PTM_Q5,0L), 0L),
    T_2PTM_Q6 = if_else(OT_2==1L, coalesce(T_2PTM_Q6,0L), 0L),
    
    T_2PTA_CGS = coalesce(T_2PTA_CGS,0L),
    T_2PTM_CGS = coalesce(T_2PTM_CGS,0L),
    
    # Ensure team totals used downstream (FGA, PTS) exist
    T_FGA_Q1 = coalesce(T_FGA_Q1,0), T_FGA_Q2 = coalesce(T_FGA_Q2,0),
    T_FGA_Q3 = coalesce(T_FGA_Q3,0), T_FGA_Q4 = coalesce(T_FGA_Q4,0),
    T_FGA_Q5 = if_else(OT_1==1L, coalesce(T_FGA_Q5,0), 0),
    T_FGA_Q6 = if_else(OT_2==1L, coalesce(T_FGA_Q6,0), 0),
    T_FGA_CGS = coalesce(T_FGA_CGS,0),
    
    T_PTS_SCORED_Q1 = coalesce(T_PTS_SCORED_Q1,0), T_PTS_SCORED_Q2 = coalesce(T_PTS_SCORED_Q2,0),
    T_PTS_SCORED_Q3 = coalesce(T_PTS_SCORED_Q3,0), T_PTS_SCORED_Q4 = coalesce(T_PTS_SCORED_Q4,0),
    T_PTS_SCORED_Q5 = if_else(OT_1==1L, coalesce(T_PTS_SCORED_Q5,0), 0),
    T_PTS_SCORED_Q6 = if_else(OT_2==1L, coalesce(T_PTS_SCORED_Q6,0), 0),
    T_PTS_SCORED_CGS = coalesce(T_PTS_SCORED_CGS,0)
  )

# --- derived: % / eFG (same as %) / PTS / PPP / RATE / PTS share (incl. OT) ---
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # 2PT%
    T_2PT_PCT_Q1 = zf(T_2PTM_Q1, T_2PTA_Q1),
    T_2PT_PCT_Q2 = zf(T_2PTM_Q2, T_2PTA_Q2),
    T_2PT_PCT_Q3 = zf(T_2PTM_Q3, T_2PTA_Q3),
    T_2PT_PCT_Q4 = zf(T_2PTM_Q4, T_2PTA_Q4),
    T_2PT_PCT_Q5 = if_else(OT_1==1L, zf(T_2PTM_Q5, T_2PTA_Q5), NA_real_),
    T_2PT_PCT_Q6 = if_else(OT_2==1L, zf(T_2PTM_Q6, T_2PTA_Q6), NA_real_),
    T_2PT_PCT_CGS = zf(T_2PTM_CGS, T_2PTA_CGS),
    
    # eFG for 2s equals FG%
    T_2PT_EFG_PCT_Q1 = T_2PT_PCT_Q1,
    T_2PT_EFG_PCT_Q2 = T_2PT_PCT_Q2,
    T_2PT_EFG_PCT_Q3 = T_2PT_PCT_Q3,
    T_2PT_EFG_PCT_Q4 = T_2PT_PCT_Q4,
    T_2PT_EFG_PCT_Q5 = T_2PT_PCT_Q5,
    T_2PT_EFG_PCT_Q6 = T_2PT_PCT_Q6,
    T_2PT_EFG_PCT_CGS = T_2PT_PCT_CGS,
    
    # Points from 2s
    T_2PT_PTS_Q1 = 2 * T_2PTM_Q1,
    T_2PT_PTS_Q2 = 2 * T_2PTM_Q2,
    T_2PT_PTS_Q3 = 2 * T_2PTM_Q3,
    T_2PT_PTS_Q4 = 2 * T_2PTM_Q4,
    T_2PT_PTS_Q5 = 2 * T_2PTM_Q5,
    T_2PT_PTS_Q6 = 2 * T_2PTM_Q6,
    T_2PT_PTS_CGS = 2 * T_2PTM_CGS,
    
    # "Possessions" proxy = attempts
    T_2PT_POSS_Q1 = T_2PTA_Q1,
    T_2PT_POSS_Q2 = T_2PTA_Q2,
    T_2PT_POSS_Q3 = T_2PTA_Q3,
    T_2PT_POSS_Q4 = T_2PTA_Q4,
    T_2PT_POSS_Q5 = T_2PTA_Q5,
    T_2PT_POSS_Q6 = T_2PTA_Q6,
    T_2PT_POSS_CGS = T_2PTA_CGS,
    
    # PPP
    T_2PT_PPP_Q1 = zf(T_2PT_PTS_Q1, T_2PT_POSS_Q1),
    T_2PT_PPP_Q2 = zf(T_2PT_PTS_Q2, T_2PT_POSS_Q2),
    T_2PT_PPP_Q3 = zf(T_2PT_PTS_Q3, T_2PT_POSS_Q3),
    T_2PT_PPP_Q4 = zf(T_2PT_PTS_Q4, T_2PT_POSS_Q4),
    T_2PT_PPP_Q5 = if_else(OT_1==1L, zf(T_2PT_PTS_Q5, T_2PT_POSS_Q5), NA_real_),
    T_2PT_PPP_Q6 = if_else(OT_2==1L, zf(T_2PT_PTS_Q6, T_2PT_POSS_Q6), NA_real_),
    T_2PT_PPP_CGS = zf(T_2PT_PTS_CGS, T_2PT_POSS_CGS),
    
    # RATE = 2PTA / FGA
    T_2PT_RATE_Q1 = zf(T_2PTA_Q1, T_FGA_Q1),
    T_2PT_RATE_Q2 = zf(T_2PTA_Q2, T_FGA_Q2),
    T_2PT_RATE_Q3 = zf(T_2PTA_Q3, T_FGA_Q3),
    T_2PT_RATE_Q4 = zf(T_2PTA_Q4, T_FGA_Q4),
    T_2PT_RATE_Q5 = if_else(OT_1==1L, zf(T_2PTA_Q5, T_FGA_Q5), NA_real_),
    T_2PT_RATE_Q6 = if_else(OT_2==1L, zf(T_2PTA_Q6, T_FGA_Q6), NA_real_),
    T_2PT_RATE_CGS = zf(T_2PTA_CGS, T_FGA_CGS),
    
    # PTS share = (2*2PTM) / Team PTS
    T_2PT_PTSHR_Q1 = zf(T_2PT_PTS_Q1, T_PTS_SCORED_Q1),
    T_2PT_PTSHR_Q2 = zf(T_2PT_PTS_Q2, T_PTS_SCORED_Q2),
    T_2PT_PTSHR_Q3 = zf(T_2PT_PTS_Q3, T_PTS_SCORED_Q3),
    T_2PT_PTSHR_Q4 = zf(T_2PT_PTS_Q4, T_PTS_SCORED_Q4),
    T_2PT_PTSHR_Q5 = if_else(OT_1==1L, zf(T_2PT_PTS_Q5, T_PTS_SCORED_Q5), NA_real_),
    T_2PT_PTSHR_Q6 = if_else(OT_2==1L, zf(T_2PT_PTS_Q6, T_PTS_SCORED_Q6), NA_real_),
    T_2PT_PTSHR_CGS = zf(T_2PT_PTS_CGS, T_PTS_SCORED_CGS)
  )

rm(twopt_base, twopt_cgs, twopt_qtr)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Pull-Up 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ==================================================================================== #
# Section: Pull-Up Shooting (ALL Pullups) — Q1–Q6 + CGS   (raw proportions, no *100)  #
# Creates:
#   FGA/FGM/PTS  -> T_PU_FGA_Q1..Q6, T_PU_FGA_CGS; T_PU_FGM_Q1..Q6, T_PU_FGM_CGS;
#                   T_PU_PTS_Q1..Q6, T_PU_PTS_CGS
#   Derived      -> T_PU_FG_PCT_*, T_PU_EFG_PCT_*, T_PU_PPP_*,
#                   T_PU_PTSHR_* (vs T_PTS_*), T_PU_RATE_* (vs T_FGA_*)
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build Pull-Up base from pm_df (detect only "Pullup") --------------------------- #
pu_base <- pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play),
    pts   = suppressWarnings(as.integer(score_value)),
    is_pu = stringr::str_detect(type_text, stringr::regex("pullup", ignore_case = TRUE))
  ) %>%
  dplyr::filter(is_pu, shot) %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    pu_fga = TRUE,
    pu_fgm = make & !is.na(pts) & pts > 0L,
    pu_pts = dplyr::if_else(make & !is.na(pts) & pts > 0L, pts, 0L)
  )

# ---- Quarter-level tallies (Q1–Q6) ------------------------------------------------- #
pu_qtr <- pu_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_PU_FGA = sum(pu_fga, na.rm = TRUE),
    T_PU_FGM = sum(pu_fgm, na.rm = TRUE),
    T_PU_PTS = sum(pu_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_PU_FGA = 0L, T_PU_FGM = 0L, T_PU_PTS = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_PU_FGA, T_PU_FGM, T_PU_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
pu_cgs <- pu_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_PU_FGA_CGS = sum(pu_fga, na.rm = TRUE),
    T_PU_FGM_CGS = sum(pu_fgm, na.rm = TRUE),
    T_PU_PTS_CGS = sum(pu_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Team_MC & mask OT by flags -------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(pu_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(pu_cgs, by  = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always
    T_PU_FGA_Q1 = coalesce(T_PU_FGA_Q1,0L), T_PU_FGA_Q2 = coalesce(T_PU_FGA_Q2,0L),
    T_PU_FGA_Q3 = coalesce(T_PU_FGA_Q3,0L), T_PU_FGA_Q4 = coalesce(T_PU_FGA_Q4,0L),
    T_PU_FGM_Q1 = coalesce(T_PU_FGM_Q1,0L), T_PU_FGM_Q2 = coalesce(T_PU_FGM_Q2,0L),
    T_PU_FGM_Q3 = coalesce(T_PU_FGM_Q3,0L), T_PU_FGM_Q4 = coalesce(T_PU_FGM_Q4,0L),
    T_PU_PTS_Q1 = coalesce(T_PU_PTS_Q1,0L), T_PU_PTS_Q2 = coalesce(T_PU_PTS_Q2,0L),
    T_PU_PTS_Q3 = coalesce(T_PU_PTS_Q3,0L), T_PU_PTS_Q4 = coalesce(T_PU_PTS_Q4,0L),
    
    # OT masked by OT_1 / OT_2
    T_PU_FGA_Q5 = if_else(OT_1==1L, coalesce(T_PU_FGA_Q5,0L), 0L),
    T_PU_FGA_Q6 = if_else(OT_2==1L, coalesce(T_PU_FGA_Q6,0L), 0L),
    T_PU_FGM_Q5 = if_else(OT_1==1L, coalesce(T_PU_FGM_Q5,0L), 0L),
    T_PU_FGM_Q6 = if_else(OT_2==1L, coalesce(T_PU_FGM_Q6,0L), 0L),
    T_PU_PTS_Q5 = if_else(OT_1==1L, coalesce(T_PU_PTS_Q5,0L), 0L),
    T_PU_PTS_Q6 = if_else(OT_2==1L, coalesce(T_PU_PTS_Q6,0L), 0L),
    
    T_PU_FGA_CGS = coalesce(T_PU_FGA_CGS,0L),
    T_PU_FGM_CGS = coalesce(T_PU_FGM_CGS,0L),
    T_PU_PTS_CGS = coalesce(T_PU_PTS_CGS,0L)
  )

# ---- Ensure team totals present for RATE / PTSHR denominators ----------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_FGA_Q1 = coalesce(T_FGA_Q1,0), T_FGA_Q2 = coalesce(T_FGA_Q2,0),
    T_FGA_Q3 = coalesce(T_FGA_Q3,0), T_FGA_Q4 = coalesce(T_FGA_Q4,0),
    T_FGA_Q5 = if_else(OT_1==1L, coalesce(T_FGA_Q5,0), 0),
    T_FGA_Q6 = if_else(OT_2==1L, coalesce(T_FGA_Q6,0), 0),
    T_FGA_CGS = coalesce(T_FGA_CGS,0),
    
    T_PTS_Q1 = coalesce(T_PTS_Q1,0), T_PTS_Q2 = coalesce(T_PTS_Q2,0),
    T_PTS_Q3 = coalesce(T_PTS_Q3,0), T_PTS_Q4 = coalesce(T_PTS_Q4,0),
    T_PTS_Q5 = if_else(OT_1==1L, coalesce(T_PTS_Q5,0), 0),
    T_PTS_Q6 = if_else(OT_2==1L, coalesce(T_PTS_Q6,0), 0),
    T_PTS_CGS = coalesce(T_PTS_CGS,0)
  )

# ---- Derived (RAW proportions, no *100) -------------------------------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG% and eFG% (no 3pt weighting here)
    T_PU_FG_PCT_Q1  = safe_div(T_PU_FGM_Q1 , T_PU_FGA_Q1),
    T_PU_FG_PCT_Q2  = safe_div(T_PU_FGM_Q2 , T_PU_FGA_Q2),
    T_PU_FG_PCT_Q3  = safe_div(T_PU_FGM_Q3 , T_PU_FGA_Q3),
    T_PU_FG_PCT_Q4  = safe_div(T_PU_FGM_Q4 , T_PU_FGA_Q4),
    T_PU_FG_PCT_Q5  = if_else(OT_1==1L, safe_div(T_PU_FGM_Q5 , T_PU_FGA_Q5), NA_real_),
    T_PU_FG_PCT_Q6  = if_else(OT_2==1L, safe_div(T_PU_FGM_Q6 , T_PU_FGA_Q6), NA_real_),
    T_PU_FG_PCT_CGS = safe_div(T_PU_FGM_CGS, T_PU_FGA_CGS),
    
    T_PU_EFG_PCT_Q1  = T_PU_FG_PCT_Q1,
    T_PU_EFG_PCT_Q2  = T_PU_FG_PCT_Q2,
    T_PU_EFG_PCT_Q3  = T_PU_FG_PCT_Q3,
    T_PU_EFG_PCT_Q4  = T_PU_FG_PCT_Q4,
    T_PU_EFG_PCT_Q5  = T_PU_FG_PCT_Q5,
    T_PU_EFG_PCT_Q6  = T_PU_FG_PCT_Q6,
    T_PU_EFG_PCT_CGS = T_PU_FG_PCT_CGS,
    
    # PPP
    T_PU_PPP_Q1  = safe_div(T_PU_PTS_Q1 , T_PU_FGA_Q1),
    T_PU_PPP_Q2  = safe_div(T_PU_PTS_Q2 , T_PU_FGA_Q2),
    T_PU_PPP_Q3  = safe_div(T_PU_PTS_Q3 , T_PU_FGA_Q3),
    T_PU_PPP_Q4  = safe_div(T_PU_PTS_Q4 , T_PU_FGA_Q4),
    T_PU_PPP_Q5  = if_else(OT_1==1L, safe_div(T_PU_PTS_Q5 , T_PU_FGA_Q5), NA_real_),
    T_PU_PPP_Q6  = if_else(OT_2==1L, safe_div(T_PU_PTS_Q6 , T_PU_FGA_Q6), NA_real_),
    T_PU_PPP_CGS = safe_div(T_PU_PTS_CGS, T_PU_FGA_CGS),
    
    # Points share vs team total points (RAW ratio)
    T_PU_PTSHR_Q1  = safe_div(T_PU_PTS_Q1 , T_PTS_SCORED_Q1 ),
    T_PU_PTSHR_Q2  = safe_div(T_PU_PTS_Q2 , T_PTS_SCORED_Q2 ),
    T_PU_PTSHR_Q3  = safe_div(T_PU_PTS_Q3 , T_PTS_SCORED_Q3 ),
    T_PU_PTSHR_Q4  = safe_div(T_PU_PTS_Q4 , T_PTS_SCORED_Q4 ),
    T_PU_PTSHR_Q5  = if_else(OT_1==1L, safe_div(T_PU_PTS_Q5 , T_PTS_SCORED_Q5 ), NA_real_),
    T_PU_PTSHR_Q6  = if_else(OT_2==1L, safe_div(T_PU_PTS_Q6 , T_PTS_SCORED_Q6 ), NA_real_),
    T_PU_PTSHR_CGS = safe_div(T_PU_PTS_CGS, T_PTS_SCORED_CGS),
    
    # Rate: pull-up FGA as share of team FGA (RAW ratio)
    T_PU_RATE_Q1  = safe_div(T_PU_FGA_Q1 , T_FGA_Q1 ),
    T_PU_RATE_Q2  = safe_div(T_PU_FGA_Q2 , T_FGA_Q2 ),
    T_PU_RATE_Q3  = safe_div(T_PU_FGA_Q3 , T_FGA_Q3 ),
    T_PU_RATE_Q4  = safe_div(T_PU_FGA_Q4 , T_FGA_Q4 ),
    T_PU_RATE_Q5  = if_else(OT_1==1L, safe_div(T_PU_FGA_Q5 , T_FGA_Q5 ), NA_real_),
    T_PU_RATE_Q6  = if_else(OT_2==1L, safe_div(T_PU_FGA_Q6 , T_FGA_Q6 ), NA_real_),
    T_PU_RATE_CGS = safe_div(T_PU_FGA_CGS, T_FGA_CGS)
  )

rm(pu_base, pu_cgs, pu_qtr)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Pull-Up 2PT  Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ===============================
# 3PT: Attempts, Makes, %, eFG%, Points, Point Share, PPP, Rate (Q1–Q6 + CGS)
# ===============================

stopifnot(all(c("game_id","team_id","qtr","shooting_play","scoring_play","score_value",
                "home_team_id","away_team_id","SHOT_ZONE_BASIC") %in% names(pm_df)))

zone_col <- "SHOT_ZONE_BASIC"
to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(num, den) ifelse(den > 0 & !is.na(den), num / den, NA_real_)

# valid teams (exactly two per game)
valid_teams <- pm_df %>%
  dplyr::distinct(game_id, home_team_id, away_team_id) %>%
  tidyr::pivot_longer(c(home_team_id, away_team_id), values_to = "team_id") %>%
  dplyr::select(game_id, team_id) %>%
  dplyr::filter(!is.na(team_id)) %>%
  dplyr::distinct()

three_labels <- c("Above the Break 3", "Right Corner 3", "Left Corner 3")
three_rx <- stringr::regex(paste(three_labels, collapse = "|"), ignore_case = TRUE)

# Row-level flags
three_base <- pm_df %>%
  dplyr::mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  dplyr::inner_join(valid_teams, by = c("game_id","team_id")) %>%
  dplyr::mutate(
    is_3pa = to_bool(shooting_play) &
      stringr::str_detect(.data[[zone_col]], three_rx),
    is_3pm = to_bool(scoring_play) &
      suppressWarnings(as.integer(score_value)) == 3L &
      stringr::str_detect(.data[[zone_col]], three_rx)
  ) %>%
  {
    id_col <- intersect(c("event_num","play_id","event_id","pbp_id"), names(.))
    if (length(id_col)) dplyr::distinct(., game_id, team_id, qtr, dplyr::across(all_of(id_col)), .keep_all = TRUE) else .
  } %>%
  dplyr::select(game_id, team_id, qtr, is_3pa, is_3pm)

# Per-quarter tallies (Q1–Q6)
t3_qtr <- three_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_3PTA = sum(is_3pa, na.rm = TRUE),
    T_3PTM = sum(is_3pm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_3PTA = 0L, T_3PTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_3PTA, T_3PTM),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# Complete-game totals
t3_cgs <- three_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_3PTA_CGS = sum(is_3pa, na.rm = TRUE),
    T_3PTM_CGS = sum(is_3pm, na.rm = TRUE),
    .groups = "drop"
  )

# Join & mask OT by OT_1/OT_2
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(t3_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(t3_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_3PTA_Q1 = coalesce(T_3PTA_Q1,0L), T_3PTA_Q2 = coalesce(T_3PTA_Q2,0L),
    T_3PTA_Q3 = coalesce(T_3PTA_Q3,0L), T_3PTA_Q4 = coalesce(T_3PTA_Q4,0L),
    T_3PTM_Q1 = coalesce(T_3PTM_Q1,0L), T_3PTM_Q2 = coalesce(T_3PTM_Q2,0L),
    T_3PTM_Q3 = coalesce(T_3PTM_Q3,0L), T_3PTM_Q4 = coalesce(T_3PTM_Q4,0L),
    
    # OT only if flags set
    T_3PTA_Q5 = if_else(OT_1==1L, coalesce(T_3PTA_Q5,0L), 0L),
    T_3PTA_Q6 = if_else(OT_2==1L, coalesce(T_3PTA_Q6,0L), 0L),
    T_3PTM_Q5 = if_else(OT_1==1L, coalesce(T_3PTM_Q5,0L), 0L),
    T_3PTM_Q6 = if_else(OT_2==1L, coalesce(T_3PTM_Q6,0L), 0L),
    
    T_3PTA_CGS = coalesce(T_3PTA_CGS,0L),
    T_3PTM_CGS = coalesce(T_3PTM_CGS,0L)
  )

# Ensure team totals exist for rate/share denominators (incl. OT masks)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_FGA_Q1 = coalesce(T_FGA_Q1,0), T_FGA_Q2 = coalesce(T_FGA_Q2,0),
    T_FGA_Q3 = coalesce(T_FGA_Q3,0), T_FGA_Q4 = coalesce(T_FGA_Q4,0),
    T_FGA_Q5 = if_else(OT_1==1L, coalesce(T_FGA_Q5,0), 0),
    T_FGA_Q6 = if_else(OT_2==1L, coalesce(T_FGA_Q6,0), 0),
    T_FGA_CGS = coalesce(T_FGA_CGS,0),
    
    T_PTS_Q1 = coalesce(T_PTS_Q1,0), T_PTS_Q2 = coalesce(T_PTS_Q2,0),
    T_PTS_Q3 = coalesce(T_PTS_Q3,0), T_PTS_Q4 = coalesce(T_PTS_Q4,0),
    T_PTS_Q5 = if_else(OT_1==1L, coalesce(T_PTS_Q5,0), 0),
    T_PTS_Q6 = if_else(OT_2==1L, coalesce(T_PTS_Q6,0), 0),
    T_PTS_CGS = coalesce(T_PTS_CGS,0)
  )

# 3PT% (Q1–Q6 + CGS)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_3PT_PCT_Q1  = safe_div(T_3PTM_Q1 , T_3PTA_Q1),
    T_3PT_PCT_Q2  = safe_div(T_3PTM_Q2 , T_3PTA_Q2),
    T_3PT_PCT_Q3  = safe_div(T_3PTM_Q3 , T_3PTA_Q3),
    T_3PT_PCT_Q4  = safe_div(T_3PTM_Q4 , T_3PTA_Q4),
    T_3PT_PCT_Q5  = if_else(OT_1==1L, safe_div(T_3PTM_Q5 , T_3PTA_Q5), NA_real_),
    T_3PT_PCT_Q6  = if_else(OT_2==1L, safe_div(T_3PTM_Q6 , T_3PTA_Q6), NA_real_),
    T_3PT_PCT_CGS = safe_div(T_3PTM_CGS, T_3PTA_CGS)
  )

# 3PT eFG% (Q1–Q6 + CGS)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_3PT_EFG_PCT_Q1  = safe_div(1.5 * T_3PTM_Q1 , T_3PTA_Q1),
    T_3PT_EFG_PCT_Q2  = safe_div(1.5 * T_3PTM_Q2 , T_3PTA_Q2),
    T_3PT_EFG_PCT_Q3  = safe_div(1.5 * T_3PTM_Q3 , T_3PTA_Q3),
    T_3PT_EFG_PCT_Q4  = safe_div(1.5 * T_3PTM_Q4 , T_3PTA_Q4),
    T_3PT_EFG_PCT_Q5  = if_else(OT_1==1L, safe_div(1.5 * T_3PTM_Q5 , T_3PTA_Q5), NA_real_),
    T_3PT_EFG_PCT_Q6  = if_else(OT_2==1L, safe_div(1.5 * T_3PTM_Q6 , T_3PTA_Q6), NA_real_),
    T_3PT_EFG_PCT_CGS = safe_div(1.5 * T_3PTM_CGS, T_3PTA_CGS)
  )

# 3PT points & share (Q1–Q6 + CGS; share is RAW ratio)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_3PT_PTS_Q1  = 3L * T_3PTM_Q1,
    T_3PT_PTS_Q2  = 3L * T_3PTM_Q2,
    T_3PT_PTS_Q3  = 3L * T_3PTM_Q3,
    T_3PT_PTS_Q4  = 3L * T_3PTM_Q4,
    T_3PT_PTS_Q5  = 3L * if_else(OT_1==1L, T_3PTM_Q5, 0L),
    T_3PT_PTS_Q6  = 3L * if_else(OT_2==1L, T_3PTM_Q6, 0L),
    T_3PT_PTS_CGS = 3L * T_3PTM_CGS,
    
    T_3PT_PTSHR_Q1  = safe_div(T_3PT_PTS_Q1 , T_PTS_SCORED_Q1 ),
    T_3PT_PTSHR_Q2  = safe_div(T_3PT_PTS_Q2 , T_PTS_SCORED_Q2 ),
    T_3PT_PTSHR_Q3  = safe_div(T_3PT_PTS_Q3 , T_PTS_SCORED_Q3 ),
    T_3PT_PTSHR_Q4  = safe_div(T_3PT_PTS_Q4 , T_PTS_SCORED_Q4 ),
    T_3PT_PTSHR_Q5  = if_else(OT_1==1L, safe_div(T_3PT_PTS_Q5 , T_PTS_SCORED_Q5 ), NA_real_),
    T_3PT_PTSHR_Q6  = if_else(OT_2==1L, safe_div(T_3PT_PTS_Q6 , T_PTS_SCORED_Q6 ), NA_real_),
    T_3PT_PTSHR_CGS = safe_div(T_3PT_PTS_CGS, T_PTS_SCORED_CGS)
  )

# 3PT PPP (Q1–Q6 + CGS)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_3PT_PPP_Q1  = safe_div(T_3PT_PTS_Q1 , T_3PTA_Q1),
    T_3PT_PPP_Q2  = safe_div(T_3PT_PTS_Q2 , T_3PTA_Q2),
    T_3PT_PPP_Q3  = safe_div(T_3PT_PTS_Q3 , T_3PTA_Q3),
    T_3PT_PPP_Q4  = safe_div(T_3PT_PTS_Q4 , T_3PTA_Q4),
    T_3PT_PPP_Q5  = if_else(OT_1==1L, safe_div(T_3PT_PTS_Q5 , T_3PTA_Q5), NA_real_),
    T_3PT_PPP_Q6  = if_else(OT_2==1L, safe_div(T_3PT_PTS_Q6 , T_3PTA_Q6), NA_real_),
    T_3PT_PPP_CGS = safe_div(T_3PT_PTS_CGS, T_3PTA_CGS)
  )

# 3PT Rate (vs FGA) — Q1–Q6 + CGS (RAW ratios)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_3PT_RATE_Q1  = safe_div(T_3PTA_Q1 , T_FGA_Q1 ),
    T_3PT_RATE_Q2  = safe_div(T_3PTA_Q2 , T_FGA_Q2 ),
    T_3PT_RATE_Q3  = safe_div(T_3PTA_Q3 , T_FGA_Q3 ),
    T_3PT_RATE_Q4  = safe_div(T_3PTA_Q4 , T_FGA_Q4 ),
    T_3PT_RATE_Q5  = if_else(OT_1==1L, safe_div(T_3PTA_Q5 , T_FGA_Q5 ), NA_real_),
    T_3PT_RATE_Q6  = if_else(OT_2==1L, safe_div(T_3PTA_Q6 , T_FGA_Q6 ), NA_real_),
    T_3PT_RATE_CGS = safe_div(T_3PTA_CGS, T_FGA_CGS)
  )

rm(t3_cgs, t3_qtr, three_base, valid_teams)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: PPP, eFG%, TS%  Field Goal Data Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ===============================
# PPP, eFG%, TS%  (Q1–Q6 + CGS with OT masking)
# Requires (and will create if missing here): 
#   T_PTS_Q*, T_POSS_Q*, T_FGA_Q*, T_FGM_Q*, T_3PTM_Q*, T_FTA_Q* (+ CGS)
# ===============================

stopifnot(all(c("game_id","qtr","team_id","type_text","scoring_play","score_value") %in% names(pm_df)))
to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---------- Build 3PT MAKES (by quarter 1–6 + CGS) ----------
threes_base <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    is_make3 = to_bool(scoring_play) &
      suppressWarnings(as.integer(score_value)) == 3L &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  )

t3_qtr <- threes_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(T_3PTM = sum(is_make3, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_3PTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = qtr, values_from = T_3PTM, values_fill = 0L,
                     names_prefix = "T_3PTM_Q")

t3_cgs <- threes_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(T_3PTM_CGS = sum(is_make3, na.rm = TRUE), .groups = "drop")

# ---------- Build FTA (by quarter 1–6 + CGS) ----------
fta_base <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    is_fta = stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE))
  )

fta_qtr <- fta_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(T_FTA = sum(is_fta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(T_FTA = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = qtr, values_from = T_FTA, values_fill = 0L,
                     names_prefix = "T_FTA_Q")

fta_cgs <- fta_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(T_FTA_CGS = sum(is_fta, na.rm = TRUE), .groups = "drop")

# ---------- Join support counts (3PTM, FTA) ----------
need_cols <- c(paste0("T_3PTM_Q", 1:6), "T_3PTM_CGS", paste0("T_FTA_Q", 1:6), "T_FTA_CGS")

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::select(-dplyr::any_of(need_cols)) %>%  # prevent dupes
  dplyr::left_join(t3_qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(t3_cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(fta_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(fta_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(dplyr::across(dplyr::any_of(need_cols), ~ dplyr::coalesce(., 0L))) %>%
  # Mask OT quarters by flags
  dplyr::mutate(
    T_3PTM_Q5 = if_else(OT_1 == 1L, T_3PTM_Q5, 0L),
    T_3PTM_Q6 = if_else(OT_2 == 1L, T_3PTM_Q6, 0L),
    T_FTA_Q5  = if_else(OT_1 == 1L, T_FTA_Q5,  0L),
    T_FTA_Q6  = if_else(OT_2 == 1L, T_FTA_Q6,  0L)
  )

# ---------- Ensure denominators exist (incl. OT masks for FGA/POSS/PTS) ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FGA
    T_FGA_Q1 = coalesce(T_FGA_Q1,0), T_FGA_Q2 = coalesce(T_FGA_Q2,0),
    T_FGA_Q3 = coalesce(T_FGA_Q3,0), T_FGA_Q4 = coalesce(T_FGA_Q4,0),
    T_FGA_Q5 = if_else(OT_1==1L, coalesce(T_FGA_Q5,0), 0),
    T_FGA_Q6 = if_else(OT_2==1L, coalesce(T_FGA_Q6,0), 0),
    T_FGA_CGS = coalesce(T_FGA_CGS,0),
    # FGM (for eFG)
    T_FGM_Q1 = coalesce(T_FGM_Q1,0), T_FGM_Q2 = coalesce(T_FGM_Q2,0),
    T_FGM_Q3 = coalesce(T_FGM_Q3,0), T_FGM_Q4 = coalesce(T_FGM_Q4,0),
    T_FGM_Q5 = if_else(OT_1==1L, coalesce(T_FGM_Q5,0), 0),
    T_FGM_Q6 = if_else(OT_2==1L, coalesce(T_FGM_Q6,0), 0),
    T_FGM_CGS = coalesce(T_FGM_CGS,0),
    # POSS
    T_POSS_Q1 = coalesce(T_POSS_Q1,0), T_POSS_Q2 = coalesce(T_POSS_Q2,0),
    T_POSS_Q3 = coalesce(T_POSS_Q3,0), T_POSS_Q4 = coalesce(T_POSS_Q4,0),
    T_POSS_Q5 = if_else(OT_1==1L, coalesce(T_POSS_Q5,0), 0),
    T_POSS_Q6 = if_else(OT_2==1L, coalesce(T_POSS_Q6,0), 0),
    T_POSS_CGS = coalesce(T_POSS_CGS,0),
    # PTS
    T_PTS_SCORED_Q1 = coalesce(T_PTS_SCORED_Q1,0), T_PTS_SCORED_Q2 = coalesce(T_PTS_SCORED_Q2,0),
    T_PTS_SCORED_Q3 = coalesce(T_PTS_SCORED_Q3,0), T_PTS_SCORED_Q4 = coalesce(T_PTS_SCORED_Q4,0),
    T_PTS_SCORED_Q5 = if_else(OT_1==1L, coalesce(T_PTS_SCORED_Q5,0), 0),
    T_PTS_SCORED_Q6 = if_else(OT_2==1L, coalesce(T_PTS_SCORED_Q6,0), 0),
    T_PTS_SCORED_CGS = coalesce(T_PTS_SCORED_CGS,0),
    # FTA (ensure present for TS%)
    T_FTA_Q1 = coalesce(T_FTA_Q1,0L), T_FTA_Q2 = coalesce(T_FTA_Q2,0L),
    T_FTA_Q3 = coalesce(T_FTA_Q3,0L), T_FTA_Q4 = coalesce(T_FTA_Q4,0L),
    T_FTA_Q5 = if_else(OT_1==1L, coalesce(T_FTA_Q5,0L), 0L),
    T_FTA_Q6 = if_else(OT_2==1L, coalesce(T_FTA_Q6,0L), 0L),
    T_FTA_CGS = coalesce(T_FTA_CGS,0L),
    # 3PTM (ensure Q1–Q6 present)
    T_3PTM_Q1 = coalesce(T_3PTM_Q1,0L), T_3PTM_Q2 = coalesce(T_3PTM_Q2,0L),
    T_3PTM_Q3 = coalesce(T_3PTM_Q3,0L), T_3PTM_Q4 = coalesce(T_3PTM_Q4,0L),
    T_3PTM_Q5 = if_else(OT_1==1L, coalesce(T_3PTM_Q5,0L), 0L),
    T_3PTM_Q6 = if_else(OT_2==1L, coalesce(T_3PTM_Q6,0L), 0L),
    T_3PTM_CGS = coalesce(T_3PTM_CGS,0L)
  )

# ---------- PPP (Points per Possession) ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_PPP_Q1  = safe_div(T_PTS_SCORED_Q1 , T_POSS_Q1 ),
    T_PPP_Q2  = safe_div(T_PTS_SCORED_Q2 , T_POSS_Q2 ),
    T_PPP_Q3  = safe_div(T_PTS_SCORED_Q3 , T_POSS_Q3 ),
    T_PPP_Q4  = safe_div(T_PTS_SCORED_Q4 , T_POSS_Q4 ),
    T_PPP_Q5  = if_else(OT_1==1L, safe_div(T_PTS_SCORED_Q5 , T_POSS_Q5 ), NA_real_),
    T_PPP_Q6  = if_else(OT_2==1L, safe_div(T_PTS_SCORED_Q6 , T_POSS_Q6 ), NA_real_),
    T_PPP_CGS = safe_div(T_PTS_SCORED_CGS, T_POSS_CGS)
  )

# ---------- eFG% = (FGM + 0.5 * 3PTM) / FGA ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_EFG_PCT_Q1  = safe_div(T_FGM_Q1  + 0.5 * T_3PTM_Q1 , T_FGA_Q1 ),
    T_EFG_PCT_Q2  = safe_div(T_FGM_Q2  + 0.5 * T_3PTM_Q2 , T_FGA_Q2 ),
    T_EFG_PCT_Q3  = safe_div(T_FGM_Q3  + 0.5 * T_3PTM_Q3 , T_FGA_Q3 ),
    T_EFG_PCT_Q4  = safe_div(T_FGM_Q4  + 0.5 * T_3PTM_Q4 , T_FGA_Q4 ),
    T_EFG_PCT_Q5  = if_else(OT_1==1L, safe_div(T_FGM_Q5 + 0.5 * T_3PTM_Q5, T_FGA_Q5), NA_real_),
    T_EFG_PCT_Q6  = if_else(OT_2==1L, safe_div(T_FGM_Q6 + 0.5 * T_3PTM_Q6, T_FGA_Q6), NA_real_),
    T_EFG_PCT_CGS = safe_div(T_FGM_CGS + 0.5 * T_3PTM_CGS, T_FGA_CGS)
  )

# ---------- TS% = PTS / (2 * (FGA + 0.44 * FTA)) ----------
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_TS_PCT_Q1  = { d <- (T_FGA_Q1  + 0.44 * T_FTA_Q1 );  ifelse(d > 0, T_PTS_SCORED_Q1  / (2 * d), NA_real_) },
    T_TS_PCT_Q2  = { d <- (T_FGA_Q2  + 0.44 * T_FTA_Q2 );  ifelse(d > 0, T_PTS_SCORED_Q2  / (2 * d), NA_real_) },
    T_TS_PCT_Q3  = { d <- (T_FGA_Q3  + 0.44 * T_FTA_Q3 );  ifelse(d > 0, T_PTS_SCORED_Q3  / (2 * d), NA_real_) },
    T_TS_PCT_Q4  = { d <- (T_FGA_Q4  + 0.44 * T_FTA_Q4 );  ifelse(d > 0, T_PTS_SCORED_Q4  / (2 * d), NA_real_) },
    T_TS_PCT_Q5  = if_else(OT_1==1L, { d <- (T_FGA_Q5 + 0.44 * T_FTA_Q5); ifelse(d > 0, T_PTS_SCORED_Q5 / (2 * d), NA_real_) }, NA_real_),
    T_TS_PCT_Q6  = if_else(OT_2==1L, { d <- (T_FGA_Q6 + 0.44 * T_FTA_Q6); ifelse(d > 0, T_PTS_SCORED_Q6 / (2 * d), NA_real_) }, NA_real_),
    T_TS_PCT_CGS = { d <- (T_FGA_CGS + 0.44 * T_FTA_CGS); ifelse(d > 0, T_PTS_SCORED_CGS / (2 * d), NA_real_) }
  )

rm(fta_base, fta_cgs, fta_qtr, t3_cgs, t3_qtr, threes_base)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: PPP, eFG%, TS%  Field Goal Data Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Second Chance Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# SECOND CHANCE: FGA/FGM/FG%, PTS, PPP, RATE, POSS  (Q1..Q6 + CGS)
# ===============================

# Required columns
stopifnot(all(c("game_id","team_id","qtr","type_id",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# ---- Build second-chance base: look at the *next* row within (game, team, qtr) ----
sec_base <- pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    idx  = dplyr::row_number(),                               # preserve original play order
    is_orb = suppressWarnings(as.integer(type_id)) == 156L,   # offensive rebound event
    sh  = to_bool(shooting_play),
    sc  = to_bool(scoring_play),
    val = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::arrange(idx) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::mutate(
    next_shoot = dplyr::lead(sh),
    next_score = dplyr::lead(sc),
    next_val   = dplyr::lead(val)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is_orb)   # only ORB rows define a second-chance opportunity

# ---- Quarter splits (Q1–Q6 incl. OT1/OT2) ----
sec_qtr <- sec_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_FGA_SEC_CHN  = sum(next_shoot, na.rm = TRUE),
    T_FGM_SEC_CHN  = sum(next_score, na.rm = TRUE),
    T_SEC_CHN_PTS  = sum(dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L), na.rm = TRUE),
    T_SEC_CHN_POSS = dplyr::n(),   # each ORB is one second-chance possession
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6,
                  fill = list(T_FGA_SEC_CHN = 0L,
                              T_FGM_SEC_CHN = 0L,
                              T_SEC_CHN_PTS = 0L,
                              T_SEC_CHN_POSS = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_FGA_SEC_CHN, T_FGM_SEC_CHN, T_SEC_CHN_PTS, T_SEC_CHN_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
sec_cgs <- sec_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_FGA_SEC_CHN_CGS  = sum(next_shoot, na.rm = TRUE),
    T_FGM_SEC_CHN_CGS  = sum(next_score, na.rm = TRUE),
    T_SEC_CHN_PTS_CGS  = sum(dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L), na.rm = TRUE),
    T_SEC_CHN_POSS_CGS = dplyr::n(),
    .groups = "drop"
  )

# ---- Join raw second-chance counts back into BaseStats_Team_MC ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(sec_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(sec_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always; OT columns only if the game had those OTs
    T_FGA_SEC_CHN_Q1 = dplyr::coalesce(T_FGA_SEC_CHN_Q1, 0L),
    T_FGA_SEC_CHN_Q2 = dplyr::coalesce(T_FGA_SEC_CHN_Q2, 0L),
    T_FGA_SEC_CHN_Q3 = dplyr::coalesce(T_FGA_SEC_CHN_Q3, 0L),
    T_FGA_SEC_CHN_Q4 = dplyr::coalesce(T_FGA_SEC_CHN_Q4, 0L),
    T_FGA_SEC_CHN_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FGA_SEC_CHN_Q5, 0L), 0L),
    T_FGA_SEC_CHN_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FGA_SEC_CHN_Q6, 0L), 0L),
    T_FGA_SEC_CHN_CGS = dplyr::coalesce(T_FGA_SEC_CHN_CGS, 0L),
    
    T_FGM_SEC_CHN_Q1 = dplyr::coalesce(T_FGM_SEC_CHN_Q1, 0L),
    T_FGM_SEC_CHN_Q2 = dplyr::coalesce(T_FGM_SEC_CHN_Q2, 0L),
    T_FGM_SEC_CHN_Q3 = dplyr::coalesce(T_FGM_SEC_CHN_Q3, 0L),
    T_FGM_SEC_CHN_Q4 = dplyr::coalesce(T_FGM_SEC_CHN_Q4, 0L),
    T_FGM_SEC_CHN_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FGM_SEC_CHN_Q5, 0L), 0L),
    T_FGM_SEC_CHN_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FGM_SEC_CHN_Q6, 0L), 0L),
    T_FGM_SEC_CHN_CGS = dplyr::coalesce(T_FGM_SEC_CHN_CGS, 0L),
    
    T_SEC_CHN_PTS_Q1 = dplyr::coalesce(T_SEC_CHN_PTS_Q1, 0L),
    T_SEC_CHN_PTS_Q2 = dplyr::coalesce(T_SEC_CHN_PTS_Q2, 0L),
    T_SEC_CHN_PTS_Q3 = dplyr::coalesce(T_SEC_CHN_PTS_Q3, 0L),
    T_SEC_CHN_PTS_Q4 = dplyr::coalesce(T_SEC_CHN_PTS_Q4, 0L),
    T_SEC_CHN_PTS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_SEC_CHN_PTS_Q5, 0L), 0L),
    T_SEC_CHN_PTS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_SEC_CHN_PTS_Q6, 0L), 0L),
    T_SEC_CHN_PTS_CGS = dplyr::coalesce(T_SEC_CHN_PTS_CGS, 0L),
    
    T_SEC_CHN_POSS_Q1 = dplyr::coalesce(T_SEC_CHN_POSS_Q1, 0L),
    T_SEC_CHN_POSS_Q2 = dplyr::coalesce(T_SEC_CHN_POSS_Q2, 0L),
    T_SEC_CHN_POSS_Q3 = dplyr::coalesce(T_SEC_CHN_POSS_Q3, 0L),
    T_SEC_CHN_POSS_Q4 = dplyr::coalesce(T_SEC_CHN_POSS_Q4, 0L),
    T_SEC_CHN_POSS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_SEC_CHN_POSS_Q5, 0L), 0L),
    T_SEC_CHN_POSS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_SEC_CHN_POSS_Q6, 0L), 0L),
    T_SEC_CHN_POSS_CGS = dplyr::coalesce(T_SEC_CHN_POSS_CGS, 0L)
  )

# ---- Second-chance FG% ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_FG_PCT_SEC_CHN_Q1  = safe_div(T_FGM_SEC_CHN_Q1 , T_FGA_SEC_CHN_Q1),
    T_FG_PCT_SEC_CHN_Q2  = safe_div(T_FGM_SEC_CHN_Q2 , T_FGA_SEC_CHN_Q2),
    T_FG_PCT_SEC_CHN_Q3  = safe_div(T_FGM_SEC_CHN_Q3 , T_FGA_SEC_CHN_Q3),
    T_FG_PCT_SEC_CHN_Q4  = safe_div(T_FGM_SEC_CHN_Q4 , T_FGA_SEC_CHN_Q4),
    T_FG_PCT_SEC_CHN_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_FGM_SEC_CHN_Q5 , T_FGA_SEC_CHN_Q5), NA_real_),
    T_FG_PCT_SEC_CHN_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_FGM_SEC_CHN_Q6 , T_FGA_SEC_CHN_Q6), NA_real_),
    T_FG_PCT_SEC_CHN_CGS = safe_div(T_FGM_SEC_CHN_CGS, T_FGA_SEC_CHN_CGS)
  )

# ---- Second-chance PPP (points per second-chance possession) ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_SEC_CHN_PPP_Q1  = safe_div(T_SEC_CHN_PTS_Q1 , T_SEC_CHN_POSS_Q1),
    T_SEC_CHN_PPP_Q2  = safe_div(T_SEC_CHN_PTS_Q2 , T_SEC_CHN_POSS_Q2),
    T_SEC_CHN_PPP_Q3  = safe_div(T_SEC_CHN_PTS_Q3 , T_SEC_CHN_POSS_Q3),
    T_SEC_CHN_PPP_Q4  = safe_div(T_SEC_CHN_PTS_Q4 , T_SEC_CHN_POSS_Q4),
    T_SEC_CHN_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_SEC_CHN_PTS_Q5 , T_SEC_CHN_POSS_Q5), NA_real_),
    T_SEC_CHN_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_SEC_CHN_PTS_Q6 , T_SEC_CHN_POSS_Q6), NA_real_),
    T_SEC_CHN_PPP_CGS = safe_div(T_SEC_CHN_PTS_CGS, T_SEC_CHN_POSS_CGS)
  )

# ---- Second-chance Rate = second-chance possessions / total possessions ----
need_poss <- c("T_POSS_Q1","T_POSS_Q2","T_POSS_Q3","T_POSS_Q4","T_POSS_CGS")
stopifnot(all(need_poss %in% names(BaseStats_Team_MC)))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_SEC_CHN_RATE_Q1  = safe_div(T_SEC_CHN_POSS_Q1 , T_POSS_Q1),
    T_SEC_CHN_RATE_Q2  = safe_div(T_SEC_CHN_POSS_Q2 , T_POSS_Q2),
    T_SEC_CHN_RATE_Q3  = safe_div(T_SEC_CHN_POSS_Q3 , T_POSS_Q3),
    T_SEC_CHN_RATE_Q4  = safe_div(T_SEC_CHN_POSS_Q4 , T_POSS_Q4),
    T_SEC_CHN_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_SEC_CHN_POSS_Q5 , T_POSS_Q4 + 0L), NA_real_), # rate only if OT1 exists
    T_SEC_CHN_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_SEC_CHN_POSS_Q6 , T_POSS_Q4 + 0L), NA_real_), # adjust denominator as desired
    T_SEC_CHN_RATE_CGS = safe_div(T_SEC_CHN_POSS_CGS, T_POSS_CGS)
  )

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_SEC_CHN_PTSHR_Q1  = safe_div(T_SEC_CHN_PTS_Q1 , T_PTS_SCORED_Q1),
    T_SEC_CHN_PTSHR_Q2  = safe_div(T_SEC_CHN_PTS_Q2 , T_PTS_SCORED_Q2),
    T_SEC_CHN_PTSHR_Q3  = safe_div(T_SEC_CHN_PTS_Q3 , T_PTS_SCORED_Q3),
    T_SEC_CHN_PTSHR_Q4  = safe_div(T_SEC_CHN_PTS_Q4 , T_PTS_SCORED_Q4),
    T_SEC_CHN_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_SEC_CHN_PTS_Q5 , T_PTS_SCORED_Q5), NA_real_),
    T_SEC_CHN_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_SEC_CHN_PTS_Q6 , T_PTS_SCORED_Q6), NA_real_),
    T_SEC_CHN_PTSHR_CGS = safe_div(T_SEC_CHN_PTS_CGS, T_PTS_SCORED_CGS)
  )

rm(sec_base, sec_cgs, sec_qtr)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Second Chance Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fast Break Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# FAST BREAK: FGA, FGM, FG%, PPP, PTS, RATE, POSS  (Q1–Q6 + CGS with OT masking)
# ===============================

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value",
                "clock_minutes","clock_seconds") %in% names(pm_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0, n / d, NA_real_)

# ---- Build "next play" context within each game & quarter ----
fb_base <- pm_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes))*60L +
      suppressWarnings(as.integer(clock_seconds)),
    trig  = stringr::str_detect(type_text, stringr::regex("Turnover|Steal|Defensive Rebound", ignore_case = TRUE))
  ) %>%
  dplyr::arrange(game_id, qtr, dplyr::desc(t_sec)) %>%     # next row = lead() at a smaller clock
  dplyr::group_by(game_id, qtr) %>%
  dplyr::mutate(
    next_team   = dplyr::lead(team_id),
    next_t_sec  = dplyr::lead(t_sec),
    next_shoot  = to_bool(dplyr::lead(shooting_play)),
    next_scoreF = to_bool(dplyr::lead(scoring_play)),
    next_pts    = suppressWarnings(as.integer(dplyr::lead(score_value))),
    dt          = as.integer(t_sec - next_t_sec)           # clock counts down
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    fb_poss = trig & dt >= 0L & dt <= 6L & !is.na(next_team),  # fast-break opp within 6s
    fb_team = next_team,                                       # credit gaining team
    fb_fga  = fb_poss & next_shoot,
    fb_fgm  = fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L,
    fb_pts  = dplyr::if_else(fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L, next_pts, 0L)
  ) %>%
  dplyr::filter(fb_poss) %>%
  dplyr::mutate(team_id = fb_team) %>%
  dplyr::select(game_id, team_id, qtr, fb_fga, fb_fgm, fb_pts, fb_poss)

# ---- Per-quarter (Q1–Q6 incl. OT1/OT2) ----
fbrk_qtr <- fb_base %>%
  dplyr::mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_FBRK_FGA  = sum(fb_fga,  na.rm = TRUE),
    T_FBRK_FGM  = sum(fb_fgm,  na.rm = TRUE),
    T_FBRK_PTS  = sum(fb_pts,  na.rm = TRUE),
    T_FBRK_POSS = sum(fb_poss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(T_FBRK_FGA = 0L, T_FBRK_FGM = 0L, T_FBRK_PTS = 0L, T_FBRK_POSS = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_FBRK_FGA, T_FBRK_FGM, T_FBRK_PTS, T_FBRK_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
fbrk_cgs <- fb_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_FBRK_FGA_CGS  = sum(fb_fga,  na.rm = TRUE),
    T_FBRK_FGM_CGS  = sum(fb_fgm,  na.rm = TRUE),
    T_FBRK_PTS_CGS  = sum(fb_pts,  na.rm = TRUE),
    T_FBRK_POSS_CGS = sum(fb_poss, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join raw counts into BaseStats_Team_MC with OT masking ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(fbrk_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(fbrk_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always; Q5/Q6 only when OT_1/OT_2 present
    T_FBRK_FGA_Q1 = dplyr::coalesce(T_FBRK_FGA_Q1, 0L),
    T_FBRK_FGA_Q2 = dplyr::coalesce(T_FBRK_FGA_Q2, 0L),
    T_FBRK_FGA_Q3 = dplyr::coalesce(T_FBRK_FGA_Q3, 0L),
    T_FBRK_FGA_Q4 = dplyr::coalesce(T_FBRK_FGA_Q4, 0L),
    T_FBRK_FGA_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FBRK_FGA_Q5, 0L), 0L),
    T_FBRK_FGA_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FBRK_FGA_Q6, 0L), 0L),
    T_FBRK_FGA_CGS = dplyr::coalesce(T_FBRK_FGA_CGS, 0L),
    
    T_FBRK_FGM_Q1 = dplyr::coalesce(T_FBRK_FGM_Q1, 0L),
    T_FBRK_FGM_Q2 = dplyr::coalesce(T_FBRK_FGM_Q2, 0L),
    T_FBRK_FGM_Q3 = dplyr::coalesce(T_FBRK_FGM_Q3, 0L),
    T_FBRK_FGM_Q4 = dplyr::coalesce(T_FBRK_FGM_Q4, 0L),
    T_FBRK_FGM_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FBRK_FGM_Q5, 0L), 0L),
    T_FBRK_FGM_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FBRK_FGM_Q6, 0L), 0L),
    T_FBRK_FGM_CGS = dplyr::coalesce(T_FBRK_FGM_CGS, 0L),
    
    T_FBRK_PTS_Q1 = dplyr::coalesce(T_FBRK_PTS_Q1, 0L),
    T_FBRK_PTS_Q2 = dplyr::coalesce(T_FBRK_PTS_Q2, 0L),
    T_FBRK_PTS_Q3 = dplyr::coalesce(T_FBRK_PTS_Q3, 0L),
    T_FBRK_PTS_Q4 = dplyr::coalesce(T_FBRK_PTS_Q4, 0L),
    T_FBRK_PTS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FBRK_PTS_Q5, 0L), 0L),
    T_FBRK_PTS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FBRK_PTS_Q6, 0L), 0L),
    T_FBRK_PTS_CGS = dplyr::coalesce(T_FBRK_PTS_CGS, 0L),
    
    T_FBRK_POSS_Q1 = dplyr::coalesce(T_FBRK_POSS_Q1, 0L),
    T_FBRK_POSS_Q2 = dplyr::coalesce(T_FBRK_POSS_Q2, 0L),
    T_FBRK_POSS_Q3 = dplyr::coalesce(T_FBRK_POSS_Q3, 0L),
    T_FBRK_POSS_Q4 = dplyr::coalesce(T_FBRK_POSS_Q4, 0L),
    T_FBRK_POSS_Q5 = dplyr::if_else(OT_1 == 1L, dplyr::coalesce(T_FBRK_POSS_Q5, 0L), 0L),
    T_FBRK_POSS_Q6 = dplyr::if_else(OT_2 == 1L, dplyr::coalesce(T_FBRK_POSS_Q6, 0L), 0L),
    T_FBRK_POSS_CGS = dplyr::coalesce(T_FBRK_POSS_CGS, 0L)
  )

# ---- Percentages, PPP, Rate ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG%
    T_FBRK_FG_PCT_Q1  = safe_div(T_FBRK_FGM_Q1 , T_FBRK_FGA_Q1),
    T_FBRK_FG_PCT_Q2  = safe_div(T_FBRK_FGM_Q2 , T_FBRK_FGA_Q2),
    T_FBRK_FG_PCT_Q3  = safe_div(T_FBRK_FGM_Q3 , T_FBRK_FGA_Q3),
    T_FBRK_FG_PCT_Q4  = safe_div(T_FBRK_FGM_Q4 , T_FBRK_FGA_Q4),
    T_FBRK_FG_PCT_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_FBRK_FGM_Q5 , T_FBRK_FGA_Q5), NA_real_),
    T_FBRK_FG_PCT_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_FBRK_FGM_Q6 , T_FBRK_FGA_Q6), NA_real_),
    T_FBRK_FG_PCT_CGS = safe_div(T_FBRK_FGM_CGS, T_FBRK_FGA_CGS),
    
    # PPP
    T_FBRK_PPP_Q1  = safe_div(T_FBRK_PTS_Q1 , T_FBRK_POSS_Q1),
    T_FBRK_PPP_Q2  = safe_div(T_FBRK_PTS_Q2 , T_FBRK_POSS_Q2),
    T_FBRK_PPP_Q3  = safe_div(T_FBRK_PTS_Q3 , T_FBRK_POSS_Q3),
    T_FBRK_PPP_Q4  = safe_div(T_FBRK_PTS_Q4 , T_FBRK_POSS_Q4),
    T_FBRK_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_FBRK_PTS_Q5 , T_FBRK_POSS_Q5), NA_real_),
    T_FBRK_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_FBRK_PTS_Q6 , T_FBRK_POSS_Q6), NA_real_),
    T_FBRK_PPP_CGS = safe_div(T_FBRK_PTS_CGS, T_FBRK_POSS_CGS),
    
    # RATE = fast-break possessions / total possessions (requires T_POSS_* present)
    T_FBRK_RATE_Q1  = safe_div(T_FBRK_POSS_Q1 , T_POSS_Q1 ),
    T_FBRK_RATE_Q2  = safe_div(T_FBRK_POSS_Q2 , T_POSS_Q2 ),
    T_FBRK_RATE_Q3  = safe_div(T_FBRK_POSS_Q3 , T_POSS_Q3 ),
    T_FBRK_RATE_Q4  = safe_div(T_FBRK_POSS_Q4 , T_POSS_Q4 ),
    T_FBRK_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_FBRK_POSS_Q5 , T_POSS_Q4 + 0L), NA_real_), # adjust denom if you track T_POSS_Q5
    T_FBRK_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_FBRK_POSS_Q6 , T_POSS_Q4 + 0L), NA_real_), # adjust denom if you track T_POSS_Q6
    T_FBRK_RATE_CGS = safe_div(T_FBRK_POSS_CGS, T_POSS_CGS)
  )

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_FBRK_PTSHR_Q1  = safe_div(T_FBRK_PTS_Q1 , T_PTS_SCORED_Q1),
    T_FBRK_PTSHR_Q2  = safe_div(T_FBRK_PTS_Q2 , T_PTS_SCORED_Q2),
    T_FBRK_PTSHR_Q3  = safe_div(T_FBRK_PTS_Q3 , T_PTS_SCORED_Q3),
    T_FBRK_PTSHR_Q4  = safe_div(T_FBRK_PTS_Q4 , T_PTS_SCORED_Q4),
    T_FBRK_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_FBRK_PTS_Q5 , T_PTS_SCORED_Q5), NA_real_),
    T_FBRK_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_FBRK_PTS_Q6 , T_PTS_SCORED_Q6), NA_real_),
    T_FBRK_PTSHR_CGS = safe_div(T_FBRK_PTS_CGS, T_PTS_SCORED_CGS)
  )

rm(fb_base, fbrk_cgs, fbrk_qtr)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fast Break Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: AND1 Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ===============================
# AND-1: FGA, FGM, FG%, PPP, PTS, RATE  (Q1–Q6 + CGS, with OT masking)
# ===============================

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool   <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div  <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build AND-1 base (rows are the FT that follows an AND-1 make) ----
and1_base <-
  pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(type_text == "Free Throw - 1 of 1") %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    and1_fga = shot,                                  # attempt flag
    and1_fgm = make & !is.na(pts) & pts > 0L,         # made flag
    and1_pts = dplyr::if_else(make & !is.na(pts) & pts > 0L, pts, 0L)
  )

# ---- Per-quarter (Q1–Q6) ----
and1_qtr <-
  and1_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_AND1_FGA = sum(and1_fga, na.rm = TRUE),
    T_AND1_FGM = sum(and1_fgm, na.rm = TRUE),
    T_AND1_PTS = sum(and1_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_AND1_FGA, T_AND1_FGM, T_AND1_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
and1_cgs <-
  and1_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_AND1_FGA_CGS = sum(and1_fga, na.rm = TRUE),
    T_AND1_FGM_CGS = sum(and1_fgm, na.rm = TRUE),
    T_AND1_PTS_CGS = sum(and1_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join raw counts into BaseStats_Team_MC + coalesce + OT masking ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(and1_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(and1_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    dplyr::across(
      c(T_AND1_FGA_Q1, T_AND1_FGA_Q2, T_AND1_FGA_Q3, T_AND1_FGA_Q4, T_AND1_FGA_Q5, T_AND1_FGA_Q6, T_AND1_FGA_CGS,
        T_AND1_FGM_Q1, T_AND1_FGM_Q2, T_AND1_FGM_Q3, T_AND1_FGM_Q4, T_AND1_FGM_Q5, T_AND1_FGM_Q6, T_AND1_FGM_CGS,
        T_AND1_PTS_Q1, T_AND1_PTS_Q2, T_AND1_PTS_Q3, T_AND1_PTS_Q4, T_AND1_PTS_Q5, T_AND1_PTS_Q6, T_AND1_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # expose OT only if those OTs happened
    T_AND1_FGA_Q5 = dplyr::if_else(OT_1 == 1L, T_AND1_FGA_Q5, 0L),
    T_AND1_FGM_Q5 = dplyr::if_else(OT_1 == 1L, T_AND1_FGM_Q5, 0L),
    T_AND1_PTS_Q5 = dplyr::if_else(OT_1 == 1L, T_AND1_PTS_Q5, 0L),
    
    T_AND1_FGA_Q6 = dplyr::if_else(OT_2 == 1L, T_AND1_FGA_Q6, 0L),
    T_AND1_FGM_Q6 = dplyr::if_else(OT_2 == 1L, T_AND1_FGM_Q6, 0L),
    T_AND1_PTS_Q6 = dplyr::if_else(OT_2 == 1L, T_AND1_PTS_Q6, 0L)
  )

# ---- FG%, PPP (points per AND-1 attempt), RATE (AND-1 FGM / Team FGA) ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG%
    T_AND1_FG_PCT_Q1  = safe_div(T_AND1_FGM_Q1 , T_AND1_FGA_Q1),
    T_AND1_FG_PCT_Q2  = safe_div(T_AND1_FGM_Q2 , T_AND1_FGA_Q2),
    T_AND1_FG_PCT_Q3  = safe_div(T_AND1_FGM_Q3 , T_AND1_FGA_Q3),
    T_AND1_FG_PCT_Q4  = safe_div(T_AND1_FGM_Q4 , T_AND1_FGA_Q4),
    T_AND1_FG_PCT_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_AND1_FGM_Q5, T_AND1_FGA_Q5), NA_real_),
    T_AND1_FG_PCT_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_AND1_FGM_Q6, T_AND1_FGA_Q6), NA_real_),
    T_AND1_FG_PCT_CGS = safe_div(T_AND1_FGM_CGS, T_AND1_FGA_CGS),
    
    # PPP (points per AND-1 attempt)
    T_AND1_PPP_Q1  = safe_div(T_AND1_PTS_Q1 , T_AND1_FGA_Q1),
    T_AND1_PPP_Q2  = safe_div(T_AND1_PTS_Q2 , T_AND1_FGA_Q2),
    T_AND1_PPP_Q3  = safe_div(T_AND1_PTS_Q3 , T_AND1_FGA_Q3),
    T_AND1_PPP_Q4  = safe_div(T_AND1_PTS_Q4 , T_AND1_FGA_Q4),
    T_AND1_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_AND1_PTS_Q5, T_AND1_FGA_Q5), NA_real_),
    T_AND1_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_AND1_PTS_Q6, T_AND1_FGA_Q6), NA_real_),
    T_AND1_PPP_CGS = safe_div(T_AND1_PTS_CGS, T_AND1_FGA_CGS),
    
    # RATE = AND-1 FGM / Team FGA
    T_AND1_RATE_Q1  = safe_div(T_AND1_FGM_Q1 , T_FGA_Q1 ),
    T_AND1_RATE_Q2  = safe_div(T_AND1_FGM_Q2 , T_FGA_Q2 ),
    T_AND1_RATE_Q3  = safe_div(T_AND1_FGM_Q3 , T_FGA_Q3 ),
    T_AND1_RATE_Q4  = safe_div(T_AND1_FGM_Q4 , T_FGA_Q4 ),
    T_AND1_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_AND1_FGM_Q5, T_FGA_Q5), NA_real_),
    T_AND1_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_AND1_FGM_Q6, T_FGA_Q6), NA_real_),
    T_AND1_RATE_CGS = safe_div(T_AND1_FGM_CGS, T_FGA_CGS)
  )

rm(and1_base, and1_cgs, and1_qtr)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: AND1 Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Catch and Shoot 2PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Catch-and-Shoot 2PT (CGS only)
# ==================================================================================== #

# ---- Dribbles-Tracked File Path ----
dribble_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/",
  "0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/",
  "nba_dribblestracked_", season_token, ".csv"
))

# ---- Load dribbles file ----
if (!exists("dr_raw") || !is.data.frame(dr_raw)) {
  if (!file.exists(dribble_path)) stop("Dribbles-tracked file not found: ", dribble_path)
  dr_raw <- read.csv(dribble_path, header = TRUE, stringsAsFactors = FALSE)
}

# ---- helpers ----
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

# ---- load file BEFORE aliasing/using it ----
stopifnot(file.exists(dribble_path))
dribble_raw <- suppressWarnings(data.table::fread(dribble_path, na.strings = c("", "NA", "NaN")))

# ---- alias actual columns present in dribble_raw ----
gid_col  <- .pick_col(dribble_raw, c("espn_game_id","ESPN_GAME_ID","game_id","GAME_ID"))
tid_col  <- .pick_col(dribble_raw, c("espn_team_id","ESPN_TEAM_ID","team_id","TEAM_ID"))
low_col  <- .pick_col(dribble_raw, c("dribbles_low","DRIBBLES_LOW","dribble_low"))
high_col <- .pick_col(dribble_raw, c("dribbles_high","DRIBBLES_HIGH","dribble_high"))
fg2a_col <- .pick_col(dribble_raw, c("FG2A","FGA_2PT","TWO_PT_FGA","FG2A_TOTAL","fg2a"))
fg2m_col <- .pick_col(dribble_raw, c("FG2M","FGM_2PT","TWO_PT_FGM","FG2M_TOTAL","fg2m"))

stopifnot(!is.na(gid_col), !is.na(tid_col),
          !is.na(low_col), !is.na(high_col),
          !is.na(fg2a_col), !is.na(fg2m_col))

# ---- build CAS 2PT = 0-dribble bucket (CGS only) ----
cas2pt_cgs <-
  dribble_raw %>%
  dplyr::transmute(
    ESPN_GAME_ID = as.character(.data[[gid_col]]),
    ESPN_TEAM_ID = as.character(.data[[tid_col]]),
    d_low        = suppressWarnings(as.integer(.data[[low_col]])),
    d_high       = suppressWarnings(as.integer(.data[[high_col]])),
    FG2A         = suppressWarnings(as.numeric(.data[[fg2a_col]])),
    FG2M         = suppressWarnings(as.numeric(.data[[fg2m_col]]))
  ) %>%
  dplyr::filter(!is.na(ESPN_GAME_ID), !is.na(ESPN_TEAM_ID)) %>%
  dplyr::filter(d_high == 0) %>%            # Catch-and-Shoot bucket: 0 dribbles
  dplyr::group_by(ESPN_GAME_ID, ESPN_TEAM_ID) %>%
  dplyr::summarise(
    T_CAS_2PT_FGA_CGS = sum(FG2A, na.rm = TRUE),
    T_CAS_2PT_FGM_CGS = sum(FG2M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    T_CAS_2PT_PTS_CGS  = 2L * T_CAS_2PT_FGM_CGS,
    T_CAS_2PT_POSS_CGS = T_CAS_2PT_FGA_CGS
  )

# ---- ensure left table exists & keys are character for join ----
if (!exists("BaseStats_Team_MC")) stop("BaseStats_Team_MC not found in environment.")

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

# ---- join + derived ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(cas2pt_cgs, by = c("ESPN_GAME_ID","ESPN_TEAM_ID")) %>%
  dplyr::mutate(
    dplyr::across(
      c(T_CAS_2PT_FGA_CGS, T_CAS_2PT_FGM_CGS, T_CAS_2PT_PTS_CGS, T_CAS_2PT_POSS_CGS),
      ~ dplyr::coalesce(., 0)
    ),
    T_CAS_2PT_FG_PCT_CGS  = safe_div(T_CAS_2PT_FGM_CGS, T_CAS_2PT_FGA_CGS),
    T_CAS_2PT_EFG_PCT_CGS = safe_div(T_CAS_2PT_FGM_CGS, T_CAS_2PT_FGA_CGS),
    T_CAS_2PT_PPP_CGS     = safe_div(T_CAS_2PT_PTS_CGS, T_CAS_2PT_POSS_CGS),
    T_CAS_2PT_PTSHR_CGS   = ifelse(!("T_PTS_CGS" %in% names(BaseStats_Team_MC)),
                                   NA_real_,
                                   safe_div(T_CAS_2PT_PTS_CGS, T_PTS_CGS) * 100),
    T_CAS_2PT_RATE_CGS    = ifelse(!("T_FGA_CGS" %in% names(BaseStats_Team_MC)),
                                   NA_real_,
                                   safe_div(T_CAS_2PT_FGA_CGS, T_FGA_CGS) * 100)
  )

rm(cas2pt_cgs)

message("[✓] Catch-and-Shoot 2PT (0-dribble) CGS aggregated and merged: ", season_token)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Catch and Shoot 2PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Catch and Shoot 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ==================================================================================== #
# Section: Catch-and-Shoot 3PT (CGS only)
# Source: nba_dribblestracked_yyyy_yyyy.csv (season tokenized)
# Creates (CGS only):
#   T_CAS_3PT_FGA_CGS, T_CAS_3PT_FGM_CGS, T_CAS_3PT_PTS_CGS, T_CAS_3PT_POSS_CGS
#   T_CAS_3PT_FG_PCT_CGS, T_CAS_3PT_EFG_PCT_CGS, T_CAS_3PT_PPP_CGS
#   T_CAS_3PT_PTSHR_CGS, T_CAS_3PT_RATE_CGS
# Notes: CGS only (dribble tracking not quartered). CAS = 0 dribbles.
# ==================================================================================== #

# ---- Dribbles-Tracked File Path ----
dribble_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/",
  "0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/",
  "nba_dribblestracked_", season_token, ".csv"
))

# ---- Load dribbles file ----
if (!exists("dr_raw") || !is.data.frame(dr_raw)) {
  if (!file.exists(dribble_path)) stop("Dribbles-tracked file not found: ", dribble_path)
  dr_raw <- read.csv(dribble_path, header = TRUE, stringsAsFactors = FALSE)
}


# ---- helpers (reuse if already defined) ----
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

# ---- load file (only if not in memory) ----
if (!exists("dribble_raw")) {
  stopifnot(file.exists(dribble_path))
  dribble_raw <- suppressWarnings(data.table::fread(dribble_path, na.strings = c("", "NA", "NaN")))
}

# ---- alias actual columns present in dribble_raw ----
gid_col   <- .pick_col(dribble_raw, c("espn_game_id","ESPN_GAME_ID","game_id","GAME_ID"))
tid_col   <- .pick_col(dribble_raw, c("espn_team_id","ESPN_TEAM_ID","team_id","TEAM_ID"))
low_col   <- .pick_col(dribble_raw, c("dribbles_low","DRIBBLES_LOW","dribble_low"))
high_col  <- .pick_col(dribble_raw, c("dribbles_high","DRIBBLES_HIGH","dribble_high"))
fg3a_col  <- .pick_col(dribble_raw, c("FG3A","FGA_3PT","THREE_PT_FGA","FG3A_TOTAL","fg3a"))
fg3m_col  <- .pick_col(dribble_raw, c("FG3M","FGM_3PT","THREE_PT_FGM","FG3M_TOTAL","fg3m"))

stopifnot(!is.na(gid_col), !is.na(tid_col),
          !is.na(low_col), !is.na(high_col),
          !is.na(fg3a_col), !is.na(fg3m_col))

# ---- build CAS 3PT = 0-dribble bucket (CGS only) ----
cas3pt_cgs <-
  dribble_raw %>%
  dplyr::transmute(
    ESPN_GAME_ID = as.character(.data[[gid_col]]),
    ESPN_TEAM_ID = as.character(.data[[tid_col]]),
    d_low        = suppressWarnings(as.integer(.data[[low_col]])),
    d_high       = suppressWarnings(as.integer(.data[[high_col]])),
    FG3A         = suppressWarnings(as.numeric(.data[[fg3a_col]])),
    FG3M         = suppressWarnings(as.numeric(.data[[fg3m_col]]))
  ) %>%
  dplyr::filter(!is.na(ESPN_GAME_ID), !is.na(ESPN_TEAM_ID)) %>%
  dplyr::filter(d_high == 0) %>%            # Catch-and-Shoot = 0 dribbles
  dplyr::group_by(ESPN_GAME_ID, ESPN_TEAM_ID) %>%
  dplyr::summarise(
    T_CAS_3PT_FGA_CGS = sum(FG3A, na.rm = TRUE),
    T_CAS_3PT_FGM_CGS = sum(FG3M, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    T_CAS_3PT_PTS_CGS  = 3L * T_CAS_3PT_FGM_CGS,
    T_CAS_3PT_POSS_CGS = T_CAS_3PT_FGA_CGS
  )

# ---- ensure left table exists & keys are character ----
if (!exists("BaseStats_Team_MC")) stop("BaseStats_Team_MC not found in environment.")
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

# ---- join + derived ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(cas3pt_cgs, by = c("ESPN_GAME_ID","ESPN_TEAM_ID")) %>%
  dplyr::mutate(
    dplyr::across(
      c(T_CAS_3PT_FGA_CGS, T_CAS_3PT_FGM_CGS, T_CAS_3PT_PTS_CGS, T_CAS_3PT_POSS_CGS),
      ~ dplyr::coalesce(., 0)
    ),
    # FG%
    T_CAS_3PT_FG_PCT_CGS  = safe_div(T_CAS_3PT_FGM_CGS, T_CAS_3PT_FGA_CGS),
    # eFG% = (FGM + 0.5*FG3M) / FGA ; here FGM==FG3M
    T_CAS_3PT_EFG_PCT_CGS = safe_div(T_CAS_3PT_FGM_CGS + 0.5 * T_CAS_3PT_FGM_CGS, T_CAS_3PT_FGA_CGS),
    # PPP (points per CAS 3PT possession = attempt)
    T_CAS_3PT_PPP_CGS     = safe_div(T_CAS_3PT_PTS_CGS, T_CAS_3PT_POSS_CGS),
    # Points share of team total points (percentage)
    T_CAS_3PT_PTSHR_CGS   = ifelse(!("T_PTS_CGS" %in% names(BaseStats_Team_MC)),
                                   NA_real_,
                                   safe_div(T_CAS_3PT_PTS_CGS, T_PTS_CGS) * 100),
    # Rate vs team total FGA (percentage)
    T_CAS_3PT_RATE_CGS    = ifelse(!("T_FGA_CGS" %in% names(BaseStats_Team_MC)),
                                   NA_real_,
                                   safe_div(T_CAS_3PT_FGA_CGS, T_FGA_CGS) * 100)
  )

rm(cas2pt_cgs, cas3pt_cgs)
message("[✓] Catch-and-Shoot 3PT (0-dribble) CGS aggregated and merged: ", season_token)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Catch and Shoot 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Mid-Range Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==================================================================================== #
# Section: Mid-Range (2PT) — Q1–Q4 + CGS                                               #
# Source: pm_df                                                                         #
# Creates:                                                                              #
#   FGA/FGM/PTS  -> T_MIDR_FGA_Q1..Q4, T_MIDR_FGA_CGS; T_MIDR_FGM_Q1..Q4, T_MIDR_FGM_CGS
#                   T_MIDR_PTS_Q1..Q4, T_MIDR_PTS_CGS                                   #
#   Derived      -> T_MIDR_PCT_Q*, T_MIDR_EFG_PCT_Q*, T_MIDR_PPP_Q*,                    #
#                   T_MIDR_PTSHR_Q*, T_MIDR_RATE_Q*                                     #
# Notes: Mid-Range is always 2PT.                                                       #
# ==================================================================================== #

# ===============================
# MID-RANGE: FGA, FGM, FG%, PPP, PTS, RATE, PTS SHARE
# Q1–Q6 (OT-aware) + CGS with OT masking
# ===============================

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# pick 'SHOT_ZONE_BASIC' if present (case-insensitive), else rely on type_text
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(pm_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Mid-Range base ----------------------------------------------------------- #
midr_base <-
  pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_midr = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) == "mid-range"
    } else {
      stringr::str_detect(type_text, stringr::regex("mid[- ]?range", ignore_case = TRUE))
    }
  ) %>%
  dplyr::filter(is_midr, shot) %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    midr_fga = TRUE,
    midr_fgm = make & !is.na(pts) & pts > 0L,
    midr_pts = dplyr::if_else(midr_fgm, pts, 0L)  # should equal 2 per make
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
midr_qtr <-
  midr_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_MIDR_FGA = sum(midr_fga, na.rm = TRUE),
    T_MIDR_FGM = sum(midr_fgm, na.rm = TRUE),
    T_MIDR_PTS = sum(midr_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_MIDR_FGA, T_MIDR_FGM, T_MIDR_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
midr_cgs <-
  midr_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_MIDR_FGA_CGS = sum(midr_fga, na.rm = TRUE),
    T_MIDR_FGM_CGS = sum(midr_fgm, na.rm = TRUE),
    T_MIDR_PTS_CGS = sum(midr_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Team_MC + coalesce + OT masking ---------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(midr_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(midr_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    dplyr::across(
      c(T_MIDR_FGA_Q1, T_MIDR_FGA_Q2, T_MIDR_FGA_Q3, T_MIDR_FGA_Q4, T_MIDR_FGA_Q5, T_MIDR_FGA_Q6, T_MIDR_FGA_CGS,
        T_MIDR_FGM_Q1, T_MIDR_FGM_Q2, T_MIDR_FGM_Q3, T_MIDR_FGM_Q4, T_MIDR_FGM_Q5, T_MIDR_FGM_Q6, T_MIDR_FGM_CGS,
        T_MIDR_PTS_Q1, T_MIDR_PTS_Q2, T_MIDR_PTS_Q3, T_MIDR_PTS_Q4, T_MIDR_PTS_Q5, T_MIDR_PTS_Q6, T_MIDR_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # expose OT only if those OTs happened
    T_MIDR_FGA_Q5 = dplyr::if_else(OT_1 == 1L, T_MIDR_FGA_Q5, 0L),
    T_MIDR_FGM_Q5 = dplyr::if_else(OT_1 == 1L, T_MIDR_FGM_Q5, 0L),
    T_MIDR_PTS_Q5 = dplyr::if_else(OT_1 == 1L, T_MIDR_PTS_Q5, 0L),
    
    T_MIDR_FGA_Q6 = dplyr::if_else(OT_2 == 1L, T_MIDR_FGA_Q6, 0L),
    T_MIDR_FGM_Q6 = dplyr::if_else(OT_2 == 1L, T_MIDR_FGM_Q6, 0L),
    T_MIDR_PTS_Q6 = dplyr::if_else(OT_2 == 1L, T_MIDR_PTS_Q6, 0L)
  )

# ---- Derived metrics ---------------------------------------------------------------- #
# 1) PCT / EFG / PPP / RATE  (no dependency on T_PTS_SCORED_*)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG% (and eFG% = FG% for 2PT)
    T_MIDR_PCT_Q1  = safe_div(T_MIDR_FGM_Q1 , T_MIDR_FGA_Q1),
    T_MIDR_PCT_Q2  = safe_div(T_MIDR_FGM_Q2 , T_MIDR_FGA_Q2),
    T_MIDR_PCT_Q3  = safe_div(T_MIDR_FGM_Q3 , T_MIDR_FGA_Q3),
    T_MIDR_PCT_Q4  = safe_div(T_MIDR_FGM_Q4 , T_MIDR_FGA_Q4),
    T_MIDR_PCT_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_MIDR_FGM_Q5, T_MIDR_FGA_Q5), NA_real_),
    T_MIDR_PCT_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_MIDR_FGM_Q6, T_MIDR_FGA_Q6), NA_real_),
    T_MIDR_PCT_CGS = safe_div(T_MIDR_FGM_CGS, T_MIDR_FGA_CGS),
    
    T_MIDR_EFG_PCT_Q1  = T_MIDR_PCT_Q1,
    T_MIDR_EFG_PCT_Q2  = T_MIDR_PCT_Q2,
    T_MIDR_EFG_PCT_Q3  = T_MIDR_PCT_Q3,
    T_MIDR_EFG_PCT_Q4  = T_MIDR_PCT_Q4,
    T_MIDR_EFG_PCT_Q5  = T_MIDR_PCT_Q5,
    T_MIDR_EFG_PCT_Q6  = T_MIDR_PCT_Q6,
    T_MIDR_EFG_PCT_CGS = T_MIDR_PCT_CGS,
    
    # PPP
    T_MIDR_PPP_Q1  = safe_div(T_MIDR_PTS_Q1 , T_MIDR_FGA_Q1),
    T_MIDR_PPP_Q2  = safe_div(T_MIDR_PTS_Q2 , T_MIDR_FGA_Q2),
    T_MIDR_PPP_Q3  = safe_div(T_MIDR_PTS_Q3 , T_MIDR_FGA_Q3),
    T_MIDR_PPP_Q4  = safe_div(T_MIDR_PTS_Q4 , T_MIDR_FGA_Q4),
    T_MIDR_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_MIDR_PTS_Q5, T_MIDR_FGA_Q5), NA_real_),
    T_MIDR_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_MIDR_PTS_Q6, T_MIDR_FGA_Q6), NA_real_),
    T_MIDR_PPP_CGS = safe_div(T_MIDR_PTS_CGS, T_MIDR_FGA_CGS),
    
    # RATE
    T_MIDR_RATE_Q1  = safe_div(T_MIDR_FGA_Q1 , T_FGA_Q1 ),
    T_MIDR_RATE_Q2  = safe_div(T_MIDR_FGA_Q2 , T_FGA_Q2 ),
    T_MIDR_RATE_Q3  = safe_div(T_MIDR_FGA_Q3 , T_FGA_Q3 ),
    T_MIDR_RATE_Q4  = safe_div(T_MIDR_FGA_Q4 , T_FGA_Q4 ),
    T_MIDR_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_MIDR_FGA_Q5, T_FGA_Q5), NA_real_),
    T_MIDR_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_MIDR_FGA_Q6, T_FGA_Q6), NA_real_),
    T_MIDR_RATE_CGS = safe_div(T_MIDR_FGA_CGS, T_FGA_CGS)
  )

# 2) PTS SHARE (requires T_PTS_SCORED_* to already exist)
needed_scored <- c(paste0("T_PTS_SCORED_Q",1:4), "T_PTS_SCORED_Q5","T_PTS_SCORED_Q6","T_PTS_SCORED_CGS")
stopifnot(all(needed_scored %in% names(BaseStats_Team_MC)))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_MIDR_PTSHR_Q1  = safe_div(T_MIDR_PTS_Q1 , T_PTS_SCORED_Q1 ),
    T_MIDR_PTSHR_Q2  = safe_div(T_MIDR_PTS_Q2 , T_PTS_SCORED_Q2 ),
    T_MIDR_PTSHR_Q3  = safe_div(T_MIDR_PTS_Q3 , T_PTS_SCORED_Q3 ),
    T_MIDR_PTSHR_Q4  = safe_div(T_MIDR_PTS_Q4 , T_PTS_SCORED_Q4 ),
    T_MIDR_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_MIDR_PTS_Q5, T_PTS_SCORED_Q5), NA_real_),
    T_MIDR_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_MIDR_PTS_Q6, T_PTS_SCORED_Q6), NA_real_),
    T_MIDR_PTSHR_CGS = safe_div(T_MIDR_PTS_CGS, T_PTS_SCORED_CGS)
  )


rm(midr_base, midr_cgs, midr_qtr)
message("[✓] Mid-Range (2PT) — Q1–Q6 (OT-aware) + CGS section complete.")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Mid-Range Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Rim (Restricted Area) Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Rim (Restricted Area, 2PT) — Q1–Q4 + CGS                                    #
# Source: pm_df                                                                         #
# Creates:                                                                              #
#   FGA/FGM/PTS  -> T_RIM_FGA_Q1..Q4, T_RIM_FGA_CGS; T_RIM_FGM_Q1..Q4, T_RIM_FGM_CGS;  #
#                   T_RIM_PTS_Q1..Q4, T_RIM_PTS_CGS                                     #
#   Derived      -> T_RIM_PCT_Q*, T_RIM_EFG_PCT_Q*, T_RIM_PPP_Q*,                       #
#                   T_RIM_PTSHR_Q*, T_RIM_RATE_Q*                                       #
# Notes: Detection = SHOT_ZONE_BASIC == "Restricted Area" (fallback to text match).     #
# ==================================================================================== #

# ==================================================================================== #
# Rim (Restricted Area, 2PT) — Q1–Q6 (OT-aware) + CGS                                  #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) { hit <- candidates[candidates %in% names(df)]; if (length(hit)) hit[[1]] else NA_character_ }
sz_col <- .pick_col(pm_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Rim base ----------------------------------------------------------------- #
rim_base <- pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_rim = tolower(trimws(.data[[sz_col]])) == "restricted area"
  ) %>%                                      # <-- pipe continues here
  dplyr::filter(is_rim, shot) %>%            # works now
  dplyr::transmute(
    game_id, team_id, qtr,
    rim_fga = TRUE,
    rim_fgm = make & !is.na(pts) & pts > 0L,
    rim_pts = dplyr::if_else(rim_fgm, pts, 0L)
  )


# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
rim_qtr <- rim_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_RIM_FGA = sum(rim_fga, na.rm = TRUE),
    T_RIM_FGM = sum(rim_fgm, na.rm = TRUE),
    T_RIM_PTS = sum(rim_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_RIM_FGA, T_RIM_FGM, T_RIM_PTS),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# ---- Complete game (CGS) ------------------------------------------------------------ #
rim_cgs <- rim_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_RIM_FGA_CGS = sum(rim_fga, na.rm = TRUE),
    T_RIM_FGM_CGS = sum(rim_fgm, na.rm = TRUE),
    T_RIM_PTS_CGS = sum(rim_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join & fill -------------------------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(rim_qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(rim_cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    dplyr::across(
      c(T_RIM_FGA_Q1, T_RIM_FGA_Q2, T_RIM_FGA_Q3, T_RIM_FGA_Q4, T_RIM_FGA_Q5, T_RIM_FGA_Q6, T_RIM_FGA_CGS,
        T_RIM_FGM_Q1, T_RIM_FGM_Q2, T_RIM_FGM_Q3, T_RIM_FGM_Q4, T_RIM_FGM_Q5, T_RIM_FGM_Q6, T_RIM_FGM_CGS,
        T_RIM_PTS_Q1, T_RIM_PTS_Q2, T_RIM_PTS_Q3, T_RIM_PTS_Q4, T_RIM_PTS_Q5, T_RIM_PTS_Q6, T_RIM_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- Derived (FG%/eFG%, PPP, Rate) — no PTSHR here --------------------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG% (and eFG% == FG% for 2PT-only)
    T_RIM_PCT_Q1  = safe_div(T_RIM_FGM_Q1 , T_RIM_FGA_Q1),
    T_RIM_PCT_Q2  = safe_div(T_RIM_FGM_Q2 , T_RIM_FGA_Q2),
    T_RIM_PCT_Q3  = safe_div(T_RIM_FGM_Q3 , T_RIM_FGA_Q3),
    T_RIM_PCT_Q4  = safe_div(T_RIM_FGM_Q4 , T_RIM_FGA_Q4),
    T_RIM_PCT_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_RIM_FGM_Q5, T_RIM_FGA_Q5), NA_real_),
    T_RIM_PCT_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_RIM_FGM_Q6, T_RIM_FGA_Q6), NA_real_),
    T_RIM_PCT_CGS = safe_div(T_RIM_FGM_CGS, T_RIM_FGA_CGS),
    
    T_RIM_EFG_PCT_Q1  = T_RIM_PCT_Q1,
    T_RIM_EFG_PCT_Q2  = T_RIM_PCT_Q2,
    T_RIM_EFG_PCT_Q3  = T_RIM_PCT_Q3,
    T_RIM_EFG_PCT_Q4  = T_RIM_PCT_Q4,
    T_RIM_EFG_PCT_Q5  = T_RIM_PCT_Q5,
    T_RIM_EFG_PCT_Q6  = T_RIM_PCT_Q6,
    T_RIM_EFG_PCT_CGS = T_RIM_PCT_CGS,
    
    # PPP
    T_RIM_PPP_Q1  = safe_div(T_RIM_PTS_Q1 , T_RIM_FGA_Q1),
    T_RIM_PPP_Q2  = safe_div(T_RIM_PTS_Q2 , T_RIM_FGA_Q2),
    T_RIM_PPP_Q3  = safe_div(T_RIM_PTS_Q3 , T_RIM_FGA_Q3),
    T_RIM_PPP_Q4  = safe_div(T_RIM_PTS_Q4 , T_RIM_FGA_Q4),
    T_RIM_PPP_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_RIM_PTS_Q5, T_RIM_FGA_Q5), NA_real_),
    T_RIM_PPP_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_RIM_PTS_Q6, T_RIM_FGA_Q6), NA_real_),
    T_RIM_PPP_CGS = safe_div(T_RIM_PTS_CGS, T_RIM_FGA_CGS),
    
    # Rate vs team FGA
    # Rate vs total team FGA
    T_RIM_RATE_Q1  = safe_div(T_RIM_FGA_Q1 , T_FGA_Q1),
    T_RIM_RATE_Q2  = safe_div(T_RIM_FGA_Q2 , T_FGA_Q2),
    T_RIM_RATE_Q3  = safe_div(T_RIM_FGA_Q3 , T_FGA_Q3),
    T_RIM_RATE_Q4  = safe_div(T_RIM_FGA_Q4 , T_FGA_Q4),
    
    # OT periods only count if they actually happened
    T_RIM_RATE_Q5  = dplyr::if_else(OT_1 == 1L, safe_div(T_RIM_FGA_Q5, T_FGA_Q5), NA_real_),
    T_RIM_RATE_Q6  = dplyr::if_else(OT_2 == 1L, safe_div(T_RIM_FGA_Q6, T_FGA_Q6), NA_real_),
    
    T_RIM_RATE_CGS = safe_div(T_RIM_FGA_CGS, T_FGA_CGS)
  )

# ---- PTSHR in its own mutate (ensures T_PTS_SCORED_* already exist) ----------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    T_RIM_PTSHR_Q1  = safe_div(T_RIM_PTS_Q1 , T_PTS_SCORED_Q1),
    T_RIM_PTSHR_Q2  = safe_div(T_RIM_PTS_Q2 , T_PTS_SCORED_Q2),
    T_RIM_PTSHR_Q3  = safe_div(T_RIM_PTS_Q3 , T_PTS_SCORED_Q3),
    T_RIM_PTSHR_Q4  = safe_div(T_RIM_PTS_Q4 , T_PTS_SCORED_Q4),
    
    T_RIM_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L,
                                     safe_div(T_RIM_PTS_Q5, T_PTS_SCORED_Q5),
                                     NA_real_),
    
    T_RIM_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L,
                                     safe_div(T_RIM_PTS_Q6, T_PTS_SCORED_Q6),
                                     NA_real_),
    
    T_RIM_PTSHR_CGS = safe_div(T_RIM_PTS_CGS, T_PTS_SCORED_CGS)
  )


message("[✓] Rim (Restricted Area) — Q1–Q6 (OT-aware) + CGS section complete (PTSHR split).")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Rim (Restricted Area) Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Putback Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Putbacks (2PT) — Q1–Q6 (OT-aware) + CGS                                    #
# Source: pm_df                                                                        #
# Creates:                                                                             #
#   FGA/FGM/PTS  -> T_PUTB_FGA_Q1..Q6, T_PUTB_FGA_CGS;                                 #
#                    T_PUTB_FGM_Q1..Q6, T_PUTB_FGM_CGS;                                #
#                    T_PUTB_PTS_Q1..Q6, T_PUTB_PTS_CGS                                 #
#   Derived       -> FG% (T_PUTB_PCT_*), EFG% (=FG%), PPP (T_PUTB_PPP_*),              #
#                    Point Share (T_PUTB_PTSHR_*), Rate (T_PUTB_RATE_*)                #
# Detection: type_text contains "putback" (case-insensitive).                          #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build Putback base ------------------------------------------------------------- #
putb_base <-
  pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_putb = stringr::str_detect(type_text,
                                  stringr::regex("\\bputback\\b", ignore_case = TRUE))
  ) %>%
  dplyr::filter(is_putb, shot) %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    putb_fga = TRUE,
    putb_fgm = make & !is.na(pts) & pts > 0L,
    putb_pts = dplyr::if_else(putb_fgm, pts, 0L)  # usually 2 per make
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
putb_qtr <-
  putb_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_PUTB_FGA = sum(putb_fga, na.rm = TRUE),
    T_PUTB_FGM = sum(putb_fgm, na.rm = TRUE),
    T_PUTB_PTS = sum(putb_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_PUTB_FGA, T_PUTB_FGM, T_PUTB_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
putb_cgs <-
  putb_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_PUTB_FGA_CGS = sum(putb_fga, na.rm = TRUE),
    T_PUTB_FGM_CGS = sum(putb_fgm, na.rm = TRUE),
    T_PUTB_PTS_CGS = sum(putb_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Team_MC ---------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(putb_qtr, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(putb_cgs, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_PUTB_FGA_Q1 = dplyr::coalesce(T_PUTB_FGA_Q1, 0L),
    T_PUTB_FGA_Q2 = dplyr::coalesce(T_PUTB_FGA_Q2, 0L),
    T_PUTB_FGA_Q3 = dplyr::coalesce(T_PUTB_FGA_Q3, 0L),
    T_PUTB_FGA_Q4 = dplyr::coalesce(T_PUTB_FGA_Q4, 0L),
    T_PUTB_FGM_Q1 = dplyr::coalesce(T_PUTB_FGM_Q1, 0L),
    T_PUTB_FGM_Q2 = dplyr::coalesce(T_PUTB_FGM_Q2, 0L),
    T_PUTB_FGM_Q3 = dplyr::coalesce(T_PUTB_FGM_Q3, 0L),
    T_PUTB_FGM_Q4 = dplyr::coalesce(T_PUTB_FGM_Q4, 0L),
    T_PUTB_PTS_Q1 = dplyr::coalesce(T_PUTB_PTS_Q1, 0L),
    T_PUTB_PTS_Q2 = dplyr::coalesce(T_PUTB_PTS_Q2, 0L),
    T_PUTB_PTS_Q3 = dplyr::coalesce(T_PUTB_PTS_Q3, 0L),
    T_PUTB_PTS_Q4 = dplyr::coalesce(T_PUTB_PTS_Q4, 0L),
    
    # OT: only keep if that OT happened; otherwise 0
    T_PUTB_FGA_Q5 = dplyr::if_else(OT_1 == 1L,
                                   dplyr::coalesce(T_PUTB_FGA_Q5, 0L),
                                   0L),
    T_PUTB_FGA_Q6 = dplyr::if_else(OT_2 == 1L,
                                   dplyr::coalesce(T_PUTB_FGA_Q6, 0L),
                                   0L),
    T_PUTB_FGM_Q5 = dplyr::if_else(OT_1 == 1L,
                                   dplyr::coalesce(T_PUTB_FGM_Q5, 0L),
                                   0L),
    T_PUTB_FGM_Q6 = dplyr::if_else(OT_2 == 1L,
                                   dplyr::coalesce(T_PUTB_FGM_Q6, 0L),
                                   0L),
    T_PUTB_PTS_Q5 = dplyr::if_else(OT_1 == 1L,
                                   dplyr::coalesce(T_PUTB_PTS_Q5, 0L),
                                   0L),
    T_PUTB_PTS_Q6 = dplyr::if_else(OT_2 == 1L,
                                   dplyr::coalesce(T_PUTB_PTS_Q6, 0L),
                                   0L),
    
    T_PUTB_FGA_CGS = dplyr::coalesce(T_PUTB_FGA_CGS, 0L),
    T_PUTB_FGM_CGS = dplyr::coalesce(T_PUTB_FGM_CGS, 0L),
    T_PUTB_PTS_CGS = dplyr::coalesce(T_PUTB_PTS_CGS, 0L)
  )

# ---- Derived metrics ---------------------------------------------------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG% (eFG% identical for 2PT)
    T_PUTB_PCT_Q1  = safe_div(T_PUTB_FGM_Q1 , T_PUTB_FGA_Q1),
    T_PUTB_PCT_Q2  = safe_div(T_PUTB_FGM_Q2 , T_PUTB_FGA_Q2),
    T_PUTB_PCT_Q3  = safe_div(T_PUTB_FGM_Q3 , T_PUTB_FGA_Q3),
    T_PUTB_PCT_Q4  = safe_div(T_PUTB_FGM_Q4 , T_PUTB_FGA_Q4),
    T_PUTB_PCT_Q5  = dplyr::if_else(OT_1 == 1L,
                                    safe_div(T_PUTB_FGM_Q5, T_PUTB_FGA_Q5),
                                    NA_real_),
    T_PUTB_PCT_Q6  = dplyr::if_else(OT_2 == 1L,
                                    safe_div(T_PUTB_FGM_Q6, T_PUTB_FGA_Q6),
                                    NA_real_),
    T_PUTB_PCT_CGS = safe_div(T_PUTB_FGM_CGS, T_PUTB_FGA_CGS),
    
    T_PUTB_EFG_PCT_Q1  = T_PUTB_PCT_Q1,
    T_PUTB_EFG_PCT_Q2  = T_PUTB_PCT_Q2,
    T_PUTB_EFG_PCT_Q3  = T_PUTB_PCT_Q3,
    T_PUTB_EFG_PCT_Q4  = T_PUTB_PCT_Q4,
    T_PUTB_EFG_PCT_Q5  = T_PUTB_PCT_Q5,
    T_PUTB_EFG_PCT_Q6  = T_PUTB_PCT_Q6,
    T_PUTB_EFG_PCT_CGS = T_PUTB_PCT_CGS,
    
    # PPP
    T_PUTB_PPP_Q1  = safe_div(T_PUTB_PTS_Q1 , T_PUTB_FGA_Q1),
    T_PUTB_PPP_Q2  = safe_div(T_PUTB_PTS_Q2 , T_PUTB_FGA_Q2),
    T_PUTB_PPP_Q3  = safe_div(T_PUTB_PTS_Q3 , T_PUTB_FGA_Q3),
    T_PUTB_PPP_Q4  = safe_div(T_PUTB_PTS_Q4 , T_PUTB_FGA_Q4),
    T_PUTB_PPP_Q5  = dplyr::if_else(OT_1 == 1L,
                                    safe_div(T_PUTB_PTS_Q5, T_PUTB_FGA_Q5),
                                    NA_real_),
    T_PUTB_PPP_Q6  = dplyr::if_else(OT_2 == 1L,
                                    safe_div(T_PUTB_PTS_Q6, T_PUTB_FGA_Q6),
                                    NA_real_),
    T_PUTB_PPP_CGS = safe_div(T_PUTB_PTS_CGS, T_PUTB_FGA_CGS),
    
    # Point Share vs team points (use T_PTS_SCORED_*)
    T_PUTB_PTSHR_Q1  = safe_div(T_PUTB_PTS_Q1 , T_PTS_SCORED_Q1),
    T_PUTB_PTSHR_Q2  = safe_div(T_PUTB_PTS_Q2 , T_PTS_SCORED_Q2),
    T_PUTB_PTSHR_Q3  = safe_div(T_PUTB_PTS_Q3 , T_PTS_SCORED_Q3),
    T_PUTB_PTSHR_Q4  = safe_div(T_PUTB_PTS_Q4 , T_PTS_SCORED_Q4),
    T_PUTB_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L,
                                      safe_div(T_PUTB_PTS_Q5, T_PTS_SCORED_Q5),
                                      NA_real_),
    T_PUTB_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L,
                                      safe_div(T_PUTB_PTS_Q6, T_PTS_SCORED_Q6),
                                      NA_real_),
    T_PUTB_PTSHR_CGS = safe_div(T_PUTB_PTS_CGS, T_PTS_SCORED_CGS),
    
    # Rate vs total FGA
    T_PUTB_RATE_Q1  = safe_div(T_PUTB_FGA_Q1 , T_FGA_Q1),
    T_PUTB_RATE_Q2  = safe_div(T_PUTB_FGA_Q2 , T_FGA_Q2),
    T_PUTB_RATE_Q3  = safe_div(T_PUTB_FGA_Q3 , T_FGA_Q3),
    T_PUTB_RATE_Q4  = safe_div(T_PUTB_FGA_Q4 , T_FGA_Q4),
    T_PUTB_RATE_Q5  = dplyr::if_else(OT_1 == 1L,
                                     safe_div(T_PUTB_FGA_Q5, T_FGA_Q5),
                                     NA_real_),
    T_PUTB_RATE_Q6  = dplyr::if_else(OT_2 == 1L,
                                     safe_div(T_PUTB_FGA_Q6, T_FGA_Q6),
                                     NA_real_),
    T_PUTB_RATE_CGS = safe_div(T_PUTB_FGA_CGS, T_FGA_CGS)
  )

rm(putb_base, putb_cgs, putb_qtr)
message("[✓] Putbacks — Q1–Q6 (OT-aware) + CGS section complete.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Putback Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Corner 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Corner 3PT — Q1–Q4 + CGS                                                    #
# Source: pm_df                                                                         #
# Creates:                                                                              #
#   FGA/FGM/PTS  -> T_CNR_3PT_FGA_Q1..Q4, T_CNR_3PT_FGA_CGS;                           #
#                   T_CNR_3PT_FGM_Q1..Q4, T_CNR_3PT_FGM_CGS;                           #
#                   T_CNR_3PT_PTS_Q1..Q4, T_CNR_3PT_PTS_CGS                             #
#   Derived      -> FG% (T_CNR_3PT_PCT_*), EFG% (T_CNR_3PT_EFG_PCT_*), PPP,            #
#                   PTSHR (share of team points), RATE (share of team FGA)             #
# Detection: SHOT_ZONE_BASIC in {"Right Corner 3","Left Corner 3"};                     #
#            fallback text contains "Corner 3".                                         #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(pm_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Corner-3 base ------------------------------------------------------------ #
cnr3_base <-
  pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_corner3 = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("right corner 3","left corner 3")
    } else {
      stringr::str_detect(type_text, stringr::regex("corner\\s*3", ignore_case = TRUE))
    }
  ) %>%
  dplyr::filter(is_corner3, shot) %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    c3_fga = TRUE,
    c3_fgm = make & !is.na(pts) & pts > 0L,
    c3_pts = dplyr::if_else(c3_fgm, pts, 0L)  # should be 3 per make
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
cnr3_qtr <-
  cnr3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_CNR_3PT_FGA = sum(c3_fga, na.rm = TRUE),
    T_CNR_3PT_FGM = sum(c3_fgm, na.rm = TRUE),
    T_CNR_3PT_PTS = sum(c3_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_CNR_3PT_FGA, T_CNR_3PT_FGM, T_CNR_3PT_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
cnr3_cgs <-
  cnr3_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_CNR_3PT_FGA_CGS = sum(c3_fga, na.rm = TRUE),
    T_CNR_3PT_FGM_CGS = sum(c3_fgm, na.rm = TRUE),
    T_CNR_3PT_PTS_CGS = sum(c3_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Team_MC ---------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(cnr3_qtr, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(cnr3_cgs, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_CNR_3PT_FGA_Q1 = dplyr::coalesce(T_CNR_3PT_FGA_Q1, 0L),
    T_CNR_3PT_FGA_Q2 = dplyr::coalesce(T_CNR_3PT_FGA_Q2, 0L),
    T_CNR_3PT_FGA_Q3 = dplyr::coalesce(T_CNR_3PT_FGA_Q3, 0L),
    T_CNR_3PT_FGA_Q4 = dplyr::coalesce(T_CNR_3PT_FGA_Q4, 0L),
    T_CNR_3PT_FGM_Q1 = dplyr::coalesce(T_CNR_3PT_FGM_Q1, 0L),
    T_CNR_3PT_FGM_Q2 = dplyr::coalesce(T_CNR_3PT_FGM_Q2, 0L),
    T_CNR_3PT_FGM_Q3 = dplyr::coalesce(T_CNR_3PT_FGM_Q3, 0L),
    T_CNR_3PT_FGM_Q4 = dplyr::coalesce(T_CNR_3PT_FGM_Q4, 0L),
    T_CNR_3PT_PTS_Q1 = dplyr::coalesce(T_CNR_3PT_PTS_Q1, 0L),
    T_CNR_3PT_PTS_Q2 = dplyr::coalesce(T_CNR_3PT_PTS_Q2, 0L),
    T_CNR_3PT_PTS_Q3 = dplyr::coalesce(T_CNR_3PT_PTS_Q3, 0L),
    T_CNR_3PT_PTS_Q4 = dplyr::coalesce(T_CNR_3PT_PTS_Q4, 0L),
    
    # OT: only if OT happened; else 0
    T_CNR_3PT_FGA_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_FGA_Q5, 0L),
                                      0L),
    T_CNR_3PT_FGA_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_FGA_Q6, 0L),
                                      0L),
    T_CNR_3PT_FGM_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_FGM_Q5, 0L),
                                      0L),
    T_CNR_3PT_FGM_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_FGM_Q6, 0L),
                                      0L),
    T_CNR_3PT_PTS_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_PTS_Q5, 0L),
                                      0L),
    T_CNR_3PT_PTS_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_CNR_3PT_PTS_Q6, 0L),
                                      0L),
    
    T_CNR_3PT_FGA_CGS = dplyr::coalesce(T_CNR_3PT_FGA_CGS, 0L),
    T_CNR_3PT_FGM_CGS = dplyr::coalesce(T_CNR_3PT_FGM_CGS, 0L),
    T_CNR_3PT_PTS_CGS = dplyr::coalesce(T_CNR_3PT_PTS_CGS, 0L)
  )

# ---- Derived metrics ---------------------------------------------------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG%
    T_CNR_3PT_PCT_Q1  = safe_div(T_CNR_3PT_FGM_Q1 , T_CNR_3PT_FGA_Q1),
    T_CNR_3PT_PCT_Q2  = safe_div(T_CNR_3PT_FGM_Q2 , T_CNR_3PT_FGA_Q2),
    T_CNR_3PT_PCT_Q3  = safe_div(T_CNR_3PT_FGM_Q3 , T_CNR_3PT_FGA_Q3),
    T_CNR_3PT_PCT_Q4  = safe_div(T_CNR_3PT_FGM_Q4 , T_CNR_3PT_FGA_Q4),
    T_CNR_3PT_PCT_Q5  = dplyr::if_else(OT_1 == 1L,
                                       safe_div(T_CNR_3PT_FGM_Q5, T_CNR_3PT_FGA_Q5),
                                       NA_real_),
    T_CNR_3PT_PCT_Q6  = dplyr::if_else(OT_2 == 1L,
                                       safe_div(T_CNR_3PT_FGM_Q6, T_CNR_3PT_FGA_Q6),
                                       NA_real_),
    T_CNR_3PT_PCT_CGS = safe_div(T_CNR_3PT_FGM_CGS, T_CNR_3PT_FGA_CGS),
    
    # eFG% (3PT => FGM * 1.5)
    T_CNR_3PT_EFG_PCT_Q1  = safe_div(T_CNR_3PT_FGM_Q1  * 1.5, T_CNR_3PT_FGA_Q1),
    T_CNR_3PT_EFG_PCT_Q2  = safe_div(T_CNR_3PT_FGM_Q2  * 1.5, T_CNR_3PT_FGA_Q2),
    T_CNR_3PT_EFG_PCT_Q3  = safe_div(T_CNR_3PT_FGM_Q3  * 1.5, T_CNR_3PT_FGA_Q3),
    T_CNR_3PT_EFG_PCT_Q4  = safe_div(T_CNR_3PT_FGM_Q4  * 1.5, T_CNR_3PT_FGA_Q4),
    T_CNR_3PT_EFG_PCT_Q5  = dplyr::if_else(OT_1 == 1L,
                                           safe_div(T_CNR_3PT_FGM_Q5 * 1.5, T_CNR_3PT_FGA_Q5),
                                           NA_real_),
    T_CNR_3PT_EFG_PCT_Q6  = dplyr::if_else(OT_2 == 1L,
                                           safe_div(T_CNR_3PT_FGM_Q6 * 1.5, T_CNR_3PT_FGA_Q6),
                                           NA_real_),
    T_CNR_3PT_EFG_PCT_CGS = safe_div(T_CNR_3PT_FGM_CGS * 1.5, T_CNR_3PT_FGA_CGS),
    
    # PPP (points / attempts)
    T_CNR_3PT_PPP_Q1  = safe_div(T_CNR_3PT_PTS_Q1 , T_CNR_3PT_FGA_Q1),
    T_CNR_3PT_PPP_Q2  = safe_div(T_CNR_3PT_PTS_Q2 , T_CNR_3PT_FGA_Q2),
    T_CNR_3PT_PPP_Q3  = safe_div(T_CNR_3PT_PTS_Q3 , T_CNR_3PT_FGA_Q3),
    T_CNR_3PT_PPP_Q4  = safe_div(T_CNR_3PT_PTS_Q4 , T_CNR_3PT_FGA_Q4),
    T_CNR_3PT_PPP_Q5  = dplyr::if_else(OT_1 == 1L,
                                       safe_div(T_CNR_3PT_PTS_Q5, T_CNR_3PT_FGA_Q5),
                                       NA_real_),
    T_CNR_3PT_PPP_Q6  = dplyr::if_else(OT_2 == 1L,
                                       safe_div(T_CNR_3PT_PTS_Q6, T_CNR_3PT_FGA_Q6),
                                       NA_real_),
    T_CNR_3PT_PPP_CGS = safe_div(T_CNR_3PT_PTS_CGS, T_CNR_3PT_FGA_CGS),
    
    # Corner-3 points columns already exist (kept as-is)
    T_CNR_3PT_PTS_Q1  = T_CNR_3PT_PTS_Q1,
    T_CNR_3PT_PTS_Q2  = T_CNR_3PT_PTS_Q2,
    T_CNR_3PT_PTS_Q3  = T_CNR_3PT_PTS_Q3,
    T_CNR_3PT_PTS_Q4  = T_CNR_3PT_PTS_Q4,
    T_CNR_3PT_PTS_Q5  = T_CNR_3PT_PTS_Q5,
    T_CNR_3PT_PTS_Q6  = T_CNR_3PT_PTS_Q6,
    T_CNR_3PT_PTS_CGS = T_CNR_3PT_PTS_CGS,
    
    # Points share vs team total *scored* points
    T_CNR_3PT_PTSHR_Q1  = safe_div(T_CNR_3PT_PTS_Q1 , T_PTS_SCORED_Q1),
    T_CNR_3PT_PTSHR_Q2  = safe_div(T_CNR_3PT_PTS_Q2 , T_PTS_SCORED_Q2),
    T_CNR_3PT_PTSHR_Q3  = safe_div(T_CNR_3PT_PTS_Q3 , T_PTS_SCORED_Q3),
    T_CNR_3PT_PTSHR_Q4  = safe_div(T_CNR_3PT_PTS_Q4 , T_PTS_SCORED_Q4),
    T_CNR_3PT_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L,
                                         safe_div(T_CNR_3PT_PTS_Q5, T_PTS_SCORED_Q5),
                                         NA_real_),
    T_CNR_3PT_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L,
                                         safe_div(T_CNR_3PT_PTS_Q6, T_PTS_SCORED_Q6),
                                         NA_real_),
    T_CNR_3PT_PTSHR_CGS = safe_div(T_CNR_3PT_PTS_CGS, T_PTS_SCORED_CGS),
    
    # Rate vs total FGA
    T_CNR_3PT_RATE_Q1  = safe_div(T_CNR_3PT_FGA_Q1 , T_FGA_Q1),
    T_CNR_3PT_RATE_Q2  = safe_div(T_CNR_3PT_FGA_Q2 , T_FGA_Q2),
    T_CNR_3PT_RATE_Q3  = safe_div(T_CNR_3PT_FGA_Q3 , T_FGA_Q3),
    T_CNR_3PT_RATE_Q4  = safe_div(T_CNR_3PT_FGA_Q4 , T_FGA_Q4),
    T_CNR_3PT_RATE_Q5  = dplyr::if_else(OT_1 == 1L,
                                        safe_div(T_CNR_3PT_FGA_Q5, T_FGA_Q5),
                                        NA_real_),
    T_CNR_3PT_RATE_Q6  = dplyr::if_else(OT_2 == 1L,
                                        safe_div(T_CNR_3PT_FGA_Q6, T_FGA_Q6),
                                        NA_real_),
    T_CNR_3PT_RATE_CGS = safe_div(T_CNR_3PT_FGA_CGS, T_FGA_CGS)
  )

rm(cnr3_base, cnr3_cgs, cnr3_qtr)
message("[✓] Corner 3PT — Q1–Q6 (OT-aware) + CGS section complete.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Corner 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Above the Break 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Above the Break 3PT — Q1–Q4 + CGS                                           #
# Source: pm_df                                                                         #
# Creates:                                                                              #
#   FGA/FGM/PTS  -> T_ATB_3PT_FGA_Q1..Q4, T_ATB_3PT_FGA_CGS;                           #
#                   T_ATB_3PT_FGM_Q1..Q4, T_ATB_3PT_FGM_CGS;                           #
#                   T_ATB_3PT_PTS_Q1..Q4, T_ATB_3PT_PTS_CGS                             #
#   Derived      -> FG%, EFG%, PPP, PTSHR, RATE (share of team FGA)                    #
# Detection: SHOT_ZONE_BASIC == "Above the Break 3"; fallback text contains "Above".   #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "shooting_play","scoring_play","score_value") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(pm_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Above-the-Break base ----------------------------------------------------- #
atb3_base <-
  pm_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_atb3 = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("above the break 3")
    } else {
      stringr::str_detect(type_text, stringr::regex("above\\s*the\\s*break", ignore_case = TRUE))
    }
  ) %>%
  dplyr::filter(is_atb3, shot) %>%
  dplyr::transmute(
    game_id, team_id, qtr,
    atb3_fga = TRUE,
    atb3_fgm = make & !is.na(pts) & pts > 0L,
    atb3_pts = dplyr::if_else(atb3_fgm, pts, 0L)
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
atb3_qtr <-
  atb3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    T_ATB_3PT_FGA = sum(atb3_fga, na.rm = TRUE),
    T_ATB_3PT_FGM = sum(atb3_fgm, na.rm = TRUE),
    T_ATB_3PT_PTS = sum(atb3_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(T_ATB_3PT_FGA, T_ATB_3PT_FGM, T_ATB_3PT_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
atb3_cgs <-
  atb3_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_ATB_3PT_FGA_CGS = sum(atb3_fga, na.rm = TRUE),
    T_ATB_3PT_FGM_CGS = sum(atb3_fgm, na.rm = TRUE),
    T_ATB_3PT_PTS_CGS = sum(atb3_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into BaseStats_Team_MC ---------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(atb3_qtr, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(atb3_cgs, by = c("ESPN_GAME_ID" = "game_id",
                                    "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::mutate(
    # Q1–Q4 always present
    T_ATB_3PT_FGA_Q1 = dplyr::coalesce(T_ATB_3PT_FGA_Q1, 0L),
    T_ATB_3PT_FGA_Q2 = dplyr::coalesce(T_ATB_3PT_FGA_Q2, 0L),
    T_ATB_3PT_FGA_Q3 = dplyr::coalesce(T_ATB_3PT_FGA_Q3, 0L),
    T_ATB_3PT_FGA_Q4 = dplyr::coalesce(T_ATB_3PT_FGA_Q4, 0L),
    T_ATB_3PT_FGM_Q1 = dplyr::coalesce(T_ATB_3PT_FGM_Q1, 0L),
    T_ATB_3PT_FGM_Q2 = dplyr::coalesce(T_ATB_3PT_FGM_Q2, 0L),
    T_ATB_3PT_FGM_Q3 = dplyr::coalesce(T_ATB_3PT_FGM_Q3, 0L),
    T_ATB_3PT_FGM_Q4 = dplyr::coalesce(T_ATB_3PT_FGM_Q4, 0L),
    T_ATB_3PT_PTS_Q1 = dplyr::coalesce(T_ATB_3PT_PTS_Q1, 0L),
    T_ATB_3PT_PTS_Q2 = dplyr::coalesce(T_ATB_3PT_PTS_Q2, 0L),
    T_ATB_3PT_PTS_Q3 = dplyr::coalesce(T_ATB_3PT_PTS_Q3, 0L),
    T_ATB_3PT_PTS_Q4 = dplyr::coalesce(T_ATB_3PT_PTS_Q4, 0L),
    
    # OT periods: only if OT happened
    T_ATB_3PT_FGA_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_FGA_Q5, 0L),
                                      0L),
    T_ATB_3PT_FGA_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_FGA_Q6, 0L),
                                      0L),
    T_ATB_3PT_FGM_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_FGM_Q5, 0L),
                                      0L),
    T_ATB_3PT_FGM_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_FGM_Q6, 0L),
                                      0L),
    T_ATB_3PT_PTS_Q5 = dplyr::if_else(OT_1 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_PTS_Q5, 0L),
                                      0L),
    T_ATB_3PT_PTS_Q6 = dplyr::if_else(OT_2 == 1L,
                                      dplyr::coalesce(T_ATB_3PT_PTS_Q6, 0L),
                                      0L),
    
    T_ATB_3PT_FGA_CGS = dplyr::coalesce(T_ATB_3PT_FGA_CGS, 0L),
    T_ATB_3PT_FGM_CGS = dplyr::coalesce(T_ATB_3PT_FGM_CGS, 0L),
    T_ATB_3PT_PTS_CGS = dplyr::coalesce(T_ATB_3PT_PTS_CGS, 0L)
  )

# ---- Derived metrics ---------------------------------------------------------------- #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # FG%
    T_ATB_3PT_PCT_Q1  = safe_div(T_ATB_3PT_FGM_Q1 , T_ATB_3PT_FGA_Q1),
    T_ATB_3PT_PCT_Q2  = safe_div(T_ATB_3PT_FGM_Q2 , T_ATB_3PT_FGA_Q2),
    T_ATB_3PT_PCT_Q3  = safe_div(T_ATB_3PT_FGM_Q3 , T_ATB_3PT_FGA_Q3),
    T_ATB_3PT_PCT_Q4  = safe_div(T_ATB_3PT_FGM_Q4 , T_ATB_3PT_FGA_Q4),
    T_ATB_3PT_PCT_Q5  = dplyr::if_else(OT_1 == 1L,
                                       safe_div(T_ATB_3PT_FGM_Q5, T_ATB_3PT_FGA_Q5),
                                       NA_real_),
    T_ATB_3PT_PCT_Q6  = dplyr::if_else(OT_2 == 1L,
                                       safe_div(T_ATB_3PT_FGM_Q6, T_ATB_3PT_FGA_Q6),
                                       NA_real_),
    T_ATB_3PT_PCT_CGS = safe_div(T_ATB_3PT_FGM_CGS, T_ATB_3PT_FGA_CGS),
    
    # eFG% (3PT: value 1.5 * FGM)
    T_ATB_3PT_EFG_PCT_Q1  = safe_div(T_ATB_3PT_FGM_Q1  * 1.5, T_ATB_3PT_FGA_Q1),
    T_ATB_3PT_EFG_PCT_Q2  = safe_div(T_ATB_3PT_FGM_Q2  * 1.5, T_ATB_3PT_FGA_Q2),
    T_ATB_3PT_EFG_PCT_Q3  = safe_div(T_ATB_3PT_FGM_Q3  * 1.5, T_ATB_3PT_FGA_Q3),
    T_ATB_3PT_EFG_PCT_Q4  = safe_div(T_ATB_3PT_FGM_Q4  * 1.5, T_ATB_3PT_FGA_Q4),
    T_ATB_3PT_EFG_PCT_Q5  = dplyr::if_else(OT_1 == 1L,
                                           safe_div(T_ATB_3PT_FGM_Q5 * 1.5, T_ATB_3PT_FGA_Q5),
                                           NA_real_),
    T_ATB_3PT_EFG_PCT_Q6  = dplyr::if_else(OT_2 == 1L,
                                           safe_div(T_ATB_3PT_FGM_Q6 * 1.5, T_ATB_3PT_FGA_Q6),
                                           NA_real_),
    T_ATB_3PT_EFG_PCT_CGS = safe_div(T_ATB_3PT_FGM_CGS * 1.5, T_ATB_3PT_FGA_CGS),
    
    # PPP (points / attempts)
    T_ATB_3PT_PPP_Q1  = safe_div(T_ATB_3PT_PTS_Q1 , T_ATB_3PT_FGA_Q1),
    T_ATB_3PT_PPP_Q2  = safe_div(T_ATB_3PT_PTS_Q2 , T_ATB_3PT_FGA_Q2),
    T_ATB_3PT_PPP_Q3  = safe_div(T_ATB_3PT_PTS_Q3 , T_ATB_3PT_FGA_Q3),
    T_ATB_3PT_PPP_Q4  = safe_div(T_ATB_3PT_PTS_Q4 , T_ATB_3PT_FGA_Q4),
    T_ATB_3PT_PPP_Q5  = dplyr::if_else(OT_1 == 1L,
                                       safe_div(T_ATB_3PT_PTS_Q5, T_ATB_3PT_FGA_Q5),
                                       NA_real_),
    T_ATB_3PT_PPP_Q6  = dplyr::if_else(OT_2 == 1L,
                                       safe_div(T_ATB_3PT_PTS_Q6, T_ATB_3PT_FGA_Q6),
                                       NA_real_),
    T_ATB_3PT_PPP_CGS = safe_div(T_ATB_3PT_PTS_CGS, T_ATB_3PT_FGA_CGS),
    
    # Points share vs team total *scored* points
    T_ATB_3PT_PTSHR_Q1  = safe_div(T_ATB_3PT_PTS_Q1 , T_PTS_SCORED_Q1),
    T_ATB_3PT_PTSHR_Q2  = safe_div(T_ATB_3PT_PTS_Q2 , T_PTS_SCORED_Q2),
    T_ATB_3PT_PTSHR_Q3  = safe_div(T_ATB_3PT_PTS_Q3 , T_PTS_SCORED_Q3),
    T_ATB_3PT_PTSHR_Q4  = safe_div(T_ATB_3PT_PTS_Q4 , T_PTS_SCORED_Q4),
    T_ATB_3PT_PTSHR_Q5  = dplyr::if_else(OT_1 == 1L,
                                         safe_div(T_ATB_3PT_PTS_Q5, T_PTS_SCORED_Q5),
                                         NA_real_),
    T_ATB_3PT_PTSHR_Q6  = dplyr::if_else(OT_2 == 1L,
                                         safe_div(T_ATB_3PT_PTS_Q6, T_PTS_SCORED_Q6),
                                         NA_real_),
    T_ATB_3PT_PTSHR_CGS = safe_div(T_ATB_3PT_PTS_CGS, T_PTS_SCORED_CGS),
    
    # Rate vs total FGA
    T_ATB_3PT_RATE_Q1  = safe_div(T_ATB_3PT_FGA_Q1 , T_FGA_Q1),
    T_ATB_3PT_RATE_Q2  = safe_div(T_ATB_3PT_FGA_Q2 , T_FGA_Q2),
    T_ATB_3PT_RATE_Q3  = safe_div(T_ATB_3PT_FGA_Q3 , T_FGA_Q3),
    T_ATB_3PT_RATE_Q4  = safe_div(T_ATB_3PT_FGA_Q4 , T_FGA_Q4),
    T_ATB_3PT_RATE_Q5  = dplyr::if_else(OT_1 == 1L,
                                        safe_div(T_ATB_3PT_FGA_Q5, T_FGA_Q5),
                                        NA_real_),
    T_ATB_3PT_RATE_Q6  = dplyr::if_else(OT_2 == 1L,
                                        safe_div(T_ATB_3PT_FGA_Q6, T_FGA_Q6),
                                        NA_real_),
    T_ATB_3PT_RATE_CGS = safe_div(T_ATB_3PT_FGA_CGS, T_FGA_CGS)
  )

rm(atb3_base, atb3_cgs, atb3_qtr)
message("[✓] Above the Break 3PT — Q1–Q6 (OT-aware) + CGS section complete.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Above the Break 3PT Shooting Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Clutch Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Clutch Windows (raw counts only)                                            #
#   Windows: L1M (≤60s), L3M (≤120s), L5M (≤300s), L10M (≤600s)                        #
# Source: pm_df                                                                         #
# Creates (for each window WIN in {L1M,L3M,L5M,L10M}):                                  #
#   T_CLTH_WIN_FGA_Q1..Q4,  T_CLTH_WIN_FGA_CGS                                          #
#   T_CLTH_WIN_FGM_Q1..Q4,  T_CLTH_WIN_FGM_CGS                                          #
# Notes: end-of-quarter time remaining is derived from clock_minutes/clock_seconds.     #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr",
                "shooting_play","scoring_play",
                "clock_minutes","clock_seconds",
                "score_diff") %in% names(pm_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

# Build a base with 't_sec' = seconds remaining in the quarter, plus shot/make flags
clutch_base <-
  pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes))*60L +
      suppressWarnings(as.integer(clock_seconds)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play)
  ) %>%
  # keep regulation + OT quarters (1..6), only if game is within 7 points
  dplyr::filter(qtr %in% 1:6, !is.na(score_diff), score_diff <= 7)

# Helper: compute per-quarter and CGS tallies for a time threshold
clutch_tallies <- function(base_df, threshold_sec, win_tag) {
  # per-quarter
  qtr_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, qtr) %>%
    dplyr::summarise(
      FGA = sum(shot, na.rm = TRUE),
      FGM = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = c(FGA, FGM),
      values_fill = 0L,
      names_sep   = "_Q"
    ) %>%
    # rename ONLY the measure columns; keep game_id/team_id unchanged
    dplyr::rename_with(
      ~ paste0("T_CLTH_", win_tag, "_", .x),
      .cols = dplyr::matches("^(FGA|FGM)_Q[1-6]$")
    )
  
  # complete game
  cgs_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::summarise(
      FGA_CGS = sum(shot, na.rm = TRUE),
      FGM_CGS = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(
      ~ paste0("T_CLTH_", win_tag, "_", .x),
      .cols = c(FGA_CGS, FGM_CGS)
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Compute for each window
res_L1M  <- clutch_tallies(clutch_base,  60L, "L1M")
res_L3M  <- clutch_tallies(clutch_base, 120L, "L3M")
res_L5M  <- clutch_tallies(clutch_base, 300L, "L5M")
res_L10M <- clutch_tallies(clutch_base, 600L, "L10M")

# Join into BaseStats_Team_MC
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # L1M
  dplyr::left_join(res_L1M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_L1M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # L3M
  dplyr::left_join(res_L3M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_L3M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # L5M
  dplyr::left_join(res_L5M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_L5M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # L10M
  dplyr::left_join(res_L10M$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_L10M$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # coalesce newly added numeric columns to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_CLTH_(L1M|L3M|L5M|L10M)_(FGA|FGM)"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# helper (use the same safe_div you already have; if not, uncomment the next line)
# safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

clutch_pct <- function(df, tag) {
  fgm <- function(p) paste0("T_CLTH_", tag, "_FGM_", p)
  fga <- function(p) paste0("T_CLTH_", tag, "_FGA_", p)
  pct <- function(p) paste0("T_CLTH_", tag, "_PCT_", p)
  
  df %>%
    dplyr::mutate(
      !!pct("Q1")  := safe_div(.data[[fgm("Q1")]],  .data[[fga("Q1")]]),
      !!pct("Q2")  := safe_div(.data[[fgm("Q2")]],  .data[[fga("Q2")]]),
      !!pct("Q3")  := safe_div(.data[[fgm("Q3")]],  .data[[fga("Q3")]]),
      !!pct("Q4")  := safe_div(.data[[fgm("Q4")]],  .data[[fga("Q4")]]),
      # OT periods: only meaningful if that OT happened
      !!pct("Q5")  := dplyr::if_else(
        .data[["OT_1"]] == 1L,
        safe_div(.data[[fgm("Q5")]], .data[[fga("Q5")]]),
        NA_real_
      ),
      !!pct("Q6")  := dplyr::if_else(
        .data[["OT_2"]] == 1L,
        safe_div(.data[[fgm("Q6")]], .data[[fga("Q6")]]),
        NA_real_
      ),
      !!pct("CGS") := safe_div(.data[[fgm("CGS")]], .data[[fga("CGS")]])
    )
}

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  clutch_pct("L1M")  %>%
  clutch_pct("L3M")  %>%
  clutch_pct("L5M")  %>%
  clutch_pct("L10M")

rm(res_L10M, res_L1M, res_L3M, res_L5M, clutch_base)
message("[✓] Clutch windows (L1M/L3M/L5M/L10M) — Q1–Q6 (OT-aware) FGA/FGM + PCT joined.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Clutch Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Clutch 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Clutch 2PT Windows (raw counts + FG%)                                       #
#   Windows: L1M (≤60s), L3M (≤120s), L5M (≤300s), L10M (≤600s)                        #
# Source: pm_df                                                                        #
# Creates (for each window WIN in {L1M,L3M,L5M,L10M}):                                 #
#   T_CLTH_2PT_WIN_FGA_Q1..Q4,  T_CLTH_2PT_WIN_FGA_CGS                                 #
#   T_CLTH_2PT_WIN_FGM_Q1..Q4,  T_CLTH_2PT_WIN_FGM_CGS                                 #
#   T_CLTH_2PT_WIN_PCT_Q1..Q4,  T_CLTH_2PT_WIN_PCT_CGS                                 #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr",
                "shooting_play","scoring_play",
                "clock_minutes","clock_seconds",
                "SHOT_ZONE_BASIC") %in% names(pm_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build filtered base: only 2PT clutch shots (exclude 3PT zones) ---------------- #
clutch_2pt_base <-
  pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes))*60L +
      suppressWarnings(as.integer(clock_seconds)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play)
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,                  # <-- include OT periods
    score_diff <= 7,
    shot,
    # exclude all 3PT zones
    !stringr::str_detect(SHOT_ZONE_BASIC, "(Above the Break 3|Right Corner 3|Left Corner 3)")
  )

# ---- Helper: compute per-quarter and CGS tallies for each time threshold ------------ #
clutch_2pt_tallies <- function(base_df, threshold_sec, win_tag) {
  
  # per-quarter breakdown
  qtr_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, qtr) %>%
    dplyr::summarise(
      FGA = sum(shot, na.rm = TRUE),
      FGM = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(game_id, team_id) %>%
    tidyr::complete(qtr = 1:6, fill = list(FGA = 0L, FGM = 0L)) %>%  # <-- ensure Q1–Q6
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = c(FGA, FGM),
      values_fill = 0L,
      names_sep   = "_Q"
    ) %>%
    dplyr::rename_with(
      ~ paste0("T_CLTH_2PT_", win_tag, "_", .x),
      .cols = dplyr::matches("^(FGA|FGM)_Q[1-6]$")  # <-- rename Q1–Q6
    )
  
  # full game (CGS)
  cgs_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::summarise(
      FGA_CGS = sum(shot, na.rm = TRUE),
      FGM_CGS = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(
      ~ paste0("T_CLTH_2PT_", win_tag, "_", .x),
      .cols = c(FGA_CGS, FGM_CGS)
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Compute for each window (L1M/L3M/L5M/L10M) ------------------------------------ #
res_2PT_L1M  <- clutch_2pt_tallies(clutch_2pt_base,  60L, "L1M")
res_2PT_L3M  <- clutch_2pt_tallies(clutch_2pt_base, 120L, "L3M")
res_2PT_L5M  <- clutch_2pt_tallies(clutch_2pt_base, 300L, "L5M")
res_2PT_L10M <- clutch_2pt_tallies(clutch_2pt_base, 600L, "L10M")

# ---- Join into BaseStats_Team_MC ---------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # L1M
  dplyr::left_join(res_2PT_L1M$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_2PT_L1M$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # L3M
  dplyr::left_join(res_2PT_L3M$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_2PT_L3M$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # L5M
  dplyr::left_join(res_2PT_L5M$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_2PT_L5M$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # L10M
  dplyr::left_join(res_2PT_L10M$qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_2PT_L10M$cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # coalesce new columns to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_CLTH_2PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q[1-6]$|^T_CLTH_2PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_CGS$"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- FG% calculation helper ------------------------------------------------------- #
clutch_2pt_pct <- function(df, tag) {
  fgm <- function(p) paste0("T_CLTH_2PT_", tag, "_FGM_", p)
  fga <- function(p) paste0("T_CLTH_2PT_", tag, "_FGA_", p)
  pct <- function(p) paste0("T_CLTH_2PT_", tag, "_PCT_", p)
  
  df %>%
    dplyr::mutate(
      !!pct("Q1")  := safe_div(.data[[fgm("Q1")]],  .data[[fga("Q1")]]),
      !!pct("Q2")  := safe_div(.data[[fgm("Q2")]],  .data[[fga("Q2")]]),
      !!pct("Q3")  := safe_div(.data[[fgm("Q3")]],  .data[[fga("Q3")]]),
      !!pct("Q4")  := safe_div(.data[[fgm("Q4")]],  .data[[fga("Q4")]]),
      !!pct("Q5")  := safe_div(.data[[fgm("Q5")]],  .data[[fga("Q5")]]),  # OT1
      !!pct("Q6")  := safe_div(.data[[fgm("Q6")]],  .data[[fga("Q6")]]),  # OT2
      !!pct("CGS") := safe_div(.data[[fgm("CGS")]], .data[[fga("CGS")]])
    )
}

# ---- Apply FG% for all clutch 2PT windows ------------------------------------------ #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  clutch_2pt_pct("L1M")  %>%
  clutch_2pt_pct("L3M")  %>%
  clutch_2pt_pct("L5M")  %>%
  clutch_2pt_pct("L10M")

rm(res_2PT_L10M, res_2PT_L1M, res_2PT_L3M, res_2PT_L5M, clutch_2pt_base)
message("[✓] Clutch 2PT windows (L1M/L3M/L5M/L10M, OT-aware) — FGA, FGM, and FG% joined.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Clutch 2PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Clutch 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Clutch 3PT Windows (raw counts + FG%)                                       #
#   Windows: L1M (≤60s), L3M (≤120s), L5M (≤300s), L10M (≤600s)                        #
# Source: pm_df                                                                        #
# Creates (for each window WIN in {L1M,L3M,L5M,L10M}):                                 #
#   T_CLTH_3PT_WIN_FGA_Q1..Q6,  T_CLTH_3PT_WIN_FGA_CGS                                 #
#   T_CLTH_3PT_WIN_FGM_Q1..Q6,  T_CLTH_3PT_WIN_FGM_CGS                                 #
#   T_CLTH_3PT_WIN_PCT_Q1..Q6,  T_CLTH_3PT_WIN_PCT_CGS                                 #
# OT-aware via OT_1 / OT_2 flags in BaseStats_Team_MC                                  #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr",
                "shooting_play","scoring_play",
                "clock_minutes","clock_seconds",
                "SHOT_ZONE_BASIC","score_diff") %in% names(pm_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

# ---- Build filtered base: only 3PT clutch shots (Q1–Q6, within 7 points) ----------- #
clutch_3pt_base <-
  pm_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes))*60L +
      suppressWarnings(as.integer(clock_seconds)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play)
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(score_diff),
    score_diff <= 7,
    shot,
    SHOT_ZONE_BASIC %in% c("Above the Break 3", "Right Corner 3", "Left Corner 3")
  )

# ---- Helper: compute per-quarter and CGS tallies for each time threshold ------------ #
clutch_3pt_tallies <- function(base_df, threshold_sec, win_tag) {
  
  # per-quarter breakdown
  qtr_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, qtr) %>%
    dplyr::summarise(
      FGA = sum(shot, na.rm = TRUE),
      FGM = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = c(FGA, FGM),
      values_fill = 0L,
      names_sep   = "_Q"
    ) %>%
    dplyr::rename_with(
      ~ paste0("T_CLTH_3PT_", win_tag, "_", .x),
      .cols = dplyr::matches("^(FGA|FGM)_Q[1-6]$")
    )
  
  # full game (CGS)
  cgs_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::summarise(
      FGA_CGS = sum(shot, na.rm = TRUE),
      FGM_CGS = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(
      ~ paste0("T_CLTH_3PT_", win_tag, "_", .x),
      .cols = c(FGA_CGS, FGM_CGS)
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Compute for each window (L1M/L3M/L5M/L10M) ------------------------------------ #
res_3PT_L1M  <- clutch_3pt_tallies(clutch_3pt_base,  60L, "L1M")
res_3PT_L3M  <- clutch_3pt_tallies(clutch_3pt_base, 120L, "L3M")
res_3PT_L5M  <- clutch_3pt_tallies(clutch_3pt_base, 300L, "L5M")
res_3PT_L10M <- clutch_3pt_tallies(clutch_3pt_base, 600L, "L10M")

# ---- Join into BaseStats_Team_MC ---------------------------------------------------- #
stopifnot(exists("BaseStats_Team_MC"))
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # L1M
  dplyr::left_join(res_3PT_L1M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_3PT_L1M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  # L3M
  dplyr::left_join(res_3PT_L3M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_3PT_L3M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  # L5M
  dplyr::left_join(res_3PT_L5M$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_3PT_L5M$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  # L10M
  dplyr::left_join(res_3PT_L10M$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_3PT_L10M$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                            "ESPN_TEAM_ID" = "team_id")) %>%
  # coalesce new columns to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_CLTH_3PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- FG% calculation helper (OT-aware) --------------------------------------------- #
# assumes safe_div(n, d) already defined in the script
clutch_3pt_pct <- function(df, tag) {
  fgm <- function(p) paste0("T_CLTH_3PT_", tag, "_FGM_", p)
  fga <- function(p) paste0("T_CLTH_3PT_", tag, "_FGA_", p)
  pct <- function(p) paste0("T_CLTH_3PT_", tag, "_PCT_", p)
  
  df %>%
    dplyr::mutate(
      !!pct("Q1")  := safe_div(.data[[fgm("Q1")]],  .data[[fga("Q1")]]),
      !!pct("Q2")  := safe_div(.data[[fgm("Q2")]],  .data[[fga("Q2")]]),
      !!pct("Q3")  := safe_div(.data[[fgm("Q3")]],  .data[[fga("Q3")]]),
      !!pct("Q4")  := safe_div(.data[[fgm("Q4")]],  .data[[fga("Q4")]]),
      !!pct("Q5")  := dplyr::if_else(
        .data[["OT_1"]] == 1L,
        safe_div(.data[[fgm("Q5")]], .data[[fga("Q5")]]),
        NA_real_
      ),
      !!pct("Q6")  := dplyr::if_else(
        .data[["OT_2"]] == 1L,
        safe_div(.data[[fgm("Q6")]], .data[[fga("Q6")]]),
        NA_real_
      ),
      !!pct("CGS") := safe_div(.data[[fgm("CGS")]], .data[[fga("CGS")]])
    )
}

# ---- Apply FG% for all clutch 3PT windows ------------------------------------------ #
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  clutch_3pt_pct("L1M")  %>%
  clutch_3pt_pct("L3M")  %>%
  clutch_3pt_pct("L5M")  %>%
  clutch_3pt_pct("L10M")

rm(res_3PT_L10M, res_3PT_L1M, res_3PT_L3M, res_3PT_L5M, clutch_3pt_base)
message("[✓] Clutch 3PT windows (L1M/L3M/L5M/L10M) — Q1–Q6 (OT-aware) FGA/FGM/PCT joined.")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Clutch 3PT Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Defender Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Section XX: Closest Defender Distance (CGS only)                                   #
# Uses: pm_df (already in memory), closest-defender tracked CSV                      #
# Adds cols to BaseStats_Team_MC                                                      #
#------------------------------------------------------------------------------------#
# New columns created (per ESPN_GAME_ID, ESPN_TEAM_ID):
#   T_DEF_DIST_VT_FGA_CGS, T_DEF_DIST_VT_FGM_CGS
#   T_DEF_DIST_T_FGA_CGS,  T_DEF_DIST_T_FGM_CGS
#   T_DEF_DIST_O_FGA_CGS,  T_DEF_DIST_O_FGM_CGS
#   T_DEF_DIST_WO_FGA_CGS, T_DEF_DIST_WO_FGM_CGS
#   T_DEF_DIST_T_FG_PCT_CGS   # (Very Tight + Tight) FG%
#   T_DEF_DIST_O_FG_PCT_CGS   # (Open + Wide Open) FG%
# Notes:
# - Joins on (ESPN_GAME_ID, ESPN_TEAM_ID) as character.
# - Percentages are NA when denominator == 0.
#====================================================================================#

# -- helper for safe percentage --
.safe_pct <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# -- 1) Locate & load defender-tracked file ----------------------------------------
def_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_closetdefendertracked_", season_token, ".csv")
) 

if (!file.exists(def_path)) {
  stop("Closest-defender file not found at: ", def_path)
}

def_raw <- read.csv(def_path, header = TRUE, stringsAsFactors = FALSE)

# -- 2) ID normalization & basic hygiene -------------------------------------------
# Expect: espn_game_id, espn_team_id, defender_range_designation, FGA, FGM
if (!"espn_game_id" %in% names(def_raw) && "game_id" %in% names(def_raw)) {
  def_raw$espn_game_id <- def_raw$game_id
}
if (!"espn_team_id" %in% names(def_raw) && "team_id" %in% names(def_raw)) {
  def_raw$espn_team_id <- def_raw$team_id
}

# Coerce IDs to character; make FGA/FGM numeric if needed
num_safely <- function(x) suppressWarnings(as.numeric(x))
def_raw <- def_raw %>%
  dplyr::mutate(
    espn_game_id = as.character(espn_game_id),
    espn_team_id = as.character(espn_team_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM)
  )

# Keep only the four known buckets
valid_designations <- c("Very Tight", "Tight", "Open", "Wide Open")
def_raw <- def_raw %>% dplyr::filter(defender_range_designation %in% valid_designations)

# -- 3) Determine the game set from pm_df (robust ESPN ID detection) ---------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID","nba_game_id","NBA_GAME_ID","game_id_alt")

.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  ids <- as.character(df[[col]])
  ids[!is.na(ids) & nzchar(ids)]
}

def_ids <- unique(as.character(def_raw$espn_game_id))

overlaps <- lapply(.cands, function(cn) {
  # optionally filter pm_df to shooting_play==TRUE (when column exists)
  pm_filtered <- if ("shooting_play" %in% names(pm_df)) {
    dplyr::filter(pm_df, isTRUE(shooting_play))
  } else pm_df
  
  ids <- .get_ids(pm_filtered, cn)   # <-- safe pull; avoids dplyr::pull errors
  inter <- length(intersect(ids, def_ids))
  list(col = cn, ids = unique(ids), inter = inter)
})

best_idx <- which.max(vapply(overlaps, `[[`, numeric(1), "inter"))
best     <- overlaps[[best_idx]]

message(sprintf("ClosestDef CGS: defender games=%s | pm_df id col=%s | intersection=%s",
                length(def_ids), best$col, best$inter))

pm_ids <- if (length(best$ids)) unique(best$ids) else character()

def_use <- if (length(pm_ids) && best$inter > 0) {
  dplyr::filter(def_raw, espn_game_id %in% pm_ids)
} else {
  message("ClosestDef CGS: No ID overlap—using all defender rows as fallback.")
  def_raw
}

# -- 4) Aggregate to CGS per (ESPN ids) --------------------------------------------
def_agg <- def_use %>%
  dplyr::group_by(espn_game_id, espn_team_id) %>%
  dplyr::summarise(
    # Very Tight (0–2 ft)
    T_DEF_DIST_VT_FGA_CGS = sum(ifelse(defender_range_designation == "Very Tight", FGA, 0), na.rm = TRUE),
    T_DEF_DIST_VT_FGM_CGS = sum(ifelse(defender_range_designation == "Very Tight", FGM, 0), na.rm = TRUE),
    
    # Tight (2–4 ft)
    T_DEF_DIST_T_FGA_CGS  = sum(ifelse(defender_range_designation == "Tight", FGA, 0), na.rm = TRUE),
    T_DEF_DIST_T_FGM_CGS  = sum(ifelse(defender_range_designation == "Tight", FGM, 0), na.rm = TRUE),
    
    # Open (4–6 ft)
    T_DEF_DIST_O_FGA_CGS  = sum(ifelse(defender_range_designation == "Open", FGA, 0), na.rm = TRUE),
    T_DEF_DIST_O_FGM_CGS  = sum(ifelse(defender_range_designation == "Open", FGM, 0), na.rm = TRUE),
    
    # Wide Open (6+ ft)
    T_DEF_DIST_WO_FGA_CGS = sum(ifelse(defender_range_designation == "Wide Open", FGA, 0), na.rm = TRUE),
    T_DEF_DIST_WO_FGM_CGS = sum(ifelse(defender_range_designation == "Wide Open", FGM, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    # Percentages (combined buckets)
    T_DEF_DIST_T_FG_PCT_CGS = .safe_pct(
      T_DEF_DIST_VT_FGM_CGS + T_DEF_DIST_T_FGM_CGS,
      T_DEF_DIST_VT_FGA_CGS + T_DEF_DIST_T_FGA_CGS
    ),
    T_DEF_DIST_O_FG_PCT_CGS = .safe_pct(
      T_DEF_DIST_O_FGM_CGS + T_DEF_DIST_WO_FGM_CGS,
      T_DEF_DIST_O_FGA_CGS + T_DEF_DIST_WO_FGA_CGS
    )
  ) %>%
  dplyr::rename(
    ESPN_GAME_ID = espn_game_id,
    ESPN_TEAM_ID = espn_team_id
  ) %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

# -- 5) Join back into BaseStats_Team_MC --------------------------------------------
.req_cols <- c("ESPN_GAME_ID", "ESPN_TEAM_ID")
.miss_bst <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Team_MC)))
if (length(.miss_bst)) {
  stop("BaseStats_Team_MC is missing required join keys: ", paste(.miss_bst, collapse = ", "))
}

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(def_agg, by = c("ESPN_GAME_ID", "ESPN_TEAM_ID"))

# Replace NA counts with 0 (percentages remain NA)
count_cols <- c(
  "T_DEF_DIST_VT_FGA_CGS","T_DEF_DIST_VT_FGM_CGS",
  "T_DEF_DIST_T_FGA_CGS","T_DEF_DIST_T_FGM_CGS",
  "T_DEF_DIST_O_FGA_CGS","T_DEF_DIST_O_FGM_CGS",
  "T_DEF_DIST_WO_FGA_CGS","T_DEF_DIST_WO_FGM_CGS"
)
for (cc in intersect(count_cols, names(BaseStats_Team_MC))) {
  BaseStats_Team_MC[[cc]] <- ifelse(is.na(BaseStats_Team_MC[[cc]]), 0, BaseStats_Team_MC[[cc]])
}

rm(best, def_agg, def_raw, def_use, overlaps)
message("Closest Defender Distance (CGS) columns added to BaseStats_Team_MC.")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Defender Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Shot Clock Tracking Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#====================================================================================#
# Section XX: Shot Clock Tracking (CGS only)                                         #
# Uses: pm_df (already in memory), shot-clock tracked CSV                            #
# Adds cols to BaseStats_Team_MC                                                      #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID):
#   T_SHOT_CLK_EE_FGA_CGS,  T_SHOT_CLK_EE_FGM_CGS,  T_SHOT_CLK_EE_FG_PCT_CGS
#   T_SHOT_CLK_VE_FGA_CGS,  T_SHOT_CLK_VE_FGM_CGS,  T_SHOT_CLK_VE_FG_PCT_CGS
#   T_SHOT_CLK_E_FGA_CGS,   T_SHOT_CLK_E_FGM_CGS,   T_SHOT_CLK_E_FG_PCT_CGS
#   T_SHOT_CLK_AVG_FGA_CGS, T_SHOT_CLK_AVG_FGM_CGS, T_SHOT_CLK_AVG_FG_PCT_CGS
#   T_SHOT_CLK_L_FGA_CGS,   T_SHOT_CLK_L_FGM_CGS,   T_SHOT_CLK_L_FG_PCT_CGS
#   T_SHOT_CLK_VL_FGA_CGS,  T_SHOT_CLK_VL_FGM_CGS,  T_SHOT_CLK_VL_FG_PCT_CGS
# Notes:
# - "Extremely Early" = rows with shot_clock_high == 24 (NOT designation).
# - We KEEP blank shot_clock_designation rows.
# - Join on (ESPN_GAME_ID, ESPN_TEAM_ID) as character. Percentages NA if denom==0.
#====================================================================================#

.safe_pct <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# -- 1) Load file -------------------------------------------------------------------
sc_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_shotclocktracked_", season_token, ".csv")
)
if (!file.exists(sc_path)) stop("Shot-clock file not found: ", sc_path)
sc_raw <- read.csv(sc_path, header = TRUE, stringsAsFactors = FALSE)

# -- 2) Normalize (NO filtering by designation; keep blanks) ------------------------
if (!"espn_game_id" %in% names(sc_raw) && "game_id" %in% names(sc_raw)) sc_raw$espn_game_id <- sc_raw$game_id
if (!"espn_team_id" %in% names(sc_raw) && "team_id" %in% names(sc_raw)) sc_raw$espn_team_id <- sc_raw$team_id

num_safely <- function(x) suppressWarnings(as.numeric(x))
sc_raw <- sc_raw |>
  dplyr::mutate(
    espn_game_id = as.character(espn_game_id),
    espn_team_id = as.character(espn_team_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM),
    shot_clock_high = num_safely(shot_clock_high),
    shot_clock_designation = trimws(as.character(shot_clock_designation))
  )
# IMPORTANT: no filter here; blanks are preserved so EE via high==24 is not lost.

# -- 3) Robust ESPN ID detection from pm_df ----------------------------------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID","nba_game_id","NBA_GAME_ID","game_id_alt")
.get_ids <- function(df, col) {
  if (!col %in% names(df)) return(character())
  ids <- as.character(df[[col]]); ids[!is.na(ids) & nzchar(ids)]
}
sc_ids_all <- unique(as.character(sc_raw$espn_game_id))
overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df)) dplyr::filter(pm_df, isTRUE(shooting_play)) else pm_df
  ids <- .get_ids(pmf, cn)
  list(col = cn, ids = unique(ids), inter = length(intersect(ids, sc_ids_all)))
})
best <- overlaps[[ which.max(vapply(overlaps, `[[`, numeric(1), "inter")) ]]
message(sprintf("ShotClock CGS: file games=%s | pm_df id col=%s | intersection=%s",
                length(sc_ids_all), best$col, best$inter))
pm_ids <- if (length(best$ids)) unique(best$ids) else character()
sc_use <- if (length(pm_ids) && best$inter > 0) dplyr::filter(sc_raw, espn_game_id %in% pm_ids) else {
  message("ShotClock CGS: No ID overlap—using all shot-clock rows as fallback."); sc_raw
}

# -- 4) Aggregate (CGS per game/team) ----------------------------------------------
sc_agg <- sc_use |>
  dplyr::group_by(espn_game_id, espn_team_id) |>
  dplyr::summarise(
    # Extremely Early: strictly by shot_clock_high == 24
    T_SHOT_CLK_EE_FGA_CGS  = sum(ifelse(shot_clock_high == 24, FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_EE_FGM_CGS  = sum(ifelse(shot_clock_high == 24, FGM, 0), na.rm = TRUE),
    
    # Others by designation (blanks won't count toward any)
    T_SHOT_CLK_VE_FGA_CGS  = sum(ifelse(shot_clock_designation == "Very Early", FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_VE_FGM_CGS  = sum(ifelse(shot_clock_designation == "Very Early", FGM, 0), na.rm = TRUE),
    
    T_SHOT_CLK_E_FGA_CGS   = sum(ifelse(shot_clock_designation == "Early", FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_E_FGM_CGS   = sum(ifelse(shot_clock_designation == "Early", FGM, 0), na.rm = TRUE),
    
    T_SHOT_CLK_AVG_FGA_CGS = sum(ifelse(shot_clock_designation == "Average", FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_AVG_FGM_CGS = sum(ifelse(shot_clock_designation == "Average", FGM, 0), na.rm = TRUE),
    
    T_SHOT_CLK_L_FGA_CGS   = sum(ifelse(shot_clock_designation == "Late", FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_L_FGM_CGS   = sum(ifelse(shot_clock_designation == "Late", FGM, 0), na.rm = TRUE),
    
    T_SHOT_CLK_VL_FGA_CGS  = sum(ifelse(shot_clock_designation == "Very Late", FGA, 0), na.rm = TRUE),
    T_SHOT_CLK_VL_FGM_CGS  = sum(ifelse(shot_clock_designation == "Very Late", FGM, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    T_SHOT_CLK_EE_FG_PCT_CGS  = .safe_pct(T_SHOT_CLK_EE_FGM_CGS,  T_SHOT_CLK_EE_FGA_CGS),
    T_SHOT_CLK_VE_FG_PCT_CGS  = .safe_pct(T_SHOT_CLK_VE_FGM_CGS,  T_SHOT_CLK_VE_FGA_CGS),
    T_SHOT_CLK_E_FG_PCT_CGS   = .safe_pct(T_SHOT_CLK_E_FGM_CGS,   T_SHOT_CLK_E_FGA_CGS),
    T_SHOT_CLK_AVG_FG_PCT_CGS = .safe_pct(T_SHOT_CLK_AVG_FGM_CGS, T_SHOT_CLK_AVG_FGA_CGS),
    T_SHOT_CLK_L_FG_PCT_CGS   = .safe_pct(T_SHOT_CLK_L_FGM_CGS,   T_SHOT_CLK_L_FGA_CGS),
    T_SHOT_CLK_VL_FG_PCT_CGS  = .safe_pct(T_SHOT_CLK_VL_FGM_CGS,  T_SHOT_CLK_VL_FGA_CGS)
  ) |>
  dplyr::rename(ESPN_GAME_ID = espn_game_id, ESPN_TEAM_ID = espn_team_id) |>
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  )

# -- 5) Join into BaseStats_Team_MC --------------------------------------------------
.req_cols <- c("ESPN_GAME_ID", "ESPN_TEAM_ID")
.miss_bst <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Team_MC)))
if (length(.miss_bst)) stop("BaseStats_Team_MC missing join keys: ", paste(.miss_bst, collapse=", "))

BaseStats_Team_MC <- BaseStats_Team_MC |>
  dplyr::mutate(ESPN_GAME_ID = as.character(ESPN_GAME_ID),
                ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)) |>
  dplyr::left_join(sc_agg, by = c("ESPN_GAME_ID","ESPN_TEAM_ID"))

# Fill NA counts with 0 (leave *_PCT_* as NA when denom==0)
count_cols <- c(
  "T_SHOT_CLK_EE_FGA_CGS","T_SHOT_CLK_EE_FGM_CGS",
  "T_SHOT_CLK_VE_FGA_CGS","T_SHOT_CLK_VE_FGM_CGS",
  "T_SHOT_CLK_E_FGA_CGS","T_SHOT_CLK_E_FGM_CGS",
  "T_SHOT_CLK_AVG_FGA_CGS","T_SHOT_CLK_AVG_FGM_CGS",
  "T_SHOT_CLK_L_FGA_CGS","T_SHOT_CLK_L_FGM_CGS",
  "T_SHOT_CLK_VL_FGA_CGS","T_SHOT_CLK_VL_FGM_CGS"
)
for (cc in intersect(count_cols, names(BaseStats_Team_MC))) {
  BaseStats_Team_MC[[cc]] <- ifelse(is.na(BaseStats_Team_MC[[cc]]), 0, BaseStats_Team_MC[[cc]])
}

rm(best, overlaps, sc_agg, sc_raw, sc_use)
message("Shot Clock Tracking (CGS) columns added to BaseStats_Team_MC (EE uses high==24; blanks kept).")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Shot Clock Tracking Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT & 3PT Defender Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#====================================================================================#
# Section XX: Closest Defender Distance — 2PT / 3PT Splits (CGS only)                #
# Uses: pm_df, closest-defender tracked CSV                                          #
# Adds cols to BaseStats_Team_MC                                                      #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID), for buckets: VT, T, O, WO
#   <bucket>_2PT_FGM/FGA_CGS and <bucket>_3PT_FGM/FGA_CGS, e.g.:
#   T_DEF_DIST_VT_2PT_FGM_CGS, T_DEF_DIST_VT_2PT_FGA_CGS, T_DEF_DIST_VT_3PT_FGM_CGS, ...
# Notes:
# - Joins on (ESPN_GAME_ID, ESPN_TEAM_ID) as character.
# - Falls back to compute FG2* from FGM/FG3* if FG2M/FG2A are missing.
#====================================================================================#

# -- 1) Load defender-distance file (re-use def_raw if it exists) -------------------
def_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_closetdefendertracked_", season_token, ".csv")
)
if (!exists("def_raw") || !is.data.frame(def_raw)) {
  if (!file.exists(def_path)) stop("Closest-defender file not found at: ", def_path)
  def_raw <- read.csv(def_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) Normalize columns & hygiene -------------------------------------------------
if (!"espn_game_id" %in% names(def_raw) && "game_id" %in% names(def_raw)) def_raw$espn_game_id <- def_raw$game_id
if (!"espn_team_id" %in% names(def_raw) && "team_id" %in% names(def_raw)) def_raw$espn_team_id <- def_raw$team_id

num_safely <- function(x) suppressWarnings(as.numeric(x))
def_raw <- def_raw |>
  dplyr::mutate(
    espn_game_id = as.character(espn_game_id),
    espn_team_id = as.character(espn_team_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM),
    FG2M = if ("FG2M" %in% names(def_raw)) num_safely(FG2M) else NA_real_,
    FG2A = if ("FG2A" %in% names(def_raw)) num_safely(FG2A) else NA_real_,
    FG3M = if ("FG3M" %in% names(def_raw)) num_safely(FG3M) else if ("FGM3" %in% names(def_raw)) num_safely(FGM3) else NA_real_,
    FG3A = if ("FG3A" %in% names(def_raw)) num_safely(FG3A) else if ("FGA3" %in% names(def_raw)) num_safely(FGA3) else NA_real_,
    defender_range_designation = as.character(defender_range_designation)
  )

# Fallback for FG2M/FG2A if missing
if (any(is.na(def_raw$FG2M))) def_raw$FG2M <- ifelse(is.na(def_raw$FG2M), pmax(def_raw$FGM - def_raw$FG3M, 0), def_raw$FG2M)
if (any(is.na(def_raw$FG2A))) def_raw$FG2A <- ifelse(is.na(def_raw$FG2A), pmax(def_raw$FGA - def_raw$FG3A, 0), def_raw$FG2A)

# Keep only valid distance buckets
valid_dd <- c("Very Tight","Tight","Open","Wide Open")
def_raw <- def_raw |> dplyr::filter(defender_range_designation %in% valid_dd)

# -- 3) Robust ESPN GAME ID detection from pm_df -----------------------------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID","nba_game_id","NBA_GAME_ID","game_id_alt")
.get_ids <- function(df, col) { if (!col %in% names(df)) return(character()); x <- as.character(df[[col]]); x[!is.na(x) & nzchar(x)] }
def_ids <- unique(as.character(def_raw$espn_game_id))
overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df)) dplyr::filter(pm_df, isTRUE(shooting_play)) else pm_df
  ids <- .get_ids(pmf, cn); list(col = cn, ids = unique(ids), inter = length(intersect(ids, def_ids)))
})
best <- overlaps[[ which.max(vapply(overlaps, `[[`, numeric(1), "inter")) ]]
message(sprintf("DefDist 2/3PT CGS: file games=%s | pm_df id col=%s | intersection=%s",
                length(def_ids), best$col, best$inter))
pm_ids <- if (length(best$ids)) unique(best$ids) else character()
def_use <- if (length(pm_ids) && best$inter > 0) dplyr::filter(def_raw, espn_game_id %in% pm_ids) else { message("No ID overlap—using all def rows."); def_raw }

# -- 4) Aggregate 2PT/3PT by bucket -------------------------------------------------
dd_agg <- def_use |>
  dplyr::group_by(espn_game_id, espn_team_id) |>
  dplyr::summarise(
    # Very Tight
    T_DEF_DIST_VT_2PT_FGM_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG2M, 0), na.rm=TRUE),
    T_DEF_DIST_VT_2PT_FGA_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG2A, 0), na.rm=TRUE),
    T_DEF_DIST_VT_3PT_FGM_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG3M, 0), na.rm=TRUE),
    T_DEF_DIST_VT_3PT_FGA_CGS = sum(ifelse(defender_range_designation=="Very Tight", FG3A, 0), na.rm=TRUE),
    
    # Tight
    T_DEF_DIST_T_2PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Tight", FG2M, 0), na.rm=TRUE),
    T_DEF_DIST_T_2PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Tight", FG2A, 0), na.rm=TRUE),
    T_DEF_DIST_T_3PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Tight", FG3M, 0), na.rm=TRUE),
    T_DEF_DIST_T_3PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Tight", FG3A, 0), na.rm=TRUE),
    
    # Open
    T_DEF_DIST_O_2PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Open", FG2M, 0), na.rm=TRUE),
    T_DEF_DIST_O_2PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Open", FG2A, 0), na.rm=TRUE),
    T_DEF_DIST_O_3PT_FGM_CGS  = sum(ifelse(defender_range_designation=="Open", FG3M, 0), na.rm=TRUE),
    T_DEF_DIST_O_3PT_FGA_CGS  = sum(ifelse(defender_range_designation=="Open", FG3A, 0), na.rm=TRUE),
    
    # Wide Open
    T_DEF_DIST_WO_2PT_FGM_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG2M, 0), na.rm=TRUE),
    T_DEF_DIST_WO_2PT_FGA_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG2A, 0), na.rm=TRUE),
    T_DEF_DIST_WO_3PT_FGM_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG3M, 0), na.rm=TRUE),
    T_DEF_DIST_WO_3PT_FGA_CGS = sum(ifelse(defender_range_designation=="Wide Open", FG3A, 0), na.rm=TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(ESPN_GAME_ID = espn_game_id, ESPN_TEAM_ID = espn_team_id) |>
  dplyr::mutate(ESPN_GAME_ID = as.character(ESPN_GAME_ID), ESPN_TEAM_ID = as.character(ESPN_TEAM_ID))

# -- 5) Join into BaseStats_Team_MC; fill NA counts with 0 --------------------------
.req_cols <- c("ESPN_GAME_ID","ESPN_TEAM_ID")
.miss_bst <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Team_MC)))
if (length(.miss_bst)) stop("BaseStats_Team_MC missing join keys: ", paste(.miss_bst, collapse=", "))
BaseStats_Team_MC <- BaseStats_Team_MC |>
  dplyr::mutate(ESPN_GAME_ID = as.character(ESPN_GAME_ID), ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)) |>
  dplyr::left_join(dd_agg, by=c("ESPN_GAME_ID","ESPN_TEAM_ID"))

dd_counts <- grep("^T_DEF_DIST_.*_(2PT|3PT)_FG[MA]_CGS$", names(BaseStats_Team_MC), value = TRUE)
for (cc in dd_counts) BaseStats_Team_MC[[cc]] <- ifelse(is.na(BaseStats_Team_MC[[cc]]), 0, BaseStats_Team_MC[[cc]])

rm(best, dd_agg, def_raw, def_raw, overlaps, def_use)
message("Defender Distance (2PT/3PT CGS) columns added to BaseStats_Team_MC.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT & 3PT Defender Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT & 3PT Shot Clock Tracking Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#====================================================================================#
# Section XX: Shot Clock — 2PT / 3PT Splits (CGS only)                               #
# Uses: pm_df, shot-clock tracked CSV                                                #
# Adds cols to BaseStats_Team_MC                                                      #
#------------------------------------------------------------------------------------#
# New columns (per ESPN_GAME_ID, ESPN_TEAM_ID), for buckets: EE, VE, E, AVG, L, VL
#   <bucket>_2PT_FGM/FGA_CGS and <bucket>_3PT_FGM/FGA_CGS, e.g.:
#   T_SHOT_CLK_EE_3PT_FGM_CGS, T_SHOT_CLK_EE_3PT_FGA_CGS, ...
# Notes:
# - EE is identified by shot_clock_high == 24 (NOT designation). Others use designation.
# - We KEEP blank shot_clock_designation rows (so EE rows aren’t lost).
# - Joins on (ESPN_GAME_ID, ESPN_TEAM_ID) as character.
#====================================================================================#

# -- 1) Load shot-clock file (re-use sc_raw if it exists) ---------------------------
sc_path <- file.path(paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/9. ShotTracking Data/nba_shotclocktracked_", season_token, ".csv")
)
if (!exists("sc_raw") || !is.data.frame(sc_raw)) {
  if (!file.exists(sc_path)) stop("Shot-clock file not found: ", sc_path)
  sc_raw <- read.csv(sc_path, header = TRUE, stringsAsFactors = FALSE)
}

# -- 2) Normalize (keep blanks) -----------------------------------------------------
if (!"espn_game_id" %in% names(sc_raw) && "game_id" %in% names(sc_raw)) sc_raw$espn_game_id <- sc_raw$game_id
if (!"espn_team_id" %in% names(sc_raw) && "team_id" %in% names(sc_raw)) sc_raw$espn_team_id <- sc_raw$team_id

num_safely <- function(x) suppressWarnings(as.numeric(x))
sc_raw <- sc_raw |>
  dplyr::mutate(
    espn_game_id = as.character(espn_game_id),
    espn_team_id = as.character(espn_team_id),
    FGA = num_safely(FGA),
    FGM = num_safely(FGM),
    FG2M = if ("FG2M" %in% names(sc_raw)) num_safely(FG2M) else NA_real_,
    FG2A = if ("FG2A" %in% names(sc_raw)) num_safely(FG2A) else NA_real_,
    FG3M = if ("FG3M" %in% names(sc_raw)) num_safely(FG3M) else if ("FGM3" %in% names(sc_raw)) num_safely(FGM3) else NA_real_,
    FG3A = if ("FG3A" %in% names(sc_raw)) num_safely(FG3A) else if ("FGA3" %in% names(sc_raw)) num_safely(FGA3) else NA_real_,
    shot_clock_high = num_safely(shot_clock_high),
    shot_clock_designation = trimws(as.character(shot_clock_designation))
  )

# Fallback for FG2 if missing
if (any(is.na(sc_raw$FG2M))) sc_raw$FG2M <- ifelse(is.na(sc_raw$FG2M), pmax(sc_raw$FGM - sc_raw$FG3M, 0), sc_raw$FG2M)
if (any(is.na(sc_raw$FG2A))) sc_raw$FG2A <- ifelse(is.na(sc_raw$FG2A), pmax(sc_raw$FGA - sc_raw$FG3A, 0), sc_raw$FG2A)

# -- 3) Robust ESPN GAME ID detection from pm_df -----------------------------------
.cands <- c("ESPN_GAME_ID","espn_game_id","game_id","GAME_ID","nba_game_id","NBA_GAME_ID","game_id_alt")
.get_ids <- function(df, col) { if (!col %in% names(df)) return(character()); x <- as.character(df[[col]]); x[!is.na(x) & nzchar(x)] }
sc_ids_all <- unique(as.character(sc_raw$espn_game_id))
overlaps <- lapply(.cands, function(cn) {
  pmf <- if ("shooting_play" %in% names(pm_df)) dplyr::filter(pm_df, isTRUE(shooting_play)) else pm_df
  ids <- .get_ids(pmf, cn); list(col = cn, ids = unique(ids), inter = length(intersect(ids, sc_ids_all)))
})
best <- overlaps[[ which.max(vapply(overlaps, `[[`, numeric(1), "inter")) ]]
message(sprintf("ShotClk 2/3PT CGS: file games=%s | pm_df id col=%s | intersection=%s",
                length(sc_ids_all), best$col, best$inter))
pm_ids <- if (length(best$ids)) unique(best$ids) else character()
sc_use <- if (length(pm_ids) && best$inter > 0) dplyr::filter(sc_raw, espn_game_id %in% pm_ids) else { message("No ID overlap—using all SC rows."); sc_raw }

# -- 4) Aggregate 2PT/3PT by shot-clock bucket -------------------------------------
sc_agg <- sc_use |>
  dplyr::group_by(espn_game_id, espn_team_id) |>
  dplyr::summarise(
    # Extremely Early: shot_clock_high == 24
    T_SHOT_CLK_EE_2PT_FGM_CGS = sum(ifelse(shot_clock_high==24, FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_EE_2PT_FGA_CGS = sum(ifelse(shot_clock_high==24, FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_EE_3PT_FGM_CGS = sum(ifelse(shot_clock_high==24, FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_EE_3PT_FGA_CGS = sum(ifelse(shot_clock_high==24, FG3A, 0), na.rm=TRUE),
    
    # Very Early (designation)
    T_SHOT_CLK_VE_2PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Very Early", FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_VE_2PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Very Early", FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_VE_3PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Very Early", FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_VE_3PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Very Early", FG3A, 0), na.rm=TRUE),
    
    # Early
    T_SHOT_CLK_E_2PT_FGM_CGS  = sum(ifelse(shot_clock_designation=="Early", FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_E_2PT_FGA_CGS  = sum(ifelse(shot_clock_designation=="Early", FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_E_3PT_FGM_CGS  = sum(ifelse(shot_clock_designation=="Early", FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_E_3PT_FGA_CGS  = sum(ifelse(shot_clock_designation=="Early", FG3A, 0), na.rm=TRUE),
    
    # Average
    T_SHOT_CLK_AVG_2PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Average", FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_AVG_2PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Average", FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_AVG_3PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Average", FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_AVG_3PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Average", FG3A, 0), na.rm=TRUE),
    
    # Late
    T_SHOT_CLK_L_2PT_FGM_CGS  = sum(ifelse(shot_clock_designation=="Late", FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_L_2PT_FGA_CGS  = sum(ifelse(shot_clock_designation=="Late", FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_L_3PT_FGM_CGS  = sum(ifelse(shot_clock_designation=="Late", FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_L_3PT_FGA_CGS  = sum(ifelse(shot_clock_designation=="Late", FG3A, 0), na.rm=TRUE),
    
    # Very Late
    T_SHOT_CLK_VL_2PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Very Late", FG2M, 0), na.rm=TRUE),
    T_SHOT_CLK_VL_2PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Very Late", FG2A, 0), na.rm=TRUE),
    T_SHOT_CLK_VL_3PT_FGM_CGS = sum(ifelse(shot_clock_designation=="Very Late", FG3M, 0), na.rm=TRUE),
    T_SHOT_CLK_VL_3PT_FGA_CGS = sum(ifelse(shot_clock_designation=="Very Late", FG3A, 0), na.rm=TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(ESPN_GAME_ID = espn_game_id, ESPN_TEAM_ID = espn_team_id) |>
  dplyr::mutate(ESPN_GAME_ID = as.character(ESPN_GAME_ID), ESPN_TEAM_ID = as.character(ESPN_TEAM_ID))

# -- 5) Join into BaseStats_Team_MC; fill NA counts with 0 --------------------------
.req_cols <- c("ESPN_GAME_ID","ESPN_TEAM_ID")
.miss_bst <- setdiff(.req_cols, intersect(.req_cols, names(BaseStats_Team_MC)))
if (length(.miss_bst)) stop("BaseStats_Team_MC missing join keys: ", paste(.miss_bst, collapse=", "))
BaseStats_Team_MC <- BaseStats_Team_MC |>
  dplyr::mutate(ESPN_GAME_ID = as.character(ESPN_GAME_ID), ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)) |>
  dplyr::left_join(sc_agg, by=c("ESPN_GAME_ID","ESPN_TEAM_ID"))

sc_counts <- grep("^T_SHOT_CLK_.*_(2PT|3PT)_FG[MA]_CGS$", names(BaseStats_Team_MC), value = TRUE)
for (cc in sc_counts) BaseStats_Team_MC[[cc]] <- ifelse(is.na(BaseStats_Team_MC[[cc]]), 0, BaseStats_Team_MC[[cc]])

rm(best, overlaps, sc_agg, sc_raw, sc_use)
message("Shot Clock (2PT/3PT CGS) columns added to BaseStats_Team_MC.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT & 3PT Shot Clock Tracking Distance Field Goal Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: TOP-LEVEL Scoring Data Aggregation Section ====





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
# === START: Turnovers Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Turnovers (Live Ball, Dead Ball, Bad Pass, All)                             #
# Source: pm_df                                                                        #
# Creates (per period Q1..Q4 + CGS):                                                   #
#   Live-ball:  T_TOV_LIVEB_Q1..Q4,   T_TOV_LIVEB_CGS                                  #
#   Dead-ball:  T_TOV_DEADB_Q1..Q4,   T_TOV_DEADB_CGS                                  #
#   Bad Pass:   T_TOV_BADP_Q1..Q4,    T_TOV_BADP_CGS                                   #
#   All:        T_TOV_Q1..Q4,         T_TOV_CGS                                        #
#   Percent:    T_TOV_PCT_Q1..Q4,     T_TOV_PCT_CGS                                    #
# Notes: Counts credited to the OPPONENT team (committing team → opposite team_id).    #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "home_team_id","away_team_id") %in% names(pm_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Common base & patterns (Q1–Q6, OT-aware later via OT_1 / OT_2) -----------------
tov_base_common <- pm_df %>%
  dplyr::mutate(
    qtr         = to_int(qtr),
    OPP_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    txt         = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(qtr %in% 1:6)

# Patterns (case-insensitive)
pat_liveb <- stringr::regex("lost ball turnover|bad pass turnover|offensive foul turnover",
                            ignore_case = TRUE)
pat_badp  <- stringr::regex("bad pass turnover", ignore_case = TRUE)
pat_any   <- stringr::regex("turnover", ignore_case = TRUE)

# ---- Separate populations (independent data frames) ---------------------------------
tov_liveb_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_liveb))

tov_all_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_any))

tov_deadb_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_any) &
                  !stringr::str_detect(txt, pat_liveb))

tov_badp_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_badp))

# ---- Helper: tally by quarter + CGS, credited to opponent ---------------------------
tally_tov <- function(df, name_prefix) {
  # Q1–Q6 (ensure quarters exist)
  qtr_df <- df %>%
    dplyr::group_by(game_id, OPP_TEAM_ID, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, OPP_TEAM_ID) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(team_id = OPP_TEAM_ID) %>%
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x),
                       dplyr::starts_with("Q"))
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build pieces
res_liveb <- tally_tov(tov_liveb_df, "TOV_LIVEB")
res_deadb <- tally_tov(tov_deadb_df, "TOV_DEADB")
res_badp  <- tally_tov(tov_badp_df,  "TOV_BADP")
res_all   <- tally_tov(tov_all_df,   "TOV")       # this feeds T_TOV_* directly

# ---- Join into BaseStats_Team_MC -----------------------------------------------------
stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # live-ball
  dplyr::left_join(res_liveb$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_liveb$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  # dead-ball
  dplyr::left_join(res_deadb$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_deadb$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  # bad pass
  dplyr::left_join(res_badp$qtr,  by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_badp$cgs,  by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  # all turnovers (final T_TOV_* metrics)
  dplyr::left_join(res_all$qtr,   by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_all$cgs,   by = c("ESPN_GAME_ID" = "game_id",
                                         "ESPN_TEAM_ID" = "team_id")) %>%
  # fill NAs from joins (Q1–Q6 + CGS for all TOV flavors)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(TOV(_(LIVEB|DEADB|BADP))?_(Q[1-6]|CGS))$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating: if no OT_1/OT_2, force Q5/Q6 TOV counts to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(TOV(_(LIVEB|DEADB|BADP))?_Q5)$"),
      ~ dplyr::if_else(OT_1 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^T_(TOV(_(LIVEB|DEADB|BADP))?_Q6)$"),
      ~ dplyr::if_else(OT_2 == 1L, ., 0L)
    )
  )

# ---- Turnover Percentage (needs T_FGA_* and T_FTA_* present) ------------------------
# T_TOV_PCT_X = T_TOV_X / (T_FGA_X + 0.44*T_FTA_X + T_TOV_X) * 100

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    # Regulation
    T_TOV_PCT_Q1  = 100 * safe_div(T_TOV_Q1,
                                   T_FGA_Q1 + 0.44 * T_FTA_Q1 + T_TOV_Q1),
    T_TOV_PCT_Q2  = 100 * safe_div(T_TOV_Q2,
                                   T_FGA_Q2 + 0.44 * T_FTA_Q2 + T_TOV_Q2),
    T_TOV_PCT_Q3  = 100 * safe_div(T_TOV_Q3,
                                   T_FGA_Q3 + 0.44 * T_FTA_Q3 + T_TOV_Q3),
    T_TOV_PCT_Q4  = 100 * safe_div(T_TOV_Q4,
                                   T_FGA_Q4 + 0.44 * T_FTA_Q4 + T_TOV_Q4),
    # OT: only if OT occurred, else NA
    T_TOV_PCT_Q5  = dplyr::if_else(
      OT_1 == 1L,
      100 * safe_div(T_TOV_Q5,
                     T_FGA_Q5 + 0.44 * T_FTA_Q5 + T_TOV_Q5),
      NA_real_
    ),
    T_TOV_PCT_Q6  = dplyr::if_else(
      OT_2 == 1L,
      100 * safe_div(T_TOV_Q6,
                     T_FGA_Q6 + 0.44 * T_FTA_Q6 + T_TOV_Q6),
      NA_real_
    ),
    # CGS (always across full game)
    T_TOV_PCT_CGS = 100 * safe_div(T_TOV_CGS,
                                   T_FGA_CGS + 0.44 * T_FTA_CGS + T_TOV_CGS)
  )

rm(res_all, res_badp, res_deadb, res_liveb,
   tov_all_df, tov_badp_df, tov_base_common, tov_deadb_df, tov_liveb_df)

message("[✓] Turnovers split (live, dead, bad pass, all) with OT-aware Q1–Q6 + TOV% computed.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Turnovers Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fouls Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Fouls (Total, Shooting, Technical, Flagrant, Offensive, Defensive, Charges) #
# Source: pm_df                                                                        #
# Creates per period (Q1..Q4) + complete game (CGS):                                   #
#   T_FOULS_*, T_SHOT_FOULS_*, T_TECH_FOULS_*, T_FLAG_FOULS_*                          #
#   T_OFF_FOULS_*, T_DEF_FOULS_*, T_CHRG_DRWN_*                                        #
# Notes: All counts are credited to the OPPONENT (team that benefits).                  #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text","home_team_id","away_team_id") %in% names(pm_df)))

to_int <- function(x) suppressWarnings(as.integer(x))

# ---- Base with opponent id & normalized text (Q1–Q6) --------------------------------
foul_base <- pm_df %>%
  dplyr::mutate(
    qtr         = to_int(qtr),
    # credit the opposite team of the one committing the foul
    OPP_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    txt         = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(qtr %in% 1:6)

# ---- Patterns (case-insensitive; operate on `txt`) ----------------------------------
pat_any_foul   <- stringr::regex("foul", ignore_case = TRUE)

pat_shoot_foul <- stringr::regex("shooting foul", ignore_case = TRUE)
pat_tech_foul  <- stringr::regex("technical foul", ignore_case = TRUE)
pat_flag_foul  <- stringr::regex("flagrant foul type 1|flagrant foul type 2", ignore_case = TRUE)

# Offensive: as specified in the sheet
pat_off_foul   <- stringr::regex(
  "offensive foul|offensive charge|offensive foul turnover|loose ball foul",
  ignore_case = TRUE
)

# Defensive: as specified in the sheet
pat_def_foul   <- stringr::regex(
  paste(
    "personal foul",
    "personal take foul",
    "away from play foul",
    "transition take foul",
    "flagrant foul type 1",
    "flagrant foul type 2",
    "clear path foul",
    sep="|"
  ),
  ignore_case = TRUE
)

# Charges drawn are recorded as "Offensive Charge" events — credit to opponent
pat_charge_drawn <- stringr::regex("offensive charge", ignore_case = TRUE)

# ---- Separate populations -----------------------------------------------------------
fouls_all_df   <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_any_foul))
fouls_shot_df  <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_shoot_foul))
fouls_tech_df  <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_tech_foul))
fouls_flag_df  <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_flag_foul))
fouls_off_df   <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_off_foul))
fouls_def_df   <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_def_foul))
charges_df     <- foul_base %>% dplyr::filter(stringr::str_detect(txt, pat_charge_drawn))

# ---- Helper: per-quarter (Q1–Q6) + CGS, credited to opponent -----------------------
tally_foul <- function(df, name_prefix) {
  # Q1–Q6 with complete quarters
  qtr_df <- df %>%
    dplyr::group_by(game_id, OPP_TEAM_ID, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, OPP_TEAM_ID) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(team_id = OPP_TEAM_ID) %>%
    dplyr::rename_with(
      ~ paste0("T_", name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Build tallies -------------------------------------------------------------------
res_fouls_all  <- tally_foul(fouls_all_df,  "FOULS")
res_fouls_shot <- tally_foul(fouls_shot_df, "SHOT_FOULS")
res_fouls_tech <- tally_foul(fouls_tech_df, "TECH_FOULS")
res_fouls_flag <- tally_foul(fouls_flag_df, "FLAG_FOULS")
res_fouls_off  <- tally_foul(fouls_off_df,  "OFF_FOULS")
res_fouls_def  <- tally_foul(fouls_def_df,  "DEF_FOULS")
res_chrg_draw  <- tally_foul(charges_df,    "CHRG_DRWN")

# ---- Join into BaseStats_Team_MC ----------------------------------------------------
stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # Total fouls
  dplyr::left_join(res_fouls_all$qtr,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_all$cgs,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Shooting
  dplyr::left_join(res_fouls_shot$qtr, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_shot$cgs, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Technical
  dplyr::left_join(res_fouls_tech$qtr, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_tech$cgs, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Flagrant
  dplyr::left_join(res_fouls_flag$qtr, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_flag$cgs, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Offensive
  dplyr::left_join(res_fouls_off$qtr,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_off$cgs,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Defensive
  dplyr::left_join(res_fouls_def$qtr,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_fouls_def$cgs,  by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Charges Drawn
  dplyr::left_join(res_chrg_draw$qtr, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(res_chrg_draw$cgs, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  # Fill any newly added NA counts with 0 (Q1–Q6 + CGS)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating: if no OT_1/OT_2, force Q5/Q6 foul counts to 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q5$"),
      ~ dplyr::if_else(OT_1 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^T_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q6$"),
      ~ dplyr::if_else(OT_2 == 1L, ., 0L)
    )
  )

rm(
  fouls_all_df, fouls_shot_df, fouls_tech_df, fouls_flag_df,
  fouls_off_df, fouls_def_df, charges_df, foul_base,
  res_fouls_all, res_fouls_shot, res_fouls_tech, res_fouls_flag,
  res_fouls_off, res_fouls_def, res_chrg_draw
)

message("[✓] Fouls (total, shooting, technical, flagrant, offensive, defensive, charges drawn) tallied Q1–Q6 (OT-aware) + CGS and joined.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fouls Data Aggregation Section ====
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
# === START: Assist Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ==================================================================================== #
# Section: Assists (counts, points, pct, AST:TOV)                                      #
# Source: pm_df                                                                        #
# Creates:                                                                             #
#   T_AST_Q1..Q4, T_AST_CGS                     (assist counts)                        #
#   T_AST_PTS_Q1..Q4, T_AST_PTS_CGS             (points on assisted makes)             #
#   T_AST_PCT_Q1..Q4, T_AST_PCT_CGS             (assist percentage)                    #
#   T_AST_TOV_Q1..Q4, T_AST_TOV_CGS             (assist-to-turnover ratio)             #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text","score_value") %in% names(pm_df)))

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
to_int   <- function(x) suppressWarnings(as.integer(x))

# ---- Build assist base from pm_df ----
ast_base <-
  pm_df %>%
  dplyr::mutate(
    qtr   = to_int(qtr),
    is_ast = stringr::str_detect(text, stringr::regex("assists", ignore_case = TRUE)),
    pts    = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(is_ast, qtr %in% 1:6)   # <-- include OT periods

# ---- Per-quarter tallies (Q1–Q6) ----
ast_qtr <-
  ast_base %>%
  dplyr::group_by(game_id, team_id, qtr) %>%
  dplyr::summarise(
    AST     = dplyr::n(),
    AST_PTS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(AST = 0L, AST_PTS = 0L)) %>%   # <-- full Q1–Q6
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(AST, AST_PTS),
    names_sep   = "_Q",
    values_fill = 0L
  ) %>%
  dplyr::rename_with(~ gsub("^AST_", "T_AST_", .x)) %>%
  dplyr::rename_with(~ gsub("^AST_PTS_", "T_AST_PTS_", .x))

# ---- Complete-game tallies (CGS) ----
ast_cgs <-
  ast_base %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(
    T_AST_CGS     = dplyr::n(),
    T_AST_PTS_CGS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join ----
stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(ast_qtr, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::left_join(ast_cgs, by = c("ESPN_GAME_ID"="game_id","ESPN_TEAM_ID"="team_id")) %>%
  dplyr::mutate(
    # Fill all Q1–Q6 + CGS
    dplyr::across(
      tidyselect::matches("^T_AST(_PTS)?_Q[1-6]$|^T_AST(_PTS)?_CGS$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # ---- OT gating: zero out Q5/Q6 when OT_1 / OT_2 did not happen ----
dplyr::mutate(
  dplyr::across(matches("^T_AST(_PTS)?_Q5$"), ~ if_else(OT_1 == 1L, ., 0L)),
  dplyr::across(matches("^T_AST(_PTS)?_Q6$"), ~ if_else(OT_2 == 1L, ., 0L))
) %>%
  # ---- Derived ----
dplyr::mutate(
  # Assist %
  T_AST_PCT_Q1  = safe_div(T_AST_Q1 , T_FGM_Q1 ) * 100,
  T_AST_PCT_Q2  = safe_div(T_AST_Q2 , T_FGM_Q2 ) * 100,
  T_AST_PCT_Q3  = safe_div(T_AST_Q3 , T_FGM_Q3 ) * 100,
  T_AST_PCT_Q4  = safe_div(T_AST_Q4 , T_FGM_Q4 ) * 100,
  T_AST_PCT_Q5  = safe_div(T_AST_Q5 , T_FGM_Q5 ) * 100,   # <-- OT1
  T_AST_PCT_Q6  = safe_div(T_AST_Q6 , T_FGM_Q6 ) * 100,   # <-- OT2
  T_AST_PCT_CGS = safe_div(T_AST_CGS, T_FGM_CGS) * 100,
  
  # AST:TOV (no if-logic anymore)
  T_AST_TOV_Q1  = safe_div(T_AST_Q1 , T_TOV_Q1 ),
  T_AST_TOV_Q2  = safe_div(T_AST_Q2 , T_TOV_Q2 ),
  T_AST_TOV_Q3  = safe_div(T_AST_Q3 , T_TOV_Q3 ),
  T_AST_TOV_Q4  = safe_div(T_AST_Q4 , T_TOV_Q4 ),
  T_AST_TOV_Q5  = safe_div(T_AST_Q5 , T_TOV_Q5 ),   # <-- OT1
  T_AST_TOV_Q6  = safe_div(T_AST_Q6 , T_TOV_Q6 ),   # <-- OT2
  T_AST_TOV_CGS = safe_div(T_AST_CGS, T_TOV_CGS)
)

rm(ast_base, ast_qtr, ast_cgs)
message("[✓] Assists (Q1–Q6 OT-aware) + clean AST:TOV complete.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Assist Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Rebounds Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ==================================================================================== #
# Section: Rebounds (OREB, DREB, Totals)                                               #
# Source: pm_df                                                                        #
# Creates (per period Q1..Q6 + CGS):                                                   #
#   Offensive: T_OREB_Q1..Q6, T_OREB_CGS                                               #
#   Defensive: T_DREB_Q1..Q6, T_DREB_CGS                                               #
#   Totals:    T_REB_Q1..Q6,  T_REB_CGS  (OREB + DREB)                                 #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text") %in% names(pm_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Base with normalized labels (now Q1–Q6) ----------------------------------------
reb_base <- pm_df %>%
  dplyr::mutate(
    qtr = to_int(qtr),
    lbl = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(qtr %in% 1:6)

# Patterns
pat_oreb <- stringr::regex("^offensive rebound$", ignore_case = TRUE)
pat_dreb <- stringr::regex("^defensive rebound$", ignore_case = TRUE)

# ---- Helper: tally by quarter + CGS, credited to the same team ----------------------
tally_reb <- function(df, name_prefix) {
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    # ensure we have Q1–Q6 even if some are missing
    dplyr::group_by(game_id, team_id) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = qtr, values_from = N,
      values_fill = 0L, names_prefix = "Q"
    ) %>%
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build pieces
res_oreb <- reb_base %>%
  dplyr::filter(stringr::str_detect(lbl, pat_oreb)) %>%
  tally_reb("OREB")

res_dreb <- reb_base %>%
  dplyr::filter(stringr::str_detect(lbl, pat_dreb)) %>%
  tally_reb("DREB")

# ---- Join into BaseStats_Team_MC -----------------------------------------------------
stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # Offensive rebounds
  dplyr::left_join(res_oreb$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_oreb$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # Defensive rebounds
  dplyr::left_join(res_dreb$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_dreb$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                        "ESPN_TEAM_ID" = "team_id")) %>%
  # Fill new NAs with 0; now also Q5/Q6
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(O|D)REB_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    ),
    # Totals = OREB + DREB (Q1–Q6 + CGS)
    T_REB_Q1  = T_OREB_Q1  + T_DREB_Q1,
    T_REB_Q2  = T_OREB_Q2  + T_DREB_Q2,
    T_REB_Q3  = T_OREB_Q3  + T_DREB_Q3,
    T_REB_Q4  = T_OREB_Q4  + T_DREB_Q4,
    T_REB_Q5  = T_OREB_Q5  + T_DREB_Q5,
    T_REB_Q6  = T_OREB_Q6  + T_DREB_Q6,
    T_REB_CGS = T_OREB_CGS + T_DREB_CGS
  )

# --- OPPONENT REBOUNDS (OREB/DREB/REB) ----------------------------------------

stopifnot(all(c("ESPN_GAME_ID","ESPN_TEAM_ID") %in% names(BaseStats_Team_MC)))

# 1) Make sure team rebound columns are numeric and 0-filled (now Q1–Q6)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^(T|OPP)_(O?D?REB)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(suppressWarnings(as.integer(.)), 0L)
    )
  )

# 2) Build an "opponent snapshot" per game (same table, just renamed)
opp_df <- BaseStats_Team_MC %>%
  dplyr::select(
    ESPN_GAME_ID,
    OPPONENT_TEAM_ID = ESPN_TEAM_ID,
    tidyselect::starts_with("T_OREB_"),
    tidyselect::starts_with("T_DREB_"),
    tidyselect::starts_with("T_REB_")
  ) %>%
  # rename T_* -> OPP_*
  dplyr::rename_with(~ paste0("OPP_", sub("^T_", "", .x)),
                     .cols = tidyselect::starts_with("T_"))

# 3) Self-join by game, keep the "other" team as the opponent
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::inner_join(opp_df, by = "ESPN_GAME_ID") %>%
  dplyr::filter(ESPN_TEAM_ID != OPPONENT_TEAM_ID) %>%
  dplyr::select(-OPPONENT_TEAM_ID)

# 4) If opponent REB_* totals aren’t present, create them as OREB + DREB
mk_opp_totals <- function(df, tag) {
  oreb <- paste0("OPP_OREB_", tag)
  dreb <- paste0("OPP_DREB_", tag)
  treb <- paste0("OPP_REB_",  tag)
  if (!treb %in% names(df)) {
    df[[treb]] <- dplyr::coalesce(df[[oreb]], 0L) + dplyr::coalesce(df[[dreb]], 0L)
  }
  df
}

for (s in c("Q1","Q2","Q3","Q4","Q5","Q6","CGS")) {
  BaseStats_Team_MC <- mk_opp_totals(BaseStats_Team_MC, s)
}

# 5) Final NA safety for any new opponent REB fields (numeric only)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^OPP_(O?D?REB)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(suppressWarnings(as.integer(.)), 0L)
    )
  )

# ============================
# Rebounding % + Allowed + Adjusted
# ============================

stopifnot(exists("BaseStats_Team_MC"))

# safety: coalesce the inputs we’ll use (Q1–Q6 + CGS)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^(T|OPP)_(O?D?REB)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(suppressWarnings(as.integer(.)), 0L)
    )
  )

tags <- c("Q1","Q2","Q3","Q4","Q5","Q6","CGS")

for (s in tags) {
  # -------- team % --------
  # Total REB% = REB / (REB + OPP_REB)
  BaseStats_Team_MC[[paste0("T_REB_PCT_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_REB_", s)]],
      BaseStats_Team_MC[[paste0("T_REB_", s)]] +
        BaseStats_Team_MC[[paste0("OPP_REB_", s)]]
    )
  
  # OREB% = OREB / (OREB + OPP_DREB)
  BaseStats_Team_MC[[paste0("T_OREB_PCT_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_OREB_", s)]],
      BaseStats_Team_MC[[paste0("T_OREB_", s)]] +
        BaseStats_Team_MC[[paste0("OPP_DREB_", s)]]
    )
  
  # DREB% = DREB / (DREB + OPP_OREB)
  BaseStats_Team_MC[[paste0("T_DREB_PCT_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_DREB_", s)]],
      BaseStats_Team_MC[[paste0("T_DREB_", s)]] +
        BaseStats_Team_MC[[paste0("OPP_OREB_", s)]]
    )
  
  # -------- opponent “allowed” % (mirror of team %) --------
  # Opp REB% allowed = 100 - team REB%
  BaseStats_Team_MC[[paste0("OPP_REB_PCT_ALLO_", s)]] <-
    100 - BaseStats_Team_MC[[paste0("T_REB_PCT_", s)]]
  
  # Opp OREB% allowed = 100 - team DREB%
  BaseStats_Team_MC[[paste0("OPP_OREB_PCT_ALLO_", s)]] <-
    100 - BaseStats_Team_MC[[paste0("T_DREB_PCT_", s)]]
  
  # Opp DREB% allowed = 100 - team OREB%
  BaseStats_Team_MC[[paste0("OPP_DREB_PCT_ALLO_", s)]] <-
    100 - BaseStats_Team_MC[[paste0("T_OREB_PCT_", s)]]
  
  # -------- adjusted index (team vs what opponent allows) --------
  # Adjusted REB index = T_REB_PCT / OPP_REB_PCT_ALLO * 100
  BaseStats_Team_MC[[paste0("T_ADJ_REB_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_REB_PCT_", s)]],
      BaseStats_Team_MC[[paste0("OPP_REB_PCT_ALLO_", s)]]
    )
}

# Optional NA tidy (leave NaN as NA)
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^(T|OPP)_(O?D?REB)(_PCT|_PCT_ALLO|_PCT_CGS|_ADJ_REB)"),
      ~ ifelse(is.nan(.), NA_real_, .)
    )
  )

rm(opp_df, reb_base, res_dreb, res_oreb)
message("[✓] Rebounding (Q1–Q6 + CGS) + opponent allowed + adjusted indices computed.")


# [✓] OPP_OREB_*, OPP_DREB_*, and OPP_REB_* added to BaseStats_Team_MC

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Rebounds Data Aggregation Section ====
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
# === START: Steals and Blocks Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==================================================================================== #
# Section: Steals & Blocks                                                             #
# Source: pm_df                                                                        #
# Creates (per period Q1..Q6 + CGS):                                                   #
#   Steals:  T_STL_Q1..Q6, T_STL_CGS, T_STL_PCT_Q1..Q6, T_STL_PCT_CGS                  #
#   Blocks:  T_BLK_Q1..Q6, T_BLK_CGS, T_BLK_PCT_Q1..Q6, T_BLK_PCT_CGS                  #
# Notes: Counts credited to the OPPONENT team (committing team → opposite team_id).    #
# ==================================================================================== #

stopifnot(all(c("game_id","team_id","qtr","type_text",
                "home_team_id","away_team_id") %in% names(pm_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Common base & normalized labels ------------------------------------------------
hustle_base <- pm_df %>%
  dplyr::mutate(
    qtr         = to_int(qtr),
    OPP_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    txt         = stringr::str_to_lower(stringr::str_trim(text))
  ) %>%
  # include OT periods now (1..6)
  dplyr::filter(qtr %in% 1:6)

# ---- Pattern definitions ------------------------------------------------------------
pat_steal <- stringr::regex("steal", ignore_case = TRUE)
pat_block <- stringr::regex("block", ignore_case = TRUE)

# ---- Subsets ------------------------------------------------------------------------
stl_df <- hustle_base %>% dplyr::filter(stringr::str_detect(txt, pat_steal))
blk_df <- hustle_base %>% dplyr::filter(stringr::str_detect(txt, pat_block))

# ---- Helper: tally by quarter + CGS (credited to opponent) --------------------------
tally_hustle <- function(df, name_prefix) {
  # Q1–Q6
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    tidyr::pivot_wider(
      names_from = qtr, values_from = N, values_fill = 0L, names_prefix = "Q"
    ) %>%
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Build pieces -------------------------------------------------------------------
res_stl <- tally_hustle(stl_df, "STL")
res_blk <- tally_hustle(blk_df, "BLK")

# ---- Join into BaseStats_Team_MC -----------------------------------------------------
stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # Steals
  dplyr::left_join(res_stl$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                       "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_stl$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                       "ESPN_TEAM_ID" = "team_id")) %>%
  # Blocks
  dplyr::left_join(res_blk$qtr, by = c("ESPN_GAME_ID" = "game_id",
                                       "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_blk$cgs, by = c("ESPN_GAME_ID" = "game_id",
                                       "ESPN_TEAM_ID" = "team_id")) %>%
  # Fill NAs for new hustle-count columns
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(STL|BLK)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- Bring in opponent T_POSS_* and T_FGA_* via group sums --------------------------
# We assume exactly 2 teams per ESPN_GAME_ID; opponent stat = sum(game) - own stat.
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::group_by(ESPN_GAME_ID) %>%
  dplyr::mutate(
    # opponent possessions
    OPP_T_POSS_Q1  = sum(T_POSS_Q1,  na.rm = TRUE) - T_POSS_Q1,
    OPP_T_POSS_Q2  = sum(T_POSS_Q2,  na.rm = TRUE) - T_POSS_Q2,
    OPP_T_POSS_Q3  = sum(T_POSS_Q3,  na.rm = TRUE) - T_POSS_Q3,
    OPP_T_POSS_Q4  = sum(T_POSS_Q4,  na.rm = TRUE) - T_POSS_Q4,
    OPP_T_POSS_Q5  = sum(T_POSS_Q5,  na.rm = TRUE) - T_POSS_Q5,
    OPP_T_POSS_Q6  = sum(T_POSS_Q6,  na.rm = TRUE) - T_POSS_Q6,
    OPP_T_POSS_CGS = sum(T_POSS_CGS, na.rm = TRUE) - T_POSS_CGS,
    
    # opponent FGA
    OPP_T_FGA_Q1  = sum(T_FGA_Q1,  na.rm = TRUE) - T_FGA_Q1,
    OPP_T_FGA_Q2  = sum(T_FGA_Q2,  na.rm = TRUE) - T_FGA_Q2,
    OPP_T_FGA_Q3  = sum(T_FGA_Q3,  na.rm = TRUE) - T_FGA_Q3,
    OPP_T_FGA_Q4  = sum(T_FGA_Q4,  na.rm = TRUE) - T_FGA_Q4,
    OPP_T_FGA_Q5  = sum(T_FGA_Q5,  na.rm = TRUE) - T_FGA_Q5,
    OPP_T_FGA_Q6  = sum(T_FGA_Q6,  na.rm = TRUE) - T_FGA_Q6,
    OPP_T_FGA_CGS = sum(T_FGA_CGS, na.rm = TRUE) - T_FGA_CGS
  ) %>%
  dplyr::ungroup()

# ---- Percentages --------------------------------------------------------------------
# Steal Percentage = Steals / Opponent Possessions * 100
# Block Percentage = Blocks / Opponent FGA * 100

pct_spans <- c("Q1","Q2","Q3","Q4","Q5","Q6","CGS")

for (s in pct_spans) {
  # STL%
  BaseStats_Team_MC[[paste0("T_STL_PCT_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_STL_", s)]],
      BaseStats_Team_MC[[paste0("OPP_T_POSS_", s)]]
    )
  
  # BLK%
  BaseStats_Team_MC[[paste0("T_BLK_PCT_", s)]] <-
    100 * safe_div(
      BaseStats_Team_MC[[paste0("T_BLK_", s)]],
      BaseStats_Team_MC[[paste0("OPP_T_FGA_", s)]]
    )
}

rm(blk_df, hustle_base, res_blk, res_stl, stl_df)
message("[✓] Steals & Blocks (counts + STL% + BLK%) computed and joined successfully.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Steals and Blocks Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: TOP-LEVEL Defense Data Aggregation Section ====





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
# === START: Runs Data Aggregation Section ====
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
    run_end_clock_seconds   = round(run_end_clock   %% 60)
  )

# (Optional) If you also want a prefiltered version, e.g., only 6+ point runs:
# run_stats_6plus <- run_stats %>% filter(run_points >= 6)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Runs Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Runs Stats Creation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# =============================
# Helpers (quarter + CGS tallies)
# =============================

.tally_qtr_cgs_count <- function(df, name_prefix) {
  # per-quarter counts
  qtr_df <- df |>
    dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) |>
    dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = qtr, values_from = N, values_fill = 0L, names_prefix = "Q"
    ) |>
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  # complete game counts
  cgs_df <- df |>
    dplyr::group_by(game_id, team_id = run_team) |>
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") |>
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

.tally_qtr_cgs_stat <- function(df, name_prefix, fun) {
  # per-quarter stat (mean, sd, etc.)
  qtr_df <- df |>
    dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) |>
    dplyr::summarise(V = fun(run_points, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = qtr, values_from = V, names_prefix = "Q"
    ) |>
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  # complete game stat
  cgs_df <- df |>
    dplyr::group_by(game_id, team_id = run_team) |>
    dplyr::summarise(V = fun(run_points, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := V)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# =============================
# Build the various run datasets
# =============================

runs_7p_df   <- run_stats |> dplyr::filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4, run_points >= 7)
runs_10p_df   <- run_stats |> dplyr::filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4, run_points >= 10)
runs_15p_df   <- run_stats |> dplyr::filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4, run_points >= 15)

# Time windows use runs >= 15 and last X minutes of the period
runs_1min_df  <- runs_15p_df |> dplyr::filter(run_start_clock_minutes < 1)
runs_3min_df  <- runs_15p_df |> dplyr::filter(run_start_clock_minutes < 3)
runs_5min_df  <- runs_15p_df |> dplyr::filter(run_start_clock_minutes < 5)
runs_10min_df <- runs_15p_df |> dplyr::filter(run_start_clock_minutes < 10)

# Efficiency (mean) and Volatility (sd) across all detected runs (no threshold)
runs_eff_df   <- run_stats |> dplyr::filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4)
runs_vol_df   <- runs_eff_df

# Success rate: share of runs with points >= 7
runs_succ_df  <- runs_eff_df |> dplyr::mutate(succ = run_points >= 7)

# =============================
# Tally each into Q1..Q4 + CGS
# =============================

res_7p   <- .tally_qtr_cgs_count(runs_10p_df, "RUNS_7P")
res_10p   <- .tally_qtr_cgs_count(runs_10p_df, "RUNS_10P")
res_15p   <- .tally_qtr_cgs_count(runs_15p_df, "RUNS_15P")

res_1min  <- .tally_qtr_cgs_count(runs_1min_df,  "RUNS_1MINL")
res_3min  <- .tally_qtr_cgs_count(runs_3min_df,  "RUNS_3MINL")
res_5min  <- .tally_qtr_cgs_count(runs_5min_df,  "RUNS_5MINL")
res_10min <- .tally_qtr_cgs_count(runs_10min_df, "RUNS_10MINL")

res_eff   <- .tally_qtr_cgs_stat(runs_eff_df, "RUN_EFF", mean)
res_vol   <- .tally_qtr_cgs_stat(runs_vol_df, "RUN_VOL", stats::sd)

# Success rate uses the mean of the logical condition (TRUE/FALSE)
res_succ_qtr <- runs_succ_df |>
  dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) |>
  dplyr::summarise(V = mean(succ, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = qtr, values_from = V, names_prefix = "Q") |>
  dplyr::rename_with(~ paste0("T_RUN_SUCC_RATE_", .x), dplyr::starts_with("Q"))

res_succ_cgs <- runs_succ_df |>
  dplyr::group_by(game_id, team_id = run_team) |>
  dplyr::summarise(V = mean(succ, na.rm = TRUE), .groups = "drop") |>
  dplyr::rename(T_RUN_SUCC_RATE_CGS = V)

# =============================
# Join into BaseStats_Team_MC
# =============================

stopifnot(exists("BaseStats_Team_MC"))

BaseStats_Team_MC <- BaseStats_Team_MC |>
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) |>
  # 10+ and 15+ run counts
  dplyr::left_join(res_7p$qtr,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_7p$cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_10p$qtr,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_10p$cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_15p$qtr,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_15p$cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  # time-window run counts (>=15)
  dplyr::left_join(res_1min$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_1min$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_3min$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_3min$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_5min$qtr,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_5min$cgs,  by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_10min$qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_10min$cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  # efficiency & volatility
  dplyr::left_join(res_eff$qtr,    by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_eff$cgs,    by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_vol$qtr,    by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_vol$cgs,    by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  # success rate
  dplyr::left_join(res_succ_qtr,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  dplyr::left_join(res_succ_cgs,   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) |>
  # coalesce new numeric fields
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(RUNS_(10P|15P|1MINL|3MINL|5MINL|10MINL)_(Q[1-4]|CGS))$"),
      ~ dplyr::coalesce(., 0L)
    )
  )


# =============================
# Runs Stopped (simple) + Momentum Shift (strict)
# =============================

# --- parameter for "momentum shift" drought (≈ two empty trips) ---
DROUGHT_SEC <- 45

# 1) previous-run team + end clock per game (ordered)
runs_with_prev <- run_stats %>%
  dplyr::group_by(game_id) %>%
  dplyr::arrange(run_start_row, .by_group = TRUE) %>%
  dplyr::mutate(
    prev_run_team       = dplyr::lag(run_team),
    prev_run_end_clock  = dplyr::lag(run_end_clock)   # opponent's last scoring time
  ) %>%
  dplyr::ungroup()

# 2) Simple stops = our run begins immediately after opponent’s run
stops_simple <- runs_with_prev %>%
  dplyr::filter(!is.na(prev_run_team),
                prev_run_team != run_team,
                !is.na(run_start_qtr), run_start_qtr %in% 1:4)

# 3) Momentum-shift stops = simple stop AND opponent drought >= DROUGHT_SEC
DROUGHT_SEC <- 45  # ≈ two empty trips; tune if you want

stops_strict <- runs_with_prev %>%
  dplyr::filter(!is.na(prev_run_team),
                prev_run_team != run_team,
                !is.na(run_start_qtr), run_start_qtr %in% 1:4,
                !is.na(prev_run_end_clock), !is.na(run_start_clock)) %>%
  dplyr::mutate(
    opp_drought_sec = prev_run_end_clock - run_start_clock  # clock counts down
  ) %>%
  dplyr::filter(opp_drought_sec >= DROUGHT_SEC)

# 4) Tally (unchanged)
.tally_stops <- function(df, name_prefix) {
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = qtr, values_from = N, values_fill = 0L, names_prefix = "Q") %>%
    dplyr::rename_with(~ paste0("T_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = run_team) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("T_", name_prefix, "_CGS") := CGS)
  list(qtr = qtr_df, cgs = cgs_df)
}

res_stops_simple <- .tally_stops(stops_simple, "RUNS_STOPS")
res_stops_strict <- .tally_stops(stops_strict, "RUNS_STOP_SHIFT")


# 6) Join into BaseStats_Team_MC
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # simple stops
  dplyr::left_join(res_stops_simple$qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_stops_simple$cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # strict momentum-shift stops
  dplyr::left_join(res_stops_strict$qtr, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(res_stops_strict$cgs, by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  # fill missing counts with 0
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^T_(RUNS_STOPS|RUNS_STOP_SHIFT)_(Q[1-4]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  )


# =============================
# Stop Rates (simple + momentum-shift)
# =============================

# We'll also keep the previous run's END QUARTER to define "opportunity quarter"
runs_with_prev <- runs_with_prev %>%
  dplyr::group_by(game_id) %>%
  dplyr::arrange(run_start_row, .by_group = TRUE) %>%
  dplyr::mutate(
    prev_run_end_qtr = dplyr::lag(run_end_qtr)
  ) %>%
  dplyr::ungroup()

# ---- Simple stop opportunities ----
# For Team T, an opportunity occurs whenever the PREVIOUS run was by the opponent.
# We attribute the opportunity to the quarter in which that previous (opponent) run ENDED.
opps_simple <- runs_with_prev %>%
  dplyr::filter(!is.na(prev_run_team), prev_run_team != run_team,
                !is.na(prev_run_end_qtr), prev_run_end_qtr %in% 1:4) %>%
  dplyr::group_by(game_id, team_id = run_team, qtr = prev_run_end_qtr) %>%
  dplyr::summarise(OPPS = dplyr::n(), .groups = "drop")

# ---- Simple stops (already defined as our run starting right after an opponent run) ----
stops_simple_q <- stops_simple %>%
  dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) %>%
  dplyr::summarise(STOPS = dplyr::n(), .groups = "drop")

# ---- Momentum-shift stops (strict) ----
stops_strict_q <- stops_strict %>%
  dplyr::group_by(game_id, team_id = run_team, qtr = run_start_qtr) %>%
  dplyr::summarise(SHIFT_STOPS = dplyr::n(), .groups = "drop")

# ---- Join and compute rates by quarter ----
stops_rates_q <- opps_simple %>%
  dplyr::full_join(stops_simple_q, by = c("game_id","team_id","qtr")) %>%
  dplyr::full_join(stops_strict_q, by = c("game_id","team_id","qtr")) %>%
  dplyr::mutate(
    OPPS        = dplyr::coalesce(OPPS, 0L),
    STOPS       = dplyr::coalesce(STOPS, 0L),
    SHIFT_STOPS = dplyr::coalesce(SHIFT_STOPS, 0L),
    T_RUNS_STOPS_RATE   = dplyr::if_else(OPPS > 0, STOPS / OPPS, NA_real_),
    T_RUNS_STOP_SHIFT_RATE = dplyr::if_else(STOPS > 0, SHIFT_STOPS / STOPS, NA_real_)
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(game_id, team_id),
    names_from = qtr,
    values_from = c(T_RUNS_STOPS_RATE, T_RUNS_STOP_SHIFT_RATE),
    names_glue = "{.value}_Q{qtr}"
  )

# ---- CGS (full game) rates ----
opps_simple_cgs <- opps_simple %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(OPPS = sum(OPPS), .groups = "drop")

stops_simple_cgs <- stops_simple_q %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(STOPS = sum(STOPS), .groups = "drop")

stops_strict_cgs <- stops_strict_q %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::summarise(SHIFT_STOPS = sum(SHIFT_STOPS), .groups = "drop")

stops_rates_cgs <- opps_simple_cgs %>%
  dplyr::full_join(stops_simple_cgs, by = c("game_id","team_id")) %>%
  dplyr::full_join(stops_strict_cgs, by = c("game_id","team_id")) %>%
  dplyr::mutate(
    OPPS        = dplyr::coalesce(OPPS, 0L),
    STOPS       = dplyr::coalesce(STOPS, 0L),
    SHIFT_STOPS = dplyr::coalesce(SHIFT_STOPS, 0L),
    T_RUNS_STOPS_RATE_CGS      = dplyr::if_else(OPPS > 0, STOPS / OPPS, NA_real_),
    T_RUNS_STOP_SHIFT_RATE_CGS  = dplyr::if_else(STOPS > 0, SHIFT_STOPS / STOPS, NA_real_)
  ) %>%
  dplyr::select(game_id, team_id,
                T_RUNS_STOPS_RATE_CGS,
                T_RUNS_STOP_SHIFT_RATE_CGS)

# ---- Join rates into BaseStats_Team_MC ----
BaseStats_Team_MC <- BaseStats_Team_MC %>%
  dplyr::left_join(stops_rates_q,
                   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id")) %>%
  dplyr::left_join(stops_rates_cgs,
                   by = c("ESPN_GAME_ID" = "game_id", "ESPN_TEAM_ID" = "team_id"))


message(sprintf("[✓] Runs Stopped (simple) + Momentum Shift (strict, ≥ %ss drought) computed and joined.", DROUGHT_SEC))

message("[✓] Runs (10+, 15+, time windows, efficiency, volatility, success rate) computed and joined.")


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Runs Stats Creation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START ==== Write BaseStats Team for Monte Carlo and Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 9.6 Export the updated BaseStats_Team data frame
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_",
  season_token,
  ".csv"
)
write.csv(BaseStats_Team_MC, file = team_output_path, row.names = FALSE)

print(paste("BaseStats_Team for Monte Carlo has been exported to:", team_output_path))

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END ==== Write BaseStats Team for Monte Carlo and Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀