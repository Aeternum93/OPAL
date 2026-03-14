#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     .oooooo.                                    .                    .o    .oooo.        ooo        ooooo                            ooooo         o8o                                                     .oooooo..o                                                                                
#    d8P'  `Y8b                                 .o8                  o888   d8P'`Y8b       `88.       .888'                            `888'         `"'                                                    d8P'    `Y8                                                                                
#   888          oooo d8b  .ooooo.   .oooo.   .o888oo  .ooooo.        888  888    888       888b     d'888   .oooo.   ooo. .oo.         888         oooo  ooo. .oo.    .ooooo.  oooo  oooo  oo.ooooo.       Y88bo.      oooo  oooo  ooo. .oo.  .oo.   ooo. .oo.  .oo.    .oooo.   oooo d8b oooo    ooo 
#   888          `888""8P d88' `88b `P  )88b    888   d88' `88b       888  888    888       8 Y88. .P  888  `P  )88b  `888P"Y88b        888         `888  `888P"Y88b  d88' `88b `888  `888   888' `88b       `"Y8888o.  `888  `888  `888P"Y88bP"Y88b  `888P"Y88bP"Y88b  `P  )88b  `888""8P  `88.  .8'  
#   888           888     888ooo888  .oP"888    888   888ooo888       888  888    888       8  `888'   888   .oP"888   888   888        888          888   888   888  888ooo888  888   888   888   888           `"Y88b  888   888   888   888   888   888   888   888   .oP"888   888       `88..8'   
#   `88b    ooo   888     888    .o d8(  888    888 . 888    .o       888  `88b  d88'       8    Y     888  d8(  888   888   888        888       o  888   888   888  888    .o  888   888   888   888      oo     .d8P  888   888   888   888   888   888   888   888  d8(  888   888        `888'    
#    `Y8bood8P'  d888b    `Y8bod8P' `Y888""8o   "888" `Y8bod8P'      o888o  `Y8bd8P'       o8o        o888o `Y888""8o o888o o888o      o888ooooood8 o888o o888o o888o `Y8bod8P'  `V88V"V8P'  888bod8P'      8""88888P'   `V88V"V8P' o888o o888o o888o o888o o888o o888o `Y888""8o d888b        .8'     
#                                                                                                                                                                                            888                                                                                           .o..P'      
#                                                                                                                                                                                           o888o                                                                                          `Y8P'       
#                                                                                                                                                                                                                                                                                                   
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Create 10-Man Team Rotation (Home + Away)  ==== 
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

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


library(data.table)
library(dplyr)
library(tidyr)
library(zoo)
library(readr)

#========================#
# 1. Load PBP Data File  #
#========================#

pbp_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/pm_nbapbp_",
  season_token, ".csv"
)

nbapbp_df <- fread(
  pbp_path,
  colClasses = "character",
  encoding = "UTF-8"
)

#==========================#
# 2. Prep for lineup work  #
#==========================#

nbapbp_df <- nbapbp_df %>%
  mutate(
    start_quarter_seconds_remaining = suppressWarnings(as.numeric(start_quarter_seconds_remaining)),
    score_value                     = suppressWarnings(as.numeric(score_value)),
    qtr                             = suppressWarnings(as.integer(qtr)),
    
    # 5-man keys
    lineup_home_ids = paste(home_P1_espn_id, home_P2_espn_id, home_P3_espn_id,
                            home_P4_espn_id, home_P5_espn_id, sep = "|"),
    
    lineup_away_ids = paste(away_P1_espn_id, away_P2_espn_id, away_P3_espn_id,
                            away_P4_espn_id, away_P5_espn_id, sep = "|"),
    
    # 🔥 10-man environment key
    LINEUP_KEY_10M = paste(lineup_home_ids, lineup_away_ids, sep = " || "),
    
    # ✅ rebound flags (team-neutral on raw PBP)
    IS_OREB = if_else(type_text %in% c("Offensive Rebound", "Offensive Team Rebound"),
                      1L, 0L),
    IS_DREB = if_else(type_text %in% c("Defensive Rebound", "Defensive Team Rebound"),
                      1L, 0L)
  ) %>%
  filter(!is.na(LINEUP_KEY_10M) &
           LINEUP_KEY_10M != "NA|NA|NA|NA|NA || NA|NA|NA|NA|NA")

# =============================================================================
# ██████  ███████  ██████  ██       █████   ██████ ███████ ███    ███ ███████ ███    ██ ████████
# ██   ██ ██      ██       ██      ██   ██ ██      ██      ████  ████ ██      ████   ██    ██
# ██████  █████   ██   ███ ██      ███████ ██      █████   ██ ████ ██ █████   ██ ██  ██    ██
# ██   ██ ██      ██    ██ ██      ██   ██ ██      ██      ██  ██  ██ ██      ██  ██ ██    ██
# ██   ██ ███████  ██████  ███████ ██   ██  ██████ ███████ ██      ██ ███████ ██   ████    ██
#
#  OPTIMIZED SINGLE-PASS DATA.TABLE INSERT BLOCK
#  ─────────────────────────────────────────────────────────────────────────────
#  HOW TO USE THIS FILE:
#
#  STEP 1: In your original script, after Section 2 (the dplyr prep/mutate block
#          ending with the filter on LINEUP_KEY_10M), ADD this entire block.
#
#  STEP 2: Then DELETE (or comment out) all of the following sections that
#          follow Section 2 up through the 10-man aggregation end:
#            - Section 3 (stint seconds)           → replaced below
#            - Section 4 (starting lineups)        → replaced below
#            - Section 5a/5b/5c (time + scoring)   → replaced below
#            - TIE section                         → replaced below
#            - POSS section                        → replaced below
#            - Lead/Trail/Neut section             → replaced below
#            - OFF/DEF success section             → replaced below
#            - POSS TIME section                   → replaced below
#            - FG section (FGA/FGM)                → replaced below
#            - FT section (FTA/FTM)                → replaced below
#            - 2PT section                         → replaced below
#            - 3PT section                         → replaced below
#            - Pull-Up section                     → replaced below
#            - Mid-Range section                   → replaced below
#            - Rim section                         → replaced below
#            - ATB 3PT section                     → replaced below
#            - Clutch FG section                   → replaced below
#            - Clutch 3PT section                  → replaced below
#
#  STEP 3: Keep everything AFTER the 10-man block intact (5-man lineup section,
#          player-level section, runs section, defense/STL/BLK, impact
#          assessment, headshot mapping, and saves). Those sections use
#          lineup_team_level_summary_df which this block produces correctly.
#
#  WHY: The original code makes ~20 independent full scans of nbapbp_df (456k
#       rows), each doing group_by → summarise → complete → pivot_wider → join.
#       This block does ONE scan with all flags computed simultaneously via
#       data.table, then dcasts once per group key. Expected speedup: 5–10x.
# =============================================================================


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: OPTIMIZED SINGLE-PASS PRE-AGGREGATION (replaces Sections 3–Clutch) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(data.table)
library(stringr)

message("[⏱] Starting optimized single-pass aggregation...")
t0 <- proc.time()

# ─────────────────────────────────────────────────────────────────────────────
# 0. Convert nbapbp_df to data.table in-place (no copy)
# ─────────────────────────────────────────────────────────────────────────────
setDT(nbapbp_df)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Resolve column names that vary by data vintage
# ─────────────────────────────────────────────────────────────────────────────
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else stop("None of [", paste(candidates, collapse=", "), "] found in nbapbp_df")
}
zone_col   <- .pick_col(nbapbp_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))
type_id_col <- if ("type_id"      %in% names(nbapbp_df)) "type_id" else
  if ("play_type_id" %in% names(nbapbp_df)) "play_type_id" else NA_character_

# ─────────────────────────────────────────────────────────────────────────────
# 2. Type coercions (done once in data.table, modifies in-place)
# ─────────────────────────────────────────────────────────────────────────────
nbapbp_df[, `:=`(
  start_quarter_seconds_remaining = suppressWarnings(as.numeric(start_quarter_seconds_remaining)),
  score_value   = suppressWarnings(as.numeric(score_value)),
  qtr           = suppressWarnings(as.integer(qtr)),
  home_score    = suppressWarnings(as.integer(home_score)),
  away_score    = suppressWarnings(as.integer(away_score)),
  sequence_number = suppressWarnings(as.integer(sequence_number))
)]

# ─────────────────────────────────────────────────────────────────────────────
# 3. Build LINEUP keys (already done in Section 2 dplyr block — skip if present)
# ─────────────────────────────────────────────────────────────────────────────
if (!"lineup_home_ids" %in% names(nbapbp_df)) {
  nbapbp_df[, lineup_home_ids := paste(home_P1_espn_id, home_P2_espn_id, home_P3_espn_id,
                                       home_P4_espn_id, home_P5_espn_id, sep = "|")]
  nbapbp_df[, lineup_away_ids := paste(away_P1_espn_id, away_P2_espn_id, away_P3_espn_id,
                                       away_P4_espn_id, away_P5_espn_id, sep = "|")]
  nbapbp_df[, LINEUP_KEY_10M := paste(lineup_home_ids, lineup_away_ids, sep = " || ")]
}

# Keep only rows with valid 10-man keys
nbapbp_df <- nbapbp_df[
  !is.na(LINEUP_KEY_10M) & LINEUP_KEY_10M != "NA|NA|NA|NA|NA || NA|NA|NA|NA|NA"
]

# ─────────────────────────────────────────────────────────────────────────────
# 4. Possession exclude types (mirrors original script)
# ─────────────────────────────────────────────────────────────────────────────
exclude_types <- as.character(c(
  97,98,99,100,101,102,103,104,105,106,107,108,157,165,166,
  584, 412,402,
  213,214,215,216,277,278,279,280,
  22,24,32,31,36,37,40,43,44,45,
  8,25,28,29,30,33,35,47,48,517,
  155,156,
  16,
  10,11,12,13,
  70,71,72,73,74
))

# ─────────────────────────────────────────────────────────────────────────────
# 5. Compute STINT SECONDS (replaces Section 3) — data.table shift() = fast lead()
# ─────────────────────────────────────────────────────────────────────────────
setorder(nbapbp_df, game_id, qtr, -start_quarter_seconds_remaining, sequence_number)
nbapbp_df[, next_qsr := shift(start_quarter_seconds_remaining, type = "lead", fill = 0),
          by = .(game_id, qtr)]
nbapbp_df[, stint_seconds := pmax(start_quarter_seconds_remaining - next_qsr, 0)]

# ─────────────────────────────────────────────────────────────────────────────
# 6. Success criteria flags (replaces OFFTEAM/DEFTEAM SUCCESS section)
# ─────────────────────────────────────────────────────────────────────────────
crit_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/success_criteria_nba.csv"
succ_crit_dt <- fread(crit_path, colClasses = "character")
off_vec <- trimws(na.omit(succ_crit_dt$offteam_success))
def_vec <- trimws(na.omit(succ_crit_dt$defteam_success))
rm(succ_crit_dt)

nbapbp_df[, OFFTEAM_SUCCESS := fifelse(type_text %in% off_vec, 1L, 0L)]
nbapbp_df[, DEFTEAM_SUCCESS := fifelse(type_text %in% def_vec, 1L, 0L)]

# ─────────────────────────────────────────────────────────────────────────────
# 7. Boolean helpers — all flags in ONE PASS
# ─────────────────────────────────────────────────────────────────────────────
.to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

three_labels <- c("Above the Break 3", "Right Corner 3", "Left Corner 3")

nbapbp_df[, `:=`(
  # Basic rebound flags
  IS_OREB = fifelse(type_text %in% c("Offensive Rebound","Offensive Team Rebound"), 1L, 0L),
  IS_DREB = fifelse(type_text %in% c("Defensive Rebound","Defensive Team Rebound"), 1L, 0L),
  
  # Possession event flag
  POSSESSION_EVENT = fifelse(!get(if (!is.na(type_id_col)) type_id_col else "type_id") %in% exclude_types, 1L, 0L),
  
  # Shot flags
  is_shot_raw = .to_bool(shooting_play),
  is_make_raw = .to_bool(scoring_play),
  score_val_i = suppressWarnings(as.integer(score_value)),
  
  # Zone flags
  is_3pt_zone   = get(zone_col) %in% three_labels,
  is_atb3_zone  = get(zone_col) == "Above the Break 3",
  is_midr_zone  = tolower(trimws(get(zone_col))) == "mid-range",
  is_rim_zone   = tolower(trimws(get(zone_col))) == "restricted area",
  
  # Acting team within the 10-man environment
  ESPN_TEAM_ID_ACT = fifelse(team_id == home_team_id, home_team_id,
                             fifelse(team_id == away_team_id, away_team_id, NA_character_)),
  DEF_TEAM_ID      = fifelse(team_id == home_team_id, away_team_id,
                             fifelse(team_id == away_team_id, home_team_id, NA_character_)),
  
  # Tie detection: home score diff
  home_diff = home_score - away_score,
  
  # Clock seconds remaining
  t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
    suppressWarnings(as.integer(clock_seconds)),
  
  # FT flag
  is_ft_type = if (!is.na(type_id_col))
    suppressWarnings(as.integer(get(type_id_col))) %in% 97:102
  else
    str_detect(type_text, regex("\\bFree Throw\\b", ignore_case = TRUE)),
  
  # Pull-up flag
  is_pu = str_detect(type_text, regex("pullup", ignore_case = TRUE))
)]

# Derived shot-type flags (depend on the above)
nbapbp_df[, `:=`(
  # FG = shot but not FT
  is_fga = is_shot_raw & !is_ft_type &
    !grepl("free throw", type_text, ignore.case = TRUE),
  is_fgm = is_make_raw & !is_ft_type &
    !grepl("free throw", type_text, ignore.case = TRUE),
  
  # FT
  is_fta = is_ft_type & is_shot_raw,
  is_ftm = is_ft_type & is_make_raw,
  
  # 3PT
  is_3pa = is_shot_raw & is_3pt_zone & !is_ft_type,
  is_3pm = is_make_raw & is_3pt_zone & !is_ft_type &
    !is.na(score_val_i) & score_val_i == 3L,
  
  # 2PT (non-FT, non-3PT shot)
  is_2pa = is_shot_raw & !is_ft_type & !is_3pt_zone,
  is_2pm = is_make_raw & !is_ft_type & !is_3pt_zone &
    !is.na(score_val_i) & score_val_i == 2L,
  
  # Pull-up FG
  is_pu_fga = is_shot_raw & is_pu & !is_ft_type,
  is_pu_fgm = is_make_raw & is_pu & !is_ft_type &
    !is.na(score_val_i) & score_val_i > 0L,
  
  # Mid-range
  is_midr_fga = is_shot_raw & is_midr_zone,
  is_midr_fgm = is_make_raw & is_midr_zone & !is.na(score_val_i) & score_val_i > 0L,
  
  # Rim
  is_rim_fga = is_shot_raw & is_rim_zone,
  is_rim_fgm = is_make_raw & is_rim_zone & !is.na(score_val_i) & score_val_i > 0L,
  
  # Above-the-break 3
  is_atb3_fga = is_shot_raw & is_atb3_zone,
  is_atb3_fgm = is_make_raw & is_atb3_zone & !is.na(score_val_i) & score_val_i == 3L,
  
  # Score diff (team relative)
  score_diff = abs(home_score - away_score)
)]

# 3PT / Pull-Up / Mid / Rim point values
nbapbp_df[, `:=`(
  pts_3pm    = fifelse(is_3pm,    3L * as.integer(is_3pm), 0L),
  pts_pu     = fifelse(is_pu_fgm, score_val_i, 0L),
  pts_midr   = fifelse(is_midr_fgm, score_val_i, 0L),
  pts_rim    = fifelse(is_rim_fgm,  score_val_i, 0L),
  pts_atb3   = fifelse(is_atb3_fgm, 3L * as.integer(is_atb3_fgm), 0L),
  pts_2pm    = fifelse(is_2pm, 2L * as.integer(is_2pm), 0L)
)]

# ─────────────────────────────────────────────────────────────────────────────
# 8. Starting lineups (replaces Section 4)
# ─────────────────────────────────────────────────────────────────────────────
starting_lineups <- nbapbp_df[qtr == 1][
  order(game_id, -start_quarter_seconds_remaining, sequence_number)
][, .(starting_LINEUP_KEY_10M = LINEUP_KEY_10M[1L]), by = .(game_id)]

# ─────────────────────────────────────────────────────────────────────────────
# 9. Tie transitions per play (replaces TIE section)
# ─────────────────────────────────────────────────────────────────────────────
setorder(nbapbp_df, game_id, qtr, sequence_number)
nbapbp_df[, prev_diff := shift(home_diff, type = "lag"), by = .(game_id, qtr)]
nbapbp_df[, tie_in := (!is.na(home_diff) & home_diff == 0L) &
            (!is.na(prev_diff) & prev_diff != 0L)]

# ─────────────────────────────────────────────────────────────────────────────
# 10. Game-state flags (replaces Lead/Trail/Neut section)
#     Computed for BOTH home and away perspectives
# ─────────────────────────────────────────────────────────────────────────────
nbapbp_df[, `:=`(
  HOME_LEAD  = as.integer(!is.na(home_diff) & home_diff > 0L),
  HOME_TRAIL = as.integer(!is.na(home_diff) & home_diff < 0L),
  HOME_NEUT  = as.integer(!is.na(home_diff) & home_diff == 0L),
  AWAY_LEAD  = as.integer(!is.na(home_diff) & home_diff < 0L),
  AWAY_TRAIL = as.integer(!is.na(home_diff) & home_diff > 0L),
  AWAY_NEUT  = as.integer(!is.na(home_diff) & home_diff == 0L)
)]

message("[✓] All row-level flags computed in one pass.")

# ─────────────────────────────────────────────────────────────────────────────
# 11. MASTER AGGREGATION: one dcast per group key — HOME perspective
#     Keys: game_id, home_team_id (ESPN_TEAM_ID), LINEUP_KEY_10M, qtr
# ─────────────────────────────────────────────────────────────────────────────

# We need TWO perspectives: home team stats and away team stats.
# Build a long "team-perspective" table, then dcast once.

# ── 11a. Home-team-perspective rows ──────────────────────────────────────────
home_long <- nbapbp_df[qtr %in% 1:6 & !is.na(ESPN_TEAM_ID_ACT)][
  ESPN_TEAM_ID_ACT == home_team_id,
  .(
    game_id,
    ESPN_TEAM_ID      = home_team_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY_10M,
    qtr,
    score_value        = score_val_i,
    stint_seconds,
    tie_in,
    POSSESSION_EVENT,
    HOME_LEAD, HOME_TRAIL, HOME_NEUT,
    OFFTEAM_SUCCESS, DEFTEAM_SUCCESS,
    is_fga, is_fgm,
    is_fta, is_ftm,
    is_2pa, is_2pm,
    is_3pa, is_3pm, pts_3pm,
    is_pu_fga, is_pu_fgm, pts_pu,
    is_midr_fga, is_midr_fgm, pts_midr,
    is_rim_fga, is_rim_fgm, pts_rim,
    is_atb3_fga, is_atb3_fgm, pts_atb3,
    pts_2pm,
    score_diff, t_sec,
    is_shot_raw, is_make_raw,
    LEAD_FLAG = HOME_LEAD,
    TRAIL_FLAG = HOME_TRAIL,
    NEUT_FLAG  = HOME_NEUT
  )
]

# ── 11b. Away-team-perspective rows ──────────────────────────────────────────
away_long <- nbapbp_df[qtr %in% 1:6 & !is.na(ESPN_TEAM_ID_ACT)][
  ESPN_TEAM_ID_ACT == away_team_id,
  .(
    game_id,
    ESPN_TEAM_ID      = away_team_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY_10M,
    qtr,
    score_value        = score_val_i,
    stint_seconds,
    tie_in,
    POSSESSION_EVENT,
    HOME_LEAD = AWAY_LEAD, HOME_TRAIL = AWAY_TRAIL, HOME_NEUT = AWAY_NEUT,
    OFFTEAM_SUCCESS, DEFTEAM_SUCCESS,
    is_fga, is_fgm,
    is_fta, is_ftm,
    is_2pa, is_2pm,
    is_3pa, is_3pm, pts_3pm,
    is_pu_fga, is_pu_fgm, pts_pu,
    is_midr_fga, is_midr_fgm, pts_midr,
    is_rim_fga, is_rim_fgm, pts_rim,
    is_atb3_fga, is_atb3_fgm, pts_atb3,
    pts_2pm,
    score_diff, t_sec,
    is_shot_raw, is_make_raw,
    LEAD_FLAG = AWAY_LEAD,
    TRAIL_FLAG = AWAY_TRAIL,
    NEUT_FLAG  = AWAY_NEUT
  )
]

team_long <- rbindlist(list(home_long, away_long))
rm(home_long, away_long)

# ── 11c. Per-quarter aggregation (all metrics in one shot) ───────────────────
agg_qtr <- team_long[, .(
  # Time
  LINEUP_IN           = sum(stint_seconds, na.rm = TRUE),
  
  # Scoring
  LINEUP_PTS          = sum(score_value, na.rm = TRUE),
  
  # Tie changes
  LINEUP_TIE          = sum(as.integer(tie_in), na.rm = TRUE),
  
  # Possessions
  LINEUP_POSS         = sum(POSSESSION_EVENT, na.rm = TRUE),
  LINEUP_POSS_LEAD    = sum(POSSESSION_EVENT * LEAD_FLAG,  na.rm = TRUE),
  LINEUP_POSS_TRAIL   = sum(POSSESSION_EVENT * TRAIL_FLAG, na.rm = TRUE),
  LINEUP_POSS_NEUT    = sum(POSSESSION_EVENT * NEUT_FLAG,  na.rm = TRUE),
  LINEUP_TOT_POSS_TIME = sum(stint_seconds, na.rm = TRUE),
  LINEUP_AVG_POSS_TIME = mean(stint_seconds, na.rm = TRUE),
  
  # OFF/DEF success
  LINEUP_POSS_OFF_SUCC = sum(OFFTEAM_SUCCESS, na.rm = TRUE),
  LINEUP_POSS_DEF_SUCC = sum(DEFTEAM_SUCCESS, na.rm = TRUE),
  
  # FG
  LINEUP_FGA = sum(as.integer(is_fga), na.rm = TRUE),
  LINEUP_FGM = sum(as.integer(is_fgm), na.rm = TRUE),
  
  # FT
  LINEUP_FTA = sum(as.integer(is_fta), na.rm = TRUE),
  LINEUP_FTM = sum(as.integer(is_ftm), na.rm = TRUE),
  
  # 2PT
  LINEUP_2PTA = sum(as.integer(is_2pa), na.rm = TRUE),
  LINEUP_2PTM = sum(as.integer(is_2pm), na.rm = TRUE),
  LINEUP_2PT_PTS = sum(pts_2pm, na.rm = TRUE),
  
  # 3PT
  LINEUP_3PTA     = sum(as.integer(is_3pa), na.rm = TRUE),
  LINEUP_3PTM     = sum(as.integer(is_3pm), na.rm = TRUE),
  LINEUP_3PT_PTS  = sum(pts_3pm, na.rm = TRUE),
  
  # Pull-up
  LINEUP_PU_FGA = sum(as.integer(is_pu_fga), na.rm = TRUE),
  LINEUP_PU_FGM = sum(as.integer(is_pu_fgm), na.rm = TRUE),
  LINEUP_PU_PTS = sum(pts_pu, na.rm = TRUE),
  
  # Mid-range
  L_MIDR_FGA = sum(as.integer(is_midr_fga), na.rm = TRUE),
  L_MIDR_FGM = sum(as.integer(is_midr_fgm), na.rm = TRUE),
  L_MIDR_PTS = sum(pts_midr, na.rm = TRUE),
  
  # Rim
  L_RIM_FGA = sum(as.integer(is_rim_fga), na.rm = TRUE),
  L_RIM_FGM = sum(as.integer(is_rim_fgm), na.rm = TRUE),
  L_RIM_PTS = sum(pts_rim, na.rm = TRUE),
  
  # ATB 3PT
  L_ATB_3PT_FGA = sum(as.integer(is_atb3_fga), na.rm = TRUE),
  L_ATB_3PT_FGM = sum(as.integer(is_atb3_fgm), na.rm = TRUE),
  L_ATB_3PT_PTS = sum(pts_atb3, na.rm = TRUE),
  
  # Clutch windows (score_diff <= 7)
  CLTH_ALL_FGA    = sum(as.integer(is_fga)     * as.integer(!is.na(score_diff) & score_diff <= 7), na.rm = TRUE),
  CLTH_ALL_FGM    = sum(as.integer(is_fgm)     * as.integer(!is.na(score_diff) & score_diff <= 7), na.rm = TRUE),
  CLTH_3PT_FGA    = sum(as.integer(is_3pa)     * as.integer(!is.na(score_diff) & score_diff <= 7), na.rm = TRUE),
  CLTH_3PT_FGM    = sum(as.integer(is_3pm)     * as.integer(!is.na(score_diff) & score_diff <= 7), na.rm = TRUE),
  
  # Clutch L1M (t_sec <= 60)
  CLTH_L1M_FGA    = sum(as.integer(is_fga) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 60L), na.rm = TRUE),
  CLTH_L1M_FGM    = sum(as.integer(is_fgm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 60L), na.rm = TRUE),
  CLTH_L1M_3PA    = sum(as.integer(is_3pa) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 60L), na.rm = TRUE),
  CLTH_L1M_3PM    = sum(as.integer(is_3pm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 60L), na.rm = TRUE),
  
  # Clutch L3M (t_sec <= 180)
  CLTH_L3M_FGA    = sum(as.integer(is_fga) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 180L), na.rm = TRUE),
  CLTH_L3M_FGM    = sum(as.integer(is_fgm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 180L), na.rm = TRUE),
  CLTH_L3M_3PA    = sum(as.integer(is_3pa) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 180L), na.rm = TRUE),
  CLTH_L3M_3PM    = sum(as.integer(is_3pm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 180L), na.rm = TRUE),
  
  # Clutch L5M (t_sec <= 300)
  CLTH_L5M_FGA    = sum(as.integer(is_fga) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 300L), na.rm = TRUE),
  CLTH_L5M_FGM    = sum(as.integer(is_fgm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 300L), na.rm = TRUE),
  CLTH_L5M_3PA    = sum(as.integer(is_3pa) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 300L), na.rm = TRUE),
  CLTH_L5M_3PM    = sum(as.integer(is_3pm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 300L), na.rm = TRUE),
  
  # Clutch L10M (t_sec <= 600)
  CLTH_L10M_FGA   = sum(as.integer(is_fga) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 600L), na.rm = TRUE),
  CLTH_L10M_FGM   = sum(as.integer(is_fgm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 600L), na.rm = TRUE),
  CLTH_L10M_3PA   = sum(as.integer(is_3pa) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 600L), na.rm = TRUE),
  CLTH_L10M_3PM   = sum(as.integer(is_3pm) * as.integer(!is.na(score_diff) & score_diff <= 7 & !is.na(t_sec) & t_sec <= 600L), na.rm = TRUE)
  
), by = .(game_id, ESPN_TEAM_ID, ESPN_HOME_TEAM_ID, ESPN_AWAY_TEAM_ID, LINEUP_KEY_10M, qtr)]

# ── 11d. Also need game_date and player name columns — pull from nbapbp_df ──
player_meta <- nbapbp_df[
  !is.na(LINEUP_KEY_10M) & LINEUP_KEY_10M != "NA|NA|NA|NA|NA || NA|NA|NA|NA|NA",
  .(
    game_date,
    home_P1, home_P2, home_P3, home_P4, home_P5,
    away_P1, away_P2, away_P3, away_P4, away_P5,
    home_P1_espn_id, home_P2_espn_id, home_P3_espn_id, home_P4_espn_id, home_P5_espn_id,
    away_P1_espn_id, away_P2_espn_id, away_P3_espn_id, away_P4_espn_id, away_P5_espn_id
  ),
  keyby = .(game_id = game_id, LINEUP_KEY_10M)
][, .SD[1L], by = .(game_id, LINEUP_KEY_10M)]

message("[✓] Per-quarter aggregation complete.")

# ─────────────────────────────────────────────────────────────────────────────
# 12. DCAST: pivot agg_qtr from long (per qtr) to wide (Q1..Q6 columns)
#     All metrics in ONE dcast call per metric group
# ─────────────────────────────────────────────────────────────────────────────
keys <- c("game_id","ESPN_TEAM_ID","ESPN_HOME_TEAM_ID","ESPN_AWAY_TEAM_ID","LINEUP_KEY_10M")

# Metric columns to pivot — all will get _Q1 through _Q6 suffixes
metrics <- c(
  "LINEUP_IN","LINEUP_PTS","LINEUP_TIE",
  "LINEUP_POSS","LINEUP_POSS_LEAD","LINEUP_POSS_TRAIL","LINEUP_POSS_NEUT",
  "LINEUP_TOT_POSS_TIME","LINEUP_AVG_POSS_TIME",
  "LINEUP_POSS_OFF_SUCC","LINEUP_POSS_DEF_SUCC",
  "LINEUP_FGA","LINEUP_FGM",
  "LINEUP_FTA","LINEUP_FTM",
  "LINEUP_2PTA","LINEUP_2PTM","LINEUP_2PT_PTS",
  "LINEUP_3PTA","LINEUP_3PTM","LINEUP_3PT_PTS",
  "LINEUP_PU_FGA","LINEUP_PU_FGM","LINEUP_PU_PTS",
  "L_MIDR_FGA","L_MIDR_FGM","L_MIDR_PTS",
  "L_RIM_FGA","L_RIM_FGM","L_RIM_PTS",
  "L_ATB_3PT_FGA","L_ATB_3PT_FGM","L_ATB_3PT_PTS",
  "CLTH_ALL_FGA","CLTH_ALL_FGM",
  "CLTH_3PT_FGA","CLTH_3PT_FGM",
  "CLTH_L1M_FGA","CLTH_L1M_FGM","CLTH_L1M_3PA","CLTH_L1M_3PM",
  "CLTH_L3M_FGA","CLTH_L3M_FGM","CLTH_L3M_3PA","CLTH_L3M_3PM",
  "CLTH_L5M_FGA","CLTH_L5M_FGM","CLTH_L5M_3PA","CLTH_L5M_3PM",
  "CLTH_L10M_FGA","CLTH_L10M_FGM","CLTH_L10M_3PA","CLTH_L10M_3PM"
)

# Single dcast — produces one wide row per (game+team+lineup) with Q1..Q6 columns
wide_qtr <- dcast(
  agg_qtr,
  formula   = game_id + ESPN_TEAM_ID + ESPN_HOME_TEAM_ID + ESPN_AWAY_TEAM_ID + LINEUP_KEY_10M ~ qtr,
  value.var = metrics,
  fill      = 0,
  fun.aggregate = sum
)

# Rename: dcast produces e.g. "LINEUP_IN_1" → we want "LINEUP_IN_Q1"
old_names <- names(wide_qtr)
new_names <- gsub("_(\\d)$", "_Q\\1", old_names)
setnames(wide_qtr, old_names, new_names)

message("[✓] dcast pivot complete.")

# ─────────────────────────────────────────────────────────────────────────────
# 13. CGS (complete-game) totals — sum across all quarters
# ─────────────────────────────────────────────────────────────────────────────
cgs_metrics <- setdiff(metrics, c("LINEUP_AVG_POSS_TIME","FIRST_APPEARANCE"))

agg_cgs <- agg_qtr[, lapply(.SD, sum, na.rm = TRUE),
                   by = keys,
                   .SDcols = cgs_metrics]
# Rename CGS columns
setnames(agg_cgs,
         cgs_metrics,
         paste0(cgs_metrics, "_CGS"))

# Special case: AVG_POSS_TIME_CGS = mean over all rows (not sum of quarterly means)
avg_cgs <- team_long[, .(LINEUP_AVG_POSS_TIME_CGS = mean(stint_seconds, na.rm = TRUE)),
                     by = keys]

# FIRST_APPEARANCE per lineup: max start_quarter_seconds_remaining across all plays
first_app <- nbapbp_df[
  !is.na(LINEUP_KEY_10M) & LINEUP_KEY_10M != "NA|NA|NA|NA|NA || NA|NA|NA|NA|NA",
  .(FIRST_APPEARANCE = suppressWarnings(max(start_quarter_seconds_remaining, na.rm = TRUE))),
  by = .(game_id, LINEUP_KEY_10M)
]
first_app_home <- merge(
  first_app,
  unique(nbapbp_df[, .(game_id, LINEUP_KEY_10M, ESPN_TEAM_ID = home_team_id,
                       ESPN_HOME_TEAM_ID = home_team_id, ESPN_AWAY_TEAM_ID = away_team_id)]),
  by = c("game_id","LINEUP_KEY_10M"), all.x = TRUE
)
first_app_away <- merge(
  first_app,
  unique(nbapbp_df[, .(game_id, LINEUP_KEY_10M, ESPN_TEAM_ID = away_team_id,
                       ESPN_HOME_TEAM_ID = home_team_id, ESPN_AWAY_TEAM_ID = away_team_id)]),
  by = c("game_id","LINEUP_KEY_10M"), all.x = TRUE
)
first_app <- unique(rbindlist(list(first_app_home, first_app_away)))
rm(first_app_home, first_app_away)

agg_cgs <- merge(agg_cgs, avg_cgs,  by = keys, all.x = TRUE)
agg_cgs <- merge(agg_cgs, first_app, by = keys, all.x = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# 14. HAS_Q5 / HAS_Q6 flags (OT indicators per game)
# ─────────────────────────────────────────────────────────────────────────────
has_ot <- nbapbp_df[, .(HAS_Q5 = as.integer(any(qtr == 5)),
                        HAS_Q6 = as.integer(any(qtr == 6))),
                    by = .(game_id)]

# ─────────────────────────────────────────────────────────────────────────────
# 15. Tie section: TIE_CHNG per lineup per game (10M, not per-team)
#     Original script uses game_id + LINEUP_KEY_10M (not team_id)
# ─────────────────────────────────────────────────────────────────────────────
tie_qtr_dt <- nbapbp_df[qtr %in% 1:6,
                        .(LINEUP_TIE_CHNG = sum(as.integer(tie_in), na.rm = TRUE)),
                        by = .(game_id, LINEUP_KEY_10M, qtr)
]
tie_wide <- dcast(tie_qtr_dt,
                  game_id + LINEUP_KEY_10M ~ qtr,
                  value.var = "LINEUP_TIE_CHNG",
                  fill = 0L,
                  fun.aggregate = sum)
setnames(tie_wide, grep("^\\d$", names(tie_wide), value = TRUE),
         paste0("LINEUP_TIE_CHNG_Q", grep("^\\d$", names(tie_wide), value = TRUE)))
tie_cgs <- nbapbp_df[,
                     .(LINEUP_TIE_CHNG_CGS = sum(as.integer(tie_in), na.rm = TRUE)),
                     by = .(game_id, LINEUP_KEY_10M)
]
tie_df <- merge(tie_wide, tie_cgs, by = c("game_id","LINEUP_KEY_10M"), all = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# 16. Assemble lineup_team_level_summary_df (replaces Sections 5–9 + all joins)
# ─────────────────────────────────────────────────────────────────────────────
setkey(wide_qtr, game_id, ESPN_TEAM_ID, ESPN_HOME_TEAM_ID, ESPN_AWAY_TEAM_ID, LINEUP_KEY_10M)
setkey(agg_cgs,  game_id, ESPN_TEAM_ID, ESPN_HOME_TEAM_ID, ESPN_AWAY_TEAM_ID, LINEUP_KEY_10M)

lineup_dt <- merge(wide_qtr, agg_cgs, by = keys, all = TRUE)

# Merge player meta (names + IDs)
setkey(player_meta, game_id, LINEUP_KEY_10M)
lineup_dt <- merge(lineup_dt, player_meta, by = c("game_id","LINEUP_KEY_10M"), all.x = TRUE)

# Merge starting lineup flag
lineup_dt <- merge(lineup_dt, starting_lineups, by = "game_id", all.x = TRUE)
lineup_dt[, LINEUP_TYPE := fifelse(LINEUP_KEY_10M == starting_LINEUP_KEY_10M, "STARTING", "BENCH")]
lineup_dt[, starting_LINEUP_KEY_10M := NULL]

# Merge HAS_Q5 / HAS_Q6
lineup_dt <- merge(lineup_dt, has_ot, by = "game_id", all.x = TRUE)
lineup_dt[, `:=`(HAS_Q5 = fifelse(is.na(HAS_Q5), 0L, HAS_Q5),
                 HAS_Q6 = fifelse(is.na(HAS_Q6), 0L, HAS_Q6))]

# Merge tie changes (game_id + LINEUP_KEY_10M — no team_id, matches original)
lineup_dt <- merge(lineup_dt, tie_df, by = c("game_id","LINEUP_KEY_10M"), all.x = TRUE)
# Coalesce tie NA → 0
tie_cols <- grep("^LINEUP_TIE_CHNG", names(lineup_dt), value = TRUE)
for (col in tie_cols) lineup_dt[is.na(get(col)), (col) := 0L]

# Rename game_id → ESPN_GAME_ID, game_date
setnames(lineup_dt, "game_id", "ESPN_GAME_ID")
if ("game_date" %in% names(lineup_dt)) setnames(lineup_dt, "game_date", "GAME_DATE")

# ESPN_TEAM_ID columns to character (for downstream joins)
lineup_dt[, `:=`(
  ESPN_TEAM_ID      = as.character(ESPN_TEAM_ID),
  ESPN_HOME_TEAM_ID = as.character(ESPN_HOME_TEAM_ID),
  ESPN_AWAY_TEAM_ID = as.character(ESPN_AWAY_TEAM_ID)
)]
lineup_dt[, QTR := "CGS"]

# ── Rename Q-columns to match original column names exactly ──────────────────
# Scoring: LINEUP_IN_Q1..Q6 → LINEUP_IN_Q1..Q6 (already correct from dcast rename)
# CGS naming: e.g. LINEUP_IN_CGS (already set from agg_cgs rename)

# Fix LINEUP_IN columns → LINEUP_IN_Q* naming already done; add _CGS alias
if (!"LINEUP_IN_CGS" %in% names(lineup_dt) && "LINEUP_IN_Q1" %in% names(lineup_dt)) {
  lineup_dt[, LINEUP_IN_CGS :=
              rowSums(.SD, na.rm = TRUE),
            .SDcols = paste0("LINEUP_IN_Q", 1:6)]
}

# Add LINEUP_ID per game (row_number by game sorted by FIRST_APPEARANCE)
setorder(lineup_dt, ESPN_GAME_ID, FIRST_APPEARANCE)
lineup_dt[, LINEUP_ID := seq_len(.N), by = .(ESPN_GAME_ID)]

# ─────────────────────────────────────────────────────────────────────────────
# 17. Map team short names (replaces Section 9 triple-join)
# ─────────────────────────────────────────────────────────────────────────────
team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"
team_map_dt <- fread(team_map_path, colClasses = "character",
                     select = c("espn_team_id","team_short_name"))
setnames(team_map_dt, c("TEAM_ID","TEAM_SHORT"))
setkey(team_map_dt, TEAM_ID)

lineup_dt <- merge(lineup_dt, team_map_dt, by.x = "ESPN_TEAM_ID",      by.y = "TEAM_ID", all.x = TRUE); setnames(lineup_dt, "TEAM_SHORT", "TEAM")
lineup_dt <- merge(lineup_dt, team_map_dt, by.x = "ESPN_HOME_TEAM_ID", by.y = "TEAM_ID", all.x = TRUE); setnames(lineup_dt, "TEAM_SHORT", "HOME_TEAM")
lineup_dt <- merge(lineup_dt, team_map_dt, by.x = "ESPN_AWAY_TEAM_ID", by.y = "TEAM_ID", all.x = TRUE); setnames(lineup_dt, "TEAM_SHORT", "AWAY_TEAM")
rm(team_map_dt)

# ─────────────────────────────────────────────────────────────────────────────
# 18. Derived rate / pct metrics (replaces all the mutate() sections)
#     Using data.table's vectorized assignment — much faster than dplyr mutate
# ─────────────────────────────────────────────────────────────────────────────
safe_div <- function(n, d) fifelse(d > 0 & !is.na(d), n / d, NA_real_)

for (q in c(paste0("Q", 1:6), "CGS")) {
  fga  <- paste0("LINEUP_FGA_",   q)
  fgm  <- paste0("LINEUP_FGM_",   q)
  fta  <- paste0("LINEUP_FTA_",   q)
  ftm  <- paste0("LINEUP_FTM_",   q)
  pts  <- paste0("LINEUP_PTS_",   q)
  poss <- paste0("LINEUP_POSS_",  q)
  
  if (all(c(fga, fgm) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("LINEUP_FG_PCT_",  q)) := safe_div(get(fgm), get(fga))]
  }
  if (all(c(fta, ftm) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("LINEUP_FT_PCT_",  q)) := safe_div(get(ftm), get(fta))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_FTR_",   q)) := safe_div(get(fta), get(fga))]
  }
  
  # 2PT rates
  a2 <- paste0("LINEUP_2PTA_", q); m2 <- paste0("LINEUP_2PTM_", q); p2 <- paste0("LINEUP_2PT_PTS_", q)
  if (all(c(a2, m2) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("LINEUP_2PT_PCT_",     q)) := safe_div(get(m2), get(a2))]
    lineup_dt[, (paste0("LINEUP_2PT_EFG_PCT_", q)) := safe_div(get(m2), get(a2))]  # eFG == FG% for 2s
    lineup_dt[, (paste0("LINEUP_2PT_PTS_",     q)) := 2L * get(m2)]
    lineup_dt[, (paste0("LINEUP_2PT_PPP_",     q)) := safe_div(2L * get(m2), get(a2))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_2PT_RATE_",    q)) := safe_div(get(a2), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_2PT_PTSHR_",   q)) := safe_div(2L * get(m2), get(pts))]
  }
  
  # 3PT rates
  a3 <- paste0("LINEUP_3PTA_", q); m3 <- paste0("LINEUP_3PTM_", q); p3 <- paste0("LINEUP_3PT_PTS_", q)
  if (all(c(a3, m3) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("LINEUP_3PT_PCT_",     q)) := safe_div(get(m3), get(a3))]
    lineup_dt[, (paste0("LINEUP_3PT_EFG_PCT_", q)) := safe_div(1.5 * get(m3), get(a3))]
    lineup_dt[, (paste0("LINEUP_3PT_PPP_",     q)) := safe_div(get(p3), get(a3))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_3PT_RATE_",    q)) := safe_div(get(a3), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_3PT_PTSHR_",   q)) := safe_div(get(p3), get(pts))]
  }
  
  # Pull-up rates
  pu_a <- paste0("LINEUP_PU_FGA_", q); pu_m <- paste0("LINEUP_PU_FGM_", q); pu_p <- paste0("LINEUP_PU_PTS_", q)
  if (all(c(pu_a, pu_m) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("LINEUP_PU_FG_PCT_",  q)) := safe_div(get(pu_m), get(pu_a))]
    lineup_dt[, (paste0("LINEUP_PU_EFG_PCT_", q)) := safe_div(get(pu_m), get(pu_a))]
    lineup_dt[, (paste0("LINEUP_PU_PPP_",     q)) := safe_div(get(pu_p), get(pu_a))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_PU_RATE_",    q)) := safe_div(get(pu_a), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("LINEUP_PU_PTSHR_",   q)) := safe_div(get(pu_p), get(pts))]
  }
  
  # Mid-range rates
  mr_a <- paste0("L_MIDR_FGA_", q); mr_m <- paste0("L_MIDR_FGM_", q); mr_p <- paste0("L_MIDR_PTS_", q)
  if (all(c(mr_a, mr_m) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("L_MIDR_PCT_",     q)) := safe_div(get(mr_m), get(mr_a))]
    lineup_dt[, (paste0("L_MIDR_EFG_PCT_", q)) := safe_div(get(mr_m), get(mr_a))]
    lineup_dt[, (paste0("L_MIDR_PPP_",     q)) := safe_div(get(mr_p), get(mr_a))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("L_MIDR_RATE_",    q)) := safe_div(get(mr_a), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("L_MIDR_PTSHR_",   q)) := safe_div(get(mr_p), get(pts))]
  }
  
  # Rim rates
  ri_a <- paste0("L_RIM_FGA_", q); ri_m <- paste0("L_RIM_FGM_", q); ri_p <- paste0("L_RIM_PTS_", q)
  if (all(c(ri_a, ri_m) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("L_RIM_PCT_",     q)) := safe_div(get(ri_m), get(ri_a))]
    lineup_dt[, (paste0("L_RIM_PPP_",     q)) := safe_div(get(ri_p), get(ri_a))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("L_RIM_RATE_",   q)) := safe_div(get(ri_a), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("L_RIM_PTSHR_",  q)) := safe_div(get(ri_p), get(pts))]
  }
  
  # ATB 3PT rates
  ab_a <- paste0("L_ATB_3PT_FGA_", q); ab_m <- paste0("L_ATB_3PT_FGM_", q); ab_p <- paste0("L_ATB_3PT_PTS_", q)
  if (all(c(ab_a, ab_m) %in% names(lineup_dt))) {
    lineup_dt[, (paste0("L_ATB_3PT_PCT_",     q)) := safe_div(get(ab_m), get(ab_a))]
    lineup_dt[, (paste0("L_ATB_3PT_EFG_PCT_", q)) := safe_div(1.5 * get(ab_m), get(ab_a))]
    lineup_dt[, (paste0("L_ATB_3PT_PPP_",     q)) := safe_div(get(ab_p), get(ab_a))]
    if (fga %in% names(lineup_dt))
      lineup_dt[, (paste0("L_ATB_3PT_RATE_",    q)) := safe_div(get(ab_a), get(fga))]
    if (pts %in% names(lineup_dt))
      lineup_dt[, (paste0("L_ATB_3PT_PTSHR_",   q)) := safe_div(get(ab_p), get(pts))]
  }
  
  # Clutch FG%
  for (win in c("L1M","L3M","L5M","L10M")) {
    cl_a <- paste0("CLTH_", win, "_FGA_", q); cl_m <- paste0("CLTH_", win, "_FGM_", q)
    c3_a <- paste0("CLTH_3PT_", win, "_FGA_", q); c3_m <- paste0("CLTH_3PT_", win, "_FGM_", q)
    if (all(c(cl_a, cl_m) %in% names(lineup_dt)))
      lineup_dt[, (paste0("L_CLTH_", win, "_PCT_", q)) := safe_div(get(cl_m), get(cl_a))]
    if (all(c(c3_a, c3_m) %in% names(lineup_dt)))
      lineup_dt[, (paste0("L_CLTH_3PT_", win, "_PCT_", q)) := safe_div(get(c3_m), get(c3_a))]
  }
}

# OT masking: zero out Q5/Q6 cells where that OT period didn't happen
for (col in grep("_Q5$", names(lineup_dt), value = TRUE)) {
  lineup_dt[HAS_Q5 == 0L, (col) := 0L]
}
for (col in grep("_Q6$", names(lineup_dt), value = TRUE)) {
  lineup_dt[HAS_Q6 == 0L, (col) := 0L]
}

# ─────────────────────────────────────────────────────────────────────────────
# 19. Coerce back to data.frame for compatibility with remaining dplyr sections
# ─────────────────────────────────────────────────────────────────────────────
lineup_team_level_summary_df <- as.data.frame(lineup_dt)

# Also ensure nbapbp_df stays as a data.table for remaining sections
# (5-man lineup section, runs section, etc. still use dplyr on nbapbp_df —
#  data.table is a data.frame subclass so dplyr works on it directly)

rm(wide_qtr, agg_cgs, agg_qtr, team_long, player_meta, tie_df, tie_wide,
   tie_qtr_dt, tie_cgs, first_app, avg_cgs, has_ot, lineup_dt, starting_lineups)

t1 <- proc.time()
message(sprintf("[✓] OPTIMIZED 10-man aggregation complete in %.1f seconds.", (t1 - t0)["elapsed"]))

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: OPTIMIZED SINGLE-PASS PRE-AGGREGATION ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ─────────────────────────────────────────────────────────────────────────────
# ▶ CONTINUE from here with the rest of your original script, starting at:
#   === START: OFFTEAM and DEFTEAM SUCCESS Play 10 MAN Identification ====
#   which you can now DELETE (flags already computed above).
#
#   Then delete all sections through to (and including):
#   === END: Clutch 3PT Field Goal Data Aggregation Section (10M LINEUP) ====
#
#   KEEP everything from the Runs section onward unchanged.
# ─────────────────────────────────────────────────────────────────────────────
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
# === START: (10M) Steals & Blocks Data Aggregation Section ====
# Credits STL/BLK to OPPONENT team (benefiting defense)
# Joins into: lineup_team_level_summary_df
# Creates (Q1..Q6 + CGS):
#   L10_STL_*, L10_BLK_*
#   L10_STL_PCT_*  (per 10M lineup possessions)
#   L10_BLK_PCT_*  (per opponent 10M lineup FGA)
# ============================================================================

stopifnot(exists("nbapbp_df"), exists("lineup_team_level_summary_df"))

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids"
) %in% names(nbapbp_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Base: opponent (benefiting) team + 10M key on floor -----------------------------
hustle_base_10m <- nbapbp_df %>%
  dplyr::mutate(
    qtr = to_int(qtr),
    
    # Benefiting defense team
    OPP_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    
    # Opponent 5-man lineup on floor (benefiting side)
    OPP_LINEUP_5 = dplyr::if_else(
      team_id == home_team_id,
      lineup_away_ids,   # committing = home → benefiting = away lineup
      lineup_home_ids    # committing = away → benefiting = home lineup
    ),
    
    # Build 10M environment key for the event row
    LINEUP_KEY_10M = paste(lineup_home_ids, lineup_away_ids, sep = " || "),
    
    txt = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(OPP_TEAM_ID),
    !is.na(OPP_LINEUP_5),
    !is.na(LINEUP_KEY_10M),
    LINEUP_KEY_10M != "NA|NA|NA|NA|NA || NA|NA|NA|NA|NA"
  )

# ---- Pattern definitions -------------------------------------------------------------
pat_steal <- stringr::regex("steal", ignore_case = TRUE)
pat_block <- stringr::regex("block", ignore_case = TRUE)

stl_df_10m <- hustle_base_10m %>% dplyr::filter(stringr::str_detect(txt, pat_steal))
blk_df_10m <- hustle_base_10m %>% dplyr::filter(stringr::str_detect(txt, pat_block))

# ---- Helper: tally by quarter + CGS (credited to benefiting team + 10M key) ----------
tally_hustle_10m <- function(df, name_prefix) {
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID, LINEUP_KEY_10M, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    tidyr::pivot_wider(
      names_from = qtr, values_from = N, values_fill = 0L, names_prefix = "Q"
    ) %>%
    dplyr::rename_with(~ paste0("L10_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID, LINEUP_KEY_10M) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("L10_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

res_stl_10m <- tally_hustle_10m(stl_df_10m, "STL")
res_blk_10m <- tally_hustle_10m(blk_df_10m, "BLK")

# ---- Join into lineup_team_level_summary_df -----------------------------------------
lineup_team_level_summary_df <- lineup_team_level_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID   = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID   = as.character(ESPN_TEAM_ID),
    LINEUP_KEY_10M = as.character(LINEUP_KEY_10M)
  ) %>%
  # Steals
  dplyr::left_join(
    res_stl_10m$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY_10M" = "LINEUP_KEY_10M")
  ) %>%
  dplyr::left_join(
    res_stl_10m$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY_10M" = "LINEUP_KEY_10M")
  ) %>%
  # Blocks
  dplyr::left_join(
    res_blk_10m$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY_10M" = "LINEUP_KEY_10M")
  ) %>%
  dplyr::left_join(
    res_blk_10m$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY_10M" = "LINEUP_KEY_10M")
  ) %>%
  # Fill NA → 0 for new count columns
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L10_(STL|BLK)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- Opponent denominators for BLK% (opponent FGA) ----------------------------------
# Opponent FGA within the same ESPN_GAME_ID + LINEUP_KEY_10M = sum(two teams) - own
lineup_team_level_summary_df <- lineup_team_level_summary_df %>%
  dplyr::group_by(ESPN_GAME_ID, LINEUP_KEY_10M) %>%
  dplyr::mutate(
    OPP_LINEUP_FGA_Q1  = sum(LINEUP_FGA_Q1,  na.rm = TRUE) - LINEUP_FGA_Q1,
    OPP_LINEUP_FGA_Q2  = sum(LINEUP_FGA_Q2,  na.rm = TRUE) - LINEUP_FGA_Q2,
    OPP_LINEUP_FGA_Q3  = sum(LINEUP_FGA_Q3,  na.rm = TRUE) - LINEUP_FGA_Q3,
    OPP_LINEUP_FGA_Q4  = sum(LINEUP_FGA_Q4,  na.rm = TRUE) - LINEUP_FGA_Q4,
    OPP_LINEUP_FGA_Q5  = sum(LINEUP_FGA_Q5,  na.rm = TRUE) - LINEUP_FGA_Q5,
    OPP_LINEUP_FGA_Q6  = sum(LINEUP_FGA_Q6,  na.rm = TRUE) - LINEUP_FGA_Q6,
    OPP_LINEUP_FGA_CGS = sum(LINEUP_FGA_CGS, na.rm = TRUE) - LINEUP_FGA_CGS
  ) %>%
  dplyr::ungroup()

# ---- Percentages --------------------------------------------------------------------
# STL%: steals / lineup possessions * 100
# BLK%: blocks / opponent lineup FGA * 100

pct_spans <- c("Q1","Q2","Q3","Q4","Q5","Q6","CGS")

for (s in pct_spans) {
  lineup_team_level_summary_df[[paste0("L10_STL_PCT_", s)]] <-
    100 * safe_div(
      lineup_team_level_summary_df[[paste0("L10_STL_", s)]],
      lineup_team_level_summary_df[[paste0("LINEUP_POSS_", s)]]
    )
  
  lineup_team_level_summary_df[[paste0("L10_BLK_PCT_", s)]] <-
    100 * safe_div(
      lineup_team_level_summary_df[[paste0("L10_BLK_", s)]],
      lineup_team_level_summary_df[[paste0("OPP_LINEUP_FGA_", s)]]
    )
}

rm(
  hustle_base_10m, stl_df_10m, blk_df_10m,
  res_stl_10m, res_blk_10m, tally_hustle_10m, pct_spans
)

message("[✓] 10M Steals & Blocks (counts + STL% + BLK%) computed and joined into lineup_team_level_summary_df.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: (10M) Steals & Blocks Data Aggregation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: OFF/DEF/NET RTG (LINEUP) — 10M lineup_team_level_summary_df ====
# Requires (already in lineup_team_level_summary_df):
#   LINEUP_PTS_Q1..Q6, LINEUP_PTS_CGS
#   LINEUP_POSS_Q1..Q6, LINEUP_POSS_CGS
# If you don't have LINEUP_POSS_* in 10M yet, compute it first (same POSSESSION_EVENT sum
# approach you used elsewhere) before running this.
#
# Produces:
#   L_OFF_RTG_Q1..Q6, L_OFF_RTG_CGS
#   L_DEF_RTG_Q1..Q6, L_DEF_RTG_CGS
#   L_NET_RTG_Q1..Q6, L_NET_RTG_CGS
#
# Uses opponent derivation via game sums (10 players on floor → two "sides" per game):
#   OPP_LINEUP_PTS_*  = sum(game) - own
#   OPP_LINEUP_POSS_* = sum(game) - own
# ============================================================================= #

stopifnot(exists("lineup_team_level_summary_df"))

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Create OT flags if you don't already have them in 10M --------------------------
# (Safe: only creates if missing)
if (!"HAS_Q5" %in% names(lineup_team_level_summary_df) ||
    !"HAS_Q6" %in% names(lineup_team_level_summary_df)) {
  
  lineup_team_level_summary_df <- lineup_team_level_summary_df %>%
    dplyr::group_by(ESPN_GAME_ID) %>%
    dplyr::mutate(
      HAS_Q5 = as.integer(any(LINEUP_IN_Q5 > 0, na.rm = TRUE)),
      HAS_Q6 = as.integer(any(LINEUP_IN_Q6 > 0, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

# ---- opponent derivation via game sums ---------------------------------------------
lineup_team_level_summary_df <- lineup_team_level_summary_df %>%
  dplyr::group_by(ESPN_GAME_ID) %>%
  dplyr::mutate(
    # Opponent points while 10-man environment is on floor
    OPP_LINEUP_PTS_Q1  = sum(LINEUP_PTS_Q1,  na.rm = TRUE) - LINEUP_PTS_Q1,
    OPP_LINEUP_PTS_Q2  = sum(LINEUP_PTS_Q2,  na.rm = TRUE) - LINEUP_PTS_Q2,
    OPP_LINEUP_PTS_Q3  = sum(LINEUP_PTS_Q3,  na.rm = TRUE) - LINEUP_PTS_Q3,
    OPP_LINEUP_PTS_Q4  = sum(LINEUP_PTS_Q4,  na.rm = TRUE) - LINEUP_PTS_Q4,
    OPP_LINEUP_PTS_Q5  = sum(LINEUP_PTS_Q5,  na.rm = TRUE) - LINEUP_PTS_Q5,
    OPP_LINEUP_PTS_Q6  = sum(LINEUP_PTS_Q6,  na.rm = TRUE) - LINEUP_PTS_Q6,
    OPP_LINEUP_PTS_CGS = sum(LINEUP_PTS_CGS, na.rm = TRUE) - LINEUP_PTS_CGS,
    
    # Opponent possessions while 10-man environment is on floor
    OPP_LINEUP_POSS_Q1  = sum(LINEUP_POSS_Q1,  na.rm = TRUE) - LINEUP_POSS_Q1,
    OPP_LINEUP_POSS_Q2  = sum(LINEUP_POSS_Q2,  na.rm = TRUE) - LINEUP_POSS_Q2,
    OPP_LINEUP_POSS_Q3  = sum(LINEUP_POSS_Q3,  na.rm = TRUE) - LINEUP_POSS_Q3,
    OPP_LINEUP_POSS_Q4  = sum(LINEUP_POSS_Q4,  na.rm = TRUE) - LINEUP_POSS_Q4,
    OPP_LINEUP_POSS_Q5  = sum(LINEUP_POSS_Q5,  na.rm = TRUE) - LINEUP_POSS_Q5,
    OPP_LINEUP_POSS_Q6  = sum(LINEUP_POSS_Q6,  na.rm = TRUE) - LINEUP_POSS_Q6,
    OPP_LINEUP_POSS_CGS = sum(LINEUP_POSS_CGS, na.rm = TRUE) - LINEUP_POSS_CGS
  ) %>%
  dplyr::ungroup()

# ---- Ratings -----------------------------------------------------------------------
lineup_team_level_summary_df <- lineup_team_level_summary_df %>%
  dplyr::mutate(
    # OFF RTG
    L_OFF_RTG_Q1  = 100 * safe_div(LINEUP_PTS_Q1,  LINEUP_POSS_Q1),
    L_OFF_RTG_Q2  = 100 * safe_div(LINEUP_PTS_Q2,  LINEUP_POSS_Q2),
    L_OFF_RTG_Q3  = 100 * safe_div(LINEUP_PTS_Q3,  LINEUP_POSS_Q3),
    L_OFF_RTG_Q4  = 100 * safe_div(LINEUP_PTS_Q4,  LINEUP_POSS_Q4),
    L_OFF_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, 100 * safe_div(LINEUP_PTS_Q5,  LINEUP_POSS_Q5), NA_real_),
    L_OFF_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, 100 * safe_div(LINEUP_PTS_Q6,  LINEUP_POSS_Q6), NA_real_),
    L_OFF_RTG_CGS = 100 * safe_div(LINEUP_PTS_CGS, LINEUP_POSS_CGS),
    
    # DEF RTG
    L_DEF_RTG_Q1  = 100 * safe_div(OPP_LINEUP_PTS_Q1,  OPP_LINEUP_POSS_Q1),
    L_DEF_RTG_Q2  = 100 * safe_div(OPP_LINEUP_PTS_Q2,  OPP_LINEUP_POSS_Q2),
    L_DEF_RTG_Q3  = 100 * safe_div(OPP_LINEUP_PTS_Q3,  OPP_LINEUP_POSS_Q3),
    L_DEF_RTG_Q4  = 100 * safe_div(OPP_LINEUP_PTS_Q4,  OPP_LINEUP_POSS_Q4),
    L_DEF_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, 100 * safe_div(OPP_LINEUP_PTS_Q5,  OPP_LINEUP_POSS_Q5), NA_real_),
    L_DEF_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, 100 * safe_div(OPP_LINEUP_PTS_Q6,  OPP_LINEUP_POSS_Q6), NA_real_),
    L_DEF_RTG_CGS = 100 * safe_div(OPP_LINEUP_PTS_CGS, OPP_LINEUP_POSS_CGS),
    
    # NET RTG
    L_NET_RTG_Q1  = L_OFF_RTG_Q1  - L_DEF_RTG_Q1,
    L_NET_RTG_Q2  = L_OFF_RTG_Q2  - L_DEF_RTG_Q2,
    L_NET_RTG_Q3  = L_OFF_RTG_Q3  - L_DEF_RTG_Q3,
    L_NET_RTG_Q4  = L_OFF_RTG_Q4  - L_DEF_RTG_Q4,
    L_NET_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, L_OFF_RTG_Q5 - L_DEF_RTG_Q5, NA_real_),
    L_NET_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, L_OFF_RTG_Q6 - L_DEF_RTG_Q6, NA_real_),
    L_NET_RTG_CGS = L_OFF_RTG_CGS - L_DEF_RTG_CGS
  )

message("[✓] 10M lineup_team_level_summary_df — OFF/DEF/NET RTG (Q1–Q6 OT-aware + CGS) complete.")
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: OFF/DEF/NET RTG (LINEUP) — 10M lineup_team_level_summary_df ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀






#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#       .oooooo.                                    .                    oooooooo      ooo        ooooo                            ooooo         o8o                                                     .oooooo..o                                                                                
#      d8P'  `Y8b                                 .o8                   dP"""""""      `88.       .888'                            `888'         `"'                                                    d8P'    `Y8                                                                                
#     888          oooo d8b  .ooooo.   .oooo.   .o888oo  .ooooo.       d88888b.         888b     d'888   .oooo.   ooo. .oo.         888         oooo  ooo. .oo.    .ooooo.  oooo  oooo  oo.ooooo.       Y88bo.      oooo  oooo  ooo. .oo.  .oo.   ooo. .oo.  .oo.    .oooo.   oooo d8b oooo    ooo 
#     888          `888""8P d88' `88b `P  )88b    888   d88' `88b          `Y88b        8 Y88. .P  888  `P  )88b  `888P"Y88b        888         `888  `888P"Y88b  d88' `88b `888  `888   888' `88b       `"Y8888o.  `888  `888  `888P"Y88bP"Y88b  `888P"Y88bP"Y88b  `P  )88b  `888""8P  `88.  .8'  
#     888           888     888ooo888  .oP"888    888   888ooo888            ]88        8  `888'   888   .oP"888   888   888        888          888   888   888  888ooo888  888   888   888   888           `"Y88b  888   888   888   888   888   888   888   888   .oP"888   888       `88..8'   
#     `88b    ooo   888     888    .o d8(  888    888 . 888    .o      o.   .88P        8    Y     888  d8(  888   888   888        888       o  888   888   888  888    .o  888   888   888   888      oo     .d8P  888   888   888   888   888   888   888   888  d8(  888   888        `888'    
#      `Y8bood8P'  d888b    `Y8bod8P' `Y888""8o   "888" `Y8bod8P'      `8bd88P'        o8o        o888o `Y888""8o o888o o888o      o888ooooood8 o888o o888o o888o `Y8bod8P'  `V88V"V8P'  888bod8P'      8""88888P'   `V88V"V8P' o888o o888o o888o o888o o888o o888o `Y888""8o d888b        .8'     
#                                                                                                                                                                                        888                                                                                           .o..P'      
#                                                                                                                                                                                       o888o                                                                                          `Y8P'       
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                                                                                                                                                                                                                                                                             



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. START: Create Team rotation file====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(data.table)
library(dplyr)
library(tidyr)

#========================#
# 1. Load PBP Data File  #
#========================#

pbp_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/pm_nbapbp_",
  season_token,
  ".csv"
)

nbapbp_df <- fread(
  pbp_path,
  colClasses = "character",
  encoding   = "UTF-8"
)

#==========================#
# 2. Prep for lineup work  #
#==========================#

# ---- Possession exclude types (as character for type_id) ----
exclude_types <- as.character(c(
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
  70,71,72,73,74
))

nbapbp_df <- nbapbp_df %>%
  mutate(
    # numeric helpers
    start_quarter_seconds_remaining = suppressWarnings(as.numeric(start_quarter_seconds_remaining)),
    score_value                     = suppressWarnings(as.numeric(score_value)),
    qtr                             = suppressWarnings(as.integer(qtr)),
    
    # build lineup keys for each TEAM (home / away) using ESPN IDs
    lineup_home_ids = paste(
      home_P1_espn_id, home_P2_espn_id, home_P3_espn_id,
      home_P4_espn_id, home_P5_espn_id,
      sep = "|"
    ),
    lineup_away_ids = paste(
      away_P1_espn_id, away_P2_espn_id, away_P3_espn_id,
      away_P4_espn_id, away_P5_espn_id,
      sep = "|"
    ),
    
    # --- rebound flags on the raw PBP (team-neutral) ---
    IS_OREB = if_else(type_text %in% c("Offensive Rebound", "Offensive Team Rebound"),
                      1L, 0L),
    IS_DREB = if_else(type_text %in% c("Defensive Rebound", "Defensive Team Rebound"),
                      1L, 0L),
    
    # --- possession event flag (for lineup-level possessions) ---
    POSSESSION_EVENT = if_else(
      !type_id %in% exclude_types,
      1L,
      0L
    )
  )

#======================================================#
# 3. Compute stint seconds between plays within quarter #
#======================================================#

nbapbp_df <- nbapbp_df %>%
  arrange(game_id, qtr, desc(start_quarter_seconds_remaining), sequence_number) %>%
  group_by(game_id, qtr) %>%
  mutate(
    next_qsr = lead(start_quarter_seconds_remaining),
    # if last row in quarter, assume it runs down to 0
    next_qsr = if_else(is.na(next_qsr), 0, next_qsr),
    stint_seconds = pmax(start_quarter_seconds_remaining - next_qsr, 0)
  ) %>%
  ungroup()

#==============================================#
# 4. Build team-level PBP rows (home + away)   #
#==============================================#

home_lineup_pbp <- nbapbp_df %>%
  filter(!is.na(lineup_home_ids), lineup_home_ids != "NA|NA|NA|NA|NA") %>%
  transmute(
    ESPN_GAME_ID       = game_id,
    game_date          = game_date,
    ESPN_TEAM_ID       = home_team_id,
    ESPN_HOME_TEAM_ID  = home_team_id,
    ESPN_AWAY_TEAM_ID  = away_team_id,
    qtr,
    start_quarter_seconds_remaining,
    stint_seconds,
    
    LINEUP_KEY = lineup_home_ids,
    
    LINEUP_P1 = home_P1,
    LINEUP_P2 = home_P2,
    LINEUP_P3 = home_P3,
    LINEUP_P4 = home_P4,
    LINEUP_P5 = home_P5,
    
    LINEUP_P1_espn_id = home_P1_espn_id,
    LINEUP_P2_espn_id = home_P2_espn_id,
    LINEUP_P3_espn_id = home_P3_espn_id,
    LINEUP_P4_espn_id = home_P4_espn_id,
    LINEUP_P5_espn_id = home_P5_espn_id,
    
    LINEUP_POINTS_ON_PLAY = if_else(team_id == home_team_id, score_value, 0),
    OREB_EVENT            = if_else(team_id == home_team_id, IS_OREB, 0L),
    DREB_EVENT            = if_else(team_id == home_team_id, IS_DREB, 0L),
    POSSESSION_EVENT,
    
    SIDE = "HOME"
  )

away_lineup_pbp <- nbapbp_df %>%
  filter(!is.na(lineup_away_ids), lineup_away_ids != "NA|NA|NA|NA|NA") %>%
  transmute(
    ESPN_GAME_ID       = game_id,
    game_date          = game_date,
    ESPN_TEAM_ID       = away_team_id,
    ESPN_HOME_TEAM_ID  = home_team_id,
    ESPN_AWAY_TEAM_ID  = away_team_id,
    qtr,
    start_quarter_seconds_remaining,
    stint_seconds,
    
    LINEUP_KEY = lineup_away_ids,
    
    LINEUP_P1 = away_P1,
    LINEUP_P2 = away_P2,
    LINEUP_P3 = away_P3,
    LINEUP_P4 = away_P4,
    LINEUP_P5 = away_P5,
    
    LINEUP_P1_espn_id = away_P1_espn_id,
    LINEUP_P2_espn_id = away_P2_espn_id,
    LINEUP_P3_espn_id = away_P3_espn_id,
    LINEUP_P4_espn_id = away_P4_espn_id,
    LINEUP_P5_espn_id = away_P5_espn_id,
    
    LINEUP_POINTS_ON_PLAY = if_else(team_id == away_team_id, score_value, 0),
    OREB_EVENT            = if_else(team_id == away_team_id, IS_OREB, 0L),
    DREB_EVENT            = if_else(team_id == away_team_id, IS_DREB, 0L),
    POSSESSION_EVENT,
    
    SIDE = "AWAY"
  )

team_lineup_pbp <- bind_rows(home_lineup_pbp, away_lineup_pbp)

#====================================#
# 5. Identify starting lineup per team
#====================================#

starting_lineups_wide <- nbapbp_df %>%
  filter(qtr == 1) %>%
  arrange(game_id, desc(start_quarter_seconds_remaining), sequence_number) %>%
  group_by(game_id) %>%
  summarise(
    ESPN_HOME_TEAM_ID     = first(home_team_id),
    ESPN_AWAY_TEAM_ID     = first(away_team_id),
    HOME_START_LINEUP_KEY = first(lineup_home_ids),
    AWAY_START_LINEUP_KEY = first(lineup_away_ids),
    .groups = "drop"
  )

starting_lineups_long <- bind_rows(
  starting_lineups_wide %>%
    transmute(
      ESPN_GAME_ID     = game_id,
      ESPN_TEAM_ID     = ESPN_HOME_TEAM_ID,
      START_LINEUP_KEY = HOME_START_LINEUP_KEY
    ),
  starting_lineups_wide %>%
    transmute(
      ESPN_GAME_ID     = game_id,
      ESPN_TEAM_ID     = ESPN_AWAY_TEAM_ID,
      START_LINEUP_KEY = AWAY_START_LINEUP_KEY
    )
)

#==============================================#
# 6. CORE SECTION: minutes + first appearance  #
#==============================================#

lineup_core_df <- team_lineup_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    LINEUP_P1, LINEUP_P2, LINEUP_P3, LINEUP_P4, LINEUP_P5,
    LINEUP_P1_espn_id, LINEUP_P2_espn_id, LINEUP_P3_espn_id,
    LINEUP_P4_espn_id, LINEUP_P5_espn_id
  ) %>%
  summarise(
    FIRST_QTR = suppressWarnings(min(qtr, na.rm = TRUE)),
    FIRST_QSR = suppressWarnings(
      max(start_quarter_seconds_remaining[qtr == min(qtr, na.rm = TRUE)],
          na.rm = TRUE)
    ),
    LINEUP_IN_Q1  = sum(stint_seconds[qtr == 1], na.rm = TRUE),
    LINEUP_IN_Q2  = sum(stint_seconds[qtr == 2], na.rm = TRUE),
    LINEUP_IN_Q3  = sum(stint_seconds[qtr == 3], na.rm = TRUE),
    LINEUP_IN_Q4  = sum(stint_seconds[qtr == 4], na.rm = TRUE),
    LINEUP_IN_Q5  = sum(stint_seconds[qtr == 5], na.rm = TRUE),
    LINEUP_IN_Q6  = sum(stint_seconds[qtr == 6], na.rm = TRUE),
    LINEUP_IN_CGS = LINEUP_IN_Q1 + LINEUP_IN_Q2 + LINEUP_IN_Q3 +
      LINEUP_IN_Q4 + LINEUP_IN_Q5 + LINEUP_IN_Q6,
    .groups = "drop"
  )

# start building the master table from the core
lineup_summary_df <- lineup_core_df



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. START: Team and Home and Away Abbervation mapping file====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#==============================#
# 7. Add LINEUP_TYPE flag      #
#==============================#

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    starting_lineups_long,
    by = c("ESPN_GAME_ID", "ESPN_TEAM_ID")
  ) %>%
  mutate(
    LINEUP_TYPE = if_else(
      !is.na(START_LINEUP_KEY) & LINEUP_KEY == START_LINEUP_KEY,
      "STARTING",
      "BENCH"
    )
  )

#==============================#
# 8. Add global LINEUP_ID     #
#==============================#

# Ensure LINEUP_KEY is character
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(LINEUP_KEY = as.character(LINEUP_KEY))

# Build a universal mapping: one LINEUP_ID per unique LINEUP_KEY
lineup_id_map <- lineup_summary_df %>%
  dplyr::arrange(ESPN_GAME_ID, ESPN_TEAM_ID, FIRST_QTR, dplyr::desc(FIRST_QSR)) %>%
  dplyr::distinct(LINEUP_KEY, .keep_all = FALSE) %>%
  dplyr::mutate(LINEUP_ID = dplyr::row_number()) %>%
  dplyr::select(LINEUP_KEY, LINEUP_ID)

# Join back so same LINEUP_KEY always gets same LINEUP_ID (across games)
lineup_summary_df <- lineup_summary_df %>%
  dplyr::select(-dplyr::any_of("LINEUP_ID")) %>%  # drop old per-game ID if present
  dplyr::left_join(lineup_id_map, by = "LINEUP_KEY")


#==============================#
# 9. Map Team Short Names      #
#==============================#

team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"

team_map <- fread(team_map_path, colClasses = "character") %>%
  select(
    espn_team_id,
    team_short_name
  ) %>%
  rename(
    TEAM_ID    = espn_team_id,
    TEAM_SHORT = team_short_name
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    team_map,
    by = c("ESPN_TEAM_ID" = "TEAM_ID")
  ) %>%
  rename(
    TEAM = TEAM_SHORT
  ) %>%
  left_join(
    team_map,
    by = c("ESPN_HOME_TEAM_ID" = "TEAM_ID")
  ) %>%
  rename(
    HOME_TEAM = TEAM_SHORT
  ) %>%
  left_join(
    team_map,
    by = c("ESPN_AWAY_TEAM_ID" = "TEAM_ID")
  ) %>%
  rename(
    AWAY_TEAM = TEAM_SHORT
  )

#==============================#
# 10. Add QTR label (optional) #
#==============================#

lineup_summary_df <- lineup_summary_df %>%
  mutate(QTR = "CGS")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. END: Team ann Home and Away Abbervation mapping file====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: PTS SECTION: Lineup scoring splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

lineup_pts_df <- team_lineup_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_PTS_Q1  = sum(LINEUP_POINTS_ON_PLAY[qtr == 1], na.rm = TRUE),
    LINEUP_PTS_Q2  = sum(LINEUP_POINTS_ON_PLAY[qtr == 2], na.rm = TRUE),
    LINEUP_PTS_Q3  = sum(LINEUP_POINTS_ON_PLAY[qtr == 3], na.rm = TRUE),
    LINEUP_PTS_Q4  = sum(LINEUP_POINTS_ON_PLAY[qtr == 4], na.rm = TRUE),
    LINEUP_PTS_Q5  = sum(LINEUP_POINTS_ON_PLAY[qtr == 5], na.rm = TRUE),
    LINEUP_PTS_Q6  = sum(LINEUP_POINTS_ON_PLAY[qtr == 6], na.rm = TRUE),
    LINEUP_PTS_CGS = LINEUP_PTS_Q1 + LINEUP_PTS_Q2 + LINEUP_PTS_Q3 +
      LINEUP_PTS_Q4 + LINEUP_PTS_Q5 + LINEUP_PTS_Q6,
    .groups = "drop"
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_pts_df,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )


rm(away_lineup_pbp, home_lineup_pbp, lineup_core_df, lineup_id_map, starting_lineups_long, starting_lineups_wide, team_map)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: PTS SECTION: Lineup scoring splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: OFFTEAM and DEFTEAM SUCCESS Play 10 MAN Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- Row-level flags on pm_df: OFFTEAM_SUCCESS / DEFTEAM_SUCCESS (1 per play) ---


# 1) Load success criteria file
crit_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/success_criteria_nba.csv"
stopifnot(file.exists(crit_path))
succ_crit_df <- fread(crit_path, colClasses = "character") %>% as_tibble()

# 2) Extract lookup values
off_vec <- succ_crit_df$offteam_success %>% na.omit() %>% trimws()
def_vec <- succ_crit_df$defteam_success %>% na.omit() %>% trimws()

# 3) Validate that nbapbp_df has type_text column
stopifnot("type_text" %in% names(nbapbp_df))

# 4) For each play in nbapbp_df, flag 1 if type_text matches an entry from criteria
nbapbp_df <- nbapbp_df %>%
  mutate(
    OFFTEAM_SUCCESS = if_else(type_text %in% off_vec, 1L, 0L),
    DEFTEAM_SUCCESS = if_else(type_text %in% def_vec, 1L, 0L)
  )

rm(succ_crit_df, tie_by_qtr, tie_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: OFFTEAM and DEFTEAM SUCCESS Play 10 MAN Identification ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# === START: TOP-LEVEL Possessions Data Aggregation 5 Man Section ====
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


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: TIE SECTION: Lineup tie-change splits ====
# (game-level tie transitions, attached to all lineups in that game)
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

tie_by_qtr <- nbapbp_df %>%
  mutate(
    hs  = suppressWarnings(as.integer(.data[["home_score"]])),
    as  = suppressWarnings(as.integer(.data[["away_score"]])),
    qtr = suppressWarnings(as.integer(qtr))
  ) %>%
  filter(qtr %in% 1:6) %>%                                   # include OT1/OT2
  arrange(game_id, qtr, sequence_number) %>%
  group_by(game_id, qtr) %>%
  mutate(
    diff   = hs - as,
    is_tie = !is.na(diff) & diff == 0L,
    tie_in = is_tie & dplyr::lag(!is_tie, default = FALSE)   # transition into a tie
  ) %>%
  summarise(ties = sum(tie_in, na.rm = TRUE), .groups = "drop") %>%
  group_by(game_id) %>%
  tidyr::complete(qtr = 1:6, fill = list(ties = 0L)) %>%     # ensure all qtrs exist
  ungroup() %>%
  tidyr::pivot_wider(
    names_from   = qtr,
    values_from  = ties,
    values_fill  = 0L,
    names_prefix = "LINEUP_TIE_CHNG_Q"
  )

tie_cgs <- nbapbp_df %>%
  mutate(
    hs = suppressWarnings(as.integer(.data[["home_score"]])),
    as = suppressWarnings(as.integer(.data[["away_score"]]))
  ) %>%
  arrange(game_id, sequence_number) %>%
  group_by(game_id) %>%
  mutate(
    diff   = hs - as,
    is_tie = !is.na(diff) & diff == 0L,
    tie_in = is_tie & dplyr::lag(!is_tie, default = FALSE)
  ) %>%
  summarise(
    LINEUP_TIE_CHNG_CGS = sum(tie_in, na.rm = TRUE),
    .groups = "drop"
  )

lineup_tie_df <- tie_by_qtr %>%
  left_join(tie_cgs, by = "game_id") %>%
  rename(ESPN_GAME_ID = game_id)

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_tie_df,
    by = "ESPN_GAME_ID"
  )

rm(lineup_pts_df, lineup_tie_df, tie_by_qtr, tie_cgs, lineup_poss_df)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: TIE SECTION: Lineup tie-change splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: POSS SECTION: Lineup possessions splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

lineup_poss_df <- team_lineup_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_POSS_Q1  = sum(POSSESSION_EVENT[qtr == 1], na.rm = TRUE),
    LINEUP_POSS_Q2  = sum(POSSESSION_EVENT[qtr == 2], na.rm = TRUE),
    LINEUP_POSS_Q3  = sum(POSSESSION_EVENT[qtr == 3], na.rm = TRUE),
    LINEUP_POSS_Q4  = sum(POSSESSION_EVENT[qtr == 4], na.rm = TRUE),
    LINEUP_POSS_Q5  = sum(POSSESSION_EVENT[qtr == 5], na.rm = TRUE),
    LINEUP_POSS_Q6  = sum(POSSESSION_EVENT[qtr == 6], na.rm = TRUE),
    LINEUP_POSS_CGS = LINEUP_POSS_Q1 + LINEUP_POSS_Q2 + LINEUP_POSS_Q3 +
      LINEUP_POSS_Q4 + LINEUP_POSS_Q5 + LINEUP_POSS_Q6,
    .groups = "drop"
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_poss_df,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: POSS SECTION: Lineup possessions splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Possessions Lead/Neut/Trail SECTION: Lineup splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Build a lightweight lineup-PBP table with game state flags
home_state_pbp <- nbapbp_df %>%
  filter(!is.na(lineup_home_ids), lineup_home_ids != "NA|NA|NA|NA|NA") %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    hs  = suppressWarnings(as.integer(home_score)),
    as  = suppressWarnings(as.integer(away_score)),
    team_diff  = hs - as,
    LEAD_FLAG  = as.integer(!is.na(team_diff) & team_diff > 0L),
    TRAIL_FLAG = as.integer(!is.na(team_diff) & team_diff < 0L),
    NEUT_FLAG  = as.integer(!is.na(team_diff) & team_diff == 0L)
  ) %>%
  transmute(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = home_team_id,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    qtr,
    LINEUP_KEY        = lineup_home_ids,
    POSSESSION_EVENT,
    LEAD_FLAG,
    TRAIL_FLAG,
    NEUT_FLAG
  )

away_state_pbp <- nbapbp_df %>%
  filter(!is.na(lineup_away_ids), lineup_away_ids != "NA|NA|NA|NA|NA") %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    hs  = suppressWarnings(as.integer(home_score)),
    as  = suppressWarnings(as.integer(away_score)),
    team_diff  = as - hs,  # away relative diff
    LEAD_FLAG  = as.integer(!is.na(team_diff) & team_diff > 0L),
    TRAIL_FLAG = as.integer(!is.na(team_diff) & team_diff < 0L),
    NEUT_FLAG  = as.integer(!is.na(team_diff) & team_diff == 0L)
  ) %>%
  transmute(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = away_team_id,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    qtr,
    LINEUP_KEY        = lineup_away_ids,
    POSSESSION_EVENT,
    LEAD_FLAG,
    TRAIL_FLAG,
    NEUT_FLAG
  )

lineup_state_pbp <- bind_rows(home_state_pbp, away_state_pbp)

# ---------- LEAD / TRAIL / NEUTRAL POSSESSIONS BY QUARTER (Q1–Q6) ----------
lineup_poss_state_qtr <- lineup_state_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_POSS_LEAD  = sum(POSSESSION_EVENT * LEAD_FLAG,  na.rm = TRUE),
    LINEUP_POSS_TRAIL = sum(POSSESSION_EVENT * TRAIL_FLAG, na.rm = TRUE),
    LINEUP_POSS_NEUT  = sum(POSSESSION_EVENT * NEUT_FLAG,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      LINEUP_POSS_LEAD  = 0L,
      LINEUP_POSS_TRAIL = 0L,
      LINEUP_POSS_NEUT  = 0L
    )
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    cols      = starts_with("LINEUP_POSS_"),
    names_to  = "kind",
    values_to = "val"
  ) %>%
  tidyr::unite("stat", kind, qtr, sep = "_Q") %>%
  tidyr::pivot_wider(
    names_from  = stat,
    values_from = val,
    values_fill = 0L
  )

# ---------- LEAD / TRAIL / NEUTRAL POSSESSIONS (COMPLETE GAME) ----------
lineup_poss_state_cgs <- lineup_state_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_POSS_LEAD_CGS  = sum(POSSESSION_EVENT * LEAD_FLAG,  na.rm = TRUE),
    LINEUP_POSS_TRAIL_CGS = sum(POSSESSION_EVENT * TRAIL_FLAG, na.rm = TRUE),
    LINEUP_POSS_NEUT_CGS  = sum(POSSESSION_EVENT * NEUT_FLAG,  na.rm = TRUE),
    .groups = "drop"
  )

lineup_poss_state_df <- lineup_poss_state_qtr %>%
  left_join(
    lineup_poss_state_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )

# ---------- JOIN INTO lineup_summary_df ----------
lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_poss_state_df,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  mutate(
    across(starts_with("LINEUP_POSS_LEAD_Q"),  ~ dplyr::coalesce(., 0L)),
    across(starts_with("LINEUP_POSS_TRAIL_Q"), ~ dplyr::coalesce(., 0L)),
    across(starts_with("LINEUP_POSS_NEUT_Q"),  ~ dplyr::coalesce(., 0L)),
    LINEUP_POSS_LEAD_CGS  = dplyr::coalesce(LINEUP_POSS_LEAD_CGS,  0L),
    LINEUP_POSS_TRAIL_CGS = dplyr::coalesce(LINEUP_POSS_TRAIL_CGS, 0L),
    LINEUP_POSS_NEUT_CGS  = dplyr::coalesce(LINEUP_POSS_NEUT_CGS,  0L)
  )

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Possessions Lead/Neut/Trail SECTION: Lineup splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: POSS SECTION: Lineup OFF / DEF possession success splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Build a per-play table that knows which lineup is on OFF and which is on DEF
succ_pbp <- nbapbp_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    acting_side = dplyr::case_when(
      team_id == home_team_id ~ "home",
      team_id == away_team_id ~ "away",
      TRUE                    ~ NA_character_
    ),
    # offensive team + lineup (acting team_id)
    OFF_TEAM_ID     = team_id,
    OFF_LINEUP_KEY  = dplyr::case_when(
      acting_side == "home" ~ lineup_home_ids,
      acting_side == "away" ~ lineup_away_ids,
      TRUE                  ~ NA_character_
    ),
    # defensive team + lineup (opponent of acting team)
    DEF_TEAM_ID     = dplyr::case_when(
      acting_side == "home" ~ away_team_id,
      acting_side == "away" ~ home_team_id,
      TRUE                  ~ NA_character_
    ),
    DEF_LINEUP_KEY  = dplyr::case_when(
      acting_side == "home" ~ lineup_away_ids,
      acting_side == "away" ~ lineup_home_ids,
      TRUE                  ~ NA_character_
    )
  )

# ---------- OFFENSIVE SUCCESS BY QUARTER (Q1–Q4) ----------
lineup_off_succ_qtr <- succ_pbp %>%
  filter(
    qtr %in% 1:4,
    !is.na(OFF_LINEUP_KEY),
    OFF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = OFF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = OFF_LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_POSS_OFF_SUCC = sum(OFFTEAM_SUCCESS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_POSS_OFF_SUCC,
    values_fill = 0L,
    names_prefix = "LINEUP_POSS_OFF_SUCC_Q"
  )

# ---------- DEFENSIVE SUCCESS BY QUARTER (Q1–Q4) ----------
lineup_def_succ_qtr <- succ_pbp %>%
  filter(
    qtr %in% 1:4,
    !is.na(DEF_LINEUP_KEY),
    DEF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = DEF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = DEF_LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_POSS_DEF_SUCC = sum(DEFTEAM_SUCCESS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_POSS_DEF_SUCC,
    values_fill = 0L,
    names_prefix = "LINEUP_POSS_DEF_SUCC_Q"
  )

# ---------- OFFENSIVE / DEFENSIVE SUCCESS (COMPLETE GAME) ----------
lineup_off_succ_cgs <- succ_pbp %>%
  filter(
    !is.na(OFF_LINEUP_KEY),
    OFF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = OFF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = OFF_LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_POSS_OFF_SUCC_CGS = sum(OFFTEAM_SUCCESS, na.rm = TRUE),
    .groups = "drop"
  )

lineup_def_succ_cgs <- succ_pbp %>%
  filter(
    !is.na(DEF_LINEUP_KEY),
    DEF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = DEF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = DEF_LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_POSS_DEF_SUCC_CGS = sum(DEFTEAM_SUCCESS, na.rm = TRUE),
    .groups = "drop"
  )

# ---------- COMBINE + JOIN INTO lineup_summary_df ----------
lineup_succ_df <- lineup_off_succ_qtr %>%
  left_join(
    lineup_def_succ_qtr,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  left_join(
    lineup_off_succ_cgs,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  left_join(
    lineup_def_succ_cgs,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_succ_df,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  mutate(
    across(
      c(
        starts_with("LINEUP_POSS_OFF_SUCC_Q"),
        starts_with("LINEUP_POSS_DEF_SUCC_Q"),
        LINEUP_POSS_OFF_SUCC_CGS,
        LINEUP_POSS_DEF_SUCC_CGS
      ),
      ~ dplyr::coalesce(., 0L)
    )
  )

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: POSS SECTION: Lineup OFF / DEF possession success splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: POSS TIME SECTION: Lineup possession time splits ====
# (total & average offensive possession time for each lineup)
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# sanity: make sure needed columns are present
stopifnot(all(c(
  "game_id", "qtr", "team_id", "stint_seconds",
  "home_team_id", "away_team_id",
  "lineup_home_ids", "lineup_away_ids"
) %in% names(nbapbp_df)))

# ---- per-play base with offensive team + offensive lineup ----
lineup_poss_pbp <- nbapbp_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    acting_side = dplyr::case_when(
      team_id == home_team_id ~ "home",
      team_id == away_team_id ~ "away",
      TRUE                    ~ NA_character_
    ),
    OFF_TEAM_ID    = team_id,
    OFF_LINEUP_KEY = dplyr::case_when(
      acting_side == "home" ~ lineup_home_ids,
      acting_side == "away" ~ lineup_away_ids,
      TRUE                  ~ NA_character_
    )
  )

# ---- totals & averages by quarter (Q1–Q6) ----
lineup_poss_time_qtr <- lineup_poss_pbp %>%
  filter(
    qtr %in% 1:6,
    !is.na(OFF_LINEUP_KEY),
    OFF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = OFF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = OFF_LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_TOT_POSS_TIME = sum(stint_seconds, na.rm = TRUE),
    LINEUP_AVG_POSS_TIME = ifelse(n() > 0, mean(stint_seconds, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  group_by(
    ESPN_GAME_ID, ESPN_TEAM_ID, game_date,
    ESPN_HOME_TEAM_ID, ESPN_AWAY_TEAM_ID, LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(LINEUP_TOT_POSS_TIME = 0, LINEUP_AVG_POSS_TIME = 0)
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    c(LINEUP_TOT_POSS_TIME, LINEUP_AVG_POSS_TIME),
    names_to  = "metric",
    values_to = "val"
  ) %>%
  tidyr::unite("stat", metric, qtr, sep = "_Q") %>%
  tidyr::pivot_wider(
    names_from  = stat,
    values_from = val,
    values_fill = 0
  )

# ---- totals & averages for complete game (CGS) ----
lineup_poss_time_cgs <- lineup_poss_pbp %>%
  filter(
    !is.na(OFF_LINEUP_KEY),
    OFF_LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  group_by(
    ESPN_GAME_ID      = game_id,
    ESPN_TEAM_ID      = OFF_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    LINEUP_KEY        = OFF_LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_TOT_POSS_TIME_CGS = sum(stint_seconds, na.rm = TRUE),
    LINEUP_AVG_POSS_TIME_CGS = ifelse(n() > 0, mean(stint_seconds, na.rm = TRUE), 0),
    .groups = "drop"
  )

# ---- combine & join into lineup_summary_df ----
lineup_poss_time_df <- lineup_poss_time_qtr %>%
  left_join(
    lineup_poss_time_cgs,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_poss_time_df,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  mutate(
    across(
      c(
        starts_with("LINEUP_TOT_POSS_TIME_Q"),
        starts_with("LINEUP_AVG_POSS_TIME_Q"),
        LINEUP_TOT_POSS_TIME_CGS,
        LINEUP_AVG_POSS_TIME_CGS
      ),
      ~ dplyr::coalesce(., 0)
    )
  )

rm(away_state_pbp, home_state_pbp, lineup_def_succ_cgs, lineup_def_succ_qtr, lineup_off_succ_cgs, lineup_def_succ_qtr, lineup_poss_df, lineup_poss_pbp, lineup_poss_state_cgs,
   lineup_poss_state_df, lineup_off_succ_qtr, lineup_poss_state_qtr, lineup_poss_time_cgs, lineup_poss_time_df, lineup_state_pbp, lineup_succ_df, succ_pbp, lineup_poss_time_qtr)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: POSS TIME SECTION: Lineup possession time splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



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





#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Raw Field Goal Data Section (LINEUP-LEVEL) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# safety: required columns on nbapbp_df
stopifnot(all(c(
  "game_id","qtr","team_id",
  "home_team_id","away_team_id",
  "shooting_play","scoring_play","type_text",
  "lineup_home_ids","lineup_away_ids",
  "game_date"
) %in% names(nbapbp_df)))

# helper to normalize boolean-ish flags
to_bool <- function(x) {
  xv <- tolower(as.character(x))
  xv %in% c("true","t","1","yes","y")
}

# -------------------------------
# A) Build base lineup FG frame
#    (one row per possession event for the *acting* team)
# -------------------------------
lineup_fg_base <- nbapbp_df %>%
  mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    
    # FG attempts: shooting_play, but not free throws
    is_shot = to_bool(shooting_play) &
      !grepl("free throw", type_text, ignore.case = TRUE),
    
    # FG makes: scoring_play, but not free throws
    is_make = to_bool(scoring_play) &
      !grepl("free throw", type_text, ignore.case = TRUE),
    
    ESPN_GAME_ID      = game_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    
    # acting team + its lineup on this play
    ESPN_TEAM_ID = dplyr::case_when(
      team_id == home_team_id ~ home_team_id,
      team_id == away_team_id ~ away_team_id,
      TRUE ~ NA_character_
    ),
    LINEUP_KEY = dplyr::case_when(
      team_id == home_team_id ~ lineup_home_ids,
      team_id == away_team_id ~ lineup_away_ids,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(ESPN_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  )

# -------------------------------
# B) LINEUP FGA (Q1–Q6 + CGS)
# -------------------------------

# quarter-level FGA
lineup_fga_qtr <- lineup_fg_base %>%
  filter(qtr %in% 1:6) %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_FGA = sum(is_shot, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(LINEUP_FGA = 0L)
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_FGA,
    values_fill = 0L,
    names_prefix = "LINEUP_FGA_Q"
  )

# complete-game FGA
lineup_fga_cgs <- lineup_fg_base %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_FGA_CGS = sum(is_shot, na.rm = TRUE),
    .groups        = "drop"
  )

# -------------------------------
# C) LINEUP FGM (Q1–Q6 + CGS)
# -------------------------------

# quarter-level FGM
lineup_fgm_qtr <- lineup_fg_base %>%
  filter(qtr %in% 1:6) %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  summarise(
    LINEUP_FGM = sum(is_make, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(LINEUP_FGM = 0L)
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_FGM,
    values_fill = 0L,
    names_prefix = "LINEUP_FGM_Q"
  )

# complete-game FGM
lineup_fgm_cgs <- lineup_fg_base %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    LINEUP_FGM_CGS = sum(is_make, na.rm = TRUE),
    .groups        = "drop"
  )

# -------------------------------
# D) Join into lineup_summary_df and compute FG%
# -------------------------------
lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_fga_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  left_join(
    lineup_fga_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  left_join(
    lineup_fgm_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  left_join(
    lineup_fgm_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  mutate(
    # coalesce FGA/FGM (Q1–Q6 + CGS)
    LINEUP_FGA_Q1 = dplyr::coalesce(LINEUP_FGA_Q1, 0L),
    LINEUP_FGA_Q2 = dplyr::coalesce(LINEUP_FGA_Q2, 0L),
    LINEUP_FGA_Q3 = dplyr::coalesce(LINEUP_FGA_Q3, 0L),
    LINEUP_FGA_Q4 = dplyr::coalesce(LINEUP_FGA_Q4, 0L),
    LINEUP_FGA_Q5 = dplyr::coalesce(LINEUP_FGA_Q5, 0L),
    LINEUP_FGA_Q6 = dplyr::coalesce(LINEUP_FGA_Q6, 0L),
    
    LINEUP_FGM_Q1 = dplyr::coalesce(LINEUP_FGM_Q1, 0L),
    LINEUP_FGM_Q2 = dplyr::coalesce(LINEUP_FGM_Q2, 0L),
    LINEUP_FGM_Q3 = dplyr::coalesce(LINEUP_FGM_Q3, 0L),
    LINEUP_FGM_Q4 = dplyr::coalesce(LINEUP_FGM_Q4, 0L),
    LINEUP_FGM_Q5 = dplyr::coalesce(LINEUP_FGM_Q5, 0L),
    LINEUP_FGM_Q6 = dplyr::coalesce(LINEUP_FGM_Q6, 0L),
    
    LINEUP_FGA_CGS = dplyr::coalesce(LINEUP_FGA_CGS, 0L),
    LINEUP_FGM_CGS = dplyr::coalesce(LINEUP_FGM_CGS, 0L)
  ) %>%
  mutate(
    LINEUP_FG_PCT_Q1 = ifelse(LINEUP_FGA_Q1 > 0, LINEUP_FGM_Q1 / LINEUP_FGA_Q1, NA_real_),
    LINEUP_FG_PCT_Q2 = ifelse(LINEUP_FGA_Q2 > 0, LINEUP_FGM_Q2 / LINEUP_FGA_Q2, NA_real_),
    LINEUP_FG_PCT_Q3 = ifelse(LINEUP_FGA_Q3 > 0, LINEUP_FGM_Q3 / LINEUP_FGA_Q3, NA_real_),
    LINEUP_FG_PCT_Q4 = ifelse(LINEUP_FGA_Q4 > 0, LINEUP_FGM_Q4 / LINEUP_FGA_Q4, NA_real_),
    LINEUP_FG_PCT_Q5 = ifelse(LINEUP_FGA_Q5 > 0, LINEUP_FGM_Q5 / LINEUP_FGA_Q5, NA_real_),
    LINEUP_FG_PCT_Q6 = ifelse(LINEUP_FGA_Q6 > 0, LINEUP_FGM_Q6 / LINEUP_FGA_Q6, NA_real_),
    
    LINEUP_FG_PCT_CGS = ifelse(LINEUP_FGA_CGS > 0, LINEUP_FGM_CGS / LINEUP_FGA_CGS, NA_real_)
  )

rm(lineup_fg_base, lineup_fga_qtr, lineup_fga_cgs,
   lineup_fgm_qtr, lineup_fgm_cgs)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Raw Field Goal Data Section (LINEUP-LEVEL) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: FT SECTION: Lineup free throw splits ====
# (LINEUP_FTA_*, LINEUP_FTM_*, LINEUP_FT_PCT_*, LINEUP_FTR_*)
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c(
  "game_id","qtr","team_id","shooting_play","scoring_play",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "game_date"
) %in% names(nbapbp_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

has_type_id  <- "type_id" %in% names(nbapbp_df) || "play_type_id" %in% names(nbapbp_df)
type_id_col  <- if ("type_id" %in% names(nbapbp_df)) {
  "type_id"
} else if ("play_type_id" %in% names(nbapbp_df)) {
  "play_type_id"
} else {
  NA_character_
}

# --- base FT frame at lineup level ---
lineup_ft_base <- nbapbp_df %>%
  mutate(
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
    is_ftm = ft_flag & to_bool(scoring_play),
    
    ESPN_GAME_ID      = game_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    
    ESPN_TEAM_ID = dplyr::case_when(
      team_id == home_team_id ~ home_team_id,
      team_id == away_team_id ~ away_team_id,
      TRUE ~ NA_character_
    ),
    LINEUP_KEY = dplyr::case_when(
      team_id == home_team_id ~ lineup_home_ids,
      team_id == away_team_id ~ lineup_away_ids,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(ESPN_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  )

# ---------- FTA: quarter (Q1–Q6) + CGS ----------
lineup_fta_qtr <- lineup_ft_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  dplyr::summarise(LINEUP_FTA = sum(is_fta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(qtr = 1:6, fill = list(LINEUP_FTA = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_FTA,
    values_fill = 0L,
    names_prefix = "LINEUP_FTA_Q"
  )

lineup_fta_cgs <- lineup_ft_base %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  dplyr::summarise(LINEUP_FTA_CGS = sum(is_fta, na.rm = TRUE), .groups = "drop")

# ---------- FTM: quarter (Q1–Q6) + CGS ----------
lineup_ftm_qtr <- lineup_ft_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  dplyr::summarise(LINEUP_FTM = sum(is_ftm, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(qtr = 1:6, fill = list(LINEUP_FTM = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = LINEUP_FTM,
    values_fill = 0L,
    names_prefix = "LINEUP_FTM_Q"
  )

lineup_ftm_cgs <- lineup_ft_base %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  dplyr::summarise(LINEUP_FTM_CGS = sum(is_ftm, na.rm = TRUE), .groups = "drop")

# ---------- Join into lineup_summary_df & compute FT% / FTR ----------
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    lineup_fta_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_fta_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_ftm_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_ftm_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::mutate(
    LINEUP_FTA_Q1 = dplyr::coalesce(LINEUP_FTA_Q1, 0L),
    LINEUP_FTA_Q2 = dplyr::coalesce(LINEUP_FTA_Q2, 0L),
    LINEUP_FTA_Q3 = dplyr::coalesce(LINEUP_FTA_Q3, 0L),
    LINEUP_FTA_Q4 = dplyr::coalesce(LINEUP_FTA_Q4, 0L),
    LINEUP_FTA_Q5 = dplyr::coalesce(LINEUP_FTA_Q5, 0L),
    LINEUP_FTA_Q6 = dplyr::coalesce(LINEUP_FTA_Q6, 0L),
    
    LINEUP_FTM_Q1 = dplyr::coalesce(LINEUP_FTM_Q1, 0L),
    LINEUP_FTM_Q2 = dplyr::coalesce(LINEUP_FTM_Q2, 0L),
    LINEUP_FTM_Q3 = dplyr::coalesce(LINEUP_FTM_Q3, 0L),
    LINEUP_FTM_Q4 = dplyr::coalesce(LINEUP_FTM_Q4, 0L),
    LINEUP_FTM_Q5 = dplyr::coalesce(LINEUP_FTM_Q5, 0L),
    LINEUP_FTM_Q6 = dplyr::coalesce(LINEUP_FTM_Q6, 0L),
    
    LINEUP_FTA_CGS = dplyr::coalesce(LINEUP_FTA_CGS, 0L),
    LINEUP_FTM_CGS = dplyr::coalesce(LINEUP_FTM_CGS, 0L)
  ) %>%
  dplyr::mutate(
    # FT%
    LINEUP_FT_PCT_Q1 = ifelse(LINEUP_FTA_Q1 > 0, LINEUP_FTM_Q1 / LINEUP_FTA_Q1, NA_real_),
    LINEUP_FT_PCT_Q2 = ifelse(LINEUP_FTA_Q2 > 0, LINEUP_FTM_Q2 / LINEUP_FTA_Q2, NA_real_),
    LINEUP_FT_PCT_Q3 = ifelse(LINEUP_FTA_Q3 > 0, LINEUP_FTM_Q3 / LINEUP_FTA_Q3, NA_real_),
    LINEUP_FT_PCT_Q4 = ifelse(LINEUP_FTA_Q4 > 0, LINEUP_FTM_Q4 / LINEUP_FTA_Q4, NA_real_),
    LINEUP_FT_PCT_Q5 = ifelse(LINEUP_FTA_Q5 > 0, LINEUP_FTM_Q5 / LINEUP_FTA_Q5, NA_real_),
    LINEUP_FT_PCT_Q6 = ifelse(LINEUP_FTA_Q6 > 0, LINEUP_FTM_Q6 / LINEUP_FTA_Q6, NA_real_),
    LINEUP_FT_PCT_CGS = ifelse(LINEUP_FTA_CGS > 0, LINEUP_FTM_CGS / LINEUP_FTA_CGS, NA_real_),
    
    # FTR = FTA / FGA using lineup-level FGA from previous FG section
    LINEUP_FTR_Q1 = ifelse(LINEUP_FGA_Q1 > 0, LINEUP_FTA_Q1 / LINEUP_FGA_Q1, NA_real_),
    LINEUP_FTR_Q2 = ifelse(LINEUP_FGA_Q2 > 0, LINEUP_FTA_Q2 / LINEUP_FGA_Q2, NA_real_),
    LINEUP_FTR_Q3 = ifelse(LINEUP_FGA_Q3 > 0, LINEUP_FTA_Q3 / LINEUP_FGA_Q3, NA_real_),
    LINEUP_FTR_Q4 = ifelse(LINEUP_FGA_Q4 > 0, LINEUP_FTA_Q4 / LINEUP_FGA_Q4, NA_real_),
    LINEUP_FTR_Q5 = ifelse(LINEUP_FGA_Q5 > 0, LINEUP_FTA_Q5 / LINEUP_FGA_Q5, NA_real_),
    LINEUP_FTR_Q6 = ifelse(LINEUP_FGA_Q6 > 0, LINEUP_FTA_Q6 / LINEUP_FGA_Q6, NA_real_),
    LINEUP_FTR_CGS = ifelse(LINEUP_FGA_CGS > 0, LINEUP_FTA_CGS / LINEUP_FGA_CGS, NA_real_)
  )

rm(lineup_ft_base, lineup_fta_qtr, lineup_fta_cgs,
   lineup_ftm_qtr, lineup_ftm_cgs)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: FT SECTION: Lineup free throw splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 2PT Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# LINEUP 2PT ATT / MAKE / % / eFG / PTS / PPP / RATE / PTS SHARE (Q1–Q6 + CGS)
# ===============================

stopifnot(all(c(
  "game_id","qtr","team_id","type_text","scoring_play","score_value",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "game_date"
) %in% names(nbapbp_df)))

zone_col <- dplyr::case_when(
  "shot_zone_basic"  %in% names(nbapbp_df) ~ "shot_zone_basic",
  "SHOT_ZONE_BASIC"  %in% names(nbapbp_df) ~ "SHOT_ZONE_BASIC",
  TRUE ~ NA_character_
)
stopifnot(!is.na(zone_col))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
zf      <- function(num, den) ifelse(den > 0, num / den, NA_real_)

# --- base: identify 2PT attempts / makes at lineup level ---
lineup_2pt_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr = suppressWarnings(as.integer(qtr)),
    
    # map to ESPN game / team + lineup key
    ESPN_GAME_ID      = game_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    ESPN_TEAM_ID = dplyr::case_when(
      team_id == home_team_id ~ home_team_id,
      team_id == away_team_id ~ away_team_id,
      TRUE ~ NA_character_
    ),
    LINEUP_KEY = dplyr::case_when(
      team_id == home_team_id ~ lineup_home_ids,
      team_id == away_team_id ~ lineup_away_ids,
      TRUE ~ NA_character_
    ),
    
    # shot classification: 2PT (exclude FTs + all 3PT zones)
    is_2pta = to_bool(shooting_play) &
      !stringr::str_detect(type_text, stringr::regex("free throw", ignore_case = TRUE)) &
      !( .data[[zone_col]] %in% c("Above the Break 3","Right Corner 3","Left Corner 3") ),
    is_2ptm = is_2pta &
      to_bool(scoring_play) &
      suppressWarnings(as.integer(score_value)) == 2L
  ) %>%
  dplyr::filter(
    !is.na(ESPN_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  )

# --- per-quarter counts (Q1–Q6) ---
lineup_2pt_qtr <- lineup_2pt_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  dplyr::summarise(
    LINEUP_2PTA = sum(is_2pta, na.rm = TRUE),
    LINEUP_2PTM = sum(is_2ptm, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(LINEUP_2PTA = 0L, LINEUP_2PTM = 0L)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(LINEUP_2PTA, LINEUP_2PTM),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# --- complete-game (CGS) counts ---
lineup_2pt_cgs <- lineup_2pt_base %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  dplyr::summarise(
    LINEUP_2PTA_CGS = sum(is_2pta, na.rm = TRUE),
    LINEUP_2PTM_CGS = sum(is_2ptm, na.rm = TRUE),
    .groups         = "drop"
  )

# --- join into lineup_summary_df and ensure supporting totals exist ---
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    lineup_2pt_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_2pt_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::mutate(
    # coalesce 2PT volume
    LINEUP_2PTA_Q1 = dplyr::coalesce(LINEUP_2PTA_Q1, 0L),
    LINEUP_2PTA_Q2 = dplyr::coalesce(LINEUP_2PTA_Q2, 0L),
    LINEUP_2PTA_Q3 = dplyr::coalesce(LINEUP_2PTA_Q3, 0L),
    LINEUP_2PTA_Q4 = dplyr::coalesce(LINEUP_2PTA_Q4, 0L),
    LINEUP_2PTA_Q5 = dplyr::coalesce(LINEUP_2PTA_Q5, 0L),
    LINEUP_2PTA_Q6 = dplyr::coalesce(LINEUP_2PTA_Q6, 0L),
    
    LINEUP_2PTM_Q1 = dplyr::coalesce(LINEUP_2PTM_Q1, 0L),
    LINEUP_2PTM_Q2 = dplyr::coalesce(LINEUP_2PTM_Q2, 0L),
    LINEUP_2PTM_Q3 = dplyr::coalesce(LINEUP_2PTM_Q3, 0L),
    LINEUP_2PTM_Q4 = dplyr::coalesce(LINEUP_2PTM_Q4, 0L),
    LINEUP_2PTM_Q5 = dplyr::coalesce(LINEUP_2PTM_Q5, 0L),
    LINEUP_2PTM_Q6 = dplyr::coalesce(LINEUP_2PTM_Q6, 0L),
    
    LINEUP_2PTA_CGS = dplyr::coalesce(LINEUP_2PTA_CGS, 0L),
    LINEUP_2PTM_CGS = dplyr::coalesce(LINEUP_2PTM_CGS, 0L),
    
    # make sure these exist for RATE + PTS share
    LINEUP_FGA_Q1  = dplyr::coalesce(LINEUP_FGA_Q1, 0),
    LINEUP_FGA_Q2  = dplyr::coalesce(LINEUP_FGA_Q2, 0),
    LINEUP_FGA_Q3  = dplyr::coalesce(LINEUP_FGA_Q3, 0),
    LINEUP_FGA_Q4  = dplyr::coalesce(LINEUP_FGA_Q4, 0),
    LINEUP_FGA_Q5  = dplyr::coalesce(LINEUP_FGA_Q5, 0),
    LINEUP_FGA_Q6  = dplyr::coalesce(LINEUP_FGA_Q6, 0),
    LINEUP_FGA_CGS = dplyr::coalesce(LINEUP_FGA_CGS, 0),
    
    LINEUP_PTS_Q1  = dplyr::coalesce(LINEUP_PTS_Q1, 0),
    LINEUP_PTS_Q2  = dplyr::coalesce(LINEUP_PTS_Q2, 0),
    LINEUP_PTS_Q3  = dplyr::coalesce(LINEUP_PTS_Q3, 0),
    LINEUP_PTS_Q4  = dplyr::coalesce(LINEUP_PTS_Q4, 0),
    LINEUP_PTS_Q5  = dplyr::coalesce(LINEUP_PTS_Q5, 0),
    LINEUP_PTS_Q6  = dplyr::coalesce(LINEUP_PTS_Q6, 0),
    LINEUP_PTS_CGS = dplyr::coalesce(LINEUP_PTS_CGS, 0)
  ) %>%
  dplyr::mutate(
    # 2PT%
    LINEUP_2PT_PCT_Q1  = zf(LINEUP_2PTM_Q1, LINEUP_2PTA_Q1),
    LINEUP_2PT_PCT_Q2  = zf(LINEUP_2PTM_Q2, LINEUP_2PTA_Q2),
    LINEUP_2PT_PCT_Q3  = zf(LINEUP_2PTM_Q3, LINEUP_2PTA_Q3),
    LINEUP_2PT_PCT_Q4  = zf(LINEUP_2PTM_Q4, LINEUP_2PTA_Q4),
    LINEUP_2PT_PCT_Q5  = zf(LINEUP_2PTM_Q5, LINEUP_2PTA_Q5),
    LINEUP_2PT_PCT_Q6  = zf(LINEUP_2PTM_Q6, LINEUP_2PTA_Q6),
    LINEUP_2PT_PCT_CGS = zf(LINEUP_2PTM_CGS, LINEUP_2PTA_CGS),
    
    # eFG for 2s == FG%
    LINEUP_2PT_EFG_PCT_Q1  = LINEUP_2PT_PCT_Q1,
    LINEUP_2PT_EFG_PCT_Q2  = LINEUP_2PT_PCT_Q2,
    LINEUP_2PT_EFG_PCT_Q3  = LINEUP_2PT_PCT_Q3,
    LINEUP_2PT_EFG_PCT_Q4  = LINEUP_2PT_PCT_Q4,
    LINEUP_2PT_EFG_PCT_Q5  = LINEUP_2PT_PCT_Q5,
    LINEUP_2PT_EFG_PCT_Q6  = LINEUP_2PT_PCT_Q6,
    LINEUP_2PT_EFG_PCT_CGS = LINEUP_2PT_PCT_CGS,
    
    # points from 2s
    LINEUP_2PT_PTS_Q1  = 2 * LINEUP_2PTM_Q1,
    LINEUP_2PT_PTS_Q2  = 2 * LINEUP_2PTM_Q2,
    LINEUP_2PT_PTS_Q3  = 2 * LINEUP_2PTM_Q3,
    LINEUP_2PT_PTS_Q4  = 2 * LINEUP_2PTM_Q4,
    LINEUP_2PT_PTS_Q5  = 2 * LINEUP_2PTM_Q5,
    LINEUP_2PT_PTS_Q6  = 2 * LINEUP_2PTM_Q6,
    LINEUP_2PT_PTS_CGS = 2 * LINEUP_2PTM_CGS,
    
    # "possessions" proxy = attempts
    LINEUP_2PT_POSS_Q1  = LINEUP_2PTA_Q1,
    LINEUP_2PT_POSS_Q2  = LINEUP_2PTA_Q2,
    LINEUP_2PT_POSS_Q3  = LINEUP_2PTA_Q3,
    LINEUP_2PT_POSS_Q4  = LINEUP_2PTA_Q4,
    LINEUP_2PT_POSS_Q5  = LINEUP_2PTA_Q5,
    LINEUP_2PT_POSS_Q6  = LINEUP_2PTA_Q6,
    LINEUP_2PT_POSS_CGS = LINEUP_2PTA_CGS,
    
    # PPP
    LINEUP_2PT_PPP_Q1  = zf(LINEUP_2PT_PTS_Q1, LINEUP_2PT_POSS_Q1),
    LINEUP_2PT_PPP_Q2  = zf(LINEUP_2PT_PTS_Q2, LINEUP_2PT_POSS_Q2),
    LINEUP_2PT_PPP_Q3  = zf(LINEUP_2PT_PTS_Q3, LINEUP_2PT_POSS_Q3),
    LINEUP_2PT_PPP_Q4  = zf(LINEUP_2PT_PTS_Q4, LINEUP_2PT_POSS_Q4),
    LINEUP_2PT_PPP_Q5  = zf(LINEUP_2PT_PTS_Q5, LINEUP_2PT_POSS_Q5),
    LINEUP_2PT_PPP_Q6  = zf(LINEUP_2PT_PTS_Q6, LINEUP_2PT_POSS_Q6),
    LINEUP_2PT_PPP_CGS = zf(LINEUP_2PT_PTS_CGS, LINEUP_2PT_POSS_CGS),
    
    # RATE = 2PTA / total FGA
    LINEUP_2PT_RATE_Q1  = zf(LINEUP_2PTA_Q1, LINEUP_FGA_Q1),
    LINEUP_2PT_RATE_Q2  = zf(LINEUP_2PTA_Q2, LINEUP_FGA_Q2),
    LINEUP_2PT_RATE_Q3  = zf(LINEUP_2PTA_Q3, LINEUP_FGA_Q3),
    LINEUP_2PT_RATE_Q4  = zf(LINEUP_2PTA_Q4, LINEUP_FGA_Q4),
    LINEUP_2PT_RATE_Q5  = zf(LINEUP_2PTA_Q5, LINEUP_FGA_Q5),
    LINEUP_2PT_RATE_Q6  = zf(LINEUP_2PTA_Q6, LINEUP_FGA_Q6),
    LINEUP_2PT_RATE_CGS = zf(LINEUP_2PTA_CGS, LINEUP_FGA_CGS),
    
    # PTS share = (2 * 2PTM) / lineup total points
    LINEUP_2PT_PTSHR_Q1  = zf(LINEUP_2PT_PTS_Q1, LINEUP_PTS_Q1),
    LINEUP_2PT_PTSHR_Q2  = zf(LINEUP_2PT_PTS_Q2, LINEUP_PTS_Q2),
    LINEUP_2PT_PTSHR_Q3  = zf(LINEUP_2PT_PTS_Q3, LINEUP_PTS_Q3),
    LINEUP_2PT_PTSHR_Q4  = zf(LINEUP_2PT_PTS_Q4, LINEUP_PTS_Q4),
    LINEUP_2PT_PTSHR_Q5  = zf(LINEUP_2PT_PTS_Q5, LINEUP_PTS_Q5),
    LINEUP_2PT_PTSHR_Q6  = zf(LINEUP_2PT_PTS_Q6, LINEUP_PTS_Q6),
    LINEUP_2PT_PTSHR_CGS = zf(LINEUP_2PT_PTS_CGS, LINEUP_PTS_CGS)
  )

rm(lineup_2pt_base, lineup_2pt_qtr, lineup_2pt_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 2PT Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: 3PT Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# LINEUP 3PT: Attempts, Makes, %, eFG%, Points, Point Share, PPP, Rate (Q1–Q6 + CGS)

stopifnot(all(c(
  "game_id","team_id","qtr","shooting_play","scoring_play","score_value",
  "home_team_id","away_team_id","SHOT_ZONE_BASIC",
  "lineup_home_ids","lineup_away_ids","game_date"
) %in% names(nbapbp_df)))

zone_col <- "SHOT_ZONE_BASIC"
to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(num, den) ifelse(den > 0 & !is.na(den), num / den, NA_real_)

three_labels <- c("Above the Break 3", "Right Corner 3", "Left Corner 3")
three_rx     <- stringr::regex(paste(three_labels, collapse = "|"), ignore_case = TRUE)

# ---- Row-level LINEUP 3PT flags ---------------------------------------------------- #
lineup_three_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    
    ESPN_GAME_ID      = game_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    ESPN_TEAM_ID = dplyr::case_when(
      team_id == home_team_id ~ home_team_id,
      team_id == away_team_id ~ away_team_id,
      TRUE ~ NA_character_
    ),
    LINEUP_KEY = dplyr::case_when(
      team_id == home_team_id ~ lineup_home_ids,
      team_id == away_team_id ~ lineup_away_ids,
      TRUE ~ NA_character_
    ),
    
    is_3pa = shot & stringr::str_detect(.data[[zone_col]], three_rx),
    is_3pm = make &
      stringr::str_detect(.data[[zone_col]], three_rx) &
      !is.na(pts) & pts == 3L
  ) %>%
  dplyr::filter(
    !is.na(ESPN_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  dplyr::transmute(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr,
    three_fga = as.integer(is_3pa),
    three_fgm = as.integer(is_3pm),
    three_pts = 3L * as.integer(is_3pm)
  )

# ---- Per-quarter tallies (Q1–Q6) --------------------------------------------------- #
lineup_three_qtr <- lineup_three_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  dplyr::summarise(
    LINEUP_3PTA = sum(three_fga, na.rm = TRUE),
    LINEUP_3PTM = sum(three_fgm, na.rm = TRUE),
    LINEUP_3PT_PTS = sum(three_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      LINEUP_3PTA     = 0L,
      LINEUP_3PTM     = 0L,
      LINEUP_3PT_PTS  = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(LINEUP_3PTA, LINEUP_3PTM, LINEUP_3PT_PTS),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# ---- Complete-game (CGS) tallies --------------------------------------------------- #
lineup_three_cgs <- lineup_three_base %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  dplyr::summarise(
    LINEUP_3PTA_CGS    = sum(three_fga, na.rm = TRUE),
    LINEUP_3PTM_CGS    = sum(three_fgm, na.rm = TRUE),
    LINEUP_3PT_PTS_CGS = sum(three_pts, na.rm = TRUE),
    .groups            = "drop"
  )

# ---- Join into lineup_summary_df --------------------------------------------------- #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    lineup_three_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_three_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::mutate(
    # coalesce 3PT volume
    LINEUP_3PTA_Q1 = dplyr::coalesce(LINEUP_3PTA_Q1, 0L),
    LINEUP_3PTA_Q2 = dplyr::coalesce(LINEUP_3PTA_Q2, 0L),
    LINEUP_3PTA_Q3 = dplyr::coalesce(LINEUP_3PTA_Q3, 0L),
    LINEUP_3PTA_Q4 = dplyr::coalesce(LINEUP_3PTA_Q4, 0L),
    LINEUP_3PTA_Q5 = dplyr::coalesce(LINEUP_3PTA_Q5, 0L),
    LINEUP_3PTA_Q6 = dplyr::coalesce(LINEUP_3PTA_Q6, 0L),
    
    LINEUP_3PTM_Q1 = dplyr::coalesce(LINEUP_3PTM_Q1, 0L),
    LINEUP_3PTM_Q2 = dplyr::coalesce(LINEUP_3PTM_Q2, 0L),
    LINEUP_3PTM_Q3 = dplyr::coalesce(LINEUP_3PTM_Q3, 0L),
    LINEUP_3PTM_Q4 = dplyr::coalesce(LINEUP_3PTM_Q4, 0L),
    LINEUP_3PTM_Q5 = dplyr::coalesce(LINEUP_3PTM_Q5, 0L),
    LINEUP_3PTM_Q6 = dplyr::coalesce(LINEUP_3PTM_Q6, 0L),
    
    LINEUP_3PT_PTS_Q1 = dplyr::coalesce(LINEUP_3PT_PTS_Q1, 0L),
    LINEUP_3PT_PTS_Q2 = dplyr::coalesce(LINEUP_3PT_PTS_Q2, 0L),
    LINEUP_3PT_PTS_Q3 = dplyr::coalesce(LINEUP_3PT_PTS_Q3, 0L),
    LINEUP_3PT_PTS_Q4 = dplyr::coalesce(LINEUP_3PT_PTS_Q4, 0L),
    LINEUP_3PT_PTS_Q5 = dplyr::coalesce(LINEUP_3PT_PTS_Q5, 0L),
    LINEUP_3PT_PTS_Q6 = dplyr::coalesce(LINEUP_3PT_PTS_Q6, 0L),
    
    LINEUP_3PTA_CGS    = dplyr::coalesce(LINEUP_3PTA_CGS, 0L),
    LINEUP_3PTM_CGS    = dplyr::coalesce(LINEUP_3PTM_CGS, 0L),
    LINEUP_3PT_PTS_CGS = dplyr::coalesce(LINEUP_3PT_PTS_CGS, 0L),
    
    # ensure lineup totals exist for denominators
    LINEUP_FGA_Q1  = dplyr::coalesce(LINEUP_FGA_Q1, 0),
    LINEUP_FGA_Q2  = dplyr::coalesce(LINEUP_FGA_Q2, 0),
    LINEUP_FGA_Q3  = dplyr::coalesce(LINEUP_FGA_Q3, 0),
    LINEUP_FGA_Q4  = dplyr::coalesce(LINEUP_FGA_Q4, 0),
    LINEUP_FGA_Q5  = dplyr::coalesce(LINEUP_FGA_Q5, 0),
    LINEUP_FGA_Q6  = dplyr::coalesce(LINEUP_FGA_Q6, 0),
    LINEUP_FGA_CGS = dplyr::coalesce(LINEUP_FGA_CGS, 0),
    
    LINEUP_PTS_Q1  = dplyr::coalesce(LINEUP_PTS_Q1, 0),
    LINEUP_PTS_Q2  = dplyr::coalesce(LINEUP_PTS_Q2, 0),
    LINEUP_PTS_Q3  = dplyr::coalesce(LINEUP_PTS_Q3, 0),
    LINEUP_PTS_Q4  = dplyr::coalesce(LINEUP_PTS_Q4, 0),
    LINEUP_PTS_Q5  = dplyr::coalesce(LINEUP_PTS_Q5, 0),
    LINEUP_PTS_Q6  = dplyr::coalesce(LINEUP_PTS_Q6, 0),
    LINEUP_PTS_CGS = dplyr::coalesce(LINEUP_PTS_CGS, 0)
  ) %>%
  dplyr::mutate(
    # 3PT% (raw ratios)
    LINEUP_3PT_PCT_Q1  = safe_div(LINEUP_3PTM_Q1 , LINEUP_3PTA_Q1),
    LINEUP_3PT_PCT_Q2  = safe_div(LINEUP_3PTM_Q2 , LINEUP_3PTA_Q2),
    LINEUP_3PT_PCT_Q3  = safe_div(LINEUP_3PTM_Q3 , LINEUP_3PTA_Q3),
    LINEUP_3PT_PCT_Q4  = safe_div(LINEUP_3PTM_Q4 , LINEUP_3PTA_Q4),
    LINEUP_3PT_PCT_Q5  = safe_div(LINEUP_3PTM_Q5 , LINEUP_3PTA_Q5),
    LINEUP_3PT_PCT_Q6  = safe_div(LINEUP_3PTM_Q6 , LINEUP_3PTA_Q6),
    LINEUP_3PT_PCT_CGS = safe_div(LINEUP_3PTM_CGS, LINEUP_3PTA_CGS),
    
    # eFG% for 3s (1.5 * makes / attempts)
    LINEUP_3PT_EFG_PCT_Q1  = safe_div(1.5 * LINEUP_3PTM_Q1 , LINEUP_3PTA_Q1),
    LINEUP_3PT_EFG_PCT_Q2  = safe_div(1.5 * LINEUP_3PTM_Q2 , LINEUP_3PTA_Q2),
    LINEUP_3PT_EFG_PCT_Q3  = safe_div(1.5 * LINEUP_3PTM_Q3 , LINEUP_3PTA_Q3),
    LINEUP_3PT_EFG_PCT_Q4  = safe_div(1.5 * LINEUP_3PTM_Q4 , LINEUP_3PTA_Q4),
    LINEUP_3PT_EFG_PCT_Q5  = safe_div(1.5 * LINEUP_3PTM_Q5 , LINEUP_3PTA_Q5),
    LINEUP_3PT_EFG_PCT_Q6  = safe_div(1.5 * LINEUP_3PTM_Q6 , LINEUP_3PTA_Q6),
    LINEUP_3PT_EFG_PCT_CGS = safe_div(1.5 * LINEUP_3PTM_CGS, LINEUP_3PTA_CGS),
    
    # PPP (points per 3PT attempt)
    LINEUP_3PT_PPP_Q1  = safe_div(LINEUP_3PT_PTS_Q1 , LINEUP_3PTA_Q1),
    LINEUP_3PT_PPP_Q2  = safe_div(LINEUP_3PT_PTS_Q2 , LINEUP_3PTA_Q2),
    LINEUP_3PT_PPP_Q3  = safe_div(LINEUP_3PT_PTS_Q3 , LINEUP_3PTA_Q3),
    LINEUP_3PT_PPP_Q4  = safe_div(LINEUP_3PT_PTS_Q4 , LINEUP_3PTA_Q4),
    LINEUP_3PT_PPP_Q5  = safe_div(LINEUP_3PT_PTS_Q5 , LINEUP_3PTA_Q5),
    LINEUP_3PT_PPP_Q6  = safe_div(LINEUP_3PT_PTS_Q6 , LINEUP_3PTA_Q6),
    LINEUP_3PT_PPP_CGS = safe_div(LINEUP_3PT_PTS_CGS, LINEUP_3PTA_CGS),
    
    # 3PT points share vs lineup total points (raw ratio)
    LINEUP_3PT_PTSHR_Q1  = safe_div(LINEUP_3PT_PTS_Q1 , LINEUP_PTS_Q1),
    LINEUP_3PT_PTSHR_Q2  = safe_div(LINEUP_3PT_PTS_Q2 , LINEUP_PTS_Q2),
    LINEUP_3PT_PTSHR_Q3  = safe_div(LINEUP_3PT_PTS_Q3 , LINEUP_PTS_Q3),
    LINEUP_3PT_PTSHR_Q4  = safe_div(LINEUP_3PT_PTS_Q4 , LINEUP_PTS_Q4),
    LINEUP_3PT_PTSHR_Q5  = safe_div(LINEUP_3PT_PTS_Q5 , LINEUP_PTS_Q5),
    LINEUP_3PT_PTSHR_Q6  = safe_div(LINEUP_3PT_PTS_Q6 , LINEUP_PTS_Q6),
    LINEUP_3PT_PTSHR_CGS = safe_div(LINEUP_3PT_PTS_CGS, LINEUP_PTS_CGS),
    
    # 3PT Rate (3PA share of lineup FGA)
    LINEUP_3PT_RATE_Q1  = safe_div(LINEUP_3PTA_Q1 , LINEUP_FGA_Q1),
    LINEUP_3PT_RATE_Q2  = safe_div(LINEUP_3PTA_Q2 , LINEUP_FGA_Q2),
    LINEUP_3PT_RATE_Q3  = safe_div(LINEUP_3PTA_Q3 , LINEUP_FGA_Q3),
    LINEUP_3PT_RATE_Q4  = safe_div(LINEUP_3PTA_Q4 , LINEUP_FGA_Q4),
    LINEUP_3PT_RATE_Q5  = safe_div(LINEUP_3PTA_Q5 , LINEUP_FGA_Q5),
    LINEUP_3PT_RATE_Q6  = safe_div(LINEUP_3PTA_Q6 , LINEUP_FGA_Q6),
    LINEUP_3PT_RATE_CGS = safe_div(LINEUP_3PTA_CGS, LINEUP_FGA_CGS)
  )

rm(lineup_three_base, lineup_three_qtr, lineup_three_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: 3PT Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Pull-Up Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# LINEUP PULL-UPS: FGA / FGM / PTS / FG% / EFG / PPP / RATE / PTS SHARE
# LINEUP_PU_FGA_Q1..Q6, LINEUP_PU_FGA_CGS
# LINEUP_PU_FGM_Q1..Q6, LINEUP_PU_FGM_CGS
# LINEUP_PU_PTS_Q1..Q6, LINEUP_PU_PTS_CGS
# LINEUP_PU_FG_PCT_*, LINEUP_PU_EFG_PCT_*,
# LINEUP_PU_PPP_*, LINEUP_PU_PTSHR_*, LINEUP_PU_RATE_*

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "shooting_play","scoring_play","score_value",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "game_date"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build pull-up base at LINEUP level ------------------------------------- #
lineup_pu_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    
    ESPN_GAME_ID      = game_id,
    ESPN_HOME_TEAM_ID = home_team_id,
    ESPN_AWAY_TEAM_ID = away_team_id,
    ESPN_TEAM_ID = dplyr::case_when(
      team_id == home_team_id ~ home_team_id,
      team_id == away_team_id ~ away_team_id,
      TRUE ~ NA_character_
    ),
    LINEUP_KEY = dplyr::case_when(
      team_id == home_team_id ~ lineup_home_ids,
      team_id == away_team_id ~ lineup_away_ids,
      TRUE ~ NA_character_
    ),
    
    is_pu = stringr::str_detect(type_text, stringr::regex("pullup", ignore_case = TRUE))
  ) %>%
  dplyr::filter(
    is_pu,
    shot,
    !is.na(ESPN_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  ) %>%
  dplyr::transmute(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr,
    pu_fga = 1L,
    pu_fgm = as.integer(make & !is.na(pts) & pts > 0L),
    pu_pts = dplyr::if_else(make & !is.na(pts) & pts > 0L, pts, 0L)
  )

# ---- Quarter-level tallies (Q1–Q6) ----------------------------------------- #
lineup_pu_qtr <- lineup_pu_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY,
    qtr
  ) %>%
  dplyr::summarise(
    LINEUP_PU_FGA = sum(pu_fga, na.rm = TRUE),
    LINEUP_PU_FGM = sum(pu_fgm, na.rm = TRUE),
    LINEUP_PU_PTS = sum(pu_pts, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      LINEUP_PU_FGA = 0L,
      LINEUP_PU_FGM = 0L,
      LINEUP_PU_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(LINEUP_PU_FGA, LINEUP_PU_FGM, LINEUP_PU_PTS),
    values_fill = 0L,
    names_glue  = "{.value}_Q{qtr}"
  )

# ---- Complete game (CGS) tallies ------------------------------------------- #
lineup_pu_cgs <- lineup_pu_base %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  dplyr::summarise(
    LINEUP_PU_FGA_CGS = sum(pu_fga, na.rm = TRUE),
    LINEUP_PU_FGM_CGS = sum(pu_fgm, na.rm = TRUE),
    LINEUP_PU_PTS_CGS = sum(pu_pts, na.rm = TRUE),
    .groups           = "drop"
  )

# ---- Join into lineup_summary_df ------------------------------------------- #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    lineup_pu_qtr,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::left_join(
    lineup_pu_cgs,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "game_date",
      "ESPN_HOME_TEAM_ID",
      "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  ) %>%
  dplyr::mutate(
    # coalesce pull-up volume
    LINEUP_PU_FGA_Q1 = dplyr::coalesce(LINEUP_PU_FGA_Q1, 0L),
    LINEUP_PU_FGA_Q2 = dplyr::coalesce(LINEUP_PU_FGA_Q2, 0L),
    LINEUP_PU_FGA_Q3 = dplyr::coalesce(LINEUP_PU_FGA_Q3, 0L),
    LINEUP_PU_FGA_Q4 = dplyr::coalesce(LINEUP_PU_FGA_Q4, 0L),
    LINEUP_PU_FGA_Q5 = dplyr::coalesce(LINEUP_PU_FGA_Q5, 0L),
    LINEUP_PU_FGA_Q6 = dplyr::coalesce(LINEUP_PU_FGA_Q6, 0L),
    
    LINEUP_PU_FGM_Q1 = dplyr::coalesce(LINEUP_PU_FGM_Q1, 0L),
    LINEUP_PU_FGM_Q2 = dplyr::coalesce(LINEUP_PU_FGM_Q2, 0L),
    LINEUP_PU_FGM_Q3 = dplyr::coalesce(LINEUP_PU_FGM_Q3, 0L),
    LINEUP_PU_FGM_Q4 = dplyr::coalesce(LINEUP_PU_FGM_Q4, 0L),
    LINEUP_PU_FGM_Q5 = dplyr::coalesce(LINEUP_PU_FGM_Q5, 0L),
    LINEUP_PU_FGM_Q6 = dplyr::coalesce(LINEUP_PU_FGM_Q6, 0L),
    
    LINEUP_PU_PTS_Q1 = dplyr::coalesce(LINEUP_PU_PTS_Q1, 0L),
    LINEUP_PU_PTS_Q2 = dplyr::coalesce(LINEUP_PU_PTS_Q2, 0L),
    LINEUP_PU_PTS_Q3 = dplyr::coalesce(LINEUP_PU_PTS_Q3, 0L),
    LINEUP_PU_PTS_Q4 = dplyr::coalesce(LINEUP_PU_PTS_Q4, 0L),
    LINEUP_PU_PTS_Q5 = dplyr::coalesce(LINEUP_PU_PTS_Q5, 0L),
    LINEUP_PU_PTS_Q6 = dplyr::coalesce(LINEUP_PU_PTS_Q6, 0L),
    
    LINEUP_PU_FGA_CGS = dplyr::coalesce(LINEUP_PU_FGA_CGS, 0L),
    LINEUP_PU_FGM_CGS = dplyr::coalesce(LINEUP_PU_FGM_CGS, 0L),
    LINEUP_PU_PTS_CGS = dplyr::coalesce(LINEUP_PU_PTS_CGS, 0L),
    
    # ensure denominators for RATE + PTS share
    LINEUP_FGA_Q1  = dplyr::coalesce(LINEUP_FGA_Q1, 0),
    LINEUP_FGA_Q2  = dplyr::coalesce(LINEUP_FGA_Q2, 0),
    LINEUP_FGA_Q3  = dplyr::coalesce(LINEUP_FGA_Q3, 0),
    LINEUP_FGA_Q4  = dplyr::coalesce(LINEUP_FGA_Q4, 0),
    LINEUP_FGA_Q5  = dplyr::coalesce(LINEUP_FGA_Q5, 0),
    LINEUP_FGA_Q6  = dplyr::coalesce(LINEUP_FGA_Q6, 0),
    LINEUP_FGA_CGS = dplyr::coalesce(LINEUP_FGA_CGS, 0),
    
    LINEUP_PTS_Q1  = dplyr::coalesce(LINEUP_PTS_Q1, 0),
    LINEUP_PTS_Q2  = dplyr::coalesce(LINEUP_PTS_Q2, 0),
    LINEUP_PTS_Q3  = dplyr::coalesce(LINEUP_PTS_Q3, 0),
    LINEUP_PTS_Q4  = dplyr::coalesce(LINEUP_PTS_Q4, 0),
    LINEUP_PTS_Q5  = dplyr::coalesce(LINEUP_PTS_Q5, 0),
    LINEUP_PTS_Q6  = dplyr::coalesce(LINEUP_PTS_Q6, 0),
    LINEUP_PTS_CGS = dplyr::coalesce(LINEUP_PTS_CGS, 0)
  ) %>%
  dplyr::mutate(
    # FG% and eFG% (raw ratio)
    LINEUP_PU_FG_PCT_Q1  = safe_div(LINEUP_PU_FGM_Q1, LINEUP_PU_FGA_Q1),
    LINEUP_PU_FG_PCT_Q2  = safe_div(LINEUP_PU_FGM_Q2, LINEUP_PU_FGA_Q2),
    LINEUP_PU_FG_PCT_Q3  = safe_div(LINEUP_PU_FGM_Q3, LINEUP_PU_FGA_Q3),
    LINEUP_PU_FG_PCT_Q4  = safe_div(LINEUP_PU_FGM_Q4, LINEUP_PU_FGA_Q4),
    LINEUP_PU_FG_PCT_Q5  = safe_div(LINEUP_PU_FGM_Q5, LINEUP_PU_FGA_Q5),
    LINEUP_PU_FG_PCT_Q6  = safe_div(LINEUP_PU_FGM_Q6, LINEUP_PU_FGA_Q6),
    LINEUP_PU_FG_PCT_CGS = safe_div(LINEUP_PU_FGM_CGS, LINEUP_PU_FGA_CGS),
    
    LINEUP_PU_EFG_PCT_Q1  = LINEUP_PU_FG_PCT_Q1,
    LINEUP_PU_EFG_PCT_Q2  = LINEUP_PU_FG_PCT_Q2,
    LINEUP_PU_EFG_PCT_Q3  = LINEUP_PU_FG_PCT_Q3,
    LINEUP_PU_EFG_PCT_Q4  = LINEUP_PU_FG_PCT_Q4,
    LINEUP_PU_EFG_PCT_Q5  = LINEUP_PU_FG_PCT_Q5,
    LINEUP_PU_EFG_PCT_Q6  = LINEUP_PU_FG_PCT_Q6,
    LINEUP_PU_EFG_PCT_CGS = LINEUP_PU_FG_PCT_CGS,
    
    # PPP (points per pull-up attempt)
    LINEUP_PU_PPP_Q1  = safe_div(LINEUP_PU_PTS_Q1, LINEUP_PU_FGA_Q1),
    LINEUP_PU_PPP_Q2  = safe_div(LINEUP_PU_PTS_Q2, LINEUP_PU_FGA_Q2),
    LINEUP_PU_PPP_Q3  = safe_div(LINEUP_PU_PTS_Q3, LINEUP_PU_FGA_Q3),
    LINEUP_PU_PPP_Q4  = safe_div(LINEUP_PU_PTS_Q4, LINEUP_PU_FGA_Q4),
    LINEUP_PU_PPP_Q5  = safe_div(LINEUP_PU_PTS_Q5, LINEUP_PU_FGA_Q5),
    LINEUP_PU_PPP_Q6  = safe_div(LINEUP_PU_PTS_Q6, LINEUP_PU_FGA_Q6),
    LINEUP_PU_PPP_CGS = safe_div(LINEUP_PU_PTS_CGS, LINEUP_PU_FGA_CGS),
    
    # points share vs lineup total points
    LINEUP_PU_PTSHR_Q1  = safe_div(LINEUP_PU_PTS_Q1, LINEUP_PTS_Q1),
    LINEUP_PU_PTSHR_Q2  = safe_div(LINEUP_PU_PTS_Q2, LINEUP_PTS_Q2),
    LINEUP_PU_PTSHR_Q3  = safe_div(LINEUP_PU_PTS_Q3, LINEUP_PTS_Q3),
    LINEUP_PU_PTSHR_Q4  = safe_div(LINEUP_PU_PTS_Q4, LINEUP_PTS_Q4),
    LINEUP_PU_PTSHR_Q5  = safe_div(LINEUP_PU_PTS_Q5, LINEUP_PTS_Q5),
    LINEUP_PU_PTSHR_Q6  = safe_div(LINEUP_PU_PTS_Q6, LINEUP_PTS_Q6),
    LINEUP_PU_PTSHR_CGS = safe_div(LINEUP_PU_PTS_CGS, LINEUP_PTS_CGS),
    
    # rate: pull-up FGA share of total FGA
    LINEUP_PU_RATE_Q1  = safe_div(LINEUP_PU_FGA_Q1, LINEUP_FGA_Q1),
    LINEUP_PU_RATE_Q2  = safe_div(LINEUP_PU_FGA_Q2, LINEUP_FGA_Q2),
    LINEUP_PU_RATE_Q3  = safe_div(LINEUP_PU_FGA_Q3, LINEUP_FGA_Q3),
    LINEUP_PU_RATE_Q4  = safe_div(LINEUP_PU_FGA_Q4, LINEUP_FGA_Q4),
    LINEUP_PU_RATE_Q5  = safe_div(LINEUP_PU_FGA_Q5, LINEUP_FGA_Q5),
    LINEUP_PU_RATE_Q6  = safe_div(LINEUP_PU_FGA_Q6, LINEUP_FGA_Q6),
    LINEUP_PU_RATE_CGS = safe_div(LINEUP_PU_FGA_CGS, LINEUP_FGA_CGS)
  )

rm(lineup_pu_base, lineup_pu_qtr, lineup_pu_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Pull-Up Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: PPP, eFG%, TS%  Field Goal Data Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ===============================
# LINEUP PPP, eFG%, TS%  (Q1–Q6 + CGS)
# Requires lineup-level:
#   LINEUP_PTS_Q*, LINEUP_POSS_Q*, LINEUP_FGA_Q*, LINEUP_FGM_Q*,
#   LINEUP_3PTM_Q*, LINEUP_FTA_Q* (+ CGS)
# ===============================

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Ensure denominators/numerators exist & coalesce to 0 ------------------------- #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    # FGA
    LINEUP_FGA_Q1  = dplyr::coalesce(LINEUP_FGA_Q1 , 0),
    LINEUP_FGA_Q2  = dplyr::coalesce(LINEUP_FGA_Q2 , 0),
    LINEUP_FGA_Q3  = dplyr::coalesce(LINEUP_FGA_Q3 , 0),
    LINEUP_FGA_Q4  = dplyr::coalesce(LINEUP_FGA_Q4 , 0),
    LINEUP_FGA_Q5  = dplyr::coalesce(LINEUP_FGA_Q5 , 0),
    LINEUP_FGA_Q6  = dplyr::coalesce(LINEUP_FGA_Q6 , 0),
    LINEUP_FGA_CGS = dplyr::coalesce(LINEUP_FGA_CGS, 0),
    
    # FGM
    LINEUP_FGM_Q1  = dplyr::coalesce(LINEUP_FGM_Q1 , 0),
    LINEUP_FGM_Q2  = dplyr::coalesce(LINEUP_FGM_Q2 , 0),
    LINEUP_FGM_Q3  = dplyr::coalesce(LINEUP_FGM_Q3 , 0),
    LINEUP_FGM_Q4  = dplyr::coalesce(LINEUP_FGM_Q4 , 0),
    LINEUP_FGM_Q5  = dplyr::coalesce(LINEUP_FGM_Q5 , 0),
    LINEUP_FGM_Q6  = dplyr::coalesce(LINEUP_FGM_Q6 , 0),
    LINEUP_FGM_CGS = dplyr::coalesce(LINEUP_FGM_CGS, 0),
    
    # 3PTM (for eFG)
    LINEUP_3PTM_Q1  = dplyr::coalesce(LINEUP_3PTM_Q1 , 0),
    LINEUP_3PTM_Q2  = dplyr::coalesce(LINEUP_3PTM_Q2 , 0),
    LINEUP_3PTM_Q3  = dplyr::coalesce(LINEUP_3PTM_Q3 , 0),
    LINEUP_3PTM_Q4  = dplyr::coalesce(LINEUP_3PTM_Q4 , 0),
    LINEUP_3PTM_Q5  = dplyr::coalesce(LINEUP_3PTM_Q5 , 0),
    LINEUP_3PTM_Q6  = dplyr::coalesce(LINEUP_3PTM_Q6 , 0),
    LINEUP_3PTM_CGS = dplyr::coalesce(LINEUP_3PTM_CGS, 0),
    
    # FTA (for TS%)
    LINEUP_FTA_Q1  = dplyr::coalesce(LINEUP_FTA_Q1 , 0),
    LINEUP_FTA_Q2  = dplyr::coalesce(LINEUP_FTA_Q2 , 0),
    LINEUP_FTA_Q3  = dplyr::coalesce(LINEUP_FTA_Q3 , 0),
    LINEUP_FTA_Q4  = dplyr::coalesce(LINEUP_FTA_Q4 , 0),
    LINEUP_FTA_Q5  = dplyr::coalesce(LINEUP_FTA_Q5 , 0),
    LINEUP_FTA_Q6  = dplyr::coalesce(LINEUP_FTA_Q6 , 0),
    LINEUP_FTA_CGS = dplyr::coalesce(LINEUP_FTA_CGS, 0),
    
    # POSS
    LINEUP_POSS_Q1  = dplyr::coalesce(LINEUP_POSS_Q1 , 0),
    LINEUP_POSS_Q2  = dplyr::coalesce(LINEUP_POSS_Q2 , 0),
    LINEUP_POSS_Q3  = dplyr::coalesce(LINEUP_POSS_Q3 , 0),
    LINEUP_POSS_Q4  = dplyr::coalesce(LINEUP_POSS_Q4 , 0),
    LINEUP_POSS_Q5  = dplyr::coalesce(LINEUP_POSS_Q5 , 0),
    LINEUP_POSS_Q6  = dplyr::coalesce(LINEUP_POSS_Q6 , 0),
    LINEUP_POSS_CGS = dplyr::coalesce(LINEUP_POSS_CGS, 0),
    
    # PTS
    LINEUP_PTS_Q1  = dplyr::coalesce(LINEUP_PTS_Q1 , 0),
    LINEUP_PTS_Q2  = dplyr::coalesce(LINEUP_PTS_Q2 , 0),
    LINEUP_PTS_Q3  = dplyr::coalesce(LINEUP_PTS_Q3 , 0),
    LINEUP_PTS_Q4  = dplyr::coalesce(LINEUP_PTS_Q4 , 0),
    LINEUP_PTS_Q5  = dplyr::coalesce(LINEUP_PTS_Q5 , 0),
    LINEUP_PTS_Q6  = dplyr::coalesce(LINEUP_PTS_Q6 , 0),
    LINEUP_PTS_CGS = dplyr::coalesce(LINEUP_PTS_CGS, 0)
  )

# ---- PPP (Points per Possession) --------------------------------------------------- #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    LINEUP_PPP_Q1  = safe_div(LINEUP_PTS_Q1 , LINEUP_POSS_Q1),
    LINEUP_PPP_Q2  = safe_div(LINEUP_PTS_Q2 , LINEUP_POSS_Q2),
    LINEUP_PPP_Q3  = safe_div(LINEUP_PTS_Q3 , LINEUP_POSS_Q3),
    LINEUP_PPP_Q4  = safe_div(LINEUP_PTS_Q4 , LINEUP_POSS_Q4),
    LINEUP_PPP_Q5  = safe_div(LINEUP_PTS_Q5 , LINEUP_POSS_Q5),
    LINEUP_PPP_Q6  = safe_div(LINEUP_PTS_Q6 , LINEUP_POSS_Q6),
    LINEUP_PPP_CGS = safe_div(LINEUP_PTS_CGS, LINEUP_POSS_CGS)
  )

# ---- eFG% = (FGM + 0.5 * 3PTM) / FGA ----------------------------------------------- #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    LINEUP_EFG_PCT_Q1  = safe_div(LINEUP_FGM_Q1  + 0.5 * LINEUP_3PTM_Q1 , LINEUP_FGA_Q1),
    LINEUP_EFG_PCT_Q2  = safe_div(LINEUP_FGM_Q2  + 0.5 * LINEUP_3PTM_Q2 , LINEUP_FGA_Q2),
    LINEUP_EFG_PCT_Q3  = safe_div(LINEUP_FGM_Q3  + 0.5 * LINEUP_3PTM_Q3 , LINEUP_FGA_Q3),
    LINEUP_EFG_PCT_Q4  = safe_div(LINEUP_FGM_Q4  + 0.5 * LINEUP_3PTM_Q4 , LINEUP_FGA_Q4),
    LINEUP_EFG_PCT_Q5  = safe_div(LINEUP_FGM_Q5  + 0.5 * LINEUP_3PTM_Q5 , LINEUP_FGA_Q5),
    LINEUP_EFG_PCT_Q6  = safe_div(LINEUP_FGM_Q6  + 0.5 * LINEUP_3PTM_Q6 , LINEUP_FGA_Q6),
    LINEUP_EFG_PCT_CGS = safe_div(LINEUP_FGM_CGS + 0.5 * LINEUP_3PTM_CGS, LINEUP_FGA_CGS)
  )

# ---- TS% = PTS / (2 * (FGA + 0.44 * FTA)) ------------------------------------------ #
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    LINEUP_TS_PCT_Q1 = {
      d <- (LINEUP_FGA_Q1 + 0.44 * LINEUP_FTA_Q1)
      ifelse(d > 0, LINEUP_PTS_Q1 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_Q2 = {
      d <- (LINEUP_FGA_Q2 + 0.44 * LINEUP_FTA_Q2)
      ifelse(d > 0, LINEUP_PTS_Q2 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_Q3 = {
      d <- (LINEUP_FGA_Q3 + 0.44 * LINEUP_FTA_Q3)
      ifelse(d > 0, LINEUP_PTS_Q3 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_Q4 = {
      d <- (LINEUP_FGA_Q4 + 0.44 * LINEUP_FTA_Q4)
      ifelse(d > 0, LINEUP_PTS_Q4 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_Q5 = {
      d <- (LINEUP_FGA_Q5 + 0.44 * LINEUP_FTA_Q5)
      ifelse(d > 0, LINEUP_PTS_Q5 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_Q6 = {
      d <- (LINEUP_FGA_Q6 + 0.44 * LINEUP_FTA_Q6)
      ifelse(d > 0, LINEUP_PTS_Q6 / (2 * d), NA_real_)
    },
    LINEUP_TS_PCT_CGS = {
      d <- (LINEUP_FGA_CGS + 0.44 * LINEUP_FTA_CGS)
      ifelse(d > 0, LINEUP_PTS_CGS / (2 * d), NA_real_)
    }
  )

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: PPP, eFG%, TS%  Field Goal Data Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Second Chance Data Aggregation Section (LINEUP) ====
# SECOND CHANCE (LINEUP): FGA/FGM/FG%, PTS, PPP, POSS (Q1..Q6 + CGS)
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id",
                "team_id",
                "home_team_id",
                "away_team_id",
                "lineup_home_ids",
                "lineup_away_ids",
                "qtr",
                "type_id",
                "shooting_play",
                "scoring_play",
                "score_value") %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(num, den) ifelse(den > 0 & !is.na(den), num / den, NA_real_)

# ---- OT flags per game (does PBP contain Q5/Q6?) ----
ot_flags <- nbapbp_df %>%
  dplyr::mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  dplyr::filter(qtr %in% c(5L, 6L)) %>%
  dplyr::distinct(game_id, qtr) %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = qtr,
    values_fn   = ~ 1L,
    values_fill = 0L,
    names_prefix = "HAS_Q"
  )
# -> columns: game_id, HAS_Q5 (0/1), HAS_Q6 (0/1)

# ---- Build second-chance base: look at next row within (game, TEAM lineup, qtr) ----
sec_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    idx  = dplyr::row_number(),
    is_orb = suppressWarnings(as.integer(type_id)) == 156L,
    sh  = to_bool(shooting_play),
    sc  = to_bool(scoring_play),
    val = suppressWarnings(as.integer(score_value)),
    # offensive lineup key for the team that got the ORB
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(!is.na(lineup_key)) %>%
  dplyr::arrange(game_id, qtr, idx) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::mutate(
    next_shoot = dplyr::lead(sh),
    next_score = dplyr::lead(sc),
    next_val   = dplyr::lead(val)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is_orb)   # each ORB defines a second-chance opportunity

# ---- Quarter splits (Q1–Q6 incl. OT1/OT2) ----
sec_qtr <- sec_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_FGA_SEC_CHN  = sum(next_shoot, na.rm = TRUE),
    L_FGM_SEC_CHN  = sum(next_score, na.rm = TRUE),
    L_SEC_CHN_PTS  = sum(
      dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L),
      na.rm = TRUE
    ),
    L_SEC_CHN_POSS = dplyr::n(),   # each ORB is one second-chance possession
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_FGA_SEC_CHN  = 0L,
      L_FGM_SEC_CHN  = 0L,
      L_SEC_CHN_PTS  = 0L,
      L_SEC_CHN_POSS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_FGA_SEC_CHN, L_FGM_SEC_CHN, L_SEC_CHN_PTS, L_SEC_CHN_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
sec_cgs <- sec_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_FGA_SEC_CHN_CGS  = sum(next_shoot, na.rm = TRUE),
    L_FGM_SEC_CHN_CGS  = sum(next_score, na.rm = TRUE),
    L_SEC_CHN_PTS_CGS  = sum(
      dplyr::if_else(next_score, dplyr::coalesce(next_val, 0L), 0L),
      na.rm = TRUE
    ),
    L_SEC_CHN_POSS_CGS = dplyr::n(),
    .groups = "drop"
  )

# ---- Join raw second-chance counts back into lineup_summary_df ----
lineup_summary_df <- lineup_summary_df %>%
  # add OT flags for masking Q5/Q6
  dplyr::left_join(
    ot_flags,
    by = c("ESPN_GAME_ID" = "game_id")
  ) %>%
  dplyr::mutate(
    HAS_Q5 = dplyr::coalesce(HAS_Q5, 0L),
    HAS_Q6 = dplyr::coalesce(HAS_Q6, 0L)
  ) %>%
  # bring in SEC counts
  dplyr::left_join(
    sec_qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    sec_cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always; OT only if the game actually had those quarters
    L_FGA_SEC_CHN_Q1 = dplyr::coalesce(L_FGA_SEC_CHN_Q1, 0L),
    L_FGA_SEC_CHN_Q2 = dplyr::coalesce(L_FGA_SEC_CHN_Q2, 0L),
    L_FGA_SEC_CHN_Q3 = dplyr::coalesce(L_FGA_SEC_CHN_Q3, 0L),
    L_FGA_SEC_CHN_Q4 = dplyr::coalesce(L_FGA_SEC_CHN_Q4, 0L),
    L_FGA_SEC_CHN_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                      dplyr::coalesce(L_FGA_SEC_CHN_Q5, 0L), 0L),
    L_FGA_SEC_CHN_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                      dplyr::coalesce(L_FGA_SEC_CHN_Q6, 0L), 0L),
    L_FGA_SEC_CHN_CGS = dplyr::coalesce(L_FGA_SEC_CHN_CGS, 0L),
    
    L_FGM_SEC_CHN_Q1 = dplyr::coalesce(L_FGM_SEC_CHN_Q1, 0L),
    L_FGM_SEC_CHN_Q2 = dplyr::coalesce(L_FGM_SEC_CHN_Q2, 0L),
    L_FGM_SEC_CHN_Q3 = dplyr::coalesce(L_FGM_SEC_CHN_Q3, 0L),
    L_FGM_SEC_CHN_Q4 = dplyr::coalesce(L_FGM_SEC_CHN_Q4, 0L),
    L_FGM_SEC_CHN_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                      dplyr::coalesce(L_FGM_SEC_CHN_Q5, 0L), 0L),
    L_FGM_SEC_CHN_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                      dplyr::coalesce(L_FGM_SEC_CHN_Q6, 0L), 0L),
    L_FGM_SEC_CHN_CGS = dplyr::coalesce(L_FGM_SEC_CHN_CGS, 0L),
    
    L_SEC_CHN_PTS_Q1 = dplyr::coalesce(L_SEC_CHN_PTS_Q1, 0L),
    L_SEC_CHN_PTS_Q2 = dplyr::coalesce(L_SEC_CHN_PTS_Q2, 0L),
    L_SEC_CHN_PTS_Q3 = dplyr::coalesce(L_SEC_CHN_PTS_Q3, 0L),
    L_SEC_CHN_PTS_Q4 = dplyr::coalesce(L_SEC_CHN_PTS_Q4, 0L),
    L_SEC_CHN_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                      dplyr::coalesce(L_SEC_CHN_PTS_Q5, 0L), 0L),
    L_SEC_CHN_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                      dplyr::coalesce(L_SEC_CHN_PTS_Q6, 0L), 0L),
    L_SEC_CHN_PTS_CGS = dplyr::coalesce(L_SEC_CHN_PTS_CGS, 0L),
    
    L_SEC_CHN_POSS_Q1 = dplyr::coalesce(L_SEC_CHN_POSS_Q1, 0L),
    L_SEC_CHN_POSS_Q2 = dplyr::coalesce(L_SEC_CHN_POSS_Q2, 0L),
    L_SEC_CHN_POSS_Q3 = dplyr::coalesce(L_SEC_CHN_POSS_Q3, 0L),
    L_SEC_CHN_POSS_Q4 = dplyr::coalesce(L_SEC_CHN_POSS_Q4, 0L),
    L_SEC_CHN_POSS_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                       dplyr::coalesce(L_SEC_CHN_POSS_Q5, 0L), 0L),
    L_SEC_CHN_POSS_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                       dplyr::coalesce(L_SEC_CHN_POSS_Q6, 0L), 0L),
    L_SEC_CHN_POSS_CGS = dplyr::coalesce(L_SEC_CHN_POSS_CGS, 0L)
  )

rm(sec_base, sec_qtr, sec_cgs, ot_flags)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Second Chance Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fast Break Data Aggregation Section (LINEUP) ====
# FAST BREAK (LINEUP): FGA, FGM, FG%, PPP, PTS, RATE, POSS  (Q1–Q6 + CGS)
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id","team_id","home_team_id","away_team_id",
                "lineup_home_ids","lineup_away_ids",
                "qtr","type_text",
                "shooting_play","scoring_play","score_value",
                "clock_minutes","clock_seconds") %in% names(nbapbp_df)))

to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build "next play" context within each game & quarter (lineup-aware) ----
fb_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds)),
    trig  = stringr::str_detect(
      type_text,
      stringr::regex("Turnover|Steal|Defensive Rebound", ignore_case = TRUE)
    )
  ) %>%
  dplyr::arrange(game_id, qtr, dplyr::desc(t_sec)) %>%   # next row has smaller clock
  dplyr::group_by(game_id, qtr) %>%
  dplyr::mutate(
    next_team        = dplyr::lead(team_id),
    next_t_sec       = dplyr::lead(t_sec),
    next_shoot       = to_bool(dplyr::lead(shooting_play)),
    next_scoreF      = to_bool(dplyr::lead(scoring_play)),
    next_pts         = suppressWarnings(as.integer(dplyr::lead(score_value))),
    next_home_ids    = dplyr::lead(lineup_home_ids),
    next_away_ids    = dplyr::lead(lineup_away_ids),
    dt               = as.integer(t_sec - next_t_sec)     # clock counts down
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fb_poss = trig & dt >= 0L & dt <= 7L & !is.na(next_team),
    fb_team = next_team,
    lineup_key = dplyr::if_else(
      fb_team == home_team_id,
      next_home_ids,
      next_away_ids
    ),
    fb_fga = fb_poss & next_shoot,
    fb_fgm = fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L,
    fb_pts = dplyr::if_else(
      fb_poss & next_scoreF & !is.na(next_pts) & next_pts > 0L,
      next_pts, 0L
    )
  ) %>%
  dplyr::filter(fb_poss & !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id = fb_team,
    lineup_key,
    qtr,
    fb_fga,
    fb_fgm,
    fb_pts,
    fb_poss
  )

# ---- Per-quarter (Q1–Q6 incl. OT1/OT2) ----
fbrk_qtr <- fb_base %>%
  dplyr::mutate(qtr = suppressWarnings(as.integer(qtr))) %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_FBRK_FGA  = sum(fb_fga,  na.rm = TRUE),
    L_FBRK_FGM  = sum(fb_fgm,  na.rm = TRUE),
    L_FBRK_PTS  = sum(fb_pts,  na.rm = TRUE),
    L_FBRK_POSS = sum(fb_poss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_FBRK_FGA  = 0L,
      L_FBRK_FGM  = 0L,
      L_FBRK_PTS  = 0L,
      L_FBRK_POSS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_FBRK_FGA, L_FBRK_FGM, L_FBRK_PTS, L_FBRK_POSS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
fbrk_cgs <- fb_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_FBRK_FGA_CGS  = sum(fb_fga,  na.rm = TRUE),
    L_FBRK_FGM_CGS  = sum(fb_fgm,  na.rm = TRUE),
    L_FBRK_PTS_CGS  = sum(fb_pts,  na.rm = TRUE),
    L_FBRK_POSS_CGS = sum(fb_poss, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join raw counts into lineup_summary_df with OT masking (uses HAS_Q5 / HAS_Q6) ----
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    fbrk_qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    fbrk_cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::mutate(
    # Q1–Q4 always; Q5/Q6 only when HAS_Q5 / HAS_Q6 == 1L
    L_FBRK_FGA_Q1 = dplyr::coalesce(L_FBRK_FGA_Q1, 0L),
    L_FBRK_FGA_Q2 = dplyr::coalesce(L_FBRK_FGA_Q2, 0L),
    L_FBRK_FGA_Q3 = dplyr::coalesce(L_FBRK_FGA_Q3, 0L),
    L_FBRK_FGA_Q4 = dplyr::coalesce(L_FBRK_FGA_Q4, 0L),
    L_FBRK_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                   dplyr::coalesce(L_FBRK_FGA_Q5, 0L), 0L),
    L_FBRK_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                   dplyr::coalesce(L_FBRK_FGA_Q6, 0L), 0L),
    L_FBRK_FGA_CGS = dplyr::coalesce(L_FBRK_FGA_CGS, 0L),
    
    L_FBRK_FGM_Q1 = dplyr::coalesce(L_FBRK_FGM_Q1, 0L),
    L_FBRK_FGM_Q2 = dplyr::coalesce(L_FBRK_FGM_Q2, 0L),
    L_FBRK_FGM_Q3 = dplyr::coalesce(L_FBRK_FGM_Q3, 0L),
    L_FBRK_FGM_Q4 = dplyr::coalesce(L_FBRK_FGM_Q4, 0L),
    L_FBRK_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                   dplyr::coalesce(L_FBRK_FGM_Q5, 0L), 0L),
    L_FBRK_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                   dplyr::coalesce(L_FBRK_FGM_Q6, 0L), 0L),
    L_FBRK_FGM_CGS = dplyr::coalesce(L_FBRK_FGM_CGS, 0L),
    
    L_FBRK_PTS_Q1 = dplyr::coalesce(L_FBRK_PTS_Q1, 0L),
    L_FBRK_PTS_Q2 = dplyr::coalesce(L_FBRK_PTS_Q2, 0L),
    L_FBRK_PTS_Q3 = dplyr::coalesce(L_FBRK_PTS_Q3, 0L),
    L_FBRK_PTS_Q4 = dplyr::coalesce(L_FBRK_PTS_Q4, 0L),
    L_FBRK_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                   dplyr::coalesce(L_FBRK_PTS_Q5, 0L), 0L),
    L_FBRK_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                   dplyr::coalesce(L_FBRK_PTS_Q6, 0L), 0L),
    L_FBRK_PTS_CGS = dplyr::coalesce(L_FBRK_PTS_CGS, 0L),
    
    L_FBRK_POSS_Q1 = dplyr::coalesce(L_FBRK_POSS_Q1, 0L),
    L_FBRK_POSS_Q2 = dplyr::coalesce(L_FBRK_POSS_Q2, 0L),
    L_FBRK_POSS_Q3 = dplyr::coalesce(L_FBRK_POSS_Q3, 0L),
    L_FBRK_POSS_Q4 = dplyr::coalesce(L_FBRK_POSS_Q4, 0L),
    L_FBRK_POSS_Q5 = dplyr::if_else(HAS_Q5 == 1L,
                                    dplyr::coalesce(L_FBRK_POSS_Q5, 0L), 0L),
    L_FBRK_POSS_Q6 = dplyr::if_else(HAS_Q6 == 1L,
                                    dplyr::coalesce(L_FBRK_POSS_Q6, 0L), 0L),
    L_FBRK_POSS_CGS = dplyr::coalesce(L_FBRK_POSS_CGS, 0L)
  ) %>%
  # ---- Percentages, PPP, Rate (lineup-level) ----
dplyr::mutate(
  # FG%
  L_FBRK_FG_PCT_Q1  = safe_div(L_FBRK_FGM_Q1 , L_FBRK_FGA_Q1),
  L_FBRK_FG_PCT_Q2  = safe_div(L_FBRK_FGM_Q2 , L_FBRK_FGA_Q2),
  L_FBRK_FG_PCT_Q3  = safe_div(L_FBRK_FGM_Q3 , L_FBRK_FGA_Q3),
  L_FBRK_FG_PCT_Q4  = safe_div(L_FBRK_FGM_Q4 , L_FBRK_FGA_Q4),
  L_FBRK_FG_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                     safe_div(L_FBRK_FGM_Q5 , L_FBRK_FGA_Q5), NA_real_),
  L_FBRK_FG_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                     safe_div(L_FBRK_FGM_Q6 , L_FBRK_FGA_Q6), NA_real_),
  L_FBRK_FG_PCT_CGS = safe_div(L_FBRK_FGM_CGS, L_FBRK_FGA_CGS),
  
  # PPP (per fast-break possession)
  L_FBRK_PPP_Q1  = safe_div(L_FBRK_PTS_Q1 , L_FBRK_POSS_Q1),
  L_FBRK_PPP_Q2  = safe_div(L_FBRK_PTS_Q2 , L_FBRK_POSS_Q2),
  L_FBRK_PPP_Q3  = safe_div(L_FBRK_PTS_Q3 , L_FBRK_POSS_Q3),
  L_FBRK_PPP_Q4  = safe_div(L_FBRK_PTS_Q4 , L_FBRK_POSS_Q4),
  L_FBRK_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                  safe_div(L_FBRK_PTS_Q5 , L_FBRK_POSS_Q5), NA_real_),
  L_FBRK_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                  safe_div(L_FBRK_PTS_Q6 , L_FBRK_POSS_Q6), NA_real_),
  L_FBRK_PPP_CGS = safe_div(L_FBRK_PTS_CGS, L_FBRK_POSS_CGS),
  
  # RATE = fast-break possessions / total possessions
  # (assumes L_POSS_Q* and L_POSS_CGS already exist in lineup_summary_df)
  L_FBRK_RATE_Q1  = safe_div(L_FBRK_POSS_Q1 , LINEUP_POSS_Q1 ),
  L_FBRK_RATE_Q2  = safe_div(L_FBRK_POSS_Q2 , LINEUP_POSS_Q2 ),
  L_FBRK_RATE_Q3  = safe_div(L_FBRK_POSS_Q3 , LINEUP_POSS_Q3 ),
  L_FBRK_RATE_Q4  = safe_div(L_FBRK_POSS_Q4 , LINEUP_POSS_Q4 ),
  L_FBRK_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                   safe_div(L_FBRK_POSS_Q5 , LINEUP_POSS_Q5), NA_real_),
  L_FBRK_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                   safe_div(L_FBRK_POSS_Q6 , LINEUP_POSS_Q6), NA_real_),
  L_FBRK_RATE_CGS = safe_div(L_FBRK_POSS_CGS, LINEUP_POSS_CGS),
  
  # Share of lineup points from fast break (assumes L_PTS_SCORED_* exist)
  L_FBRK_PTSHR_Q1  = safe_div(L_FBRK_PTS_Q1 , LINEUP_PTS_Q1),
  L_FBRK_PTSHR_Q2  = safe_div(L_FBRK_PTS_Q2 , LINEUP_PTS_Q2),
  L_FBRK_PTSHR_Q3  = safe_div(L_FBRK_PTS_Q3 , LINEUP_PTS_Q3),
  L_FBRK_PTSHR_Q4  = safe_div(L_FBRK_PTS_Q4 , LINEUP_PTS_Q4),
  L_FBRK_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                    safe_div(L_FBRK_PTS_Q5 , LINEUP_PTS_Q5), NA_real_),
  L_FBRK_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                    safe_div(L_FBRK_PTS_Q6 , LINEUP_PTS_Q6), NA_real_),
  L_FBRK_PTSHR_CGS = safe_div(L_FBRK_PTS_CGS, LINEUP_PTS_CGS)
)

rm(fb_base, fbrk_qtr, fbrk_cgs)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fast Break Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: AND1 Data Aggregation Section (LINEUP) ====
# AND-1 (LINEUP): FGA, FGM, FG%, PPP, PTS, RATE  (Q1–Q6 + CGS, with OT masking)
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stopifnot(all(c("game_id",
                "team_id",
                "home_team_id",
                "away_team_id",
                "lineup_home_ids",
                "lineup_away_ids",
                "qtr",
                "type_text",
                "shooting_play",
                "scoring_play",
                "score_value") %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build AND-1 lineup base (FT "1 of 1" after an and-1 make) ----
and1_base_lineup <- nbapbp_df %>%
  dplyr::mutate(
    qtr        = suppressWarnings(as.integer(qtr)),
    shot       = to_bool(shooting_play),
    make       = to_bool(scoring_play),
    pts        = suppressWarnings(as.integer(score_value)),
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(
    type_text == "Free Throw - 1 of 1",
    !is.na(lineup_key)
  ) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    and1_fga = shot,                                  # FT attempt flag
    and1_fgm = make & !is.na(pts) & pts > 0L,         # FT made flag
    and1_pts = dplyr::if_else(and1_fgm, pts, 0L)
  )

# ---- Per-quarter (Q1–Q6) ----
and1_qtr_lineup <- and1_base_lineup %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_AND1_FGA = sum(and1_fga, na.rm = TRUE),
    L_AND1_FGM = sum(and1_fgm, na.rm = TRUE),
    L_AND1_PTS = sum(and1_pts, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_AND1_FGA = 0L,
      L_AND1_FGM = 0L,
      L_AND1_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_AND1_FGA, L_AND1_FGM, L_AND1_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ----
and1_cgs_lineup <- and1_base_lineup %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_AND1_FGA_CGS = sum(and1_fga, na.rm = TRUE),
    L_AND1_FGM_CGS = sum(and1_fgm, na.rm = TRUE),
    L_AND1_PTS_CGS = sum(and1_pts, na.rm = TRUE),
    .groups        = "drop"
  )

# ---- Join into lineup_summary_df + coalesce + OT masking via HAS_Q5/HAS_Q6 ----
lineup_summary_df <- lineup_summary_df %>%
  dplyr::left_join(
    and1_qtr_lineup,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    and1_cgs_lineup,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(L_AND1_FGA_Q1, L_AND1_FGA_Q2, L_AND1_FGA_Q3, L_AND1_FGA_Q4,
        L_AND1_FGA_Q5, L_AND1_FGA_Q6, L_AND1_FGA_CGS,
        L_AND1_FGM_Q1, L_AND1_FGM_Q2, L_AND1_FGM_Q3, L_AND1_FGM_Q4,
        L_AND1_FGM_Q5, L_AND1_FGM_Q6, L_AND1_FGM_CGS,
        L_AND1_PTS_Q1, L_AND1_PTS_Q2, L_AND1_PTS_Q3, L_AND1_PTS_Q4,
        L_AND1_PTS_Q5, L_AND1_PTS_Q6, L_AND1_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT only if game actually had Q5/Q6
    L_AND1_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_AND1_FGA_Q5, 0L),
    L_AND1_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_AND1_FGM_Q5, 0L),
    L_AND1_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_AND1_PTS_Q5, 0L),
    
    L_AND1_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_AND1_FGA_Q6, 0L),
    L_AND1_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_AND1_FGM_Q6, 0L),
    L_AND1_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_AND1_PTS_Q6, 0L)
  ) %>%
  # ---- Derived: FG%, PPP (per AND-1 attempt), RATE (AND-1 makes / lineup FGA) ----
dplyr::mutate(
  # FG%
  L_AND1_FG_PCT_Q1  = safe_div(L_AND1_FGM_Q1 , L_AND1_FGA_Q1),
  L_AND1_FG_PCT_Q2  = safe_div(L_AND1_FGM_Q2 , L_AND1_FGA_Q2),
  L_AND1_FG_PCT_Q3  = safe_div(L_AND1_FGM_Q3 , L_AND1_FGA_Q3),
  L_AND1_FG_PCT_Q4  = safe_div(L_AND1_FGM_Q4 , L_AND1_FGA_Q4),
  L_AND1_FG_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                     safe_div(L_AND1_FGM_Q5, L_AND1_FGA_Q5), NA_real_),
  L_AND1_FG_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                     safe_div(L_AND1_FGM_Q6, L_AND1_FGA_Q6), NA_real_),
  L_AND1_FG_PCT_CGS = safe_div(L_AND1_FGM_CGS, L_AND1_FGA_CGS),
  
  # PPP (points per AND-1 FT attempt)
  L_AND1_PPP_Q1  = safe_div(L_AND1_PTS_Q1 , L_AND1_FGA_Q1),
  L_AND1_PPP_Q2  = safe_div(L_AND1_PTS_Q2 , L_AND1_FGA_Q2),
  L_AND1_PPP_Q3  = safe_div(L_AND1_PTS_Q3 , L_AND1_FGA_Q3),
  L_AND1_PPP_Q4  = safe_div(L_AND1_PTS_Q4 , L_AND1_FGA_Q4),
  L_AND1_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                  safe_div(L_AND1_PTS_Q5, L_AND1_FGA_Q5), NA_real_),
  L_AND1_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                  safe_div(L_AND1_PTS_Q6, L_AND1_FGA_Q6), NA_real_),
  L_AND1_PPP_CGS = safe_div(L_AND1_PTS_CGS, L_AND1_FGA_CGS),
  
  # RATE = AND-1 FGM / lineup FGA (assumes LINEUP_FGA_* exist)
  L_AND1_RATE_Q1  = safe_div(L_AND1_FGM_Q1 , LINEUP_FGA_Q1 ),
  L_AND1_RATE_Q2  = safe_div(L_AND1_FGM_Q2 , LINEUP_FGA_Q2 ),
  L_AND1_RATE_Q3  = safe_div(L_AND1_FGM_Q3 , LINEUP_FGA_Q3 ),
  L_AND1_RATE_Q4  = safe_div(L_AND1_FGM_Q4 , LINEUP_FGA_Q4 ),
  L_AND1_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                   safe_div(L_AND1_FGM_Q5, LINEUP_FGA_Q5), NA_real_),
  L_AND1_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                   safe_div(L_AND1_FGM_Q6, LINEUP_FGA_Q6), NA_real_),
  L_AND1_RATE_CGS = safe_div(L_AND1_FGM_CGS, LINEUP_FGA_CGS)
)

rm(and1_base_lineup, and1_qtr_lineup, and1_cgs_lineup)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: AND1 Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Mid-Range Shooting Data Aggregation Section (LINEUP) ====
# MID-RANGE (LINEUP): FGA, FGM, FG%, PPP, PTS, RATE, PTS SHARE
# Q1–Q6 (OT-aware via HAS_Q5/HAS_Q6) + CGS
# Source: nbapbp_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id",
  "team_id",
  "home_team_id",
  "away_team_id",
  "lineup_home_ids",
  "lineup_away_ids",
  "qtr",
  "type_text",
  "shooting_play",
  "scoring_play",
  "score_value"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# helper to pick SHOT_ZONE_BASIC if present
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

sz_col <- .pick_col(nbapbp_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Mid-Range base (lineup-aware) ------------------------------------------- #
midr_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_midr = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) == "mid-range"
    } else {
      stringr::str_detect(type_text, stringr::regex("mid[- ]?range", ignore_case = TRUE))
    },
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(is_midr, shot, !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    midr_fga = TRUE,
    midr_fgm = make & !is.na(pts) & pts > 0L,
    midr_pts = dplyr::if_else(midr_fgm, pts, 0L)  # should be 2 per make
  )

# ---- Quarter tallies (Q1–Q6) ------------------------------------------------------ #
midr_qtr <- midr_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_MIDR_FGA = sum(midr_fga, na.rm = TRUE),
    L_MIDR_FGM = sum(midr_fgm, na.rm = TRUE),
    L_MIDR_PTS = sum(midr_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_MIDR_FGA = 0L,
      L_MIDR_FGM = 0L,
      L_MIDR_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_MIDR_FGA, L_MIDR_FGM, L_MIDR_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies -------------------------------------------------- #
midr_cgs <- midr_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_MIDR_FGA_CGS = sum(midr_fga, na.rm = TRUE),
    L_MIDR_FGM_CGS = sum(midr_fgm, na.rm = TRUE),
    L_MIDR_PTS_CGS = sum(midr_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into lineup_summary_df + coalesce + OT masking via HAS_Q5/HAS_Q6 -------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    midr_qtr,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::left_join(
    midr_cgs,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(L_MIDR_FGA_Q1, L_MIDR_FGA_Q2, L_MIDR_FGA_Q3, L_MIDR_FGA_Q4,
        L_MIDR_FGA_Q5, L_MIDR_FGA_Q6, L_MIDR_FGA_CGS,
        L_MIDR_FGM_Q1, L_MIDR_FGM_Q2, L_MIDR_FGM_Q3, L_MIDR_FGM_Q4,
        L_MIDR_FGM_Q5, L_MIDR_FGM_Q6, L_MIDR_FGM_CGS,
        L_MIDR_PTS_Q1, L_MIDR_PTS_Q2, L_MIDR_PTS_Q3, L_MIDR_PTS_Q4,
        L_MIDR_PTS_Q5, L_MIDR_PTS_Q6, L_MIDR_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # expose OT only if those OTs happened (HAS_Q5 / HAS_Q6)
    L_MIDR_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_MIDR_FGA_Q5, 0L),
    L_MIDR_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_MIDR_FGM_Q5, 0L),
    L_MIDR_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_MIDR_PTS_Q5, 0L),
    
    L_MIDR_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_MIDR_FGA_Q6, 0L),
    L_MIDR_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_MIDR_FGM_Q6, 0L),
    L_MIDR_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_MIDR_PTS_Q6, 0L)
  ) %>%
  # ---- Derived metrics at lineup level ---------------------------------------------
dplyr::mutate(
  # FG% (and eFG% = FG% for 2PT)
  L_MIDR_PCT_Q1  = safe_div(L_MIDR_FGM_Q1 , L_MIDR_FGA_Q1),
  L_MIDR_PCT_Q2  = safe_div(L_MIDR_FGM_Q2 , L_MIDR_FGA_Q2),
  L_MIDR_PCT_Q3  = safe_div(L_MIDR_FGM_Q3 , L_MIDR_FGA_Q3),
  L_MIDR_PCT_Q4  = safe_div(L_MIDR_FGM_Q4 , L_MIDR_FGA_Q4),
  L_MIDR_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                  safe_div(L_MIDR_FGM_Q5, L_MIDR_FGA_Q5), NA_real_),
  L_MIDR_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                  safe_div(L_MIDR_FGM_Q6, L_MIDR_FGA_Q6), NA_real_),
  L_MIDR_PCT_CGS = safe_div(L_MIDR_FGM_CGS, L_MIDR_FGA_CGS),
  
  L_MIDR_EFG_PCT_Q1  = L_MIDR_PCT_Q1,
  L_MIDR_EFG_PCT_Q2  = L_MIDR_PCT_Q2,
  L_MIDR_EFG_PCT_Q3  = L_MIDR_PCT_Q3,
  L_MIDR_EFG_PCT_Q4  = L_MIDR_PCT_Q4,
  L_MIDR_EFG_PCT_Q5  = L_MIDR_PCT_Q5,
  L_MIDR_EFG_PCT_Q6  = L_MIDR_PCT_Q6,
  L_MIDR_EFG_PCT_CGS = L_MIDR_PCT_CGS,
  
  # PPP (per mid-range FGA)
  L_MIDR_PPP_Q1  = safe_div(L_MIDR_PTS_Q1 , L_MIDR_FGA_Q1),
  L_MIDR_PPP_Q2  = safe_div(L_MIDR_PTS_Q2 , L_MIDR_FGA_Q2),
  L_MIDR_PPP_Q3  = safe_div(L_MIDR_PTS_Q3 , L_MIDR_FGA_Q3),
  L_MIDR_PPP_Q4  = safe_div(L_MIDR_PTS_Q4 , L_MIDR_FGA_Q4),
  L_MIDR_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                  safe_div(L_MIDR_PTS_Q5, L_MIDR_FGA_Q5), NA_real_),
  L_MIDR_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                  safe_div(L_MIDR_PTS_Q6, L_MIDR_FGA_Q6), NA_real_),
  L_MIDR_PPP_CGS = safe_div(L_MIDR_PTS_CGS, L_MIDR_FGA_CGS),
  
  # RATE = mid-range FGA / total lineup FGA (assumes LINEUP_FGA_* exist)
  L_MIDR_RATE_Q1  = safe_div(L_MIDR_FGA_Q1 , LINEUP_FGA_Q1 ),
  L_MIDR_RATE_Q2  = safe_div(L_MIDR_FGA_Q2 , LINEUP_FGA_Q2 ),
  L_MIDR_RATE_Q3  = safe_div(L_MIDR_FGA_Q3 , LINEUP_FGA_Q3 ),
  L_MIDR_RATE_Q4  = safe_div(L_MIDR_FGA_Q4 , LINEUP_FGA_Q4 ),
  L_MIDR_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                   safe_div(L_MIDR_FGA_Q5, LINEUP_FGA_Q5), NA_real_),
  L_MIDR_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                   safe_div(L_MIDR_FGA_Q6, LINEUP_FGA_Q6), NA_real_),
  L_MIDR_RATE_CGS = safe_div(L_MIDR_FGA_CGS, LINEUP_FGA_CGS),
  
  # PTS SHARE = mid-range PTS / lineup total PTS (assumes LINEUP_PTS_* exist)
  L_MIDR_PTSHR_Q1  = safe_div(L_MIDR_PTS_Q1 , LINEUP_PTS_Q1),
  L_MIDR_PTSHR_Q2  = safe_div(L_MIDR_PTS_Q2 , LINEUP_PTS_Q2),
  L_MIDR_PTSHR_Q3  = safe_div(L_MIDR_PTS_Q3 , LINEUP_PTS_Q3),
  L_MIDR_PTSHR_Q4  = safe_div(L_MIDR_PTS_Q4 , LINEUP_PTS_Q4),
  L_MIDR_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                    safe_div(L_MIDR_PTS_Q5 , LINEUP_PTS_Q5), NA_real_),
  L_MIDR_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                    safe_div(L_MIDR_PTS_Q6 , LINEUP_PTS_Q6), NA_real_),
  L_MIDR_PTSHR_CGS = safe_div(L_MIDR_PTS_CGS, LINEUP_PTS_CGS)
)

rm(midr_base, midr_qtr, midr_cgs)
message("[✓] Mid-Range (2PT) — Q1–Q6 (OT-aware) + CGS lineup section complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Mid-Range Shooting Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Rim (Restricted Area) Shooting Data Aggregation Section (LINEUP) ====
# RIM (LINEUP): FGA, FGM, FG%, PPP, PTS, RATE, PTS SHARE
# Q1–Q6 (OT-aware via HAS_Q5/HAS_Q6) + CGS
# Source: nbapbp_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id",
  "team_id",
  "home_team_id",
  "away_team_id",
  "lineup_home_ids",
  "lineup_away_ids",
  "qtr",
  "type_text",
  "shooting_play",
  "scoring_play",
  "score_value"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

# Prefer SHOT_ZONE_BASIC where available
sz_col <- .pick_col(nbapbp_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Rim base (lineup-aware) ------------------------------------------------- #
rim_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_rim = if (!is.na(sz_col)) {
      tolower(trimws(.data[[sz_col]])) == "restricted area"
    } else {
      stringr::str_detect(
        type_text,
        stringr::regex("restricted area|at rim|layup", ignore_case = TRUE)
      )
    },
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(is_rim, shot, !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    rim_fga = TRUE,
    rim_fgm = make & !is.na(pts) & pts > 0L,
    rim_pts = dplyr::if_else(rim_fgm, pts, 0L)
  )

# ---- Quarter tallies (Q1–Q6) ------------------------------------------------------ #
rim_qtr <- rim_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_RIM_FGA = sum(rim_fga, na.rm = TRUE),
    L_RIM_FGM = sum(rim_fgm, na.rm = TRUE),
    L_RIM_PTS = sum(rim_pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_RIM_FGA = 0L,
      L_RIM_FGM = 0L,
      L_RIM_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_RIM_FGA, L_RIM_FGM, L_RIM_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) ---------------------------------------------------------- #
rim_cgs <- rim_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_RIM_FGA_CGS = sum(rim_fga, na.rm = TRUE),
    L_RIM_FGM_CGS = sum(rim_fgm, na.rm = TRUE),
    L_RIM_PTS_CGS = sum(rim_pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join & fill into lineup_summary_df ------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    rim_qtr,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::left_join(
    rim_cgs,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(L_RIM_FGA_Q1, L_RIM_FGA_Q2, L_RIM_FGA_Q3, L_RIM_FGA_Q4,
        L_RIM_FGA_Q5, L_RIM_FGA_Q6, L_RIM_FGA_CGS,
        L_RIM_FGM_Q1, L_RIM_FGM_Q2, L_RIM_FGM_Q3, L_RIM_FGM_Q4,
        L_RIM_FGM_Q5, L_RIM_FGM_Q6, L_RIM_FGM_CGS,
        L_RIM_PTS_Q1, L_RIM_PTS_Q2, L_RIM_PTS_Q3, L_RIM_PTS_Q4,
        L_RIM_PTS_Q5, L_RIM_PTS_Q6, L_RIM_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking via HAS_Q5 / HAS_Q6
    L_RIM_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_RIM_FGA_Q5, 0L),
    L_RIM_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_RIM_FGM_Q5, 0L),
    L_RIM_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_RIM_PTS_Q5, 0L),
    
    L_RIM_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_RIM_FGA_Q6, 0L),
    L_RIM_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_RIM_FGM_Q6, 0L),
    L_RIM_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_RIM_PTS_Q6, 0L)
  ) %>%
  # ---- Derived metrics (lineup-level) ----------------------------------------------
dplyr::mutate(
  # FG% (and eFG% == FG% for 2PT-only)
  L_RIM_PCT_Q1  = safe_div(L_RIM_FGM_Q1 , L_RIM_FGA_Q1),
  L_RIM_PCT_Q2  = safe_div(L_RIM_FGM_Q2 , L_RIM_FGA_Q2),
  L_RIM_PCT_Q3  = safe_div(L_RIM_FGM_Q3 , L_RIM_FGA_Q3),
  L_RIM_PCT_Q4  = safe_div(L_RIM_FGM_Q4 , L_RIM_FGA_Q4),
  L_RIM_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                 safe_div(L_RIM_FGM_Q5, L_RIM_FGA_Q5), NA_real_),
  L_RIM_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                 safe_div(L_RIM_FGM_Q6, L_RIM_FGA_Q6), NA_real_),
  L_RIM_PCT_CGS = safe_div(L_RIM_FGM_CGS, L_RIM_FGA_CGS),
  
  L_RIM_EFG_PCT_Q1  = L_RIM_PCT_Q1,
  L_RIM_EFG_PCT_Q2  = L_RIM_PCT_Q2,
  L_RIM_EFG_PCT_Q3  = L_RIM_PCT_Q3,
  L_RIM_EFG_PCT_Q4  = L_RIM_PCT_Q4,
  L_RIM_EFG_PCT_Q5  = L_RIM_PCT_Q5,
  L_RIM_EFG_PCT_Q6  = L_RIM_PCT_Q6,
  L_RIM_EFG_PCT_CGS = L_RIM_PCT_CGS,
  
  # PPP (per rim FGA)
  L_RIM_PPP_Q1  = safe_div(L_RIM_PTS_Q1 , L_RIM_FGA_Q1),
  L_RIM_PPP_Q2  = safe_div(L_RIM_PTS_Q2 , L_RIM_FGA_Q2),
  L_RIM_PPP_Q3  = safe_div(L_RIM_PTS_Q3 , L_RIM_FGA_Q3),
  L_RIM_PPP_Q4  = safe_div(L_RIM_PTS_Q4 , L_RIM_FGA_Q4),
  L_RIM_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                 safe_div(L_RIM_PTS_Q5, L_RIM_FGA_Q5), NA_real_),
  L_RIM_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                 safe_div(L_RIM_PTS_Q6, L_RIM_FGA_Q6), NA_real_),
  L_RIM_PPP_CGS = safe_div(L_RIM_PTS_CGS, L_RIM_FGA_CGS),
  
  # Rate = rim FGA / total lineup FGA (assumes LINEUP_FGA_* exist)
  L_RIM_RATE_Q1  = safe_div(L_RIM_FGA_Q1 , LINEUP_FGA_Q1),
  L_RIM_RATE_Q2  = safe_div(L_RIM_FGA_Q2 , LINEUP_FGA_Q2),
  L_RIM_RATE_Q3  = safe_div(L_RIM_FGA_Q3 , LINEUP_FGA_Q3),
  L_RIM_RATE_Q4  = safe_div(L_RIM_FGA_Q4 , LINEUP_FGA_Q4),
  L_RIM_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                  safe_div(L_RIM_FGA_Q5, LINEUP_FGA_Q5), NA_real_),
  L_RIM_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                  safe_div(L_RIM_FGA_Q6, LINEUP_FGA_Q6), NA_real_),
  L_RIM_RATE_CGS = safe_div(L_RIM_FGA_CGS, LINEUP_FGA_CGS),
  
  # PTS SHARE = rim PTS / lineup PTS (assumes LINEUP_PTS_* exist)
  L_RIM_PTSHR_Q1  = safe_div(L_RIM_PTS_Q1 , LINEUP_PTS_Q1),
  L_RIM_PTSHR_Q2  = safe_div(L_RIM_PTS_Q2 , LINEUP_PTS_Q2),
  L_RIM_PTSHR_Q3  = safe_div(L_RIM_PTS_Q3 , LINEUP_PTS_Q3),
  L_RIM_PTSHR_Q4  = safe_div(L_RIM_PTS_Q4 , LINEUP_PTS_Q4),
  L_RIM_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                   safe_div(L_RIM_PTS_Q5 , LINEUP_PTS_Q5), NA_real_),
  L_RIM_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                   safe_div(L_RIM_PTS_Q6 , LINEUP_PTS_Q6), NA_real_),
  L_RIM_PTSHR_CGS = safe_div(L_RIM_PTS_CGS, LINEUP_PTS_CGS)
)

rm(rim_base, rim_qtr, rim_cgs)
message("[✓] Rim (Restricted Area) — Q1–Q6 (OT-aware) + CGS lineup section complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Rim (Restricted Area) Shooting Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Putback Shooting Data Aggregation Section (LINEUP) ====
# PUTBACKS (LINEUP): FGA, FGM, FG%, PPP, PTS, RATE, PTS SHARE
# Q1–Q6 (OT-aware via HAS_Q5/HAS_Q6) + CGS
# Source: nbapbp_df
# Detection: type_text contains "putback" (case-insensitive).
# ==================================================================================== #

stopifnot(all(c(
  "game_id",
  "team_id",
  "home_team_id",
  "away_team_id",
  "lineup_home_ids",
  "lineup_away_ids",
  "qtr",
  "type_text",
  "shooting_play",
  "scoring_play",
  "score_value"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build Putback base (lineup-aware) --------------------------------------------- #
putb_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_putb = stringr::str_detect(
      type_text,
      stringr::regex("\\bputback\\b", ignore_case = TRUE)
    ),
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(is_putb, shot, !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    putb_fga = TRUE,
    putb_fgm = make & !is.na(pts) & pts > 0L,
    putb_pts = dplyr::if_else(putb_fgm, pts, 0L)  # usually 2 per make
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
putb_qtr <- putb_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_PUTB_FGA = sum(putb_fga, na.rm = TRUE),
    L_PUTB_FGM = sum(putb_fgm, na.rm = TRUE),
    L_PUTB_PTS = sum(putb_pts, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_PUTB_FGA = 0L,
      L_PUTB_FGM = 0L,
      L_PUTB_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_PUTB_FGA, L_PUTB_FGM, L_PUTB_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
putb_cgs <- putb_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_PUTB_FGA_CGS = sum(putb_fga, na.rm = TRUE),
    L_PUTB_FGM_CGS = sum(putb_fgm, na.rm = TRUE),
    L_PUTB_PTS_CGS = sum(putb_pts, na.rm = TRUE),
    .groups        = "drop"
  )

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    putb_qtr,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::left_join(
    putb_cgs,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::mutate(
    # Coalesce all raw counts
    dplyr::across(
      c(L_PUTB_FGA_Q1, L_PUTB_FGA_Q2, L_PUTB_FGA_Q3, L_PUTB_FGA_Q4,
        L_PUTB_FGA_Q5, L_PUTB_FGA_Q6, L_PUTB_FGA_CGS,
        L_PUTB_FGM_Q1, L_PUTB_FGM_Q2, L_PUTB_FGM_Q3, L_PUTB_FGM_Q4,
        L_PUTB_FGM_Q5, L_PUTB_FGM_Q6, L_PUTB_FGM_CGS,
        L_PUTB_PTS_Q1, L_PUTB_PTS_Q2, L_PUTB_PTS_Q3, L_PUTB_PTS_Q4,
        L_PUTB_PTS_Q5, L_PUTB_PTS_Q6, L_PUTB_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking via HAS_Q5/HAS_Q6
    L_PUTB_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_PUTB_FGA_Q5, 0L),
    L_PUTB_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_PUTB_FGM_Q5, 0L),
    L_PUTB_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_PUTB_PTS_Q5, 0L),
    
    L_PUTB_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_PUTB_FGA_Q6, 0L),
    L_PUTB_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_PUTB_FGM_Q6, 0L),
    L_PUTB_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_PUTB_PTS_Q6, 0L),
    
    L_PUTB_FGA_CGS = dplyr::coalesce(L_PUTB_FGA_CGS, 0L),
    L_PUTB_FGM_CGS = dplyr::coalesce(L_PUTB_FGM_CGS, 0L),
    L_PUTB_PTS_CGS = dplyr::coalesce(L_PUTB_PTS_CGS, 0L)
  ) %>%
  # ---- Derived metrics (lineup-level) ---------------------------------------------- #
  dplyr::mutate(
    # FG% (and eFG% identical for 2PT)
    L_PUTB_PCT_Q1  = safe_div(L_PUTB_FGM_Q1 , L_PUTB_FGA_Q1),
    L_PUTB_PCT_Q2  = safe_div(L_PUTB_FGM_Q2 , L_PUTB_FGA_Q2),
    L_PUTB_PCT_Q3  = safe_div(L_PUTB_FGM_Q3 , L_PUTB_FGA_Q3),
    L_PUTB_PCT_Q4  = safe_div(L_PUTB_FGM_Q4 , L_PUTB_FGA_Q4),
    L_PUTB_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                    safe_div(L_PUTB_FGM_Q5, L_PUTB_FGA_Q5),
                                    NA_real_),
    L_PUTB_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                    safe_div(L_PUTB_FGM_Q6, L_PUTB_FGA_Q6),
                                    NA_real_),
    L_PUTB_PCT_CGS = safe_div(L_PUTB_FGM_CGS, L_PUTB_FGA_CGS),
    
    L_PUTB_EFG_PCT_Q1  = L_PUTB_PCT_Q1,
    L_PUTB_EFG_PCT_Q2  = L_PUTB_PCT_Q2,
    L_PUTB_EFG_PCT_Q3  = L_PUTB_PCT_Q3,
    L_PUTB_EFG_PCT_Q4  = L_PUTB_PCT_Q4,
    L_PUTB_EFG_PCT_Q5  = L_PUTB_PCT_Q5,
    L_PUTB_EFG_PCT_Q6  = L_PUTB_PCT_Q6,
    L_PUTB_EFG_PCT_CGS = L_PUTB_PCT_CGS,
    
    # PPP
    L_PUTB_PPP_Q1  = safe_div(L_PUTB_PTS_Q1 , L_PUTB_FGA_Q1),
    L_PUTB_PPP_Q2  = safe_div(L_PUTB_PTS_Q2 , L_PUTB_FGA_Q2),
    L_PUTB_PPP_Q3  = safe_div(L_PUTB_PTS_Q3 , L_PUTB_FGA_Q3),
    L_PUTB_PPP_Q4  = safe_div(L_PUTB_PTS_Q4 , L_PUTB_FGA_Q4),
    L_PUTB_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                    safe_div(L_PUTB_PTS_Q5, L_PUTB_FGA_Q5),
                                    NA_real_),
    L_PUTB_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                    safe_div(L_PUTB_PTS_Q6, L_PUTB_FGA_Q6),
                                    NA_real_),
    L_PUTB_PPP_CGS = safe_div(L_PUTB_PTS_CGS, L_PUTB_FGA_CGS),
    
    # Point Share vs lineup points (assumes LINEUP_PTS_* exist)
    L_PUTB_PTSHR_Q1  = safe_div(L_PUTB_PTS_Q1 , LINEUP_PTS_Q1),
    L_PUTB_PTSHR_Q2  = safe_div(L_PUTB_PTS_Q2 , LINEUP_PTS_Q2),
    L_PUTB_PTSHR_Q3  = safe_div(L_PUTB_PTS_Q3 , LINEUP_PTS_Q3),
    L_PUTB_PTSHR_Q4  = safe_div(L_PUTB_PTS_Q4 , LINEUP_PTS_Q4),
    L_PUTB_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                      safe_div(L_PUTB_PTS_Q5, LINEUP_PTS_Q5),
                                      NA_real_),
    L_PUTB_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                      safe_div(L_PUTB_PTS_Q6, LINEUP_PTS_Q6),
                                      NA_real_),
    L_PUTB_PTSHR_CGS = safe_div(L_PUTB_PTS_CGS, LINEUP_PTS_CGS),
    
    # Rate vs total lineup FGA (assumes LINEUP_FGA_* exist)
    L_PUTB_RATE_Q1  = safe_div(L_PUTB_FGA_Q1 , LINEUP_FGA_Q1),
    L_PUTB_RATE_Q2  = safe_div(L_PUTB_FGA_Q2 , LINEUP_FGA_Q2),
    L_PUTB_RATE_Q3  = safe_div(L_PUTB_FGA_Q3 , LINEUP_FGA_Q3),
    L_PUTB_RATE_Q4  = safe_div(L_PUTB_FGA_Q4 , LINEUP_FGA_Q4),
    L_PUTB_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                     safe_div(L_PUTB_FGA_Q5, LINEUP_FGA_Q5),
                                     NA_real_),
    L_PUTB_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                     safe_div(L_PUTB_FGA_Q6, LINEUP_FGA_Q6),
                                     NA_real_),
    L_PUTB_RATE_CGS = safe_div(L_PUTB_FGA_CGS, LINEUP_FGA_CGS)
  )

rm(putb_base, putb_qtr, putb_cgs)
message("[✓] Putbacks (LINEUP) — Q1–Q6 (OT-aware) + CGS section complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Putback Shooting Data Aggregation Section (5-Man LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Corner 3PT Shooting Data Aggregation Section (LINEUP) ====
# CORNER 3 (LINEUP): FGA, FGM, FG%, EFG%, PPP, PTS, PTSHR, RATE
# Q1–Q6 (OT-aware via HAS_Q5/HAS_Q6) + CGS
# Source: nbapbp_df
# Detection: SHOT_ZONE_BASIC in {"Right Corner 3","Left Corner 3"};
#            fallback text contains "Corner 3".
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "qtr","type_text",
  "shooting_play","scoring_play","score_value"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(nbapbp_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Corner-3 base (lineup-aware) -------------------------------------------- #
cnr3_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_corner3 = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("right corner 3","left corner 3")
    } else {
      stringr::str_detect(type_text, stringr::regex("corner\\s*3", ignore_case = TRUE))
    },
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(is_corner3, shot, !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    c3_fga = TRUE,
    c3_fgm = make & !is.na(pts) & pts > 0L,
    c3_pts = dplyr::if_else(c3_fgm, pts, 0L)  # should be 3 per make
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
cnr3_qtr <- cnr3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_CNR_3PT_FGA = sum(c3_fga, na.rm = TRUE),
    L_CNR_3PT_FGM = sum(c3_fgm, na.rm = TRUE),
    L_CNR_3PT_PTS = sum(c3_pts, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_CNR_3PT_FGA = 0L,
      L_CNR_3PT_FGM = 0L,
      L_CNR_3PT_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_CNR_3PT_FGA, L_CNR_3PT_FGM, L_CNR_3PT_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
cnr3_cgs <- cnr3_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_CNR_3PT_FGA_CGS = sum(c3_fga, na.rm = TRUE),
    L_CNR_3PT_FGM_CGS = sum(c3_fgm, na.rm = TRUE),
    L_CNR_3PT_PTS_CGS = sum(c3_pts, na.rm = TRUE),
    .groups           = "drop"
  )

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    cnr3_qtr,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::left_join(
    cnr3_cgs,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::mutate(
    # Coalesce all raw counts
    dplyr::across(
      c(L_CNR_3PT_FGA_Q1, L_CNR_3PT_FGA_Q2, L_CNR_3PT_FGA_Q3, L_CNR_3PT_FGA_Q4,
        L_CNR_3PT_FGA_Q5, L_CNR_3PT_FGA_Q6, L_CNR_3PT_FGA_CGS,
        L_CNR_3PT_FGM_Q1, L_CNR_3PT_FGM_Q2, L_CNR_3PT_FGM_Q3, L_CNR_3PT_FGM_Q4,
        L_CNR_3PT_FGM_Q5, L_CNR_3PT_FGM_Q6, L_CNR_3PT_FGM_CGS,
        L_CNR_3PT_PTS_Q1, L_CNR_3PT_PTS_Q2, L_CNR_3PT_PTS_Q3, L_CNR_3PT_PTS_Q4,
        L_CNR_3PT_PTS_Q5, L_CNR_3PT_PTS_Q6, L_CNR_3PT_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking via HAS_Q5/HAS_Q6
    L_CNR_3PT_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_CNR_3PT_FGA_Q5, 0L),
    L_CNR_3PT_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_CNR_3PT_FGM_Q5, 0L),
    L_CNR_3PT_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_CNR_3PT_PTS_Q5, 0L),
    
    L_CNR_3PT_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_CNR_3PT_FGA_Q6, 0L),
    L_CNR_3PT_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_CNR_3PT_FGM_Q6, 0L),
    L_CNR_3PT_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_CNR_3PT_PTS_Q6, 0L),
    
    L_CNR_3PT_FGA_CGS = dplyr::coalesce(L_CNR_3PT_FGA_CGS, 0L),
    L_CNR_3PT_FGM_CGS = dplyr::coalesce(L_CNR_3PT_FGM_CGS, 0L),
    L_CNR_3PT_PTS_CGS = dplyr::coalesce(L_CNR_3PT_PTS_CGS, 0L)
  ) %>%
  # ---- Derived metrics (lineup-level) ----------------------------------------------- #
  dplyr::mutate(
    # FG%
    L_CNR_3PT_PCT_Q1  = safe_div(L_CNR_3PT_FGM_Q1 , L_CNR_3PT_FGA_Q1),
    L_CNR_3PT_PCT_Q2  = safe_div(L_CNR_3PT_FGM_Q2 , L_CNR_3PT_FGA_Q2),
    L_CNR_3PT_PCT_Q3  = safe_div(L_CNR_3PT_FGM_Q3 , L_CNR_3PT_FGA_Q3),
    L_CNR_3PT_PCT_Q4  = safe_div(L_CNR_3PT_FGM_Q4 , L_CNR_3PT_FGA_Q4),
    L_CNR_3PT_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                       safe_div(L_CNR_3PT_FGM_Q5, L_CNR_3PT_FGA_Q5),
                                       NA_real_),
    L_CNR_3PT_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                       safe_div(L_CNR_3PT_FGM_Q6, L_CNR_3PT_FGA_Q6),
                                       NA_real_),
    L_CNR_3PT_PCT_CGS = safe_div(L_CNR_3PT_FGM_CGS, L_CNR_3PT_FGA_CGS),
    
    # eFG% (3PT => FGM * 1.5)
    L_CNR_3PT_EFG_PCT_Q1  = safe_div(L_CNR_3PT_FGM_Q1  * 1.5, L_CNR_3PT_FGA_Q1),
    L_CNR_3PT_EFG_PCT_Q2  = safe_div(L_CNR_3PT_FGM_Q2  * 1.5, L_CNR_3PT_FGA_Q2),
    L_CNR_3PT_EFG_PCT_Q3  = safe_div(L_CNR_3PT_FGM_Q3  * 1.5, L_CNR_3PT_FGA_Q3),
    L_CNR_3PT_EFG_PCT_Q4  = safe_div(L_CNR_3PT_FGM_Q4  * 1.5, L_CNR_3PT_FGA_Q4),
    L_CNR_3PT_EFG_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                           safe_div(L_CNR_3PT_FGM_Q5 * 1.5, L_CNR_3PT_FGA_Q5),
                                           NA_real_),
    L_CNR_3PT_EFG_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                           safe_div(L_CNR_3PT_FGM_Q6 * 1.5, L_CNR_3PT_FGA_Q6),
                                           NA_real_),
    L_CNR_3PT_EFG_PCT_CGS = safe_div(L_CNR_3PT_FGM_CGS * 1.5, L_CNR_3PT_FGA_CGS),
    
    # PPP (points / attempts)
    L_CNR_3PT_PPP_Q1  = safe_div(L_CNR_3PT_PTS_Q1 , L_CNR_3PT_FGA_Q1),
    L_CNR_3PT_PPP_Q2  = safe_div(L_CNR_3PT_PTS_Q2 , L_CNR_3PT_FGA_Q2),
    L_CNR_3PT_PPP_Q3  = safe_div(L_CNR_3PT_PTS_Q3 , L_CNR_3PT_FGA_Q3),
    L_CNR_3PT_PPP_Q4  = safe_div(L_CNR_3PT_PTS_Q4 , L_CNR_3PT_FGA_Q4),
    L_CNR_3PT_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                       safe_div(L_CNR_3PT_PTS_Q5, L_CNR_3PT_FGA_Q5),
                                       NA_real_),
    L_CNR_3PT_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                       safe_div(L_CNR_3PT_PTS_Q6, L_CNR_3PT_FGA_Q6),
                                       NA_real_),
    L_CNR_3PT_PPP_CGS = safe_div(L_CNR_3PT_PTS_CGS, L_CNR_3PT_FGA_CGS),
    
    # Point share vs lineup points (assumes LINEUP_PTS_* exist)
    L_CNR_3PT_PTSHR_Q1  = safe_div(L_CNR_3PT_PTS_Q1 , LINEUP_PTS_Q1),
    L_CNR_3PT_PTSHR_Q2  = safe_div(L_CNR_3PT_PTS_Q2 , LINEUP_PTS_Q2),
    L_CNR_3PT_PTSHR_Q3  = safe_div(L_CNR_3PT_PTS_Q3 , LINEUP_PTS_Q3),
    L_CNR_3PT_PTSHR_Q4  = safe_div(L_CNR_3PT_PTS_Q4 , LINEUP_PTS_Q4),
    L_CNR_3PT_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                         safe_div(L_CNR_3PT_PTS_Q5, LINEUP_PTS_Q5),
                                         NA_real_),
    L_CNR_3PT_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                         safe_div(L_CNR_3PT_PTS_Q6, LINEUP_PTS_Q6),
                                         NA_real_),
    L_CNR_3PT_PTSHR_CGS = safe_div(L_CNR_3PT_PTS_CGS, LINEUP_PTS_CGS),
    
    # Rate vs total lineup FGA (assumes LINEUP_FGA_* exist)
    L_CNR_3PT_RATE_Q1  = safe_div(L_CNR_3PT_FGA_Q1 , LINEUP_FGA_Q1),
    L_CNR_3PT_RATE_Q2  = safe_div(L_CNR_3PT_FGA_Q2 , LINEUP_FGA_Q2),
    L_CNR_3PT_RATE_Q3  = safe_div(L_CNR_3PT_FGA_Q3 , LINEUP_FGA_Q3),
    L_CNR_3PT_RATE_Q4  = safe_div(L_CNR_3PT_FGA_Q4 , LINEUP_FGA_Q4),
    L_CNR_3PT_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                        safe_div(L_CNR_3PT_FGA_Q5, LINEUP_FGA_Q5),
                                        NA_real_),
    L_CNR_3PT_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                        safe_div(L_CNR_3PT_FGA_Q6, LINEUP_FGA_Q6),
                                        NA_real_),
    L_CNR_3PT_RATE_CGS = safe_div(L_CNR_3PT_FGA_CGS, LINEUP_FGA_CGS)
  )

rm(cnr3_base, cnr3_qtr, cnr3_cgs)
message("[✓] Corner 3PT (LINEUP) — Q1–Q6 (OT-aware) + CGS section complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Corner 3PT Shooting Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Above the Break 3PT Shooting Data Aggregation Section (LINEUP) ====
# ABOVE THE BREAK 3 (LINEUP): FGA, FGM, FG%, EFG%, PPP, PTS, PTSHR, RATE
# Q1–Q6 (OT-aware via HAS_Q5/HAS_Q6) + CGS
# Source: nbapbp_df
# Detection: SHOT_ZONE_BASIC == "Above the Break 3"; fallback text contains
#            "above the break".
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "qtr","type_text",
  "shooting_play","scoring_play","score_value"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
.pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}
sz_col <- .pick_col(nbapbp_df, c("SHOT_ZONE_BASIC","shot_zone_basic","Shot_Zone_Basic"))

# ---- Build Above-the-Break base (lineup-aware) ------------------------------------- #
atb3_base <- nbapbp_df %>%
  dplyr::mutate(
    qtr  = suppressWarnings(as.integer(qtr)),
    shot = to_bool(shooting_play),
    make = to_bool(scoring_play),
    pts  = suppressWarnings(as.integer(score_value)),
    is_atb3 = if (!is.na(sz_col)) {
      tolower(.data[[sz_col]]) %in% c("above the break 3")
    } else {
      stringr::str_detect(
        type_text,
        stringr::regex("above\\s*the\\s*break", ignore_case = TRUE)
      )
    },
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(is_atb3, shot, !is.na(lineup_key)) %>%
  dplyr::transmute(
    game_id,
    team_id,
    lineup_key,
    qtr,
    atb3_fga = TRUE,
    atb3_fgm = make & !is.na(pts) & pts > 0L,
    atb3_pts = dplyr::if_else(atb3_fgm, pts, 0L)
  )

# ---- Quarter tallies (Q1–Q6) -------------------------------------------------------- #
atb3_qtr <- atb3_base %>%
  dplyr::filter(qtr %in% 1:6) %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    L_ATB_3PT_FGA = sum(atb3_fga, na.rm = TRUE),
    L_ATB_3PT_FGM = sum(atb3_fgm, na.rm = TRUE),
    L_ATB_3PT_PTS = sum(atb3_pts, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(
    qtr = 1:6,
    fill = list(
      L_ATB_3PT_FGA = 0L,
      L_ATB_3PT_FGM = 0L,
      L_ATB_3PT_PTS = 0L
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(L_ATB_3PT_FGA, L_ATB_3PT_FGM, L_ATB_3PT_PTS),
    values_fill = 0L,
    names_sep   = "_Q"
  )

# ---- Complete game (CGS) tallies ---------------------------------------------------- #
atb3_cgs <- atb3_base %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_ATB_3PT_FGA_CGS = sum(atb3_fga, na.rm = TRUE),
    L_ATB_3PT_FGM_CGS = sum(atb3_fgm, na.rm = TRUE),
    L_ATB_3PT_PTS_CGS = sum(atb3_pts, na.rm = TRUE),
    .groups           = "drop"
  )

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    atb3_qtr,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::left_join(
    atb3_cgs,
    by = c(
      "ESPN_GAME_ID" = "game_id",
      "ESPN_TEAM_ID" = "team_id",
      "LINEUP_KEY"   = "lineup_key"
    )
  ) %>%
  dplyr::mutate(
    # Coalesce raw counts
    dplyr::across(
      c(L_ATB_3PT_FGA_Q1, L_ATB_3PT_FGA_Q2, L_ATB_3PT_FGA_Q3, L_ATB_3PT_FGA_Q4,
        L_ATB_3PT_FGA_Q5, L_ATB_3PT_FGA_Q6, L_ATB_3PT_FGA_CGS,
        L_ATB_3PT_FGM_Q1, L_ATB_3PT_FGM_Q2, L_ATB_3PT_FGM_Q3, L_ATB_3PT_FGM_Q4,
        L_ATB_3PT_FGM_Q5, L_ATB_3PT_FGM_Q6, L_ATB_3PT_FGM_CGS,
        L_ATB_3PT_PTS_Q1, L_ATB_3PT_PTS_Q2, L_ATB_3PT_PTS_Q3, L_ATB_3PT_PTS_Q4,
        L_ATB_3PT_PTS_Q5, L_ATB_3PT_PTS_Q6, L_ATB_3PT_PTS_CGS),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking via HAS_Q5 / HAS_Q6
    L_ATB_3PT_FGA_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_ATB_3PT_FGA_Q5, 0L),
    L_ATB_3PT_FGM_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_ATB_3PT_FGM_Q5, 0L),
    L_ATB_3PT_PTS_Q5 = dplyr::if_else(HAS_Q5 == 1L, L_ATB_3PT_PTS_Q5, 0L),
    
    L_ATB_3PT_FGA_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_ATB_3PT_FGA_Q6, 0L),
    L_ATB_3PT_FGM_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_ATB_3PT_FGM_Q6, 0L),
    L_ATB_3PT_PTS_Q6 = dplyr::if_else(HAS_Q6 == 1L, L_ATB_3PT_PTS_Q6, 0L),
    
    L_ATB_3PT_FGA_CGS = dplyr::coalesce(L_ATB_3PT_FGA_CGS, 0L),
    L_ATB_3PT_FGM_CGS = dplyr::coalesce(L_ATB_3PT_FGM_CGS, 0L),
    L_ATB_3PT_PTS_CGS = dplyr::coalesce(L_ATB_3PT_PTS_CGS, 0L)
  ) %>%
  # ---- Derived metrics (lineup-level) ----------------------------------------------- #
  dplyr::mutate(
    # FG%
    L_ATB_3PT_PCT_Q1  = safe_div(L_ATB_3PT_FGM_Q1 , L_ATB_3PT_FGA_Q1),
    L_ATB_3PT_PCT_Q2  = safe_div(L_ATB_3PT_FGM_Q2 , L_ATB_3PT_FGA_Q2),
    L_ATB_3PT_PCT_Q3  = safe_div(L_ATB_3PT_FGM_Q3 , L_ATB_3PT_FGA_Q3),
    L_ATB_3PT_PCT_Q4  = safe_div(L_ATB_3PT_FGM_Q4 , L_ATB_3PT_FGA_Q4),
    L_ATB_3PT_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                       safe_div(L_ATB_3PT_FGM_Q5, L_ATB_3PT_FGA_Q5),
                                       NA_real_),
    L_ATB_3PT_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                       safe_div(L_ATB_3PT_FGM_Q6, L_ATB_3PT_FGA_Q6),
                                       NA_real_),
    L_ATB_3PT_PCT_CGS = safe_div(L_ATB_3PT_FGM_CGS, L_ATB_3PT_FGA_CGS),
    
    # eFG% (3PT => FGM * 1.5)
    L_ATB_3PT_EFG_PCT_Q1  = safe_div(L_ATB_3PT_FGM_Q1  * 1.5, L_ATB_3PT_FGA_Q1),
    L_ATB_3PT_EFG_PCT_Q2  = safe_div(L_ATB_3PT_FGM_Q2  * 1.5, L_ATB_3PT_FGA_Q2),
    L_ATB_3PT_EFG_PCT_Q3  = safe_div(L_ATB_3PT_FGM_Q3  * 1.5, L_ATB_3PT_FGA_Q3),
    L_ATB_3PT_EFG_PCT_Q4  = safe_div(L_ATB_3PT_FGM_Q4  * 1.5, L_ATB_3PT_FGA_Q4),
    L_ATB_3PT_EFG_PCT_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                           safe_div(L_ATB_3PT_FGM_Q5 * 1.5, L_ATB_3PT_FGA_Q5),
                                           NA_real_),
    L_ATB_3PT_EFG_PCT_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                           safe_div(L_ATB_3PT_FGM_Q6 * 1.5, L_ATB_3PT_FGA_Q6),
                                           NA_real_),
    L_ATB_3PT_EFG_PCT_CGS = safe_div(L_ATB_3PT_FGM_CGS * 1.5, L_ATB_3PT_FGA_CGS),
    
    # PPP
    L_ATB_3PT_PPP_Q1  = safe_div(L_ATB_3PT_PTS_Q1 , L_ATB_3PT_FGA_Q1),
    L_ATB_3PT_PPP_Q2  = safe_div(L_ATB_3PT_PTS_Q2 , L_ATB_3PT_FGA_Q2),
    L_ATB_3PT_PPP_Q3  = safe_div(L_ATB_3PT_PTS_Q3 , L_ATB_3PT_FGA_Q3),
    L_ATB_3PT_PPP_Q4  = safe_div(L_ATB_3PT_PTS_Q4 , L_ATB_3PT_FGA_Q4),
    L_ATB_3PT_PPP_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                       safe_div(L_ATB_3PT_PTS_Q5, L_ATB_3PT_FGA_Q5),
                                       NA_real_),
    L_ATB_3PT_PPP_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                       safe_div(L_ATB_3PT_PTS_Q6, L_ATB_3PT_FGA_Q6),
                                       NA_real_),
    L_ATB_3PT_PPP_CGS = safe_div(L_ATB_3PT_PTS_CGS, L_ATB_3PT_FGA_CGS),
    
    # Points share vs lineup points
    L_ATB_3PT_PTSHR_Q1  = safe_div(L_ATB_3PT_PTS_Q1 , LINEUP_PTS_Q1),
    L_ATB_3PT_PTSHR_Q2  = safe_div(L_ATB_3PT_PTS_Q2 , LINEUP_PTS_Q2),
    L_ATB_3PT_PTSHR_Q3  = safe_div(L_ATB_3PT_PTS_Q3 , LINEUP_PTS_Q3),
    L_ATB_3PT_PTSHR_Q4  = safe_div(L_ATB_3PT_PTS_Q4 , LINEUP_PTS_Q4),
    L_ATB_3PT_PTSHR_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                         safe_div(L_ATB_3PT_PTS_Q5, LINEUP_PTS_Q5),
                                         NA_real_),
    L_ATB_3PT_PTSHR_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                         safe_div(L_ATB_3PT_PTS_Q6, LINEUP_PTS_Q6),
                                         NA_real_),
    L_ATB_3PT_PTSHR_CGS = safe_div(L_ATB_3PT_PTS_CGS, LINEUP_PTS_CGS),
    
    # Rate vs total lineup FGA
    L_ATB_3PT_RATE_Q1  = safe_div(L_ATB_3PT_FGA_Q1 , LINEUP_FGA_Q1),
    L_ATB_3PT_RATE_Q2  = safe_div(L_ATB_3PT_FGA_Q2 , LINEUP_FGA_Q2),
    L_ATB_3PT_RATE_Q3  = safe_div(L_ATB_3PT_FGA_Q3 , LINEUP_FGA_Q3),
    L_ATB_3PT_RATE_Q4  = safe_div(L_ATB_3PT_FGA_Q4 , LINEUP_FGA_Q4),
    L_ATB_3PT_RATE_Q5  = dplyr::if_else(HAS_Q5 == 1L,
                                        safe_div(L_ATB_3PT_FGA_Q5, LINEUP_FGA_Q5),
                                        NA_real_),
    L_ATB_3PT_RATE_Q6  = dplyr::if_else(HAS_Q6 == 1L,
                                        safe_div(L_ATB_3PT_FGA_Q6, LINEUP_FGA_Q6),
                                        NA_real_),
    L_ATB_3PT_RATE_CGS = safe_div(L_ATB_3PT_FGA_CGS, LINEUP_FGA_CGS)
  )

rm(atb3_base, atb3_qtr, atb3_cgs)
message("[✓] Above the Break 3PT (LINEUP) — Q1–Q6 (OT-aware) + CGS section complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Above the Break 3PT Shooting Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Clutch Field Goal Data Aggregation Section (LINEUP) ====
# CLUTCH WINDOWS (LINEUP, raw counts + FG%)
# Windows: L1M (≤60s), L3M (≤120s), L5M (≤300s), L10M (≤600s)
# Source: nbapbp_df
# Creates, for WIN in {L1M,L3M,L5M,L10M}:
#   L_CLTH_WIN_FGA_Q1..Q6,  L_CLTH_WIN_FGA_CGS
#   L_CLTH_WIN_FGM_Q1..Q6,  L_CLTH_WIN_FGM_CGS
#   L_CLTH_WIN_PCT_Q1..Q6,  L_CLTH_WIN_PCT_CGS
# Clutch filter: score_diff <= 7 (team score vs opp score)
# OT masking via HAS_Q5 / HAS_Q6
# ==================================================================================== #

# --- 1. Check required columns (except score_diff, which we build here) --------------
required_cols <- c(
  "game_id","team_id","home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "qtr",
  "shooting_play","scoring_play",
  "clock_minutes","clock_seconds"
)

missing_cols <- setdiff(required_cols, names(nbapbp_df))
if (length(missing_cols) > 0) {
  stop(
    "Missing columns in nbapbp_df for clutch lineup section: ",
    paste(missing_cols, collapse = ", ")
  )
}

# --- 2. Build score_diff inside nbapbp_df if it doesn't exist yet --------------------
if (!"score_diff" %in% names(nbapbp_df)) {
  if (!all(c("home_score","away_score") %in% names(nbapbp_df))) {
    stop(
      "To construct score_diff in nbapbp_df, expected home_score and away_score ",
      "columns to be present."
    )
  }
  
  nbapbp_df <- nbapbp_df %>%
    dplyr::mutate(
      home_score = suppressWarnings(as.integer(home_score)),
      away_score = suppressWarnings(as.integer(away_score)),
      team_score = dplyr::if_else(team_id == home_team_id, home_score, away_score),
      opp_score  = dplyr::if_else(team_id == home_team_id, away_score, home_score),
      score_diff = abs(team_score - opp_score)
    )
}

stopifnot("score_diff" %in% names(nbapbp_df))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build clutch_base with lineup_key + t_sec (seconds remaining in quarter) ------ #
clutch_base <-
  nbapbp_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play),
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  # keep regulation + OT quarters (1..6), only if game is within 7 points
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(score_diff),
    score_diff <= 7,
    !is.na(lineup_key)
  )

# ---- Helper: compute per-quarter + CGS tallies for a time threshold ---------------- #
clutch_tallies <- function(base_df, threshold_sec, win_tag) {
  # per-quarter tallies (Q1–Q6)
  qtr_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
    dplyr::summarise(
      FGA = sum(shot, na.rm = TRUE),
      FGM = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(game_id, team_id, lineup_key) %>%
    tidyr::complete(
      qtr = 1:6,
      fill = list(FGA = 0L, FGM = 0L)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = c(FGA, FGM),
      values_fill = 0L,
      names_sep   = "_Q"
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_CLTH_", win_tag, "_", .x),
      .cols = dplyr::matches("^(FGA|FGM)_Q[1-6]$")
    )
  
  # CGS tallies
  cgs_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, lineup_key) %>%
    dplyr::summarise(
      FGA_CGS = sum(shot, na.rm = TRUE),
      FGM_CGS = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_CLTH_", win_tag, "_", .x),
      .cols = c(FGA_CGS, FGM_CGS)
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Compute windows: L1M, L3M, L5M, L10M ------------------------------------------ #
res_L1M  <- clutch_tallies(clutch_base,  60L, "L1M")
res_L3M  <- clutch_tallies(clutch_base, 120L, "L3M")
res_L5M  <- clutch_tallies(clutch_base, 300L, "L5M")
res_L10M <- clutch_tallies(clutch_base, 600L, "L10M")

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # L1M
  dplyr::left_join(
    res_L1M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_L1M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L3M
  dplyr::left_join(
    res_L3M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_L3M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L5M
  dplyr::left_join(
    res_L5M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_L5M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L10M
  dplyr::left_join(
    res_L10M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_L10M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Coalesce newly added numeric columns to 0, then OT masking via HAS_Q5/HAS_Q6
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_CLTH_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q[1-6]$"),
      ~ dplyr::coalesce(., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_CLTH_(L1M|L3M|L5M|L10M)_(FGA|FGM)_CGS$"),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking
    dplyr::across(
      tidyselect::matches("^L_CLTH_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q5$"),
      ~ dplyr::if_else(HAS_Q5 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_CLTH_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q6$"),
      ~ dplyr::if_else(HAS_Q6 == 1L, ., 0L)
    )
  )

# ---- FG% helper (uses HAS_Q5/HAS_Q6 + *_CGS fields) -------------------------------- #
clutch_pct <- function(df, tag) {
  fgm <- function(p) paste0("L_CLTH_", tag, "_FGM_", p)
  fga <- function(p) paste0("L_CLTH_", tag, "_FGA_", p)
  pct <- function(p) paste0("L_CLTH_", tag, "_PCT_", p)
  
  df %>%
    dplyr::mutate(
      !!pct("Q1")  := safe_div(.data[[fgm("Q1")]],  .data[[fga("Q1")]]),
      !!pct("Q2")  := safe_div(.data[[fgm("Q2")]],  .data[[fga("Q2")]]),
      !!pct("Q3")  := safe_div(.data[[fgm("Q3")]],  .data[[fga("Q3")]]),
      !!pct("Q4")  := safe_div(.data[[fgm("Q4")]],  .data[[fga("Q4")]]),
      !!pct("Q5")  := dplyr::if_else(
        .data[["HAS_Q5"]] == 1L,
        safe_div(.data[[fgm("Q5")]], .data[[fga("Q5")]]),
        NA_real_
      ),
      !!pct("Q6")  := dplyr::if_else(
        .data[["HAS_Q6"]] == 1L,
        safe_div(.data[[fgm("Q6")]], .data[[fga("Q6")]]),
        NA_real_
      ),
      !!pct("CGS") := safe_div(.data[[fgm("CGS")]], .data[[fga("CGS")]])
    )
}

lineup_summary_df <- lineup_summary_df %>%
  clutch_pct("L1M")  %>%
  clutch_pct("L3M")  %>%
  clutch_pct("L5M")  %>%
  clutch_pct("L10M")

rm(res_L10M, res_L1M, res_L3M, res_L5M, clutch_base, clutch_tallies, clutch_pct)
message("[✓] Clutch windows (L1M/L3M/L5M/L10M, LINEUP) — Q1–Q6 (OT-aware) FGA/FGM + PCT joined.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Clutch Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Clutch 3PT Field Goal Data Aggregation Section (LINEUP) ====
# Clutch 3PT Windows (raw counts + FG%)
# Windows: L1M (≤60s), L3M (≤120s), L5M (≤300s), L10M (≤600s)
# Source: nbapbp_df
# Creates (for WIN in {L1M,L3M,L5M,L10M}):
#   L_CLTH_3PT_WIN_FGA_Q1..Q6,  L_CLTH_3PT_WIN_FGA_CGS
#   L_CLTH_3PT_WIN_FGM_Q1..Q6,  L_CLTH_3PT_WIN_FGM_CGS
#   L_CLTH_3PT_WIN_PCT_Q1..Q6,  L_CLTH_3PT_WIN_PCT_CGS
# OT-aware via HAS_Q5 / HAS_Q6 flags in lineup_summary_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "qtr",
  "shooting_play","scoring_play",
  "clock_minutes","clock_seconds",
  "SHOT_ZONE_BASIC","score_diff"
) %in% names(nbapbp_df)))

to_bool  <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Build filtered base: only 3PT clutch shots (Q1–Q6, within 7 points) ----------- #
clutch_3pt_base <-
  nbapbp_df %>%
  dplyr::mutate(
    qtr   = suppressWarnings(as.integer(qtr)),
    t_sec = suppressWarnings(as.integer(clock_minutes)) * 60L +
      suppressWarnings(as.integer(clock_seconds)),
    shot  = to_bool(shooting_play),
    make  = to_bool(scoring_play),
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(score_diff),
    score_diff <= 7,
    shot,
    !is.na(lineup_key),
    SHOT_ZONE_BASIC %in% c("Above the Break 3", "Right Corner 3", "Left Corner 3")
  )

# ---- Helper: compute per-quarter and CGS tallies for each time threshold ------------ #
clutch_3pt_tallies <- function(base_df, threshold_sec, win_tag) {
  
  # per-quarter breakdown
  qtr_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
    dplyr::summarise(
      FGA = sum(shot, na.rm = TRUE),
      FGM = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(game_id, team_id, lineup_key) %>%
    tidyr::complete(
      qtr = 1:6,
      fill = list(FGA = 0L, FGM = 0L)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = c(FGA, FGM),
      values_fill = 0L,
      names_sep   = "_Q"
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_CLTH_3PT_", win_tag, "_", .x),
      .cols = dplyr::matches("^(FGA|FGM)_Q[1-6]$")
    )
  
  # full game (CGS)
  cgs_df <-
    base_df %>%
    dplyr::filter(t_sec <= threshold_sec, !is.na(t_sec)) %>%
    dplyr::group_by(game_id, team_id, lineup_key) %>%
    dplyr::summarise(
      FGA_CGS = sum(shot, na.rm = TRUE),
      FGM_CGS = sum(make, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_CLTH_3PT_", win_tag, "_", .x),
      .cols = c(FGA_CGS, FGM_CGS)
    )
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Compute for each window (L1M/L3M/L5M/L10M) ------------------------------------ #
res_3PT_L1M  <- clutch_3pt_tallies(clutch_3pt_base,  60L, "L1M")
res_3PT_L3M  <- clutch_3pt_tallies(clutch_3pt_base, 120L, "L3M")
res_3PT_L5M  <- clutch_3pt_tallies(clutch_3pt_base, 300L, "L5M")
res_3PT_L10M <- clutch_3pt_tallies(clutch_3pt_base, 600L, "L10M")

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # L1M
  dplyr::left_join(
    res_3PT_L1M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_3PT_L1M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L3M
  dplyr::left_join(
    res_3PT_L3M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_3PT_L3M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L5M
  dplyr::left_join(
    res_3PT_L5M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_3PT_L5M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # L10M
  dplyr::left_join(
    res_3PT_L10M$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_3PT_L10M$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # coalesce new columns to 0, then OT masking using HAS_Q5 / HAS_Q6
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_CLTH_3PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q[1-6]$"),
      ~ dplyr::coalesce(., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_CLTH_3PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_CGS$"),
      ~ dplyr::coalesce(., 0L)
    ),
    # OT masking
    dplyr::across(
      tidyselect::matches("^L_CLTH_3PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q5$"),
      ~ dplyr::if_else(HAS_Q5 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_CLTH_3PT_(L1M|L3M|L5M|L10M)_(FGA|FGM)_Q6$"),
      ~ dplyr::if_else(HAS_Q6 == 1L, ., 0L)
    )
  )

# ---- FG% calculation helper (OT-aware, lineup) ------------------------------------- #
clutch_3pt_pct <- function(df, tag) {
  fgm <- function(p) paste0("L_CLTH_3PT_", tag, "_FGM_", p)
  fga <- function(p) paste0("L_CLTH_3PT_", tag, "_FGA_", p)
  pct <- function(p) paste0("L_CLTH_3PT_", tag, "_PCT_", p)
  
  df %>%
    dplyr::mutate(
      !!pct("Q1")  := safe_div(.data[[fgm("Q1")]],  .data[[fga("Q1")]]),
      !!pct("Q2")  := safe_div(.data[[fgm("Q2")]],  .data[[fga("Q2")]]),
      !!pct("Q3")  := safe_div(.data[[fgm("Q3")]],  .data[[fga("Q3")]]),
      !!pct("Q4")  := safe_div(.data[[fgm("Q4")]],  .data[[fga("Q4")]]),
      !!pct("Q5")  := dplyr::if_else(
        .data[["HAS_Q5"]] == 1L,
        safe_div(.data[[fgm("Q5")]], .data[[fga("Q5")]]),
        NA_real_
      ),
      !!pct("Q6")  := dplyr::if_else(
        .data[["HAS_Q6"]] == 1L,
        safe_div(.data[[fgm("Q6")]], .data[[fga("Q6")]]),
        NA_real_
      ),
      !!pct("CGS") := safe_div(.data[[fgm("CGS")]], .data[[fga("CGS")]])
    )
}

# ---- Apply FG% for all clutch 3PT windows ------------------------------------------ #
lineup_summary_df <- lineup_summary_df %>%
  clutch_3pt_pct("L1M")  %>%
  clutch_3pt_pct("L3M")  %>%
  clutch_3pt_pct("L5M")  %>%
  clutch_3pt_pct("L10M")

rm(
  res_3PT_L10M, res_3PT_L1M, res_3PT_L3M, res_3PT_L5M,
  clutch_3pt_base, clutch_3pt_tallies, clutch_3pt_pct
)

message("[✓] Clutch 3PT windows (L1M/L3M/L5M/L10M, LINEUP, OT-aware) — FGA, FGM, and FG% joined.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Clutch 3PT Field Goal Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀






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





# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Turnovers Data Aggregation Section (LINEUP) ====
# Turnovers credited to DEFENSIVE lineup (opponent of committing team)
# Live Ball / Dead Ball / Bad Pass / All + Turnover%
# Source: nbapbp_df  →  lineup_summary_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids"
) %in% names(nbapbp_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Common base & patterns (Q1–Q6, OT-aware via HAS_Q5/HAS_Q6 in lineup_summary_df) ----
# NOTE: Stats are credited to the DEFENSIVE lineup:
#   committing team_id -> opponent team_id + opponent lineup_key
tov_base_common <- nbapbp_df %>%
  dplyr::mutate(
    qtr   = to_int(qtr),
    # defensive / opponent team + lineup key
    DEF_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    DEF_LINEUP_KEY = dplyr::if_else(
      team_id == home_team_id,   # offense = home → defense = away lineup
      lineup_away_ids,
      lineup_home_ids
    ),
    txt = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(DEF_LINEUP_KEY)
  )

# Patterns (case-insensitive)
pat_liveb <- stringr::regex("lost ball turnover|bad pass turnover|offensive foul turnover",
                            ignore_case = TRUE)
pat_badp  <- stringr::regex("bad pass turnover", ignore_case = TRUE)
pat_any   <- stringr::regex("turnover", ignore_case = TRUE)

# ---- Separate populations ----------------------------------------------------------- #
tov_liveb_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_liveb))

tov_all_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_any))

tov_deadb_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_any) &
                  !stringr::str_detect(txt, pat_liveb))

tov_badp_df <- tov_base_common %>%
  dplyr::filter(stringr::str_detect(txt, pat_badp))

# ---- Helper: tally by quarter + CGS, credited to defensive lineup ------------------- #
tally_tov_lineup <- function(df, name_prefix) {
  # Q1–Q6
  qtr_df <- df %>%
    dplyr::group_by(game_id, DEF_TEAM_ID, DEF_LINEUP_KEY, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, DEF_TEAM_ID, DEF_LINEUP_KEY) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(
      team_id    = DEF_TEAM_ID,
      lineup_key = DEF_LINEUP_KEY
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_", name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = DEF_TEAM_ID, lineup_key = DEF_LINEUP_KEY) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("L_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# Build pieces
res_liveb <- tally_tov_lineup(tov_liveb_df, "TOV_LIVEB")
res_deadb <- tally_tov_lineup(tov_deadb_df, "TOV_DEADB")
res_badp  <- tally_tov_lineup(tov_badp_df,  "TOV_BADP")
res_all   <- tally_tov_lineup(tov_all_df,   "TOV")       # feeds L_TOV_* directly

# ---- Join into lineup_summary_df ---------------------------------------------------- #
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # live-ball
  dplyr::left_join(
    res_liveb$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_liveb$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # dead-ball
  dplyr::left_join(
    res_deadb$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_deadb$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # bad pass
  dplyr::left_join(
    res_badp$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_badp$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # all turnovers
  dplyr::left_join(
    res_all$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_all$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # fill NA → 0 for all turnover counts (Q1–Q6 + CGS)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_(TOV(_(LIVEB|DEADB|BADP))?_(Q[1-6]|CGS))$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating with HAS_Q5/HAS_Q6 (if no OT, force Q5/Q6 to 0)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_(TOV(_(LIVEB|DEADB|BADP))?_Q5)$"),
      ~ dplyr::if_else(HAS_Q5 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_(TOV(_(LIVEB|DEADB|BADP))?_Q6)$"),
      ~ dplyr::if_else(HAS_Q6 == 1L, ., 0L)
    )
  )

# ---- Turnover Percentage (lineup-level) -------------------------------------------- #
# L_TOV_PCT_X = L_TOV_X / (LINEUP_FGA_X + 0.44*LINEUP_FTA_X + L_TOV_X) * 100

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    # Regulation
    L_TOV_PCT_Q1  = 100 * safe_div(
      L_TOV_Q1,
      LINEUP_FGA_Q1 + 0.44 * LINEUP_FTA_Q1 + L_TOV_Q1
    ),
    L_TOV_PCT_Q2  = 100 * safe_div(
      L_TOV_Q2,
      LINEUP_FGA_Q2 + 0.44 * LINEUP_FTA_Q2 + L_TOV_Q2
    ),
    L_TOV_PCT_Q3  = 100 * safe_div(
      L_TOV_Q3,
      LINEUP_FGA_Q3 + 0.44 * LINEUP_FTA_Q3 + L_TOV_Q3
    ),
    L_TOV_PCT_Q4  = 100 * safe_div(
      L_TOV_Q4,
      LINEUP_FGA_Q4 + 0.44 * LINEUP_FTA_Q4 + L_TOV_Q4
    ),
    # OT (only if lineup has those quarters)
    L_TOV_PCT_Q5  = dplyr::if_else(
      HAS_Q5 == 1L,
      100 * safe_div(
        L_TOV_Q5,
        LINEUP_FGA_Q5 + 0.44 * LINEUP_FTA_Q5 + L_TOV_Q5
      ),
      NA_real_
    ),
    L_TOV_PCT_Q6  = dplyr::if_else(
      HAS_Q6 == 1L,
      100 * safe_div(
        L_TOV_Q6,
        LINEUP_FGA_Q6 + 0.44 * LINEUP_FTA_Q6 + L_TOV_Q6
      ),
      NA_real_
    ),
    # CGS
    L_TOV_PCT_CGS = 100 * safe_div(
      L_TOV_CGS,
      LINEUP_FGA_CGS + 0.44 * LINEUP_FTA_CGS + L_TOV_CGS
    )
  )

rm(
  res_all, res_badp, res_deadb, res_liveb,
  tov_all_df, tov_badp_df, tov_base_common, tov_deadb_df, tov_liveb_df,
  tally_tov_lineup
)

message("[✓] Turnovers (live/dead/bad pass/all) — LINEUP-level, OT-aware Q1–Q6 + L_TOV% complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Turnovers Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Fouls Data Aggregation Section (LINEUP) ====
# Fouls credited to the OPPONENT lineup (team that benefits)
# Source: nbapbp_df  → lineup_summary_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids"
) %in% names(nbapbp_df)))

to_int <- function(x) suppressWarnings(as.integer(x))

# ---- Base with opponent team + lineup key (Q1–Q6) -----------------------------------
foul_base_lineup <- nbapbp_df %>%
  dplyr::mutate(
    qtr = to_int(qtr),
    # committing team → opponent team/lineup gets the credit
    OPP_TEAM_ID    = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    OPP_LINEUP_KEY = dplyr::if_else(
      team_id == home_team_id,
      lineup_away_ids,   # opponent is away
      lineup_home_ids    # opponent is home
    ),
    txt = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(OPP_LINEUP_KEY)
  )

# ---- Patterns (same as team version, on `txt`) --------------------------------------
pat_any_foul   <- stringr::regex("foul", ignore_case = TRUE)

pat_shoot_foul <- stringr::regex("shooting foul", ignore_case = TRUE)
pat_tech_foul  <- stringr::regex("technical foul", ignore_case = TRUE)
pat_flag_foul  <- stringr::regex("flagrant foul type 1|flagrant foul type 2",
                                 ignore_case = TRUE)

# Offensive fouls (as in team section)
pat_off_foul   <- stringr::regex(
  "offensive foul|offensive charge|offensive foul turnover|loose ball foul",
  ignore_case = TRUE
)

# Defensive fouls (as in team section)
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

# Charges drawn: "Offensive Charge" → credit to opponent lineup
pat_charge_drawn <- stringr::regex("offensive charge", ignore_case = TRUE)

# ---- Separate populations -----------------------------------------------------------
fouls_all_df   <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_any_foul))
fouls_shot_df  <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_shoot_foul))
fouls_tech_df  <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_tech_foul))
fouls_flag_df  <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_flag_foul))
fouls_off_df   <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_off_foul))
fouls_def_df   <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_def_foul))
charges_df     <- foul_base_lineup %>% dplyr::filter(stringr::str_detect(txt, pat_charge_drawn))

# ---- Helper: per-quarter (Q1–Q6) + CGS, credited to opponent lineup -----------------
tally_foul_lineup <- function(df, name_prefix) {
  # Q1–Q6 with complete quarters
  qtr_df <- df %>%
    dplyr::group_by(game_id, OPP_TEAM_ID, OPP_LINEUP_KEY, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(game_id, OPP_TEAM_ID, OPP_LINEUP_KEY) %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    dplyr::rename(
      team_id    = OPP_TEAM_ID,
      lineup_key = OPP_LINEUP_KEY
    ) %>%
    dplyr::rename_with(
      ~ paste0("L_", name_prefix, "_", .x),
      dplyr::starts_with("Q")
    )
  
  cgs_df <- df %>%
    dplyr::group_by(
      game_id,
      team_id    = OPP_TEAM_ID,
      lineup_key = OPP_LINEUP_KEY
    ) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("L_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# ---- Build tallies (LINEUP-level) ---------------------------------------------------
res_fouls_all  <- tally_foul_lineup(fouls_all_df,  "FOULS")
res_fouls_shot <- tally_foul_lineup(fouls_shot_df, "SHOT_FOULS")
res_fouls_tech <- tally_foul_lineup(fouls_tech_df, "TECH_FOULS")
res_fouls_flag <- tally_foul_lineup(fouls_flag_df, "FLAG_FOULS")
res_fouls_off  <- tally_foul_lineup(fouls_off_df,  "OFF_FOULS")
res_fouls_def  <- tally_foul_lineup(fouls_def_df,  "DEF_FOULS")
res_chrg_draw  <- tally_foul_lineup(charges_df,    "CHRG_DRWN")

# ---- Join into lineup_summary_df ----------------------------------------------------
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  # Total fouls
  dplyr::left_join(
    res_fouls_all$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_all$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Shooting
  dplyr::left_join(
    res_fouls_shot$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_shot$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Technical
  dplyr::left_join(
    res_fouls_tech$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_tech$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Flagrant
  dplyr::left_join(
    res_fouls_flag$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_flag$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Offensive
  dplyr::left_join(
    res_fouls_off$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_off$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Defensive
  dplyr::left_join(
    res_fouls_def$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_fouls_def$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Charges drawn
  dplyr::left_join(
    res_chrg_draw$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_chrg_draw$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Fill NA → 0 for all foul counts (Q1–Q6 + CGS)
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating via HAS_Q5 / HAS_Q6 for lineup
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q5$"),
      ~ dplyr::if_else(HAS_Q5 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_(FOULS|SHOT_FOULS|TECH_FOULS|FLAG_FOULS|OFF_FOULS|DEF_FOULS|CHRG_DRWN)_Q6$"),
      ~ dplyr::if_else(HAS_Q6 == 1L, ., 0L)
    )
  )

rm(
  foul_base_lineup,
  fouls_all_df, fouls_shot_df, fouls_tech_df, fouls_flag_df,
  fouls_off_df, fouls_def_df, charges_df,
  res_fouls_all, res_fouls_shot, res_fouls_tech, res_fouls_flag,
  res_fouls_off, res_fouls_def, res_chrg_draw,
  tally_foul_lineup
)

message("[✓] Fouls (total, shooting, technical, flagrant, offensive, defensive, charges drawn) — LINEUP-level, OT-aware Q1–Q6 + CGS complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Fouls Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀





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





#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: REB SECTION: Lineup rebounding splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

lineup_reb_df <- team_lineup_pbp %>%
  group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    game_date,
    ESPN_HOME_TEAM_ID,
    ESPN_AWAY_TEAM_ID,
    LINEUP_KEY
  ) %>%
  summarise(
    T_OREB_Q1  = sum(OREB_EVENT[qtr == 1], na.rm = TRUE),
    T_OREB_Q2  = sum(OREB_EVENT[qtr == 2], na.rm = TRUE),
    T_OREB_Q3  = sum(OREB_EVENT[qtr == 3], na.rm = TRUE),
    T_OREB_Q4  = sum(OREB_EVENT[qtr == 4], na.rm = TRUE),
    T_OREB_Q5  = sum(OREB_EVENT[qtr == 5], na.rm = TRUE),
    T_OREB_Q6  = sum(OREB_EVENT[qtr == 6], na.rm = TRUE),
    T_OREB_CGS = T_OREB_Q1 + T_OREB_Q2 + T_OREB_Q3 +
      T_OREB_Q4 + T_OREB_Q5 + T_OREB_Q6,
    
    T_DREB_Q1  = sum(DREB_EVENT[qtr == 1], na.rm = TRUE),
    T_DREB_Q2  = sum(DREB_EVENT[qtr == 2], na.rm = TRUE),
    T_DREB_Q3  = sum(DREB_EVENT[qtr == 3], na.rm = TRUE),
    T_DREB_Q4  = sum(DREB_EVENT[qtr == 4], na.rm = TRUE),
    T_DREB_Q5  = sum(DREB_EVENT[qtr == 5], na.rm = TRUE),
    T_DREB_Q6  = sum(DREB_EVENT[qtr == 6], na.rm = TRUE),
    T_DREB_CGS = T_DREB_Q1 + T_DREB_Q2 + T_DREB_Q3 +
      T_DREB_Q4 + T_DREB_Q5 + T_DREB_Q6,
    
    T_REB_Q1   = T_OREB_Q1 + T_DREB_Q1,
    T_REB_Q2   = T_OREB_Q2 + T_DREB_Q2,
    T_REB_Q3   = T_OREB_Q3 + T_DREB_Q3,
    T_REB_Q4   = T_OREB_Q4 + T_DREB_Q4,
    T_REB_Q5   = T_OREB_Q5 + T_DREB_Q5,
    T_REB_Q6   = T_OREB_Q6 + T_DREB_Q6,
    T_REB_CGS  = T_OREB_CGS + T_DREB_CGS,
    
    .groups = "drop"
  )

lineup_summary_df <- lineup_summary_df %>%
  left_join(
    lineup_reb_df,
    by = c(
      "ESPN_GAME_ID", "ESPN_TEAM_ID", "game_date",
      "ESPN_HOME_TEAM_ID", "ESPN_AWAY_TEAM_ID",
      "LINEUP_KEY"
    )
  )

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: REB SECTION: Lineup rebounding splits ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Assist Data Aggregation Section (LINEUP) ====
# Lineup-level assists: counts, points, pct, AST:TOV
# Source: nbapbp_df  → lineup_summary_df
# ==================================================================================== #

stopifnot(all(c(
  "game_id","team_id","qtr","type_text","text","score_value",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids"
) %in% names(nbapbp_df)))

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)
to_int   <- function(x) suppressWarnings(as.integer(x))

# ---- Build assist base from nbapbp_df (credit to OFFENSIVE lineup) ------------------
ast_base_lineup <-
  nbapbp_df %>%
  dplyr::mutate(
    qtr        = to_int(qtr),
    lineup_key = dplyr::if_else(
      team_id == home_team_id,
      lineup_home_ids,     # offense is home
      lineup_away_ids      # offense is away
    ),
    is_ast = stringr::str_detect(text, stringr::regex("assists", ignore_case = TRUE)),
    pts    = suppressWarnings(as.integer(score_value))
  ) %>%
  dplyr::filter(
    is_ast,
    qtr %in% 1:6,
    !is.na(lineup_key)
  )

# ---- Per-quarter tallies (Q1–Q6) ----------------------------------------------------
ast_qtr_lineup <-
  ast_base_lineup %>%
  dplyr::group_by(game_id, team_id, lineup_key, qtr) %>%
  dplyr::summarise(
    AST     = dplyr::n(),
    AST_PTS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  tidyr::complete(qtr = 1:6, fill = list(AST = 0L, AST_PTS = 0L)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from  = qtr,
    values_from = c(AST, AST_PTS),
    names_sep   = "_Q",
    values_fill = 0L
  ) %>%
  dplyr::rename_with(~ gsub("^AST_",      "L_AST_",      .x)) %>%
  dplyr::rename_with(~ gsub("^AST_PTS_",  "L_AST_PTS_",  .x))

# ---- Complete-game tallies (CGS) ----------------------------------------------------
ast_cgs_lineup <-
  ast_base_lineup %>%
  dplyr::group_by(game_id, team_id, lineup_key) %>%
  dplyr::summarise(
    L_AST_CGS     = dplyr::n(),
    L_AST_PTS_CGS = sum(pts, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join into lineup_summary_df ----------------------------------------------------
stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID)
  ) %>%
  dplyr::left_join(
    ast_qtr_lineup,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    ast_cgs_lineup,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Fill all Q1–Q6 + CGS AST / AST_PTS
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_AST(_PTS)?_Q[1-6]$|^L_AST(_PTS)?_CGS$"),
      ~ dplyr::coalesce(., 0L)
    )
  ) %>%
  # OT gating via HAS_Q5 / HAS_Q6
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_AST(_PTS)?_Q5$"),
      ~ dplyr::if_else(HAS_Q5 == 1L, ., 0L)
    ),
    dplyr::across(
      tidyselect::matches("^L_AST(_PTS)?_Q6$"),
      ~ dplyr::if_else(HAS_Q6 == 1L, ., 0L)
    )
  ) %>%
  # ---- Derived lineup-level assist % and AST:TOV -----------------------------------
dplyr::mutate(
  # Assist %
  L_AST_PCT_Q1  = safe_div(L_AST_Q1 , LINEUP_FGM_Q1 ) * 100,
  L_AST_PCT_Q2  = safe_div(L_AST_Q2 , LINEUP_FGM_Q2 ) * 100,
  L_AST_PCT_Q3  = safe_div(L_AST_Q3 , LINEUP_FGM_Q3 ) * 100,
  L_AST_PCT_Q4  = safe_div(L_AST_Q4 , LINEUP_FGM_Q4 ) * 100,
  L_AST_PCT_Q5  = safe_div(L_AST_Q5 , LINEUP_FGM_Q5 ) * 100,   # OT1
  L_AST_PCT_Q6  = safe_div(L_AST_Q6 , LINEUP_FGM_Q6 ) * 100,   # OT2
  L_AST_PCT_CGS = safe_div(L_AST_CGS, LINEUP_FGM_CGS) * 100,
  
  # AST:TOV ratio
  L_AST_TOV_Q1  = safe_div(L_AST_Q1 , L_TOV_Q1 ),
  L_AST_TOV_Q2  = safe_div(L_AST_Q2 , L_TOV_Q2 ),
  L_AST_TOV_Q3  = safe_div(L_AST_Q3 , L_TOV_Q3 ),
  L_AST_TOV_Q4  = safe_div(L_AST_Q4 , L_TOV_Q4 ),
  L_AST_TOV_Q5  = safe_div(L_AST_Q5 , L_TOV_Q5 ),   # OT1
  L_AST_TOV_Q6  = safe_div(L_AST_Q6 , L_TOV_Q6 ),   # OT2
  L_AST_TOV_CGS = safe_div(L_AST_CGS, L_TOV_CGS)
)

rm(ast_base_lineup, ast_qtr_lineup, ast_cgs_lineup)
message("[✓] Assists (Q1–Q6 OT-aware, LINEUP-level) + AST_PTS + AST% + AST:TOV complete.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Assist Data Aggregation Section (LINEUP) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀






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
# === START: (5M) Steals & Blocks Data Aggregation Section ====
# Credits STL/BLK to OPPONENT team (benefiting defense)
# Joins into: lineup_summary_df
# Creates (Q1..Q6 + CGS):
#   L_STL_*, L_BLK_*
#   L_STL_PCT_*  (per lineup possessions)
#   L_BLK_PCT_*  (per opponent lineup FGA)
# ============================================================================

stopifnot(exists("nbapbp_df"), exists("lineup_summary_df"))

stopifnot(all(c(
  "game_id","team_id","qtr","type_text",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids"
) %in% names(nbapbp_df)))

to_int   <- function(x) suppressWarnings(as.integer(x))
safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- Base: opponent (benefiting) team + 5M key on floor -----------------------------
hustle_base_5m <- nbapbp_df %>%
  dplyr::mutate(
    qtr = to_int(qtr),
    
    # Benefiting defense team
    OPP_TEAM_ID = dplyr::if_else(team_id == home_team_id, away_team_id, home_team_id),
    
    # Benefiting 5-man lineup on floor
    LINEUP_KEY = dplyr::if_else(
      team_id == home_team_id,
      lineup_away_ids,   # committing = home → benefiting = away lineup
      lineup_home_ids    # committing = away → benefiting = home lineup
    ),
    
    txt = stringr::str_to_lower(stringr::str_trim(type_text))
  ) %>%
  dplyr::filter(
    qtr %in% 1:6,
    !is.na(OPP_TEAM_ID),
    !is.na(LINEUP_KEY),
    LINEUP_KEY != "NA|NA|NA|NA|NA"
  )

# ---- Pattern definitions -------------------------------------------------------------
pat_steal <- stringr::regex("steal", ignore_case = TRUE)
pat_block <- stringr::regex("block", ignore_case = TRUE)

stl_df_5m <- hustle_base_5m %>% dplyr::filter(stringr::str_detect(txt, pat_steal))
blk_df_5m <- hustle_base_5m %>% dplyr::filter(stringr::str_detect(txt, pat_block))

# ---- Helper: tally by quarter + CGS (credited to benefiting team + lineup key) ------
tally_hustle_5m <- function(df, name_prefix) {
  
  # Q1–Q6
  qtr_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID, lineup_key = LINEUP_KEY, qtr) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(qtr = 1:6, fill = list(N = 0L)) %>%
    tidyr::pivot_wider(
      names_from = qtr, values_from = N, values_fill = 0L, names_prefix = "Q"
    ) %>%
    dplyr::rename_with(~ paste0("L_", name_prefix, "_", .x), dplyr::starts_with("Q"))
  
  # CGS
  cgs_df <- df %>%
    dplyr::group_by(game_id, team_id = OPP_TEAM_ID, lineup_key = LINEUP_KEY) %>%
    dplyr::summarise(CGS = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!paste0("L_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

res_stl_5m <- tally_hustle_5m(stl_df_5m, "STL")
res_blk_5m <- tally_hustle_5m(blk_df_5m, "BLK")

# ---- Join into lineup_summary_df ----------------------------------------------------
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    LINEUP_KEY   = as.character(LINEUP_KEY)
  ) %>%
  # Steals
  dplyr::left_join(
    res_stl_5m$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_stl_5m$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Blocks
  dplyr::left_join(
    res_blk_5m$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  dplyr::left_join(
    res_blk_5m$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  # Fill NA → 0 for new count columns
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^L_(STL|BLK)_(Q[1-6]|CGS)$"),
      ~ dplyr::coalesce(., 0L)
    )
  )

# ---- Opponent denominators for BLK% (opponent lineup FGA) ---------------------------
# Opponent lineup FGA within the same game_id + lineup_key: sum(two teams) - own
lineup_summary_df <- lineup_summary_df %>%
  dplyr::group_by(ESPN_GAME_ID, LINEUP_KEY) %>%
  dplyr::mutate(
    OPP_LINEUP_FGA_Q1  = sum(LINEUP_FGA_Q1,  na.rm = TRUE) - LINEUP_FGA_Q1,
    OPP_LINEUP_FGA_Q2  = sum(LINEUP_FGA_Q2,  na.rm = TRUE) - LINEUP_FGA_Q2,
    OPP_LINEUP_FGA_Q3  = sum(LINEUP_FGA_Q3,  na.rm = TRUE) - LINEUP_FGA_Q3,
    OPP_LINEUP_FGA_Q4  = sum(LINEUP_FGA_Q4,  na.rm = TRUE) - LINEUP_FGA_Q4,
    OPP_LINEUP_FGA_Q5  = sum(LINEUP_FGA_Q5,  na.rm = TRUE) - LINEUP_FGA_Q5,
    OPP_LINEUP_FGA_Q6  = sum(LINEUP_FGA_Q6,  na.rm = TRUE) - LINEUP_FGA_Q6,
    OPP_LINEUP_FGA_CGS = sum(LINEUP_FGA_CGS, na.rm = TRUE) - LINEUP_FGA_CGS
  ) %>%
  dplyr::ungroup()

# ---- Percentages --------------------------------------------------------------------
# STL%: steals / lineup possessions * 100
# BLK%: blocks / opponent lineup FGA * 100

pct_spans <- c("Q1","Q2","Q3","Q4","Q5","Q6","CGS")

for (s in pct_spans) {
  lineup_summary_df[[paste0("L_STL_PCT_", s)]] <-
    100 * safe_div(
      lineup_summary_df[[paste0("L_STL_", s)]],
      lineup_summary_df[[paste0("LINEUP_POSS_", s)]]
    )
  
  lineup_summary_df[[paste0("L_BLK_PCT_", s)]] <-
    100 * safe_div(
      lineup_summary_df[[paste0("L_BLK_", s)]],
      lineup_summary_df[[paste0("OPP_LINEUP_FGA_", s)]]
    )
}

rm(
  hustle_base_5m, stl_df_5m, blk_df_5m,
  res_stl_5m, res_blk_5m, tally_hustle_5m, pct_spans
)

message("[✓] 5M Steals & Blocks (counts + STL% + BLK%) computed and joined into lineup_summary_df.")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: (5M) Steals & Blocks Data Aggregation Section ====
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


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: (LINEUP LEVEL) Runs Data Aggregation & Stats Creation Section ====
# Uses nbapbp_df with lineup_home_ids / lineup_away_ids to:
#   1) Detect runs exactly like team-level run_stats
#   2) Attribute runs to the lineup on the floor for the scoring team
#   3) Build lineup-level run stats (counts, windows, eff, vol, success, stops)
# Writes into: lineup_summary_df
#   L_RUNS_7P_*, L_RUNS_10P_*, L_RUNS_15P_*
#   L_RUNS_1MINL_*, L_RUNS_3MINL_*, L_RUNS_5MINL_*, L_RUNS_10MINL_*
#   L_RUN_EFF_*, L_RUN_VOL_*, L_RUN_SUCC_RATE_*
#   L_RUNS_STOPS_*, L_RUNS_STOP_SHIFT_*, L_RUNS_STOPS_RATE_*, L_RUNS_STOP_SHIFT_RATE_*
# ============================================================================

library(dplyr)
library(tidyr)

stopifnot(exists("nbapbp_df"), exists("lineup_summary_df"))

# ── 1) Prepare PBP (order events, normalize types, lineup key) ─────────────────────
required_cols_lineup_runs <- c(
  "game_id","team_id",
  "home_team_id","away_team_id",
  "lineup_home_ids","lineup_away_ids",
  "qtr","period","sequence_number",
  "scoring_play","score_value",
  "clock_minutes","clock_seconds"
)

missing_cols_lineup_runs <- setdiff(required_cols_lineup_runs, names(nbapbp_df))
if (length(missing_cols_lineup_runs) > 0) {
  stop(
    "Missing columns in nbapbp_df for LINEUP runs section: ",
    paste(missing_cols_lineup_runs, collapse = ", ")
  )
}

to_int  <- function(x) suppressWarnings(as.integer(x))
to_num  <- function(x) suppressWarnings(as.numeric(x))
to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")

pbp_prepped_lu <- nbapbp_df %>%
  mutate(
    clock_seconds_total = to_num(clock_minutes) * 60 + to_num(clock_seconds),
    qtr     = to_int(coalesce(as.numeric(qtr), as.numeric(period))),
    period  = to_int(period),
    sequence_number = to_int(sequence_number),
    lineup_key = if_else(
      team_id == home_team_id,
      lineup_home_ids,
      lineup_away_ids
    )
  ) %>%
  arrange(game_id, qtr, desc(clock_seconds_total), sequence_number)

# ── 2) Keep only scoring plays & row_id per game ────────────────────────────────────
scoring_events_lu <- pbp_prepped_lu %>%
  filter(to_bool(scoring_play)) %>%
  mutate(points = to_num(score_value)) %>%
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
    clock_seconds_total,
    lineup_key
  )

# ── 3) Detect contiguous runs (same team scores on consecutive scoring events) ─────
detect_runs_vec_lu <- function(df) {
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

run_stats_lu <- scoring_events_lu %>%
  group_by(game_id) %>%
  group_split() %>%
  lapply(detect_runs_vec_lu) %>%
  bind_rows()

# ── 4) Enrich runs with start/end quarter & clock + duration ───────────────────────
se_start_lu <- scoring_events_lu %>%
  select(game_id, row_id,
         run_start_qtr    = qtr,
         run_start_period = period,
         run_start_clock  = clock_seconds_total)

se_end_lu <- scoring_events_lu %>%
  select(game_id, row_id,
         run_end_qtr    = qtr,
         run_end_period = period,
         run_end_clock  = clock_seconds_total)

run_stats_lu <- run_stats_lu %>%
  left_join(se_start_lu, by = c("game_id", "run_start_row" = "row_id")) %>%
  left_join(se_end_lu,   by = c("game_id", "run_end_row"   = "row_id")) %>%
  mutate(
    run_start_clock_minutes = floor(run_start_clock / 60),
    run_start_clock_seconds = round(run_start_clock %% 60),
    run_end_clock_minutes   = floor(run_end_clock   / 60),
    run_end_clock_seconds   = round(run_end_clock   %% 60),
    run_duration_sec        = pmax(0, run_start_clock - run_end_clock)
  )

# ── 5) Attach lineup at run start (so we can credit runs to lineups) ───────────────
run_start_lineup_lu <- scoring_events_lu %>%
  select(
    game_id,
    row_id,
    scoring_team,
    lineup_key
  )

run_stats_lu <- run_stats_lu %>%
  left_join(
    run_start_lineup_lu,
    by = c("game_id", "run_start_row" = "row_id", "run_team" = "scoring_team")
  ) %>%
  rename(run_lineup_key = lineup_key)

# =============================
# Helpers (quarter + CGS tallies, LINEUP dimension)
# =============================

.tally_qtr_cgs_count_lu <- function(df, name_prefix) {
  # per-quarter counts Q1–Q4
  qtr_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key,
             qtr        = run_start_qtr) %>%
    summarise(N = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    rename_with(~ paste0("L_", name_prefix, "_", .x), starts_with("Q"))
  
  # complete-game counts
  cgs_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key) %>%
    summarise(CGS = n(), .groups = "drop") %>%
    rename(!!paste0("L_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

.tally_qtr_cgs_stat_lu <- function(df, name_prefix, fun) {
  # per-quarter stat
  qtr_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key,
             qtr        = run_start_qtr) %>%
    summarise(V = fun(run_points, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from  = qtr,
      values_from = V,
      names_prefix = "Q"
    ) %>%
    rename_with(~ paste0("L_", name_prefix, "_", .x), starts_with("Q"))
  
  # complete game stat
  cgs_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key) %>%
    summarise(V = fun(run_points, na.rm = TRUE), .groups = "drop") %>%
    rename(!!paste0("L_", name_prefix, "_CGS") := V)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

# =============================
# Build the various run datasets (LINEUP)
# =============================

runs_7p_l   <- run_stats_lu %>%
  filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4,
         run_points >= 7, !is.na(run_lineup_key))

runs_10p_l  <- run_stats_lu %>%
  filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4,
         run_points >= 10, !is.na(run_lineup_key))

runs_15p_l  <- run_stats_lu %>%
  filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4,
         run_points >= 15, !is.na(run_lineup_key))

# Time windows use runs >= 15 and last X minutes of the period
runs_1min_l  <- runs_15p_l %>% filter(run_start_clock_minutes < 1)
runs_3min_l  <- runs_15p_l %>% filter(run_start_clock_minutes < 3)
runs_5min_l  <- runs_15p_l %>% filter(run_start_clock_minutes < 5)
runs_10min_l <- runs_15p_l %>% filter(run_start_clock_minutes < 10)

# Efficiency (mean) and Volatility (sd) across all detected runs (no threshold)
runs_eff_l <- run_stats_lu %>%
  filter(!is.na(run_start_qtr), run_start_qtr %in% 1:4, !is.na(run_lineup_key))

runs_vol_l <- runs_eff_l

# Success rate: share of runs with points >= 7
runs_succ_l <- runs_eff_l %>%
  mutate(succ = run_points >= 7)

# =============================
# Tally each into Q1..Q4 + CGS (LINEUP)
# =============================

res_7p_l    <- .tally_qtr_cgs_count_lu(runs_7p_l,   "RUNS_7P")
res_10p_l   <- .tally_qtr_cgs_count_lu(runs_10p_l,  "RUNS_10P")
res_15p_l   <- .tally_qtr_cgs_count_lu(runs_15p_l,  "RUNS_15P")

res_1min_l  <- .tally_qtr_cgs_count_lu(runs_1min_l,  "RUNS_1MINL")
res_3min_l  <- .tally_qtr_cgs_count_lu(runs_3min_l,  "RUNS_3MINL")
res_5min_l  <- .tally_qtr_cgs_count_lu(runs_5min_l,  "RUNS_5MINL")
res_10min_l <- .tally_qtr_cgs_count_lu(runs_10min_l, "RUNS_10MINL")

res_eff_l   <- .tally_qtr_cgs_stat_lu(runs_eff_l, "RUN_EFF", mean)
res_vol_l   <- .tally_qtr_cgs_stat_lu(runs_vol_l, "RUN_VOL", stats::sd)

# Success rate (mean of succ) per quarter & CGS
res_succ_qtr_l <- runs_succ_l %>%
  group_by(game_id,
           team_id    = run_team,
           lineup_key = run_lineup_key,
           qtr        = run_start_qtr) %>%
  summarise(V = mean(succ, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = qtr,
    values_from = V,
    names_prefix = "Q"
  ) %>%
  rename_with(~ paste0("L_RUN_SUCC_RATE_", .x), starts_with("Q"))

res_succ_cgs_l <- runs_succ_l %>%
  group_by(game_id,
           team_id    = run_team,
           lineup_key = run_lineup_key) %>%
  summarise(V = mean(succ, na.rm = TRUE), .groups = "drop") %>%
  rename(L_RUN_SUCC_RATE_CGS = V)

# =============================
# Join lineup run stats into lineup_summary_df
# =============================

stopifnot(exists("lineup_summary_df"))

lineup_summary_df <- lineup_summary_df %>%
  mutate(
    ESPN_GAME_ID = as.character(ESPN_GAME_ID),
    ESPN_TEAM_ID = as.character(ESPN_TEAM_ID),
    LINEUP_KEY   = as.character(LINEUP_KEY)
  ) %>%
  # run-count thresholds
  left_join(res_7p_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_7p_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_10p_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_10p_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_15p_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_15p_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  # time-window run counts (>=15)
  left_join(res_1min_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_1min_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_3min_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_3min_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_5min_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_5min_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_10min_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_10min_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  # efficiency & volatility
  left_join(res_eff_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_eff_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_vol_l$qtr,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_vol_l$cgs,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  # success rate
  left_join(res_succ_qtr_l,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  left_join(res_succ_cgs_l,
            by = c("ESPN_GAME_ID" = "game_id",
                   "ESPN_TEAM_ID" = "team_id",
                   "LINEUP_KEY"   = "lineup_key")) %>%
  # coalesce run-count fields (leave rates/means as NA when no runs)
  mutate(
    across(
      matches("^L_RUNS_(7P|10P|15P|1MINL|3MINL|5MINL|10MINL)_(Q[1-4]|CGS)$"),
      ~ coalesce(., 0L)
    )
  )

# =============================
# Lineup Runs Stopped (simple) + Momentum Shift (strict)
# =============================

# Reuse run_stats_lu but with previous run info
runs_with_prev_lu <- run_stats_lu %>%
  group_by(game_id) %>%
  arrange(run_start_row, .by_group = TRUE) %>%
  mutate(
    prev_run_team      = lag(run_team),
    prev_run_end_clock = lag(run_end_clock),
    prev_run_end_qtr   = lag(run_end_qtr)
  ) %>%
  ungroup()

# --- parameter for "momentum shift" drought (≈ two empty trips) ---
DROUGHT_SEC <- 45

# 1) Simple stops = our run begins immediately after opponent’s run
stops_simple_lu <- runs_with_prev_lu %>%
  filter(
    !is.na(prev_run_team),
    prev_run_team != run_team,
    !is.na(run_start_qtr),
    run_start_qtr %in% 1:4,
    !is.na(run_lineup_key)
  )

# 2) Momentum-shift stops = simple stop AND opponent drought >= DROUGHT_SEC
stops_strict_lu <- runs_with_prev_lu %>%
  filter(
    !is.na(prev_run_team),
    prev_run_team != run_team,
    !is.na(run_start_qtr),
    run_start_qtr %in% 1:4,
    !is.na(prev_run_end_clock),
    !is.na(run_start_clock),
    !is.na(run_lineup_key)
  ) %>%
  mutate(
    opp_drought_sec = prev_run_end_clock - run_start_clock  # clock counts down
  ) %>%
  filter(opp_drought_sec >= DROUGHT_SEC)

# 3) Tally stops (LINEUP) ---------------------------------------------------
.tally_stops_lu <- function(df, name_prefix) {
  qtr_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key,
             qtr        = run_start_qtr) %>%
    summarise(N = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = qtr,
      values_from = N,
      values_fill = 0L,
      names_prefix = "Q"
    ) %>%
    rename_with(~ paste0("L_", name_prefix, "_", .x), starts_with("Q"))
  
  cgs_df <- df %>%
    group_by(game_id,
             team_id    = run_team,
             lineup_key = run_lineup_key) %>%
    summarise(CGS = n(), .groups = "drop") %>%
    rename(!!paste0("L_", name_prefix, "_CGS") := CGS)
  
  list(qtr = qtr_df, cgs = cgs_df)
}

res_stops_simple_lu <- .tally_stops_lu(stops_simple_lu, "RUNS_STOPS")
res_stops_strict_lu <- .tally_stops_lu(stops_strict_lu, "RUNS_STOP_SHIFT")

# 4) Join stops counts into lineup_summary_df ---------------------------------------
lineup_summary_df <- lineup_summary_df %>%
  left_join(
    res_stops_simple_lu$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  left_join(
    res_stops_simple_lu$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  left_join(
    res_stops_strict_lu$qtr,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  left_join(
    res_stops_strict_lu$cgs,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  mutate(
    across(
      matches("^L_(RUNS_STOPS|RUNS_STOP_SHIFT)_(Q[1-4]|CGS)$"),
      ~ coalesce(., 0L)
    )
  )

# =============================
# Lineup Stop Rates (simple + momentum-shift)
# =============================

# Opportunities: previous run was by opponent; attribute opportunity
# to lineup + team that starts the new (current) run.
opps_simple_lu <- runs_with_prev_lu %>%
  filter(
    !is.na(prev_run_team),
    prev_run_team != run_team,
    !is.na(prev_run_end_qtr),
    prev_run_end_qtr %in% 1:4,
    !is.na(run_lineup_key)
  ) %>%
  group_by(game_id,
           team_id    = run_team,
           lineup_key = run_lineup_key,
           qtr        = prev_run_end_qtr) %>%
  summarise(OPPS = n(), .groups = "drop")

stops_simple_q_lu <- stops_simple_lu %>%
  group_by(game_id,
           team_id    = run_team,
           lineup_key = run_lineup_key,
           qtr        = run_start_qtr) %>%
  summarise(STOPS = n(), .groups = "drop")

stops_strict_q_lu <- stops_strict_lu %>%
  group_by(game_id,
           team_id    = run_team,
           lineup_key = run_lineup_key,
           qtr        = run_start_qtr) %>%
  summarise(SHIFT_STOPS = n(), .groups = "drop")

stops_rates_q_lu <- opps_simple_lu %>%
  full_join(stops_simple_q_lu,
            by = c("game_id","team_id","lineup_key","qtr")) %>%
  full_join(stops_strict_q_lu,
            by = c("game_id","team_id","lineup_key","qtr")) %>%
  mutate(
    OPPS        = coalesce(OPPS, 0L),
    STOPS       = coalesce(STOPS, 0L),
    SHIFT_STOPS = coalesce(SHIFT_STOPS, 0L),
    L_RUNS_STOPS_RATE        = if_else(OPPS > 0, STOPS / OPPS, NA_real_),
    L_RUNS_STOP_SHIFT_RATE   = if_else(STOPS > 0, SHIFT_STOPS / STOPS, NA_real_)
  ) %>%
  pivot_wider(
    id_cols     = c(game_id, team_id, lineup_key),
    names_from  = qtr,
    values_from = c(L_RUNS_STOPS_RATE, L_RUNS_STOP_SHIFT_RATE),
    names_glue  = "{.value}_Q{qtr}"
  )

# CGS (full-game) rates (LINEUP)
opps_simple_cgs_lu <- opps_simple_lu %>%
  group_by(game_id, team_id, lineup_key) %>%
  summarise(OPPS = sum(OPPS), .groups = "drop")

stops_simple_cgs_lu <- stops_simple_q_lu %>%
  group_by(game_id, team_id, lineup_key) %>%
  summarise(STOPS = sum(STOPS), .groups = "drop")

stops_strict_cgs_lu <- stops_strict_q_lu %>%
  group_by(game_id, team_id, lineup_key) %>%
  summarise(SHIFT_STOPS = sum(SHIFT_STOPS), .groups = "drop")

stops_rates_cgs_lu <- opps_simple_cgs_lu %>%
  full_join(stops_simple_cgs_lu, by = c("game_id","team_id","lineup_key")) %>%
  full_join(stops_strict_cgs_lu, by = c("game_id","team_id","lineup_key")) %>%
  mutate(
    OPPS        = coalesce(OPPS, 0L),
    STOPS       = coalesce(STOPS, 0L),
    SHIFT_STOPS = coalesce(SHIFT_STOPS, 0L),
    L_RUNS_STOPS_RATE_CGS      = if_else(OPPS > 0, STOPS / OPPS, NA_real_),
    L_RUNS_STOP_SHIFT_RATE_CGS = if_else(STOPS > 0, SHIFT_STOPS / STOPS, NA_real_)
  ) %>%
  select(game_id, team_id, lineup_key,
         L_RUNS_STOPS_RATE_CGS,
         L_RUNS_STOP_SHIFT_RATE_CGS)

# Join rates into lineup_summary_df
lineup_summary_df <- lineup_summary_df %>%
  left_join(
    stops_rates_q_lu,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  ) %>%
  left_join(
    stops_rates_cgs_lu,
    by = c("ESPN_GAME_ID" = "game_id",
           "ESPN_TEAM_ID" = "team_id",
           "LINEUP_KEY"   = "lineup_key")
  )

message(sprintf(
  "[✓] Lineup runs (7+, 10+, 15+, time windows, efficiency, volatility, success, stops, stop rates; DROUGHT_SEC=%s) computed and joined into lineup_summary_df.",
  DROUGHT_SEC
))

# Cleanup
rm(
  pbp_prepped_lu, scoring_events_lu, detect_runs_vec_lu,
  run_stats_lu, se_start_lu, se_end_lu, run_start_lineup_lu,
  runs_7p_l, runs_10p_l, runs_15p_l,
  runs_1min_l, runs_3min_l, runs_5min_l, runs_10min_l,
  runs_eff_l, runs_vol_l, runs_succ_l,
  res_7p_l, res_10p_l, res_15p_l,
  res_1min_l, res_3min_l, res_5min_l, res_10min_l,
  res_eff_l, res_vol_l, res_succ_qtr_l, res_succ_cgs_l,
  runs_with_prev_lu, stops_simple_lu, stops_strict_lu,
  res_stops_simple_lu, res_stops_strict_lu,
  opps_simple_lu, stops_simple_q_lu, stops_strict_q_lu,
  stops_rates_q_lu, opps_simple_cgs_lu,
  stops_simple_cgs_lu, stops_strict_cgs_lu, stops_rates_cgs_lu
)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: (LINEUP LEVEL) Runs Data Aggregation & Stats Creation Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: OFF/DEF/NET RTG (LINEUP) — 5M lineup_summary_df ====
# Requires (already in lineup_summary_df):
#   LINEUP_PTS_Q1..Q6, LINEUP_PTS_CGS
#   LINEUP_POSS_Q1..Q6, LINEUP_POSS_CGS
#   HAS_Q5, HAS_Q6
# Produces:
#   L_OFF_RTG_Q1..Q6, L_OFF_RTG_CGS
#   L_DEF_RTG_Q1..Q6, L_DEF_RTG_CGS
#   L_NET_RTG_Q1..Q6, L_NET_RTG_CGS
# Uses opponent derivation:
#   OPP_LINEUP_PTS_*  = sum(game) - own
#   OPP_LINEUP_POSS_* = sum(game) - own
# ============================================================================= #

stopifnot(exists("lineup_summary_df"))

safe_div <- function(n, d) ifelse(d > 0 & !is.na(d), n / d, NA_real_)

# ---- opponent derivation via game sums ---------------------------------------------
lineup_summary_df <- lineup_summary_df %>%
  dplyr::group_by(ESPN_GAME_ID) %>%
  dplyr::mutate(
    # Opponent points while lineup is on floor
    OPP_LINEUP_PTS_Q1  = sum(LINEUP_PTS_Q1,  na.rm = TRUE) - LINEUP_PTS_Q1,
    OPP_LINEUP_PTS_Q2  = sum(LINEUP_PTS_Q2,  na.rm = TRUE) - LINEUP_PTS_Q2,
    OPP_LINEUP_PTS_Q3  = sum(LINEUP_PTS_Q3,  na.rm = TRUE) - LINEUP_PTS_Q3,
    OPP_LINEUP_PTS_Q4  = sum(LINEUP_PTS_Q4,  na.rm = TRUE) - LINEUP_PTS_Q4,
    OPP_LINEUP_PTS_Q5  = sum(LINEUP_PTS_Q5,  na.rm = TRUE) - LINEUP_PTS_Q5,
    OPP_LINEUP_PTS_Q6  = sum(LINEUP_PTS_Q6,  na.rm = TRUE) - LINEUP_PTS_Q6,
    OPP_LINEUP_PTS_CGS = sum(LINEUP_PTS_CGS, na.rm = TRUE) - LINEUP_PTS_CGS,
    
    # Opponent possessions while lineup is on floor
    OPP_LINEUP_POSS_Q1  = sum(LINEUP_POSS_Q1,  na.rm = TRUE) - LINEUP_POSS_Q1,
    OPP_LINEUP_POSS_Q2  = sum(LINEUP_POSS_Q2,  na.rm = TRUE) - LINEUP_POSS_Q2,
    OPP_LINEUP_POSS_Q3  = sum(LINEUP_POSS_Q3,  na.rm = TRUE) - LINEUP_POSS_Q3,
    OPP_LINEUP_POSS_Q4  = sum(LINEUP_POSS_Q4,  na.rm = TRUE) - LINEUP_POSS_Q4,
    OPP_LINEUP_POSS_Q5  = sum(LINEUP_POSS_Q5,  na.rm = TRUE) - LINEUP_POSS_Q5,
    OPP_LINEUP_POSS_Q6  = sum(LINEUP_POSS_Q6,  na.rm = TRUE) - LINEUP_POSS_Q6,
    OPP_LINEUP_POSS_CGS = sum(LINEUP_POSS_CGS, na.rm = TRUE) - LINEUP_POSS_CGS
  ) %>%
  dplyr::ungroup()

# ---- Ratings -----------------------------------------------------------------------
lineup_summary_df <- lineup_summary_df %>%
  dplyr::mutate(
    # OFF RTG
    L_OFF_RTG_Q1  = 100 * safe_div(LINEUP_PTS_Q1,  LINEUP_POSS_Q1),
    L_OFF_RTG_Q2  = 100 * safe_div(LINEUP_PTS_Q2,  LINEUP_POSS_Q2),
    L_OFF_RTG_Q3  = 100 * safe_div(LINEUP_PTS_Q3,  LINEUP_POSS_Q3),
    L_OFF_RTG_Q4  = 100 * safe_div(LINEUP_PTS_Q4,  LINEUP_POSS_Q4),
    L_OFF_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, 100 * safe_div(LINEUP_PTS_Q5,  LINEUP_POSS_Q5), NA_real_),
    L_OFF_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, 100 * safe_div(LINEUP_PTS_Q6,  LINEUP_POSS_Q6), NA_real_),
    L_OFF_RTG_CGS = 100 * safe_div(LINEUP_PTS_CGS, LINEUP_POSS_CGS),
    
    # DEF RTG
    L_DEF_RTG_Q1  = 100 * safe_div(OPP_LINEUP_PTS_Q1,  OPP_LINEUP_POSS_Q1),
    L_DEF_RTG_Q2  = 100 * safe_div(OPP_LINEUP_PTS_Q2,  OPP_LINEUP_POSS_Q2),
    L_DEF_RTG_Q3  = 100 * safe_div(OPP_LINEUP_PTS_Q3,  OPP_LINEUP_POSS_Q3),
    L_DEF_RTG_Q4  = 100 * safe_div(OPP_LINEUP_PTS_Q4,  OPP_LINEUP_POSS_Q4),
    L_DEF_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, 100 * safe_div(OPP_LINEUP_PTS_Q5,  OPP_LINEUP_POSS_Q5), NA_real_),
    L_DEF_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, 100 * safe_div(OPP_LINEUP_PTS_Q6,  OPP_LINEUP_POSS_Q6), NA_real_),
    L_DEF_RTG_CGS = 100 * safe_div(OPP_LINEUP_PTS_CGS, OPP_LINEUP_POSS_CGS),
    
    # NET RTG
    L_NET_RTG_Q1  = L_OFF_RTG_Q1  - L_DEF_RTG_Q1,
    L_NET_RTG_Q2  = L_OFF_RTG_Q2  - L_DEF_RTG_Q2,
    L_NET_RTG_Q3  = L_OFF_RTG_Q3  - L_DEF_RTG_Q3,
    L_NET_RTG_Q4  = L_OFF_RTG_Q4  - L_DEF_RTG_Q4,
    L_NET_RTG_Q5  = dplyr::if_else(HAS_Q5 == 1L, L_OFF_RTG_Q5 - L_DEF_RTG_Q5, NA_real_),
    L_NET_RTG_Q6  = dplyr::if_else(HAS_Q6 == 1L, L_OFF_RTG_Q6 - L_DEF_RTG_Q6, NA_real_),
    L_NET_RTG_CGS = L_OFF_RTG_CGS - L_DEF_RTG_CGS
  )

rm(team_lineup_pbp)

message("[✓] 5M lineup_summary_df — OFF/DEF/NET RTG (Q1–Q6 OT-aware + CGS) complete.")
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: OFF/DEF/NET RTG (LINEUP) — 5M lineup_summary_df ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#     ooooooooo.   oooo                                                ooooooooo.                 .                 .    o8o                             ooooooooooooo                              oooo         o8o                              oooooooooooo  o8o  oooo            
#     `888   `Y88. `888                                                `888   `Y88.             .o8               .o8    `"'                             8'   888   `8                              `888         `"'                              `888'     `8  `"'  `888            
#      888   .d88'  888   .oooo.   oooo    ooo  .ooooo.  oooo d8b       888   .d88'  .ooooo.  .o888oo  .oooo.   .o888oo oooo   .ooooo.  ooo. .oo.             888      oooo d8b  .oooo.    .ooooo.   888  oooo  oooo  ooo. .oo.    .oooooooo       888         oooo   888   .ooooo.  
#      888ooo88P'   888  `P  )88b   `88.  .8'  d88' `88b `888""8P       888ooo88P'  d88' `88b   888   `P  )88b    888   `888  d88' `88b `888P"Y88b            888      `888""8P `P  )88b  d88' `"Y8  888 .8P'   `888  `888P"Y88b  888' `88b        888oooo8    `888   888  d88' `88b 
#      888          888   .oP"888    `88..8'   888ooo888  888           888`88b.    888   888   888    .oP"888    888    888  888   888  888   888            888       888      .oP"888  888        888888.     888   888   888  888   888        888    "     888   888  888ooo888 
#      888          888  d8(  888     `888'    888    .o  888           888  `88b.  888   888   888 . d8(  888    888 .  888  888   888  888   888            888       888     d8(  888  888   .o8  888 `88b.   888   888   888  `88bod8P'        888          888   888  888    .o 
#     o888o        o888o `Y888""8o     .8'     `Y8bod8P' d888b         o888o  o888o `Y8bod8P'   "888" `Y888""8o   "888" o888o `Y8bod8P' o888o o888o          o888o     d888b    `Y888""8o `Y8bod8P' o888o o888o o888o o888o o888o `8oooooo.       o888o        o888o o888o `Y8bod8P' 
#                                  .o..P'                                                                                                                                                                                         d"     YD                                          
#                                  `Y8P'                                                                                                                                                                                          "Y88888P'                                          
#
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                                                                                                                                                                                                                                                               
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Player–Teammate Rotation Summary (player_lineup_summary_df) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# One row per (player, teammate, game, team)
#  - total time player was on floor (seconds + minutes)
#  - time shared with each specific teammate (seconds)
#  - quarter-level breakdown (LINEUP_IN_Q1..Q6) for time shared
#  - top 5 lineups (LINEUP_ID_1..5) by shared time
# Source: lineup_summary_df (lineup-level seconds + player ids/names)
# ============================================================================

# ---- sanity check for required columns ------------------------------------
required_cols <- c(
  "ESPN_GAME_ID","ESPN_TEAM_ID","LINEUP_KEY","LINEUP_ID","game_date",
  "TEAM","HOME_TEAM","AWAY_TEAM","LINEUP_TYPE",
  paste0("LINEUP_P", 1:5),
  paste0("LINEUP_P", 1:5, "_espn_id"),
  "LINEUP_IN_CGS",
  paste0("LINEUP_IN_Q", 1:4)   # at least Q1–Q4
)

missing_cols <- setdiff(required_cols, names(lineup_summary_df))
if (length(missing_cols) > 0) {
  stop(
    "Missing columns in lineup_summary_df for player_lineup_summary_df: ",
    paste(missing_cols, collapse = ", ")
  )
}

# Collect all quarter-level time columns that exist (Q1–Q4, optionally Q5/Q6)
in_q_cols <- grep("^LINEUP_IN_Q[1-6]$", names(lineup_summary_df), value = TRUE)
in_q_cols <- sort(in_q_cols)  # ensure Q1..Q6 order if present

# ---- 1) Long form: one row per player per lineup --------------------------
# Pivot NAMES and IDs separately, then join on (game, team, lineup, slot)

# Names + context
lineup_players_names <-
  lineup_summary_df %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    TEAM,
    game_date,
    HOME_TEAM,
    AWAY_TEAM,
    LINEUP_KEY,
    LINEUP_ID,
    LINEUP_TYPE,
    dplyr::all_of(in_q_cols),
    LINEUP_IN_CGS,
    dplyr::matches("^LINEUP_P[1-5]$")
  ) %>%
  tidyr::pivot_longer(
    cols      = dplyr::matches("^LINEUP_P[1-5]$"),
    names_to  = "slot",
    names_pattern = "LINEUP_(P[1-5])",
    values_to = "PLAYER_NAME"
  )

# IDs
lineup_players_ids <-
  lineup_summary_df %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    LINEUP_KEY,
    dplyr::matches("^LINEUP_P[1-5]_espn_id$")
  ) %>%
  tidyr::pivot_longer(
    cols      = dplyr::matches("^LINEUP_P[1-5]_espn_id$"),
    names_to  = "slot",
    names_pattern = "LINEUP_(P[1-5])_espn_id",
    values_to = "PLAYER_ID"
  )

# Join names + ids + time columns
lineup_players_long <-
  lineup_players_names %>%
  dplyr::inner_join(
    lineup_players_ids,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","LINEUP_KEY","slot")
  ) %>%
  dplyr::filter(!is.na(PLAYER_ID), PLAYER_ID != "")

# ---- 2) Per-player total floor time + context (within game + team) --------
player_totals <-
  lineup_players_long %>%
  dplyr::group_by(ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID, PLAYER_NAME) %>%
  dplyr::summarise(
    GAME_DATE = dplyr::first(game_date),
    TEAM      = dplyr::first(TEAM),
    HOME_TEAM = dplyr::first(HOME_TEAM),
    AWAY_TEAM = dplyr::first(AWAY_TEAM),
    STARTER_STATUS = dplyr::if_else(
      any(LINEUP_TYPE == "STARTING", na.rm = TRUE),
      "STARTER",
      "BENCH"
    ),
    PLAYER_IN_CGS = sum(LINEUP_IN_CGS, na.rm = TRUE),
    dplyr::across(
      dplyr::all_of(in_q_cols),
      ~ sum(.x, na.rm = TRUE),
      .names = "PLAYER_{.col}"
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    MINS = PLAYER_IN_CGS / 60
  )

# ---- 3) Player–teammate pair expansion per lineup -------------------------
# ---- 3) Player–teammate pair expansion per lineup -------------------------
# make sure LINEUP_ID is carried through

lp_left <- lineup_players_long %>%
  dplyr::rename(
    PLAYER_SLOT = slot
  )
# NOTE: lp_left already has LINEUP_ID from lineup_players_names

lp_right <- lineup_players_long %>%
  dplyr::select(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    LINEUP_KEY,
    LINEUP_ID,      # <-- keep LINEUP_ID here
    slot,
    PLAYER_ID,
    PLAYER_NAME
  ) %>%
  dplyr::rename(
    TEAMMATE_SLOT = slot,
    TEAMMATE_ID   = PLAYER_ID,
    TEAMMATE_NAME = PLAYER_NAME
  )

pairs_long <-
  lp_left %>%
  dplyr::inner_join(
    lp_right,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","LINEUP_KEY","LINEUP_ID")
  ) %>%
  dplyr::filter(PLAYER_SLOT != TEAMMATE_SLOT)

# ---- 4) Top-5 lineups by shared time for each player–teammate pair --------
pair_lineup_rank <-
  pairs_long %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    PLAYER_ID,
    PLAYER_NAME,
    TEAMMATE_ID,
    TEAMMATE_NAME,
    LINEUP_ID
  ) %>%
  dplyr::summarise(
    SHARED_IN_CGS_LINEUP = sum(LINEUP_IN_CGS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID, TEAMMATE_ID,
    dplyr::desc(SHARED_IN_CGS_LINEUP)
  ) %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    PLAYER_ID,
    PLAYER_NAME,
    TEAMMATE_ID,
    TEAMMATE_NAME
  ) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::filter(rank <= 15L) %>%
  tidyr::pivot_wider(
    id_cols     = c(ESPN_GAME_ID, ESPN_TEAM_ID, PLAYER_ID, PLAYER_NAME,
                    TEAMMATE_ID, TEAMMATE_NAME),
    names_from  = rank,
    values_from = LINEUP_ID,
    names_glue  = "LINEUP_ID_{rank}"
  ) %>%
  dplyr::ungroup()

# ---- 5) Aggregate pair seconds (time shared together) ----------------------
player_lineup_summary_df <-
  pairs_long %>%
  dplyr::group_by(
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    PLAYER_ID,
    PLAYER_NAME,
    TEAMMATE_ID,
    TEAMMATE_NAME
  ) %>%
  dplyr::summarise(
    SHARED_IN_CGS = sum(LINEUP_IN_CGS, na.rm = TRUE),
    dplyr::across(
      dplyr::all_of(in_q_cols),
      ~ sum(.x, na.rm = TRUE),
      .names = "SHARED_{.col}"
    ),
    .groups = "drop"
  ) %>%
  # join per-player totals (TEAM/HOME/AWAY/STARTER/MINS + totals)
  dplyr::left_join(
    player_totals,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "PLAYER_ID",
      "PLAYER_NAME"
    )
  ) %>%
  # join top-5 lineup IDs by shared time
  dplyr::left_join(
    pair_lineup_rank,
    by = c(
      "ESPN_GAME_ID",
      "ESPN_TEAM_ID",
      "PLAYER_ID",
      "PLAYER_NAME",
      "TEAMMATE_ID",
      "TEAMMATE_NAME"
    )
  ) %>%
  # optional: flags for which quarters they shared the floor
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("SHARED_LINEUP_IN_Q"),
      ~ .x > 0,
      .names = "WITH_{.col}"
    )
  )

message("[✓] player_lineup_summary_df built (context + shared seconds + top 5 shared lineups).")

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Player–Teammate Rotation Summary (player_lineup_summary_df) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#
#     #ooooooooo.   oooo                                                ooooo                                                      .   
#     `888   `Y88. `888                                                `888'                                                    .o8   
#      888   .d88'  888   .oooo.   oooo    ooo  .ooooo.  oooo d8b       888  ooo. .oo.  .oo.   oo.ooooo.   .oooo.    .ooooo.  .o888oo 
#      888ooo88P'   888  `P  )88b   `88.  .8'  d88' `88b `888""8P       888  `888P"Y88bP"Y88b   888' `88b `P  )88b  d88' `"Y8   888   
#      888          888   .oP"888    `88..8'   888ooo888  888           888   888   888   888   888   888  .oP"888  888         888   
#      888          888  d8(  888     `888'    888    .o  888           888   888   888   888   888   888 d8(  888  888   .o8   888 . 
#     o888o        o888o `Y888""8o     .8'     `Y8bod8P' d888b         o888o o888o o888o o888o  888bod8P' `Y888""8o `Y8bod8P'   "888" 
#                                  .o..P'                                                       888                                   
#                                  `Y8P'                                                       o888o                                  
#                                                                                                                                
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                                                                                                                




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Player Impact Assessment (Minutes Delta) — HISTORICAL BUILD ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====================================================================================
# Player Impact Assessment (Minutes Delta) — HISTORICAL BUILD v2
# ====================================================================================
# Output: player_impact_assessment_delta_<season_token>.csv
#
# Changes from v1:
#   1. Quarter-level minutes (PLAYER_LINEUP_IN_Q1-Q4) carried through entire pipeline
#      alongside game-level MINS. Values are in SECONDS — divided by 60 on intake.
#   2. Baseline replaced: rolling 5-game average per player (instead of first game only)
#      so deltas stay current throughout the season as roles evolve.
#   3. Output includes DELTA_MINS_Q1-Q4 per affected player per OUT player —
#      directly consumable by simulate_quarter_offense_player() for mins_q replacement.
#   4. Usage weight columns added (usage_share_Q1-Q4) for zero-sum usage bump logic
#      in the sim — each player's share of team shots per quarter in baseline.
#
# Output columns used by sim:
#   ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE
#   OUT_PLAYER_ID, OUT_PLAYER_NAME
#   AFFECTED_PLAYER_ID, AFFECTED_PLAYER_NAME
#   BASE_MINS_Q1-Q4         — rolling 5-game avg minutes per quarter (healthy baseline)
#   MINS_Q1-Q4              — actual minutes this game
#   DELTA_MINS_Q1-Q4        — actual minus baseline (positive = got more minutes)
#   BASE_USAGE_Q1-Q4        — baseline usage share per quarter (for usage bump logic)
#   DELTA_MINS_48           — game-level delta (kept for backward compat)
# ====================================================================================

suppressWarnings({
  library(dplyr)
  library(data.table)
  library(stringr)
  library(tidyr)
})

stopifnot(exists("player_lineup_summary_df"), exists("season_token"))

# -----------------------------
# Injury file load
# -----------------------------
injury_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)"
injury_data_file <- file.path(injury_dir, paste0("Injury_Database_", season_token, ".csv"))
injury_data <- fread(injury_data_file, colClasses = "character", encoding = "UTF-8")
stopifnot(exists("injury_data"))

# -----------------------------
# 0) Helpers
# -----------------------------
num0 <- function(x, d = 0) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), d, x) }

pick_first <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) NA_character_ else hit[1]
}

# -----------------------------
# 1) Normalize required fields
# -----------------------------
pr_raw <- player_lineup_summary_df

date_col  <- pick_first(pr_raw, c("GAME_DATE","game_date","game_date_dt","DATE","date"))
pid_col   <- pick_first(pr_raw, c("PLAYER_ID","ESPN_PLAYER_ID","espn_player_id"))
tid_col   <- pick_first(pr_raw, c("ESPN_TEAM_ID","espn_team_id","TEAM_ID","team_id"))
gid_col   <- pick_first(pr_raw, c("ESPN_GAME_ID","espn_game_id","GAME_ID","game_id"))
mins_col  <- pick_first(pr_raw, c("MINS","MIN","minutes","PLAYER_IN_CGS","PLAYER_LINEUP_IN_CGS"))
pname_col <- pick_first(pr_raw, c("PLAYER_NAME","PLAYER","player","NAME"))

if (is.na(date_col))  stop("Missing date column.")
if (is.na(pid_col))   stop("Missing player id column.")
if (is.na(tid_col) || is.na(gid_col)) stop("Missing team/game id column.")
if (is.na(mins_col))  stop("Missing minutes column.")

# Quarter minute columns — PLAYER_LINEUP_IN_Q1-Q4 are in SECONDS, divide by 60
q_cols <- c("PLAYER_LINEUP_IN_Q1","PLAYER_LINEUP_IN_Q2",
            "PLAYER_LINEUP_IN_Q3","PLAYER_LINEUP_IN_Q4")
has_q_cols <- all(q_cols %in% names(pr_raw))
if (!has_q_cols) warning("Quarter minute columns not found — quarter deltas will be NA.")

# -----------------------------
# STEP A: Collapse to one row per player-game-team
# player_lineup_summary_df is pair-level (player x teammate) so minutes repeat.
# Take max() for game minutes, sum() is wrong here since rows are pairs not stints.
# For quarter columns also take max() — same player appears in multiple pair rows
# but PLAYER_LINEUP_IN_Qx is the same value for that player across all pair rows.
# -----------------------------
pr_player_game <- pr_raw %>%
  transmute(
    ESPN_GAME_ID         = as.character(.data[[gid_col]]),
    ESPN_TEAM_ID         = as.character(.data[[tid_col]]),
    GAME_DATE            = as.Date(.data[[date_col]]),
    AFFECTED_PLAYER_ID   = as.character(.data[[pid_col]]),
    AFFECTED_PLAYER_NAME = if (!is.na(pname_col)) as.character(.data[[pname_col]]) else NA_character_,
    MINS                 = num0(.data[[mins_col]], NA_real_),
    # Quarter minutes: seconds -> minutes
    MINS_Q1 = if (has_q_cols) num0(.data[["PLAYER_LINEUP_IN_Q1"]], NA_real_) / 60 else NA_real_,
    MINS_Q2 = if (has_q_cols) num0(.data[["PLAYER_LINEUP_IN_Q2"]], NA_real_) / 60 else NA_real_,
    MINS_Q3 = if (has_q_cols) num0(.data[["PLAYER_LINEUP_IN_Q3"]], NA_real_) / 60 else NA_real_,
    MINS_Q4 = if (has_q_cols) num0(.data[["PLAYER_LINEUP_IN_Q4"]], NA_real_) / 60 else NA_real_
  ) %>%
  filter(
    !is.na(GAME_DATE),
    !is.na(ESPN_GAME_ID), ESPN_GAME_ID != "",
    !is.na(ESPN_TEAM_ID), ESPN_TEAM_ID != "",
    !is.na(AFFECTED_PLAYER_ID), AFFECTED_PLAYER_ID != ""
  ) %>%
  group_by(ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE, AFFECTED_PLAYER_ID) %>%
  summarise(
    AFFECTED_PLAYER_NAME = dplyr::first(na.omit(AFFECTED_PLAYER_NAME)),
    MINS   = suppressWarnings(max(MINS,   na.rm = TRUE)),
    MINS_Q1 = suppressWarnings(max(MINS_Q1, na.rm = TRUE)),
    MINS_Q2 = suppressWarnings(max(MINS_Q2, na.rm = TRUE)),
    MINS_Q3 = suppressWarnings(max(MINS_Q3, na.rm = TRUE)),
    MINS_Q4 = suppressWarnings(max(MINS_Q4, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Replace -Inf from max() on all-NA with NA
  mutate(across(c(MINS, MINS_Q1, MINS_Q2, MINS_Q3, MINS_Q4),
                ~ifelse(is.infinite(.), NA_real_, .)))

pr <- pr_player_game

# -----------------------------
# 2) OT-safe per-48 minutes (game level, kept for backward compat)
# -----------------------------
team_game_minutes <- pr %>%
  group_by(ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE) %>%
  summarise(
    TEAM_MINS_SUM       = sum(MINS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    GAME_MINUTES_ACTUAL = ifelse(TEAM_MINS_SUM > 0, TEAM_MINS_SUM / 5, NA_real_)
  )

pr48 <- pr %>%
  left_join(team_game_minutes, by = c("ESPN_GAME_ID","ESPN_TEAM_ID","GAME_DATE")) %>%
  mutate(
    MINS_48 = ifelse(!is.na(GAME_MINUTES_ACTUAL) & GAME_MINUTES_ACTUAL > 0,
                     MINS * (48 / GAME_MINUTES_ACTUAL),
                     NA_real_)
  )

# -----------------------------
# 3) Rolling 5-game baseline per player per team
#    Baseline for game N = average of the 5 most recent games BEFORE game N
#    where the player was active (MINS > 0).
#    This keeps the baseline current throughout the season.
#    Also compute baseline usage share per quarter = player Q mins / team Q mins
#    (used for zero-sum usage bump logic in the sim).
# -----------------------------

# Team quarter totals per game (for usage share calculation)
team_quarter_totals <- pr48 %>%
  group_by(ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE) %>%
  summarise(
    TEAM_Q1 = sum(MINS_Q1, na.rm = TRUE),
    TEAM_Q2 = sum(MINS_Q2, na.rm = TRUE),
    TEAM_Q3 = sum(MINS_Q3, na.rm = TRUE),
    TEAM_Q4 = sum(MINS_Q4, na.rm = TRUE),
    .groups = "drop"
  )

pr48 <- pr48 %>%
  left_join(team_quarter_totals, by = c("ESPN_GAME_ID","ESPN_TEAM_ID","GAME_DATE")) %>%
  mutate(
    USAGE_Q1 = ifelse(TEAM_Q1 > 0, MINS_Q1 / TEAM_Q1, NA_real_),
    USAGE_Q2 = ifelse(TEAM_Q2 > 0, MINS_Q2 / TEAM_Q2, NA_real_),
    USAGE_Q3 = ifelse(TEAM_Q3 > 0, MINS_Q3 / TEAM_Q3, NA_real_),
    USAGE_Q4 = ifelse(TEAM_Q4 > 0, MINS_Q4 / TEAM_Q4, NA_real_)
  )

# Sort for rolling window
pr48 <- pr48 %>%
  arrange(ESPN_TEAM_ID, AFFECTED_PLAYER_ID, GAME_DATE, ESPN_GAME_ID)

# Compute rolling 5-game baseline — lag so current game not included
compute_rolling_baseline <- function(df, n = 5) {
  df %>%
    group_by(ESPN_TEAM_ID, AFFECTED_PLAYER_ID) %>%
    mutate(
      # Only use games where player was active
      active = MINS > 0 & !is.na(MINS),
      # Rolling mean of last n active games before current game
      BASE_MINS_48 = zoo::rollapplyr(
        ifelse(active, MINS_48, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_MINS_Q1 = zoo::rollapplyr(
        ifelse(active, MINS_Q1, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_MINS_Q2 = zoo::rollapplyr(
        ifelse(active, MINS_Q2, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_MINS_Q3 = zoo::rollapplyr(
        ifelse(active, MINS_Q3, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_MINS_Q4 = zoo::rollapplyr(
        ifelse(active, MINS_Q4, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_USAGE_Q1 = zoo::rollapplyr(
        ifelse(active, USAGE_Q1, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_USAGE_Q2 = zoo::rollapplyr(
        ifelse(active, USAGE_Q2, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_USAGE_Q3 = zoo::rollapplyr(
        ifelse(active, USAGE_Q3, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      ),
      BASE_USAGE_Q4 = zoo::rollapplyr(
        ifelse(active, USAGE_Q4, NA_real_),
        width = n, FUN = function(x) mean(x[!is.na(x)], na.rm = TRUE),
        fill = NA, partial = TRUE, align = "right"
      )
    ) %>%
    # Lag by 1 so baseline doesn't include current game
    mutate(
      BASE_MINS_48  = lag(BASE_MINS_48),
      BASE_MINS_Q1  = lag(BASE_MINS_Q1),
      BASE_MINS_Q2  = lag(BASE_MINS_Q2),
      BASE_MINS_Q3  = lag(BASE_MINS_Q3),
      BASE_MINS_Q4  = lag(BASE_MINS_Q4),
      BASE_USAGE_Q1 = lag(BASE_USAGE_Q1),
      BASE_USAGE_Q2 = lag(BASE_USAGE_Q2),
      BASE_USAGE_Q3 = lag(BASE_USAGE_Q3),
      BASE_USAGE_Q4 = lag(BASE_USAGE_Q4)
    ) %>%
    ungroup()
}

# Requires zoo packag

pr48_baseline <- compute_rolling_baseline(pr48, n = 5)

# -----------------------------
# 4) OUT players per team-date from injury_data
# -----------------------------
inj_team_col  <- pick_first(injury_data, c("espn_team_id","ESPN_TEAM_ID","team_id"))
inj_pid_col   <- pick_first(injury_data, c("espn_player_id","ESPN_PLAYER_ID","player_id"))
inj_name_col  <- pick_first(injury_data, c("player_clean","player","PLAYER","name"))
inj_stat_col  <- pick_first(injury_data, c("status","STATUS"))
inj_date_col  <- pick_first(injury_data, c("game_date_dt","GAME_DATE","date","game_date"))

if (is.na(inj_team_col)) stop("injury_data missing team id column.")
if (is.na(inj_pid_col))  stop("injury_data missing player id column.")
if (is.na(inj_stat_col)) stop("injury_data missing status column.")
if (is.na(inj_date_col)) stop("injury_data missing date column.")

inj <- injury_data %>%
  transmute(
    ESPN_TEAM_ID    = as.character(.data[[inj_team_col]]),
    GAME_DATE       = as.Date(.data[[inj_date_col]]),
    OUT_PLAYER_ID   = as.character(.data[[inj_pid_col]]),
    OUT_PLAYER_NAME = if (!is.na(inj_name_col)) as.character(.data[[inj_name_col]]) else NA_character_,
    status          = tolower(as.character(.data[[inj_stat_col]]))
  ) %>%
  filter(
    !is.na(GAME_DATE),
    !is.na(ESPN_TEAM_ID), ESPN_TEAM_ID != "",
    !is.na(OUT_PLAYER_ID), OUT_PLAYER_ID != "",
    status == "out"
  ) %>%
  distinct(ESPN_TEAM_ID, GAME_DATE, OUT_PLAYER_ID, .keep_all = TRUE)

# Join OUT list onto games we have
out_players_team_game <- team_game_minutes %>%
  select(ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE) %>%
  left_join(inj, by = c("ESPN_TEAM_ID","GAME_DATE")) %>%
  filter(!is.na(OUT_PLAYER_ID))

# Drop players marked OUT who actually played
played_ids <- pr48 %>%
  filter(MINS > 0) %>%
  distinct(ESPN_TEAM_ID, GAME_DATE, AFFECTED_PLAYER_ID)

out_players_team_game <- out_players_team_game %>%
  anti_join(
    played_ids,
    by = c("ESPN_TEAM_ID"  = "ESPN_TEAM_ID",
           "GAME_DATE"     = "GAME_DATE",
           "OUT_PLAYER_ID" = "AFFECTED_PLAYER_ID")
  )

# -----------------------------
# 5) Affected players with rolling baseline + deltas
# -----------------------------
affected_team_game <- pr48_baseline %>%
  select(
    ESPN_GAME_ID, ESPN_TEAM_ID, GAME_DATE,
    AFFECTED_PLAYER_ID, AFFECTED_PLAYER_NAME,
    MINS, MINS_48, MINS_Q1, MINS_Q2, MINS_Q3, MINS_Q4,
    GAME_MINUTES_ACTUAL,
    BASE_MINS_48,
    BASE_MINS_Q1, BASE_MINS_Q2, BASE_MINS_Q3, BASE_MINS_Q4,
    BASE_USAGE_Q1, BASE_USAGE_Q2, BASE_USAGE_Q3, BASE_USAGE_Q4
  ) %>%
  mutate(
    # Fallback: if no baseline yet (first few games), use game average
    BASE_MINS_48  = ifelse(is.na(BASE_MINS_48), MINS_48, BASE_MINS_48),
    BASE_MINS_Q1  = ifelse(is.na(BASE_MINS_Q1), MINS_Q1, BASE_MINS_Q1),
    BASE_MINS_Q2  = ifelse(is.na(BASE_MINS_Q2), MINS_Q2, BASE_MINS_Q2),
    BASE_MINS_Q3  = ifelse(is.na(BASE_MINS_Q3), MINS_Q3, BASE_MINS_Q3),
    BASE_MINS_Q4  = ifelse(is.na(BASE_MINS_Q4), MINS_Q4, BASE_MINS_Q4),
    # Deltas: positive = player got more minutes than baseline (absorbed injured player's load)
    DELTA_MINS_48 = ifelse(is.na(MINS_48), NA_real_, MINS_48 - BASE_MINS_48),
    DELTA_MINS_Q1 = ifelse(is.na(MINS_Q1), NA_real_, MINS_Q1 - BASE_MINS_Q1),
    DELTA_MINS_Q2 = ifelse(is.na(MINS_Q2), NA_real_, MINS_Q2 - BASE_MINS_Q2),
    DELTA_MINS_Q3 = ifelse(is.na(MINS_Q3), NA_real_, MINS_Q3 - BASE_MINS_Q3),
    DELTA_MINS_Q4 = ifelse(is.na(MINS_Q4), NA_real_, MINS_Q4 - BASE_MINS_Q4)
  )

# -----------------------------
# 6) Expand OUT x AFFECTED per team-game
# -----------------------------
impact_df <- out_players_team_game %>%
  inner_join(
    affected_team_game,
    by = c("ESPN_GAME_ID","ESPN_TEAM_ID","GAME_DATE")
  ) %>%
  filter(OUT_PLAYER_ID != AFFECTED_PLAYER_ID) %>%
  select(
    GAME_DATE,
    ESPN_GAME_ID,
    ESPN_TEAM_ID,
    
    OUT_PLAYER_ID,
    OUT_PLAYER_NAME,
    
    AFFECTED_PLAYER_ID,
    AFFECTED_PLAYER_NAME,
    
    # Actual minutes this game
    MINS,
    MINS_Q1, MINS_Q2, MINS_Q3, MINS_Q4,
    MINS_48,
    GAME_MINUTES_ACTUAL,
    
    # Rolling 5-game baseline
    BASE_MINS_48,
    BASE_MINS_Q1, BASE_MINS_Q2, BASE_MINS_Q3, BASE_MINS_Q4,
    
    # Baseline usage shares (for zero-sum usage bump in sim)
    BASE_USAGE_Q1, BASE_USAGE_Q2, BASE_USAGE_Q3, BASE_USAGE_Q4,
    
    # Deltas (what the sim applies)
    DELTA_MINS_48,
    DELTA_MINS_Q1, DELTA_MINS_Q2, DELTA_MINS_Q3, DELTA_MINS_Q4
  )

# -----------------------------
# 6b) Filter to meaningful deltas only — STRUCTURAL FIX
#     Instead of just a threshold filter, require that the affected player
#     shows a consistent pattern across multiple absences of the same OUT player.
#     This removes correlation noise and keeps only genuine rotation responses.
#
#     Step 1: Count observations per OUT/AFFECTED pair across all games
#     Step 2: Keep only pairs with >= 3 observations (consistent pattern)
#     Step 3: Apply threshold filter on top to remove trivial changes
#     Step 4: Cap at top 5 affected players per OUT player per game by magnitude
# -----------------------------

# Step 1 & 2: Minimum observation count per OUT/AFFECTED pair
pair_obs_count <- impact_df %>%
  group_by(ESPN_TEAM_ID, OUT_PLAYER_ID, AFFECTED_PLAYER_ID) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= 3)

impact_df <- impact_df %>%
  inner_join(pair_obs_count,
             by = c("ESPN_TEAM_ID", "OUT_PLAYER_ID", "AFFECTED_PLAYER_ID"))

# Step 3: Threshold — only meaningful minute changes
impact_df <- impact_df %>%
  filter(abs(DELTA_MINS_48) >= 1.5)

# Step 4: Cap at top 5 affected players per OUT player per game by delta magnitude
impact_df <- impact_df %>%
  group_by(ESPN_GAME_ID, ESPN_TEAM_ID, OUT_PLAYER_ID) %>%
  slice_max(abs(DELTA_MINS_48), n = 5, with_ties = FALSE) %>%
  ungroup()

# Drop helper column
impact_df <- impact_df %>% select(-n_obs)

# -----------------------------
# 7) Save
# -----------------------------
out_dir  <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations"
out_path <- file.path(out_dir, paste0("player_impact_assessment_delta_", season_token, ".csv"))

data.table::fwrite(impact_df, out_path)

message("[✓] player_impact_assessment_delta saved: ", out_path, " | rows=", nrow(impact_df))

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Player Impact Assessment (Minutes Delta) v2 ===
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ---------------------------- Paths ----------------------------
base_stats_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_",
  season_token, ".csv"
)

BaseStats_Player <- read_csv(
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

# ============================================================
# Build lookup: ESPN_PLAYER_ID -> headshot
# ============================================================
player_headshot_lookup <- BaseStats_Player %>%
  select(
    espn_player_id,
    headshot
  ) %>%
  mutate(
    espn_player_id = as.character(espn_player_id)
  )

# ============================================================
# Helper function: map headshot for a single lineup column
# ============================================================
map_headshot_col <- function(df, player_col, lookup_df) {
  df %>%
    mutate(
      "{player_col}_headshot" := lookup_df$headshot[
        match(
          as.character(.data[[player_col]]),
          lookup_df$espn_player_id
        )
      ]
    )
}

# ============================================================
# 1) Apply to lineup_team_level_summary_df
# ============================================================
team_lineup_cols <- c(
  "home_P1_espn_id", "home_P2_espn_id", "home_P3_espn_id",
  "home_P4_espn_id", "home_P5_espn_id",
  "away_P1_espn_id", "away_P2_espn_id", "away_P3_espn_id",
  "away_P4_espn_id", "away_P5_espn_id"
)

for (col in team_lineup_cols) {
  lineup_team_level_summary_df <-
    map_headshot_col(
      lineup_team_level_summary_df,
      col,
      player_headshot_lookup
    )
}

# ============================================================
# 2) Apply to lineup_summary_df
# ============================================================
lineup_cols <- c(
  "LINEUP_P1_espn_id", "LINEUP_P2_espn_id", "LINEUP_P3_espn_id",
  "LINEUP_P4_espn_id", "LINEUP_P5_espn_id"
)

for (col in lineup_cols) {
  lineup_summary_df <-
    map_headshot_col(
      lineup_summary_df,
      col,
      player_headshot_lookup
    )
}

# Cleanup
rm(player_headshot_lookup, map_headshot_col, team_lineup_cols, lineup_cols)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Save lineup_team_level_summary_df, lineup_summary_df and player_lineup_summary_df to 12. Player Rotations ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


save_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations"


fwrite(
  lineup_team_level_summary_df,
  file = paste0(
    save_dir,
    "/pm_nba_player_rotations_10M_",
    season_token,
    ".csv"
  )
)

message("[✓] Saved 10M lineup rotations")


fwrite(
  lineup_summary_df,
  file = paste0(
    save_dir,
    "/pm_nba_player_rotations_5M_",
    season_token,
    ".csv"
  )
)

message("[✓] Saved 5M lineup rotations")


fwrite(
  player_lineup_summary_df,
  file = paste0(
    save_dir,
    "/pm_nba_player_level_rotations_",
    season_token,
    ".csv"
  )
)

message("[✓] Saved player-level lineup rotations")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Save lineup_team_level_summary_df, lineup_summary_df and player_lineup_summary_df to 12. Player Rotations ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀