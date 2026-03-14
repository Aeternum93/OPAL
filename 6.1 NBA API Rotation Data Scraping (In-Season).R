# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== NBA LINEUP BUILDER (V5) ====
#     - combines rotations_*.csv into rotations_master_<season>.csv
#     - rebuilds TRUE 5-man lineup segments from IN/OUT intervals
#     - IN_TIME_REAL / OUT_TIME_REAL normalized to COUNTDOWN format
#     - output: nba_lineup_data_<season>.csv
#     - strict row-level + game-level quality enforcement
#     - quality_flag + game_quality columns drive golden copy replacement
#
# Column renames from V4:
#     PopMac_Game -> NBA_Game
#     PopMac_Date -> NBA_Date
#     pm_fix()    -> nba_fix()
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

rm(list = setdiff(ls(), c(
  "current_date","formatted_date","formatted_year",
  "next_year_date","pbp_season","season_token","season_token2",
  "base_path","logo_dir","essential_names",
  "scripts","batches","run_script","run_batch"
)))

library(data.table)
library(stringr)
library(lubridate)
library(dplyr)

# ============================================================
# STANDARD TEAM ABBREVIATION MAP (used everywhere)
# ============================================================
nba_fix <- function(x) {
  x <- as.character(x)
  x[x == "NOP"]  <- "NOR"
  x[x == "PHX"]  <- "PHO"
  x[x == "UTA"]  <- "UTH"
  x[x == "SA"]   <- "SAS"
  x[x == "GS"]   <- "GSW"
  x[x == "NY"]   <- "NYK"
  x[x == "NO"]   <- "NOR"
  x[x == "WSH"]  <- "WAS"
  x[x == "UTAH"] <- "UTH"
  x
}

VALID_TEAMS <- c(
  "ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
  "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOR","NYK",
  "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTH","WAS"
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================
mmss_to_sec <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) {
    if (length(p) != 2) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2])
  }, numeric(1))
}

format_clock <- function(sec, period) {
  sec <- as.numeric(sec)
  quarter_max <- ifelse(grepl("Period [56]", period), 300, 720)
  remaining <- sec %% quarter_max
  if (remaining == 0) remaining <- quarter_max
  sprintf("%d:%02d", as.integer(floor(remaining / 60)), as.integer(remaining %% 60))
}

# ------------------------------------------------------------
# 0) PATHS
# ------------------------------------------------------------
pm_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine"

nba_lineup_path <- file.path(pm_dir, paste0("nba_lineup_data_", season_token, ".csv"))
rot_master_path <- file.path(pm_dir, paste0("rotations_master_", season_token, ".csv"))

team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"

# ------------------------------------------------------------
# 1) COMBINE ROTATIONS FILES -> rotations_master
# ------------------------------------------------------------
rot_files <- list.files(
  pm_dir,
  pattern = "^rotations_\\d{4}-\\d{2}-\\d{2}_to_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)
if (length(rot_files) == 0) stop("No rotations_*.csv files found in Popcorn Machine folder.")

rot_list <- lapply(rot_files, function(fp) {
  dt <- fread(fp, fill = TRUE)
  for (nm in names(dt)) set(dt, j = nm, value = as.character(dt[[nm]]))
  dt[, source_file := basename(fp)]
  dt
})

rot_master <- rbindlist(rot_list, fill = TRUE)

required_cols <- c(
  "GAME_ID","TEAM_ID","PLAYER_FIRST","PLAYER_LAST",
  "IN_TIME_REAL","OUT_TIME_REAL","game_date","home_away"
)
missing_required <- setdiff(required_cols, names(rot_master))
if (length(missing_required) > 0) {
  stop(paste0("rotations master missing required cols: ", paste(missing_required, collapse = ", ")))
}

# Normalize types
rot_master[, GAME_ID       := as.character(GAME_ID)]
rot_master[, TEAM_ID       := as.character(TEAM_ID)]
rot_master[, home_away     := tolower(as.character(home_away))]
rot_master[, PLAYER_FIRST  := as.character(PLAYER_FIRST)]
rot_master[, PLAYER_LAST   := as.character(PLAYER_LAST)]
rot_master[, IN_TIME_REAL  := suppressWarnings(as.numeric(IN_TIME_REAL))  / 10]
rot_master[, OUT_TIME_REAL := suppressWarnings(as.numeric(OUT_TIME_REAL)) / 10]

# Per-row countdown normalization
needs_swap <- rot_master[IN_TIME_REAL < OUT_TIME_REAL]
if (nrow(needs_swap) > 0) {
  message("Normalizing ", nrow(needs_swap), " rows with IN < OUT (swapping to countdown)")
  rot_master[IN_TIME_REAL < OUT_TIME_REAL,
             c("IN_TIME_REAL", "OUT_TIME_REAL") := .(OUT_TIME_REAL, IN_TIME_REAL)]
}

fwrite(rot_master, rot_master_path)
message("Saved rotations master: ", rot_master_path)

rot_dt <- copy(rot_master)

# ------------------------------------------------------------
# 2) ATTACH TEAM METADATA
# ------------------------------------------------------------
team_map <- fread(team_map_path, colClasses = "character")

need_team_cols <- c("nba_team_id","espn_team_id","team_short_name","nbaapi_team_long_name")
missing_team_cols <- setdiff(need_team_cols, names(team_map))
if (length(missing_team_cols) > 0) {
  stop(paste0("team_id_mapping.csv missing cols: ", paste(missing_team_cols, collapse = ", ")))
}

setnames(team_map, "nba_team_id", "TEAM_ID")

rot_dt <- merge(
  rot_dt,
  team_map[, .(TEAM_ID, espn_team_id, team_short_name, nbaapi_team_long_name)],
  by = "TEAM_ID",
  all.x = TRUE
)

# Normalize team codes at source
rot_dt[, team_short_name := nba_fix(team_short_name)]

# ------------------------------------------------------------
# 3) DATE -> NBA_Date
# ------------------------------------------------------------
rot_dt[, game_date := trimws(as.character(game_date))]
rot_dt[, game_date_parsed := as.Date(game_date, format="%Y%m%d")]
rot_dt[is.na(game_date_parsed), game_date_parsed := as.Date(game_date, format="%Y-%m-%d")]
rot_dt[is.na(game_date_parsed), game_date_parsed := suppressWarnings(mdy(game_date))]
rot_dt[, NBA_Date := format(game_date_parsed, "%Y%m%d")]

# ------------------------------------------------------------
# 4) BUILD NBA_Game + Opp Map
# ------------------------------------------------------------
game_meta <- unique(rot_dt[, .(GAME_ID, home_away, team_short_name, NBA_Date)])
game_meta <- game_meta[, .(
  NBA_Date    = NBA_Date[1],
  away_abbrev = team_short_name[home_away == "away"][1],
  home_abbrev = team_short_name[home_away == "home"][1]
), by = GAME_ID]
game_meta[, NBA_Game := paste0(away_abbrev, home_abbrev)]
game_meta[, game_id  := paste0(NBA_Game, "_", NBA_Date)]

team_per_game <- unique(rot_dt[, .(GAME_ID, TEAM_ID, team_short_name)])
opp_map <- team_per_game[
  team_per_game, on = .(GAME_ID), allow.cartesian = TRUE
][TEAM_ID != i.TEAM_ID, .(GAME_ID, TEAM_ID, Opp = i.team_short_name)]

# ------------------------------------------------------------
# 5) TRUE LINEUP ENGINE (interval-based)
# ------------------------------------------------------------
rot_dt[, player_name := str_squish(paste(PLAYER_FIRST, PLAYER_LAST))]

build_true_lineups <- function(dt_game_team) {
  dt_game_team <- dt_game_team[!is.na(IN_TIME_REAL) & !is.na(OUT_TIME_REAL)]
  if (nrow(dt_game_team) == 0) return(data.table())
  
  all_times <- sort(unique(c(dt_game_team$IN_TIME_REAL, dt_game_team$OUT_TIME_REAL)), decreasing = TRUE)
  if (length(all_times) < 2) return(data.table())
  
  out <- list()
  for (idx in seq_len(length(all_times) - 1)) {
    seg_start <- all_times[idx]
    seg_end   <- all_times[idx + 1]
    if (seg_start <= seg_end) next
    
    mid <- (seg_start + seg_end) / 2
    on_court <- dt_game_team[IN_TIME_REAL >= mid & OUT_TIME_REAL <= mid, unique(player_name)]
    
    if (length(on_court) == 5) {
      lp <- sort(on_court)
      out[[length(out) + 1]] <- data.table(
        start_time = seg_start, end_time = seg_end,
        P1 = lp[1], P2 = lp[2], P3 = lp[3], P4 = lp[4], P5 = lp[5]
      )
    }
  }
  
  if (length(out) == 0) return(data.table())
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

lineup_dt <- rot_dt[, build_true_lineups(.SD), by = .(GAME_ID, TEAM_ID)]

if (nrow(lineup_dt) == 0) {
  message("No true lineups produced. Check IN/OUT timing.")
  quit(save = "no")
}

# ------------------------------------------------------------
# 6) DERIVE PERIOD + CLOCK FROM COUNTDOWN VALUES
# Use fixed period boundaries instead of game_max offset
# Regulation: Q1=2880-2161, Q2=2160-1441, Q3=1440-721, Q4=720-1
# OT1: 3180-2881, OT2: 3480-3181
# We detect OT by checking if any player has IN_TIME > 2880
# ------------------------------------------------------------

# Detect game length from rotation data
game_max_dt <- rot_dt[, .(game_max = max(IN_TIME_REAL, na.rm = TRUE)), by = GAME_ID]

# Determine number of periods per game
# Regulation = 2880 sec. Each OT = 300 sec.
game_max_dt[, n_periods := fcase(
  game_max <= 2900,  4L,   # regulation (with small buffer)
  game_max <= 3200,  5L,   # 1 OT
  game_max <= 3500,  6L,   # 2 OT
  game_max <= 3800,  7L,   # 3 OT
  default = 8L              # 4+ OT
)]

# Build period boundaries: work backwards from 0
# Period 4 = 0 to 720, Period 3 = 720 to 1440, etc.
# OT periods stack on top: OT1 = 2880 to 3180, OT2 = 3180 to 3480
game_max_dt[, reg_top := n_periods * 720 - (pmax(n_periods - 4L, 0L) * 720) + pmax(n_periods - 4L, 0L) * 300]

lineup_dt <- merge(lineup_dt, game_max_dt[, .(GAME_ID, n_periods, game_max)], by = "GAME_ID", all.x = TRUE)

# Assign periods using fixed 720-sec boundaries from bottom up
# Period 4 always = 0 to 720
# Period 3 always = 720 to 1440
# Period 2 always = 1440 to 2160
# Period 1 always = 2160 to 2880
# OT1 = 2880 to 3180
# OT2 = 3180 to 3480
lineup_dt[, Period := fcase(
  start_time <= 720,                          "Period 4",
  start_time <= 1440,                         "Period 3",
  start_time <= 2160,                         "Period 2",
  start_time <= 2880,                         "Period 1",
  start_time <= 3180 & n_periods >= 5,        "Period 5",
  start_time <= 3480 & n_periods >= 6,        "Period 6",
  default = "Period 1"  # fallback
)]

lineup_dt[, c("n_periods", "game_max") := NULL]

# Format clock: convert raw seconds to mm:ss within the period
# Each regulation period is 720 sec, OT is 300 sec
# Clock counts down within the period
lineup_dt[, period_num := as.integer(gsub("Period ", "", Period))]

# Calculate seconds remaining in this period
lineup_dt[, period_base := fcase(
  period_num == 1L, 2880L,
  period_num == 2L, 2160L,
  period_num == 3L, 1440L,
  period_num == 4L, 720L,
  period_num == 5L, 3180L,
  period_num == 6L, 3480L,
  default = 720L
)]
lineup_dt[, period_bottom := fcase(
  period_num == 1L, 2160L,
  period_num == 2L, 1440L,
  period_num == 3L, 720L,
  period_num == 4L, 0L,
  period_num == 5L, 2880L,
  period_num == 6L, 3180L,
  default = 0L
)]

# Clock = seconds from period bottom (0:00 = at the bottom boundary)
lineup_dt[, clock_on  := start_time - period_bottom]
lineup_dt[, clock_off := end_time - period_bottom]

# Cap at period max (720 for reg, 300 for OT)
lineup_dt[, period_max := fifelse(period_num >= 5L, 300L, 720L)]
lineup_dt[clock_on > period_max, clock_on := period_max]
lineup_dt[clock_off < 0, clock_off := 0]

# Format to mm:ss
lineup_dt[, Time_On  := sprintf("%d:%02d", as.integer(floor(clock_on / 60)), as.integer(clock_on %% 60))]
lineup_dt[, Time_Off := sprintf("%d:%02d", as.integer(floor(clock_off / 60)), as.integer(clock_off %% 60))]

# Fix any Time_Off > Time_On (shouldn't happen but safety)
lineup_dt[, ton := mmss_to_sec(Time_On)]
lineup_dt[, toff := mmss_to_sec(Time_Off)]
lineup_dt[toff > ton, Time_Off := "0:00"]

# Cleanup temp columns
lineup_dt[, c("period_num", "period_base", "period_bottom", "clock_on", "clock_off", 
              "period_max", "ton", "toff") := NULL]

# ------------------------------------------------------------
# 7) ATTACH METADATA + BUILD FINAL SCHEMA
# ------------------------------------------------------------
lineup_dt <- merge(lineup_dt, game_meta[, .(GAME_ID, NBA_Game, NBA_Date, game_id)],
                   by = "GAME_ID", all.x = TRUE)

team_meta_dt <- unique(rot_dt[, .(GAME_ID, TEAM_ID, Team = team_short_name, Team_Long = nbaapi_team_long_name)])
lineup_dt <- merge(lineup_dt, team_meta_dt, by = c("GAME_ID","TEAM_ID"), all.x = TRUE)
lineup_dt <- merge(lineup_dt, opp_map, by = c("GAME_ID","TEAM_ID"), all.x = TRUE)

lineup_dt[, Team := nba_fix(Team)]
lineup_dt[, Opp  := nba_fix(Opp)]

nba_lineup <- lineup_dt[, .(
  NBA_Game  = as.character(NBA_Game),
  NBA_Date  = as.character(NBA_Date),
  game_id   = as.character(game_id),
  Period    = as.character(Period),
  Time_On   = as.character(Time_On),
  Time_Off  = as.character(Time_Off),
  Team_Long = as.character(Team_Long),
  Team      = as.character(Team),
  Opp       = as.character(Opp),
  P_M       = NA_character_,
  P1 = as.character(P1), P2 = as.character(P2), P3 = as.character(P3),
  P4 = as.character(P4), P5 = as.character(P5)
)]

nba_lineup <- unique(nba_lineup[!is.na(game_id) & !is.na(Team) & !is.na(P1) & !is.na(Time_On) & !is.na(Time_Off)])

# ------------------------------------------------------------
# 7A) REPAIR MISSING TEAM / OPP
# ------------------------------------------------------------
nba_lineup[, Team := trimws(Team)]
nba_lineup[, Opp  := trimws(Opp)]
nba_lineup[Team == "", Team := NA_character_]
nba_lineup[Opp  == "", Opp  := NA_character_]

teams_from_team <- nba_lineup[!is.na(Team), .(game_id, team = Team)]
teams_from_opp  <- nba_lineup[!is.na(Opp),  .(game_id, team = Opp)]
game_teams      <- unique(rbindlist(list(teams_from_team, teams_from_opp)))
game_teams      <- game_teams[order(game_id, team)][, .SD[seq_len(min(.N, 2L))], by = game_id]

team_pairs <- merge(game_teams, game_teams, by = "game_id", allow.cartesian = TRUE, 
                    suffixes = c("","_other"))[team != team_other]
setnames(team_pairs, c("team","team_other"), c("team","other_team"))

nba_lineup <- merge(nba_lineup, team_pairs[, .(game_id, team, other_team)],
                    by.x = c("game_id","Team"), by.y = c("game_id","team"), all.x = TRUE)
nba_lineup[is.na(Opp) & !is.na(other_team), Opp := other_team]
nba_lineup[, other_team := NULL]

nba_lineup <- merge(nba_lineup, team_pairs[, .(game_id, team, other_team)],
                    by.x = c("game_id","Opp"), by.y = c("game_id","team"), all.x = TRUE, 
                    suffixes = c("","_teamfill"))
nba_lineup[is.na(Team) & !is.na(other_team), Team := other_team]
nba_lineup[, other_team := NULL]

message("Remaining NA Team: ", sum(is.na(nba_lineup$Team)))
message("Remaining NA Opp : ", sum(is.na(nba_lineup$Opp)))

# ------------------------------------------------------------
# 7B) ENFORCE CANONICAL COLUMN ORDER
# ------------------------------------------------------------
canonical_cols <- c(
  "NBA_Game","NBA_Date","game_id",
  "Period","Time_On","Time_Off",
  "Team_Long","Team","Opp","P_M",
  "P1","P2","P3","P4","P5"
)
nba_lineup <- nba_lineup[, ..canonical_cols]

# ------------------------------------------------------------
# 8) RECOVERY PASS: Low-coverage games retried as count-up
# ------------------------------------------------------------
nba_lineup[, stint_sec := mmss_to_sec(Time_On) - mmss_to_sec(Time_Off)]
game_coverage <- nba_lineup[, .(total_min = sum(stint_sec, na.rm = TRUE) / 60), by = .(game_id, Team)]
low_cov_teams <- game_coverage[total_min < 40]

if (nrow(low_cov_teams) > 0) {
  message("Recovery pass: ", nrow(low_cov_teams), " low-coverage game/team combos")
  
  rot_raw <- rbindlist(lapply(rot_files, function(fp) {
    dt <- fread(fp, fill = TRUE)
    dt[, GAME_ID := as.character(GAME_ID)]
    dt[, TEAM_ID := as.character(TEAM_ID)]
    dt[, PLAYER_FIRST := as.character(PLAYER_FIRST)]
    dt[, PLAYER_LAST := as.character(PLAYER_LAST)]
    dt[, IN_TIME_REAL := suppressWarnings(as.numeric(IN_TIME_REAL)) / 10]
    dt[, OUT_TIME_REAL := suppressWarnings(as.numeric(OUT_TIME_REAL)) / 10]
    dt
  }), fill = TRUE)
  rot_raw[, player_name := str_squish(paste(PLAYER_FIRST, PLAYER_LAST))]
  
  recovery_keys <- merge(low_cov_teams[, .(game_id, Team)],
                         game_meta[, .(GAME_ID, game_id)], by = "game_id", all.x = TRUE)
  recovery_keys <- merge(recovery_keys, 
                         unique(rot_dt[, .(GAME_ID, TEAM_ID, team_short_name)]),
                         by.x = c("GAME_ID", "Team"), by.y = c("GAME_ID", "team_short_name"), all.x = TRUE)
  
  recovered <- list()
  for (r in seq_len(nrow(recovery_keys))) {
    gid <- recovery_keys$GAME_ID[r]
    tid <- recovery_keys$TEAM_ID[r]
    if (is.na(gid) || is.na(tid)) next
    
    dt_raw <- rot_raw[GAME_ID == gid & TEAM_ID == tid & !is.na(IN_TIME_REAL) & !is.na(OUT_TIME_REAL)]
    if (nrow(dt_raw) == 0) next
    
    dt_raw[IN_TIME_REAL > OUT_TIME_REAL, 
           c("IN_TIME_REAL", "OUT_TIME_REAL") := .(OUT_TIME_REAL, IN_TIME_REAL)]
    
    all_times <- sort(unique(c(dt_raw$IN_TIME_REAL, dt_raw$OUT_TIME_REAL)))
    if (length(all_times) < 2) next
    game_max_raw <- max(all_times)
    
    for (idx in seq_len(length(all_times) - 1)) {
      seg_start <- all_times[idx]
      seg_end   <- all_times[idx + 1]
      if (seg_end <= seg_start) next
      mid <- (seg_start + seg_end) / 2
      on_court <- dt_raw[IN_TIME_REAL <= mid & OUT_TIME_REAL >= mid, unique(player_name)]
      if (length(on_court) == 5) {
        lp <- sort(on_court)
        recovered[[length(recovered) + 1]] <- data.table(
          GAME_ID = gid, TEAM_ID = tid,
          start_time = game_max_raw - seg_start, end_time = game_max_raw - seg_end,
          P1 = lp[1], P2 = lp[2], P3 = lp[3], P4 = lp[4], P5 = lp[5]
        )
      }
    }
  }
  
  if (length(recovered) > 0) {
    recovered_dt <- rbindlist(recovered, use.names = TRUE, fill = TRUE)
    message("Recovery produced ", nrow(recovered_dt), " additional rows")
    
    game_max_rec <- recovered_dt[, .(game_max = max(start_time)), by = GAME_ID]
    recovered_dt <- merge(recovered_dt, game_max_rec, by = "GAME_ID", all.x = TRUE)
    recovered_dt[, Period := fcase(
      start_time > (game_max - 720),  "Period 1",
      start_time > (game_max - 1440), "Period 2",
      start_time > (game_max - 2160), "Period 3",
      start_time > (game_max - 2880), "Period 4",
      start_time > (game_max - 3180), "Period 5",
      start_time >= 0,                 "Period 6"
    )]
    recovered_dt[, game_max := NULL]
    recovered_dt[, Time_On  := mapply(format_clock, start_time, Period)]
    recovered_dt[, Time_Off := mapply(format_clock, end_time,   Period)]
    recovered_dt[, ton := mmss_to_sec(Time_On)]
    recovered_dt[, toff := mmss_to_sec(Time_Off)]
    recovered_dt[toff > ton, Time_Off := "0:00"]
    recovered_dt[, c("ton", "toff") := NULL]
    
    recovered_dt <- merge(recovered_dt, game_meta[, .(GAME_ID, NBA_Game, NBA_Date, game_id)],
                          by = "GAME_ID", all.x = TRUE)
    team_meta_rec <- unique(rot_dt[, .(GAME_ID, TEAM_ID, Team = team_short_name, Team_Long = nbaapi_team_long_name)])
    recovered_dt <- merge(recovered_dt, team_meta_rec, by = c("GAME_ID", "TEAM_ID"), all.x = TRUE)
    recovered_dt <- merge(recovered_dt, opp_map, by = c("GAME_ID", "TEAM_ID"), all.x = TRUE)
    
    rec_lineup <- recovered_dt[, .(
      NBA_Game = as.character(NBA_Game), NBA_Date = as.character(NBA_Date),
      game_id = as.character(game_id), Period = as.character(Period),
      Time_On = as.character(Time_On), Time_Off = as.character(Time_Off),
      Team_Long = as.character(Team_Long), Team = nba_fix(as.character(Team)),
      Opp = nba_fix(as.character(Opp)), P_M = NA_character_,
      P1 = as.character(P1), P2 = as.character(P2), P3 = as.character(P3),
      P4 = as.character(P4), P5 = as.character(P5)
    )]
    rec_lineup <- unique(rec_lineup[!is.na(game_id) & !is.na(Team) & !is.na(P1)])
    
    recovered_game_teams <- unique(rec_lineup[, .(game_id, Team)])
    nba_lineup <- nba_lineup[!recovered_game_teams, on = .(game_id, Team)]
    nba_lineup <- rbindlist(list(nba_lineup, rec_lineup), use.names = TRUE, fill = TRUE)
    nba_lineup <- nba_lineup[, ..canonical_cols]
    message("After recovery: ", nrow(nba_lineup), " total rows")
  } else {
    message("Recovery pass produced no additional rows")
  }
}
nba_lineup[, stint_sec := NULL]

# ============================================================
# 9) ROW-LEVEL QUALITY FLAGS
# ============================================================
message("\n=== ROW-LEVEL QUALITY VALIDATION ===")

nba_lineup[, ton_sec  := mmss_to_sec(Time_On)]
nba_lineup[, toff_sec := mmss_to_sec(Time_Off)]

player_cols <- c("P1","P2","P3","P4","P5")
nba_lineup[, n_uniq := apply(.SD, 1, function(x) length(unique(na.omit(x)))), .SDcols = player_cols]
nba_lineup[, time_valid   := (ton_sec >= toff_sec) & !is.na(ton_sec) & !is.na(toff_sec)]
nba_lineup[, team_valid   := Team %in% VALID_TEAMS & Opp %in% VALID_TEAMS]
nba_lineup[, format_valid := grepl("^\\d+:\\d{2}$", Time_On) & grepl("^\\d+:\\d{2}$", Time_Off)]

nba_lineup[, quality_flag := fcase(
  n_uniq != 5,   "BAD_PLAYER_COUNT",
  !time_valid,   "TIME_INVERTED",
  !team_valid,   "BAD_TEAM_CODE",
  !format_valid, "BAD_TIME_FORMAT",
  default = "PASS"
)]

n_pass <- sum(nba_lineup$quality_flag == "PASS")
n_fail_rows <- sum(nba_lineup$quality_flag != "PASS")
message("Row quality: ", n_pass, " PASS | ", n_fail_rows, " FAIL")
if (n_fail_rows > 0) {
  message("Failures by type:")
  print(table(nba_lineup$quality_flag[nba_lineup$quality_flag != "PASS"]))
}

nba_lineup[, c("ton_sec", "toff_sec", "n_uniq", "time_valid", "team_valid", "format_valid") := NULL]

# ============================================================
# 10) GAME-LEVEL QUALITY FLAGS
# ============================================================
message("\n=== GAME-LEVEL QUALITY VALIDATION ===")

game_quality <- nba_lineup[quality_flag == "PASS", {
  periods <- unique(Period)
  has_4_periods <- all(paste0("Period ", 1:4) %in% periods)
  
  period_start_ok <- TRUE
  period_end_ok <- TRUE
  
  for (per in paste0("Period ", 1:4)) {
    per_rows <- .SD[Period == per]
    if (nrow(per_rows) > 0) {
      max_on  <- max(mmss_to_sec(per_rows$Time_On), na.rm = TRUE)
      min_off <- min(mmss_to_sec(per_rows$Time_Off), na.rm = TRUE)
      if (max_on != 720) period_start_ok <- FALSE
      if (min_off != 0) period_end_ok <- FALSE
    }
  }
  
  for (per in paste0("Period ", 5:6)) {
    per_rows <- .SD[Period == per]
    if (nrow(per_rows) > 0) {
      max_on  <- max(mmss_to_sec(per_rows$Time_On), na.rm = TRUE)
      min_off <- min(mmss_to_sec(per_rows$Time_Off), na.rm = TRUE)
      if (max_on != 300) period_start_ok <- FALSE
      if (min_off != 0) period_end_ok <- FALSE
    }
  }
  
  .(has_4_periods = has_4_periods,
    period_start_ok = period_start_ok,
    period_end_ok = period_end_ok,
    game_quality = fifelse(has_4_periods & period_start_ok & period_end_ok, "PASS", "NEEDS_PBP"))
}, by = .(game_id, Team)]

nba_lineup <- merge(nba_lineup, game_quality[, .(game_id, Team, game_quality)],
                    by = c("game_id", "Team"), all.x = TRUE)
nba_lineup[is.na(game_quality), game_quality := "NEEDS_PBP"]

n_gq_pass <- nrow(game_quality[game_quality == "PASS"])
n_gq_fail <- nrow(game_quality[game_quality != "PASS"])
message("Game/team quality: ", n_gq_pass, " PASS | ", n_gq_fail, " NEEDS_PBP")

if (n_gq_fail > 0) {
  message("\nSample games needing PBP replacement:")
  sample_bad <- game_quality[game_quality != "PASS"][1:min(10, n_gq_fail)]
  for (i in seq_len(nrow(sample_bad))) {
    issues <- c()
    if (!sample_bad$has_4_periods[i]) issues <- c(issues, "missing_periods")
    if (!sample_bad$period_start_ok[i]) issues <- c(issues, "bad_period_start")
    if (!sample_bad$period_end_ok[i]) issues <- c(issues, "bad_period_end")
    message("  ", sample_bad$game_id[i], " ", sample_bad$Team[i], " — ", paste(issues, collapse = ", "))
  }
}

# ------------------------------------------------------------
# 11) SAVE OUTPUT
# ------------------------------------------------------------
fwrite(nba_lineup, nba_lineup_path)
message("\n\u2705 Saved: ", nba_lineup_path)
message("Total rows: ", nrow(nba_lineup))
message("Rows PASS: ", sum(nba_lineup$quality_flag == "PASS"))
message("Game/team PASS: ", n_gq_pass, " / ", n_gq_pass + n_gq_fail)

# ------------------------------------------------------------
# 12) CLEANUP
# ------------------------------------------------------------
gc()
message("\n\u2705 NBA Lineup Builder (V5) complete.")