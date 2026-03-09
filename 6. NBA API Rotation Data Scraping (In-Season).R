# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== V4: ROTATIONS → MASTER → PM_LINEUP APPEND (TRUE LINEUPS) ====
#     - combines rotations_*.csv into rotations_master_<season>.csv
#     - rebuilds TRUE 5-man lineup segments from IN/OUT events
#     - IN_TIME_REAL / OUT_TIME_REAL are COUNTDOWN (high = start, low = end)
#     - reformats to pm_lineup_data_<season>.csv schema
#     - appends only NEW lineup rows (no overwrite)
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿

rm(list = setdiff(ls(), c(
  "current_date","formatted_date","formatted_year",
  "next_year_date","pbp_season","season_token","season_token2",
  "base_path","logo_dir","essential_names",
  "scripts","batches","run_script","run_batch"
)))

library(data.table)
library(stringr)
library(lubridate)

# ------------------------------------------------------------
# 0) PATHS
# ------------------------------------------------------------
pm_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine"

pm_lineup_path  <- file.path(pm_dir, paste0("pm_lineup_data_", season_token, ".csv"))
rot_master_path <- file.path(pm_dir, paste0("rotations_master_", season_token, ".csv"))

team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"

# ------------------------------------------------------------
# 1) LOAD EXISTING PM LINEUP (APPEND-ONLY SAFETY)
# ------------------------------------------------------------
if (file.exists(pm_lineup_path) && file.info(pm_lineup_path)$size > 0) {
  pm_lineup_prev <- fread(pm_lineup_path, colClasses = "character")
} else {
  pm_lineup_prev <- data.table()
}

existing_key <- character(0)
if (nrow(pm_lineup_prev) > 0) {
  existing_key <- unique(paste(
    pm_lineup_prev$game_id,
    pm_lineup_prev$Team,
    pm_lineup_prev$Period,
    pm_lineup_prev$Time_On,
    pm_lineup_prev$Time_Off,
    pm_lineup_prev$P1,
    pm_lineup_prev$P2,
    pm_lineup_prev$P3,
    pm_lineup_prev$P4,
    pm_lineup_prev$P5,
    sep="|"
  ))
}

# ------------------------------------------------------------
# 2) COMBINE ROTATIONS FILES -> rotations_master_<season>.csv
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

# normalize
# NOTE: IN_TIME_REAL and OUT_TIME_REAL are in tenths of a second, COUNTDOWN format
# IN_TIME_REAL  = when player enters (higher value = earlier in game)
# OUT_TIME_REAL = when player exits  (lower value  = later in game)
rot_master[, GAME_ID       := as.character(GAME_ID)]
rot_master[, TEAM_ID       := as.character(TEAM_ID)]
rot_master[, home_away     := tolower(as.character(home_away))]
rot_master[, PLAYER_FIRST  := as.character(PLAYER_FIRST)]
rot_master[, PLAYER_LAST   := as.character(PLAYER_LAST)]
rot_master[, IN_TIME_REAL  := suppressWarnings(as.numeric(IN_TIME_REAL))  / 10]
rot_master[, OUT_TIME_REAL := suppressWarnings(as.numeric(OUT_TIME_REAL)) / 10]

# Per-row normalization: ensure IN_TIME_REAL > OUT_TIME_REAL (countdown format)
# Some source files have columns already in correct orientation, others are swapped.
# Instead of a blanket column swap, fix row-by-row.
needs_swap <- rot_master[IN_TIME_REAL < OUT_TIME_REAL]
if (nrow(needs_swap) > 0) {
  message("Normalizing ", nrow(needs_swap), " rows with IN < OUT (swapping to countdown format)")
  rot_master[IN_TIME_REAL < OUT_TIME_REAL,
             c("IN_TIME_REAL", "OUT_TIME_REAL") := .(OUT_TIME_REAL, IN_TIME_REAL)]
}


# write master
fwrite(rot_master, rot_master_path)
message("Saved rotations master: ", rot_master_path)

# working copy
rot_dt <- copy(rot_master)


# ------------------------------------------------------------
# 3) ATTACH TEAM METADATA
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

# ------------------------------------------------------------
# 4) DATE -> PopMac_Date
# ------------------------------------------------------------
rot_dt[, game_date := trimws(as.character(game_date))]
rot_dt[, game_date_parsed := as.Date(game_date, format="%Y%m%d")]
rot_dt[is.na(game_date_parsed), game_date_parsed := as.Date(game_date, format="%Y-%m-%d")]
rot_dt[is.na(game_date_parsed), game_date_parsed := suppressWarnings(mdy(game_date))]
rot_dt[, PopMac_Date := format(game_date_parsed, "%Y%m%d")]

# ------------------------------------------------------------
# 5) BUILD PopMac_Game + Opp Map
# ------------------------------------------------------------
pm_fix <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "NOP", "NOR", x)
  x <- ifelse(x == "PHX", "PHO", x)
  x <- ifelse(x == "UTA", "UTH", x)
  x
}

game_popmac <- unique(rot_dt[, .(GAME_ID, home_away, team_short_name, PopMac_Date)])
game_popmac <- game_popmac[, .(
  PopMac_Date = PopMac_Date[1],
  away_abbrev = team_short_name[home_away == "away"][1],
  home_abbrev = team_short_name[home_away == "home"][1]
), by = GAME_ID]
game_popmac[, PopMac_Game := paste0(pm_fix(away_abbrev), pm_fix(home_abbrev))]
game_popmac[, game_id := paste0(PopMac_Game, "_", PopMac_Date)]

team_per_game <- unique(rot_dt[, .(GAME_ID, TEAM_ID, team_short_name)])
opp_map <- team_per_game[
  team_per_game, on = .(GAME_ID), allow.cartesian = TRUE
][TEAM_ID != i.TEAM_ID, .(GAME_ID, TEAM_ID, Opp = i.team_short_name)]

# ------------------------------------------------------------
# 6) TRUE LINEUP ENGINE
# Data is COUNTDOWN: IN_TIME_REAL > OUT_TIME_REAL
# Player is ON court when clock is between IN_TIME and OUT_TIME
# Sort events descending (high -> low) to process game forward
# ------------------------------------------------------------

rot_dt[, player_name := str_squish(paste(PLAYER_FIRST, PLAYER_LAST))]

build_true_lineups <- function(dt_game_team) {
  dt_game_team <- dt_game_team[!is.na(IN_TIME_REAL) & !is.na(OUT_TIME_REAL)]
  if (nrow(dt_game_team) == 0) return(data.table())
  
  # IN = player enters at high clock value, OUT = player exits at low clock value
  ev_in  <- dt_game_team[, .(time = IN_TIME_REAL,  player = player_name, type = "IN")]
  ev_out <- dt_game_team[, .(time = OUT_TIME_REAL, player = player_name, type = "OUT")]
  events <- rbindlist(list(ev_in, ev_out), use.names = TRUE, fill = TRUE)
  
  # Sort DESCENDING (high clock = start of game first)
  # At same timestamp: IN before OUT (player enters before another exits)
  events[type == "IN",  type_ord := 0L]
  events[type == "OUT", type_ord := 1L]
  setorder(events, -time, type_ord)
  events[, type_ord := NULL]
  
  active <- character(0)
  out    <- list()
  
  times           <- sort(unique(events$time), decreasing = TRUE)
  events_by_time  <- split(events, by = "time", keep.by = FALSE)
  
  for (idx in seq_along(times)) {
    t      <- times[idx]
    next_t <- if (idx < length(times)) times[idx + 1] else NA_real_
    
    ev <- events_by_time[[as.character(t)]]
    
    for (j in seq_len(nrow(ev))) {
      if (ev$type[j] == "IN") {
        active <- unique(c(active, ev$player[j]))
      } else {
        active <- setdiff(active, ev$player[j])
      }
    }
    
    # next_t is lower (later in game), segment runs from t down to next_t
    if (!is.na(next_t) && next_t < t && length(active) == 5) {
      lp <- sort(active)
      out[[length(out) + 1]] <- data.table(
        start_time = t,      # higher clock value = earlier in game = Time_On
        end_time   = next_t, # lower clock value  = later in game   = Time_Off
        P1 = lp[1], P2 = lp[2], P3 = lp[3], P4 = lp[4], P5 = lp[5]
      )
    }
  }
  
  if (length(out) == 0) return(data.table())
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

lineup_dt <- rot_dt[
  , build_true_lineups(.SD),
  by = .(GAME_ID, TEAM_ID)
]

if (nrow(lineup_dt) == 0) {
  message("No true lineups produced. Check IN/OUT timing.")
  quit(save = "no")
}

# ------------------------------------------------------------
# 7) DERIVE PERIOD + CLOCK FROM COUNTDOWN VALUES
# Regulation: 3600 -> 0 (4 x 720 seconds per quarter)
# OT resets to 300 and counts down for each OT period
# We derive period from start_time (Time_On = higher clock value)
# ------------------------------------------------------------
# Derive period boundaries dynamically based on game max (handles OT games)
game_max <- rot_dt[, .(game_max = max(IN_TIME_REAL)), by = GAME_ID]
lineup_dt <- merge(lineup_dt, game_max, by = "GAME_ID", all.x = TRUE)

lineup_dt[, Period := fcase(
  start_time > (game_max - 720),  "Period 1",
  start_time > (game_max - 1440), "Period 2",
  start_time > (game_max - 2160), "Period 3",
  start_time > (game_max - 2880), "Period 4",
  start_time > (game_max - 3180), "Period 5",
  start_time >= 0,                 "Period 6"
)]

lineup_dt[, game_max := NULL]
format_clock <- function(sec, period) {
  sec <- as.numeric(sec)
  quarter_max <- ifelse(grepl("Period [56]", period), 300, 720)
  remaining <- sec %% quarter_max
  if (remaining == 0) remaining <- quarter_max
  sprintf("%d:%02d", as.integer(floor(remaining / 60)), as.integer(remaining %% 60))
}

lineup_dt[, Time_On  := mapply(format_clock, start_time, Period)]
lineup_dt[, Time_Off := mapply(format_clock, end_time,   Period)]

# Fix Time_Off > Time_On at quarter boundaries
mmss_to_sec <- function(x) {
  parts <- strsplit(x, ":")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

lineup_dt[, time_on_num  := vapply(Time_On,  mmss_to_sec, numeric(1))]
lineup_dt[, time_off_num := vapply(Time_Off, mmss_to_sec, numeric(1))]

lineup_dt[
  time_off_num > time_on_num,
  Time_Off := "0:00"
]

lineup_dt[, c("time_on_num", "time_off_num") := NULL]

# Force first lineup of OT periods to start at 5:00
lineup_dt[, time_on_num := vapply(Time_On, mmss_to_sec, numeric(1))]
lineup_dt[, max_time_on := max(time_on_num), by = .(GAME_ID, TEAM_ID, Period)]

lineup_dt[
  Period %in% c("Period 5", "Period 6") & time_on_num == max_time_on & max_time_on < 300,
  Time_On := "5:00"
]

lineup_dt[, c("time_on_num", "max_time_on") := NULL]

# ------------------------------------------------------------
# 8) ATTACH PopMac + Team/Opp + Build final schema
# ------------------------------------------------------------
lineup_dt <- merge(
  lineup_dt,
  game_popmac[, .(GAME_ID, PopMac_Game, PopMac_Date, game_id)],
  by = "GAME_ID", all.x = TRUE
)

team_meta <- unique(rot_dt[, .(GAME_ID, TEAM_ID, Team = team_short_name, Team_Long = nbaapi_team_long_name)])
lineup_dt <- merge(lineup_dt, team_meta, by = c("GAME_ID","TEAM_ID"), all.x = TRUE)
lineup_dt <- merge(lineup_dt, opp_map,   by = c("GAME_ID","TEAM_ID"), all.x = TRUE)

pm_lineup_new <- lineup_dt[, .(
  PopMac_Game = as.character(PopMac_Game),
  PopMac_Date = as.character(PopMac_Date),
  game_id     = as.character(game_id),
  Period      = as.character(Period),
  Time_On     = as.character(Time_On),
  Time_Off    = as.character(Time_Off),
  Team_Long   = as.character(Team_Long),
  Team        = as.character(Team),
  Opp         = as.character(Opp),
  P_M         = NA_character_,
  P1 = as.character(P1),
  P2 = as.character(P2),
  P3 = as.character(P3),
  P4 = as.character(P4),
  P5 = as.character(P5)
)]

pm_lineup_new <- unique(pm_lineup_new[
  !is.na(game_id) & !is.na(Team) & !is.na(P1) & !is.na(Time_On) & !is.na(Time_Off)
])

# ------------------------------------------------------------
# 8A) NORMALIZE TEAM / OPP TO POPCORN MACHINE CODES
# ------------------------------------------------------------
pm_lineup_new[, Team := case_when(
  Team == "NOP" ~ "NOR",
  Team == "SA"  ~ "SAS",
  Team == "PHX" ~ "PHO",
  Team == "UTA" ~ "UTH",
  TRUE ~ Team
)]

pm_lineup_new[, Opp := case_when(
  Opp == "NOP" ~ "NOR",
  Opp == "SA"  ~ "SAS",
  Opp == "PHX" ~ "PHO",
  Opp == "UTA" ~ "UTH",
  TRUE ~ Opp
)]

# ------------------------------------------------------------
# 8B) REPAIR MISSING TEAM / OPP
# ------------------------------------------------------------
pm_lineup_new[, Team := trimws(as.character(Team))]
pm_lineup_new[, Opp  := trimws(as.character(Opp))]
pm_lineup_new[Team == "", Team := NA_character_]
pm_lineup_new[Opp  == "", Opp  := NA_character_]

teams_from_team <- pm_lineup_new[!is.na(Team), .(game_id, team = Team)]
teams_from_opp  <- pm_lineup_new[!is.na(Opp),  .(game_id, team = Opp)]
game_teams      <- unique(rbindlist(list(teams_from_team, teams_from_opp)))
game_teams      <- game_teams[order(game_id, team)][, .SD[seq_len(min(.N, 2L))], by = game_id]

team_pairs <- merge(game_teams, game_teams, by = "game_id", allow.cartesian = TRUE, suffixes = c("","_other"))[team != team_other]
setnames(team_pairs, c("team","team_other"), c("team","other_team"))

pm_lineup_new <- merge(pm_lineup_new, team_pairs[, .(game_id, team, other_team)],
                       by.x = c("game_id","Team"), by.y = c("game_id","team"), all.x = TRUE)
pm_lineup_new[is.na(Opp) & !is.na(other_team), Opp := other_team]
pm_lineup_new[, other_team := NULL]

pm_lineup_new <- merge(pm_lineup_new, team_pairs[, .(game_id, team, other_team)],
                       by.x = c("game_id","Opp"), by.y = c("game_id","team"), all.x = TRUE, suffixes = c("","_teamfill"))
pm_lineup_new[is.na(Team) & !is.na(other_team), Team := other_team]
pm_lineup_new[, other_team := NULL]

message("Remaining NA Team: ", sum(is.na(pm_lineup_new$Team)))
message("Remaining NA Opp : ", sum(is.na(pm_lineup_new$Opp)))

# ------------------------------------------------------------
# 8C) ENFORCE CANONICAL COLUMN ORDER
# ------------------------------------------------------------
canonical_cols <- c(
  "PopMac_Game","PopMac_Date","game_id",
  "Period","Time_On","Time_Off",
  "Team_Long","Team","Opp","P_M",
  "P1","P2","P3","P4","P5"
)

pm_lineup_new <- pm_lineup_new[, ..canonical_cols]

# ------------------------------------------------------------
# 9) APPEND ONLY NEW ROWS
# ------------------------------------------------------------
pm_lineup_new[, row_key := paste(game_id, Team, Period, Time_On, Time_Off, P1, P2, P3, P4, P5, sep="|")]
pm_lineup_new_to_append <- pm_lineup_new[!(row_key %in% existing_key)]
pm_lineup_new_to_append[, row_key := NULL]
pm_lineup_new_to_append <- pm_lineup_new_to_append[, ..canonical_cols]

message("New lineup rows to append: ", nrow(pm_lineup_new_to_append))

if (nrow(pm_lineup_new_to_append) > 0) {
  fwrite(
    pm_lineup_new_to_append,
    pm_lineup_path,
    append    = file.exists(pm_lineup_path),
    col.names = !file.exists(pm_lineup_path)
  )
  message("Appended lineup rows to: ", pm_lineup_path)
} else {
  message("No new lineup rows found to append.")
}


# 💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊
# 💊💊💊 START: V4 Lineup Data Cleanse & Diagnostic Layer
# 💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊

# ===========================================================================
# This cleanse validates pm_lineup_new (the newly built lineup rows)
# and pm_lineup_prev (the existing file) after V4 processing.
# Insert this AFTER Section 9 (append) and BEFORE Section 10 (cleanup).
#
# Reads:
#   pm_lineup_new           — newly built lineup rows (pre-append filter)
#   pm_lineup_new_to_append — rows actually being appended
#   pm_lineup_prev          — previously existing lineup file
#   rot_master              — raw rotations master (for cross-checks)
#
# Uses the same logging framework + Shiny popup as AURELIUS cleanse.
# ===========================================================================

suppressWarnings({
  library(dplyr)
  library(data.table)
})

# --------------------------------------------------
# Logging infrastructure
# --------------------------------------------------
cleanse_log <- data.frame(
  section  = character(),
  check    = character(),
  level    = character(),
  detail   = character(),
  stringsAsFactors = FALSE
)

log_check <- function(section, check, level, detail) {
  cleanse_log <<- rbind(cleanse_log, data.frame(
    section = section,
    check   = check,
    level   = level,
    detail  = as.character(detail),
    stringsAsFactors = FALSE
  ))
  sym <- switch(level, INFO = "\u2139\uFE0F ", WARN = "\u26A0\uFE0F ", FAIL = "\u274C ")
  cat(sym, "[", section, "]", check, "\u2014", detail, "\n")
}

na_rate <- function(x) mean(is.na(x))
fmt_pct <- function(x) paste0(round(x * 100, 1), "%")

cat("\n========================================================\n")
cat("  V4 LINEUP DATA CLEANSE\n")
cat("========================================================\n\n")

# --------------------------------------------------
# Determine which dataset to validate
# --------------------------------------------------
# If we just appended new rows, validate those.
# Also validate the full file (prev + new) for global checks.
if (exists("pm_lineup_new") && nrow(pm_lineup_new) > 0) {
  lineup_check <- copy(pm_lineup_new)
} else {
  cat("No pm_lineup_new found. Skipping cleanse.\n")
  return(invisible(NULL))
}

# Full file = prev + new (for global coverage checks)
if (exists("pm_lineup_prev") && nrow(pm_lineup_prev) > 0) {
  lineup_full <- rbindlist(list(pm_lineup_prev, pm_lineup_new), fill = TRUE, use.names = TRUE)
} else {
  lineup_full <- copy(lineup_check)
}

# ==========================================================================================================
# SECTION 1: STRUCTURAL INTEGRITY
# ==========================================================================================================
cat("\n--- SECTION 1: STRUCTURAL INTEGRITY ---\n\n")

# --------------------------------------------------
# 1A: Row counts
# --------------------------------------------------
n_new    <- nrow(lineup_check)
n_append <- if (exists("pm_lineup_new_to_append")) nrow(pm_lineup_new_to_append) else NA
n_prev   <- if (exists("pm_lineup_prev")) nrow(pm_lineup_prev) else 0
n_full   <- nrow(lineup_full)

log_check("S1", "Row counts", "INFO",
          paste0("new_built=", n_new,
                 " | to_append=", ifelse(is.na(n_append), "N/A", n_append),
                 " | prev_file=", n_prev,
                 " | full=", n_full))

# --------------------------------------------------
# 1B: Column completeness — canonical columns must exist
# --------------------------------------------------
canonical_cols <- c(
  "PopMac_Game","PopMac_Date","game_id",
  "Period","Time_On","Time_Off",
  "Team_Long","Team","Opp","P_M",
  "P1","P2","P3","P4","P5"
)

missing_cols <- setdiff(canonical_cols, names(lineup_check))
if (length(missing_cols) > 0) {
  log_check("S1", "Column completeness", "FAIL",
            paste0("Missing columns: ", paste(missing_cols, collapse = ", ")))
} else {
  log_check("S1", "Column completeness", "INFO",
            paste0("All ", length(canonical_cols), " canonical columns present \u2713"))
}

# --------------------------------------------------
# 1C: Critical field NA checks — zero tolerance
# --------------------------------------------------
critical_fields <- c("game_id", "Team", "Opp", "Period", "Time_On", "Time_Off",
                     "P1", "P2", "P3", "P4", "P5")

for (col in critical_fields) {
  if (!col %in% names(lineup_check)) next
  n_na <- sum(is.na(lineup_check[[col]]) | trimws(as.character(lineup_check[[col]])) == "")
  if (n_na > 0) {
    log_check("S1", paste0("NA check: ", col), "FAIL",
              paste0(n_na, " rows with NA/blank (", fmt_pct(n_na / n_new), ")"))
  } else {
    log_check("S1", paste0("NA check: ", col), "INFO", "0 NA/blank \u2713")
  }
}

# --------------------------------------------------
# 1D: Date range
# --------------------------------------------------
dates <- suppressWarnings(as.Date(as.character(lineup_check$PopMac_Date), format = "%Y%m%d"))
if (sum(!is.na(dates)) > 0) {
  log_check("S1", "Date range (new rows)", "INFO",
            paste0(min(dates, na.rm = TRUE), " to ", max(dates, na.rm = TRUE),
                   " | ", length(unique(dates)), " unique dates"))
} else {
  log_check("S1", "Date range", "WARN", "Could not parse any dates from PopMac_Date")
}

# --------------------------------------------------
# 1E: Game count
# --------------------------------------------------
n_games <- length(unique(lineup_check$game_id))
log_check("S1", "Game count (new rows)", "INFO", paste0(n_games, " unique games"))

# --------------------------------------------------
# 1F: Duplicate row check
# --------------------------------------------------
dup_count <- nrow(lineup_check) - nrow(unique(lineup_check[, .(game_id, Team, Period, Time_On, Time_Off, P1, P2, P3, P4, P5)]))
if (dup_count > 0) {
  log_check("S1", "Duplicate rows", "WARN",
            paste0(dup_count, " duplicate lineup rows detected"))
} else {
  log_check("S1", "Duplicate rows", "INFO", "No duplicates \u2713")
}


# ==========================================================================================================
# SECTION 2: LINEUP VALIDITY
# ==========================================================================================================
cat("\n--- SECTION 2: LINEUP VALIDITY ---\n\n")

# --------------------------------------------------
# 2A: Each row must have 5 unique players
# --------------------------------------------------
player_cols <- c("P1","P2","P3","P4","P5")

lineup_check[, n_unique_players := apply(.SD, 1, function(x) length(unique(na.omit(x)))), .SDcols = player_cols]

n_not5 <- sum(lineup_check$n_unique_players != 5)
if (n_not5 > 0) {
  log_check("S2", "5 unique players per row", "FAIL",
            paste0(n_not5, " rows do NOT have exactly 5 unique players"))
  # Show a sample
  bad_rows <- lineup_check[n_unique_players != 5][1:min(3, n_not5)]
  for (i in seq_len(nrow(bad_rows))) {
    log_check("S2", "  Bad lineup sample", "FAIL",
              paste0(bad_rows$game_id[i], " ", bad_rows$Team[i], " ", bad_rows$Period[i],
                     " — players: ", paste(bad_rows[i, ..player_cols], collapse=", "),
                     " (unique=", bad_rows$n_unique_players[i], ")"))
  }
} else {
  log_check("S2", "5 unique players per row", "INFO",
            paste0("All ", n_new, " rows have exactly 5 unique players \u2713"))
}

lineup_check[, n_unique_players := NULL]

# --------------------------------------------------
# 2B: No blank player names
# --------------------------------------------------
n_blank_players <- sum(apply(lineup_check[, ..player_cols], 1, function(x) any(trimws(x) == "" | is.na(x))))
if (n_blank_players > 0) {
  log_check("S2", "Blank player names", "FAIL",
            paste0(n_blank_players, " rows contain blank/NA player names"))
} else {
  log_check("S2", "Blank player names", "INFO", "No blank player names \u2713")
}

# --------------------------------------------------
# 2C: Player does not appear on both teams in the same game
# --------------------------------------------------
players_by_game_team <- lineup_check[, {
  all_players <- unique(c(P1, P2, P3, P4, P5))
  data.table(player = all_players)
}, by = .(game_id, Team)]

cross_team <- players_by_game_team[
  players_by_game_team,
  on = .(game_id, player),
  allow.cartesian = TRUE,
  nomatch = 0
][Team != i.Team]

if (nrow(cross_team) > 0) {
  n_crossover <- length(unique(cross_team$player))
  log_check("S2", "Cross-team player check", "FAIL",
            paste0(n_crossover, " players appear on BOTH teams in the same game"))
  sample_cross <- head(unique(cross_team[, .(game_id, player)]), 5)
  for (i in seq_len(nrow(sample_cross))) {
    log_check("S2", "  Cross-team sample", "FAIL",
              paste0(sample_cross$player[i], " in ", sample_cross$game_id[i]))
  }
} else {
  log_check("S2", "Cross-team player check", "INFO",
            "No players appear on both sides of a game \u2713")
}

# --------------------------------------------------
# 2D: Player count per game/team (roster sanity)
# --------------------------------------------------
players_per_game <- players_by_game_team[, .(n_players = uniqueN(player)), by = .(game_id, Team)]

n_too_few  <- sum(players_per_game$n_players < 5)
n_too_many <- sum(players_per_game$n_players > 15)
mean_roster <- round(mean(players_per_game$n_players), 1)

if (n_too_few > 0) {
  log_check("S2", "Roster size per game", "WARN",
            paste0(n_too_few, " game/team combos with < 5 unique players"))
} else if (n_too_many > 0) {
  log_check("S2", "Roster size per game", "WARN",
            paste0(n_too_many, " game/team combos with > 15 unique players | mean=", mean_roster))
} else {
  log_check("S2", "Roster size per game", "INFO",
            paste0("Mean=", mean_roster, " unique players per game/team (expected 8-13) \u2713"))
}


# ==========================================================================================================
# SECTION 3: TIMING LOGIC
# ==========================================================================================================
cat("\n--- SECTION 3: TIMING LOGIC ---\n\n")

# Helper: convert mm:ss to seconds
mmss_to_sec <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) {
    if (length(p) != 2) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2])
  }, numeric(1))
}

lineup_check[, time_on_sec  := mmss_to_sec(Time_On)]
lineup_check[, time_off_sec := mmss_to_sec(Time_Off)]

# --------------------------------------------------
# 3A: Time_On must be >= Time_Off (countdown format)
# --------------------------------------------------
n_inverted <- sum(lineup_check$time_on_sec < lineup_check$time_off_sec, na.rm = TRUE)
if (n_inverted > 0) {
  log_check("S3", "Time_On >= Time_Off", "FAIL",
            paste0(n_inverted, " rows where Time_On < Time_Off (clock running wrong direction)"))
  bad_time <- lineup_check[time_on_sec < time_off_sec][1:min(3, n_inverted)]
  for (i in seq_len(nrow(bad_time))) {
    log_check("S3", "  Inverted sample", "FAIL",
              paste0(bad_time$game_id[i], " ", bad_time$Team[i], " ", bad_time$Period[i],
                     " — On=", bad_time$Time_On[i], " Off=", bad_time$Time_Off[i]))
  }
} else {
  log_check("S3", "Time_On >= Time_Off", "INFO",
            paste0("All ", n_new, " rows have valid countdown ordering \u2713"))
}

# --------------------------------------------------
# 3B: No negative times
# --------------------------------------------------
n_neg <- sum(lineup_check$time_on_sec < 0 | lineup_check$time_off_sec < 0, na.rm = TRUE)
if (n_neg > 0) {
  log_check("S3", "Negative times", "FAIL", paste0(n_neg, " rows with negative time values"))
} else {
  log_check("S3", "Negative times", "INFO", "No negative time values \u2713")
}

# --------------------------------------------------
# 3C: Time bounds — regulation max 12:00, OT max 5:00
# --------------------------------------------------
lineup_check[, is_ot := grepl("Period [56]", Period)]

n_reg_over12 <- sum(!lineup_check$is_ot & lineup_check$time_on_sec > 720, na.rm = TRUE)
n_ot_over5   <- sum(lineup_check$is_ot  & lineup_check$time_on_sec > 300, na.rm = TRUE)

if (n_reg_over12 > 0) {
  log_check("S3", "Regulation time > 12:00", "WARN",
            paste0(n_reg_over12, " regulation rows with Time_On > 12:00"))
} else {
  log_check("S3", "Regulation time bounds", "INFO", "All regulation Time_On <= 12:00 \u2713")
}

if (n_ot_over5 > 0) {
  log_check("S3", "OT time > 5:00", "WARN",
            paste0(n_ot_over5, " OT rows with Time_On > 5:00"))
} else {
  log_check("S3", "OT time bounds", "INFO", "All OT Time_On <= 5:00 \u2713")
}

# --------------------------------------------------
# 3D: Period coverage — every game/team should have Q1-Q4
# --------------------------------------------------
period_coverage <- lineup_check[, .(
  has_Q1 = any(Period == "Period 1"),
  has_Q2 = any(Period == "Period 2"),
  has_Q3 = any(Period == "Period 3"),
  has_Q4 = any(Period == "Period 4")
), by = .(game_id, Team)]

n_missing_q <- sum(!period_coverage$has_Q1 | !period_coverage$has_Q2 |
                     !period_coverage$has_Q3 | !period_coverage$has_Q4)

if (n_missing_q > 0) {
  log_check("S3", "Period coverage (Q1-Q4)", "WARN",
            paste0(n_missing_q, " game/team combos missing at least one regulation quarter"))
  missing_examples <- period_coverage[!has_Q1 | !has_Q2 | !has_Q3 | !has_Q4][1:min(3, n_missing_q)]
  for (i in seq_len(nrow(missing_examples))) {
    missing_qs <- c()
    if (!missing_examples$has_Q1[i]) missing_qs <- c(missing_qs, "Q1")
    if (!missing_examples$has_Q2[i]) missing_qs <- c(missing_qs, "Q2")
    if (!missing_examples$has_Q3[i]) missing_qs <- c(missing_qs, "Q3")
    if (!missing_examples$has_Q4[i]) missing_qs <- c(missing_qs, "Q4")
    log_check("S3", "  Missing period sample", "WARN",
              paste0(missing_examples$game_id[i], " ", missing_examples$Team[i],
                     " — missing: ", paste(missing_qs, collapse = ", ")))
  }
} else {
  log_check("S3", "Period coverage (Q1-Q4)", "INFO",
            paste0("All ", nrow(period_coverage), " game/team combos have Q1-Q4 \u2713"))
}

# --------------------------------------------------
# 3E: Total lineup time per game/team (~48 min = 2880 sec for regulation)
# --------------------------------------------------
lineup_check[, stint_duration := time_on_sec - time_off_sec]

game_total_time <- lineup_check[, .(
  total_sec = sum(stint_duration, na.rm = TRUE)
), by = .(game_id, Team)]

game_total_time[, total_min := round(total_sec / 60, 1)]

# Expected: 5 players on court at all times = 5 * 48 = 240 player-minutes
# Each lineup row covers 5 players, so total stint time should be ~48 min
mean_total <- round(mean(game_total_time$total_min), 1)
n_low  <- sum(game_total_time$total_min < 40)
n_high <- sum(game_total_time$total_min > 58)  # 53 min allows for OT

if (n_low > 0) {
  log_check("S3", "Total lineup time (low)", "WARN",
            paste0(n_low, " game/team combos with < 40 min total lineup time (gaps in coverage)"))
  low_examples <- game_total_time[total_min < 40][order(total_min)][1:min(3, n_low)]
  for (i in seq_len(nrow(low_examples))) {
    log_check("S3", "  Low coverage sample", "WARN",
              paste0(low_examples$game_id[i], " ", low_examples$Team[i],
                     " — ", low_examples$total_min[i], " min"))
  }
} else {
  log_check("S3", "Total lineup time (low)", "INFO",
            paste0("No game/team combos below 40 min \u2713"))
}

if (n_high > 0) {
  log_check("S3", "Total lineup time (high)", "INFO",
            paste0(n_high, " game/team combos > 58 min (likely OT games)"))
}

log_check("S3", "Mean total lineup time", "INFO",
          paste0("Mean=", mean_total, " min per game/team (expected ~48)"))

# Cleanup temp columns
lineup_check[, c("time_on_sec", "time_off_sec", "is_ot", "stint_duration") := NULL]


# ==========================================================================================================
# SECTION 4: TEAM & GAME CONSISTENCY
# ==========================================================================================================
cat("\n--- SECTION 4: TEAM & GAME CONSISTENCY ---\n\n")

# --------------------------------------------------
# 4A: Every game_id should have exactly 2 teams
# --------------------------------------------------
teams_per_game <- lineup_check[, .(n_teams = uniqueN(Team)), by = game_id]

n_not2 <- sum(teams_per_game$n_teams != 2)
if (n_not2 > 0) {
  log_check("S4", "2 teams per game", "FAIL",
            paste0(n_not2, " games do NOT have exactly 2 teams"))
  bad_games <- teams_per_game[n_teams != 2][1:min(5, n_not2)]
  for (i in seq_len(nrow(bad_games))) {
    game_teams_list <- unique(lineup_check[game_id == bad_games$game_id[i], Team])
    log_check("S4", "  Bad team count", "FAIL",
              paste0(bad_games$game_id[i], " — ", bad_games$n_teams[i],
                     " teams: ", paste(game_teams_list, collapse = ", ")))
  }
} else {
  log_check("S4", "2 teams per game", "INFO",
            paste0("All ", nrow(teams_per_game), " games have exactly 2 teams \u2713"))
}

# --------------------------------------------------
# 4B: Team != Opp (no team playing itself)
# --------------------------------------------------
n_self_play <- sum(lineup_check$Team == lineup_check$Opp, na.rm = TRUE)
if (n_self_play > 0) {
  log_check("S4", "Team != Opp", "FAIL",
            paste0(n_self_play, " rows where Team == Opp"))
} else {
  log_check("S4", "Team != Opp", "INFO", "No team playing itself \u2713")
}

# --------------------------------------------------
# 4C: Valid team abbreviation codes (3 letters, known NBA teams)
# --------------------------------------------------
valid_pm_teams <- c(
  "ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
  "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOR","NYK",
  "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTH","WAS"
)

all_teams <- unique(c(lineup_check$Team, lineup_check$Opp))
unknown_teams <- setdiff(all_teams, valid_pm_teams)

if (length(unknown_teams) > 0) {
  log_check("S4", "Valid team codes", "WARN",
            paste0(length(unknown_teams), " unknown team code(s): ",
                   paste(unknown_teams, collapse = ", ")))
} else {
  log_check("S4", "Valid team codes", "INFO",
            paste0("All ", length(all_teams), " team codes are valid PM abbreviations \u2713"))
}

# --------------------------------------------------
# 4D: PopMac_Game matches Team + Opp pairing
# --------------------------------------------------
game_team_map <- unique(lineup_check[, .(game_id, PopMac_Game, Team, Opp)])

# For each game, the PopMac_Game should be AwayHome — check both orderings
game_team_map[, expected_code_1 := paste0(Team, Opp)]
game_team_map[, expected_code_2 := paste0(Opp, Team)]
game_team_map[, code_match := (PopMac_Game == expected_code_1 | PopMac_Game == expected_code_2)]

n_mismatch <- sum(!game_team_map$code_match, na.rm = TRUE)
if (n_mismatch > 0) {
  log_check("S4", "PopMac_Game vs Team/Opp", "WARN",
            paste0(n_mismatch, " rows where PopMac_Game doesn't match Team+Opp pairing"))
  bad_codes <- game_team_map[code_match == FALSE][1:min(3, n_mismatch)]
  for (i in seq_len(nrow(bad_codes))) {
    log_check("S4", "  Code mismatch sample", "WARN",
              paste0(bad_codes$game_id[i], " — PopMac=", bad_codes$PopMac_Game[i],
                     " Team=", bad_codes$Team[i], " Opp=", bad_codes$Opp[i]))
  }
} else {
  log_check("S4", "PopMac_Game vs Team/Opp", "INFO",
            "All PopMac_Game codes match Team/Opp pairings \u2713")
}

# --------------------------------------------------
# 4E: Cross-check game count against rotations master
# --------------------------------------------------
if (exists("rot_master") && nrow(rot_master) > 0) {
  rot_games   <- uniqueN(rot_master$GAME_ID)
  lineup_games <- uniqueN(lineup_check$game_id)
  
  # Not a 1:1 match expected (lineup uses PM game_id format), but counts should be close
  log_check("S4", "Game count: rot_master vs lineup", "INFO",
            paste0("Rotations master: ", rot_games, " games | Lineup output: ", lineup_games, " games"))
  
  if (lineup_games < rot_games * 0.90) {
    log_check("S4", "Game count gap", "WARN",
              paste0("Lineup output has ", fmt_pct(1 - lineup_games / rot_games),
                     " fewer games than rotations master — check for dropped games"))
  }
}


# ==========================================================================================================
# SECTION 5: COVERAGE & QUALITY METRICS
# ==========================================================================================================
cat("\n--- SECTION 5: COVERAGE & QUALITY ---\n\n")

# --------------------------------------------------
# 5A: Lineup stints per game/team (expect 15-40 per team)
# --------------------------------------------------
stints_per <- lineup_check[, .N, by = .(game_id, Team)]
mean_stints <- round(mean(stints_per$N), 1)
n_very_low  <- sum(stints_per$N < 5)
n_very_high <- sum(stints_per$N > 60)

if (n_very_low > 0) {
  log_check("S5", "Stints per game/team (low)", "WARN",
            paste0(n_very_low, " game/team combos with < 5 stint segments"))
} else if (n_very_high > 0) {
  log_check("S5", "Stints per game/team (high)", "WARN",
            paste0(n_very_high, " game/team combos with > 60 stint segments (fragmentation?)"))
} else {
  log_check("S5", "Stints per game/team", "INFO",
            paste0("Mean=", mean_stints, " stints per game/team (expected 15-40) \u2713"))
}

# --------------------------------------------------
# 5B: Most common lineup per team (sanity — starters should dominate)
# --------------------------------------------------
lineup_check[, lineup_key := paste(P1, P2, P3, P4, P5, sep = " | ")]

top_lineups <- lineup_check[, .N, by = .(Team, lineup_key)][order(Team, -N)]
top_per_team <- top_lineups[, .SD[1], by = Team]

mean_top_count <- round(mean(top_per_team$N), 1)
log_check("S5", "Top lineup frequency", "INFO",
          paste0("Mean appearances of most-used lineup per team: ", mean_top_count))

lineup_check[, lineup_key := NULL]

# --------------------------------------------------
# 5C: Unique player count across entire dataset
# --------------------------------------------------
all_player_names <- unique(unlist(lineup_check[, ..player_cols]))
all_player_names <- all_player_names[!is.na(all_player_names) & trimws(all_player_names) != ""]

log_check("S5", "Unique players (new rows)", "INFO",
          paste0(length(all_player_names), " unique player names"))

# Flag suspiciously short names (likely data issues)
short_names <- all_player_names[nchar(all_player_names) < 4]
if (length(short_names) > 0) {
  log_check("S5", "Short player names", "WARN",
            paste0(length(short_names), " names < 4 chars: ",
                   paste(head(short_names, 5), collapse = ", ")))
}

# --------------------------------------------------
# 5D: Append overlap check — how many rows were already in the file?
# --------------------------------------------------
if (exists("pm_lineup_new_to_append") && exists("pm_lineup_new")) {
  n_overlap <- nrow(pm_lineup_new) - nrow(pm_lineup_new_to_append)
  pct_overlap <- fmt_pct(n_overlap / max(nrow(pm_lineup_new), 1))
  
  if (n_overlap > 0) {
    log_check("S5", "Append overlap", "INFO",
              paste0(n_overlap, " rows already existed in file (",
                     pct_overlap, " overlap) — skipped correctly"))
  } else {
    log_check("S5", "Append overlap", "INFO", "All new rows are fresh (0 overlap)")
  }
}


# ==========================================================================================================
# SUMMARY
# ==========================================================================================================
cat("\n========================================================\n")
cat("  V4 CLEANSE SUMMARY\n")
cat("========================================================\n\n")

n_info <- sum(cleanse_log$level == "INFO")
n_warn <- sum(cleanse_log$level == "WARN")
n_fail <- sum(cleanse_log$level == "FAIL")

cat("  \u2139\uFE0F  INFO :", n_info, "\n")
cat("  \u26A0\uFE0F  WARN :", n_warn, "\n")
cat("  \u274C FAIL :", n_fail, "\n\n")

if (n_fail > 0) {
  cat("\u274C FAIL items:\n")
  fail_items <- cleanse_log[cleanse_log$level == "FAIL", ]
  for (i in seq_len(nrow(fail_items))) {
    cat("  - [", fail_items$section[i], "] ", fail_items$check[i], ": ", fail_items$detail[i], "\n")
  }
  cat("\n")
}

if (n_warn > 0) {
  cat("\u26A0\uFE0F  WARN items:\n")
  warn_items <- cleanse_log[cleanse_log$level == "WARN", ]
  for (i in seq_len(nrow(warn_items))) {
    cat("  - [", warn_items$section[i], "] ", warn_items$check[i], ": ", warn_items$detail[i], "\n")
  }
  cat("\n")
}

cleanse_passed <- n_fail == 0

if (cleanse_passed) {
  cat("\u2705 V4 lineup data cleanse passed \u2014 no FAIL-level issues.\n\n")
} else {
  cat("\u274C V4 lineup data cleanse FAILED \u2014 review cleanse_log before proceeding.\n\n")
  cat("View full log with: View(cleanse_log)\n\n")
}


# ==========================================================================================================
# CLEANSE SUMMARY POPUP (Interactive)
# ==========================================================================================================
if (interactive()) {
  
  library(shiny)
  library(DT)
  
  cleanse_decision <- NULL
  
  app <- shinyApp(
    ui = fluidPage(
      tags$head(tags$style(HTML("
        body { background-color: #1a1a2e; color: #e0e0e0; font-family: 'Consolas', monospace; padding: 20px; }
        h2 { color: #00d4ff; }
        h3 { color: #ffcc00; margin-top: 20px; }
        .btn-continue { background-color: #00c853; color: white; font-size: 18px; padding: 12px 40px; border: none; border-radius: 6px; margin-right: 15px; }
        .btn-continue:hover { background-color: #00e676; color: white; }
        .btn-stop { background-color: #ff1744; color: white; font-size: 18px; padding: 12px 40px; border: none; border-radius: 6px; }
        .btn-stop:hover { background-color: #ff5252; color: white; }
        .summary-box { background-color: #16213e; border-radius: 8px; padding: 15px; margin: 10px 0; display: inline-block; margin-right: 20px; text-align: center; }
        .summary-box .count { font-size: 36px; font-weight: bold; }
        .info-count { color: #4fc3f7; }
        .warn-count { color: #ffcc00; }
        .fail-count { color: #ff1744; }
      "))),
      
      h2("V4 Lineup Data Cleanse Results"),
      
      div(style = "display: flex; margin-bottom: 20px;",
          div(class = "summary-box",
              div(class = "count info-count", textOutput("n_info_txt", inline = TRUE)),
              div("INFO")),
          div(class = "summary-box",
              div(class = "count warn-count", textOutput("n_warn_txt", inline = TRUE)),
              div("WARN")),
          div(class = "summary-box",
              div(class = "count fail-count", textOutput("n_fail_txt", inline = TRUE)),
              div("FAIL"))
      ),
      
      h3("Full Cleanse Log"),
      DTOutput("cleanse_table"),
      
      br(),
      div(style = "text-align: center; margin-top: 20px;",
          actionButton("btn_continue", "Continue", class = "btn-continue"),
          actionButton("btn_stop", "Stop", class = "btn-stop")
      )
    ),
    
    server = function(input, output, session) {
      
      output$n_info_txt <- renderText(sum(cleanse_log$level == "INFO"))
      output$n_warn_txt <- renderText(sum(cleanse_log$level == "WARN"))
      output$n_fail_txt <- renderText(sum(cleanse_log$level == "FAIL"))
      
      output$cleanse_table <- renderDT({
        datatable(
          cleanse_log,
          options = list(
            pageLength = 50,
            dom = 'ftip',
            order = list(list(2, 'desc'))
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            'level',
            backgroundColor = styleEqual(
              c('INFO', 'WARN', 'FAIL'),
              c('#1b3a2a', '#3a3a1b', '#3a1b1b')
            ),
            color = styleEqual(
              c('INFO', 'WARN', 'FAIL'),
              c('#4fc3f7', '#ffcc00', '#ff1744')
            ),
            fontWeight = 'bold'
          )
      })
      
      observeEvent(input$btn_continue, {
        cleanse_decision <<- "continue"
        stopApp()
      })
      
      observeEvent(input$btn_stop, {
        cleanse_decision <<- "stop"
        stopApp()
      })
    }
  )
  
  runApp(app, launch.browser = TRUE)
  
  if (is.null(cleanse_decision) || cleanse_decision == "stop") {
    stop("V4 Cleanse stopped by user. Review cleanse_log before proceeding.")
  } else {
    cat("\u2705 V4 Cleanse approved. Continuing to cleanup...\n\n")
  }
  
} else {
  # Non-interactive fallback
  if (!cleanse_passed) {
    stop("V4 lineup data cleanse failed \u2014 review cleanse_log before proceeding.")
  }
}

# 💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊
# 💊💊💊 END: V4 Lineup Data Cleanse & Diagnostic Layer
# 💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊💊

# ------------------------------------------------------------
# 10) CLEANUP
# ------------------------------------------------------------
gc()