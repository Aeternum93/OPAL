# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== V3: ROTATIONS → MASTER → PM_LINEUP APPEND (TRUE LINEUPS) ====
#     - combines rotations_*.csv into rotations_master_<season>.csv
#     - rebuilds TRUE 5-man lineup segments from IN/OUT events
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
rot_master[, GAME_ID := as.character(GAME_ID)]
rot_master[, TEAM_ID := as.character(TEAM_ID)]
rot_master[, home_away := tolower(as.character(home_away))]
rot_master[, PLAYER_FIRST := as.character(PLAYER_FIRST)]
rot_master[, PLAYER_LAST  := as.character(PLAYER_LAST)]
rot_master[, IN_TIME_REAL  := suppressWarnings(as.numeric(IN_TIME_REAL)) / 10]
rot_master[, OUT_TIME_REAL := suppressWarnings(as.numeric(OUT_TIME_REAL)) / 10]

# write master
fwrite(rot_master, rot_master_path)
message("Saved rotations master: ", rot_master_path)

# working copy
rot_dt <- copy(rot_master)

# ------------------------------------------------------------
# 3) ATTACH TEAM METADATA
#     - team_short_name (ATL)
#     - espn_team_id
#     - nbaapi_team_long_name (Hawks / Warriors / etc.)  <-- Team_Long source
# ------------------------------------------------------------
team_map <- fread(team_map_path, colClasses = "character")

need_team_cols <- c("nba_team_id","espn_team_id","team_short_name","nbaapi_team_long_name")
missing_team_cols <- setdiff(need_team_cols, names(team_map))
if (length(missing_team_cols) > 0) {
  stop(paste0("team_id_mapping.csv missing cols: ", paste(missing_team_cols, collapse = ", ")))
}

setnames(team_map, "nba_team_id", "TEAM_ID")  # join key name

rot_dt <- merge(
  rot_dt,
  team_map[, .(TEAM_ID, espn_team_id, team_short_name, nbaapi_team_long_name)],
  by = "TEAM_ID",
  all.x = TRUE
)

# ------------------------------------------------------------
# 4) DATE -> PopMac_Date (fully robust)
# ------------------------------------------------------------

rot_dt[, game_date := trimws(as.character(game_date))]

# Try YYYYMMDD
rot_dt[, game_date_parsed := as.Date(game_date, format="%Y%m%d")]

# Try YYYY-MM-DD
rot_dt[is.na(game_date_parsed),
       game_date_parsed := as.Date(game_date, format="%Y-%m-%d")
]

# Try mdy() last
rot_dt[is.na(game_date_parsed),
       game_date_parsed := suppressWarnings(mdy(game_date))
]

rot_dt[, PopMac_Date := format(game_date_parsed, "%Y%m%d")]

# ------------------------------------------------------------
# 5) BUILD PopMac_Game (away+home) PER GAME_ID
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

# ------------------------------------------------------------
# Build Opp Map Correctly (guaranteed flip)
# ------------------------------------------------------------

team_per_game <- unique(rot_dt[, .(GAME_ID, TEAM_ID, team_short_name)])

opp_map <- team_per_game[
  team_per_game,
  on = .(GAME_ID),
  allow.cartesian = TRUE
][
  TEAM_ID != i.TEAM_ID,
  .(
    GAME_ID,
    TEAM_ID,
    Opp = i.team_short_name
  )
]

# ------------------------------------------------------------
# 6) TRUE LINEUP ENGINE (IN/OUT EVENTS)
#     -> start_time/end_time + P1..P5
# ------------------------------------------------------------
rot_dt[, player_name := str_squish(paste(PLAYER_FIRST, PLAYER_LAST))]

build_true_lineups <- function(dt_game_team) {
  dt_game_team <- dt_game_team[!is.na(IN_TIME_REAL) & !is.na(OUT_TIME_REAL)]
  if (nrow(dt_game_team) == 0) return(data.table())
  
  ev_in  <- dt_game_team[, .(time = IN_TIME_REAL,  player = player_name, type = "IN")]
  ev_out <- dt_game_team[, .(time = OUT_TIME_REAL, player = player_name, type = "OUT")]
  events <- rbindlist(list(ev_in, ev_out), use.names = TRUE, fill = TRUE)
  
  # sort: OUT before IN at same timestamp to avoid phantom 6-man
  events[type == "OUT", type_ord := 0L]
  events[type == "IN",  type_ord := 1L]
  setorder(events, time, type_ord)
  events[, type_ord := NULL]
  
  active <- character(0)
  out <- list()
  
  times <- sort(unique(events$time))
  events_by_time <- split(events, by = "time", keep.by = FALSE)
  
  for (idx in seq_along(times)) {
    t <- times[idx]
    next_t <- if (idx < length(times)) times[idx + 1] else NA_real_
    
    ev <- events_by_time[[as.character(t)]]
    
    for (j in seq_len(nrow(ev))) {
      if (ev$type[j] == "OUT") {
        active <- setdiff(active, ev$player[j])
      } else {
        active <- unique(c(active, ev$player[j]))
      }
    }
    
    if (!is.na(next_t) && next_t > t && length(active) == 5) {
      lp <- sort(active)
      out[[length(out) + 1]] <- data.table(
        start_time = t,
        end_time   = next_t,
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
  message("No true lineups were produced (active==5 never occurred). Check IN/OUT timing meaning.")
  quit(save = "no")
}

# ------------------------------------------------------------
# 7) SECONDS -> Period + Clock (mm:ss remaining)
# ------------------------------------------------------------
sec_to_period_clock <- function(sec) {
  
  if (is.na(sec)) {
    return(list(period = NA_character_, mmss = NA_character_))
  }
  
  sec <- as.integer(floor(sec))   # <-- FORCE INTEGER SECONDS HERE
  
  period_num <- floor(sec / 720) + 1
  remaining  <- 720 - (sec %% 720)
  
  list(
    period = paste0("Period ", period_num),
    mmss   = sprintf(
      "%d:%02d",
      as.integer(floor(remaining / 60)),
      as.integer(remaining %% 60)
    )
  )
}

lineup_dt[, Period   := vapply(start_time, function(x) sec_to_period_clock(x)$period, character(1))]
lineup_dt[, Time_On  := vapply(start_time, function(x) sec_to_period_clock(x)$mmss, character(1))]
lineup_dt[, Time_Off := vapply(end_time,   function(x) sec_to_period_clock(x)$mmss, character(1))]


# Fix Time_Off > Time_On at quarter boundaries
lineup_dt[, Time_On_sec  := start_time]
lineup_dt[, Time_Off_sec := end_time]
 
lineup_dt[, max_time_on := max(Time_On_sec), by = .(GAME_ID, TEAM_ID, Period)]

lineup_dt[
  Time_Off_sec > max_time_on,
  Time_Off := "0:00"
]

lineup_dt[, c("Time_On_sec", "Time_Off_sec", "max_time_on") := NULL]

# ------------------------------------------------------------
# 8) ATTACH PopMac + Team/Opp + Build final schema
# ------------------------------------------------------------
lineup_dt <- merge(
  lineup_dt,
  game_popmac[, .(GAME_ID, PopMac_Game, PopMac_Date, game_id)],
  by = "GAME_ID",
  all.x = TRUE
)

# attach Team + Team_Long from rot_dt (stable per GAME_ID+TEAM_ID)
team_meta <- unique(rot_dt[, .(GAME_ID, TEAM_ID, Team = team_short_name, Team_Long = nbaapi_team_long_name)])
lineup_dt <- merge(
  lineup_dt,
  team_meta,
  by = c("GAME_ID","TEAM_ID"),
  all.x = TRUE
)

lineup_dt <- merge(
  lineup_dt,
  opp_map,
  by = c("GAME_ID","TEAM_ID"),
  all.x = TRUE
)

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

# hard filters
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
# 8B) REPAIR MISSING TEAM / OPP USING GAME_ID (ROBUST)
# ------------------------------------------------------------

# Normalize blanks to NA
pm_lineup_new[, Team := trimws(as.character(Team))]
pm_lineup_new[, Opp  := trimws(as.character(Opp))]

pm_lineup_new[Team == "", Team := NA_character_]
pm_lineup_new[Opp  == "", Opp  := NA_character_]

# Build full list of teams per game using BOTH Team and Opp columns
teams_from_team <- pm_lineup_new[!is.na(Team), .(game_id, team = Team)]
teams_from_opp  <- pm_lineup_new[!is.na(Opp),  .(game_id, team = Opp)]

game_teams <- unique(rbindlist(list(teams_from_team, teams_from_opp)))

# Keep max two teams per game_id
game_teams <- game_teams[
  order(game_id, team)
][
  , .SD[seq_len(min(.N, 2L))],
  by = game_id
]

# Build symmetric mapping (game_id, team) -> other_team
team_pairs <- merge(
  game_teams,
  game_teams,
  by = "game_id",
  allow.cartesian = TRUE,
  suffixes = c("", "_other")
)[team != team_other]

setnames(team_pairs, c("team", "team_other"), c("team", "other_team"))

# ------------------------------------------------------------
# Fill missing Opp when Team exists
# ------------------------------------------------------------

pm_lineup_new <- merge(
  pm_lineup_new,
  team_pairs[, .(game_id, team, other_team)],
  by.x = c("game_id", "Team"),
  by.y = c("game_id", "team"),
  all.x = TRUE
)

pm_lineup_new[
  is.na(Opp) & !is.na(other_team),
  Opp := other_team
]

pm_lineup_new[, other_team := NULL]

# ------------------------------------------------------------
# Fill missing Team when Opp exists
# ------------------------------------------------------------

pm_lineup_new <- merge(
  pm_lineup_new,
  team_pairs[, .(game_id, team, other_team)],
  by.x = c("game_id", "Opp"),
  by.y = c("game_id", "team"),
  all.x = TRUE,
  suffixes = c("", "_teamfill")
)

pm_lineup_new[
  is.na(Team) & !is.na(other_team),
  Team := other_team
]

pm_lineup_new[, other_team := NULL]

# Diagnostics
message("Remaining NA Team: ", sum(is.na(pm_lineup_new$Team)))
message("Remaining NA Opp : ", sum(is.na(pm_lineup_new$Opp)))

# ------------------------------------------------------------
# 8C) ENFORCE CANONICAL COLUMN ORDER (CRITICAL FOR APPEND)
# ------------------------------------------------------------

canonical_cols <- c(
  "PopMac_Game",
  "PopMac_Date",
  "game_id",
  "Period",
  "Time_On",
  "Time_Off",
  "Team_Long",
  "Team",
  "Opp",
  "P_M",
  "P1","P2","P3","P4","P5"
)

pm_lineup_new <- pm_lineup_new[, ..canonical_cols]

# ------------------------------------------------------------
# 9) APPEND ONLY NEW ROWS (COLUMN ORDER SAFE)
# ------------------------------------------------------------

pm_lineup_new[, row_key := paste(
  game_id, Team, Period, Time_On, Time_Off,
  P1, P2, P3, P4, P5,
  sep="|"
)]

pm_lineup_new_to_append <- pm_lineup_new[!(row_key %in% existing_key)]
pm_lineup_new_to_append[, row_key := NULL]

# Enforce order again before writing
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

# ------------------------------------------------------------
# 10) CLEANUP
# ------------------------------------------------------------

gc()