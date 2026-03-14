# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== PBP LINEUP RECONSTRUCTION (V2) + GOLDEN COPY BUILDER ====
#
# PART 1: PBP Lineup Reconstruction
#   - Reads raw ESPN PBP (nbapbp), extracts substitutions
#   - Infers starters from Q1 actions
#   - Walks subs chronologically to build lineup segments
#   - Converts ESPN abbreviations (nba_schedule_abv) to standard
#     codes (nba_api_abv) via team_id_mapping.csv
#   - Adds row-level + game-level quality flags
#   - Output: pbp_lineup_data_<season>.csv
#
# PART 2: Golden Copy Builder
#   - Loads nba_lineup_data (from V5 rotation builder) with quality flags
#   - Loads pbp_lineup_data (from Part 1) with quality flags
#   - Game-by-game, team-by-team, period-by-period comparison
#   - Takes PBP where both pass (more precise), nba where only it passes,
#     fallback to higher coverage when neither passes
#   - NO duplicate rows — one source per game/team/period
#   - Output: golden_lineup_data_<season>.csv
#
# PART 3: Golden Copy Data Cleanse (S1-S6)
#   - Full validation of final golden copy
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

rm(list = setdiff(ls(), c(
  "current_date","formatted_date","formatted_year",
  "next_year_date","pbp_season","season_token","season_token2",
  "base_path","logo_dir","essential_names",
  "scripts","batches","run_script","run_batch"
)))

library(data.table)
library(stringr)

# ============================================================
# SHARED HELPERS + TEAM MAPPING
# ============================================================
team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"
team_map <- fread(team_map_path, colClasses = "character")

# Build schedule_abv -> api_abv lookup
abv_lookup <- unique(team_map[, .(nba_schedule_abv, nba_api_abv)])
abv_lookup <- abv_lookup[!is.na(nba_schedule_abv) & nba_schedule_abv != ""]

# Function to convert ESPN schedule abbreviations to standard nba_api_abv
to_std_abv <- function(x) {
  x <- as.character(x)
  idx <- match(x, abv_lookup$nba_schedule_abv)
  out <- ifelse(!is.na(idx), abv_lookup$nba_api_abv[idx], x)
  out
}

VALID_TEAMS <- c(
  "ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
  "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOR","NYK",
  "OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTH","WAS"
)

mmss_to_sec <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) {
    if (length(p) != 2) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2])
  }, numeric(1))
}


# ████████████████████████████████████████████████████████████
# ██  PART 1: PBP LINEUP RECONSTRUCTION                    ██
# ████████████████████████████████████████████████████████████

# ------------------------------------------------------------
# 0) PATHS
# ------------------------------------------------------------
pbp_path      <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")
name_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"
pbp_lineup_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/pbp_lineup_data_", season_token, ".csv")

# ------------------------------------------------------------
# 1) LOAD DATA
# ------------------------------------------------------------
message("Loading PBP data...")
pbp <- fread(pbp_path)

message("Loading name mapping...")
name_map <- fread(name_map_path)

message("PBP rows: ", nrow(pbp))
message("Unique games: ", uniqueN(pbp$game_id))

# ------------------------------------------------------------
# 2) BUILD ESPN ID -> CLEAN NAME LOOKUP
# ------------------------------------------------------------
name_lookup <- unique(name_map[!is.na(espn_player_id) & espn_player_id != "" & clean_player_name != "-",
                               .(espn_player_id = as.integer(espn_player_id), clean_player_name)])
name_lookup <- name_lookup[, .SD[1], by = espn_player_id]
message("Name lookup: ", nrow(name_lookup), " player ID -> name mappings")

# ------------------------------------------------------------
# 3) EXTRACT SUBSTITUTION EVENTS
# ------------------------------------------------------------
subs <- pbp[type_text == "Substitution" & !is.na(athlete_id_1) & !is.na(athlete_id_2),
            .(game_id, period_number, clock_display_value,
              start_game_seconds_remaining,
              player_in_id = as.integer(athlete_id_1),
              player_out_id = as.integer(athlete_id_2),
              team_id = as.integer(team_id),
              home_team_id = as.integer(home_team_id),
              away_team_id = as.integer(away_team_id),
              home_team_abbrev, away_team_abbrev,
              game_date)]

# Strip decimal seconds from clock (e.g., "0:02.1" -> "0:02")
subs[, clock_display_value := sub("\\.\\d+$", "", clock_display_value)]

# Convert team abbreviations to standard nba_api_abv format
subs[, home_team_abbrev := to_std_abv(home_team_abbrev)]
subs[, away_team_abbrev := to_std_abv(away_team_abbrev)]

# Attach player names
subs <- merge(subs, name_lookup, by.x = "player_in_id", by.y = "espn_player_id", all.x = TRUE)
setnames(subs, "clean_player_name", "player_in_name")
subs <- merge(subs, name_lookup, by.x = "player_out_id", by.y = "espn_player_id", all.x = TRUE)
setnames(subs, "clean_player_name", "player_out_name")

n_missing_in  <- sum(is.na(subs$player_in_name))
n_missing_out <- sum(is.na(subs$player_out_name))
if (n_missing_in > 0) message("WARNING: ", n_missing_in, " sub events missing player_in name")
if (n_missing_out > 0) message("WARNING: ", n_missing_out, " sub events missing player_out name")

setorder(subs, game_id, -start_game_seconds_remaining)

# ------------------------------------------------------------
# 4) INFER STARTERS FROM PRE-SUB ACTIONS
# ------------------------------------------------------------
q1_actions <- pbp[period_number == 1 & !is.na(athlete_id_1),
                  .(game_id, team_id = as.integer(team_id),
                    athlete_id = as.integer(athlete_id_1),
                    start_game_seconds_remaining,
                    type_text)]
q1_actions <- merge(q1_actions, name_lookup, by.x = "athlete_id", by.y = "espn_player_id", all.x = TRUE)

infer_starters <- function(game_subs, game_actions, tid) {
  team_subs_q1 <- game_subs[team_id == tid & period_number == 1]
  
  subbed_out_first <- character(0)
  subbed_in <- character(0)
  
  for (i in seq_len(nrow(team_subs_q1))) {
    p_in  <- team_subs_q1$player_in_name[i]
    p_out <- team_subs_q1$player_out_name[i]
    if (!is.na(p_out) && !p_out %in% subbed_in) {
      subbed_out_first <- unique(c(subbed_out_first, p_out))
    }
    if (!is.na(p_in)) {
      subbed_in <- unique(c(subbed_in, p_in))
    }
  }
  
  first_sub_time <- if (nrow(team_subs_q1) > 0) {
    max(team_subs_q1$start_game_seconds_remaining, na.rm = TRUE)
  } else { 0 }
  
  early_actors <- game_actions[team_id == tid & 
                                 start_game_seconds_remaining >= first_sub_time &
                                 !is.na(clean_player_name),
                               unique(clean_player_name)]
  
  starters <- unique(c(subbed_out_first, setdiff(early_actors, subbed_in)))
  if (length(starters) > 5) starters <- starters[1:5]
  starters
}

# ------------------------------------------------------------
# 5) BUILD LINEUP SEGMENTS FROM SUBS
# ------------------------------------------------------------
build_pbp_lineups <- function(game_id_val) {
  game_subs <- subs[game_id == game_id_val]
  game_acts <- q1_actions[game_id == game_id_val]
  
  if (nrow(game_subs) == 0) return(data.table())
  
  home_tid <- game_subs$home_team_id[1]
  away_tid <- game_subs$away_team_id[1]
  home_abbrev <- game_subs$home_team_abbrev[1]
  away_abbrev <- game_subs$away_team_abbrev[1]
  gdate <- game_subs$game_date[1]
  
  max_period <- max(game_subs$period_number, na.rm = TRUE)
  
  # Skip All-Star / special games
  if (any(c(home_abbrev, away_abbrev) %in% c("STARS", "STRIPES", "WORLD"))) {
    return(data.table())
  }
  
  results <- list()
  
  for (tid in c(home_tid, away_tid)) {
    team_abbrev <- if (tid == home_tid) home_abbrev else away_abbrev
    opp_abbrev  <- if (tid == home_tid) away_abbrev else home_abbrev
    
    starters <- infer_starters(game_subs, game_acts, tid)
    if (length(starters) < 5) {
      message("  SKIP: ", team_abbrev, " in game ", game_id_val, 
              " — only ", length(starters), " starters inferred")
      next
    }
    
    team_subs <- game_subs[team_id == tid][order(-start_game_seconds_remaining)]
    
    # Deduplicate subs at the same clock time and period
    # ESPN sometimes records the same sub event twice
    team_subs <- unique(team_subs, by = c("period_number", "clock_display_value", "player_in_id", "player_out_id"))
    
    active <- starters[1:5]
    last_clock <- "12:00"
    last_period <- 1L
    
    # Track which periods have been closed to prevent double-closing
    closed_periods <- integer(0)
    
    # Helper to create a stint row
    make_stint <- function(per, t_on, t_off, lineup) {
      lp <- sort(lineup)
      data.table(
        game_id_espn = game_id_val, game_date = as.character(gdate),
        Period = paste0("Period ", per),
        Time_On = t_on, Time_Off = t_off,
        Team = team_abbrev, Opp = opp_abbrev,
        home_abbrev = home_abbrev, away_abbrev = away_abbrev,
        P1 = lp[1], P2 = lp[2], P3 = lp[3], P4 = lp[4], P5 = lp[5]
      )
    }
    
    for (s in seq_len(nrow(team_subs))) {
      sub_clock  <- team_subs$clock_display_value[s]
      sub_period <- team_subs$period_number[s]
      p_in       <- team_subs$player_in_name[s]
      p_out      <- team_subs$player_out_name[s]
      
      if (is.na(p_in) || is.na(p_out) || is.na(sub_clock) || is.na(sub_period)) next
      
      # Period change: close previous period at 0:00, start new period
      if (sub_period != last_period) {
        
        # Close previous period IF not already closed and we have 5 active
        if (length(active) == 5 && !last_period %in% closed_periods) {
          # Only create stint if there's actual time between last_clock and 0:00
          if (last_clock != "0:00") {
            results[[length(results) + 1]] <- make_stint(last_period, last_clock, "0:00", active)
          }
          closed_periods <- c(closed_periods, last_period)
        }
        
        # Fill any skipped periods (e.g., jump from Period 2 to Period 4)
        if (sub_period > last_period + 1 && length(active) == 5) {
          for (skip_p in (last_period + 1):(sub_period - 1)) {
            if (!skip_p %in% closed_periods) {
              skip_start <- if (skip_p >= 5) "5:00" else "12:00"
              results[[length(results) + 1]] <- make_stint(skip_p, skip_start, "0:00", active)
              closed_periods <- c(closed_periods, skip_p)
            }
          }
        }
        
        # Start new period
        last_clock <- if (sub_period >= 5) "5:00" else "12:00"
        last_period <- sub_period
      }
      
      # Create segment from last event to this sub
      if (length(active) == 5 && last_clock != sub_clock) {
        results[[length(results) + 1]] <- make_stint(last_period, last_clock, sub_clock, active)
      }
      
      # Apply substitution
      active <- setdiff(active, p_out)
      active <- unique(c(active, p_in))
      last_clock <- sub_clock
    }
    
    # Final segment: close the last period we were in
    if (length(active) == 5 && !last_period %in% closed_periods) {
      if (last_clock != "0:00") {
        results[[length(results) + 1]] <- make_stint(last_period, last_clock, "0:00", active)
      }
      closed_periods <- c(closed_periods, last_period)
    }
    
    # OT carry-forward: if game has more periods than the last sub period,
    # the same lineup played through those periods without subbing
    if (length(active) == 5 && last_period < max_period) {
      for (p in (last_period + 1):max_period) {
        if (!p %in% closed_periods) {
          ot_start <- if (p >= 5) "5:00" else "12:00"
          results[[length(results) + 1]] <- make_stint(p, ot_start, "0:00", active)
          closed_periods <- c(closed_periods, p)
        }
      }
    }
  }
  
  if (length(results) == 0) return(data.table())
  rbindlist(results, use.names = TRUE, fill = TRUE)
}

# ------------------------------------------------------------
# 6) PROCESS ALL GAMES
# ------------------------------------------------------------
all_espn_games <- unique(pbp$game_id)
message("\nProcessing ", length(all_espn_games), " games...")

all_lineups <- list()
n_processed <- 0
n_skipped <- 0

for (gid in all_espn_games) {
  result <- tryCatch({
    build_pbp_lineups(gid)
  }, error = function(e) {
    message("  ERROR on game ", gid, ": ", e$message)
    data.table()
  })
  
  if (nrow(result) > 0) {
    all_lineups[[length(all_lineups) + 1]] <- result
    n_processed <- n_processed + 1
  } else {
    n_skipped <- n_skipped + 1
  }
  
  if ((n_processed + n_skipped) %% 100 == 0) {
    message("  Progress: ", n_processed + n_skipped, " / ", length(all_espn_games), " games")
  }
}

pbp_lineup_dt <- rbindlist(all_lineups, use.names = TRUE, fill = TRUE)

message("\nPBP lineup reconstruction complete:")
message("  Games processed: ", n_processed)
message("  Games skipped: ", n_skipped)
message("  Total lineup rows: ", nrow(pbp_lineup_dt))

# ------------------------------------------------------------
# 7) FORMAT OUTPUT
# game_id = AwayHome_YYYYMMDD using nba_api_abv (already converted)
# away_abbrev and home_abbrev are carried from the PBP data
# so game_id is ALWAYS AwayHome — never flipped
# ------------------------------------------------------------
pbp_lineup_dt[, PBP_Date := gsub("-", "", substr(game_date, 1, 10))]

# game_id = AwayHome_Date — using the PBP's actual away/home designation
pbp_lineup_dt[, PBP_Game := paste0(away_abbrev, home_abbrev)]
pbp_lineup_dt[, game_id := paste0(PBP_Game, "_", PBP_Date)]
setnames(pbp_lineup_dt, "game_id_espn", "espn_game_id")

pbp_lineup_dt[, Team_Long := NA_character_]
pbp_lineup_dt[, P_M := NA_character_]

# Remove bogus full-period rows that overlap real stints
pbp_lineup_dt[, n_rows := .N, by = .(game_id, Team, Period)]
pbp_lineup_dt <- pbp_lineup_dt[!(n_rows > 1 & Time_Off == "0:00" & Time_On %in% c("12:00", "5:00"))]
pbp_lineup_dt[, n_rows := NULL]

# Fix Time_Off > Time_On
pbp_lineup_dt[, ton := mmss_to_sec(Time_On)]
pbp_lineup_dt[, toff := mmss_to_sec(Time_Off)]
pbp_lineup_dt[toff > ton, Time_Off := "0:00"]
pbp_lineup_dt[, c("ton", "toff") := NULL]

# Canonical columns
pbp_canonical <- c(
  "PBP_Game","PBP_Date","game_id","espn_game_id",
  "Period","Time_On","Time_Off",
  "Team_Long","Team","Opp","P_M",
  "P1","P2","P3","P4","P5"
)
pbp_lineup_out <- unique(pbp_lineup_dt[, ..pbp_canonical])

# ------------------------------------------------------------
# 8) ROW-LEVEL QUALITY FLAGS
# ------------------------------------------------------------
message("\n=== PBP ROW-LEVEL QUALITY ===")

pbp_lineup_out[, ton_sec  := mmss_to_sec(Time_On)]
pbp_lineup_out[, toff_sec := mmss_to_sec(Time_Off)]

player_cols <- c("P1","P2","P3","P4","P5")
pbp_lineup_out[, n_uniq := apply(.SD, 1, function(x) length(unique(na.omit(x)))), .SDcols = player_cols]
pbp_lineup_out[, time_valid   := (ton_sec >= toff_sec) & !is.na(ton_sec) & !is.na(toff_sec)]
pbp_lineup_out[, team_valid   := Team %in% VALID_TEAMS & Opp %in% VALID_TEAMS]
pbp_lineup_out[, format_valid := grepl("^\\d+:\\d{2}$", Time_On) & grepl("^\\d+:\\d{2}$", Time_Off)]

pbp_lineup_out[, quality_flag := fcase(
  n_uniq != 5,   "BAD_PLAYER_COUNT",
  !time_valid,   "TIME_INVERTED",
  !team_valid,   "BAD_TEAM_CODE",
  !format_valid, "BAD_TIME_FORMAT",
  default = "PASS"
)]

n_pass <- sum(pbp_lineup_out$quality_flag == "PASS")
n_fail <- sum(pbp_lineup_out$quality_flag != "PASS")
message("Row quality: ", n_pass, " PASS | ", n_fail, " FAIL")
if (n_fail > 0) {
  message("Failures by type:")
  print(table(pbp_lineup_out$quality_flag[pbp_lineup_out$quality_flag != "PASS"]))
}
pbp_lineup_out[, c("ton_sec","toff_sec","n_uniq","time_valid","team_valid","format_valid") := NULL]

# ------------------------------------------------------------
# 9) GAME-LEVEL QUALITY FLAGS
# ------------------------------------------------------------
message("\n=== PBP GAME-LEVEL QUALITY ===")

pbp_game_quality <- pbp_lineup_out[quality_flag == "PASS", {
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
    game_quality = fifelse(has_4_periods & period_start_ok & period_end_ok, "PASS", "FAIL"))
}, by = .(game_id, Team)]

pbp_lineup_out <- merge(pbp_lineup_out, pbp_game_quality[, .(game_id, Team, game_quality)],
                        by = c("game_id", "Team"), all.x = TRUE)
pbp_lineup_out[is.na(game_quality), game_quality := "FAIL"]

n_gq_pass <- nrow(pbp_game_quality[game_quality == "PASS"])
n_gq_fail <- nrow(pbp_game_quality[game_quality != "PASS"])
message("Game/team quality: ", n_gq_pass, " PASS | ", n_gq_fail, " FAIL")

# ------------------------------------------------------------
# 10) SAVE PBP LINEUP
# ------------------------------------------------------------
fwrite(pbp_lineup_out, pbp_lineup_path)
message("\nSaved PBP lineup: ", pbp_lineup_path)
message("Total rows: ", nrow(pbp_lineup_out))

# Log skipped games (starter inference failures)
cat("\n=== STARTER INFERENCE FAILURES ===\n")
for (gid in all_espn_games) {
  game_subs <- subs[game_id == gid]
  if (nrow(game_subs) == 0) next
  home_tid <- game_subs$home_team_id[1]
  away_tid <- game_subs$away_team_id[1]
  for (tid in c(home_tid, away_tid)) {
    starters <- infer_starters(game_subs, q1_actions[game_id == gid], tid)
    if (length(starters) < 5) {
      abbrev <- if (tid == home_tid) game_subs$home_team_abbrev[1] else game_subs$away_team_abbrev[1]
      cat("Game:", gid, "Team:", abbrev, "— starters found:", length(starters), "\n")
    }
  }
}


# ████████████████████████████████████████████████████████████
# ██  PART 2: GOLDEN COPY BUILDER (Period-Level)           ██
# ████████████████████████████████████████████████████████████

message("\n\n========================================================")
message("  GOLDEN COPY BUILDER (Period-Level)")
message("========================================================\n")

pm_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine"

nba_lineup <- fread(file.path(pm_dir, paste0("nba_lineup_data_", season_token, ".csv")))
pbp_lineup <- fread(file.path(pm_dir, paste0("pbp_lineup_data_", season_token, ".csv")))

message("NBA lineup: ", nrow(nba_lineup), " rows | ", uniqueN(nba_lineup$game_id), " games")
message("PBP lineup: ", nrow(pbp_lineup), " rows | ", uniqueN(pbp_lineup$game_id), " games")

# ------------------------------------------------------------
# Period-level quality check function
# Returns TRUE if a set of rows for one game/team/period passes all criteria
# ------------------------------------------------------------
check_period_quality <- function(dt) {
  if (nrow(dt) == 0) return(FALSE)
  if (any(dt$quality_flag != "PASS")) return(FALSE)
  if (any(!grepl("^\\d+:\\d{2}$", dt$Time_On)) || any(!grepl("^\\d+:\\d{2}$", dt$Time_Off))) return(FALSE)
  
  tons  <- mmss_to_sec(dt$Time_On)
  toffs <- mmss_to_sec(dt$Time_Off)
  
  if (any(tons < toffs, na.rm = TRUE)) return(FALSE)
  
  period_num <- as.integer(gsub("Period ", "", dt$Period[1]))
  expected_start <- if (period_num >= 5) 300 else 720
  if (max(tons, na.rm = TRUE) != expected_start) return(FALSE)
  if (min(toffs, na.rm = TRUE) != 0) return(FALSE)
  
  for (i in seq_len(nrow(dt))) {
    if (length(unique(c(dt$P1[i], dt$P2[i], dt$P3[i], dt$P4[i], dt$P5[i]))) != 5) return(FALSE)
  }
  
  # Check for overlapping stints — only flag extreme overlaps (2x+ period length)
  total_cov <- sum(tons - toffs, na.rm = TRUE)
  period_max <- if (period_num >= 5) 300 else 720
  if (total_cov > period_max * 1.5) return(FALSE)
  
  TRUE
}
# ------------------------------------------------------------
# Build all unique game_id + Team + Period combos across both sources
# ------------------------------------------------------------
nba_gtp <- unique(nba_lineup[, .(game_id, Team, Period)])
nba_gtp[, in_nba := TRUE]
pbp_gtp <- unique(pbp_lineup[, .(game_id, Team, Period)])
pbp_gtp[, in_pbp := TRUE]

all_gtp <- merge(nba_gtp, pbp_gtp, by = c("game_id", "Team", "Period"), all = TRUE)
all_gtp[is.na(in_nba), in_nba := FALSE]
all_gtp[is.na(in_pbp), in_pbp := FALSE]

message("Total game/team/period combos: ", nrow(all_gtp))

# ------------------------------------------------------------
# Period-by-period decision loop
# ------------------------------------------------------------
golden_rows <- list()
decision_log <- list()
n_total <- nrow(all_gtp)

message("Evaluating period-by-period...")

for (i in seq_len(n_total)) {
  gid <- all_gtp$game_id[i]
  tm  <- all_gtp$Team[i]
  per <- all_gtp$Period[i]
  
  pbp_rows <- pbp_lineup[game_id == gid & Team == tm & Period == per]
  nba_rows <- nba_lineup[game_id == gid & Team == tm & Period == per]
  
  pbp_ok <- check_period_quality(pbp_rows)
  nba_ok <- check_period_quality(nba_rows)
  
  if (pbp_ok) {
    # PBP passes -> take PBP (more precise)
    golden_rows[[length(golden_rows) + 1]] <- pbp_rows
    src <- "PBP"
  } else if (nba_ok) {
    # Only NBA passes -> take NBA
    golden_rows[[length(golden_rows) + 1]] <- nba_rows
    src <- "NBA"
  } else {
    # Neither passes -> take whichever has more coverage, but penalize overlaps
    pbp_cov <- if (nrow(pbp_rows) > 0) sum(mmss_to_sec(pbp_rows$Time_On) - mmss_to_sec(pbp_rows$Time_Off), na.rm = TRUE) else 0
    nba_cov <- if (nrow(nba_rows) > 0) sum(mmss_to_sec(nba_rows$Time_On) - mmss_to_sec(nba_rows$Time_Off), na.rm = TRUE) else 0
    
    # Cap coverage at period max to prevent overlapping stints from winning
    period_num_fb <- as.integer(gsub("Period ", "", per))
    period_max_fb <- if (period_num_fb >= 5) 300 else 720
    pbp_cov <- min(pbp_cov, period_max_fb)
    nba_cov <- min(nba_cov, period_max_fb)
    
    if (pbp_cov >= nba_cov && nrow(pbp_rows) > 0) {
      golden_rows[[length(golden_rows) + 1]] <- pbp_rows
      src <- "PBP_fallback"
    } else if (nrow(nba_rows) > 0) {
      golden_rows[[length(golden_rows) + 1]] <- nba_rows
      src <- "NBA_fallback"
    } else {
      src <- "NONE"
    }
  }
  
  decision_log[[i]] <- data.table(game_id = gid, Team = tm, Period = per, source = src)
  
  if (i %% 500 == 0) message("  Progress: ", i, " / ", n_total, " periods")
}

golden_lineup <- rbindlist(golden_rows, use.names = TRUE, fill = TRUE)
decisions <- rbindlist(decision_log)

# Print decision summary
cat("\n=== GOLDEN COPY SOURCE DECISIONS (Period-Level) ===\n")
print(table(decisions$source))

# Print game-level summary
game_level <- decisions[, .(sources = paste(sort(unique(source)), collapse = "+")), by = .(game_id, Team)]
cat("\nGames using mixed sources:\n")
cat("Pure PBP:", sum(game_level$sources == "PBP"), "\n")
cat("Pure NBA:", sum(game_level$sources == "NBA"), "\n")
cat("Mixed:", sum(grepl("\\+", game_level$sources)), "\n")

# Unify column names
if ("NBA_Game" %in% names(golden_lineup)) setnames(golden_lineup, "NBA_Game", "game_code", skip_absent = TRUE)
if ("PBP_Game" %in% names(golden_lineup)) {
  if ("game_code" %in% names(golden_lineup)) {
    golden_lineup[is.na(game_code) & !is.na(PBP_Game), game_code := PBP_Game]
    golden_lineup[, PBP_Game := NULL]
  } else {
    setnames(golden_lineup, "PBP_Game", "game_code")
  }
}

if ("NBA_Date" %in% names(golden_lineup)) setnames(golden_lineup, "NBA_Date", "game_date_id", skip_absent = TRUE)
if ("PBP_Date" %in% names(golden_lineup)) {
  if ("game_date_id" %in% names(golden_lineup)) {
    golden_lineup[is.na(game_date_id) & !is.na(PBP_Date), game_date_id := PBP_Date]
    golden_lineup[, PBP_Date := NULL]
  } else {
    setnames(golden_lineup, "PBP_Date", "game_date_id")
  }
}

# Drop quality columns
drop_cols <- intersect(c("quality_flag", "game_quality"), names(golden_lineup))
if (length(drop_cols) > 0) golden_lineup[, (drop_cols) := NULL]

golden_lineup <- unique(golden_lineup)

# Fix single-digit seconds: "0:2" -> "0:02", "0:0" -> "0:00"
golden_lineup[, Time_On := sub("^(\\d+):(\\d)$", "\\1:0\\2", Time_On)]
golden_lineup[, Time_Off := sub("^(\\d+):(\\d)$", "\\1:0\\2", Time_Off)]

# Dedup overlapping stints within each game/team/period
golden_lineup[, ton_sec := mmss_to_sec(Time_On)]
golden_lineup[, toff_sec := mmss_to_sec(Time_Off)]
golden_lineup[, stint_dur := ton_sec - toff_sec]

period_cov <- golden_lineup[, .(total_cov = sum(stint_dur, na.rm = TRUE)), by = .(game_id, Team, Period)]
period_cov[, period_num := as.integer(gsub("Period ", "", Period))]
period_cov[, period_max := fifelse(period_num >= 5, 300, 720)]
overlap_periods <- period_cov[total_cov > period_max * 1.1]

if (nrow(overlap_periods) > 0) {
  message("Deduplicating ", nrow(overlap_periods), " periods with overlapping stints")
  
  for (r in seq_len(nrow(overlap_periods))) {
    gid <- overlap_periods$game_id[r]
    tm  <- overlap_periods$Team[r]
    per <- overlap_periods$Period[r]
    
    idx <- which(golden_lineup$game_id == gid & golden_lineup$Team == tm & golden_lineup$Period == per)
    rows <- golden_lineup[idx][order(-ton_sec, toff_sec)]
    
    # Walk top-down: keep stints that don't overlap with what we've already kept
    keep <- rep(FALSE, nrow(rows))
    keep[1] <- TRUE
    last_end <- rows$toff_sec[1]
    
    for (j in 2:nrow(rows)) {
      if (rows$ton_sec[j] <= last_end) {
        keep[j] <- TRUE
        last_end <- rows$toff_sec[j]
      }
    }
    
    # Remove the overlapping rows from golden_lineup
    drop_idx <- idx[!keep]
    if (length(drop_idx) > 0) {
      golden_lineup <- golden_lineup[-drop_idx]
    }
  }
}

golden_lineup[, c("ton_sec", "toff_sec", "stint_dur") := NULL]


# Attach espn_game_id from PBP lineup
espn_id_lookup <- unique(pbp_lineup[, .(game_id, espn_game_id)])
golden_lineup[espn_id_lookup, espn_game_id := i.espn_game_id, on = "game_id"]


# ------------------------------------------------------------
# GOLDEN COPY ENRICHMENT
# ------------------------------------------------------------

# --- E1: Fix espn_game_id column name (remove .y suffix if present) ---
if ("espn_game_id.y" %in% names(golden_lineup)) {
  if ("espn_game_id" %in% names(golden_lineup)) {
    golden_lineup[is.na(espn_game_id), espn_game_id := espn_game_id.y]
    golden_lineup[, espn_game_id.y := NULL]
  } else {
    setnames(golden_lineup, "espn_game_id.y", "espn_game_id")
  }
}
if ("espn_game_id.x" %in% names(golden_lineup)) {
  setnames(golden_lineup, "espn_game_id.x", "espn_game_id")
}
message("espn_game_id coverage: ", sum(!is.na(golden_lineup$espn_game_id)), " / ", nrow(golden_lineup), " rows")

# Remove any duplicate espn_game_id columns before merging
if ("espn_game_id.y" %in% names(golden_lineup)) golden_lineup[, espn_game_id.y := NULL]
if ("espn_game_id.x" %in% names(golden_lineup)) setnames(golden_lineup, "espn_game_id.x", "espn_game_id")

# --- E2: Pivot P1-P5 to get player IDs from hoopr_name_mapping ---
name_map <- fread("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv")
player_id_lookup <- unique(name_map[!is.na(clean_player_name) & clean_player_name != "-" & 
                                      !is.na(espn_player_id) & espn_player_id != "",
                                    .(clean_player_name, 
                                      espn_player_id = as.character(espn_player_id),
                                      nba_player_id = as.character(nba_player_id))])
player_id_lookup <- player_id_lookup[, .SD[1], by = clean_player_name]

# Add ESPN and NBA player IDs for P1-P5
for (p in 1:5) {
  pcol <- paste0("P", p)
  espn_col <- paste0("P", p, "_espn_id")
  nba_col <- paste0("P", p, "_nba_id")
  
  golden_lineup <- merge(golden_lineup, 
                         player_id_lookup[, .(clean_player_name, espn_pid = espn_player_id, nba_pid = nba_player_id)],
                         by.x = pcol, by.y = "clean_player_name", all.x = TRUE)
  setnames(golden_lineup, "espn_pid", espn_col)
  setnames(golden_lineup, "nba_pid", nba_col)
}

n_missing_ids <- sum(is.na(golden_lineup$P1_espn_id))
message("Player ID coverage: ", nrow(golden_lineup) - n_missing_ids, " / ", nrow(golden_lineup), " rows have P1 espn_id")

# --- E3: Pull nba_game_id from BaseStats_Team ---
bs_team_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. BaseStats_Team/BaseStats_Team_MC_", season_token, ".csv")

if (file.exists(bs_team_path)) {
  bs_team <- fread(bs_team_path, select = c("ESPN_GAME_ID", "NBA_GAME_ID"))
  bs_team[, ESPN_GAME_ID := as.character(ESPN_GAME_ID)]
  bs_team <- unique(bs_team)
  
  golden_lineup[, espn_game_id := as.character(espn_game_id)]
  golden_lineup <- merge(golden_lineup, bs_team, 
                         by.x = "espn_game_id", by.y = "ESPN_GAME_ID", all.x = TRUE)
  setnames(golden_lineup, "NBA_GAME_ID", "nba_game_id")
  
  message("nba_game_id coverage: ", sum(!is.na(golden_lineup$nba_game_id)), " / ", nrow(golden_lineup), " rows")
} else {
  message("WARNING: BaseStats_Team not found at: ", bs_team_path)
  golden_lineup[, nba_game_id := NA_character_]
}

message("\n=== GOLDEN COPY RESULTS ===")
message("Rows: ", nrow(golden_lineup))
message("Games: ", uniqueN(golden_lineup$game_id))
message("Teams: ", uniqueN(golden_lineup$Team))

golden_lineup[, stint_sec := mmss_to_sec(Time_On) - mmss_to_sec(Time_Off)]
golden_cov <- golden_lineup[, .(golden_min = round(sum(stint_sec, na.rm = TRUE) / 60, 1)), by = .(game_id, Team)]
golden_lineup[, stint_sec := NULL]

message("Mean coverage: ", round(mean(golden_cov$golden_min), 1), " min")
message("< 40 min: ", sum(golden_cov$golden_min < 40))

golden_path <- file.path(pm_dir, paste0("golden_lineup_data_", season_token, ".csv"))
fwrite(golden_lineup, golden_path)
message("\nSaved golden copy: ", golden_path)


# ████████████████████████████████████████████████████████████
# ██  PART 3: GOLDEN COPY DATA CLEANSE                     ██
# ████████████████████████████████████████████████████████████

lineup_check <- copy(golden_lineup)

cleanse_log <- data.frame(section = character(), check = character(),
                          level = character(), detail = character(), stringsAsFactors = FALSE)

log_check <- function(section, check, level, detail) {
  cleanse_log <<- rbind(cleanse_log, data.frame(section = section, check = check,
                                                level = level, detail = as.character(detail), stringsAsFactors = FALSE))
  sym <- switch(level, INFO = "\u2139\uFE0F ", WARN = "\u26A0\uFE0F ", FAIL = "\u274C ")
  cat(sym, "[", section, "]", check, "\u2014", detail, "\n")
}

fmt_pct <- function(x) paste0(round(x * 100, 1), "%")

mmss_v <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) { if (length(p)!=2) return(NA_real_); as.numeric(p[1])*60+as.numeric(p[2]) }, numeric(1))
}

cat("\n========================================================\n")
cat("  GOLDEN COPY DATA CLEANSE\n")
cat("========================================================\n\n")

n_new <- nrow(lineup_check)

# --- S1: STRUCTURAL INTEGRITY ---
cat("--- SECTION 1: STRUCTURAL INTEGRITY ---\n\n")

log_check("S1", "Row counts", "INFO", paste0("rows=", n_new, " | games=", uniqueN(lineup_check$game_id), " | teams=", uniqueN(lineup_check$Team)))

for (col in c("game_id","Team","Opp","Period","Time_On","Time_Off","P1","P2","P3","P4","P5")) {
  if (!col %in% names(lineup_check)) next
  n_na <- sum(is.na(lineup_check[[col]]) | trimws(as.character(lineup_check[[col]])) == "")
  if (n_na > 0) {
    log_check("S1", paste0("NA: ", col), "FAIL", paste0(n_na, " NA/blank (", fmt_pct(n_na / n_new), ")"))
  } else {
    log_check("S1", paste0("NA: ", col), "INFO", "0 NA/blank \u2713")
  }
}

dates <- suppressWarnings(as.Date(as.character(lineup_check$game_date_id), format = "%Y%m%d"))
if (sum(!is.na(dates)) > 0) {
  log_check("S1", "Date range", "INFO", paste0(min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE)))
}

dup_count <- nrow(lineup_check) - nrow(unique(lineup_check[, .(game_id, Team, Period, Time_On, Time_Off, P1, P2, P3, P4, P5)]))
if (dup_count > 0) {
  log_check("S1", "Duplicates", "FAIL", paste0(dup_count, " duplicate rows"))
} else {
  log_check("S1", "Duplicates", "INFO", "No duplicates \u2713")
}

# --- S2: LINEUP VALIDITY ---
cat("\n--- SECTION 2: LINEUP VALIDITY ---\n\n")

player_cols <- c("P1","P2","P3","P4","P5")
lineup_check[, n_uniq := apply(.SD, 1, function(x) length(unique(na.omit(x)))), .SDcols = player_cols]
n_not5 <- sum(lineup_check$n_uniq != 5)
if (n_not5 > 0) {
  log_check("S2", "5 unique players", "FAIL", paste0(n_not5, " rows without 5 unique players"))
} else {
  log_check("S2", "5 unique players", "INFO", paste0("All ", n_new, " rows have 5 unique players \u2713"))
}
lineup_check[, n_uniq := NULL]

n_blank <- sum(apply(lineup_check[, ..player_cols], 1, function(x) any(trimws(x) == "" | is.na(x))))
if (n_blank > 0) {
  log_check("S2", "Blank player names", "FAIL", paste0(n_blank, " rows with blank names"))
} else {
  log_check("S2", "Blank player names", "INFO", "No blank names \u2713")
}

pgt <- lineup_check[, { data.table(player = unique(c(P1,P2,P3,P4,P5))) }, by = .(game_id, Team)]
cross <- pgt[pgt, on = .(game_id, player), allow.cartesian = TRUE, nomatch = 0][Team != i.Team]
if (nrow(cross) > 0) {
  log_check("S2", "Cross-team players", "FAIL", paste0(uniqueN(cross$player), " players on both teams"))
} else {
  log_check("S2", "Cross-team players", "INFO", "No cross-team players \u2713")
}

roster <- pgt[, .(n = uniqueN(player)), by = .(game_id, Team)]
log_check("S2", "Roster size", "INFO", paste0("Mean=", round(mean(roster$n), 1), " per game/team"))

# --- S3: TIMING LOGIC ---
cat("\n--- SECTION 3: TIMING LOGIC ---\n\n")

lineup_check[, ton := mmss_v(Time_On)]
lineup_check[, toff := mmss_v(Time_Off)]

n_inv <- sum(lineup_check$ton < lineup_check$toff, na.rm = TRUE)
if (n_inv > 0) {
  log_check("S3", "Time_On >= Time_Off", "FAIL", paste0(n_inv, " inverted rows"))
} else {
  log_check("S3", "Time_On >= Time_Off", "INFO", paste0("All ", n_new, " rows valid \u2713"))
}

n_neg <- sum(lineup_check$ton < 0 | lineup_check$toff < 0, na.rm = TRUE)
if (n_neg > 0) { log_check("S3", "Negative times", "FAIL", paste0(n_neg, " rows"))
} else { log_check("S3", "Negative times", "INFO", "None \u2713") }

lineup_check[, is_ot := grepl("Period [56]", Period)]
n_reg12 <- sum(!lineup_check$is_ot & lineup_check$ton > 720, na.rm = TRUE)
n_ot5   <- sum(lineup_check$is_ot & lineup_check$ton > 300, na.rm = TRUE)
if (n_reg12 > 0) { log_check("S3", "Reg > 12:00", "WARN", paste0(n_reg12, " rows"))
} else { log_check("S3", "Reg time bounds", "INFO", "All <= 12:00 \u2713") }
if (n_ot5 > 0) { log_check("S3", "OT > 5:00", "WARN", paste0(n_ot5, " rows"))
} else { log_check("S3", "OT time bounds", "INFO", "All <= 5:00 \u2713") }

pc <- lineup_check[, .(Q1=any(Period=="Period 1"), Q2=any(Period=="Period 2"),
                       Q3=any(Period=="Period 3"), Q4=any(Period=="Period 4")), by=.(game_id,Team)]
n_mq <- sum(!pc$Q1 | !pc$Q2 | !pc$Q3 | !pc$Q4)
if (n_mq > 0) {
  log_check("S3", "Period coverage Q1-Q4", "WARN", paste0(n_mq, " game/team combos missing quarters"))
} else {
  log_check("S3", "Period coverage Q1-Q4", "INFO", paste0("All ", nrow(pc), " have Q1-Q4 \u2713"))
}

lineup_check[, dur := ton - toff]
gt <- lineup_check[, .(total_min = round(sum(dur, na.rm=TRUE)/60, 1)), by=.(game_id, Team)]
mean_t <- round(mean(gt$total_min), 1)
n_low <- sum(gt$total_min < 40)
log_check("S3", "Mean lineup time", "INFO", paste0("Mean=", mean_t, " min (expected ~48)"))
if (n_low > 0) {
  log_check("S3", "< 40 min coverage", "WARN", paste0(n_low, " game/team combos"))
}

lineup_check[, c("ton","toff","is_ot","dur") := NULL]

# --- S4: TEAM & GAME CONSISTENCY ---
cat("\n--- SECTION 4: TEAM & GAME CONSISTENCY ---\n\n")

tpg <- lineup_check[, .(n_teams = uniqueN(Team)), by = game_id]
n_not2 <- sum(tpg$n_teams != 2)
if (n_not2 > 0) {
  log_check("S4", "2 teams per game", "FAIL", paste0(n_not2, " games without 2 teams"))
  bg <- tpg[n_teams != 2][1:min(5, n_not2)]
  for (i in seq_len(nrow(bg))) {
    tl <- unique(lineup_check[game_id == bg$game_id[i], Team])
    log_check("S4", "  Bad game", "FAIL", paste0(bg$game_id[i], " — ", bg$n_teams[i], " teams: ", paste(tl, collapse=", ")))
  }
} else {
  log_check("S4", "2 teams per game", "INFO", paste0("All ", nrow(tpg), " games have 2 teams \u2713"))
}

n_self <- sum(lineup_check$Team == lineup_check$Opp, na.rm = TRUE)
if (n_self > 0) { log_check("S4", "Team != Opp", "FAIL", paste0(n_self, " rows"))
} else { log_check("S4", "Team != Opp", "INFO", "No self-play \u2713") }

all_t <- unique(c(lineup_check$Team, lineup_check$Opp))
unk <- setdiff(all_t, VALID_TEAMS)
if (length(unk) > 0) { log_check("S4", "Valid team codes", "FAIL", paste0(length(unk), " unknown: ", paste(unk, collapse=", ")))
} else { log_check("S4", "Valid team codes", "INFO", paste0("All ", length(all_t), " codes valid \u2713")) }

# --- S5: COVERAGE & QUALITY ---
cat("\n--- SECTION 5: COVERAGE & QUALITY ---\n\n")

sp <- lineup_check[, .N, by=.(game_id, Team)]
log_check("S5", "Stints per game/team", "INFO", paste0("Mean=", round(mean(sp$N), 1)))

lineup_check[, lk := paste(P1,P2,P3,P4,P5, sep=" | ")]
tl <- lineup_check[, .N, by=.(Team, lk)][order(Team, -N)]
tp <- tl[, .SD[1], by=Team]
log_check("S5", "Top lineup frequency", "INFO", paste0("Mean=", round(mean(tp$N), 1), " appearances"))
lineup_check[, lk := NULL]

ap <- unique(unlist(lineup_check[, ..player_cols]))
ap <- ap[!is.na(ap) & trimws(ap) != ""]
log_check("S5", "Unique players", "INFO", paste0(length(ap), " players"))

# --- S6: GOLDEN COPY INTEGRITY ---
cat("\n--- SECTION 6: GOLDEN COPY INTEGRITY ---\n\n")

# 6A: Each game must have minimum 4 periods per team
periods_per_game_team <- lineup_check[, .(n_periods = uniqueN(Period)), by = .(game_id, Team)]
n_under4 <- sum(periods_per_game_team$n_periods < 4)
if (n_under4 > 0) {
  log_check("S6", "Min 4 periods per game/team", "FAIL",
            paste0(n_under4, " game/team combos with < 4 periods"))
} else {
  log_check("S6", "Min 4 periods per game/team", "INFO",
            paste0("All ", nrow(periods_per_game_team), " game/team combos have 4+ periods \u2713"))
}

# 6C: All regulation periods must start with 12:00
reg_periods <- lineup_check[Period %in% c("Period 1", "Period 2", "Period 3", "Period 4")]
period_starts <- reg_periods[, .(max_time_on = max(mmss_v(Time_On), na.rm = TRUE)), 
                             by = .(game_id, Team, Period)]
n_not_12 <- sum(period_starts$max_time_on != 720, na.rm = TRUE)
if (n_not_12 > 0) {
  log_check("S6", "Regulation periods start at 12:00", "WARN",
            paste0(n_not_12, " game/team/period combos don't start at 12:00"))
} else {
  log_check("S6", "Regulation periods start at 12:00", "INFO",
            paste0("All regulation period starts = 12:00 \u2713"))
}

# 6D: All periods must end with 0:00
period_ends <- lineup_check[, .(min_time_off = min(mmss_v(Time_Off), na.rm = TRUE)),
                            by = .(game_id, Team, Period)]
n_not_0 <- sum(period_ends$min_time_off != 0, na.rm = TRUE)
if (n_not_0 > 0) {
  log_check("S6", "All periods end at 0:00", "WARN",
            paste0(n_not_0, " game/team/period combos don't end at 0:00"))
} else {
  log_check("S6", "All periods end at 0:00", "INFO",
            paste0("All period endings = 0:00 \u2713"))
}

# 6E: No raw seconds in Time_On/Time_Off
bad_format_on  <- sum(!grepl("^\\d+:\\d{2}$", lineup_check$Time_On), na.rm = TRUE)
bad_format_off <- sum(!grepl("^\\d+:\\d{2}$", lineup_check$Time_Off), na.rm = TRUE)
if (bad_format_on > 0 || bad_format_off > 0) {
  log_check("S6", "Time format (mm:ss)", "FAIL",
            paste0("Time_On bad: ", bad_format_on, " | Time_Off bad: ", bad_format_off))
} else {
  log_check("S6", "Time format (mm:ss)", "INFO", "All in mm:ss format \u2713")
}

# 6F: No old team codes
old_codes <- c("WSH", "NY", "GS", "UTAH", "NO", "NOP", "PHX", "UTA", "SA")
bad_team_codes <- lineup_check[Team %in% old_codes | Opp %in% old_codes]
if (nrow(bad_team_codes) > 0) {
  found_codes <- unique(c(bad_team_codes$Team[bad_team_codes$Team %in% old_codes],
                          bad_team_codes$Opp[bad_team_codes$Opp %in% old_codes]))
  log_check("S6", "No old team codes", "FAIL",
            paste0(nrow(bad_team_codes), " rows with old codes: ", paste(found_codes, collapse = ", ")))
} else {
  log_check("S6", "No old team codes", "INFO", "No old abbreviations found \u2713")
}

# 6G: No duplicate game_ids for same matchup
lineup_check[, sorted_teams := paste0(pmin(Team, Opp), pmax(Team, Opp))]
dup_matchups <- lineup_check[, .(n_game_ids = uniqueN(game_id)), by = .(game_date_id, sorted_teams)]
n_dup_matchups <- sum(dup_matchups$n_game_ids > 1)
if (n_dup_matchups > 0) {
  log_check("S6", "No duplicate game_ids per matchup", "FAIL",
            paste0(n_dup_matchups, " matchups with multiple game_ids"))
} else {
  log_check("S6", "No duplicate game_ids per matchup", "INFO",
            paste0("All matchups have single game_id \u2713"))
}
lineup_check[, sorted_teams := NULL]

# --- SUMMARY ---
cat("\n========================================================\n")
cat("  GOLDEN COPY CLEANSE SUMMARY\n")
cat("========================================================\n\n")

n_i <- sum(cleanse_log$level == "INFO")
n_w <- sum(cleanse_log$level == "WARN")
n_f <- sum(cleanse_log$level == "FAIL")
cat("  \u2139\uFE0F  INFO:", n_i, "\n  \u26A0\uFE0F  WARN:", n_w, "\n  \u274C FAIL:", n_f, "\n\n")

if (n_f > 0) {
  cat("\u274C FAIL:\n")
  fi <- cleanse_log[cleanse_log$level == "FAIL",]
  for (i in seq_len(nrow(fi))) cat("  -", fi$check[i], ":", fi$detail[i], "\n")
}
if (n_w > 0) {
  cat("\u26A0\uFE0F  WARN:\n")
  wi <- cleanse_log[cleanse_log$level == "WARN",]
  for (i in seq_len(nrow(wi))) cat("  -", wi$check[i], ":", wi$detail[i], "\n")
}

if (n_f == 0) {
  cat("\n\u2705 Golden copy cleanse PASSED\n")
} else {
  cat("\n\u274C Golden copy cleanse FAILED\n")
}

message("\n\u2705 PBP Lineup + Golden Copy Builder complete.")
gc()


# ============================================================
# MINUTES VALIDATION: Golden Copy vs ESPN Box Score
# ============================================================
library(data.table)
library(hoopR)

message("=== MINUTES VALIDATION ===\n")

# 1) Load golden copy
golden <- fread("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/golden_lineup_data_2025_2026.csv")

# 2) Load ESPN box scores
message("Loading ESPN player box scores...")
box <- as.data.table(load_nba_player_box(seasons = 2026))
message("Box score rows: ", nrow(box))

# 3) Load name mapping for player ID lookup
name_map <- fread("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv")
player_id_lookup <- unique(name_map[!is.na(clean_player_name) & clean_player_name != "-" &
                                      !is.na(espn_player_id) & espn_player_id != "",
                                    .(clean_player_name, espn_player_id = as.integer(espn_player_id))])
player_id_lookup <- player_id_lookup[, .SD[1], by = clean_player_name]

# 4) Helper
mmss_to_sec <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) {
    if (length(p) != 2) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2])
  }, numeric(1))
}

# 5) Pivot golden copy P1-P5 to long format and compute stint minutes
golden[, stint_sec := mmss_to_sec(Time_On) - mmss_to_sec(Time_Off)]
golden[, row_id := .I]

golden_long <- melt(golden, 
                    id.vars = c("row_id", "game_id", "espn_game_id", "Team", "Period", "stint_sec"),
                    measure.vars = c("P1", "P2", "P3", "P4", "P5"),
                    variable.name = "slot", value.name = "player_name")

# Drop rows where stint_sec is 0 or NA
golden_long <- golden_long[!is.na(stint_sec) & stint_sec > 0]

# 6) Join player IDs
golden_long <- merge(golden_long, player_id_lookup, by.x = "player_name", by.y = "clean_player_name", all.x = TRUE)

n_no_id <- sum(is.na(golden_long$espn_player_id))
message("Player name -> ID match: ", nrow(golden_long) - n_no_id, " matched | ", n_no_id, " unmatched")

# Show unmatched players
if (n_no_id > 0) {
  unmatched <- unique(golden_long[is.na(espn_player_id), .(player_name)])
  message("Unmatched players (sample): ", paste(head(unmatched$player_name, 10), collapse = ", "))
}

# 7) Aggregate golden copy minutes per player per game
golden_mins <- golden_long[!is.na(espn_player_id), 
                           .(golden_total_min = round(sum(stint_sec, na.rm = TRUE) / 60, 2)),
                           by = .(espn_game_id, espn_player_id)]

# Also get per-quarter breakdown
golden_q_mins <- golden_long[!is.na(espn_player_id),
                             .(q_min = round(sum(stint_sec, na.rm = TRUE) / 60, 2)),
                             by = .(espn_game_id, espn_player_id, Period)]
golden_q_wide <- dcast(golden_q_mins, espn_game_id + espn_player_id ~ Period, value.var = "q_min", fill = 0)

# 8) Prep box score data
box_mins <- box[!is.na(minutes) & minutes > 0, 
                .(espn_game_id = as.character(game_id), 
                  espn_player_id = as.integer(athlete_id),
                  player_name = athlete_display_name,
                  team = team_short_display_name,
                  box_total_min = as.numeric(minutes))]
box_mins <- unique(box_mins)

message("Box score player-games: ", nrow(box_mins))
message("Golden copy player-games: ", nrow(golden_mins))

# 9) Join golden copy minutes to box score minutes
golden_mins[, espn_game_id := as.character(espn_game_id)]
box_mins[, espn_game_id := as.character(espn_game_id)]
compare <- merge(box_mins, golden_mins, by = c("espn_game_id", "espn_player_id"), all = TRUE)

# Calculate coverage
compare[, diff_min := golden_total_min - box_total_min]
compare[, pct_coverage := round(golden_total_min / box_total_min * 100, 1)]

# 10) Summary stats
cat("\n========================================================\n")
cat("  MINUTES VALIDATION SUMMARY\n")
cat("========================================================\n\n")

# Players in box score but not golden copy (completely missing)
missing_from_golden <- compare[is.na(golden_total_min) & !is.na(box_total_min)]
cat("Players in box score but MISSING from golden copy:", nrow(missing_from_golden), "\n")
if (nrow(missing_from_golden) > 0) {
  cat("  Sample:", paste(head(missing_from_golden$player_name, 5), collapse = ", "), "\n")
}

# Players in golden copy but not box score
extra_in_golden <- compare[!is.na(golden_total_min) & is.na(box_total_min)]
cat("Players in golden copy but NOT in box score:", nrow(extra_in_golden), "\n\n")

# For matched players, how close are the minutes?
matched <- compare[!is.na(golden_total_min) & !is.na(box_total_min)]
cat("Matched player-games:", nrow(matched), "\n\n")

cat("--- Coverage Distribution ---\n")
cat("Within 5% (95-105%):", sum(matched$pct_coverage >= 95 & matched$pct_coverage <= 105, na.rm = TRUE), 
    "(", round(mean(matched$pct_coverage >= 95 & matched$pct_coverage <= 105, na.rm = TRUE) * 100, 1), "%)\n")
cat("Within 10% (90-110%):", sum(matched$pct_coverage >= 90 & matched$pct_coverage <= 110, na.rm = TRUE),
    "(", round(mean(matched$pct_coverage >= 90 & matched$pct_coverage <= 110, na.rm = TRUE) * 100, 1), "%)\n")
cat("Within 20% (80-120%):", sum(matched$pct_coverage >= 80 & matched$pct_coverage <= 120, na.rm = TRUE),
    "(", round(mean(matched$pct_coverage >= 80 & matched$pct_coverage <= 120, na.rm = TRUE) * 100, 1), "%)\n")
cat("Over 120%:", sum(matched$pct_coverage > 120, na.rm = TRUE), "\n")
cat("Under 80%:", sum(matched$pct_coverage < 80, na.rm = TRUE), "\n\n")

cat("Mean coverage %:", round(mean(matched$pct_coverage, na.rm = TRUE), 1), "%\n")
cat("Median coverage %:", round(median(matched$pct_coverage, na.rm = TRUE), 1), "%\n")
cat("Mean absolute diff:", round(mean(abs(matched$diff_min), na.rm = TRUE), 2), "min\n")
cat("Median absolute diff:", round(median(abs(matched$diff_min), na.rm = TRUE), 2), "min\n\n")

# Worst over-counts (golden > box by most)
cat("--- Worst Over-Counts (golden >> box) ---\n")
worst_over <- matched[order(-diff_min)][1:10]
for (i in seq_len(nrow(worst_over))) {
  cat("  ", worst_over$player_name[i], "| game:", worst_over$espn_game_id[i],
      "| box:", worst_over$box_total_min[i], "| golden:", worst_over$golden_total_min[i],
      "| diff:", round(worst_over$diff_min[i], 1), "\n")
}

cat("\n--- Worst Under-Counts (golden << box) ---\n")
worst_under <- matched[order(diff_min)][1:10]
for (i in seq_len(nrow(worst_under))) {
  cat("  ", worst_under$player_name[i], "| game:", worst_under$espn_game_id[i],
      "| box:", worst_under$box_total_min[i], "| golden:", worst_under$golden_total_min[i],
      "| diff:", round(worst_under$diff_min[i], 1), "\n")
}

# 11) Save comparison for further analysis
compare_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/minutes_validation_2025_2026.csv"
fwrite(compare, compare_path)
message("\nSaved validation file: ", compare_path)