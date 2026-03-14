# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== PBP LINEUP BUILDER (V3) ====
#
# PART 1: PBP Lineup Reconstruction
#   - PBP is the SOLE source of truth for lineup data
#   - Reads raw ESPN PBP (nbapbp), extracts substitutions
#   - Infers starters, walks subs, builds lineup segments
#   - Converts abbreviations via team_id_mapping.csv
#   - Output: golden_lineup_data_<season>.csv (direct output, no merge)
#
# PART 2: Validation & Enrichment
#   - Row-level + game-level quality checks
#   - Attach espn_game_id, player IDs, nba_game_id
#   - Data cleanse (S1-S6)
#
# PART 3: Minutes Validation vs ESPN Box Scores
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

abv_lookup <- unique(team_map[, .(nba_schedule_abv, nba_api_abv)])
abv_lookup <- abv_lookup[!is.na(nba_schedule_abv) & nba_schedule_abv != ""]

to_std_abv <- function(x) {
  x <- as.character(x)
  idx <- match(x, abv_lookup$nba_schedule_abv)
  ifelse(!is.na(idx), abv_lookup$nba_api_abv[idx], x)
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
pm_dir        <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine"
pbp_path      <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")
name_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"
golden_path   <- file.path(pm_dir, paste0("golden_lineup_data_", season_token, ".csv"))

# ------------------------------------------------------------
# 1) LOAD DATA
# ------------------------------------------------------------
message("Loading PBP data...")
pbp <- fread(pbp_path)
message("Loading name mapping...")
name_map <- fread(name_map_path)
message("PBP rows: ", nrow(pbp), " | Unique games: ", uniqueN(pbp$game_id))

# ------------------------------------------------------------
# 2) BUILD ESPN ID -> CLEAN NAME LOOKUP
# ------------------------------------------------------------
name_lookup <- unique(name_map[!is.na(espn_player_id) & espn_player_id != "" & clean_player_name != "-",
                               .(espn_player_id = as.integer(espn_player_id), clean_player_name)])
name_lookup <- name_lookup[, .SD[1], by = espn_player_id]
message("Name lookup: ", nrow(name_lookup), " mappings")

# ------------------------------------------------------------
# 3) EXTRACT SUBSTITUTION EVENTS
# ------------------------------------------------------------
subs <- pbp[type_text == "Substitution" & !is.na(athlete_id_1) & !is.na(athlete_id_2),
            .(game_id, period_number, clock_display_value,
              start_game_seconds_remaining,
              end_quarter_seconds_remaining,
              player_in_id  = as.integer(athlete_id_1),
              player_out_id = as.integer(athlete_id_2),
              team_id       = as.integer(team_id),
              home_team_id  = as.integer(home_team_id),
              away_team_id  = as.integer(away_team_id),
              home_team_abbrev, away_team_abbrev,
              game_date)]

# Strip decimal seconds
subs[, clock_display_value := sub("\\.\\d+$", "", clock_display_value)]
# Zero-pad single digit seconds: "0:8" -> "0:08"
subs[, clock_display_value := sub("^(\\d+):(\\d)$", "\\1:0\\2", clock_display_value)]

# Remove sub events with bad clock format
n_bad_clock <- nrow(subs[!grepl("^\\d+:\\d{2}$", clock_display_value)])
if (n_bad_clock > 0) message("Removing ", n_bad_clock, " sub events with bad clock format")
subs <- subs[grepl("^\\d+:\\d{2}$", clock_display_value)]

# Convert abbreviations
subs[, home_team_abbrev := to_std_abv(home_team_abbrev)]
subs[, away_team_abbrev := to_std_abv(away_team_abbrev)]

# Attach player names
subs <- merge(subs, name_lookup, by.x = "player_in_id",  by.y = "espn_player_id", all.x = TRUE)
setnames(subs, "clean_player_name", "player_in_name")
subs <- merge(subs, name_lookup, by.x = "player_out_id", by.y = "espn_player_id", all.x = TRUE)
setnames(subs, "clean_player_name", "player_out_name")

n_missing_in  <- sum(is.na(subs$player_in_name))
n_missing_out <- sum(is.na(subs$player_out_name))
if (n_missing_in  > 0) message("WARNING: ", n_missing_in,  " sub events missing player_in name")
if (n_missing_out > 0) message("WARNING: ", n_missing_out, " sub events missing player_out name")

setorder(subs, game_id, -start_game_seconds_remaining)

# ------------------------------------------------------------
# 4) INFER STARTERS
# ------------------------------------------------------------
q1_actions <- pbp[period_number == 1 & !is.na(athlete_id_1),
                  .(game_id, team_id = as.integer(team_id),
                    athlete_id = as.integer(athlete_id_1),
                    start_game_seconds_remaining, type_text)]
q1_actions <- merge(q1_actions, name_lookup, by.x = "athlete_id", by.y = "espn_player_id", all.x = TRUE)

infer_starters <- function(game_subs, game_actions, tid) {
  team_subs_q1     <- game_subs[team_id == tid & period_number == 1]
  subbed_out_first <- character(0)
  subbed_in        <- character(0)
  for (i in seq_len(nrow(team_subs_q1))) {
    p_in  <- team_subs_q1$player_in_name[i]
    p_out <- team_subs_q1$player_out_name[i]
    if (!is.na(p_out) && !p_out %in% subbed_in)
      subbed_out_first <- unique(c(subbed_out_first, p_out))
    if (!is.na(p_in))
      subbed_in <- unique(c(subbed_in, p_in))
  }
  first_sub_time <- if (nrow(team_subs_q1) > 0) max(team_subs_q1$start_game_seconds_remaining, na.rm = TRUE) else 0
  early_actors   <- game_actions[team_id == tid & start_game_seconds_remaining >= first_sub_time &
                                   !is.na(clean_player_name), unique(clean_player_name)]
  starters <- unique(c(subbed_out_first, setdiff(early_actors, subbed_in)))
  if (length(starters) > 5) starters <- starters[1:5]
  starters
}

# ------------------------------------------------------------
# 5) BUILD LINEUP SEGMENTS
# Logic:
#   - 5 players is the target; 4 is acceptable if ESPN data
#     corrupts the count mid-game
#   - Starters must be exactly 5 to begin (strict gate)
#   - Stints are created when active >= 4
#   - Safety trim keeps active at max 5
# ------------------------------------------------------------
build_pbp_lineups <- function(game_id_val) {
  game_subs <- subs[game_id == game_id_val]
  game_acts <- q1_actions[game_id == game_id_val]
  if (nrow(game_subs) == 0) return(data.table())
  
  home_tid    <- game_subs$home_team_id[1]
  away_tid    <- game_subs$away_team_id[1]
  home_abbrev <- game_subs$home_team_abbrev[1]
  away_abbrev <- game_subs$away_team_abbrev[1]
  gdate       <- game_subs$game_date[1]
  max_period  <- max(game_subs$period_number, na.rm = TRUE)
  
  # Skip All-Star / special games
  if (any(c(home_abbrev, away_abbrev) %in% c("STARS", "STRIPES", "WORLD"))) return(data.table())
  
  results <- list()
  
  for (tid in c(home_tid, away_tid)) {
    team_abbrev <- if (tid == home_tid) home_abbrev else away_abbrev
    opp_abbrev  <- if (tid == home_tid) away_abbrev else home_abbrev
    
    starters <- infer_starters(game_subs, game_acts, tid)
    # Strict gate: must have 5 starters to begin
    if (length(starters) < 5) {
      message("  SKIP: ", team_abbrev, " in game ", game_id_val,
              " — only ", length(starters), " starters inferred")
      next
    }
    
    # Get ALL subs for this team, sorted chronologically (earliest first)
    team_subs <- game_subs[team_id == tid][order(period_number, -end_quarter_seconds_remaining)]
    
    active         <- starters[1:5]
    last_clock     <- "12:00"
    last_period    <- 1L
    closed_periods <- integer(0)
    
    make_stint <- function(per, t_on, t_off, lineup) {
      lp <- sort(lineup)
      # Pad to 5 if active dropped to 4 due to ESPN data errors
      lp <- c(lp, rep(NA_character_, max(0, 5 - length(lp))))
      data.table(
        espn_game_id = game_id_val, game_date = as.character(gdate),
        Period       = paste0("Period ", per),
        Time_On      = t_on, Time_Off = t_off,
        Team         = team_abbrev, Opp = opp_abbrev,
        home_abbrev  = home_abbrev, away_abbrev = away_abbrev,
        P1 = lp[1], P2 = lp[2], P3 = lp[3], P4 = lp[4], P5 = lp[5]
      )
    }
    
    # --- WALK SUBSTITUTIONS ---
    for (s in seq_len(nrow(team_subs))) {
      sub_clock  <- team_subs$clock_display_value[s]
      sub_period <- team_subs$period_number[s]
      p_in       <- team_subs$player_in_name[s]
      p_out      <- team_subs$player_out_name[s]
      
      if (is.na(p_in) || is.na(p_out) || is.na(sub_clock) || is.na(sub_period)) next
      
      # Skip subs at period start time when we haven't moved yet —
      # these are carry-forward artifacts from ESPN data
      period_start <- if (sub_period >= 5) "5:00" else "12:00"
      if (sub_clock == period_start && sub_clock == last_clock) next
      
      # --- PERIOD CHANGE ---
      if (sub_period != last_period) {
        # Close previous period at 0:00
        if (length(active) >= 4 && !last_period %in% closed_periods && last_clock != "0:00") {
          results[[length(results) + 1]] <- make_stint(last_period, last_clock, "0:00", active)
          closed_periods <- c(closed_periods, last_period)
        }
        # Fill any skipped periods entirely with the carried lineup
        if (sub_period > last_period + 1 && length(active) >= 4) {
          for (skip_p in (last_period + 1):(sub_period - 1)) {
            if (!skip_p %in% closed_periods) {
              skip_start <- if (skip_p >= 5) "5:00" else "12:00"
              results[[length(results) + 1]] <- make_stint(skip_p, skip_start, "0:00", active)
              closed_periods <- c(closed_periods, skip_p)
            }
          }
        }
        # Start new period
        last_clock  <- if (sub_period >= 5) "5:00" else "12:00"
        last_period <- sub_period
        
        # Skip this sub if it's at the new period start — carry-forward artifact
        period_start <- if (sub_period >= 5) "5:00" else "12:00"
        if (sub_clock == period_start) next
      }
      
      # --- CREATE STINT from last event to this sub ---
      # Accept 4 players as fallback if ESPN data corrupted the count
      if (length(active) >= 4 && last_clock != sub_clock) {
        results[[length(results) + 1]] <- make_stint(last_period, last_clock, sub_clock, active)
      }
      
      # --- APPLY SUBSTITUTION ---
      if (p_out %in% active) {
        active <- setdiff(active, p_out)
      } else {
        # p_out not on court — remove last player to make room (ESPN data error)
        # Only trim if at 5; never go below 4
        if (length(active) >= 5) active <- active[-length(active)]
      }
      active <- unique(c(active, p_in))
      # Safety: trim back to 5 if somehow over
      if (length(active) > 5) active <- tail(active, 5)
      
      last_clock <- sub_clock
    } # ← end for s (substitution walk)
    
    # --- CLOSE LAST PERIOD ---
    if (length(active) >= 4 && !last_period %in% closed_periods && last_clock != "0:00") {
      results[[length(results) + 1]] <- make_stint(last_period, last_clock, "0:00", active)
      closed_periods <- c(closed_periods, last_period)
    }
    
    # --- OT CARRY-FORWARD ---
    if (length(active) >= 4 && last_period < max_period) {
      for (p in (last_period + 1):max_period) {
        if (!p %in% closed_periods) {
          ot_start <- if (p >= 5) "5:00" else "12:00"
          results[[length(results) + 1]] <- make_stint(p, ot_start, "0:00", active)
          closed_periods <- c(closed_periods, p)
        }
      }
    }
    
  } # ← end for tid (team loop)
  
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
n_skipped   <- 0

for (gid in all_espn_games) {
  result <- tryCatch(build_pbp_lineups(gid), error = function(e) {
    message("  ERROR on game ", gid, ": ", e$message)
    data.table()
  })
  if (nrow(result) > 0) {
    all_lineups[[length(all_lineups) + 1]] <- result
    n_processed <- n_processed + 1
  } else {
    n_skipped <- n_skipped + 1
  }
  if ((n_processed + n_skipped) %% 100 == 0)
    message("  Progress: ", n_processed + n_skipped, " / ", length(all_espn_games))
}

golden_lineup <- rbindlist(all_lineups, use.names = TRUE, fill = TRUE)
message("\nPBP reconstruction: ", n_processed, " games | ", n_skipped, " skipped | ", nrow(golden_lineup), " rows")

# ------------------------------------------------------------
# 7) FORMAT OUTPUT
# ------------------------------------------------------------
golden_lineup[, game_date_id := gsub("-", "", substr(game_date, 1, 10))]
golden_lineup[, game_code    := paste0(away_abbrev, home_abbrev)]
golden_lineup[, game_id      := paste0(game_code, "_", game_date_id)]
golden_lineup[, Team_Long    := NA_character_]
golden_lineup[, P_M          := NA_character_]

# Fix Time_Off > Time_On (inverted stints)
golden_lineup[, ton  := mmss_to_sec(Time_On)]
golden_lineup[, toff := mmss_to_sec(Time_Off)]
golden_lineup[toff > ton, Time_Off := "0:00"]
golden_lineup[, c("ton", "toff") := NULL]

# Fix single-digit seconds: "6:7" -> "6:07"
golden_lineup[, Time_On  := sub("^(\\d+):(\\d)$", "\\1:0\\2", Time_On)]
golden_lineup[, Time_Off := sub("^(\\d+):(\\d)$", "\\1:0\\2", Time_Off)]

# Remove temp columns, keep clean schema
golden_lineup[, c("game_date", "home_abbrev", "away_abbrev") := NULL]
golden_lineup <- unique(golden_lineup)

message("Formatted: ", nrow(golden_lineup), " rows | ", uniqueN(golden_lineup$game_id), " games")


# ████████████████████████████████████████████████████████████
# ██  PART 2: VALIDATION & ENRICHMENT                      ██
# ████████████████████████████████████████████████████████████

message("\n========================================================")
message("  VALIDATION & ENRICHMENT")
message("========================================================\n")

# --- Overlap dedup: remove overlapping stints within a period ---
golden_lineup[, ton_sec   := mmss_to_sec(Time_On)]
golden_lineup[, toff_sec  := mmss_to_sec(Time_Off)]
golden_lineup[, stint_dur := ton_sec - toff_sec]

period_cov <- golden_lineup[, .(total_cov = sum(stint_dur, na.rm = TRUE)), by = .(game_id, Team, Period)]
period_cov[, period_num := as.integer(gsub("Period ", "", Period))]
period_cov[, period_max := fifelse(period_num >= 5, 300, 720)]
overlap_periods <- period_cov[total_cov > period_max * 1.1]

if (nrow(overlap_periods) > 0) {
  message("Deduplicating ", nrow(overlap_periods), " periods with overlapping stints")
  for (r in seq_len(nrow(overlap_periods))) {
    gid  <- overlap_periods$game_id[r]
    tm   <- overlap_periods$Team[r]
    per  <- overlap_periods$Period[r]
    idx  <- which(golden_lineup$game_id == gid & golden_lineup$Team == tm & golden_lineup$Period == per)
    rows <- golden_lineup[idx][order(-ton_sec, toff_sec)]
    keep <- rep(FALSE, nrow(rows))
    keep[1] <- TRUE
    last_end <- rows$toff_sec[1]
    for (j in 2:nrow(rows)) {
      if (!is.na(rows$ton_sec[j]) && !is.na(last_end) && rows$ton_sec[j] <= last_end) {
        keep[j]  <- TRUE
        last_end <- rows$toff_sec[j]
      }
    }
    drop_idx <- idx[!keep]
    if (length(drop_idx) > 0) golden_lineup <- golden_lineup[-drop_idx]
  }
}
golden_lineup[, c("ton_sec", "toff_sec", "stint_dur") := NULL]

# --- Enrichment: Player IDs ---
player_id_lookup <- unique(name_map[!is.na(clean_player_name) & clean_player_name != "-" &
                                      !is.na(espn_player_id) & espn_player_id != "",
                                    .(clean_player_name,
                                      espn_player_id = as.character(espn_player_id),
                                      nba_player_id  = as.character(nba_player_id))])
player_id_lookup <- player_id_lookup[, .SD[1], by = clean_player_name]

for (p in 1:5) {
  pcol     <- paste0("P", p)
  espn_col <- paste0("P", p, "_espn_id")
  nba_col  <- paste0("P", p, "_nba_id")
  golden_lineup <- merge(golden_lineup,
                         player_id_lookup[, .(clean_player_name, espn_pid = espn_player_id, nba_pid = nba_player_id)],
                         by.x = pcol, by.y = "clean_player_name", all.x = TRUE)
  setnames(golden_lineup, "espn_pid", espn_col)
  setnames(golden_lineup, "nba_pid",  nba_col)
}
message("Player ID coverage: ", sum(!is.na(golden_lineup$P1_espn_id)), " / ", nrow(golden_lineup), " rows have P1 espn_id")

# --- Enrichment: nba_game_id from BaseStats_Team ---
bs_team_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. BaseStats_Team/BaseStats_Team_MC_", season_token, ".csv")
if (file.exists(bs_team_path)) {
  bs_team <- fread(bs_team_path, select = c("ESPN_GAME_ID", "NBA_GAME_ID"))
  bs_team[, ESPN_GAME_ID := as.character(ESPN_GAME_ID)]
  bs_team <- unique(bs_team)
  golden_lineup[, espn_game_id := as.character(espn_game_id)]
  golden_lineup[bs_team, nba_game_id := i.NBA_GAME_ID, on = c("espn_game_id" = "ESPN_GAME_ID")]
  message("nba_game_id coverage: ", sum(!is.na(golden_lineup$nba_game_id)), " / ", nrow(golden_lineup))
} else {
  message("WARNING: BaseStats_Team not found")
  golden_lineup[, nba_game_id := NA_character_]
}

# --- Save golden lineup ---
fwrite(golden_lineup, golden_path)
message("\nSaved: ", golden_path)
message("Rows: ", nrow(golden_lineup), " | Games: ", uniqueN(golden_lineup$game_id))


# ████████████████████████████████████████████████████████████
# ██  DATA CLEANSE (S1-S6)                                 ██
# ████████████████████████████████████████████████████████████

lineup_check <- copy(golden_lineup)
cleanse_log  <- data.frame(section = character(), check = character(), level = character(), detail = character(), stringsAsFactors = FALSE)

log_check <- function(section, check, level, detail) {
  cleanse_log <<- rbind(cleanse_log, data.frame(section = section, check = check, level = level, detail = as.character(detail), stringsAsFactors = FALSE))
  sym <- switch(level, INFO = "\u2139\uFE0F ", WARN = "\u26A0\uFE0F ", FAIL = "\u274C ")
  cat(sym, "[", section, "]", check, "\u2014", detail, "\n")
}
fmt_pct <- function(x) paste0(round(x * 100, 1), "%")
mmss_v  <- function(x) {
  parts <- strsplit(as.character(x), ":")
  vapply(parts, function(p) { if (length(p) != 2) return(NA_real_); as.numeric(p[1]) * 60 + as.numeric(p[2]) }, numeric(1))
}

cat("\n========================================================\n")
cat("  GOLDEN LINEUP DATA CLEANSE\n")
cat("========================================================\n\n")
n_new <- nrow(lineup_check)

# S1: STRUCTURAL
cat("--- S1: STRUCTURAL INTEGRITY ---\n\n")
log_check("S1", "Row counts", "INFO", paste0("rows=", n_new, " | games=", uniqueN(lineup_check$game_id), " | teams=", uniqueN(lineup_check$Team)))
for (col in c("game_id","Team","Opp","Period","Time_On","Time_Off","P1","P2","P3","P4","P5")) {
  n_na <- sum(is.na(lineup_check[[col]]) | trimws(as.character(lineup_check[[col]])) == "")
  if (n_na > 0) log_check("S1", paste0("NA: ", col), "WARN", paste0(n_na, " NA/blank")) else log_check("S1", paste0("NA: ", col), "INFO", "0 NA/blank \u2713")
}
dup_count <- nrow(lineup_check) - nrow(unique(lineup_check[, .(game_id, Team, Period, Time_On, Time_Off, P1, P2, P3, P4, P5)]))
if (dup_count > 0) log_check("S1", "Duplicates", "FAIL", paste0(dup_count, " duplicates")) else log_check("S1", "Duplicates", "INFO", "No duplicates \u2713")

# S2: LINEUP VALIDITY
cat("\n--- S2: LINEUP VALIDITY ---\n\n")
player_cols <- c("P1","P2","P3","P4","P5")
lineup_check[, n_uniq := apply(.SD, 1, function(x) length(unique(na.omit(x)))), .SDcols = player_cols]
n_under4 <- sum(lineup_check$n_uniq < 4)  # flag rows with fewer than 4 unique players
n_4p     <- sum(lineup_check$n_uniq == 4) # informational: 4-player stints
if (n_under4 > 0) log_check("S2", "< 4 unique players", "FAIL", paste0(n_under4, " rows")) else log_check("S2", "Min 4 unique players", "INFO", paste0("All ", n_new, " rows \u2713"))
if (n_4p    > 0) log_check("S2", "4-player stints",    "WARN", paste0(n_4p, " rows (ESPN data gaps)"))
lineup_check[, n_uniq := NULL]

pgt   <- lineup_check[, { data.table(player = unique(na.omit(c(P1, P2, P3, P4, P5)))) }, by = .(game_id, Team)]
cross <- pgt[pgt, on = .(game_id, player), allow.cartesian = TRUE, nomatch = 0][Team != i.Team]
if (nrow(cross) > 0) log_check("S2", "Cross-team", "FAIL", paste0(uniqueN(cross$player), " players")) else log_check("S2", "Cross-team", "INFO", "None \u2713")

roster <- pgt[, .(n = uniqueN(player)), by = .(game_id, Team)]
log_check("S2", "Roster size", "INFO", paste0("Mean=", round(mean(roster$n), 1)))

# S3: TIMING
cat("\n--- S3: TIMING LOGIC ---\n\n")
lineup_check[, ton  := mmss_v(Time_On)]
lineup_check[, toff := mmss_v(Time_Off)]
n_inv <- sum(lineup_check$ton < lineup_check$toff, na.rm = TRUE)
if (n_inv > 0) log_check("S3", "Time_On >= Time_Off", "FAIL", paste0(n_inv, " inverted")) else log_check("S3", "Time_On >= Time_Off", "INFO", paste0("All valid \u2713"))

lineup_check[, is_ot := grepl("Period [5678]", Period)]
n_reg12 <- sum(!lineup_check$is_ot & lineup_check$ton > 720, na.rm = TRUE)
n_ot5   <- sum(lineup_check$is_ot  & lineup_check$ton > 300, na.rm = TRUE)
if (n_reg12 > 0) log_check("S3", "Reg > 12:00", "WARN", paste0(n_reg12)) else log_check("S3", "Reg bounds", "INFO", "All <= 12:00 \u2713")
if (n_ot5   > 0) log_check("S3", "OT > 5:00",   "WARN", paste0(n_ot5))   else log_check("S3", "OT bounds",  "INFO", "All <= 5:00 \u2713")

pc   <- lineup_check[, .(Q1 = any(Period == "Period 1"), Q2 = any(Period == "Period 2"),
                         Q3 = any(Period == "Period 3"), Q4 = any(Period == "Period 4")), by = .(game_id, Team)]
n_mq <- sum(!pc$Q1 | !pc$Q2 | !pc$Q3 | !pc$Q4)
if (n_mq > 0) log_check("S3", "Q1-Q4 coverage", "WARN", paste0(n_mq, " missing")) else log_check("S3", "Q1-Q4 coverage", "INFO", paste0("All ", nrow(pc), " \u2713"))

lineup_check[, dur := ton - toff]
gt <- lineup_check[, .(total_min = round(sum(dur, na.rm = TRUE) / 60, 1)), by = .(game_id, Team)]
log_check("S3", "Mean coverage", "INFO", paste0("Mean=", round(mean(gt$total_min), 1), " min"))
n_low <- sum(gt$total_min < 40)
if (n_low > 0) log_check("S3", "< 40 min", "WARN", paste0(n_low, " game/team combos"))
lineup_check[, c("ton","toff","is_ot","dur") := NULL]

# S4: TEAM CONSISTENCY
cat("\n--- S4: TEAM CONSISTENCY ---\n\n")
tpg    <- lineup_check[, .(n_teams = uniqueN(Team)), by = game_id]
n_not2 <- sum(tpg$n_teams != 2)
if (n_not2 > 0) log_check("S4", "2 teams/game", "FAIL", paste0(n_not2, " games")) else log_check("S4", "2 teams/game", "INFO", paste0("All ", nrow(tpg), " \u2713"))

n_self <- sum(lineup_check$Team == lineup_check$Opp, na.rm = TRUE)
if (n_self > 0) log_check("S4", "Team!=Opp", "FAIL", paste0(n_self)) else log_check("S4", "Team!=Opp", "INFO", "None \u2713")

all_t <- unique(c(lineup_check$Team, lineup_check$Opp))
unk   <- setdiff(all_t, VALID_TEAMS)
if (length(unk) > 0) log_check("S4", "Valid codes", "FAIL", paste(unk, collapse = ", ")) else log_check("S4", "Valid codes", "INFO", paste0("All ", length(all_t), " \u2713"))

# S5: COVERAGE
cat("\n--- S5: COVERAGE ---\n\n")
sp <- lineup_check[, .N, by = .(game_id, Team)]
log_check("S5", "Stints/game/team", "INFO", paste0("Mean=", round(mean(sp$N), 1)))
ap <- unique(unlist(lineup_check[, ..player_cols]))
ap <- ap[!is.na(ap) & trimws(ap) != ""]
log_check("S5", "Unique players", "INFO", paste0(length(ap)))

# S6: GOLDEN INTEGRITY
cat("\n--- S6: GOLDEN INTEGRITY ---\n\n")

periods_per <- lineup_check[, .(n_per = uniqueN(Period)), by = .(game_id, Team)]
n_u4 <- sum(periods_per$n_per < 4)
if (n_u4 > 0) log_check("S6", "Min 4 periods", "FAIL", paste0(n_u4)) else log_check("S6", "Min 4 periods", "INFO", paste0("All ", nrow(periods_per), " \u2713"))

reg_p <- lineup_check[Period %in% paste0("Period ", 1:4)]
ps    <- reg_p[, .(max_on = max(mmss_v(Time_On), na.rm = TRUE)), by = .(game_id, Team, Period)]
n_n12 <- sum(ps$max_on != 720, na.rm = TRUE)
if (n_n12 > 0) log_check("S6", "Reg start 12:00", "WARN", paste0(n_n12)) else log_check("S6", "Reg start 12:00", "INFO", "All \u2713")

pe   <- lineup_check[, .(min_off = min(mmss_v(Time_Off), na.rm = TRUE)), by = .(game_id, Team, Period)]
n_n0 <- sum(pe$min_off != 0 & is.finite(pe$min_off), na.rm = TRUE)
if (n_n0 > 0) log_check("S6", "Periods end 0:00", "WARN", paste0(n_n0)) else log_check("S6", "Periods end 0:00", "INFO", "All \u2713")

bad_on  <- sum(!grepl("^\\d+:\\d{2}$", lineup_check$Time_On),  na.rm = TRUE)
bad_off <- sum(!grepl("^\\d+:\\d{2}$", lineup_check$Time_Off), na.rm = TRUE)
if (bad_on > 0 | bad_off > 0) log_check("S6", "Time format", "FAIL", paste0("On:", bad_on, " Off:", bad_off)) else log_check("S6", "Time format", "INFO", "All mm:ss \u2713")

old_codes <- c("WSH","NY","GS","UTAH","NO","NOP","PHX","UTA","SA")
bad_tc    <- lineup_check[Team %in% old_codes | Opp %in% old_codes]
if (nrow(bad_tc) > 0) log_check("S6", "Old codes", "FAIL", paste0(nrow(bad_tc))) else log_check("S6", "Old codes", "INFO", "None \u2713")

lineup_check[, st := paste0(pmin(Team, Opp), pmax(Team, Opp))]
dm   <- lineup_check[, .(ng = uniqueN(game_id)), by = .(game_date_id, st)]
n_dm <- sum(dm$ng > 1)
if (n_dm > 0) log_check("S6", "Dup game_ids", "FAIL", paste0(n_dm)) else log_check("S6", "Dup game_ids", "INFO", "All \u2713")
lineup_check[, st := NULL]

# SUMMARY
cat("\n========================================================\n")
cat("  CLEANSE SUMMARY\n")
cat("========================================================\n\n")
n_i <- sum(cleanse_log$level == "INFO")
n_w <- sum(cleanse_log$level == "WARN")
n_f <- sum(cleanse_log$level == "FAIL")
cat("  INFO:", n_i, "| WARN:", n_w, "| FAIL:", n_f, "\n\n")
if (n_f > 0) { cat("FAIL:\n"); fi <- cleanse_log[cleanse_log$level == "FAIL",]; for (i in seq_len(nrow(fi))) cat("  -", fi$check[i], ":", fi$detail[i], "\n") }
if (n_w > 0) { cat("WARN:\n"); wi <- cleanse_log[cleanse_log$level == "WARN",]; for (i in seq_len(nrow(wi))) cat("  -", wi$check[i], ":", wi$detail[i], "\n") }
if (n_f == 0) cat("\n\u2705 Cleanse PASSED\n") else cat("\n\u274C Cleanse FAILED\n")


# ████████████████████████████████████████████████████████████
# ██  PART 3: MINUTES VALIDATION vs ESPN BOX SCORES         ██
# ████████████████████████████████████████████████████████████

message("\n========================================================")
message("  MINUTES VALIDATION")
message("========================================================\n")

library(hoopR)
box <- as.data.table(load_nba_player_box(seasons = 2026))

golden_v <- fread(golden_path)
golden_v[, stint_sec := mmss_to_sec(Time_On) - mmss_to_sec(Time_Off)]
golden_v[, row_id := .I]

golden_long <- melt(golden_v,
                    id.vars       = c("row_id","game_id","espn_game_id","Team","Period","stint_sec"),
                    measure.vars  = c("P1","P2","P3","P4","P5"),
                    variable.name = "slot", value.name = "player_name")
golden_long <- golden_long[!is.na(stint_sec) & stint_sec > 0 & !is.na(player_name)]

pid_lookup <- unique(name_map[!is.na(clean_player_name) & clean_player_name != "-" &
                                !is.na(espn_player_id) & espn_player_id != "",
                              .(clean_player_name, espn_player_id = as.integer(espn_player_id))])
pid_lookup  <- pid_lookup[, .SD[1], by = clean_player_name]
golden_long <- merge(golden_long, pid_lookup, by.x = "player_name", by.y = "clean_player_name", all.x = TRUE)

golden_mins <- golden_long[!is.na(espn_player_id),
                           .(golden_min = round(sum(stint_sec) / 60, 2)),
                           by = .(espn_game_id, espn_player_id)]

box_mins <- box[!is.na(minutes) & minutes > 0,
                .(espn_game_id   = as.character(game_id),
                  espn_player_id = as.integer(athlete_id),
                  player_name    = athlete_display_name,
                  team           = team_short_display_name,
                  box_min        = as.numeric(minutes))]
box_mins <- unique(box_mins)

golden_mins[, espn_game_id := as.character(espn_game_id)]
compare <- merge(box_mins, golden_mins, by = c("espn_game_id","espn_player_id"), all = TRUE)
compare[, diff := golden_min - box_min]
compare[, pct  := round(golden_min / box_min * 100, 1)]

matched <- compare[!is.na(golden_min) & !is.na(box_min)]

cat("\n--- MINUTES VALIDATION SUMMARY ---\n")
cat("Matched player-games:", nrow(matched), "\n")
cat("Within 5%:",  sum(matched$pct >= 95  & matched$pct <= 105, na.rm = TRUE), "(", round(mean(matched$pct >= 95  & matched$pct <= 105, na.rm = TRUE) * 100, 1), "%)\n")
cat("Within 10%:", sum(matched$pct >= 90  & matched$pct <= 110, na.rm = TRUE), "(", round(mean(matched$pct >= 90  & matched$pct <= 110, na.rm = TRUE) * 100, 1), "%)\n")
cat("Within 20%:", sum(matched$pct >= 80  & matched$pct <= 120, na.rm = TRUE), "(", round(mean(matched$pct >= 80  & matched$pct <= 120, na.rm = TRUE) * 100, 1), "%)\n")
cat("Over 120%:",  sum(matched$pct > 120, na.rm = TRUE), "\n")
cat("Under 80%:",  sum(matched$pct < 80,  na.rm = TRUE), "\n")
cat("Mean coverage:",   round(mean(matched$pct,   na.rm = TRUE), 1), "%\n")
cat("Median coverage:", round(median(matched$pct, na.rm = TRUE), 1), "%\n")
cat("Mean abs diff:",   round(mean(abs(matched$diff), na.rm = TRUE), 2), "min\n\n")

cat("--- SPOT CHECK: Jalen Brunson game 401810581 ---\n")
brunson_check <- golden_v[espn_game_id == "401810581", {
  tmp <- .SD[apply(.SD[, .(P1,P2,P3,P4,P5)], 1, function(x) "Jalen Brunson" %in% x)]
  tmp[, stint_min := (mmss_to_sec(Time_On) - mmss_to_sec(Time_Off)) / 60]
  list(total_min = sum(tmp$stint_min), by_period = list(tmp[, .(mins = round(sum(stint_min), 2)), by = Period]))
}]
cat("Brunson total minutes:", round(brunson_check$total_min, 2), "(box score: 47)\n")
cat("By period:\n")
print(brunson_check$by_period[[1]])

fwrite(compare, file.path(pm_dir, paste0("minutes_validation_", season_token, ".csv")))
message("\nSaved minutes validation")

message("\n\u2705 PBP Lineup Builder V3 complete.")
gc()


cat("\n--- SPOT CHECK: WAS/MIA game 401810793 ---\n")
golden_v[espn_game_id == "401810793" & Team == "WAS",
         .(Period, Time_On, Time_Off, P1, P2, P3, P4, P5)
][order(Period, -mmss_to_sec(Time_On))]

cat("\n--- WAS Period 2 subs in PBP ---\n")
pbp[game_id == 401810793 & type_text == "Substitution" & period_number == 2,
    .(clock_display_value, text, team_id)][order(-clock_display_value)]

cat("\n--- WAS Period 1 detail ---\n")
golden_v[espn_game_id == "401810793" & Team == "WAS" & Period == "Period 1",
         .(Time_On, Time_Off, P1, P2, P3, P4, P5)][order(-mmss_to_sec(Time_On))]




pbp[game_id == 401810793 & type_text == "Substitution" & 
      period_number == 3 & team_id == 27,
    .(clock_display_value, text)][order(-clock_display_value)]


if (sub_period == 3 && last_period == 2 && team_abbrev == "WAS") {
  message("  Entering P3 | active=", paste(active, collapse=", "))
}