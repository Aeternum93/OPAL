# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Injury Data Aggregation Script (nbainjuries via reticulate) ====
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


library(data.table)
library(dplyr)
library(reticulate)


# ============================================================
# 1. PYTHON SETUP
# ============================================================
# Use whatever Python reticulate already initialized
# (avoids conflicts if another script already set it)
if (!reticulate::py_module_available("nbainjuries")) {
  reticulate::py_install("nbainjuries", pip = TRUE)
}

nbainjuries <- import("nbainjuries")
py_datetime <- import("datetime")
injury_mod  <- nbainjuries$injury


# ============================================================
# 2. PATHS
# ============================================================
base_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)"

injury_path <- paste0(
  base_dir,
  "/9. Historical Injuries (RapidAPI)/Injury_Database_",
  season_token,
  ".csv"
)

hist_odds_path <- paste0(
  base_dir,
  "/8. Historical Odds (Odds API)/nba_historical_odds_",
  season_token,
  ".csv"
)

dir.create(dirname(injury_path), recursive = TRUE, showWarnings = FALSE)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Load Historical Odds to get Comparison Dates ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

if (!file.exists(hist_odds_path)) {
  stop("Historical odds file not found at: ", hist_odds_path)
}

nba_historical_odds <- fread(
  file       = hist_odds_path,
  colClasses = "character",
  encoding   = "UTF-8"
)

if (!"commence_date_est" %in% names(nba_historical_odds)) {
  stop("Historical odds file is missing `commence_date_est` column.")
}

# Normalize commence_date_est to YYYY-MM-DD
nba_historical_odds[, commence_date_est := trimws(commence_date_est)]
nba_historical_odds[commence_date_est == "", commence_date_est := NA_character_]

# Handle MM/DD/YYYY format
mask_mdy <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", nba_historical_odds$commence_date_est)
if (any(mask_mdy, na.rm = TRUE)) {
  nba_historical_odds[mask_mdy, commence_date_est := format(
    as.Date(commence_date_est, format = "%m/%d/%Y"), "%Y-%m-%d"
  )]
}

all_dates <- sort(unique(nba_historical_odds[!is.na(commence_date_est), commence_date_est]))
cat("Historical odds unique dates:", length(all_dates), "\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  Compare Injury DB vs Historical Odds to find MISSING DATES ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

if (!file.exists(injury_path) || file.info(injury_path)$size == 0) {
  
  cat("No existing Injury_Database file found. Full backfill.\n")
  MISSING_DATES <- all_dates
  
} else {
  
  injury_db_check <- fread(injury_path, colClasses = "character", encoding = "UTF-8")
  
  if (!"pulled_date" %in% names(injury_db_check)) {
    stop("Injury database is missing `pulled_date` column.")
  }
  
  # Normalize pulled_date
  injury_db_check[, pulled_date := trimws(pulled_date)]
  injury_db_check[pulled_date == "", pulled_date := NA_character_]
  mask_mdy2 <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", injury_db_check$pulled_date)
  if (any(mask_mdy2, na.rm = TRUE)) {
    injury_db_check[mask_mdy2, pulled_date := format(
      as.Date(pulled_date, format = "%m/%d/%Y"), "%Y-%m-%d"
    )]
  }
  
  pulled_dates <- sort(unique(injury_db_check[!is.na(pulled_date), pulled_date]))
  cat("Injury DB pulled dates:", length(pulled_dates), "\n")
  
  MISSING_DATES <- setdiff(all_dates, pulled_dates)
  rm(injury_db_check)
}

MISSING_DATES <- sort(MISSING_DATES)
cat("Missing dates to pull injuries for:", length(MISSING_DATES), "\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Pull Injuries for Missing Dates (nbainjuries) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================================
# 3. FUNCTION: Pull injuries for one date via nbainjuries
# ============================================================
get_injuries_for_date <- function(dt_str) {
  dt_str <- as.character(dt_str)
  parts <- as.integer(strsplit(dt_str, "-")[[1]])
  snapshot_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  
  result <- tryCatch({
    # Run entirely in Python to avoid reticulate type conversion issues
    py_run_string(sprintf(
      "from nbainjuries import injury
from datetime import datetime
_inj_result = injury.get_reportdata(datetime(%d, %d, %d, 17, 0), return_df=True)",
      parts[1], parts[2], parts[3]
    ))
    
    df_r <- as.data.frame(py$`_inj_result`)
    
    if (nrow(df_r) == 0) {
      return(data.table(
        game_date = character(), game_time = character(),
        matchup = character(), team = character(),
        player = character(), status = character(),
        reason = character(), pulled_date = dt_str,
        snapshot_datetime_utc = snapshot_utc
      ))
    }
    
    colnames(df_r) <- tolower(gsub("\\s+", "_", trimws(colnames(df_r))))
    
    if ("player_name"    %in% names(df_r)) names(df_r)[names(df_r) == "player_name"]    <- "player"
    if ("current_status" %in% names(df_r)) names(df_r)[names(df_r) == "current_status"] <- "status"
    
    df_r$pulled_date           <- dt_str
    df_r$snapshot_datetime_utc <- snapshot_utc
    
    as.data.table(df_r)
    
  }, error = function(e) {
    message("  Error pulling date: ", dt_str, " -- ", conditionMessage(e))
    data.table(
      game_date = character(), game_time = character(),
      matchup = character(), team = character(),
      player = character(), status = character(),
      reason = character(), pulled_date = dt_str,
      snapshot_datetime_utc = snapshot_utc
    )
  })
  
  return(result)
}


# ============================================================
# 4. LOOP: Pull all missing dates with progress logging
# ============================================================

total_dates <- length(MISSING_DATES)

if (total_dates == 0) {
  message("No missing dates to pull. injuries_all will be empty.")
  injuries_all <- data.table()
} else {
  
  injuries_list <- vector("list", total_dates)
  
  for (i in seq_along(MISSING_DATES)) {
    dt <- MISSING_DATES[i]
    remaining <- total_dates - i
    
    message(sprintf(
      "[%d/%d] Pulling injuries for %s  |  Remaining: %d dates",
      i, total_dates, dt, remaining
    ))
    
    injuries_list[[i]] <- get_injuries_for_date(dt)
    
    # Small delay to be respectful to NBA servers
    Sys.sleep(0.5)
  }
  
  injuries_all <- rbindlist(injuries_list, use.names = TRUE, fill = TRUE)
}


# ============================================================
# 5. PLAYER NAME CORRECTIONS
# ============================================================
if (nrow(injuries_all) > 0 && "player" %in% names(injuries_all)) {
  
  # nbainjuries returns "Last, First" format -- convert to "First Last"
  has_comma <- grepl(",", injuries_all$player)
  if (any(has_comma)) {
    split_names <- strsplit(injuries_all$player[has_comma], ",\\s*")
    injuries_all$player[has_comma] <- vapply(split_names, function(x) {
      if (length(x) == 2) paste(x[2], x[1]) else paste(x, collapse = " ")
    }, character(1))
  }
  
  # Known name fixes
  injuries_all[, player := fcase(
    grepl("^Karl\\s*-\\s*Towns$",          player, ignore.case = TRUE), "Karl-Anthony Towns",
    grepl("^Eli\\s+Ndiaye$",              player, ignore.case = TRUE), "Eli John Ndiaye",
    grepl("^Yanic\\s+Niederhauser$",      player, ignore.case = TRUE), "Yanic Konan Niederhauser",
    grepl("^Olivier\\s*-\\s*Prosper$",    player, ignore.case = TRUE), "Olivier-Maxence Prosper",
    grepl("^Shai\\s+Alexander$",          player, ignore.case = TRUE), "Shai Gilgeous-Alexander",
    rep(TRUE, .N), player
  )]
}

message("Pulled ", nrow(injuries_all), " total injury rows for ", total_dates, " dates.")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Pull Injuries for Missing Dates ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Append pulled injuries to Injury_Database ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================================
# 6. SAVE / APPEND: Injury_Database_<season_token>.csv
# ============================================================

new_dt <- copy(injuries_all)

# Force all columns to character
for (nm in names(new_dt)) {
  set(new_dt, j = nm, value = as.character(new_dt[[nm]]))
}

required_cols <- c(
  "game_date","game_time","matchup","team","player","status","reason",
  "pulled_date","snapshot_datetime_utc"
)
for (col in required_cols) {
  if (!col %in% names(new_dt)) new_dt[, (col) := ""]
}

# -----------------------------
# Append + dedupe
# -----------------------------
if (file.exists(injury_path) && file.info(injury_path)$size > 0) {
  
  old_dt <- fread(injury_path, colClasses = "character", encoding = "UTF-8")
  
  # Align columns
  all_cols <- union(names(old_dt), names(new_dt))
  for (col in setdiff(all_cols, names(old_dt))) old_dt[, (col) := ""]
  for (col in setdiff(all_cols, names(new_dt))) new_dt[, (col) := ""]
  setcolorder(old_dt, all_cols)
  setcolorder(new_dt, all_cols)
  
  combined_dt <- rbindlist(list(old_dt, new_dt), use.names = TRUE, fill = TRUE)
  
  # Dedupe
  dedupe_key <- c("pulled_date","team","player","status","reason")
  dedupe_key <- dedupe_key[dedupe_key %in% names(combined_dt)]
  
  if (length(dedupe_key) > 0) {
    combined_dt <- unique(combined_dt, by = dedupe_key)
  } else {
    combined_dt <- unique(combined_dt)
  }
  
} else {
  combined_dt <- new_dt
}

# Save
fwrite(combined_dt, injury_path, quote = TRUE, na = "", sep = ",")

message("Saved Injury_Database")
message("This run rows: ", nrow(new_dt), " | Total rows now: ", nrow(combined_dt))


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Append pulled injuries to Injury_Database ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Patch espn_player_id + espn_team_id INTO Injury_Database ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

name_map_path <- paste0(base_dir, "/6. Data Cleanup/hoopr_name_mapping.csv")
team_map_path <- paste0(base_dir, "/6. Data Cleanup/team_id_mapping.csv")

# ------------------------------------------------------------
# Load hoopR name mapping (for espn_player_id patch)
# ------------------------------------------------------------
hoopr_name_map <- fread(
  file       = name_map_path,
  colClasses = "character",
  encoding   = "UTF-8"
) %>%
  select(nba_player_name, clean_player_name, nba_player_id, espn_player_id) %>%
  mutate(
    clean_key = toupper(trimws(gsub("[^A-Za-z ]", "", clean_player_name))),
    nba_key   = toupper(trimws(gsub("[^A-Za-z ]", "", nba_player_name)))
  )

# Build lookup vectors
lookup_clean <- setNames(hoopr_name_map$espn_player_id, hoopr_name_map$clean_key)
lookup_nba   <- setNames(hoopr_name_map$espn_player_id, hoopr_name_map$nba_key)

# ------------------------------------------------------------
# Load team mapping (for espn_team_id patch)
# ------------------------------------------------------------
if (!file.exists(team_map_path)) {
  stop("team_id_mapping.csv not found at: ", team_map_path)
}

team_map <- fread(
  file       = team_map_path,
  colClasses = "character",
  encoding   = "UTF-8"
) %>%
  mutate(
    team_long_key = toupper(trimws(gsub("[^A-Za-z ]", "", team_long_name)))
  )

# ------------------------------------------------------------
# Load injury database for patching
# ------------------------------------------------------------
injury_db <- fread(injury_path, colClasses = "character", encoding = "UTF-8") %>%
  as.data.frame()

# ------------------------------------------------------------
# Manual player-name fixes
# ------------------------------------------------------------
if ("player" %in% names(injury_db)) {
  injury_db$player <- as.character(injury_db$player)
  injury_db$player <- ifelse(grepl("Karl-\\s*Towns", injury_db$player, ignore.case = TRUE),
                             "Karl-Anthony Towns", injury_db$player)
  injury_db$player <- ifelse(grepl("^Eli\\s+Ndiaye$", injury_db$player, ignore.case = TRUE),
                             "Eli John Ndiaye", injury_db$player)
  injury_db$player <- ifelse(grepl("^Yanic\\s+Niederhauser$", injury_db$player, ignore.case = TRUE),
                             "Yanic Konan Niederhauser", injury_db$player)
  injury_db$player <- ifelse(grepl("Olivier-\\s*Prosper", injury_db$player, ignore.case = TRUE),
                             "Olivier-Maxence Prosper", injury_db$player)
  injury_db$player <- ifelse(grepl("^Shai\\s+Alexander$", injury_db$player, ignore.case = TRUE),
                             "Shai Gilgeous-Alexander", injury_db$player)
}

# ------------------------------------------------------------
# Ensure espn_player_id column exists
# ------------------------------------------------------------
if (!"espn_player_id" %in% names(injury_db)) {
  injury_db$espn_player_id <- ""
}

# ------------------------------------------------------------
# Build cleaned player key + patch espn_player_id
# ------------------------------------------------------------
injury_db$player_clean <- toupper(trimws(gsub("[^A-Za-z ]", "", injury_db$player)))

# PASS 1: clean_player_name
idx_missing <- which(is.na(injury_db$espn_player_id) | injury_db$espn_player_id == "")
injury_db$espn_player_id[idx_missing] <- lookup_clean[injury_db$player_clean[idx_missing]]

# PASS 2: fallback nba_player_name
idx_still_missing <- which(is.na(injury_db$espn_player_id) | injury_db$espn_player_id == "")
if (length(idx_still_missing) > 0) {
  message("Fallback pass: matching remaining players using nba_player_name")
  injury_db$espn_player_id[idx_still_missing] <- lookup_nba[injury_db$player_clean[idx_still_missing]]
}

injury_db$espn_player_id[is.na(injury_db$espn_player_id)] <- ""

# ------------------------------------------------------------
# Patch TEAM IDs
# ------------------------------------------------------------
if ("team" %in% names(injury_db)) {
  
  injury_db$team <- as.character(injury_db$team)
  injury_db$team_long_key <- toupper(trimws(gsub("[^A-Za-z ]", "", injury_db$team)))
  
  idx_team <- match(injury_db$team_long_key, team_map$team_long_key)
  
  add_cols <- setdiff(names(team_map), "team_long_key")
  for (cc in add_cols) {
    if (!cc %in% names(injury_db)) {
      injury_db[[cc]] <- team_map[[cc]][idx_team]
    } else {
      injury_db[[paste0(cc, "_teammap")]] <- team_map[[cc]][idx_team]
    }
  }
  
  n_unmatched <- sum(is.na(idx_team))
  message("Team mapping unmatched rows: ", n_unmatched, " / ", nrow(injury_db))
}


# ============================================================
# PATCH TEAM_LOGO + HEADSHOT INTO Injury_Database
# ============================================================

base_stats_team_path <- paste0(
  base_dir, "/1. hoopR/1. BaseStats_Team/BaseStats_Team_", season_token, ".csv"
)
base_stats_player_path <- paste0(
  base_dir, "/1. hoopR/2. BaseStats_Player/BaseStats_Player_", season_token, ".csv"
)

# --- TEAM LOGO ---
if (file.exists(base_stats_team_path)) {
  team_dt <- fread(base_stats_team_path, select = c("espn_team_id", "team_logo"), colClasses = "character")
  team_dt <- unique(team_dt)
  team_lookup <- setNames(team_dt$team_logo, team_dt$espn_team_id)
  
  if (!"team_logo" %in% names(injury_db)) injury_db$team_logo <- ""
  idx_missing_logo <- which(is.na(injury_db$team_logo) | injury_db$team_logo == "")
  injury_db$team_logo[idx_missing_logo] <- team_lookup[injury_db$espn_team_id[idx_missing_logo]]
} else {
  message("BaseStats_Team file not found for team_logo patch.")
}

# --- PLAYER HEADSHOT ---
if (file.exists(base_stats_player_path)) {
  player_dt <- fread(base_stats_player_path, select = c("espn_player_id", "headshot"), colClasses = "character")
  player_dt <- unique(player_dt)
  player_lookup <- setNames(player_dt$headshot, player_dt$espn_player_id)
  
  if (!"headshot" %in% names(injury_db)) injury_db$headshot <- ""
  idx_missing_headshot <- which(is.na(injury_db$headshot) | injury_db$headshot == "")
  injury_db$headshot[idx_missing_headshot] <- player_lookup[injury_db$espn_player_id[idx_missing_headshot]]
} else {
  message("BaseStats_Player file not found for headshot patch.")
}

# Final cleanup
injury_db$team_logo[is.na(injury_db$team_logo)] <- ""
injury_db$headshot[is.na(injury_db$headshot)] <- ""

message("Patched team_logo + headshot into Injury_Database.")

# ------------------------------------------------------------
# Save back to disk
# ------------------------------------------------------------
fwrite(
  injury_db,
  injury_path,
  quote = TRUE,
  na    = "",
  sep   = ","
)

message("Saved Injury_Database with patched espn_player_id + team columns: ", injury_path)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Injury Data Aggregation Script ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀