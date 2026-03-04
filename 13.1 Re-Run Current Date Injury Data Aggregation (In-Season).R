# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Injury Data Aggregation Script ====
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


library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(purrr)


# ============================================================
# 1. RAPID API KEY
# ============================================================
api_key <- "2fc0c14f17msh807fad54364a128p1f9a5ajsn4d9fea0e9a3c"


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

dir.create(dirname(injury_path), recursive = TRUE, showWarnings = FALSE)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Pull Injury Data for current_date ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================================
# 3. FUNCTION: PULL INJURIES FOR ONE DATE
# ============================================================
get_injuries_for_date <- function(dt) {
  url <- paste0(
    "https://nba-injuries-reports.p.rapidapi.com/injuries/nba/",
    dt
  )
  
  resp <- GET(
    url,
    add_headers(
      "X-RapidAPI-Key"  = api_key,
      "X-RapidAPI-Host" = "nba-injuries-reports.p.rapidapi.com"
    )
  )
  
  snapshot_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  
  # If API fails, return empty tibble
  if (http_error(resp)) {
    message("Error pulling date: ", dt)
    return(tibble(
      date = character(), team = character(), player = character(),
      status = character(), reason = character(), reportTime = character(),
      pulled_date = dt, snapshot_datetime_utc = snapshot_utc
    ))
  }
  
  dat <- content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(simplifyDataFrame = TRUE)
  
  df <- as_tibble(dat)
  
  if (nrow(df) == 0) {
    df <- tibble(
      date = character(), team = character(), player = character(),
      status = character(), reason = character(), reportTime = character()
    )
  }
  
  df %>%
    mutate(
      pulled_date = dt,
      snapshot_datetime_utc = snapshot_utc
    )
}


# ============================================================
# 4. PULL INJURIES FOR current_date
# ============================================================
message("Pulling injury data for current_date: ", current_date)

injuries_all <- get_injuries_for_date(current_date)

# ------------------------------------------------------------
# Name corrections for known API weirdness
# ------------------------------------------------------------
injuries_all <- injuries_all %>%
  mutate(
    player = case_when(
      grepl("^Karl\\s*-\\s*Towns$", player, ignore.case = TRUE)       ~ "Karl-Anthony Towns",
      grepl("^Eli\\s+Ndiaye$", player, ignore.case = TRUE)            ~ "Eli John Ndiaye",
      grepl("^Yanic\\s+Niederhauser$", player, ignore.case = TRUE)    ~ "Yanic Konan Niederhauser",
      grepl("^Olivier\\s*-\\s*Prosper$", player, ignore.case = TRUE)  ~ "Olivier-Maxence Prosper",
      grepl("^Shai\\s+Alexander$", player, ignore.case = TRUE) ~ "Shai Gilgeous-Alexander",
      TRUE ~ player
    )
  )

message("Pulled ", nrow(injuries_all), " injury rows for ", current_date)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Pull Injury Data for current_date ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Append pulled injuries to Injury_Database ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ============================================================
# 5. SAVE / APPEND: Injury_Database_<season_token>.csv
# ============================================================

new_dt <- as.data.table(injuries_all)

# Force all columns to character
for (nm in names(new_dt)) {
  set(new_dt, j = nm, value = as.character(new_dt[[nm]]))
}

required_cols <- c(
  "date","team","player","status","reason",
  "reportTime","pulled_date","snapshot_datetime_utc"
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
  dedupe_key <- c("pulled_date","team","player","status","reason","reportTime")
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