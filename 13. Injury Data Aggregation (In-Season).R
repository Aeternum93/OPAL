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


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Load NBA Historical Odds to get Comparison Dates (Injury DB vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(data.table)
library(dplyr)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
injury_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)/Injury_Database_",
  season_token,
  ".csv"
)

hist_odds_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_",
  season_token,
  ".csv"
)

# ------------------------------------------------------------
# Load historical odds and get unique game dates (AS CHARACTER)
# ------------------------------------------------------------
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

# Normalize commence_date_est to YYYY-MM-DD as character
nba_historical_odds <- nba_historical_odds %>%
  mutate(
    commence_date_est = trimws(commence_date_est),
    commence_date_est = ifelse(is.na(commence_date_est) | commence_date_est == "", NA_character_, commence_date_est),
    # if your file is sometimes MM/DD/YYYY, normalize it:
    commence_date_est = ifelse(
      grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", commence_date_est),
      format(as.Date(commence_date_est, format = "%m/%d/%Y"), "%Y-%m-%d"),
      commence_date_est
    ),
    # if your file is already YYYY-MM-DD, leave it
    commence_date_est = ifelse(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", commence_date_est),
      commence_date_est,
      commence_date_est
    )
  )

all_dates <- nba_historical_odds %>%
  filter(!is.na(commence_date_est)) %>%
  distinct(commence_date_est) %>%
  arrange(commence_date_est)

cat("Historical odds unique dates:", nrow(all_dates), "\n")

# ------------------------------------------------------------
# Load injury DB (if exists) and get pulled dates (AS CHARACTER)
# ------------------------------------------------------------
if (!file.exists(injury_path) || file.info(injury_path)$size == 0) {
  
  cat("No existing Injury_Database file found. Full backfill.\n")
  MISSING_DATES <- all_dates$commence_date_est
  
} else {
  
  injury_db <- fread(
    file       = injury_path,
    colClasses = "character",
    encoding   = "UTF-8"
  )
  
  if (!"pulled_date" %in% names(injury_db)) {
    stop("Injury database is missing `pulled_date` column.")
  }
  
  injury_db <- injury_db %>%
    mutate(
      pulled_date = trimws(pulled_date),
      pulled_date = ifelse(is.na(pulled_date) | pulled_date == "", NA_character_, pulled_date),
      # normalize if pulled_date is MM/DD/YYYY
      pulled_date = ifelse(
        grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", pulled_date),
        format(as.Date(pulled_date, format = "%m/%d/%Y"), "%Y-%m-%d"),
        pulled_date
      )
    )
  
  pulled_dates <- injury_db %>%
    filter(!is.na(pulled_date)) %>%
    distinct(pulled_date) %>%
    arrange(pulled_date)
  
  cat("Injury DB pulled dates:", nrow(pulled_dates), "\n")
  
  # Dates present in odds but NOT yet in injury DB (character vs character)
  MISSING_DATES <- setdiff(
    all_dates$commence_date_est,
    pulled_dates$pulled_date
  )
}

# ------------------------------------------------------------
# Build outputs
# ------------------------------------------------------------
MISSING_DATES <- sort(MISSING_DATES)
cat("Missing dates to pull injuries for:", length(MISSING_DATES), "\n")

missing_dates_df <- data.frame(
  missing_date = MISSING_DATES,
  stringsAsFactors = FALSE
)

# This is what you pass to RapidAPI (already YYYY-MM-DD)
injury_date_vector <- MISSING_DATES


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Load NBA Historical Odds to get Comparison Dates (Injury DB vs Historical Odds) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Pull Historical Injuries for file using dates that are missing ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ============================================================
# 3. FUNCTION: PULL INJURIES FOR ONE DATE TO PRIME THE API
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
  
  # If API fails, return empty tibble
  if (http_error(resp)) {
    message("Error pulling date: ", dt)
    return(tibble(
      date = character(),
      team = character(),
      player = character(),
      status = character(),
      reason = character(),
      reportTime = character(),
      pulled_date = dt,
      snapshot_datetime_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ))
  }
  
  snapshot_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  
  dat <- content(resp, "text", encoding = "UTF-8") %>%
    fromJSON(simplifyDataFrame = TRUE)
  
  # Convert empty lists into empty tibble
  df <- as_tibble(dat)
  
  if (nrow(df) == 0) {
    df <- tibble(
      date = character(),
      team = character(),
      player = character(),
      status = character(),
      reason = character(),
      reportTime = character()
    )
  }
  
  df %>%
    mutate(
      pulled_date = dt,
      snapshot_datetime_utc = snapshot_utc
    )
}


# ============================================================
# 4. PULL INJURIES FOR TODAY
# ============================================================
today_str <- format(Sys.Date(), "%Y-%m-%d")

url_today <- paste0(
  "https://nba-injuries-reports.p.rapidapi.com/injuries/nba/",
  today_str
)

resp_today <- GET(
  url_today,
  add_headers(
    "X-RapidAPI-Key"  = api_key,
    "X-RapidAPI-Host" = "nba-injuries-reports.p.rapidapi.com"
  )
)

stop_for_status(resp_today)

injuries_today <- content(resp_today, "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = TRUE) %>%
  as_tibble() %>%
  mutate(
    pulled_date = today_str,
    snapshot_datetime_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )


# ============================================================
# 5. LOOP: PULL ALL HISTORICAL INJURIES  (with progress logging)
# ============================================================

# Use ONLY the missing dates from the comparison block
date_vector <- injury_date_vector  # <-- key change

total_dates <- length(date_vector)

if (total_dates == 0) {
  message("No missing dates to pull. injuries_all will be empty.")
  injuries_all <- tibble()
} else {

  injuries_list <- vector("list", total_dates)

  for (i in seq_along(date_vector)) {
    dt <- date_vector[i]
    remaining <- total_dates - i

    message(sprintf(
      "[%d/%d] Pulling injuries for %s  |  Remaining: %d dates",
      i, total_dates, dt, remaining
    ))

    Sys.sleep(1)
    injuries_list[[i]] <- get_injuries_for_date(dt)
  }

  injuries_all <- dplyr::bind_rows(injuries_list)
}

injuries_all <- injuries_all %>%
  mutate(
    player = case_when(
      grepl("^Karl\\s*-\\s*Towns$", player, ignore.case = TRUE) ~ "Karl-Anthony Towns",
      grepl("^Eli\\s+Ndiaye$", player, ignore.case = TRUE)      ~ "Eli John Ndiaye",
      grepl("^Yanic\\s+Niederhauser$", player, ignore.case = TRUE) ~ "Yanic Konan Niederhauser",
      grepl("^Olivier\\s*-\\s*Prosper$", player, ignore.case = TRUE) ~ "Olivier-Maxence Prosper",
      grepl("^Shai\\s+Alexander$", player, ignore.case = TRUE) ~ "Shai Gilgeous-Alexander",
      TRUE ~ player
    )
  )


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Pull Historical IUnjuries for file using dates that are missing ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Append pulled injuries to injury_db dataframe ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ============================================================
# 6. SAVE / APPEND: Injury_Database_<season_token>.csv
# ============================================================

library(data.table)

base_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)"

injury_path <- paste0(
  base_dir,
  "/9. Historical Injuries (RapidAPI)/Injury_Database_",
  season_token,
  ".csv"
)

dir.create(dirname(injury_path), recursive = TRUE, showWarnings = FALSE)

if (!exists("injuries_all")) stop("`injuries_all` not found. Run the API pull loop first.")

# -----------------------------
# New data
# -----------------------------
new_dt <- as.data.table(injuries_all)

# force character (no copies via set)
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
# Append + dedupe (SAFE)
# -----------------------------
if (file.exists(injury_path) && file.info(injury_path)$size > 0) {
  
  old_dt <- fread(injury_path, colClasses = "character", encoding = "UTF-8")
  
  # align columns without sorting
  all_cols <- union(names(old_dt), names(new_dt))
  for (col in setdiff(all_cols, names(old_dt))) old_dt[, (col) := ""]
  for (col in setdiff(all_cols, names(new_dt))) new_dt[, (col) := ""]
  setcolorder(old_dt, all_cols)
  setcolorder(new_dt, all_cols)
  
  combined_dt <- rbindlist(list(old_dt, new_dt), use.names = TRUE, fill = TRUE)
  
  # ---- MEMORY SAFE DEDUPE ----
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

# -----------------------------
# Save
# -----------------------------
fwrite(combined_dt, injury_path, quote = TRUE, na = "", sep = ",")

message("Saved Injury_Database")
message("This run rows: ", nrow(new_dt), " | Total rows now: ", nrow(combined_dt))



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  END: Append pulled injuries to injury_db dataframe ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === START: Patch espn_player_id + espn_team_id INTO Injury_Database (NO JOINS) ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(data.table)
library(dplyr)

base_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)"

injury_path <- paste0(
  base_dir,
  "/9. Historical Injuries (RapidAPI)/Injury_Database_",
  season_token,
  ".csv"
)

name_map_path <- paste0(
  base_dir,
  "/6. Data Cleanup/hoopr_name_mapping.csv"
)

team_map_path <- paste0(
  base_dir,
  "/6. Data Cleanup/team_id_mapping.csv"
)

# ------------------------------------------------------------
# Ensure folder exists
# ------------------------------------------------------------
dir.create(dirname(injury_path), recursive = TRUE, showWarnings = FALSE)

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

# Build lookup vectors (FAST, no joins)
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
)

# Ensure required columns exist
req_team_cols <- c("team_long_name")
missing_team_cols <- setdiff(req_team_cols, names(team_map))
if (length(missing_team_cols) > 0) {
  stop("team_id_mapping.csv missing required columns: ", paste(missing_team_cols, collapse = ", "))
}

# Build a robust match key on the mapping side
team_map <- team_map %>%
  mutate(
    team_long_key = toupper(trimws(gsub("[^A-Za-z ]", "", team_long_name)))
  )

# ------------------------------------------------------------
# Load injury database (first-run safe)
# ------------------------------------------------------------
if (!file.exists(injury_path) || file.info(injury_path)$size == 0) {
  
  message("First run: Injury_Database not found. Using injuries_all from this run.")
  
  if (!exists("injuries_all")) {
    stop("injuries_all not found — run injury API pull first.")
  }
  
  injury_db <- as.data.frame(injuries_all)
  
} else {
  
  message("Existing Injury_Database found. Loading file.")
  injury_db <- fread(injury_path, colClasses = "character", encoding = "UTF-8") %>%
    as.data.frame()
}

# ------------------------------------------------------------
# Manual player-name fixes for known API weirdness (optional but keeps mapping clean)
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
# Build cleaned player key from injury file
# ------------------------------------------------------------
injury_db$player_clean <- toupper(trimws(gsub("[^A-Za-z ]", "", injury_db$player)))

# ------------------------------------------------------------
# PASS 1: Patch using clean_player_name
# ------------------------------------------------------------
idx_missing <- which(is.na(injury_db$espn_player_id) | injury_db$espn_player_id == "")
injury_db$espn_player_id[idx_missing] <- lookup_clean[injury_db$player_clean[idx_missing]]

# ------------------------------------------------------------
# PASS 2: Fallback using nba_player_name
# ------------------------------------------------------------
idx_still_missing <- which(is.na(injury_db$espn_player_id) | injury_db$espn_player_id == "")
if (length(idx_still_missing) > 0) {
  message("Fallback pass: matching remaining players using nba_player_name")
  injury_db$espn_player_id[idx_still_missing] <- lookup_nba[injury_db$player_clean[idx_still_missing]]
}

# Final cleanup
injury_db$espn_player_id[is.na(injury_db$espn_player_id)] <- ""

# ------------------------------------------------------------
# Patch TEAM IDs: match injury_db$team -> team_map$team_long_name (NO JOINS)
# Pull ALL columns from team_map into injury_db
# ------------------------------------------------------------
if (!"team" %in% names(injury_db)) {
  message("injury_db has no `team` column; skipping team mapping.")
} else {
  
  injury_db$team <- as.character(injury_db$team)
  
  injury_db$team_long_key <- toupper(trimws(gsub("[^A-Za-z ]", "", injury_db$team)))
  
  idx_team <- match(injury_db$team_long_key, team_map$team_long_key)
  
  # Add all team_map columns (except the helper key) into injury_db
  add_cols <- setdiff(names(team_map), "team_long_key")
  for (cc in add_cols) {
    # Avoid overwriting if already present
    if (!cc %in% names(injury_db)) {
      injury_db[[cc]] <- team_map[[cc]][idx_team]
    } else {
      # If already present, leave it and create a suffixed version (rare, but safe)
      injury_db[[paste0(cc, "_teammap")]] <- team_map[[cc]][idx_team]
    }
  }
  
  # Quick sanity logging
  n_unmatched <- sum(is.na(idx_team))
  message("Team mapping unmatched rows: ", n_unmatched, " / ", nrow(injury_db))
}


# ============================================================
# PATCH TEAM_LOGO + HEADSHOT INTO Injury_Database
# ============================================================

library(data.table)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

base_stats_team_path <- paste0(
  base_dir,
  "/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token,
  ".csv"
)

base_stats_player_path <- paste0(
  base_dir,
  "/1. hoopR/2. BaseStats_Player/BaseStats_Player_",
  season_token,
  ".csv"
)

# ------------------------------------------------------------
# TEAM LOGO PATCH
# ------------------------------------------------------------

if (file.exists(base_stats_team_path)) {
  
  team_dt <- fread(
    base_stats_team_path,
    select = c("espn_team_id", "team_logo"),
    colClasses = "character"
  )
  
  team_dt <- unique(team_dt)
  
  team_lookup <- setNames(team_dt$team_logo, team_dt$espn_team_id)
  
  if (!"team_logo" %in% names(injury_db)) {
    injury_db$team_logo <- ""
  }
  
  idx_missing_logo <- which(
    is.na(injury_db$team_logo) | injury_db$team_logo == ""
  )
  
  injury_db$team_logo[idx_missing_logo] <-
    team_lookup[injury_db$espn_team_id[idx_missing_logo]]
  
} else {
  message("BaseStats_Team file not found for team_logo patch.")
}

# ------------------------------------------------------------
# PLAYER HEADSHOT PATCH
# ------------------------------------------------------------

if (file.exists(base_stats_player_path)) {
  
  player_dt <- fread(
    base_stats_player_path,
    select = c("espn_player_id", "headshot"),
    colClasses = "character"
  )
  
  player_dt <- unique(player_dt)
  
  player_lookup <- setNames(player_dt$headshot, player_dt$espn_player_id)
  
  if (!"headshot" %in% names(injury_db)) {
    injury_db$headshot <- ""
  }
  
  idx_missing_headshot <- which(
    is.na(injury_db$headshot) | injury_db$headshot == ""
  )
  
  injury_db$headshot[idx_missing_headshot] <-
    player_lookup[injury_db$espn_player_id[idx_missing_headshot]]
  
} else {
  message("BaseStats_Player file not found for headshot patch.")
}

# ------------------------------------------------------------
# Final cleanup
# ------------------------------------------------------------

injury_db$team_logo[is.na(injury_db$team_logo)] <- ""
injury_db$headshot[is.na(injury_db$headshot)] <- ""

message("Patched team_logo + headshot into Injury_Database.")


# ------------------------------------------------------------
# Save back to disk (no join suffixes)
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
# === END: Patch espn_player_id + espn_team_id INTO Injury_Database ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
