#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 1. START: Pull nba_schedule and enrich it with nba_team_id for nba_commonteamroster pull ====
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

library(hoopR)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(data.table)
library(lubridate)
library(purrr)

# ---- Load BaseStats_Team (still available for later use) ----
baseST_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token,
  ".csv"
)

if (!file.exists(baseST_path) || file.info(baseST_path)$size == 0) {
  stop("BaseStats_Team file not found or empty at: ", baseST_path)
}

BaseStats_Team <- read_csv(baseST_path, col_types = cols(.default = "c")) %>%
  mutate(game_date = as.Date(game_date))

cat("Loaded BaseStats_Team rows: ", nrow(BaseStats_Team), "\n\n")

# ---- Load full-season NBA schedule ----
schedule_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_",
  season_token,
  ".csv"
)

if (!file.exists(schedule_path) || file.info(schedule_path)$size == 0) {
  stop("NBA schedule file not found or empty at: ", schedule_path)
}

nba_schedule <- fread(
  schedule_path,
  colClasses = "character",
  encoding  = "UTF-8"
) %>%
  mutate(game_date = as.Date(game_date))

cat("nba_schedule rows: ", nrow(nba_schedule), "\n")

# ---- Load static ESPN → NBA team_id mapping file ----
team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/team_id_mapping.csv"

if (!file.exists(team_map_path) || file.info(team_map_path)$size == 0) {
  stop("team_id_mapping file not found or empty at: ", team_map_path)
}

team_id_map <- read_csv(
  team_map_path,
  col_types = cols(
    espn_team_id = col_character(),
    nba_team_id  = col_character()
  )
) %>%
  distinct()

cat("Mapping rows (espn_team_id → nba_team_id): ", nrow(team_id_map), "\n")

# ---- Add nba_team_id to schedule using mapping file ----
nba_schedule_enriched <- nba_schedule %>%
  left_join(
    team_id_map,
    by = c("team_id" = "espn_team_id")
  )

cat("nba_schedule_enriched rows: ", nrow(nba_schedule_enriched), "\n\n")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 1. END: Pull nba_schedule and enrich it with nba_team_id for nba_commonteamroster pull ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. START: Common Team Roster Pull (date-aware) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ---- Player-Enriched Odds Path ----
enriched_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/nba_player_odds_enrich_",
  season_token,
  ".csv"
)

cat("BaseStats_Team path: ", baseST_path, "\n")
cat("Enriched path:      ", enriched_path, "\n\n")


# ============================
# 1. LOAD ENRICHED FILE STATE (GAME_DATE_CTR ONLY)
# ============================

if (!file.exists(enriched_path) || file.info(enriched_path)$size == 0) {
  stop("Enriched player odds file missing or empty: ", enriched_path)
}

cat("Enriched file exists → determining last enriched GAME_DATE_CTR\n")

enriched_df <- fread(
  enriched_path,
  colClasses = "character",
  encoding  = "UTF-8"
)

# HARD REQUIREMENT
if (!"GAME_DATE_CTR" %in% names(enriched_df)) {
  stop("Enriched file is missing GAME_DATE_CTR column.")
}

# GAME_DATE_CTR may be MM/DD/YYYY or YYYY-MM-DD → normalize both to Date before taking max
raw_dates <- trimws(enriched_df$GAME_DATE_CTR)

enriched_dates <- as.Date(raw_dates, format = "%m/%d/%Y")   # try MM/DD/YYYY first
failed <- is.na(enriched_dates)
enriched_dates[failed] <- as.Date(raw_dates[failed], format = "%Y-%m-%d")  # fall back to YYYY-MM-DD

# drop remaining garbage
enriched_dates <- enriched_dates[!is.na(enriched_dates)]

if (length(enriched_dates) == 0) {
  stop("GAME_DATE_CTR exists but no valid dates parsed — check file contents.")
}

last_enriched_date <- max(enriched_dates)

cat("Last enriched date: ", as.character(last_enriched_date), "\n\n")


# ==========================================
# 2. USE SCHEDULE DATES (NOT BaseStats_Team)
# ==========================================

schedule_dates <- sort(unique(nba_schedule_enriched$game_date))

cat("nba_schedule_enriched date range: ",
    as.character(min(schedule_dates)), " to ", as.character(max(schedule_dates)), "\n")

# Diagnostic: confirm current_date is parsing correctly
cat("current_date raw:   ", current_date, "\n")
cat("current_date parsed:", as.character(as.Date(current_date, format = "%m/%d/%Y")), "\n")
cat("last_enriched_date: ", as.character(last_enriched_date), "\n\n")

# allow up through today — explicit format to prevent NA parse
if (exists("current_date")) {
  max_allowed_date <- min(
    as.Date(current_date, format = "%m/%d/%Y"),  # ← explicit format
    max(schedule_dates)
  )
} else {
  max_allowed_date <- max(schedule_dates)
}

cat("max_allowed_date:   ", as.character(max_allowed_date), "\n\n")

dates_to_run <- schedule_dates[
  schedule_dates > last_enriched_date &
    schedule_dates <= max_allowed_date
]

cat("Dates to run (", length(dates_to_run), "): ",
    paste(as.character(dates_to_run), collapse = ", "),
    "\n\n", sep = "")


# ===========================
# 3. BUILD ROSTER KEYS
# ===========================

if (length(dates_to_run) == 0) {
  
  cat("✅ Nothing new to enrich. Exiting roster pull.\n")
  Final_Roster_df <- data.frame()
  
} else {
  
  BaseStats_Team_keys <- nba_schedule_enriched %>%
    filter(game_date %in% dates_to_run) %>%
    transmute(
      espn_game_id  = game_id,
      espn_team_id  = team_id,
      nba_team_id   = nba_team_id,
      team          = team,
      opp           = opp,
      home_away_sym = home_away,
      game_date     = game_date
    ) %>%
    distinct() %>%
    filter(nchar(trimws(team)) <= 3)  # ← Drop All-Star/non-NBA teams (STRIPES, STARS, etc.)
  
  cat("Rows to process in BaseStats_Team_keys: ",
      nrow(BaseStats_Team_keys), "\n\n")
  
  
  # ===========================
  # 4. ROSTER LOOP
  # ===========================
  
  ROSTER_RUNNING <- data.frame()
  
  for (i in seq_len(nrow(BaseStats_Team_keys))) {
    
    this_row       <- BaseStats_Team_keys[i, ]
    espn_game_id_i <- this_row$espn_game_id
    espn_team_id_i <- this_row$espn_team_id
    nba_team_id_i  <- this_row$nba_team_id
    team_i         <- this_row$team
    opp_i          <- this_row$opp
    home_away_i    <- this_row$home_away_sym
    game_date_i    <- this_row$game_date
    
    cat("[", i, "/", nrow(BaseStats_Team_keys), "] ",
        "Game:", espn_game_id_i,
        " | NBA team:", nba_team_id_i,
        " | ESPN team:", espn_team_id_i,
        " | team:", team_i,
        " | opp:", opp_i,
        " | H/A:", home_away_i,
        " | game_date:", as.character(game_date_i),
        "\n", sep = "")
    
    raw <- nba_commonteamroster(
      team_id = nba_team_id_i,
      season  = season_token2
    )
    
    roster_i <- raw$CommonTeamRoster %>%
      select(-LeagueID, -NICKNAME, -PLAYER_SLUG, -BIRTH_DATE, -SCHOOL, -HOW_ACQUIRED) %>%
      rename(
        NBA_TEAM_ID   = TeamID,
        NBA_PLAYER_ID = PLAYER_ID
      ) %>%
      mutate(
        ESPN_GAME_ID   = espn_game_id_i,
        ESPN_TEAM_ID   = espn_team_id_i,
        NBA_TEAM_ID    = nba_team_id_i,
        TEAM           = team_i,
        OPP            = opp_i,
        HOME_AWAY_SYM  = home_away_i,
        GAME_DATE      = format(as.Date(game_date_i), "%Y-%m-%d"),  # ← always YYYY-MM-DD
        GAME_DATE_CTR  = format(as.Date(game_date_i), "%Y-%m-%d")   # ← always YYYY-MM-DD
      )
    
    ROSTER_RUNNING <- bind_rows(ROSTER_RUNNING, roster_i)
    Sys.sleep(0.5)
  }
  
  Final_Roster_df <- ROSTER_RUNNING
}

rm(raw, this_row, roster_i, BaseStats_Team_keys, ROSTER_RUNNING)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 2. END: Common Team Roster Pull (date-aware) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 3. START: Pull additional columns from BaseStats_Player file ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ==== 1. Load BaseStats_Player for this season ====

player_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_",
  season_token,
  ".csv"
)

BaseStats_Player <- read_csv(player_path, col_types = cols(.default = "c"))

# Keep only what we need for the join
player_lookup <- BaseStats_Player %>%
  select(
    espn_game_id,      # key
    nba_player_id,     # key
    headshot,
    team_logo,
    opp_logo,
    espn_player_id,
    starter_status,
    game_date,
    reason
  )

# ==== 2. Join onto Final_Roster_df (NBA_PLAYER_ID + espn_game_id) ====

Final_Roster_df <- Final_Roster_df %>%
  left_join(
    player_lookup,
    by = c(
      "ESPN_GAME_ID"   = "espn_game_id",
      "NBA_PLAYER_ID"  = "nba_player_id"
    )
  ) %>%
  select(-GAME_DATE) %>%   # remove the old one
  rename(
    ESPN_PLAYER_ID  = espn_player_id,
    HEADSHOT        = headshot,
    TEAM_LOGO       = team_logo,
    OPP_LOGO        = opp_logo,
    STARTER_STATUS  = starter_status,
    REASON          = reason,
    GAME_DATE       = game_date     # rename the new one
  )

rm(player_lookup, BaseStats_Player)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 3. END: Pull additional columns from BaseStats_Player file ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 4. START: Clean Player Names with hoopR name mapping file ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


name_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"

hoopr_name_map <- read_csv(name_map_path, col_types = cols(.default = "c")) %>%
  select(
    nba_player_id,
    espn_player_id,
    nba_player_name,
    clean_player_name
  )

# ==== 2. Join Final_Roster_df USING PLAYER → nba_player_name ====
#      Fill ESPN_PLAYER_ID and overwrite PLAYER with clean name

Final_Roster_df <- Final_Roster_df %>%
  left_join(
    hoopr_name_map,
    by = c("PLAYER" = "nba_player_name")       # <-- key change
  ) %>%
  mutate(
    # Fill missing ESPN_PLAYER_ID when mapping provides it
    ESPN_PLAYER_ID = if_else(
      is.na(ESPN_PLAYER_ID) & !is.na(espn_player_id),
      espn_player_id,
      ESPN_PLAYER_ID
    ),
    
    # Overwrite PLAYER with clean name when available
    PLAYER = if_else(
      !is.na(clean_player_name) & clean_player_name != "",
      clean_player_name,
      PLAYER
    )
  ) %>%
  select(-clean_player_name, -espn_player_id)   # cleanup helper cols

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 4. END: Clean Player Names with hoopR name mapping file ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 5. START: Create Current Slate Roster Dataframe and Fill in Final_roster_df todays slate ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

Final_Roster_today <- Final_Roster_df %>%
  filter(GAME_DATE_CTR == as.Date(current_date))

## 1) Build player → info lookup from the FULL Final_Roster_df ----
player_ref <- Final_Roster_df %>%
  group_by(PLAYER) %>%
  summarize(
    HEADSHOT_REF = dplyr::first(
      HEADSHOT[!is.na(HEADSHOT) & HEADSHOT != ""],
      default = NA_character_
    ),
    TEAM_LOGO_REF = dplyr::first(
      TEAM_LOGO[!is.na(TEAM_LOGO) & TEAM_LOGO != ""],
      default = NA_character_
    ),
    ESPN_PLAYER_ID_REF = dplyr::first(
      ESPN_PLAYER_ID[!is.na(ESPN_PLAYER_ID) & ESPN_PLAYER_ID != ""],
      default = NA_character_
    ),
    .groups = "drop"
  )

## 2) Build OPP → OPP_LOGO lookup from the FULL Final_Roster_df ----
opp_logo_ref <- Final_Roster_df %>%
  group_by(OPP) %>%
  summarize(
    OPP_LOGO_REF = dplyr::first(
      OPP_LOGO[!is.na(OPP_LOGO) & OPP_LOGO != ""],
      default = NA_character_
    ),
    .groups = "drop"
  )

## 3) Helper to apply fills to any roster df ----
fill_roster_gaps <- function(df, player_ref, opp_logo_ref) {
  df %>%
    # join player-level info
    left_join(player_ref, by = "PLAYER") %>%
    # join opponent logo based on OPP
    left_join(opp_logo_ref, by = "OPP") %>%
    mutate(
      HEADSHOT       = coalesce(HEADSHOT, HEADSHOT_REF),
      TEAM_LOGO      = coalesce(TEAM_LOGO, TEAM_LOGO_REF),
      ESPN_PLAYER_ID = coalesce(ESPN_PLAYER_ID, ESPN_PLAYER_ID_REF),
      OPP_LOGO       = coalesce(OPP_LOGO, OPP_LOGO_REF)
    ) %>%
    select(-HEADSHOT_REF, -TEAM_LOGO_REF, -ESPN_PLAYER_ID_REF, -OPP_LOGO_REF)
}

## 4) Apply to BOTH data frames ----
Final_Roster_df    <- fill_roster_gaps(Final_Roster_df,    player_ref, opp_logo_ref)
Final_Roster_today <- fill_roster_gaps(Final_Roster_today, player_ref, opp_logo_ref)

rm(opp_logo_ref, player_ref)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 5. END: Create Current Slate Roster Dataframe and Fill in Final_roster_df todays slate ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 6. START: (Establishing list of dates needed if there are none) Date Check Logic again nba_historical_ods ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# ----------------------------------------------------------
# 2.1 FILE PATHS
# ----------------------------------------------------------

team_hist_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_",
  season_token,
  ".csv"
)

# SINGLE player-points file
points_file_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/nba_historical_player_odds_player_points_",
  season_token,
  ".csv"
)

cat("TEAM hist path:   ", team_hist_path,  "\n")
cat("POINTS file path: ", points_file_path, "\n\n")


# ----------------------------------------------------------
# 2.2 LOAD TEAM HISTORICAL ODDS AS DRIVER BASE
#     (TEAM odds defines the universe; do NOT constrain to BaseStats_Team)
# ----------------------------------------------------------

if (!file.exists(team_hist_path) || file.info(team_hist_path)$size == 0) {
  stop("Team historical odds file not found or empty at: ", team_hist_path)
}

team_hist_df <- read_csv(
  team_hist_path,
  col_types = cols(
    game_id               = col_character(),
    snapshot_datetime_utc = col_character(),
    commence_date_est     = col_character()
  )
)

# Derive commence_date_est when needed
if (!"commence_date_est" %in% names(team_hist_df)) {
  if (!"commence_time" %in% names(team_hist_df)) {
    stop("commence_date_est and commence_time both missing from team historical odds file.")
  }
  
  cat("commence_date_est missing; deriving from commence_time...\n")
  
  team_hist_df$commence_time_utc <- as.POSIXct(
    team_hist_df$commence_time,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  
  team_hist_df$commence_date_est <- as.Date(
    format(team_hist_df$commence_time_utc, tz = "America/New_York", usetz = TRUE)
  )
}

# TEAM file: treat commence_date_est as Date (assumed yyyy-mm-dd)
team_hist_df$commence_date_est <- as.Date(
  team_hist_df$commence_date_est,
  tryFormats = c("%Y-%m-%d", "%m/%d/%Y")
)

# Only one row per game/date (correct for MISSING DATE logic)
driver_base <- team_hist_df %>%
  distinct(game_id, commence_date_est) %>%
  filter(!is.na(commence_date_est))

cat("Driver base rows: ", nrow(driver_base), "\n")
if (nrow(driver_base) > 0) {
  cat("Driver base date range: ",
      as.character(min(driver_base$commence_date_est)),
      " → ",
      as.character(max(driver_base$commence_date_est)),
      "\n\n")
} else {
  cat("Driver base date range: <empty>\n\n")
}


# ----------------------------------------------------------
# CLAMP TO current_date (+1 DAY)  (optional safety)
# ----------------------------------------------------------

if (exists("current_date")) {
  current_date_val <- as.Date(current_date)
  upper_date_val   <- current_date_val + 1
  
  before_clamp <- nrow(driver_base)
  
  driver_base <- driver_base %>%
    filter(commence_date_est <= upper_date_val)
  
  cat("After clamp to current_date+1 (",
      as.character(current_date_val), " → ",
      as.character(upper_date_val), "): ",
      before_clamp, " → ", nrow(driver_base),
      " rows\n\n", sep = "")
}


# ----------------------------------------------------------
# 2.3 LOAD PLAYER POINTS FILE AND FIND MISSING GAME/DATE PAIRS
# ----------------------------------------------------------

if (!file.exists(points_file_path) || file.info(points_file_path)$size == 0) {
  
  cat("Points file missing → treating as empty (full backfill).\n")
  
  existing_pairs <- tibble(
    game_id           = character(0),
    commence_date_est = as.Date(character(0))
  )
  
} else {
  
  player_points_odds_df <- fread(points_file_path, colClasses = "character")
  
  if (!all(c("game_id", "commence_date_est") %in% names(player_points_odds_df))) {
    stop("Points file missing game_id or commence_date_est.")
  }
  
  # Robust parse: handle both yyyy-mm-dd and mm/dd/yyyy from the points file
  player_points_odds_df$commence_date_est <- as.Date(
    player_points_odds_df$commence_date_est,
    tryFormats = c("%Y-%m-%d", "%m/%d/%Y")
  )
  
  existing_pairs <- player_points_odds_df %>%
    distinct(game_id, commence_date_est) %>%
    filter(!is.na(commence_date_est))
}

cat("Existing points game/date pairs: ", nrow(existing_pairs), "\n\n")


# ----------------------------------------------------------
# FIND MISSING DATES (THIS IS THE FINAL DATE LIST)
# ----------------------------------------------------------

missing_pairs <- driver_base %>%
  anti_join(existing_pairs, by = c("game_id", "commence_date_est"))

cat("Missing game/date pairs (to pull): ", nrow(missing_pairs), "\n\n")


# ----------------------------------------------------------
# FINAL DRIVER_DF FOR LOOP
# ----------------------------------------------------------

driver_df <- missing_pairs %>%
  mutate(
    # force snapshot to noon UTC
    snapshot_datetime_utc = paste0(commence_date_est, "T12:00:00Z")
  ) %>%
  select(game_id, snapshot_datetime_utc, commence_date_est) %>%
  arrange(commence_date_est)

if (nrow(driver_df) == 0) {
  
  cat("No missing game/date combos to process.\n\n")
  
} else {
  
  cat("Final driver_df rows to process: ", nrow(driver_df), "\n")
  cat("Final driver_df date range: ",
      as.character(min(driver_df$commence_date_est)), " → ",
      as.character(max(driver_df$commence_date_est)),
      "\n\n")
}


# ----------------------------------------------------------
# Add ODDS_API_MAP to roster tables (unchanged)
# ----------------------------------------------------------

Final_Roster_df <- Final_Roster_df %>%
  mutate(
    ODDS_API_MAP = paste0(TEAM, "_", OPP, "_", GAME_DATE_CTR)
  )

Final_Roster_today <- Final_Roster_today %>%
  mutate(
    ODDS_API_MAP = paste0(TEAM, "_", OPP, "_", GAME_DATE_CTR)
  )

rm(player_points_odds_df)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 6. END: (Establishing list of dates needed if there are none) Date Check Logic again nba_historical_ods ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 7. START: Pull All Player Prop odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

api_key <- "4e644f6b78c9bd755f7ce726ee410e2f"

# If nothing to do, stop here
if (nrow(driver_df) == 0) {
  cat("✅ Player props already up to date.\n")
} else {
  
  markets_to_pull <- c(
    "player_points", "player_rebounds", "player_assists",
    "player_threes", "player_blocks", "player_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds", "player_points_assists",
    "player_rebounds_assists", "player_double_double",
    "player_triple_double"
  )
  
  # Base directory for all per-market files
  player_odds_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds/"
  
  for (i in seq_len(nrow(driver_df))) {
    event_id      <- driver_df$game_id[i]
    
    # Use commence_date_est as game date, then build midnight UTC snapshot
    commence_date     <- as.Date(driver_df$commence_date_est[i])
    snapshot_datetime <- paste0(commence_date, "T12:00:00Z")
    
    cat("\n🔄 [", i, "/", nrow(driver_df), "] Event:", event_id,
        " | game_date(EST):", as.character(commence_date),
        " | snapshot_datetime_utc:", snapshot_datetime, "\n")
    
    for (market in markets_to_pull) {
      cat("   ⏳ Market:", market, "\n")
      
      # 🔧 Per-market output file
      market_file <- paste0(
        player_odds_dir,
        "nba_historical_player_odds_",
        market, "_",
        season_token,
        ".csv"
      )
      
      # Check if this market file already exists (controls header vs append)
      market_file_exists <- file.exists(market_file) && file.info(market_file)$size > 0
      
      url <- paste0(
        "https://api.the-odds-api.com/v4/historical/sports/basketball_nba/events/",
        event_id,
        "/odds?regions=us",
        "&markets=", market,
        "&oddsFormat=american",
        "&date=", URLencode(snapshot_datetime),
        "&apiKey=", api_key
      )
      
      res    <- GET(url)
      status <- status_code(res)
      
      if (status == 200) {
        odds_data_raw <- content(res, as = "text", encoding = "UTF-8")
        odds_data     <- fromJSON(odds_data_raw, simplifyDataFrame = FALSE)
        
        if (!is.null(odds_data$data)) {
          game <- odds_data$data
          
          rows <- list()
          if (!is.null(game$bookmakers) && length(game$bookmakers) > 0) {
            for (bookmaker in game$bookmakers) {
              if (!is.null(bookmaker$markets) && length(bookmaker$markets) > 0) {
                for (mkt in bookmaker$markets) {
                  if (!is.null(mkt$outcomes) && length(mkt$outcomes) > 0) {
                    for (outcome in mkt$outcomes) {
                      rows[[length(rows) + 1]] <- tibble(
                        snapshot_datetime_utc = snapshot_datetime,
                        game_id               = game$id,
                        sport                 = game$sport_key,
                        commence_time         = game$commence_time,
                        home_team             = game$home_team,
                        away_team             = game$away_team,
                        bookmaker             = bookmaker$key,
                        market                = mkt$key,
                        outcome_type           = outcome$name,
                        player_name          = outcome$description,
                        line                  = outcome$point,
                        odds                  = outcome$price
                      )
                    }
                  }
                }
              }
            }
          }
          
          if (length(rows) == 0) {
            cat("   ⚠️ No outcomes found in parsed data for market:", market, "\n")
          } else {
            flat_df <- bind_rows(rows)
            
            flat_df$commence_time_utc <- as.POSIXct(
              flat_df$commence_time,
              format = "%Y-%m-%dT%H:%M:%SZ",
              tz = "UTC"
            )
            flat_df$commence_time_est <- with_tz(
              flat_df$commence_time_utc,
              tzone = "America/New_York"
            )
            flat_df$commence_date_est <- as.Date(
              format(flat_df$commence_time_utc, tz = "America/New_York", usetz = TRUE)
            )
            
            fwrite(
              flat_df,
              file      = market_file,
              append    = market_file_exists,
              col.names = !market_file_exists
            )
            
            cat("   ✅ Wrote", nrow(flat_df), "rows for", market,
                "into:\n      ", market_file, "\n")
          }
        } else {
          cat("   ⚠️ No `data` field in response for market:", market, "\n")
        }
      } else {
        cat("   ❌ API error:", status, "for market:", market, "\n")
      }
      
      Sys.sleep(1)
    }
    
    Sys.sleep(1)
  }
  
  cat("\n✅ Player props script completed. Per-market files written to:\n",
      player_odds_dir, "\n")
}




rm(bookmaker, driver_base, existing_pairs, flat_df, game, missing_pairs, mkt, odds_data, outcome, res, player_points_odds_df, team_hist_df, rows)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 7. END: Pull All Player Prop odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 8. START: Create Odds API Map in Final Rosters ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Add ODDSAPI_MAP column to the Final_Roster_df dataframe 
Final_Roster_df <- Final_Roster_df %>%
  mutate(
    ODDSAPI_MAP = paste0(
      TEAM, "_",
      OPP, "_",
      GAME_DATE_CTR
    )
  )
# Add ODDSAPI_MAP column to the Final_Roster_today dataframe 
Final_Roster_today <- Final_Roster_today %>%
  mutate(
    ODDSAPI_MAP = paste0(
      TEAM, "_",
      OPP, "_",
      GAME_DATE_CTR
    )
  )

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 8. END: Create Odds API Map in Final Rosters ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 9. START: Save / Reload / Patch Final_Roster_df & Final_Roster_today ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(data.table)

# --- 0) Paths ---
season_file_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/",
  "nba_player_odds_enrich_", season_token, ".csv"
)

daily_file_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/",
  "nba_player_odds_enrich_", formatted_date, ".csv"
)

# --- 1) Check earliest BaseStats_Team date vs Final_Roster_df ---

# get earliest game_date from BaseStats_Team as character (no pipes)
tmp_dates <- as.character(BaseStats_Team$game_date)
tmp_dates <- tmp_dates[!is.na(tmp_dates)]
tmp_dates <- sort(tmp_dates)

earliest_bs_date <- if (length(tmp_dates) > 0) tmp_dates[1] else NA_character_

# ensure Final_Roster_df exists and GAME_DATE_CTR is character
if (exists("Final_Roster_df")) {
  Final_Roster_df$GAME_DATE_CTR <- as.character(Final_Roster_df$GAME_DATE_CTR)
} else {
  # if it truly doesn't exist, start it from today's roster
  Final_Roster_df <- Final_Roster_today
  Final_Roster_df$GAME_DATE_CTR <- as.character(Final_Roster_df$GAME_DATE_CTR)
}

# if earliest BaseStats_Team date is not in Final_Roster_df, reload season file (if present)
if (!is.na(earliest_bs_date) &&
    !(earliest_bs_date %in% Final_Roster_df$GAME_DATE_CTR) &&
    file.exists(season_file_path)) {
  
  cat("↻ Earliest BaseStats_Team date not found in Final_Roster_df — reloading season file.\n")
  
  Final_Roster_df <- fread(
    season_file_path,
    colClasses = "character",
    encoding   = "UTF-8"
  )
} else {
  cat("✓ Final_Roster_df already contains earliest BaseStats_Team date or season file missing.\n")
}

# --- 2) Patch-append Final_Roster_today into Final_Roster_df (no joins) ---

# enforce character keys for matching
Final_Roster_df <- Final_Roster_df %>%
  mutate(
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP),
    GAME_DATE_CTR  = as.character(GAME_DATE_CTR)
  )

Final_Roster_today <- Final_Roster_today %>%
  mutate(
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP),
    GAME_DATE_CTR  = as.character(GAME_DATE_CTR)
  )


# remove any existing rows in Final_Roster_df that match today's (ESPN_PLAYER_ID, ODDSAPI_MAP),
# then bind today's rows on top (patch / overwrite logic)
Final_Roster_df <- bind_rows(
  Final_Roster_df %>%
    anti_join(
      Final_Roster_today,
      by = c("ESPN_PLAYER_ID", "ODDSAPI_MAP")
    ),
  Final_Roster_today
)

cat("✓ Patched Final_Roster_df with today's rows (overwrite by ESPN_PLAYER_ID + ODDSAPI_MAP).\n")

# --- 3) SAVE FULL-SEASON PLAYER ODDS ENRICH ---

unique_dates <- unique(Final_Roster_df$GAME_DATE_CTR)

if (length(unique_dates) > 1) {
  
  fwrite(
    Final_Roster_df,
    file      = season_file_path,
    col.names = TRUE
  )
  
  cat("✔ Saved season-enriched file to:\n", season_file_path, "\n")
  
} else {
  
  cat("⏭ Skipped season CSV write — only 1 unique GAME_DATE_CTR in Final_Roster_df.\n")
  
}

# --- 4) SAVE DAILY PLAYER ODDS ENRICH ---

fwrite(
  Final_Roster_today,
  file      = daily_file_path,
  col.names = TRUE
)

cat("✔ Saved daily-enriched file to:\n", daily_file_path, "\n")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 9. END: Save / Reload / Patch Final_Roster_df & Final_Roster_today ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 10. START: Pull + Enrich All Player Prop Odds (per-market dataframes) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# Folder containing all your historical player odds files
player_odds_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/2. Player Odds"

# List CSV files in this folder
files <- list.files(
  player_odds_dir,
  pattern    = "\\.csv$",
  full.names = TRUE
)

cat("Found", length(files), "player odds files\n")

# Name map: clean name -> ESPN_PLAYER_ID
name_lookup <- hoopr_name_map %>%
  select(espn_player_id, clean_player_name)

# Function: load + normalize + add ESPN_PLAYER_ID
load_market_file <- function(path) {
  
  # Extract market name from filename
  # ex: nba_historical_player_odds_player_points_2025_2026.csv
  market <- str_match(
    basename(path),
    "odds_(.*?)_\\d{4}_\\d{4}\\.csv"
  )[, 2]
  
  if (is.na(market)) {
    stop(paste("Could not parse market name from:", basename(path)))
  }
  
  # Read as character
  df <- fread(path, colClasses = "character", encoding = "UTF-8")
  
  # Force *date* column (yyyy-mm-dd) -> "yyyy-MM-dd" (string)
  # Leave commence_time_* columns alone
  df <- df %>%
    mutate(
      commence_date_est = as.character(
        as.Date(commence_date_est, format = "%Y-%m-%d")
      )
    )
  
  # Attach ESPN_PLAYER_ID via name map
  df <- df %>%
    left_join(
      name_lookup,
      by = c("player_name" = "clean_player_name")
    ) %>%
    rename(ESPN_PLAYER_ID = espn_player_id)
  
  # Give dataframe a clean name, e.g. player_odds_odds_player_points
  df_name <- paste0("player_odds_odds_", market)
  assign(df_name, df, envir = .GlobalEnv)
  
  cat("Loaded & enriched:", df_name, "(", nrow(df), "rows )\n")
  
  invisible(TRUE)
}

# Load every file
lapply(files, load_market_file)


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 10. END Pull + Enrich All Player Prop Odds ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 11. START: Pull All Player Prop odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(readr)
library(tidyr)

# ---- Load team mapping once ----
oddsapi_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/oddsapi_team_mapping.csv"

oddsapi_df <- read.csv(oddsapi_path, stringsAsFactors = FALSE)

home_map <- oddsapi_df %>%
  select(team_name, ESPN.ID, team_abv) %>%
  rename(
    home_team          = team_name,
    espn_team_id_home  = ESPN.ID,
    espn_team_abv_home = team_abv
  )

away_map <- oddsapi_df %>%
  select(team_name, ESPN.ID, team_abv) %>%
  rename(
    away_team          = team_name,
    espn_team_id_away  = ESPN.ID,
    espn_team_abv_away = team_abv
  )

# ---- Add ODDS_API_MAP (both directions) to each player odds df ----
player_df_names <- ls(pattern = "^player_odds_odds_")

for (nm in player_df_names) {
  df <- get(nm)
  
  df <- df %>%
    left_join(home_map, by = "home_team") %>%
    left_join(away_map, by = "away_team") %>%
    mutate(
      espn_team_abv_home = as.character(espn_team_abv_home),
      espn_team_abv_away = as.character(espn_team_abv_away),
      # primary key: HOME_AWAY_DATE
      ODDS_API_MAP        = paste0(espn_team_abv_home, "_", espn_team_abv_away, "_", commence_date_est),
      # flipped key: AWAY_HOME_DATE
      ODDS_API_MAP_FLIPPED = paste0(espn_team_abv_away, "_", espn_team_abv_home, "_", commence_date_est)
    ) %>%
    # keep both key variants as separate rows for robust joining
    pivot_longer(
      cols      = c(ODDS_API_MAP, ODDS_API_MAP_FLIPPED),
      names_to  = "key_type",
      values_to = "ODDS_API_MAP"
    ) %>%
    select(-key_type)
  
  assign(nm, df, envir = .GlobalEnv)
}

rm(df, nm)  # cleanup

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 11. END: Pull All Player Prop odds for NBA from the Odds API (Full File Aggregation) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 12. START: Points Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)

# ============================================================
# Helper: row-by-row merge one market file into a roster df
# ============================================================

add_market_to_roster <- function(roster_df, market_df, market_prefix) {
  
  # 1. Target books + suffixes
  target_books <- c("draftkings", "fanduel", "betmgm", "fanatics")
  book_suffix  <- c(
    draftkings = "DK",
    fanduel    = "FD",
    betmgm     = "BMGM",
    fanatics   = "FNA"
  )
  
  # 2. Filter to books + Over/Under, and keep only fields we need
  market_df_filt <- market_df %>%
    filter(
      bookmaker    %in% target_books,
      outcome_type %in% c("Over", "Under")
    ) %>%
    mutate(
      book_code    = book_suffix[bookmaker],
      outcome_code = ifelse(outcome_type == "Over", "O", "U")
    ) %>%
    select(
      ESPN_PLAYER_ID,
      ODDS_API_MAP,
      book_code,
      outcome_code,
      line,
      odds
    )
  
  # expose filtered rows for debugging if you want to inspect them
  assign(paste0("market_rows_", market_prefix), market_df_filt, envir = .GlobalEnv)
  
  if (nrow(market_df_filt) == 0) {
    message("[", market_prefix, "] No rows for target books / O-U — skipping.")
    return(roster_df)
  }
  
  # 3. Row-by-row patch into roster_df
  patched_rows <- 0L
  
  for (i in seq_len(nrow(market_df_filt))) {
    
    pid   <- market_df_filt$ESPN_PLAYER_ID[i]
    gid   <- market_df_filt$ODDS_API_MAP[i]   # odds file key
    book  <- market_df_filt$book_code[i]      # DK / FD / BMGM / FNA
    ou    <- market_df_filt$outcome_code[i]   # "O" or "U"
    ln    <- market_df_filt$line[i]
    od    <- market_df_filt$odds[i]
    
    # column names we want on the roster side
    line_col <- paste0(market_prefix, "_", book, "_LINE_", ou)
    odds_col <- paste0(market_prefix, "_", book, "_ODDS_", ou)
    
    # ensure columns exist
    if (!line_col %in% names(roster_df)) {
      roster_df[[line_col]] <- NA_character_
    }
    if (!odds_col %in% names(roster_df)) {
      roster_df[[odds_col]] <- NA_character_
    }
    
    # match on ESPN_PLAYER_ID + game key
    idx <- which(
      roster_df$ESPN_PLAYER_ID == pid &
        roster_df$ODDSAPI_MAP    == gid      # roster uses ODDSAPI_MAP
    )
    
    if (length(idx) > 0) {
      roster_df[[line_col]][idx] <- ln
      roster_df[[odds_col]][idx] <- od
      patched_rows <- patched_rows + length(idx)
    }
  }
  
  message("[", market_prefix, "] patched ", patched_rows, " roster rows.")
  
  return(roster_df)
}

# ============================================================
# APPLY — Points Market (player_odds_odds_player_points)
# ============================================================

pts_df <- player_odds_odds_player_points

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    pts_df, "PTS")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, pts_df, "PTS")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 12. END: Points Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 13. START: Assists Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# assumes add_market_to_roster() is already defined above in the script

# ============================================================
# APPLY — Assists Market (player_odds_odds_player_assists)
# ============================================================

ast_df <- player_odds_odds_player_assists

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    ast_df, "AST")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, ast_df, "AST")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 13. END: Assists Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 14. START: Rebounds Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

reb_df <- player_odds_odds_player_rebounds

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    reb_df, "REB")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, reb_df, "REB")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 14. END: Rebounds Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 15. START: Steals Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

stl_df <- player_odds_odds_player_steals

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    stl_df, "STL")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, stl_df, "STL")

rm(market_rows_STL, player_odds_odds_player_steals)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 15. END: Steals Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 16. START: Blocks Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

blk_df <- player_odds_odds_player_blocks

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    blk_df, "BLK")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, blk_df, "BLK")

rm(market_rows_BLK, player_odds_odds_player_blocks)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 16. END: Blocks Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 17. START: Threes Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

tre_df <- player_odds_odds_player_threes

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    tre_df, "3PM")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, tre_df, "3PM")

rm(market_rows_3PM, player_odds_odds_player_threes)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 17. END: Threes Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 18. START: Points Assists Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


library(dplyr)

# ============================================================
# Helper: row-by-row merge one market file into a roster df
# ============================================================

add_market_to_roster <- function(roster_df, market_df, market_prefix) {
  
  # 1. Target books + suffixes
  target_books <- c("draftkings", "fanduel", "betmgm", "fanatics")
  book_suffix  <- c(
    draftkings = "DK",
    fanduel    = "FD",
    betmgm     = "BMGM",
    fanatics   = "FNA"
  )
  
  # 2. Filter to books + Over/Under, and keep only fields we need
  market_df_filt <- market_df %>%
    filter(
      bookmaker   %in% target_books,
      outcome_type %in% c("Over", "Under")
    ) %>%
    mutate(
      book_code    = book_suffix[bookmaker],
      outcome_code = ifelse(outcome_type == "Over", "O", "U")
    ) %>%
    select(
      ESPN_PLAYER_ID,
      ODDS_API_MAP,
      book_code,
      outcome_code,
      line,
      odds
    )
  
  # expose filtered rows for debugging if you want to inspect them
  assign(paste0("market_rows_", market_prefix), market_df_filt, envir = .GlobalEnv)
  
  if (nrow(market_df_filt) == 0) {
    message("[", market_prefix, "] No rows for target books / O-U — skipping.")
    return(roster_df)
  }
  
  # 3. Row-by-row patch into roster_df
  patched_rows <- 0L
  
  for (i in seq_len(nrow(market_df_filt))) {
    
    pid   <- market_df_filt$ESPN_PLAYER_ID[i]
    gid   <- market_df_filt$ODDS_API_MAP[i]   # odds file key
    book  <- market_df_filt$book_code[i]      # DK / FD / BMGM / FNA
    ou    <- market_df_filt$outcome_code[i]   # "O" or "U"
    ln    <- market_df_filt$line[i]
    od    <- market_df_filt$odds[i]
    
    # column names we want on the roster side
    line_col <- paste0(market_prefix, "_", book, "_LINE_", ou)
    odds_col <- paste0(market_prefix, "_", book, "_ODDS_", ou)
    
    # ensure columns exist
    if (!line_col %in% names(roster_df)) {
      roster_df[[line_col]] <- NA_character_
    }
    if (!odds_col %in% names(roster_df)) {
      roster_df[[odds_col]] <- NA_character_
    }
    
    # match on ESPN_PLAYER_ID + game key
    idx <- which(
      roster_df$ESPN_PLAYER_ID == pid &
        roster_df$ODDSAPI_MAP    == gid      # roster uses ODDSAPI_MAP
    )
    
    if (length(idx) > 0) {
      roster_df[[line_col]][idx] <- ln
      roster_df[[odds_col]][idx] <- od
      patched_rows <- patched_rows + length(idx)
    }
  }
  
  message("[", market_prefix, "] patched ", patched_rows, " roster rows.")
  
  return(roster_df)
}

# ============================================================
# TEST / APPLY — Points + Assists Market
# ============================================================

pts_ast_df <- player_odds_odds_player_points_assists

Final_Roster_df    <- add_market_to_roster(Final_Roster_df,    pts_ast_df, "PTS_AST")
Final_Roster_today <- add_market_to_roster(Final_Roster_today, pts_ast_df, "PTS_AST")

rm(market_rows_PTS_AST, player_odds_odds_player_points_rebounds_assists, pts_ast_df)

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 18. END: Points Assists Odds Mapping to NBA Player Odds Enrich File ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 19. START: Patch Actual Player Box Score (BaseStats_Player) into Final_Roster_df ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

library(dplyr)
library(data.table)

# ------------------------------------------------------------
# 1) Load ONLY needed columns from BaseStats_Player
# ------------------------------------------------------------

base_player_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/",
  "BaseStats_Player_", season_token, ".csv"
)

cat("Loading BaseStats_Player from:\n", base_player_path, "\n")

BaseStats_Player <- fread(
  base_player_path,
  colClasses = "character",
  encoding   = "UTF-8",
  select = c(
    "game_date", "team", "opp", "is_home",
    "espn_player_id",
    "MIN", "PTS", "3PTM", "REB", "AST", "STL", "BLK"
  )
)

# ------------------------------------------------------------
# 2) Build ODDSAPI_MAP to match odds files
# ------------------------------------------------------------

BaseStats_Player <- BaseStats_Player %>%
  mutate(
    game_date_iso = format(as.Date(game_date, "%m/%d/%Y"), "%Y-%m-%d"),
    home_team     = if_else(is_home == "1", team, opp),
    away_team     = if_else(is_home == "1", opp, team),
    ODDSAPI_MAP   = paste0(home_team, "_", away_team, "_", game_date)
  )

# 3) Reduce to key + stats, explicitly rename to *_bs
BaseStats_Player_patch <- BaseStats_Player %>%
  select(
    espn_player_id,
    ODDSAPI_MAP,
    MIN_bs    = MIN,
    PTS_bs    = PTS,
    `3PTM_bs` = `3PTM`,
    REB_bs    = REB,
    AST_bs    = AST,
    STL_bs    = STL,
    BLK_bs    = BLK
  ) %>%
  mutate(
    espn_player_id = as.character(espn_player_id),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP)
  )

# 3.5) Ensure target stat cols exist in Final_Roster_df (patch style)
stat_cols <- c("MIN", "PTS", "3PTM", "REB", "AST", "STL", "BLK")
for (cc in stat_cols) {
  if (!cc %in% names(Final_Roster_df)) {
    Final_Roster_df[[cc]] <- NA_character_
  }
}

# 4) Patch (overwrite) into Final_Roster_df using left_join
Final_Roster_df <- Final_Roster_df %>%
  mutate(
    ESPN_PLAYER_ID = as.character(ESPN_PLAYER_ID),
    ODDSAPI_MAP    = as.character(ODDSAPI_MAP)
  ) %>%
  left_join(
    BaseStats_Player_patch,
    by = c(
      "ESPN_PLAYER_ID" = "espn_player_id",
      "ODDSAPI_MAP"    = "ODDSAPI_MAP"
    )
  ) %>%
  mutate(
    MIN    = if_else(!is.na(MIN_bs),    MIN_bs,    MIN),
    PTS    = if_else(!is.na(PTS_bs),    PTS_bs,    PTS),
    `3PTM` = if_else(!is.na(`3PTM_bs`), `3PTM_bs`, `3PTM`),
    REB    = if_else(!is.na(REB_bs),    REB_bs,    REB),
    AST    = if_else(!is.na(AST_bs),    AST_bs,    AST),
    STL    = if_else(!is.na(STL_bs),    STL_bs,    STL),
    BLK    = if_else(!is.na(BLK_bs),    BLK_bs,    BLK)
  ) %>%
  select(
    -MIN_bs, -PTS_bs, -`3PTM_bs`,
    -REB_bs, -AST_bs, -STL_bs, -BLK_bs
  )

cat("✓ Patched BaseStats_Player box score stats into Final_Roster_df via limited left_join.\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 19. END: Patch Actual Player Box Score (BaseStats_Player) into Final_Roster_df ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 20. START: Build stat cover flags + cover amounts (FD Over line vs actual box score) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

Final_Roster_df <- Final_Roster_df %>%
  mutate(
    # coerce FD lines to numeric safely
    PTS_FD_LINE_O_num   = suppressWarnings(as.numeric(PTS_FD_LINE_O)),
    AST_FD_LINE_O_num   = suppressWarnings(as.numeric(AST_FD_LINE_O)),
    REB_FD_LINE_O_num   = suppressWarnings(as.numeric(REB_FD_LINE_O)),
    `3PM_FD_LINE_O_num` = suppressWarnings(as.numeric(`3PM_FD_LINE_O`)),
    STL_FD_LINE_O_num   = suppressWarnings(as.numeric(STL_FD_LINE_O)),
    BLK_FD_LINE_O_num   = suppressWarnings(as.numeric(BLK_FD_LINE_O)),
    
    # box-score stats as numeric
    PTS_num    = suppressWarnings(as.numeric(PTS)),
    AST_num    = suppressWarnings(as.numeric(AST)),
    REB_num    = suppressWarnings(as.numeric(REB)),
    `3PTM_num` = suppressWarnings(as.numeric(`3PTM`)),
    STL_num    = suppressWarnings(as.numeric(STL)),
    BLK_num    = suppressWarnings(as.numeric(BLK)),
    
    # cover flags: 1 if FD line > actual stat, else 0
    PTS_COVER  = if_else(!is.na(PTS_FD_LINE_O_num)   & !is.na(PTS_num)   & PTS_FD_LINE_O_num   > PTS_num,   1L, 0L),
    AST_COVER  = if_else(!is.na(AST_FD_LINE_O_num)   & !is.na(AST_num)   & AST_FD_LINE_O_num   > AST_num,   1L, 0L),
    REB_COVER  = if_else(!is.na(REB_FD_LINE_O_num)   & !is.na(REB_num)   & REB_FD_LINE_O_num   > REB_num,   1L, 0L),
    `3PM_COVER`= if_else(!is.na(`3PM_FD_LINE_O_num`) & !is.na(`3PTM_num`) & `3PM_FD_LINE_O_num` > `3PTM_num`, 1L, 0L),
    STL_COVER  = if_else(!is.na(STL_FD_LINE_O_num)   & !is.na(STL_num)   & STL_FD_LINE_O_num   > STL_num,   1L, 0L),
    BLK_COVER  = if_else(!is.na(BLK_FD_LINE_O_num)   & !is.na(BLK_num)   & BLK_FD_LINE_O_num   > BLK_num,   1L, 0L),
    
    # cover amounts: |actual - line|
    PTS_COVER_AMT  = if_else(!is.na(PTS_FD_LINE_O_num)   & !is.na(PTS_num),
                             abs(PTS_num    - PTS_FD_LINE_O_num),   NA_real_),
    AST_COVER_AMT  = if_else(!is.na(AST_FD_LINE_O_num)   & !is.na(AST_num),
                             abs(AST_num    - AST_FD_LINE_O_num),   NA_real_),
    REB_COVER_AMT  = if_else(!is.na(REB_FD_LINE_O_num)   & !is.na(REB_num),
                             abs(REB_num    - REB_FD_LINE_O_num),   NA_real_),
    `3PM_COVER_AMT`= if_else(!is.na(`3PM_FD_LINE_O_num`) & !is.na(`3PTM_num`),
                             abs(`3PTM_num` - `3PM_FD_LINE_O_num`), NA_real_),
    STL_COVER_AMT  = if_else(!is.na(STL_FD_LINE_O_num)   & !is.na(STL_num),
                             abs(STL_num    - STL_FD_LINE_O_num),   NA_real_),
    BLK_COVER_AMT  = if_else(!is.na(BLK_FD_LINE_O_num)   & !is.na(BLK_num),
                             abs(BLK_num    - BLK_FD_LINE_O_num),   NA_real_)
  ) %>%
  # drop temp numeric helper columns
  select(
    -PTS_FD_LINE_O_num, -AST_FD_LINE_O_num, -REB_FD_LINE_O_num,
    -`3PM_FD_LINE_O_num`, -STL_FD_LINE_O_num, -BLK_FD_LINE_O_num,
    -PTS_num, -AST_num, -REB_num, -`3PTM_num`, -STL_num, -BLK_num
  )

cat("✓ Built *_COVER and *_COVER_AMT (PTS, AST, REB, 3PM, STL, BLK).\n")

#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 20. END: Build stat cover flags + cover amounts (FD Over line vs actual box score) ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 22. START: Save Final_Roster_df and Final_Roster_today to nba_player_odds_enrich files ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# ---- Write Final_Roster_df ----
final_roster_season_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/nba_player_odds_enrich_",
  season_token,
  ".csv"
)

fwrite(
  Final_Roster_df,
  file      = final_roster_season_path,
  append    = FALSE,   # overwrite
  col.names = TRUE
)

cat("✓ Final_Roster_df written to:\n", final_roster_season_path, "\n")


# ---- Write Final_Roster_today ----
final_roster_daily_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/nba_player_odds_enrich_",
  formatted_date,
  ".csv"
)

fwrite(
  Final_Roster_today,
  file      = final_roster_daily_path,
  append    = FALSE,   # overwrite
  col.names = TRUE
)

cat("✓ Final_Roster_today written to:\n", final_roster_daily_path, "\n")


#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== 22. END: Save Final_Roster_df and Final_Roster_today to nba_player_odds_enrich files ====
#🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀