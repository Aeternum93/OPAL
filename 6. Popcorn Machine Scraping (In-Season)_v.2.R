# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== START: NBA Schedule Load and Configuration ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿

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
library(rvest)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)
library(purrr)
library(data.table)



lineup_dataprev <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/pm_lineup_data_",
  season_token,
  ".csv"
)

# 1.1 Load nba_schedule from NBA API (full season)
nba_schedule <- load_nba_schedule(as.numeric(formatted_year))

# 1.2 Ensure game_date is Date, keep only past games, sort
nba_schedule <- nba_schedule %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(game_date < as.Date(current_date)) %>%
  arrange(game_date)

# 1.3 Define columns to remove (same cleanup set)
columns_to_remove <- c(
  "attendance", "time_valid", "neutral_site", "play_by_play_available", "recent", 
  "start_date", "broadcast", "highlights", "notes_type", "notes_headline", 
  "broadcast_market", "broadcast_name", "venue_id", "venue_full_name", 
  "venue_address_city", "venue_indoor", "status_clock", "status_display_clock", 
  "status_type_id", "status_type_name", "status_type_state", "status_type_completed", 
  "status_type_description", "status_type_detail", "status_type_short_detail", 
  "format_regulation_periods", "home_uid", "home_location", "home_name", 
  "home_is_active", "home_venue_id", "home_linescores", "home_records", 
  "away_uid", "away_location", "away_name", 
  "away_is_active", "away_venue_id", "away_linescores", "away_records", 
  "venue_address_state", "status_type_alt_detail", "game_json", "game_json_url", 
  "PBP", "team_box", "type_abbreviation", "type_id", "player_box"
)

nba_schedule <- nba_schedule[, !(names(nba_schedule) %in% columns_to_remove)]

# 1.4 Abbreviation fixes on raw home/away abbreviations
nba_schedule <- nba_schedule %>%
  mutate(
    home_abbreviation = case_when(
      home_abbreviation == "GS"   ~ "GSW",
      home_abbreviation == "NY"   ~ "NYK",
      home_abbreviation == "WSH"  ~ "WAS",
      home_abbreviation == "NO"   ~ "NOP",
      home_abbreviation == "SA"   ~ "SAS",
      home_abbreviation == "UTAH" ~ "UTA",
      TRUE ~ home_abbreviation
    ),
    away_abbreviation = case_when(
      away_abbreviation == "GS"   ~ "GSW",
      away_abbreviation == "NY"   ~ "NYK",
      away_abbreviation == "WSH"  ~ "WAS",
      away_abbreviation == "UTAH" ~ "UTA",
      away_abbreviation == "SA"   ~ "SAS",
      away_abbreviation == "NO"   ~ "NOP",
      TRUE ~ away_abbreviation
    )
  )

# 1.5 Format game_date_time as time-only in CST
if ("game_date_time" %in% names(nba_schedule)) {
  nba_schedule$game_date_time <- as.POSIXct(
    nba_schedule$game_date_time,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  nba_schedule$game_date_time <- format(
    nba_schedule$game_date_time,
    tz = "America/Chicago",
    format = "%H:%M:%S"
  )
}

# 1.6 Rename home/away -> team/opp (one row per game; NO flip)
nba_schedule <- nba_schedule %>%
  rename(
    team_id               = home_id,
    team                  = home_abbreviation,
    team_display_name     = home_display_name,
    team_short_display_name = home_short_display_name,
    team_color            = home_color,
    team_logo             = home_logo,
    team_score            = home_score,
    team_winner           = home_winner,
    opp_id                = away_id,
    opp                   = away_abbreviation,
    opp_display_name      = away_display_name,
    opp_short_display_name = away_short_display_name,
    opp_color             = away_color,
    opp_logo              = away_logo,
    opp_score             = away_score,
    opp_winner            = away_winner,
    opp_alternate_color   = away_alternate_color
  ) %>%
  mutate(
    home_away = "home",
    is_home   = 1L,
    is_away   = 0L,
    home_away_sym = "vs."
  )

# 1.7 Add Popcorn Machine mapping fields (PopMac_Game_Date, PopMac_GAME, PM_team/PM_opp)
nba_schedule <- nba_schedule %>%
  mutate(
    PopMac_Game_Date = format(as.Date(game_date), "%Y%m%d"),
    PM_team = team,
    PM_opp  = opp
  ) %>%
  mutate(
    PM_team = case_when(
      PM_team == "NOP" ~ "NOR",
      PM_team == "SA"  ~ "SAS",
      PM_team == "PHX" ~ "PHO",
      PM_team == "UTA" ~ "UTH",
      TRUE ~ PM_team
    ),
    PM_opp = case_when(
      PM_opp == "NOP" ~ "NOR",
      PM_opp == "SA"  ~ "SAS",
      PM_opp == "PHX" ~ "PHO",
      PM_opp == "UTA" ~ "UTH",
      TRUE ~ PM_opp
    ),
    PopMac_GAME = paste0(trimws(PM_opp), trimws(PM_team))
  )

# 1.8 Optionally keep a season-level object like the rest of your codebase
assign(paste0("nba_schedule_", season_token), nba_schedule)

# 1.9 Load existing pm_lineup_data_<season>.csv (if present)
if (file.exists(lineup_dataprev) && file.info(lineup_dataprev)$size > 0) {
  lineup_dataprev <- read.csv(
    file   = lineup_dataprev,
    sep    = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
} else {
  lineup_dataprev <- data.frame()  # empty if file doesn't exist or is empty
}

# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== END: NBA Schedule Load and Configuration ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿



# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== START: Date Comparison Check ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿

# ------------------------------------------------------------
# BOOTSTRAP GUARD:
# If pm_lineup_data file does not exist or is empty,
# treat this as a first-run and scrape ALL dates
# ------------------------------------------------------------
if (nrow(lineup_dataprev) == 0) {
  
  message("Popcorn Machine lineup file missing or empty → full bootstrap run")
  
  # All schedule dates are considered missing
  MISSING_DATES <- unique(as.character(nba_schedule$PopMac_Game_Date))
  
  print("Missing dates identified (formatted):")
  print(MISSING_DATES)
  
  lineup_dataprev_filtered <- nba_schedule %>%
    filter(PopMac_Game_Date %in% MISSING_DATES)
  
} else {
  
  # ----------------------------------------------------------
  # ORIGINAL LOGIC (UNCHANGED) – SAFE TO RUN
  # ----------------------------------------------------------
  
  # 1.4 Format PopMac_Date / PopMac_Game_Date as character
  if (!"PopMac_Date" %in% names(lineup_dataprev)) {
    lineup_dataprev$PopMac_Date <- NA_character_
  }
  
  lineup_dataprev$PopMac_Date   <- as.character(lineup_dataprev$PopMac_Date)
  nba_schedule$PopMac_Game_Date <- as.character(nba_schedule$PopMac_Game_Date)
  
  # 1.5 Debugging: Check for NA values
  if (any(is.na(lineup_dataprev$PopMac_Date))) {
    print("Warning: NA values found in lineup_dataprev$PopMac_Date")
  }
  if (any(is.na(nba_schedule$PopMac_Game_Date))) {
    print("Warning: NA values found in nba_schedule$PopMac_Game_Date")
  }
  
  # 1.6 Unique date lists
  PM_Dates    <- unique(lineup_dataprev$PopMac_Date)
  NBASC_Dates <- unique(nba_schedule$PopMac_Game_Date)
  
  PM_Dates    <- as.character(PM_Dates)
  NBASC_Dates <- as.character(NBASC_Dates)
  
  # 1.7 Identify missing dates in pm_lineup_data_
  MISSING_DATES <- setdiff(NBASC_Dates, PM_Dates)
  
  print("Missing dates identified (formatted):")
  print(MISSING_DATES)
  
  # 1.8 Filter nba_schedule for rows with missing dates (these are the games to scrape)
  lineup_dataprev_filtered <- nba_schedule %>%
    filter(PopMac_Game_Date %in% MISSING_DATES)
}

# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== END: Date Comparison Check ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿


############################################

# Initialize the start_time variable
start_time <- Sys.time()

# Replace placeholders with actual values
for (i in seq_len(nrow(lineup_dataprev_filtered))) {
  PopMac_Game <- lineup_dataprev_filtered$PopMac_GAME[i]
  PopMac_Date <- lineup_dataprev_filtered$PopMac_Game_Date[i]
  PopMac_T1 <- lineup_dataprev_filtered$PM_opp[i]
  PopMac_T2 <- lineup_dataprev_filtered$PM_team[i]
  url <- paste0("https://www.popcornmachine.net/gf?date=", trimws(PopMac_Date), "&game=", trimws(PopMac_Game))
  
  # Parse HTML page
  page <- read_html(url)
  cat(sprintf("Processing URL: %s\n", url))
  cat(sprintf("Processing game %d of %d. Games left: %d\n", i, nrow(lineup_dataprev_filtered), nrow(lineup_dataprev_filtered) - i))
  
  # Calculate elapsed time
  elapsed_time <- Sys.time() - start_time
  cat(sprintf("Game processed in %.2f seconds\n", as.numeric(elapsed_time, units = "secs")))
  start_time <- Sys.time()
  
  # Extract raw stats
  player_stats <- page %>% html_nodes(".text1") %>% html_text(trim = TRUE)
  
  # Pre-clean the raw data
  player_stats <- gsub("&nbsp;", " ", player_stats)      # Replace non-breaking spaces
  player_stats <- gsub("Â", "'", player_stats, fixed = TRUE) # Replace "Â’" explicitly with "'"
  player_stats <- trimws(player_stats)                  # Trim whitespace
  
  # Debugging: Save cleaned raw stats to a CSV
  write_csv(data.frame(Stats = player_stats), "debug_cleaned_stats.csv")
  print("Cleaned raw stats saved to debug_cleaned_stats.csv")
  
  # Ensure 'period_rows' exists before trying to find min/max
  period_rows <- which(grepl("^Period", player_stats))
  if (length(period_rows) > 0) {
    first_period_row <- min(period_rows)
    last_period_row <- max(period_rows)
  } else {
    first_period_row <- NA
    last_period_row <- NA
  }
  
  # Ensure other sections handle cases where no "Period" rows exist
  # (additional safeguards included below in your processing steps)
  
  
  # Process Player Stats
  player_stats_tab1 <- data.frame(Stats = player_stats) %>%
    mutate(
      Player = ifelse(
        grepl("Game", Stats),
        str_extract(Stats, ".*(?=Game)"), # Extract everything before "Game"
        ifelse(grepl("Stint", Stats),
               str_extract(Stats, ".*(?= Stint)"), # Extract everything before " Stint"
               NA)
      ),
      Stints = ifelse(grepl("Stint [0-9]+", Stats), str_extract(Stats, "Stint [0-9]+"), NA),
      Periods = ifelse(grepl("Period [0-9]+", Stats), str_extract(Stats, "Period [0-9]+"), NA),
      Min = ifelse(
        grepl("Game", Stats),
        str_extract(Stats, "\\d{1,2}:\\d{2}"), # Extract mm:ss format
        ifelse(grepl("Stint", Stats),
               str_extract(Stats, "\\d{1,2}:\\d{2}"),
               NA)
      ),
      FG = ifelse(grepl("FG [0-9]+-[0-9]+", Stats), str_extract(Stats, "FG [0-9]+-[0-9]+"), NA),
      FGA = ifelse(!is.na(FG), as.numeric(str_extract(FG, "[0-9]+(?=-)")), NA),
      FGM = ifelse(!is.na(FG), as.numeric(str_extract(FG, "(?<=-)[0-9]+")), NA),
      Ast = ifelse(grepl("Ast [0-9]+", Stats), as.numeric(str_extract(Stats, "(?<=Ast )[0-9]+")), NA),
      Reb = ifelse(grepl("Reb [0-9]+", Stats), as.numeric(str_extract(Stats, "(?<=Reb )[0-9]+")), NA),
      TO = ifelse(grepl("TO [0-9]+", Stats), as.numeric(str_extract(Stats, "(?<=TO )[0-9]+")), NA),
      Time_On = ifelse(grepl("\\d{1,2}:\\d{2}\\s→", Stats), str_extract(Stats, "\\d{1,2}:\\d{2}(?=\\s→)"), NA),
      Time_Off = ifelse(grepl("\\s→\\s\\d{1,2}:\\d{2}", Stats), str_extract(Stats, "(?<=\\s→\\s)\\d{1,2}:\\d{2}"), NA),
      P_M = ifelse(
        grepl("Game", Stats),
        as.numeric(str_extract(Stats, "[\\+\\-]?\\d+(?= FG)")), # Extract P_M before "FG" for Game rows
        ifelse(grepl("Stint", Stats),
               as.numeric(str_extract(Stats, "(?<=\\(\\+/-\\):)[\\+\\-]?\\d+")), # Extract P_M for Stint rows
               NA)
      ),
      Team = ifelse(
        row_number() < first_period_row, PopMac_T1,
        ifelse(row_number() > last_period_row, PopMac_T2, NA)
      ),
      Opp = ifelse(
        row_number() < first_period_row, PopMac_T2,
        ifelse(row_number() > last_period_row, PopMac_T1, NA)
      ),
      PopMac_Date = PopMac_Date,
      PopMac_Game = PopMac_Game
    ) %>%
    group_by(Player) %>%
    mutate(
      P_M = ifelse(is.na(Periods) & is.na(Stints), sum(P_M, na.rm = TRUE), P_M)
    ) %>%
    ungroup() %>%
    mutate(game_id = paste(PopMac_Game, PopMac_Date, sep = "_")) %>%
    select(PopMac_Game, Team, Opp, PopMac_Date, game_id, Player, Periods, Stints, Time_On, Time_Off, P_M, Min, FGA, FGM, Ast, Reb, TO ) %>%
    filter(!is.na(Player) | !is.na(Min) | !is.na(FGA) | !is.na(FGM) | !is.na(Ast) | !is.na(Reb) | !is.na(TO))
  
  
  file_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/pm_player_stats_", season_token,".csv")
  
  
  # Append or create a new file
  fwrite(player_stats_tab1, file_path, append = file.exists(file_path))
  print("Processed Player Stats Data written to player_stats.csv")
  
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== START: Map Player Names from hoopr_name_mapping file  ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
  # Read name list and team mapping data
  hoopr_name_map <- fread(
    "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv",
    colClasses = "character"
  ) %>%
    mutate(
      pm_player_name = str_trim(pm_player_name)
    ) %>%
    filter(
      !is.na(pm_player_name),
      pm_player_name != "-",
      pm_player_name != ""
    )
  
  # vector used for string detection (drop-in replacement for name_list)
  name_list <- hoopr_name_map$pm_player_name
  
  team_mapping_raw <- read.delim("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/PopMac_team_mapping.txt", header = FALSE)
  
  # Fix the Trail Blazers case before separating the columns
  team_mapping_raw <- team_mapping_raw %>%
    mutate(V1 = ifelse(grepl("Trail Blazers", V1), "Trail Blazers POR", V1))
  
  team_mapping <- team_mapping_raw %>%
    separate(V1, into = c("TeamName", "MappedTeam"), sep = " ") %>%
    mutate(
      TeamName = ifelse(TeamName == "Trail", "Trail Blazers", TeamName),
      MappedTeam = ifelse(MappedTeam == "Blazers", "POR", MappedTeam)
    )
  
  # Create a list of players and teams from player_stats_tab1
  player_team_list <- player_stats_tab1 %>%
    select(Player, Team) %>%
    filter(!is.na(Player) & !is.na(Team))
  
  # Process lineup data
  player_stats_tab2 <- data.frame(Stats = player_stats) %>%
    # Clean up names by replacing the "Â’" sequence with an apostrophe
    mutate(
      Stats = gsub("\u0092", "'", Stats, fixed = TRUE),
      Stats = gsub("D\u0092", "D'", Stats, fixed = TRUE),
      Stats = gsub("▯", " ", Stats, fixed = TRUE),
      Stats = gsub("Â", "'", Stats, fixed = TRUE),
      Stats = stringr::str_squish(Stats)
    ) %>%
    # Replace "Â’" with "'"
    filter(grepl("^Period", Stats)) %>%  # Keep only rows starting with "Period"
    mutate(
      Period = str_extract(Stats, "^Period [0-9]+"),
      Time_On = str_extract(Stats, "\\d{1,2}:\\d{2}(?=\\s→)"),
      Time_Off = str_extract(Stats, "(?<=\\s→\\s)\\d{1,2}:\\d{2}"),
      Team_Long = ifelse(
        !is.na(Time_Off) & mapply(grepl, Time_Off, Stats),
        str_trim(mapply(function(time, stat) {
          if (!is.na(time)) {
            str_extract(stat, paste0("(?<=", time, ").*?(?=\\slineup)"))
          } else {
            NA
          }
        }, Time_Off, Stats)),
        NA
      ),
      Team = map_chr(Team_Long, ~ {
        match_row <- which(team_mapping$TeamName == .x)
        if (length(match_row) == 1) {
          team_mapping$MappedTeam[match_row]
        } else if (length(match_row) > 1) {
          warning("Multiple matches found; taking the first.")
          team_mapping$MappedTeam[match_row[1]]
        } else {
          NA
        }
      }),
      Opp = ifelse(Team == PopMac_T1, PopMac_T2, ifelse(Team == PopMac_T2, PopMac_T1, NA)),
      P_M = str_extract(Stats, "(?<=lineup\\s)[\\+\\-]?\\d+"),
      Players = sub(".*lineup\\s[\\+\\-]?\\d+", "", Stats),
      PopMac_Date = PopMac_Date,
      PopMac_Game = PopMac_Game
    ) %>%
    mutate(
      Matched_Names = map(Stats, ~ name_list[str_detect(.x, fixed(name_list))]), # Match players using the name list
      P1 = map_chr(Matched_Names, ~ .x[1] %||% NA_character_),
      P2 = map_chr(Matched_Names, ~ .x[2] %||% NA_character_),
      P3 = map_chr(Matched_Names, ~ .x[3] %||% NA_character_),
      P4 = map_chr(Matched_Names, ~ .x[4] %||% NA_character_),
      P5 = map_chr(Matched_Names, ~ .x[5] %||% NA_character_)
    ) %>%
    mutate(
      Matched_Names = map(Stats, ~ name_list[str_detect(.x, fixed(name_list))]),
      P1 = map_chr(Matched_Names, ~ .x[1] %||% NA_character_),
      P2 = map_chr(Matched_Names, ~ .x[2] %||% NA_character_),
      P3 = map_chr(Matched_Names, ~ .x[3] %||% NA_character_),
      P4 = map_chr(Matched_Names, ~ .x[4] %||% NA_character_),
      P5 = map_chr(Matched_Names, ~ .x[5] %||% NA_character_)
    ) %>%
    ungroup() %>%
    mutate(
      P1_nba_player_id  = hoopr_name_map$nba_player_id[
        match(P1, hoopr_name_map$pm_player_name)
      ],
      P1_espn_player_id = hoopr_name_map$espn_player_id[
        match(P1, hoopr_name_map$pm_player_name)
      ],
      
      P2_nba_player_id  = hoopr_name_map$nba_player_id[
        match(P2, hoopr_name_map$pm_player_name)
      ],
      P2_espn_player_id = hoopr_name_map$espn_player_id[
        match(P2, hoopr_name_map$pm_player_name)
      ],
      
      P3_nba_player_id  = hoopr_name_map$nba_player_id[
        match(P3, hoopr_name_map$pm_player_name)
      ],
      P3_espn_player_id = hoopr_name_map$espn_player_id[
        match(P3, hoopr_name_map$pm_player_name)
      ],
      
      P4_nba_player_id  = hoopr_name_map$nba_player_id[
        match(P4, hoopr_name_map$pm_player_name)
      ],
      P4_espn_player_id = hoopr_name_map$espn_player_id[
        match(P4, hoopr_name_map$pm_player_name)
      ],
      
      P5_nba_player_id  = hoopr_name_map$nba_player_id[
        match(P5, hoopr_name_map$pm_player_name)
      ],
      P5_espn_player_id = hoopr_name_map$espn_player_id[
        match(P5, hoopr_name_map$pm_player_name)
      ]
    ) %>%
    mutate(game_id = paste(PopMac_Game, PopMac_Date, sep = "_")) %>%
    select(PopMac_Game, PopMac_Date, game_id, Period, Time_On, Time_Off, Team_Long, Team, Opp, P_M, P1, P2, P3, P4, P5) %>% # Include Opp and Team_Long
    filter(!is.na(P1))  # Keep only rows where at least one player is matched
  
  # File path
  file_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/pm_lineup_data_", season_token, ".csv")
  
  # Append or create a new file
  fwrite(player_stats_tab2, file_path, append = file.exists(file_path))
  print("Processed Lineup Data written to lineup_data.csv")
  
  Sys.sleep(2)
}  # <- This is the closing brace for the loop

rm(page, player_stats_tab1, player_stats_tab2, player_team_list, team_mapping_raw, lineup_dataprev, lineup_dataprev_filtered, team_mapping)

str(current_date)
str(formatted_date)
str(formatted_year)
str(next_year_date)
str(pbp_season)
str(season_token)
str(season_token2)


# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== END: Popcorn Machine Loop Logic through nba_schedule to get Popcorn Machine data for missing dates ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿