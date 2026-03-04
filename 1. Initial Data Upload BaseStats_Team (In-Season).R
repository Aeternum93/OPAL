# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#     oooooooooo.               o8o  oooo        .o8       oooooooooo.                                .oooooo..o     .                 .                        ooooooooooooo                                            oooo  .oooooo..o ooooooooooooo       .o.       ooooooooo.   ooooooooooooo oooo 
#     `888'   `Y8b              `"'  `888       "888       `888'   `Y8b                              d8P'    `Y8   .o8               .o8                        8'   888   `8                                            8    d8P'    `Y8 8'   888   `8      .888.      `888   `Y88. 8'   888   `8    8 
#      888     888 oooo  oooo  oooo   888   .oooo888        888     888  .oooo.    .oooo.o  .ooooo.  Y88bo.      .o888oo  .oooo.   .o888oo  .oooo.o                  888       .ooooo.   .oooo.   ooo. .oo.  .oo.        8    Y88bo.           888          .8"888.      888   .d88'      888         8 
#      888oooo888' `888  `888  `888   888  d88' `888        888oooo888' `P  )88b  d88(  "8 d88' `88b  `"Y8888o.    888   `P  )88b    888   d88(  "8                  888      d88' `88b `P  )88b  `888P"Y88bP"Y88b       8     `"Y8888o.       888         .8' `888.     888ooo88P'       888         8 
#      888    `88b  888   888   888   888  888   888        888    `88b  .oP"888  `"Y88b.  888ooo888      `"Y88b   888    .oP"888    888   `"Y88b.                   888      888ooo888  .oP"888   888   888   888       8         `"Y88b      888        .88ooo8888.    888`88b.         888         8 
#      888    .88P  888   888   888   888  888   888        888    .88P d8(  888  o.  )88b 888    .o oo     .d8P   888 . d8(  888    888 . o.  )88b                  888      888    .o d8(  888   888   888   888       8    oo     .d8P      888       .8'     `888.   888  `88b.       888         8 
#     o888bood8P'   `V88V"V8P' o888o o888o `Y8bod88P"      o888bood8P'  `Y888""8o 8""888P' `Y8bod8P' 8""88888P'    "888" `Y888""8o   "888" 8""888P' ooooooooooo     o888o     `Y8bod8P' `Y888""8o o888o o888o o888o      8ooo 8""88888P'      o888o     o88o     o8888o o888o  o888o     o888o     ooo8 
#
#
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Date configuration & library loading logic ====
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

# 0.0 Remove previous data from cache and load hoopR data
gc()
library(hoopR)
library(dplyr)
library(parallel)
library(stringr)
library(readr)
library(data.table)
library(future.apply)

# 0.1 Store today's date dynamically
current_date <- Sys.Date()

# 0.2 Add one year to current_date
next_year_date <- current_date + lubridate::years(1) 

# 0.3 Generate dynamic year and formatted date
formatted_date <- format(current_date, "%Y%m%d")        # Today's date in 'yyyMMdd' format
formatted_year <- as.numeric(format(current_date, "%Y"))
formatted_year <- as.character(formatted_year)
pbp_season <- as.numeric(2026)
season_token <- "2025_2026" # Convert back to character if needed
season_token2 <- "2025-26"
# 0.4 Set up parallel processing (adjust workers based on your system)
plan(multisession, workers = 7)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== END: Date configuration & library loading logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: NBA Schedule For Current Day Logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 1.1 Load nba_schedule data frame dynamically for both formatted_year and formatted_yearMMdd
nba_schedule <- load_nba_schedule(as.numeric(formatted_year))

# 1.2 Assign nba_scheduleformatted_year (entire year schedule)
assign(paste0("nba_schedule_", season_token), nba_schedule)

# 1.3 Filter for the current date to create nba_scheduleformatted_date
nba_schedule_current_date <- nba_schedule %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d")) %>%
  filter(game_date == as.Date(current_date, format = "%Y-%m-%d"))

# Step 1: Extract the static rows (original schedule for the day)
nba_schedule_static <- nba_schedule_current_date

# Step 2: Add concatenated columns for home and away teams
nba_schedule_static <- nba_schedule_static %>%
  mutate(
    home_concat = paste0(home_abbreviation, "home"),
    away_concat = paste0(away_abbreviation, "away")
  )

# Step 3: Duplicate the static rows for the flips
nba_schedule_flipped <- nba_schedule_static

# Step 4: Perform both flips directly on the duplicated rows
# Away-to-Home Flip
nba_schedule_flipped$away_id <- nba_schedule_static$home_id
nba_schedule_flipped$away_abbreviation <- nba_schedule_static$home_abbreviation
nba_schedule_flipped$away_display_name <- nba_schedule_static$home_display_name
nba_schedule_flipped$away_color <- nba_schedule_static$home_color
nba_schedule_flipped$away_alternate_color <- nba_schedule_static$home_alternate_color
nba_schedule_flipped$away_logo <- nba_schedule_static$home_logo
nba_schedule_flipped$away_score <- nba_schedule_static$home_score
nba_schedule_flipped$away_winner <- nba_schedule_static$home_winner
nba_schedule_flipped$away_short_display_name <- nba_schedule_static$home_short_display_name 

# Home-to-Away Flip
nba_schedule_flipped$home_id <- nba_schedule_static$away_id
nba_schedule_flipped$home_abbreviation <- nba_schedule_static$away_abbreviation
nba_schedule_flipped$home_display_name <- nba_schedule_static$away_display_name
nba_schedule_flipped$home_color <- nba_schedule_static$away_color
nba_schedule_flipped$home_alternate_color <- nba_schedule_static$away_alternate_color
nba_schedule_flipped$home_logo <- nba_schedule_static$away_logo
nba_schedule_flipped$home_score <- nba_schedule_static$away_score
nba_schedule_flipped$home_winner <- nba_schedule_static$away_winner
nba_schedule_flipped$home_short_display_name  <- nba_schedule_static$away_short_display_name

# Optional: Add a column to distinguish static rows from flipped rows
nba_schedule_static$is_static <- TRUE
nba_schedule_flipped$is_static <- FALSE

# Step 5: Append the flipped rows back to the static rows
nba_schedule_expanded <- bind_rows(nba_schedule_static, nba_schedule_flipped)

# Step 6: Parse concatenated values and populate home_away column
nba_schedule_expanded <- nba_schedule_expanded %>%
  mutate(
    home_away = case_when(
      grepl("home", home_concat) ~ "home",
      grepl("away", away_concat) ~ "away",
      TRUE ~ NA_character_
    )
  )

# Step 7: Add new columns with conditional logic based on home_away
nba_schedule_expanded <- nba_schedule_expanded %>%
  mutate(
    is_home = ifelse(home_away == "home", 1, 0),
    is_away = ifelse(home_away == "away", 1, 0),
    home_away_sym = case_when(
      home_away == "home" ~ "vs.",
      home_away == "away" ~ "@",
      TRUE ~ NA_character_
    )
  )

# Remove logic for PopMac_GAME and PopMac_Game_Date
# Assign updated nba_schedule(formatted_year) back
assign(paste0("nba_schedule_", season_token), nba_schedule)

# Assign the expanded schedule to nba_scheduleformatted_date
assign(paste0("nba_schedule", formatted_date), nba_schedule_expanded)

# 1.1 Load nba_schedule data frame dynamically for both formatted_year and formatted_yearMMdd
nba_schedule <- load_nba_schedule(as.numeric(formatted_year))

# ==== FULL-SEASON FLIP (run this immediately after loading) ====

# Full-season static rows (original schedule for all games)
nba_schedule_static_full <- nba_schedule %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"))

# Add concatenated columns for home and away teams
nba_schedule_static_full <- nba_schedule_static_full %>%
  mutate(
    home_concat = paste0(home_abbreviation, "home"),
    away_concat = paste0(away_abbreviation, "away")
  )

# Duplicate the static rows for the flips (full season)
nba_schedule_flipped_full <- nba_schedule_static_full

# Away-to-Home Flip
nba_schedule_flipped_full$away_id                 <- nba_schedule_static_full$home_id
nba_schedule_flipped_full$away_abbreviation       <- nba_schedule_static_full$home_abbreviation
nba_schedule_flipped_full$away_display_name       <- nba_schedule_static_full$home_display_name
nba_schedule_flipped_full$away_color              <- nba_schedule_static_full$home_color
nba_schedule_flipped_full$away_alternate_color    <- nba_schedule_static_full$home_alternate_color
nba_schedule_flipped_full$away_logo               <- nba_schedule_static_full$home_logo
nba_schedule_flipped_full$away_score              <- nba_schedule_static_full$home_score
nba_schedule_flipped_full$away_winner             <- nba_schedule_static_full$home_winner
nba_schedule_flipped_full$away_short_display_name <- nba_schedule_static_full$home_short_display_name 

# Home-to-Away Flip
nba_schedule_flipped_full$home_id                 <- nba_schedule_static_full$away_id
nba_schedule_flipped_full$home_abbreviation       <- nba_schedule_static_full$away_abbreviation
nba_schedule_flipped_full$home_display_name       <- nba_schedule_static_full$away_display_name
nba_schedule_flipped_full$home_color              <- nba_schedule_static_full$away_color
nba_schedule_flipped_full$home_alternate_color    <- nba_schedule_static_full$away_alternate_color
nba_schedule_flipped_full$home_logo               <- nba_schedule_static_full$away_logo
nba_schedule_flipped_full$home_score              <- nba_schedule_static_full$away_score
nba_schedule_flipped_full$home_winner             <- nba_schedule_static_full$away_winner
nba_schedule_flipped_full$home_short_display_name <- nba_schedule_static_full$away_short_display_name

# Optional flags
nba_schedule_static_full$is_static  <- TRUE
nba_schedule_flipped_full$is_static <- FALSE

# Bind static + flipped for the full season
nba_schedule_expanded_full <- bind_rows(nba_schedule_static_full, nba_schedule_flipped_full)

# Parse concatenated values and populate home_away + flags
nba_schedule_expanded_full <- nba_schedule_expanded_full %>%
  mutate(
    home_away = case_when(
      grepl("home", home_concat) ~ "home",
      grepl("away", away_concat) ~ "away",
      TRUE ~ NA_character_
    ),
    is_home = ifelse(home_away == "home", 1, 0),
    is_away = ifelse(home_away == "away", 1, 0),
    home_away_sym = case_when(
      home_away == "home" ~ "vs.",
      home_away == "away" ~ "@",
      TRUE ~ NA_character_
    )
  )

# 👉 This is now your season-level schedule object that will go through all the cleanup logic below
assign(paste0("nba_schedule_", season_token), nba_schedule_expanded_full)
# ==== END NEW ====
# 1.4 Define the list of columns to remove
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

# 1.5 Dynamically remove columns from both nba_scheduleformatted_year and nba_scheduleformatted_date
for (name in c(paste0("nba_schedule_", season_token), paste0("nba_schedule", formatted_date))) {
  temp_df <- get(name)
  temp_df <- temp_df[, !(names(temp_df) %in% columns_to_remove)]
  
  # Assign updated data frame back
  assign(name, temp_df)
}

# 1.6 Update abbreviations dynamically
for (name in c(paste0("nba_schedule_", season_token), paste0("nba_schedule", formatted_date))) {
  temp_df <- get(name)
  temp_df <- temp_df %>%
    mutate(
      home_abbreviation = case_when(
        home_abbreviation == "GS" ~ "GSW",
        home_abbreviation == "NY" ~ "NYK",
        home_abbreviation == "WSH" ~ "WAS",
        home_abbreviation == "NO" ~ "NOP",
        home_abbreviation == "SA" ~ "SAS",
        home_abbreviation == "UTAH" ~ "UTA",
        TRUE ~ home_abbreviation
      ),
      away_abbreviation = case_when(
        away_abbreviation == "GS" ~ "GSW",
        away_abbreviation == "NY" ~ "NYK",
        away_abbreviation == "WSH" ~ "WAS",
        away_abbreviation == "UTAH" ~ "UTA",
        away_abbreviation == "SA" ~ "SAS",
        away_abbreviation == "NO" ~ "NOP",
        TRUE ~ away_abbreviation
      )
    )
  assign(name, temp_df)
}

# 1.7 Format "game_date_time" as time only in CST for both data frames
for (name in c(paste0("nba_schedule_", season_token), paste0("nba_schedule", formatted_date))) {
  temp_df <- get(name)
  
  # Convert "game_date_time" to POSIXct and adjust to CST
  temp_df$game_date_time <- as.POSIXct(temp_df$game_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  temp_df$game_date_time <- format(temp_df$game_date_time, tz = "America/Chicago", format = "%H:%M:%S")
  
  # Assign updated data frame back
  assign(name, temp_df)
}

# Add cleanup steps to the existing data manipulation process
for (name in c(paste0("nba_schedule_", season_token), paste0("nba_schedule", formatted_date))) {
  temp_df <- get(name)
  
  # Rename columns first
  temp_df <- temp_df %>%
    rename(
      team_id = home_id,
      team = home_abbreviation,
      team_display_name = home_display_name,
      team_short_display_name = home_short_display_name,
      team_color = home_color,
      team_logo = home_logo,
      team_score = home_score,
      team_winner = home_winner,
      opp_id = away_id,
      opp = away_abbreviation,
      opp_display_name = away_display_name,
      opp_short_display_name = away_short_display_name,
      opp_color = away_color,
      opp_logo = away_logo,
      opp_score = away_score,
      opp_winner = away_winner,
      opp_alternate_color = away_alternate_color
    )
  
  # Add new columns with conditional logic
  temp_df <- temp_df %>%
    mutate(
      home_away = case_when(
        team == opp ~ "away",
        TRUE ~ "home"
      ),
      is_home = ifelse(home_away == "home", 1, 0),
      is_away = ifelse(home_away == "away", 1, 0),
      home_away_sym = case_when(
        home_away == "home" ~ "vs.",
        home_away == "away" ~ "@",
        TRUE ~ NA_character_
      )
    )
  
  # Assign the updated data frame back
  assign(name, temp_df)
}

# 1.8 Remove unused variables
rm(temp_df, columns_to_remove, nba_schedule, nba_schedule_static, nba_schedule_flipped, nba_schedule_current_date)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: NBA Schedule For Current Day Logic ==== 
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: NBA Schedule For Current Day Logic Clean Up Logic  ==== 
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 1.a.1 Dynamically reference the data frame
data_frame_name <- paste0("nba_schedule", formatted_date)
nba_schedule <- get(data_frame_name)  # Retrieve the data frame

# 1.a.2 Update the `home_away` column and related logic
nba_schedule <- nba_schedule %>%
  mutate(
    # Update `home_away` column
    home_away = case_when(
      str_detect(home_concat, paste0("^", team)) ~ str_extract(home_concat, "(home|away)"),  # Match home team
      str_detect(away_concat, paste0("^", team)) ~ str_extract(away_concat, "(home|away)"),  # Match away team
      TRUE ~ home_away  # Retain existing value if no match
    ),
    # Update `is_home` and `is_away` columns based on `home_away`
    is_home = ifelse(home_away == "home", 1, 0),
    is_away = ifelse(home_away == "away", 1, 0),
    # Update `home_away_sym` column based on `home_away`
    home_away_sym = case_when(
      home_away == "home" ~ "vs.",
      home_away == "away" ~ "@",
      TRUE ~ NA_character_  # Retain NA for unmatched rows
    ),
    # Add `PopMac_Game_Date` column in yyyyMMdd format
    PopMac_Game_Date = format(as.Date(game_date, format = "%Y-%m-%d"), "%Y%m%d"),
    # Add `PM_team` and `PM_opp` as copies of `team` and `opp`
    PM_team = team,
    PM_opp = opp
  ) %>%
  # Apply abbreviation corrections to PM_team and PM_opp
  mutate(
    PM_team = case_when(
      PM_team == "NOP" ~ "NOR",
      PM_team == "SA" ~ "SAS",
      PM_team == "UTA" ~ "UTH",
      TRUE ~ PM_team
    ),
    PM_opp = case_when(
      PM_opp == "NOP" ~ "NOR",
      PM_opp == "SA" ~ "SAS",
      PM_opp == "UTA" ~ "UTH",
      TRUE ~ PM_opp
    ),
    # Update `PopMac_GAME` as a concatenation of `PM_opp` and `PM_team`
    PopMac_GAME = paste0(trimws(PM_opp), trimws(PM_team))
  )

# 1.a.3 Remove `home_concat` and `away_concat` with error handling
tryCatch({
  nba_schedule <- nba_schedule %>%
    select(-home_concat, -away_concat)  # Drop these columns
}, error = function(e) {
  message("Columns `home_concat` or `away_concat` already removed or do not exist. Skipping column removal.")
})

# 1.a.4 Assign the updated data frame back to its original name
assign(data_frame_name, nba_schedule)

# 1.a.5 Apply the same logic to the `nba_schedule(formatted_year)` data frame
data_frame_year_name <- paste0("nba_schedule_", season_token)
nba_schedule_year <- get(data_frame_year_name)  # Retrieve the data frame

nba_schedule_year <- nba_schedule_year %>%
  mutate(
    # Add `PopMac_Game_Date` column in yyyyMMdd format
    PopMac_Game_Date = format(as.Date(game_date, format = "%Y-%m-%d"), "%Y%m%d"),
    # Add `PM_team` and `PM_opp` as copies of `team` and `opp`
    PM_team = team,
    PM_opp = opp
  ) %>%
  # Apply abbreviation corrections to PM_team and PM_opp
  mutate(
    PM_team = case_when(
      PM_team == "NOP" ~ "NOR",
      PM_team == "SA" ~ "SAS",
      PM_team == "PHX" ~ "PHO",
      PM_team == "UTA" ~ "UTH",
      TRUE ~ PM_team
    ),
    PM_opp = case_when(
      PM_opp == "NOP" ~ "NOR",
      PM_opp == "SA" ~ "SAS",
      PM_opp == "PHX" ~ "PHO",
      PM_opp == "UTA" ~ "UTH",
      TRUE ~ PM_opp
    ),
    # Update `PopMac_GAME` as a concatenation of `PM_opp` and `PM_team`
    PopMac_GAME = paste0(trimws(PM_opp), trimws(PM_team))
  )

# Assign the updated `nba_schedule_year` back to its original name
assign(data_frame_year_name, nba_schedule_year)

# 1.a.6 Remove unnecessary data frames
rm(nba_schedule, nba_schedule_year)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: NBA Schedule For Current Day Logic Clean Up Logic  ==== 
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Initial NBA League Standings File Build Logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 1.b.1 Retrieve and store league standings in a data frame
league_id <- "00"  # NBA league ID
season <- year_to_season(most_recent_nba_season() - 1)  # Previous season
season_type <- "Regular Season"  # Regular season type

NBA_LeagueStandings <- nba_leaguestandings(
  league_id = league_id,
  season = season,
  season_type = season_type
)$Standings  # Extract the Standings tibble

# 1.b.2 Dynamically reference the nba_schedule(formatted_date) data frame
data_frame_name <- paste0("nba_schedule", formatted_date)
nba_schedule <- get(data_frame_name)  # Retrieve the data frame

# 1.b.3 Merge the standings data with the schedule data
nba_schedule <- nba_schedule %>%
  left_join(
    NBA_LeagueStandings %>%
      select(
        TeamName,
        ConferenceRecord,
        TeamID,
        PlayoffRank,
        Conference,
        Division,
        DivisionRecord,
        WINS,
        LOSSES,
        HOME,
        ROAD
      ) %>%
      rename(
        ConfRec = ConferenceRecord,
        NBA_Team_ID = TeamID,
        Rank = PlayoffRank,
        CONF = Conference,
        DIV = Division,
        DivRec = DivisionRecord,
        W = WINS,
        L = LOSSES,
        HomeREC = HOME,
        RoadREC = ROAD
      ),
    by = c("team_short_display_name" = "TeamName")  # Match on team_short_display_name and TeamName
  )

# 1.b.4 Split HomeREC and RoadREC into wins and losses
nba_schedule <- nba_schedule %>%
  mutate(
    HomeW = as.integer(str_extract(HomeREC, "^[0-9]+")),  # Extract wins from HomeREC
    HomeL = as.integer(str_extract(HomeREC, "(?<=-)[0-9]+")),  # Extract losses from HomeREC
    RoadW = as.integer(str_extract(RoadREC, "^[0-9]+")),  # Extract wins from RoadREC
    RoadL = as.integer(str_extract(RoadREC, "(?<=-)[0-9]+"))  # Extract losses from RoadREC
  ) %>%
  select(-HomeREC, -RoadREC)  # Remove original HomeREC and RoadREC columns

# 1.b.5 Assign the updated nba_schedule back to the original data frame
assign(data_frame_name, nba_schedule)

# 1.b.6 View the updated nba_schedule(formatted_date) if needed
str(get(data_frame_name))

# 1.b.7 Clean up stray data frames
rm(nba_schedule, nba_schedule_expanded)

# Define the output file path
league_standings_output <- "C:\\Users\\Austin\\OneDrive\\Desktop\\1\\Data Analytics\\NBA Data\\0. Datahub (Temp)\\1. hoopR\\8. Leauge Standings\\NBA_League Standings.csv"

# Save NBA_LeagueStandings to CSV (overwrite each time)
write.csv(NBA_LeagueStandings, file = league_standings_output, row.names = FALSE)

# Print confirmation message
print(paste("NBA_LeagueStandings has been exported to:", league_standings_output))


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Initial NBA League Standings File Build Logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Create nba-team_box_filtered to get a list of dates that need to pulled ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

##### 2. ***START*** Base-Level Stats Calculation For All Teams Logic (Daily) ----

# 2.1 Path
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token, ".csv"
)

# 2.2 Load BaseStats_Team (if exists)
if (file.exists(team_output_path) && file.info(team_output_path)$size > 0) {
  BaseStats_Team <- read.csv(team_output_path, stringsAsFactors = FALSE)
  if ("nba_game_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_game_id <- as.character(BaseStats_Team$nba_game_id)
  if ("game_date"  %in% names(BaseStats_Team)) BaseStats_Team$game_date  <- as.Date(BaseStats_Team$game_date)
} else {
  BaseStats_Team <- data.frame()
}

# 2.3 Load team box
nba_team_box <- load_nba_team_box()
nba_team_box$game_date <- as.Date(nba_team_box$game_date)

# ---------------- Core branching ----------------

if (nrow(BaseStats_Team) == 0) {
  # =============================
  # First run — build full dataset
  # =============================
  message("BaseStats_Team is blank — skipping date check and building full dataset.")
  build_src <- nba_team_box
} else {
  # =============================
  # Ongoing run — filter missing dates
  # =============================
  BSGAME_DATEs  <- unique(BaseStats_Team$game_date)
  NBAGAME_DATEs <- unique(nba_team_box$game_date)
  MISSING_DATES <- as.Date(setdiff(NBAGAME_DATEs, BSGAME_DATEs), origin = "1970-01-01")
  
  if (length(MISSING_DATES) == 0) {
    message("Up to date — mirroring BaseStats_Team into nba_team_box_filtered.")
    nba_team_box_filtered <- BaseStats_Team
  } else {
    message("Building only missing dates: ", length(MISSING_DATES))
    build_src <- dplyr::filter(nba_team_box, game_date %in% MISSING_DATES)
  }
}

# ---------------- Build logic ----------------
if (exists("build_src")) {
  built <- build_src %>%
    dplyr::group_by(game_id, team_id) %>%
    dplyr::summarise(
      game_date        = dplyr::first(game_date),
      espn_team_id          = dplyr::first(team_id),
      team_winner      = dplyr::first(team_winner),
      team_logo        = dplyr::first(team_logo),
      home_away        = dplyr::first(team_home_away),
      home_away_sym    = ifelse(dplyr::first(team_home_away) == "home", "vs.", "@"),
      team             = dplyr::first(team_abbreviation),
      opp              = dplyr::first(opponent_team_abbreviation),
      T_PTS            = as.numeric(dplyr::first(team_score)),
      T_FGA            = as.numeric(dplyr::first(field_goals_attempted)),
      T_FGM            = as.numeric(dplyr::first(field_goals_made)),
      T_FG_PCT         = as.numeric(dplyr::first(field_goal_pct)) * 0.01,
      T_3PTA           = as.numeric(dplyr::first(three_point_field_goals_attempted)),
      T_3PTM           = as.numeric(dplyr::first(three_point_field_goals_made)),
      T_3PT_PCT        = as.numeric(dplyr::first(three_point_field_goal_pct)) * 0.01,
      T_FTM            = as.numeric(dplyr::first(free_throws_made)),
      T_FTA            = as.numeric(dplyr::first(free_throws_attempted)),
      T_FT_PCT         = as.numeric(dplyr::first(free_throw_pct)) * 0.01,
      T_PITP           = as.numeric(dplyr::first(points_in_paint)),
      T_OREB           = as.numeric(dplyr::first(offensive_rebounds)),
      T_DREB           = as.numeric(dplyr::first(defensive_rebounds)),
      T_REB            = T_OREB + T_DREB,
      T_AST            = as.numeric(dplyr::first(assists)),
      T_BLK            = as.numeric(dplyr::first(blocks)),
      T_STL            = as.numeric(dplyr::first(steals)),
      T_FBRK_PTS          = as.numeric(dplyr::first(fast_break_points)),
      T_OFF_TOV_PTS    = as.numeric(dplyr::first(turnover_points)),
      T_TOV            = as.numeric(dplyr::first(turnovers)),
      T_FOULS          = as.numeric(dplyr::first(fouls)),
      T_TECHFOUL       = as.numeric(dplyr::first(technical_fouls)),
      T_FLAGFOUL       = as.numeric(dplyr::first(flagrant_fouls)),
      T_FTR = ifelse(dplyr::first(field_goals_attempted) > 0,
                     round(dplyr::first(free_throws_attempted) / dplyr::first(field_goals_attempted), 3), NA_real_),
      T_PPS = ifelse(dplyr::first(field_goals_attempted) > 0,
                     round(dplyr::first(team_score) / dplyr::first(field_goals_attempted), 3), NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      is_home = ifelse(home_away == "home", 1, 0),
      is_away = ifelse(home_away == "away", 1, 0),
      team = dplyr::case_when(
        team == "NY" ~ "NYK", team == "GS" ~ "GSW", team == "WSH" ~ "WAS",
        team == "UTAH" ~ "UTA", team == "SA" ~ "SAS", team == "NO" ~ "NOP",
        TRUE ~ team
      ),
      opp = dplyr::case_when(
        opp == "NY" ~ "NYK", opp == "GS" ~ "GSW", opp == "WSH" ~ "WAS",
        opp == "UTAH" ~ "UTA", opp == "SA" ~ "SAS", opp == "NO" ~ "NOP",
        TRUE ~ opp
      ),
      CONCAT_ID = paste(team, home_away_sym, opp, game_date, sep = "-")
    )
  
  # Opponent rebound join
  opp_map <- built %>%
    dplyr::select(CONCAT_ID, T_OREB, T_DREB) %>%
    dplyr::rename(opponent_concat_id = CONCAT_ID,
                  opponent_oreb = T_OREB, opponent_dreb = T_DREB)
  
  built <- built %>%
    dplyr::mutate(opponent_concat_id = paste(opp, ifelse(home_away_sym=="vs.","@","vs."),
                                             team, game_date, sep="-")) %>%
    dplyr::left_join(opp_map, by = "opponent_concat_id") %>%
    dplyr::mutate(
      T_OREB_PCT = ifelse(!is.na(opponent_dreb) & (T_OREB + opponent_dreb) > 0,
                          T_OREB/(T_OREB + opponent_dreb), NA_real_),
      T_DREB_PCT = ifelse(!is.na(opponent_oreb) & (T_DREB + opponent_oreb) > 0,
                          T_DREB/(T_DREB + opponent_oreb), NA_real_)
    )
  
  # Assign to filtered
  nba_team_box_filtered <- built
}


##### 2. ***END*** ---------------------------------------------------------------


# 2.9 Cleanup
if (exists("opponent_rebound_mapping")) rm(opponent_rebound_mapping)
##### 2. ***END*** ---------------------------------------------------------------

# 2.10 Cleanup
rm(opponent_rebound_mapping, build_src, built, opp_map, nba_team_box)
##### 2. ***END*** ---------------------------------------------------------------

nba_team_box_filtered <- nba_team_box_filtered %>% dplyr::group_by(game_id) %>%
  dplyr::mutate(opp_logo = team_logo[match(opp, team)]) %>%
  dplyr::ungroup()

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Create nba-team_box_filtered to get a list of dates that need to pulled ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Add nba_team_id and nba_game_id to BaseStats_Team ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

##### 3. ***START*** Create Mapping Values for Data frames that contain alt ID ---------------------------------

# 3.1 Pull in nba_teams data
nba <- nba_teams()

# 3.2 Initialize an empty data frame to store results
id_mapping <- data.frame()

# 3.3 Loop through each team_id and retrieve game logs
for (team_id in nba$team_id) {
  # Debug: Print the team_id being processed
  print(paste("Processing team_id:", team_id))
  
  # 3.4 Fetch game logs for the current team_id
  team_game_log <- tryCatch({
    nba_teamgamelog(
      date_from = "",
      date_to = "",
      league_id = "00",
      season = season_token2,
      season_type = "Regular Season",
      team_id = team_id
    )
  }, error = function(e) {
    message(paste("Error fetching data for team_id:", team_id, "Error:", e$message))
    return(NULL)
  })
  
  # 3.5 Skip if no data is returned
  if (is.null(team_game_log)) {
    message(paste("No data returned for team_id:", team_id))
    next
  }
  
  # 3.6 Extract the TeamGameLog tibble
  team_game_log_data <- team_game_log$TeamGameLog
  
  # 3.7 Skip if TeamGameLog is empty
  if (nrow(team_game_log_data) == 0) {
    message(paste("Empty game log for team_id:", team_id))
    next
  }
  
  # 3.8 Parse GAME_DATE and format as 'yyyy-MM-dd'
  team_game_log_data <- team_game_log_data %>%
    mutate(
      GAME_DATE = as.Date(GAME_DATE, format = "%b %d, %Y"),    # Parse GAME_DATE
      GAME_DATE = format(GAME_DATE, "%Y-%m-%d")               # Format as 'yyyy-MM-dd'
    )
  
  # 3.9 Replace spaces in MATCHUP with underscores and create MATCHUP_DATE
  team_game_log_data <- team_game_log_data %>%
    mutate(
      MATCHUP = gsub(" ", "-", MATCHUP),                      # Replace spaces with underscores
      MATCHUP_DATE = paste0(MATCHUP, "-", GAME_DATE)          # Concatenate MATCHUP and GAME_DATE
    )
  
  # 3.10 Replace incorrect team abbreviations in the MATCHUP column
  team_game_log_data <- team_game_log_data %>%
    mutate(
      MATCHUP = gsub("SAS", "SA", MATCHUP)  # Replace SAS with SA in MATCHUP
    )
  
  # 3.11 Append the result to id_mapping, including Team_ID
  id_mapping <- bind_rows(id_mapping, team_game_log_data %>%
                            select(MATCHUP_DATE, Game_ID, Team_ID) %>%
                            rename(
                              nba_game_id = Game_ID,
                              nba_team_id = Team_ID
                            ))
  
  # 3.12 Add a 1-second delay
  Sys.sleep(.50)  # Delay to avoid rate limits
}

# 3.13 Remove unnecessary data frames
rm(nba, team_game_log_data, team_game_log)

nba_team_box_filtered <- nba_team_box_filtered %>%
  mutate(CONCAT_ID = paste0(team, "-", home_away_sym, "-", opp, "-", game_date))

# 3.14 Dynamically update BaseStats_Team data frame name using formatted_date
nba_team_box_filtered <- nba_team_box_filtered %>%
  left_join(
    id_mapping %>% select(MATCHUP_DATE, nba_game_id, nba_team_id),
    by = c("CONCAT_ID" = "MATCHUP_DATE")
  )

# === INSERTED: 3.14b Playoffs fallback for missing nba_game_id ===
if (any(is.na(nba_team_box_filtered$nba_game_id))) {
  message("Found missing nba_game_id values. Re-attempting those games using Playoffs...")
  
  missing_concat_ids <- nba_team_box_filtered %>%
    filter(is.na(nba_game_id)) %>%
    pull(CONCAT_ID)
  
  nba <- nba_teams()
  id_mapping_playoffs <- data.frame()
  
  for (team_id in nba$team_id) {
    print(paste("Retrying for Playoffs - team_id:", team_id))
    
    team_game_log <- tryCatch({
      nba_teamgamelog(
        date_from = "",
        date_to = "",
        league_id = "00",
        season = season_token2,
        season_type = "Regular Season",
        team_id = team_id
      )
    }, error = function(e) {
      message(paste("Error fetching playoff data for team_id:", team_id, "Error:", e$message))
      return(NULL)
    })
    
    if (is.null(team_game_log) || nrow(team_game_log$TeamGameLog) == 0) next
    
    team_game_log_data <- team_game_log$TeamGameLog %>%
      mutate(
        GAME_DATE = as.Date(GAME_DATE, format = "%b %d, %Y"),
        GAME_DATE = format(GAME_DATE, "%Y-%m-%d"),
        MATCHUP = gsub(" ", "-", MATCHUP),
        MATCHUP_DATE = paste0(MATCHUP, "-", GAME_DATE),
        MATCHUP = gsub("SAS", "SA", MATCHUP)
      )
    
    filtered <- team_game_log_data %>%
      filter(MATCHUP_DATE %in% missing_concat_ids)
    
    id_mapping_playoffs <- bind_rows(
      id_mapping_playoffs,
      filtered %>%
        select(MATCHUP_DATE, Game_ID, Team_ID) %>%
        rename(
          nba_game_id = Game_ID,
          nba_team_id = Team_ID
        )
    )
    
    Sys.sleep(.50)
  }
  
  # Merge the fallback data
  nba_team_box_filtered <- nba_team_box_filtered %>%
    left_join(id_mapping_playoffs, by = c("CONCAT_ID" = "MATCHUP_DATE")) %>%
    mutate(
      nba_game_id = coalesce(nba_game_id.x, nba_game_id.y),
      nba_team_id = coalesce(nba_team_id.x, nba_team_id.y)
    ) %>%
    select(-nba_game_id.x, -nba_game_id.y, -nba_team_id.x, -nba_team_id.y)
  
  message("Playoffs fallback merge completed.")
}
# === END INSERT ===

# 3.15 Remove id_mapping
rm(id_mapping)

# 3.16 Debug: Print completion message
print("Team game logs successfully pulled and merged into BaseStats_Team.")



nba_team_box_filtered <- nba_team_box_filtered[nchar(nba_team_box_filtered$team) <= 3, ]

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Add nba_team_id and nba_game_id to BaseStats_Team ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Pull in Second Chance Points using Mapping values from nba_boxscoremiscv3 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

##### 4. ***START*** Boxscore Misc V3 Data Extraction (team + opp) ----

n_rows <- nrow(nba_team_box_filtered)

points_second_chance      <- rep(NA_real_, n_rows)
opp_points_off_turnovers  <- rep(NA_real_, n_rows)
opp_points_second_chance  <- rep(NA_real_, n_rows)
opp_points_fast_break     <- rep(NA_real_, n_rows)
opp_points_paint          <- rep(NA_real_, n_rows)

game_ids <- nba_team_box_filtered$nba_game_id
team_ids <- nba_team_box_filtered$nba_team_id

for (i in seq_along(game_ids)) {
  game_id <- game_ids[i]
  team_id <- team_ids[i]
  
  print(paste("Processing game_id:", game_id, "team_id:", team_id, "(", i, "of", length(game_ids), ")"))
  
  temp_result <- nba_boxscoremiscv3(game_id = game_id)
  
  if (!is.null(temp_result)) {
    home_team_totals_misc <- temp_result$home_team_totals_misc
    away_team_totals_misc <- temp_result$away_team_totals_misc
    
    # enforce game_id match first
    home_team_totals_misc <- home_team_totals_misc %>% dplyr::filter(game_id == !!game_id)
    away_team_totals_misc <- away_team_totals_misc %>% dplyr::filter(game_id == !!game_id)
    
    # try home first (game + team)
    this_team_row <- home_team_totals_misc %>% dplyr::filter(team_id == !!team_id)
    opp_team_row  <- away_team_totals_misc
    
    # if not in home, try away (game + team)
    if (nrow(this_team_row) == 0) {
      this_team_row <- away_team_totals_misc %>% dplyr::filter(team_id == !!team_id)
      opp_team_row  <- home_team_totals_misc
    }
    
    if (nrow(this_team_row) > 0) {
      # team stats
      points_second_chance[i]  <- this_team_row$points_second_chance
      
      # opp stats
      if (nrow(opp_team_row) > 0) {
        opp_points_off_turnovers[i] <- opp_team_row$points_off_turnovers
        opp_points_second_chance[i] <- opp_team_row$points_second_chance
        opp_points_fast_break[i]    <- opp_team_row$points_fast_break
        opp_points_paint[i]         <- opp_team_row$points_paint
      }
    } else {
      message("No match in either totals table for game_id=", game_id, " team_id=", team_id)
    }
  }
  
  Sys.sleep(.50)
}

nba_team_box_filtered$T_SEC_CHN_PTS   <- points_second_chance
nba_team_box_filtered$OPP_OFF_TOV_PTS <- opp_points_off_turnovers
nba_team_box_filtered$OPP_2ND_CHN_PTS <- opp_points_second_chance
nba_team_box_filtered$OPP_FBRK_PTS      <- opp_points_fast_break
nba_team_box_filtered$OPP_PITP_PTS    <- opp_points_paint

assign("nba_team_box_filtered", nba_team_box_filtered)

print("Misc v3 columns added to nba_team_box_filtered.")
##### 4. ***END*** ----------------------------------------------------

rm(away_team_totals_misc, home_team_totals_misc, temp_result, this_team_row, opp_team_row)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# END: Pull in Second Chance Points using Mapping values from nba_boxscoremiscv3  ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ====  START: Retrieve DFL, OLBR, DLBR, LBR, BOXREB, BOXOUTS, CHRGDRWN, SCRNAST, SCRNASTPts from nba_hustlestatsboxscore ====  
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

#  5.1 Extract nba_team_box_filtered data frame
nba_team_box_filtered <- nba_team_box_filtered
 
#  5.2 Extract the nba_game_id and nba_team_id columns
game_ids <- nba_team_box_filtered$nba_game_id
team_ids <- nba_team_box_filtered$nba_team_id

#  5.3 Initialize empty vectors to store results
DFL_results <- numeric(nrow(nba_team_box_filtered))
OLBR_results <- numeric(nrow(nba_team_box_filtered))
DLBR_results <- numeric(nrow(nba_team_box_filtered))
LBR_results <- numeric(nrow(nba_team_box_filtered))
BOXREB_results <- numeric(nrow(nba_team_box_filtered))
BOXOUTS_results <- numeric(nrow(nba_team_box_filtered))
CHRGDRWN_results <- numeric(nrow(nba_team_box_filtered))
SCRNAST_results <- numeric(nrow(nba_team_box_filtered))
SCRNASTPts_results <- numeric(nrow(nba_team_box_filtered))

#  5.4 Loop through each nba_game_id in nba_team_box_filtered
for (i in seq_along(game_ids)) {
  game_id <- game_ids[i]
  team_id <- team_ids[i]
  
  #  5.5 Debug: Print progress
  print(paste("Processing game_id:", game_id, "TEAM_ID:", team_id, "(", i, "of", length(game_ids), ")"))
  flush.console()
  
  #  5.6 Query nba_hustlestatsboxscore for the current game_id
  temp_result <- tryCatch({
    nba_hustlestatsboxscore(game_id = game_id)
  }, error = function(e) {
    message(paste("Error fetching data for game_id:", game_id, "Error:", e$message))
    return(NULL)
  })
  
  #  5.7 Check if the result contains the TeamStats tibble
  if (!is.null(temp_result) && "TeamStats" %in% names(temp_result)) {
    team_stats <- temp_result$TeamStats
    
    #  5.8 Filter the TeamStats tibble for the correct TEAM_ID and GAME_ID
    matching_row <- team_stats %>%
      filter(TEAM_ID == team_id & GAME_ID == game_id)
    
    #  5.9 If a match is found, extract metrics
    if (nrow(matching_row) > 0) {
      DFL_results[i] <- matching_row$DEFLECTIONS
      OLBR_results[i] <- matching_row$OFF_LOOSE_BALLS_RECOVERED
      DLBR_results[i] <- matching_row$DEF_LOOSE_BALLS_RECOVERED
      LBR_results[i] <- matching_row$LOOSE_BALLS_RECOVERED
      BOXREB_results[i] <- matching_row$BOX_OUT_PLAYER_REBS
      BOXOUTS_results[i] <- matching_row$BOX_OUTS
      CHRGDRWN_results[i] <- matching_row$CHARGES_DRAWN
      SCRNAST_results[i] <- matching_row$SCREEN_ASSISTS
      SCRNASTPts_results[i] <- matching_row$SCREEN_AST_PTS
    } else {
      #  5.10 Assign NA if no match is found
      DFL_results[i] <- NA
      OLBR_results[i] <- NA
      DLBR_results[i] <- NA
      LBR_results[i] <- NA
      BOXREB_results[i] <- NA
      BOXOUTS_results[i] <- NA
      CHRGDRWN_results[i] <- NA
      SCRNAST_results[i] <- NA
      SCRNASTPts_results[i] <- NA
    }
  } else {
    #  5.11 Handle missing data
    print(paste("No hustle stats available for game_id:", game_id))
    DFL_results[i] <- NA
    OLBR_results[i] <- NA
    DLBR_results[i] <- NA
    LBR_results[i] <- NA
    BOXREB_results[i] <- NA
    BOXOUTS_results[i] <- NA
    CHRGDRWN_results[i] <- NA
    SCRNAST_results[i] <- NA
    SCRNASTPts_results[i] <- NA
  }
  
  #  5.12 Add a 1-second delay between requests
  Sys.sleep(.50)
}

#  5.13 Add the results to the nba_team_box_filtered data frame
nba_team_box_filtered$T_DFL <- DFL_results
nba_team_box_filtered$T_OLBR <- OLBR_results
nba_team_box_filtered$T_DLBR <- DLBR_results
nba_team_box_filtered$T_LBR <- LBR_results
nba_team_box_filtered$T_BOXREB <- BOXREB_results
nba_team_box_filtered$T_BOXOUTS <- BOXOUTS_results
nba_team_box_filtered$T_CHRGDRWN <- CHRGDRWN_results
nba_team_box_filtered$T_SCRNAST <- SCRNAST_results
nba_team_box_filtered$T_SCRN_AST_PTS <- SCRNASTPts_results

#  5.14 Dynamically assign the updated data frame back to the global environment
assign("nba_team_box_filtered", nba_team_box_filtered)

#  5.15 Debug: Print completion message
print("All hustle stats metrics have been successfully added to nba_team_box_filtered.")

#  5.16 Remove unnecessary objects
rm(team_stats, temp_result, DFL_results, OLBR_results, DLBR_results, LBR_results, 
   BOXREB_results, BOXOUTS_results, CHRGDRWN_results, SCRNAST_results, SCRNASTPts_results, 
   game_ids, team_ids, game_id, team_id, i, matching_row)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Retrieve DFL, OLBR, DLBR, LBR, BOXREB, BOXOUTS, CHRGDRWN, SCRNAST, SCRNASTPts from nba_hustlestatsboxscore ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START:  Retrieve TS_PCT, NET_RATING, USG_PCT, OFF_RATING, DEF_RATING, POSS, AST_TOV, AST_PCT, OREB_PCT, DREB_PCT, REB_PCT from nba_boxscoreadvancedv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


##### 5. ***START*** Boxscore Advanced V3 Data Extraction (team) ----

# 5.1 prep
n_rows <- nrow(nba_team_box_filtered)

T_OFF_RTG    <- rep(NA_real_, n_rows)
T_DEF_RTG    <- rep(NA_real_, n_rows)
T_NET_RTG    <- rep(NA_real_, n_rows)
T_AST_TOV    <- rep(NA_real_, n_rows)
T_OREB_PCT   <- rep(NA_real_, n_rows)
T_DREB_PCT   <- rep(NA_real_, n_rows)
T_TOV_RATIO  <- rep(NA_real_, n_rows)
T_eFG_PCT    <- rep(NA_real_, n_rows)
T_TS_PCT     <- rep(NA_real_, n_rows)
T_PACE       <- rep(NA_real_, n_rows)
T_PACE_P40   <- rep(NA_real_, n_rows)
T_POSS       <- rep(NA_real_, n_rows)
T_AST_PCT        <- rep(NA_real_, n_rows)

game_ids <- nba_team_box_filtered$nba_game_id
team_ids <- nba_team_box_filtered$nba_team_id

# 5.2 loop
for (i in seq_along(game_ids)) {
  game_id <- game_ids[i]
  team_id <- team_ids[i]
  
  print(paste(
    "ADV v3 -> game_id:", game_id,
    "team_id:", team_id,
    "(", i, "of", length(game_ids), ")"
  ))
  
  tmp_adv <- tryCatch({
    nba_boxscoreadvancedv3(game_id = game_id)
  }, error = function(e) {
    message("adv error for game_id: ", game_id, " -> ", e$message)
    return(NULL)
  })
  
  if (!is.null(tmp_adv)) {
    home_adv <- tmp_adv$home_team_totals_advanced
    away_adv <- tmp_adv$away_team_totals_advanced
    
    # 1) lock to THIS game_id first (same as misc section)
    home_adv <- home_adv %>% dplyr::filter(game_id == !!game_id)
    away_adv <- away_adv %>% dplyr::filter(game_id == !!game_id)
    
    # 2) try home first by team_id
    this_team <- home_adv %>% dplyr::filter(team_id == !!team_id)
    
    # 3) if not in home, try away, and flip the source
    if (nrow(this_team) == 0) {
      this_team <- away_adv %>% dplyr::filter(team_id == !!team_id)
    }
    
    # 4) if we found the row, map it
    if (nrow(this_team) > 0) {
      T_OFF_RTG[i]   <- this_team$offensive_rating
      T_DEF_RTG[i]   <- this_team$defensive_rating
      T_NET_RTG[i]   <- this_team$net_rating
      T_AST_TOV[i]   <- this_team$assist_to_turnover
      T_OREB_PCT[i]  <- this_team$offensive_rebound_percentage
      T_DREB_PCT[i]  <- this_team$defensive_rebound_percentage
      T_TOV_RATIO[i] <- this_team$turnover_ratio
      T_eFG_PCT[i]   <- this_team$effective_field_goal_percentage
      T_TS_PCT[i]    <- this_team$true_shooting_percentage
      T_PACE[i]      <- this_team$pace
      T_PACE_P40[i]  <- this_team$pace_per40
      T_POSS[i]      <- this_team$possessions
      T_AST_PCT[i]   <- this_team$assist_percentage
    } else {
      message("ADV v3: no matching team row for game_id ", game_id, " team_id ", team_id)
    }
  }
  
  Sys.sleep(.50)
}

# 5.3 attach to main df
nba_team_box_filtered$T_OFF_RTG    <- T_OFF_RTG
nba_team_box_filtered$T_DEF_RTG    <- T_DEF_RTG
nba_team_box_filtered$T_NET_RTG    <- T_NET_RTG
nba_team_box_filtered$T_AST_TOV    <- T_AST_TOV
nba_team_box_filtered$T_OREB_PCT   <- T_OREB_PCT
nba_team_box_filtered$T_DREB_PCT   <- T_DREB_PCT
nba_team_box_filtered$T_TOV_RAT  <- T_TOV_RATIO
nba_team_box_filtered$T_eFG_PCT    <- T_eFG_PCT
nba_team_box_filtered$T_TS_PCT     <- T_TS_PCT
nba_team_box_filtered$T_PACE       <- T_PACE
nba_team_box_filtered$T_PACE_P40   <- T_PACE_P40
nba_team_box_filtered$T_POSS       <- T_POSS
nba_team_box_filtered$T_AST_PCT    <- T_AST_PCT

assign("nba_team_box_filtered", nba_team_box_filtered)

print("Boxscore Advanced V3 columns added to nba_team_box_filtered.")
##### 5. ***END*** ----------------------------------------------------

rm(this_team, tmp_adv, home_adv, away_adv)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END:  Retrieve TS_PCT, NET_RATING, USG_PCT, OFF_RATING, DEF_RATING, POSS, AST_TOV, AST_PCT, OREB_PCT, DREB_PCT, REB_PCT from nba_boxscoreadvancedv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# # ==== START: CHAN_OREB, CHAN_DREB, CHAN_REB, CFGM, CFGA, CFG%, UCFGM, UCFGA, UCFG%, DFGM, DFGA, DFG% from nba_boxscoreplayertrackv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

##### 6. ***START*** Boxscore Player Tracking V3 Data Extraction (team) ----

# 6.1 prep
n_rows <- nrow(nba_team_box_filtered)

T_DREB_CHAN    <- rep(NA_real_, n_rows)  # rebound_chances_defensive
T_OREB_CHAN    <- rep(NA_real_, n_rows)  # rebound_chances_offensive
T_REB_CHAN     <- rep(NA_real_, n_rows)  # rebound_chances_total

T_CFGM         <- rep(NA_real_, n_rows)  # contested_field_goals_made
T_CFGA         <- rep(NA_real_, n_rows)  # contested_field_goals_attempted
T_CFG_PCT      <- rep(NA_real_, n_rows)  # contested_field_goal_percentage

T_UCFGM        <- rep(NA_real_, n_rows)  # uncontested_field_goals_made
T_UCFGA         <- rep(NA_real_, n_rows)  # uncontested_field_goals_attempted
T_UCFG_PCT      <- rep(NA_real_, n_rows)  # uncontested_field_goals_percentage

T_RIM_DFGM     <- rep(NA_real_, n_rows)  # defended_at_rim_field_goals_made
T_RIM_DFGA     <- rep(NA_real_, n_rows)  # defended_at_rim_field_goals_attempted
T_RIM_DFG_PCT  <- rep(NA_real_, n_rows)  # defended_at_rim_field_goals_percentage

game_ids <- nba_team_box_filtered$nba_game_id
team_ids <- nba_team_box_filtered$nba_team_id

# 6.2 loop
for (i in seq_along(game_ids)) {
  game_id <- game_ids[i]
  team_id <- team_ids[i]
  
  print(paste(
    "PLAYERTRACK v3 -> game_id:", game_id,
    "team_id:", team_id,
    "(", i, "of", length(game_ids), ")"
  ))
  
  # pull tracking for this game
  tmp_track <- tryCatch({
    nba_boxscoreplayertrackv3(game_id = game_id)
  }, error = function(e) {
    message("playertrack error for game_id: ", game_id, " -> ", e$message)
    return(NULL)
  })
  
  if (!is.null(tmp_track)) {
    home_track <- tmp_track$home_team_totals_player_track
    away_track <- tmp_track$away_team_totals_player_track
    
    # 1) lock to this game_id (same pattern as misc/advanced)
    home_track <- home_track %>% dplyr::filter(game_id == !!game_id)
    away_track <- away_track %>% dplyr::filter(game_id == !!game_id)
    
    # 2) try to match on team_id in home first
    this_team <- home_track %>% dplyr::filter(team_id == !!team_id)
    
    # 3) if not in home, try away
    if (nrow(this_team) == 0) {
      this_team <- away_track %>% dplyr::filter(team_id == !!team_id)
    }
    
    # 4) if we actually found it, map to your names
    if (nrow(this_team) > 0) {
      # rebounds / chances
      T_DREB_CHAN[i]   <- this_team$rebound_chances_defensive
      T_OREB_CHAN[i]   <- this_team$rebound_chances_offensive
      T_REB_CHAN[i]    <- this_team$rebound_chances_total
      
      # contested
      T_CFGM[i]        <- this_team$contested_field_goals_made
      T_CFGA[i]        <- this_team$contested_field_goals_attempted
      T_CFG_PCT[i]     <- this_team$contested_field_goal_percentage
      
      # uncontested
      T_UCFGM[i]       <- this_team$uncontested_field_goals_made
      T_UCFGA[i]        <- this_team$uncontested_field_goals_attempted
      T_UCFG_PCT[i]     <- this_team$uncontested_field_goals_percentage
      
      # defended at rim
      T_RIM_DFGM[i]    <- this_team$defended_at_rim_field_goals_made
      T_RIM_DFGA[i]    <- this_team$defended_at_rim_field_goals_attempted
      T_RIM_DFG_PCT[i] <- this_team$defended_at_rim_field_goal_percentage
    } else {
      message("PLAYERTRACK v3: no matching team row for game_id ", game_id, " team_id ", team_id)
    }
  }
  
  Sys.sleep(.50)
}

# 6.3 attach to main df
nba_team_box_filtered$T_DREB_CHAN    <- T_DREB_CHAN
nba_team_box_filtered$T_OREB_CHAN    <- T_OREB_CHAN
nba_team_box_filtered$T_REB_CHAN     <- T_REB_CHAN

nba_team_box_filtered$T_CFGM         <- T_CFGM
nba_team_box_filtered$T_CFGA         <- T_CFGA
nba_team_box_filtered$T_CFG_PCT      <- T_CFG_PCT

nba_team_box_filtered$T_UCFGM        <- T_UCFGM
nba_team_box_filtered$T_UCFGA         <- T_UCFGA
nba_team_box_filtered$T_UCFG_PCT      <- T_UCFG_PCT

nba_team_box_filtered$T_RIM_DFGM     <- T_RIM_DFGM
nba_team_box_filtered$T_RIM_DFGA     <- T_RIM_DFGA
nba_team_box_filtered$T_RIM_DFG_PCT  <- T_RIM_DFG_PCT

assign("nba_team_box_filtered", nba_team_box_filtered)

print("Boxscore Player Tracking V3 columns added to nba_team_box_filtered.")
##### 6. ***END*** ----------------------------------------------------

rm(away_track, home_track, this_team, tmp_track)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: CHAN_OREB, CHAN_DREB, CHAN_REB, CFGM, CFGA, CFG%, UCFGM, UCFGA, UCFG%, DFGM, DFGA, DFG% from nba_boxscoreplayertrackv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



## --- fix duplicate espn_team_id columns (base R only) ---
# 1) If there are multiple espn_team_id columns, coalesce into the first then drop the rest
idx <- which(names(nba_team_box_filtered) == "espn_team_id")
if (length(idx) > 1) {
  for (j in idx[-1]) {
    na_first <- is.na(nba_team_box_filtered[[idx[1]]]) | nba_team_box_filtered[[idx[1]]] == ""
    nba_team_box_filtered[[idx[1]]][na_first] <- nba_team_box_filtered[[j]][na_first]
  }
  nba_team_box_filtered <- nba_team_box_filtered[, -idx[-1], drop = FALSE]
}

# 2) Also remove the suffix column explicitly if it exists
if ("espn_team_id.1" %in% names(nba_team_box_filtered)) nba_team_box_filtered[["espn_team_id.1"]] <- NULL

# 3) (Optional safety) remove any other duplicate-named columns keeping the first
dups <- names(nba_team_box_filtered)[duplicated(names(nba_team_box_filtered))]
if (length(dups)) {
  for (nm in unique(dups)) {
    i <- which(names(nba_team_box_filtered) == nm)
    if (length(i) > 1) nba_team_box_filtered <- nba_team_box_filtered[, -i[-1], drop = FALSE]
  }
}

# ==============================================================================================

## --- fix duplicate espn_team_id columns (base R only) ---
# 1) If there are multiple espn_team_id columns, coalesce into the first then drop the rest
idx <- which(names(BaseStats_Team) == "espn_team_id")
if (length(idx) > 1) {
  for (j in idx[-1]) {
    na_first <- is.na(BaseStats_Team[[idx[1]]]) | BaseStats_Team[[idx[1]]] == ""
    BaseStats_Team[[idx[1]]][na_first] <- BaseStats_Team[[j]][na_first]
  }
  BaseStats_Team <- BaseStats_Team[, -idx[-1], drop = FALSE]
}

# 2) Also remove the suffix column explicitly if it exists
if ("espn_team_id.1" %in% names(BaseStats_Team)) BaseStats_Team[["espn_team_id.1"]] <- NULL

# 3) (Optional safety) remove any other duplicate-named columns keeping the first
dups <- names(BaseStats_Team)[duplicated(names(BaseStats_Team))]
if (length(dups)) {
  for (nm in unique(dups)) {
    i <- which(names(nba_team_box_filtered) == nm)
    if (length(i) > 1) BaseStats_Team <- BaseStats_Team[, -i[-1], drop = FALSE]
  }
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Data processing, aligning, and merging BaseStats_Team with nba_team_box_filtered ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Rename columns if they exist in BaseStats_Team
if ("game_id"       %in% names(BaseStats_Team)) names(BaseStats_Team)[names(BaseStats_Team) == "game_id"]       <- "espn_game_id"
if ("opponent_oreb" %in% names(BaseStats_Team)) names(BaseStats_Team)[names(BaseStats_Team) == "opponent_oreb"] <- "OPP_OREB"
if ("opponent_dreb" %in% names(BaseStats_Team)) names(BaseStats_Team)[names(BaseStats_Team) == "opponent_dreb"] <- "OPP_DREB"

# Rename columns if they exist in nba_team_box_filtered
if ("game_id"       %in% names(nba_team_box_filtered)) names(nba_team_box_filtered)[names(nba_team_box_filtered) == "game_id"]       <- "espn_game_id"
if ("opponent_oreb" %in% names(nba_team_box_filtered)) names(nba_team_box_filtered)[names(nba_team_box_filtered) == "opponent_oreb"] <- "OPP_OREB"
if ("opponent_dreb" %in% names(nba_team_box_filtered)) names(nba_team_box_filtered)[names(nba_team_box_filtered) == "opponent_dreb"] <- "OPP_DREB"


numeric_columns_as_double <- c(
  "nba_team_id", "T_OFF_RTG", "T_DEF_RTG", "T_NET_RTG", "T_AST_TOV", "T_FT_PCT", "T_TS_PCT", "T_eFG_PCT", "T_CFG_PCT", 
  "T_UCFG_PCT", "T_RIM_DFG_PCT", "T_FTR", "T_PPS", "T_OREB_PCT", "T_DREB_PCT", "T_AST_PCT"
)
numeric_columns_as_integer <- c(
 "T_PTS", "T_FGA", "T_FGM", "T_3PTA", 
  "T_3PTM", "T_FTM", "T_FTA", "T_PITP", "T_OREB", "T_DREB", "T_REB", "T_AST", "T_BLK", 
  "T_STL", "T_FBRK_PTS", "T_OFF_TOV_PTS", "T_TOV", "T_FOULS", "T_TECHFOUL", "T_FLAGFOUL", 
  "T_SEC_CHN_PTS", "T_DFL", "T_OLBR", "T_DLBR", "T_LBR", "T_BOXREB", "T_BOXOUTS", "T_CHRGDRWN", 
  "T_SCRNAST", "T_SCRN_AST_PTS", "T_POSS", "T_PACE", "T_CFGM", "T_CFGA", "T_UCFGM", "T_UCFGA", 
  "T_RIM_DFGM", "T_RIM_DFGA", "T_OREB_CHAN", "T_DREB_CHAN", "T_REB_CHAN")

nba_team_box_filtered[, numeric_columns_as_double]  <- lapply(nba_team_box_filtered[, numeric_columns_as_double],  as.double)
nba_team_box_filtered[, numeric_columns_as_integer] <- lapply(nba_team_box_filtered[, numeric_columns_as_integer], as.integer)


# ---- 2) In BaseStats_Team: remove columns ----
BaseStats_Team <- dplyr::select(
  BaseStats_Team,
  -dplyr::any_of(c("CONCAT_ID", "opponent_concat_id", "team_id"))
)

# ---- 2) In BaseStats_Team: remove columns ----
nba_team_box_filtered <- dplyr::select(
  nba_team_box_filtered,
  -dplyr::any_of(c("CONCAT_ID", "opponent_concat_id", "team_id"))
)


# ---- 1) Merge/append nba_team_box_filtered into BaseStats_Team ----
if (!exists("BaseStats_Team") || is.null(BaseStats_Team) || nrow(BaseStats_Team) == 0) {
  # If BaseStats_Team is empty/missing, overwrite
  BaseStats_Team <- nba_team_box_filtered
} else {
  # Align columns (union), then append
  all_cols <- union(names(BaseStats_Team), names(nba_team_box_filtered))
  # Add missing columns as NA to each side
  for (nm in setdiff(all_cols, names(BaseStats_Team))) BaseStats_Team[[nm]] <- NA
  for (nm in setdiff(all_cols, names(nba_team_box_filtered))) nba_team_box_filtered[[nm]] <- NA
  # Reorder rhs to match lhs, then bind
  nba_team_box_filtered <- nba_team_box_filtered[, names(BaseStats_Team)]
  BaseStats_Team <- dplyr::bind_rows(BaseStats_Team, nba_team_box_filtered)
}


# ---- 4) In BaseStats_Team: relocate IDs ----
if (all(c("espn_game_id", "nba_game_id") %in% names(BaseStats_Team))) {
  BaseStats_Team <- dplyr::relocate(BaseStats_Team, nba_game_id, .after = espn_game_id)
}
if (all(c("espn_team_id", "nba_team_id") %in% names(BaseStats_Team))) {
  BaseStats_Team <- dplyr::relocate(BaseStats_Team, nba_team_id, .after = espn_team_id)
}

if ("opp_logo" %in% names(BaseStats_Team)) BaseStats_Team <- BaseStats_Team %>% dplyr::relocate(opp_logo, .after = team_logo)


# ---- 5) Write nba_schedule and BaseStats_Team ----
# 10.5 Export nba_schedule_(formatted_date) to the specified path
nba_schedule_path <- paste0("C:\\Users\\Austin\\OneDrive\\Desktop\\1\\Data Analytics\\NBA Data\\0. Datahub (Temp)\\1. hoopR\\0. Current Slate\\nba_schedule_", formatted_date, ".csv")
write.csv(get(paste0("nba_schedule", formatted_date)), file = nba_schedule_path, row.names = FALSE)
print(paste("nba_schedule_(formatted_date) has been exported to:", nba_schedule_path))

# 10.5 Export nba_schedule_(formatted_date) to the specified path
  nba_schedule_path <- paste0("C:\\Users\\Austin\\OneDrive\\Desktop\\1\\Data Analytics\\NBA Data\\0. Datahub (Temp)\\1. hoopR\\10. NBA Schedule\\nba_schedule_", season_token, ".csv")
write.csv(get(paste0("nba_schedule_", season_token)), file = nba_schedule_path, row.names = FALSE)
print(paste("nba_schedule_(season_token) has been exported to:", nba_schedule_path))

# 9.6 Export the updated BaseStats_Team data frame
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token,
  ".csv"
)
write.csv(BaseStats_Team, file = team_output_path, row.names = FALSE)
print(paste("BaseStats_Team has been exported to:", team_output_path))

rm(nba_team_box_filtered)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Data processing, aligning, and merging BaseStats_Team with nba_team_box_filtered ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀