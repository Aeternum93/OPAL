# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== START: Pull NBAPBP data and enrich it with Popcorn Machine Lineup Data for each play of the game ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿

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
library(stringr)
library(lubridate)
library(dplyr)
library(future)
library(future.apply)

#====================#
# 1. Load Data Files #
#====================#

plan(multisession, workers = 10)
options(future.rng.onMisuse = "ignore")  # Optional, suppress RNG warnings
# Load play-by-play data
pbp_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")
nba_pbp <- fread(pbp_path, colClasses = "character")

# Load PopcornMachine lineup data
lineup_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/2. Popcorn Machine/pm_lineup_data_", season_token, ".csv")
pm_lineup_data <- fread(lineup_path, colClasses = "character")

# Load ESPN team ID mapping
espn_team_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/espn_team_id_mapping.txt"
espn_team_id_mapping <- fread(espn_team_map_path, sep = "\t", colClasses = "character")

#==================================#
# 2. Add pm_game_id to nba_pbp     #
#==================================#

#==================================#
# 2. Normalize Team Abbrevs & Build pm_game_id #
#==================================#

abbrev_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/pbp_to_pm_abbrev_mapping.txt"

abbrev_map <- fread(
  file = abbrev_map_path,
  sep = "\t",                     # Tab-delimited for .txt
  colClasses = "character"
)

# Add columns to pm_lineup_data that convert Popcorn Machine abbreviations to PBP-style
# Don't convert - just keep the original Team value
pm_lineup_data[, pbpb_team := Team]
pm_lineup_data[, pbpb_opp := Opp]

# Map home_team_abbrev → pm_home_team_conv
nba_pbp <- merge(
  nba_pbp,
  abbrev_map,
  by.x = "home_team_abbrev",
  by.y = "pbp_team_abbrev",
  all.x = TRUE
)
setnames(nba_pbp, "pm_team_abbrev", "pm_home_team_conv")

# Map away_team_abbrev → pm_away_team_conv
nba_pbp <- merge(
  nba_pbp,
  abbrev_map,
  by.x = "away_team_abbrev",
  by.y = "pbp_team_abbrev",
  all.x = TRUE
)
setnames(nba_pbp, "pm_team_abbrev", "pm_away_team_conv")

# Create pm_game_id using mapped team abbreviations
nba_pbp[, pm_game_id := paste0(
  pm_away_team_conv,
  pm_home_team_conv,
  "_",
  format(parse_date_time(game_date, orders = c("mdy", "ymd")), "%Y%m%d")
)]

#==================================#
# 3. Clean Lineup Data             #
#==================================#

# Remove "Period " and keep number
pm_lineup_data[, Period := str_remove(Period, "Period ")]

# Time_On -> time_on_minutes and time_on_seconds
pm_lineup_data[, time_on_minutes := str_extract(Time_On, "^\\d+")]
pm_lineup_data[, time_on_seconds := str_extract(Time_On, "(?<=:)\\d+")]

# Time_Off -> time_off_minutes and time_off_seconds
pm_lineup_data[, time_off_minutes := str_extract(Time_Off, "^\\d+")]
pm_lineup_data[, time_off_seconds := str_extract(Time_Off, "(?<=:)\\d+")]

#==================================#
# 4. Add ESPN Team ID to Lineups   #
#==================================#

pm_lineup_data <- merge(
  pm_lineup_data,
  espn_team_id_mapping[, .(Abbreviation, `ESPN ID`)],
  by.x = "Team", by.y = "Abbreviation", all.x = TRUE
)
setnames(pm_lineup_data, "ESPN ID", "espn_team_id")

#====================================================#
# 5. Prepare for Merge: Add Empty Columns to nba_pbp #
#====================================================#

# Add lineup placeholders
for (prefix in c("home", "away")) {
  for (i in 1:5) {
    nba_pbp[[paste0(prefix, "_P", i)]] <- NA_character_
  }
}

# Ensure clock_minutes and clock_seconds are character
nba_pbp[, clock_minutes := as.character(clock_minutes)]
nba_pbp[, clock_seconds := as.character(clock_seconds)]

#==============================================================#
# 6. Merge Logic: Match lineups based on time, period, and ID  #
#==============================================================#

total_rows <- nrow(nba_pbp)

for (i in seq_len(total_rows)) {
  row <- nba_pbp[i]
  
  # Debug print every 100 rows
  if (i %% 100 == 0 || i == 1) {
    cat("\n---------------------------\n")
    cat(sprintf("Row: %d / %d\n", i, total_rows))
    cat(sprintf("Game ID: %s | QTR: %s | Clock: %s:%s\n",
                row$pm_game_id, row$qtr, row$clock_minutes, row$clock_seconds))
  }
  
  # Step 1: Match game_id
  game_matches <- pm_lineup_data[game_id == row$pm_game_id]
  if (i %% 100 == 0 || i == 1) cat("Game matches:", nrow(game_matches), "\n")
  
  # Step 2: Match period
  period_matches <- game_matches[Period == row$qtr]
  if (i %% 100 == 0 || i == 1) cat("Period matches:", nrow(period_matches), "\n")
  
  # Step 3: Match clock window
  clock_sec <- as.integer(row$clock_minutes) * 60 + as.integer(row$clock_seconds)
  
  time_matches <- period_matches[
    clock_sec <= (as.integer(time_on_minutes) * 60 + as.integer(time_on_seconds)) &
      fifelse(rep(clock_sec == 0, nrow(period_matches)),
              clock_sec >= (as.integer(time_off_minutes) * 60 + as.integer(time_off_seconds)),
              clock_sec >  (as.integer(time_off_minutes) * 60 + as.integer(time_off_seconds)))
  ]
  if (i %% 100 == 0 || i == 1) cat("Time matches:", nrow(time_matches), "\n")
  
  # Step 4: Assign lineup if match found
  if (nrow(time_matches) > 0) {
    for (j in seq_len(nrow(time_matches))) {
      lineup <- time_matches[j]
      
      if (lineup$pbpb_team == row$pm_home_team_conv) {
        for (k in 1:5) nba_pbp[i, paste0("home_P", k) := lineup[[paste0("P", k)]]]
      } else if (lineup$pbpb_team == row$pm_away_team_conv) {
        for (k in 1:5) nba_pbp[i, paste0("away_P", k) := lineup[[paste0("P", k)]]]
      }
    }
  }
}


#==================================#
# Post-Loop Diagnostic Summary     #
#==================================#

# Classify each row by fill status
nba_pbp[, home_filled := !is.na(home_P1)]
nba_pbp[, away_filled := !is.na(away_P1)]
nba_pbp <- nba_pbp[pm_game_id != "NANA_20260215"]
nba_pbp[, fill_status := fcase(
  home_filled & away_filled,   "Both filled",
  home_filled & !away_filled,  "Home only",
  !home_filled & away_filled,  "Away only",
  !home_filled & !away_filled, "Neither filled"
)]



cat("\n========== LINEUP FILL SUMMARY ==========\n")
print(table(nba_pbp$fill_status))
cat("\nFill rate:", round(sum(nba_pbp$home_filled) / nrow(nba_pbp) * 100, 1), "%\n")

# Which games have the most unfilled rows?
cat("\n--- Games with most unfilled rows ---\n")
unfilled_by_game <- nba_pbp[fill_status == "Neither filled", .N, by = pm_game_id][order(-N)]
print(head(unfilled_by_game, 20))

# Which periods have the most unfilled rows?
cat("\n--- Unfilled rows by period ---\n")
print(nba_pbp[fill_status == "Neither filled", .N, by = qtr][order(qtr)])

# One-sided fills - these indicate team assignment issues
cat("\n--- Games with Home Only fills (team assignment suspect) ---\n")
print(nba_pbp[fill_status == "Home only", .N, by = pm_game_id][order(-N)] |> head(10))

cat("\n--- Games with Away Only fills (team assignment suspect) ---\n")
print(nba_pbp[fill_status == "Away only", .N, by = pm_game_id][order(-N)] |> head(10))


# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# START: Bring in espn_ids using hoopr_name_mapping file
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿

library(data.table)
library(dplyr)
library(tidyr)

#------------------------------------------------------------------
# 1) Load name mapping (clean_player_name -> espn_player_id)
#------------------------------------------------------------------
hoopr_name_map_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"

name_map <- fread(
  hoopr_name_map_path,
  colClasses = "character",
  encoding   = "UTF-8"
) %>%
  as.data.frame()

# Keep only the columns we need
name_map <- name_map %>%
  select(pm_player_name, espn_player_id)

#------------------------------------------------------------------
# 2) Map home_P1–home_P5 / away_P1–away_P5 to ESPN IDs in nba_pbp
#    (nba_pbp is assumed to already be in memory)
#------------------------------------------------------------------

# Add a row_id so we can pivot and then merge back cleanly
nba_pbp <- nba_pbp %>%
  mutate(row_id = dplyr::row_number())

# Long form: one row per (row_id, slot)
pbp_long <- nba_pbp %>%
  select(
    row_id,
    home_P1, home_P2, home_P3, home_P4, home_P5,
    away_P1, away_P2, away_P3, away_P4, away_P5
  ) %>%
  pivot_longer(
    cols      = home_P1:away_P5,
    names_to  = "slot",
    values_to = "pm_player_name"
  )

# Join to name_map to pull espn_player_id
pbp_long_ids <- pbp_long %>%
  left_join(name_map, by = "pm_player_name")

# Wide form: create *_espn_id columns for each slot
pbp_id_wide <- pbp_long_ids %>%
  select(row_id, slot, espn_player_id) %>%
  pivot_wider(
    names_from  = slot,
    values_from = espn_player_id,
    names_glue  = "{slot}_espn_id"
  )

# Merge back into nba_pbp and drop row_id helper
nba_pbp <- nba_pbp %>%
  left_join(pbp_id_wide, by = "row_id") %>%
  select(-row_id)

#==================================#
# Enrich: Team Logos & Colors      #
#==================================#
schedule_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_", season_token, ".csv")
nba_schedule <- fread(schedule_path, colClasses = "character")

# Keep only what we need from schedule
schedule_slim <- nba_schedule[, .(
  game_id,
  team_logo, team_color, home_alternate_color,
  opp_logo,  opp_color,  opp_alternate_color
)][!duplicated(game_id)]



# Join to pbp on game_id
nba_pbp <- merge(
  nba_pbp,
  schedule_slim,
  by = "game_id",
  all.x = TRUE
)

#==================================#
# Enrich: Player Headshots         #
#==================================#

player_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_", season_token, ".csv")
base_player <- fread(player_path, colClasses = "character")

# Keep only espn_player_id and headshot
headshot_map <- unique(base_player[, .(espn_player_id, headshot)])

# Function to map a vector of IDs to headshots
map_headshots <- function(ids) {
  headshot_map$headshot[match(ids, headshot_map$espn_player_id)]
}

# Map headshots for athlete_id columns
nba_pbp[, athlete_id_1_headshot := map_headshots(athlete_id_1)]
nba_pbp[, athlete_id_2_headshot := map_headshots(athlete_id_2)]
nba_pbp[, athlete_id_3_headshot := map_headshots(athlete_id_3)]

# Map headshots for home lineup
nba_pbp[, home_P1_headshot := map_headshots(home_P1_espn_id)]
nba_pbp[, home_P2_headshot := map_headshots(home_P2_espn_id)]
nba_pbp[, home_P3_headshot := map_headshots(home_P3_espn_id)]
nba_pbp[, home_P4_headshot := map_headshots(home_P4_espn_id)]
nba_pbp[, home_P5_headshot := map_headshots(home_P5_espn_id)]

# Map headshots for away lineup
nba_pbp[, away_P1_headshot := map_headshots(away_P1_espn_id)]
nba_pbp[, away_P2_headshot := map_headshots(away_P2_espn_id)]
nba_pbp[, away_P3_headshot := map_headshots(away_P3_espn_id)]
nba_pbp[, away_P4_headshot := map_headshots(away_P4_espn_id)]
nba_pbp[, away_P5_headshot := map_headshots(away_P5_espn_id)]

# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# END: Bring in espn_ids using hoopr_name_mapping file
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿


#=========================#
# Final Preview (No Save) #
#=========================#

nba_pbp_preview <- nba_pbp[!is.na(home_P1) | !is.na(away_P1)][1:20]
fwrite(
  nba_pbp,
  paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/pm_nbapbp_", season_token, ".csv")
)

# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# ==== END: Pull NBAPBP data and enrich it with Popcorn Machine Lineup Data for each play of the game ====
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿
# 🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿🍿