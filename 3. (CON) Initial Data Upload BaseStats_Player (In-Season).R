# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== START: Date configuration & library loading logic ====
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


# 2.1 Path
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token, ".csv"
)

# 2.2 Load BaseStats_Team (if exists)
if (file.exists(team_output_path) && file.info(team_output_path)$size > 0) {
  BaseStats_Team <- read.csv(team_output_path, stringsAsFactors = FALSE)
  if ("nba_game_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_game_id <- as.character(BaseStats_Team$nba_game_id)
  if ("nba_game_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_team_id <- as.character(BaseStats_Team$nba_team_id)
  if ("game_date"  %in% names(BaseStats_Team)) BaseStats_Team$game_date  <- as.Date(BaseStats_Team$game_date)
} else {
  BaseStats_Team <- data.frame()
}

# 1.1 Define the path to the BaseStats_Player CSV file`
player_output_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_", season_token, ".csv")

# 1.2 Load BaseStats_Player.csv into a data frame
if (file.exists(player_output_path) && file.info(player_output_path)$size > 0) {
  BaseStats_Player <- read.csv(
    file = player_output_path,
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
} else {
  BaseStats_Player <- data.frame()  # Create an empty data frame if the file doesn't exist or is empty
}



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== END: Date configuration & library loading logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== START: Load NBA Player Box and Baseline for Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# 1.3 Load player box from PBP game IDs using espn_nba_player_box() (replaces load_nba_player_box)
pbp_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_",
  season_token, ".csv"
)
pbp_dates <- data.table::fread(pbp_path, select = c("game_id", "game_date")) %>%
  dplyr::distinct(game_id, .keep_all = TRUE) %>%
  dplyr::mutate(game_date = as.Date(game_date))

espn_game_ids <- unique(as.character(pbp_dates$game_id))

# Filter to only pull games for dates NOT already in BaseStats_Player
if (nrow(BaseStats_Player) > 0) {
  existing_dates <- unique(as.Date(BaseStats_Player$game_date))
  missing_game_ids <- pbp_dates %>%
    dplyr::filter(!game_date %in% existing_dates) %>%
    dplyr::pull(game_id) %>%
    unique() %>%
    as.character()
  message("BaseStats_Player has through ", max(existing_dates),
          ". Only pulling ", length(missing_game_ids), " new game(s).")
  espn_game_ids <- missing_game_ids
}

nba_player_box <- data.frame()
for (gid in espn_game_ids) {
  message("Pulling player box for game_id: ", gid, " (", which(espn_game_ids == gid), " of ", length(espn_game_ids), ")")
  
  pb <- tryCatch({
    espn_nba_player_box(game_id = gid)
  }, error = function(e) {
    message("  Error for game_id: ", gid, " -> ", e$message)
    return(NULL)
  })
  if (!is.null(pb) && nrow(pb) > 0) {
    nba_player_box <- dplyr::bind_rows(nba_player_box, pb)
  }
  Sys.sleep(0.50)
}

message("Loaded ", nrow(nba_player_box), " player box rows.")

# 1.4 Format game_date as Date objects in both data frames
BaseStats_Player$game_date <- as.Date(BaseStats_Player$game_date, format = "%Y-%m-%d")
nba_player_box$game_date <- as.Date(nba_player_box$game_date, format = "%Y-%m-%d")

# 1.5 Debugging: Check for NA values in game_date
if (any(is.na(BaseStats_Player$game_date))) {
  print("Warning: NA values found in BaseStats_Player$game_date")
}
if (any(is.na(nba_player_box$game_date))) {
  print("Warning: NA values found in nba_player_box$game_date")
}

# 1.6 Create unique lists of game dates
BSGAME_DATES <- unique(BaseStats_Player$game_date)
NBAGAME_DATEs <- unique(nba_player_box$game_date)

BSGAME_DATES <- as.Date(BSGAME_DATES, format = "%Y-%m-%d")
NBAGAME_DATEs <- as.Date(NBAGAME_DATEs, format = "%Y-%m-%d")

# 1.7 Identify missing dates in BaseStats_Player
MISSING_DATES <- setdiff(NBAGAME_DATEs, BSGAME_DATES)
MISSING_DATES <- as.Date(MISSING_DATES, origin = "1970-01-01")

print("Missing dates identified (formatted):")
print(MISSING_DATES)

# 1.8 Filter nba_player_box for rows with missing dates
nba_player_box_filtered <- nba_player_box %>%
  filter(game_date %in% MISSING_DATES)

print("Filtered nba_player_box for missing dates:")
print(head(nba_player_box_filtered))

# 1.9 Process the filtered nba_player_box to calculate required metrics
if (nrow(nba_player_box_filtered) > 0) {
  nba_player_box_filtered <- nba_player_box_filtered %>%
    group_by(game_id, athlete_id) %>%
    summarise(
      game_date = first(game_date),
      espn_player_id = first(athlete_id),
      headshot = first(athlete_headshot_href),
      player_name = first(athlete_display_name),
      position = first(athlete_position_abbreviation),
      espn_team_id = first(team_id),
      team = first(team_abbreviation),
      team_score = first(team_score),
      team_logo = first(team_logo),
      home_away = first(home_away),
      home_away_sym = ifelse(first(home_away) == "home", "vs.", "@"),
      opp = first(opponent_team_abbreviation),
      opp_score = first(opponent_team_score),
      opp_logo = first(opponent_team_logo),
      team_winner = first(team_winner),
      starter_status = first(starter),
      ejected = first(ejected),
      reason = first(reason),
      MIN = first(minutes),
      PTS = first(points),
      FGA = first(field_goals_attempted),
      FGM = first(field_goals_made),
      FG_PCT = ifelse(first(field_goals_attempted) > 0, 
                      first(field_goals_made) / first(field_goals_attempted), NA),
      FTA = first(free_throws_attempted),
      FTM = first(free_throws_made),
      FT_PCT = ifelse(first(free_throws_attempted) > 0, 
                      first(free_throws_made) / first(free_throws_attempted), NA),
      FTR = ifelse(first(field_goals_attempted) > 0, 
                   first(free_throws_attempted) / first(field_goals_attempted), NA),
      `3PTA` = first(three_point_field_goals_attempted),
      `3PTM` = first(three_point_field_goals_made),
      `3PT_PCT` = ifelse(first(three_point_field_goals_attempted) > 0,
                         first(three_point_field_goals_made) / first(three_point_field_goals_attempted), NA),
      REB = first(rebounds),
      OREB = first(offensive_rebounds),
      DREB = first(defensive_rebounds),
      AST = first(assists),
      P_M = first(plus_minus),
      STL = first(steals),
      BLK = first(blocks)
    ) %>%
    ungroup() %>%
    mutate(
      across(c('FG_PCT', 'FT_PCT', 'FTR', '3PT_PCT'), ~ round(., 3)),
      is_home = ifelse(home_away == "home", 1, 0),
      is_away = ifelse(home_away == "away", 1, 0),
      team = case_when(
        team == "NY" ~ "NYK",
        team == "GS" ~ "GSW",
        team == "WSH" ~ "WAS",
        team == "UTAH" ~ "UTA",
        team == "NO" ~ "NOP",
        TRUE ~ team
      )
    )
}

# 1.10 Output Results
print("Final MISSING_DATES:")
print(MISSING_DATES)
print("nba_player_box_filtered:")
print(head(nba_player_box_filtered))

# 1.11 Remove unnecessary data frames
rm(nba_player_box)

if (
  exists("BaseStats_Player") &&
  is.data.frame(BaseStats_Player) &&
  nrow(BaseStats_Player) > 0 &&
  all(c("X3PTA","X3PTM","X3PT_PCT") %in% names(BaseStats_Player))
) {
  BaseStats_Player <- BaseStats_Player %>%
    dplyr::rename(
      `3PTA`    = X3PTA,
      `3PTM`    = X3PTM,
      `3PT_PCT` = X3PT_PCT
    )
}

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== END: Load NBA Player Box and Baseline for Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== START: Load NBA Player Box and Baseline for Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# 2.1 Load BaseStats_Team dynamically
BaseStats_Team <- BaseStats_Team

# 2.2 Use nba_player_box_filtered directly (no dynamic assignment)
nba_player_box_filtered <- nba_player_box_filtered

# 2.3 Create a per-game, per-team mapping
#     (keep BOTH teams for each espn_game_id)
unique_game_id_mapping <- BaseStats_Team %>%
  select(
    espn_game_id,
    espn_team_id,   # <— we need this to match the player df's team_ID
    nba_game_id,
    nba_team_id
  ) %>%
  distinct(espn_game_id, espn_team_id, .keep_all = TRUE)

# 2.4 Join the mapping to nba_player_box_filtered
#     your player df has 'team_ID' (ESPN), so match on both keys
nba_player_box_filtered <- nba_player_box_filtered %>%
  left_join(
    unique_game_id_mapping,
    by = c(
      "game_id" = "espn_game_id",
      "espn_team_id" = "espn_team_id"   # <- this is the piece that keeps OKC from becoming HOU
    )
  )

# 2.5 make sure player df team id is character (so it matches roster ids later)
nba_player_box_filtered <- nba_player_box_filtered %>%
  mutate(
    nba_team_id = as.character(nba_team_id),
    nba_game_id = as.character(nba_game_id)
  )


# 2.5 Setup paths / season
name_mapping_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/hoopr_name_mapping.csv"
output_dir <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/Temp"
output_file <- file.path(output_dir, paste0("nba_commonteamroster_", gsub("-", "_", season_token2), ".csv"))

# Create the directory if it doesn’t exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Created output directory:", output_dir, "\n")
}

# Create the file if it doesn’t exist
if (!file.exists(output_file)) {
  fwrite(data.frame(), file = output_file)  # create empty CSV placeholder
  cat("✅ Created new CSV file:", output_file, "\n")
}

# 2.6 Load name mapping CSV
name_mapping <- read_csv(name_mapping_path, show_col_types = FALSE)

# 2.7 Ensure nba_player_id column exists on player df
if (!"nba_player_id" %in% names(nba_player_box_filtered)) {
  nba_player_box_filtered$nba_player_id <- NA_character_
}

# 2.8 Build the team list from BaseStats_Team (not from player df)
team_ids <- BaseStats_Team %>%
  distinct(nba_team_id) %>%
  filter(!is.na(nba_team_id), nba_team_id != "") %>%
  mutate(nba_team_id = as.character(nba_team_id)) %>%
  pull(nba_team_id)

cat("Starting roster mapping for", length(team_ids), "teams (from BaseStats_Team)...\n")

for (i in seq_along(team_ids)) {
  team_id <- team_ids[i]
  cat("\n--------------------------------------\n")
  cat("Processing team", i, "of", length(team_ids), "| team_id:", team_id, "\n")
  
  # 1) pull roster for this team
  team_roster <- tryCatch({
    nba_commonteamroster(
      league_id = "00",
      season = season_token2,
      team_id = team_id
    )$CommonTeamRoster
  }, error = function(e) {
    message(paste("❌ Error fetching roster for team_id", team_id, ":", e$message))
    return(NULL)
  })
  
  if (is.null(team_roster) || !"PLAYER" %in% names(team_roster)) {
    message(paste("⚠️ No valid roster data for team_id", team_id))
    Sys.sleep(1)
    next
  }
  
  # normalize roster team id to match our naming
  team_roster <- team_roster %>%
    rename(nba_team_id = TeamID) %>%
    mutate(nba_team_id = as.character(nba_team_id))
  
  cat(" → Pulled", nrow(team_roster), "players from NBA API.\n")
  
  # 2) Append raw roster to CSV (create if needed)
  fwrite(
    team_roster,
    file   = output_file,
    append = TRUE
  )
  cat(" → Appended CommonTeamRoster to:", output_file, "\n")
  
  # 3) Normalize names using name_mapping (nba_player_name → clean_player_name)
  roster_mapped <- team_roster %>%
    left_join(
      name_mapping,
      by = c("PLAYER" = "nba_player_name")
    ) %>%
    mutate(PLAYER = ifelse(!is.na(clean_player_name), clean_player_name, PLAYER)) %>%
    select(-clean_player_name)
  
  
  # 4) Write PLAYER_ID back into nba_player_box_filtered as nba_player_id
  updates <- 0
  for (r in seq_len(nrow(roster_mapped))) {
    player_name <- roster_mapped$PLAYER[r]
    player_id   <- as.character(roster_mapped$PLAYER_ID[r])
    
    # find matching rows in the player df for this team
    idx <- which(
      nba_player_box_filtered$player_name == player_name &
        nba_player_box_filtered$nba_team_id == team_id
    )
    
    if (length(idx) > 0) {
      nba_player_box_filtered$nba_player_id[idx] <- player_id
      updates <- updates + length(idx)
    } else {
      # debug message so we can see which ones aren't matching
      cat("   ⚠️ No match in nba_player_box_filtered for:",
          player_name, "| team_id:", team_id, "\n")
    }
  }
  
  cat(" → Updated", updates, "player(s) for this team in nba_player_box_filtered.\n")
  
  # 5) rate limit
  Sys.sleep(1)
}

nba_player_box_filtered$nba_game_id <- paste0("00", as.character(nba_player_box_filtered$nba_game_id))

rm(roster_mapped, unique_game_id_mapping, team_roster)

cat("\n✅ Finished mapping nba_player_id values for all teams (from BaseStats_Team).\n")
##### 2. ***END*** Mapping NBA game_ids and player_ids to nba_player_box_filtered data frame ----




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== END: Load NBA Player Box and Baseline for Data Aggregation ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== START: Calculate REB and AST Percentages ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# Build per-team lookup (one row per game+team)
team_lookup <- BaseStats_Team %>%
  dplyr::select(espn_game_id, espn_team_id, T_REB, T_FGM) %>%
  dplyr::distinct()

# Join by game + team (many players -> one team row)
nba_player_box_filtered <- nba_player_box_filtered %>%
  dplyr::left_join(
    team_lookup,
    by = c("game_id" = "espn_game_id", "espn_team_id" = "espn_team_id"),
    relationship = "many-to-one"  # dplyr >= 1.1: silences the warn when keys are correct
  )

# REB share of team rebounds
nba_player_box_filtered <- nba_player_box_filtered %>%
  dplyr::mutate(
    REB_PCT = dplyr::if_else(!is.na(T_REB) & T_REB > 0, REB / T_REB, NA_real_)
  )

# AST% = share of teammate makes (exclude the player's own FGM)
nba_player_box_filtered <- nba_player_box_filtered %>%
  dplyr::mutate(
    AST_PCT = dplyr::if_else(!is.na(T_FGM) & (T_FGM - FGM) > 0,
                             (AST / (T_FGM - FGM)),
                             NA_real_)
  )

rm(team_lookup)



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#  ==== END: Calculate REB and AST Percentages ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Retrieve PTS_2ND_CHANCE, PLAYER_ID, PTS_OFF_TOV, PTS_PAINT, FBRK_PTS from nba_boxscoremiscv2 ===
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

nba_player_box_filtered <- nba_player_box_filtered

# 1) load name mapping (keep)
name_mapping_file <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/6. Data Cleanup/name_mapping.txt"
name_mapping <- read.delim(name_mapping_file, encoding = "UTF-8")

# 2) make sure columns exist
needed_cols <- c("nba_player_id","SEC_CHN_PTS","OFF_TOV_PTS","PITP","FBRK_PTS")
for (col in needed_cols) {
  if (!col %in% names(nba_player_box_filtered)) {
    nba_player_box_filtered[[col]] <- NA
  }
}

# 3) unique games from *player* df
game_ids <- nba_player_box_filtered %>%
  distinct(nba_game_id) %>%
  filter(!is.na(nba_game_id), nba_game_id != "") %>%
  pull(nba_game_id)

total_games <- length(game_ids)

for (g_i in seq_along(game_ids)) {
  g <- game_ids[g_i]
  
  cat("\n------------------------------\n")
  cat("Processing game_id:", g, "(", g_i, "of", total_games, ")\n")
  
  # pull misc v3 for this game
  misc_res <- tryCatch({
    nba_boxscoremiscv3(game_id = g)
  }, error = function(e) {
    message("Error pulling nba_boxscoremiscv3 for game_id=", g, " -> ", e$message)
    return(NULL)
  })
  
  if (is.null(misc_res)) {
    cat("  no response for this game\n")
    Sys.sleep(1)
    next
  }
  
  home_player <- misc_res$home_team_player_misc %>% dplyr::filter(game_id == !!g)
  away_player <- misc_res$away_team_player_misc %>% dplyr::filter(game_id == !!g)
  
  if (nrow(home_player) == 0 && nrow(away_player) == 0) {
    cat("  no player misc rows for this game\n")
    Sys.sleep(1)
    next
  }
  
  home_player <- home_player %>% mutate(team_side = "home")
  away_player <- away_player %>% mutate(team_side = "away")
  all_players_misc <- bind_rows(home_player, away_player)
  
  # normalize names if present
  name_col <- intersect(c("player_name", "PLAYER_NAME"), names(all_players_misc))
  if (length(name_col) == 1) {
    nmcol <- name_col[[1]]
    all_players_misc <- all_players_misc %>%
      left_join(name_mapping, by = setNames("PLAYER", nmcol)) %>%
      mutate(!!nmcol := ifelse(!is.na(Player_Conv), Player_Conv, .data[[nmcol]])) %>%
      select(-Player_Conv)
  } else {
    nmcol <- NULL
  }
  
  # players from our df for THIS game
  idx_game <- which(nba_player_box_filtered$nba_game_id == g)
  num_players_this_game <- length(idx_game)
  
  for (p_i in seq_along(idx_game)) {
    row_index <- idx_game[p_i]
    this_player_name <- nba_player_box_filtered$player_name[row_index]
    this_player_id   <- nba_player_box_filtered$nba_player_id[row_index]
    
    # ⬇️ your original-style line
    print(paste("Processing game_id:", g,
                "player:", this_player_name,
                "(", p_i, "of", num_players_this_game, ")"))
    
    # try by existing id first
    matching_misc <- NULL
    if (!is.na(this_player_id) && this_player_id != "") {
      matching_misc <- all_players_misc %>% filter(person_id == this_player_id)
    }
    
    # fallback: try by name
    if ((is.null(matching_misc) || nrow(matching_misc) == 0) &&
        !is.null(nmcol) &&
        !is.na(this_player_name) && this_player_name != "") {
      matching_misc <- all_players_misc %>% filter(.data[[nmcol]] == this_player_name)
    }
    
    # still nothing → skip
    if (is.null(matching_misc) || nrow(matching_misc) == 0) {
      next
    }
    
    matching_misc <- matching_misc[1, ]
    
    # update id from misc
    new_person_id <- as.character(matching_misc$person_id)
    nba_player_box_filtered$nba_player_id[row_index] <- new_person_id
    
    # pull values
    sc_val  <- if ("points_second_chance" %in% names(matching_misc)) matching_misc$points_second_chance else
      if ("PTS_2ND_CHANCE" %in% names(matching_misc)) matching_misc$PTS_2ND_CHANCE else NA_real_
    
    tov_val <- if ("points_off_turnovers" %in% names(matching_misc)) matching_misc$points_off_turnovers else
      if ("PTS_OFF_TOV" %in% names(matching_misc)) matching_misc$PTS_OFF_TOV else NA_real_
    
    pitp_val <- if ("points_in_paint" %in% names(matching_misc)) matching_misc$points_in_paint else
      if ("points_paint" %in% names(matching_misc)) matching_misc$points_paint else
        if ("PTS_PAINT" %in% names(matching_misc)) matching_misc$PTS_PAINT else NA_real_
    
    fb_val  <- if ("points_fast_break" %in% names(matching_misc)) matching_misc$points_fast_break else
      if ("FBRK_PTS" %in% names(matching_misc)) matching_misc$FBRK_PTS else NA_real_
    
    nba_player_box_filtered$SEC_CHN_PTS[row_index] <- sc_val
    nba_player_box_filtered$OFF_TOV_PTS[row_index]             <- tov_val
    nba_player_box_filtered$PITP[row_index]                  <- pitp_val
    nba_player_box_filtered$FBRK_PTS[row_index]                 <- fb_val
  }
  
  Sys.sleep(1)
}

rm(all_players_misc, home_player, matching_misc, misc_res, away_player)

cat("\n✅ Player-level Misc v3 metrics added to nba_player_box_filtered.\n")
assign("nba_player_box_filtered", nba_player_box_filtered)

##### 3. ***END*** ------------------------------------------------------------


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Retrieve PTS_2ND_CHANCE, PLAYER_ID, PTS_OFF_TOV, PTS_PAINT, FBRK_PTS from nba_boxscoremiscv2 ===
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# START: Retrieve TS_PCT, NET_RATING, USG_PCT, OFF_RATING, DEF_RATING, POSS, AST_TOV, AST_PCT, OREB_PCT, DREB_PCT, REB_PCT from nba_boxscoreadvancedv2
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

nba_player_box_filtered <- nba_player_box_filtered

# 1) make sure target cols exist on player df
needed_cols <- c(
  "OFF_RTG",      # offensive_rating
  "DEF_RTG",      # defensive_rating
  "NET_RTG",      # net_rating
  "AST_TOV",      # assist_to_turnover
  "OREB_PCT",     # offensive_rebound_percentage
  "DREB_PCT",     # defensive_rebound_percentage
  "TOV_RATIO",    # turnover_ratio
  "EFG_PCT",      # effective_field_goal_percentage
  "TS_PCT",       # true_shooting_percentage
  "USG_PCT",      # usage (player table has this)
  "PACE",         # pace (usually team, but sometimes player table includes pace_on/off)
  "POSS"          # possessions
)
for (col in needed_cols) {
  if (!col %in% names(nba_player_box_filtered)) {
    nba_player_box_filtered[[col]] <- NA_real_
  }
}

# 2) unique games from player df
game_ids <- nba_player_box_filtered %>%
  distinct(nba_game_id) %>%
  filter(!is.na(nba_game_id), nba_game_id != "") %>%
  pull(nba_game_id)

total_games <- length(game_ids)

for (g_i in seq_along(game_ids)) {
  g <- game_ids[g_i]
  
  cat("\n------------------------------\n")
  cat("ADV v3 (player) -> game_id:", g, "(", g_i, "of", total_games, ")\n")
  
  # pull adv v3 for this game
  adv_res <- tryCatch({
    nba_boxscoreadvancedv3(game_id = g)
  }, error = function(e) {
    message("adv v3 error for game_id=", g, " -> ", e$message)
    return(NULL)
  })
  
  # if API failed
  if (is.null(adv_res)) {
    cat("  no advanced data for this game\n")
    Sys.sleep(1)
    next
  }
  
  # player-level tibbles
  home_players_adv <- adv_res$home_team_player_advanced %>% dplyr::filter(game_id == !!g)
  away_players_adv <- adv_res$away_team_player_advanced %>% dplyr::filter(game_id == !!g)
  
  if (nrow(home_players_adv) == 0 && nrow(away_players_adv) == 0) {
    cat("  no player advanced rows for this game\n")
    Sys.sleep(1)
    next
  }
  
  all_players_adv <- bind_rows(home_players_adv, away_players_adv)
  
  # 3) find our players for THIS game
  idx_game <- which(nba_player_box_filtered$nba_game_id == g)
  num_players_this_game <- length(idx_game)
  
  for (p_i in seq_along(idx_game)) {
    row_index <- idx_game[p_i]
    
    this_player_name <- nba_player_box_filtered$player_name[row_index]
    this_player_id   <- nba_player_box_filtered$nba_player_id[row_index]
    
    # your print style
    print(paste(
      "Processing game_id:", g,
      "player:", this_player_name,
      "(", p_i, "of", num_players_this_game, ")"
    ))
    
    # 1st try: match by person_id if we have it
    matching_adv <- NULL
    if (!is.na(this_player_id) && this_player_id != "") {
      matching_adv <- all_players_adv %>%
        filter(person_id == this_player_id)
    }
    
    # 2nd try: match by name (no name-mapping here)
    if ((is.null(matching_adv) || nrow(matching_adv) == 0) &&
        !is.na(this_player_name) && this_player_name != "") {
      # player_name column name in the API table is usually "player_name"
      name_col <- intersect(c("player_name", "PLAYER_NAME"), names(all_players_adv))
      if (length(name_col) == 1) {
        matching_adv <- all_players_adv %>%
          filter(.data[[name_col[[1]]]] == this_player_name)
      }
    }
    
    # still nothing → skip
    if (is.null(matching_adv) || nrow(matching_adv) == 0) {
      next
    }
    
    # take first match
    matching_adv <- matching_adv[1, ]
    
    # standardize id back onto our df
    nba_player_box_filtered$nba_player_id[row_index] <- as.character(matching_adv$person_id)
    
    # pull values, defensive for col-name drift
    off_rtg <- matching_adv$offensive_rating                       %||% NA_real_
    def_rtg <- matching_adv$defensive_rating                       %||% NA_real_
    net_rtg <- matching_adv$net_rating                             %||% NA_real_
    ast_tov <- matching_adv$assist_to_turnover                     %||% NA_real_
    orebpct <- matching_adv$offensive_rebound_percentage           %||% NA_real_
    drebpct <- matching_adv$defensive_rebound_percentage           %||% NA_real_
    tovrate <- matching_adv$turnover_ratio                         %||% NA_real_
    efgpct  <- matching_adv$effective_field_goal_percentage        %||% NA_real_
    tspct   <- matching_adv$true_shooting_percentage               %||% NA_real_
    usgpct  <- matching_adv$usage_percentage                       %||% NA_real_
    pace    <- matching_adv$pace                                   %||% NA_real_
    poss    <- matching_adv$possessions                            %||% NA_real_
    
    # write back
    nba_player_box_filtered$OFF_RTG[row_index]   <- off_rtg
    nba_player_box_filtered$DEF_RTG[row_index]   <- def_rtg
    nba_player_box_filtered$NET_RTG[row_index]   <- net_rtg
    nba_player_box_filtered$AST_TOV[row_index]   <- ast_tov
    nba_player_box_filtered$OREB_PCT[row_index]  <- orebpct
    nba_player_box_filtered$DREB_PCT[row_index]  <- drebpct
    nba_player_box_filtered$TOV_RATIO[row_index] <- tovrate
    nba_player_box_filtered$EFG_PCT[row_index]   <- efgpct
    nba_player_box_filtered$TS_PCT[row_index]    <- tspct
    nba_player_box_filtered$USG_PCT[row_index]   <- usgpct
    nba_player_box_filtered$PACE[row_index]      <- pace
    nba_player_box_filtered$POSS[row_index]      <- poss
  }
  
  Sys.sleep(1)
}

rm(adv_res, all_players_adv, away_players_adv, home_players_adv, matching_adv)

cat("\n✅ Player-level Advanced v3 metrics added to nba_player_box_filtered.\n")
assign("nba_player_box_filtered", nba_player_box_filtered)


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# === END: Retrieve TS_PCT, NET_RATING, USG_PCT, OFF_RATING, DEF_RATING, POSS, AST_TOV, AST_PCT, OREB_PCT, DREB_PCT, REB_PCT from nba_boxscoreadvancedv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Retrieve CHAN_OREB, CHAN_DREB, CHAN_REB, CFGM, CFGA, CFG_PCT, UCFGM, UCFGA, UCFG_PCT, DFGM, DFGA, DFG_PCT from nba_boxscoreplayertrackv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# work on current df
nba_player_box_filtered <- nba_player_box_filtered

# 1) make sure target cols exist on player df
track_cols <- c(
  "CHAN_OREB",    # rebound_chances_offensive
  "CHAN_DREB",    # rebound_chances_defensive
  "CHAN_REB",     # rebound_chances_total
  "CFGM",         # contested_field_goals_made
  "CFGA",         # contested_field_goals_attempted
  "CFG_PCT",      # contested_field_goal_percentage
  "UCFGM",        # uncontested_field_goals_made
  "UCFGA",        # uncontested_field_goals_attempted
  "UCFG_PCT",     # uncontested_field_goals_percentage
  "DFGM",         # defended_at_rim_field_goals_made
  "DFGA",         # defended_at_rim_field_goals_attempted
  "DFG_PCT"       # defended_at_rim_field_goal_percentage
)
for (col in track_cols) {
  if (!col %in% names(nba_player_box_filtered)) {
    nba_player_box_filtered[[col]] <- NA_real_
  }
}

# 2) distinct games from player df
game_ids <- nba_player_box_filtered %>%
  distinct(nba_game_id) %>%
  filter(!is.na(nba_game_id), nba_game_id != "") %>%
  pull(nba_game_id)

total_games <- length(game_ids)

for (g_i in seq_along(game_ids)) {
  g <- game_ids[g_i]
  
  cat("\n------------------------------\n")
  cat("PLAYER TRACK v3 -> game_id:", g, "(", g_i, "of", total_games, ")\n")
  
  # 3) call the v3 endpoint
  pt_res <- tryCatch({
    nba_boxscoreplayertrackv3(game_id = g)
  }, error = function(e) {
    message("playertrack v3 error for game_id=", g, " -> ", e$message)
    return(NULL)
  })
  
  # if the API failed, skip
  if (is.null(pt_res)) {
    cat("  no player track data for this game\n")
    Sys.sleep(1)
    next
  }
  
  # 4) v3 player tibbles
  if (!("home_team_player_player_track" %in% names(pt_res)) &&
      !("away_team_player_player_track" %in% names(pt_res))) {
    cat("  player track tibbles not found for this game\n")
    Sys.sleep(1)
    next
  }
  
  home_trk <- if ("home_team_player_player_track" %in% names(pt_res)) pt_res$home_team_player_player_track else NULL
  away_trk <- if ("away_team_player_player_track" %in% names(pt_res)) pt_res$away_team_player_player_track else NULL
  
  # filter by game_id if present
  if (!is.null(home_trk) && "game_id" %in% names(home_trk)) {
    home_trk <- home_trk %>% filter(game_id == !!g)
  }
  if (!is.null(away_trk) && "game_id" %in% names(away_trk)) {
    away_trk <- away_trk %>% filter(game_id == !!g)
  }
  
  player_track <- bind_rows(home_trk, away_trk)
  
  if (nrow(player_track) == 0) {
    cat("  no rows in player track for this game\n")
    Sys.sleep(1)
    next
  }
  
  # 5) rows in OUR df for this game
  idx_game <- which(nba_player_box_filtered$nba_game_id == g)
  num_players_this_game <- length(idx_game)
  
  for (p_i in seq_along(idx_game)) {
    row_index <- idx_game[p_i]
    this_player_name <- nba_player_box_filtered$player_name[row_index]
    this_player_id   <- nba_player_box_filtered$nba_player_id[row_index]
    
    print(paste(
      "Processing game_id:", g,
      "player:", this_player_name,
      "(", p_i, "of", num_players_this_game, ")"
    ))
    
    # match only by player_id
    matching_trk <- player_track %>%
      filter(person_id == this_player_id)
    
    # still no match → skip this player
    if (nrow(matching_trk) == 0) {
      next
    }
    
    # take first
    matching_trk <- matching_trk[1, ]
    
    # ---- map the actual column names ----
    nba_player_box_filtered$CHAN_OREB[row_index] <- if ("rebound_chances_offensive"  %in% names(matching_trk)) matching_trk$rebound_chances_offensive  else NA_real_
    nba_player_box_filtered$CHAN_DREB[row_index] <- if ("rebound_chances_defensive"  %in% names(matching_trk)) matching_trk$rebound_chances_defensive  else NA_real_
    nba_player_box_filtered$CHAN_REB[row_index]  <- if ("rebound_chances_total"      %in% names(matching_trk)) matching_trk$rebound_chances_total      else NA_real_
    
    nba_player_box_filtered$CFGM[row_index]      <- if ("contested_field_goals_made"      %in% names(matching_trk)) matching_trk$contested_field_goals_made      else NA_real_
    nba_player_box_filtered$CFGA[row_index]      <- if ("contested_field_goals_attempted" %in% names(matching_trk)) matching_trk$contested_field_goals_attempted else NA_real_
    nba_player_box_filtered$CFG_PCT[row_index]   <- if ("contested_field_goal_percentage" %in% names(matching_trk)) matching_trk$contested_field_goal_percentage else NA_real_
    
    nba_player_box_filtered$UCFGM[row_index]     <- if ("uncontested_field_goals_made"      %in% names(matching_trk)) matching_trk$uncontested_field_goals_made      else NA_real_
    nba_player_box_filtered$UCFGA[row_index]     <- if ("uncontested_field_goals_attempted" %in% names(matching_trk)) matching_trk$uncontested_field_goals_attempted else NA_real_
    nba_player_box_filtered$UCFG_PCT[row_index]  <- if ("uncontested_field_goals_percentage" %in% names(matching_trk)) matching_trk$uncontested_field_goals_percentage else NA_real_
    
    nba_player_box_filtered$DFGM[row_index]      <- if ("defended_at_rim_field_goals_made"        %in% names(matching_trk)) matching_trk$defended_at_rim_field_goals_made        else NA_real_
    nba_player_box_filtered$DFGA[row_index]      <- if ("defended_at_rim_field_goals_attempted"   %in% names(matching_trk)) matching_trk$defended_at_rim_field_goals_attempted   else NA_real_
    nba_player_box_filtered$DFG_PCT[row_index]   <- if ("defended_at_rim_field_goal_percentage"   %in% names(matching_trk)) matching_trk$defended_at_rim_field_goal_percentage   else NA_real_
  }
  
  Sys.sleep(1)
}


rm(away_trk, home_trk, matching_trk, player_track, pt_res)

cat("\n✅ Player-track v3 metrics added to nba_player_box_filtered.\n")
assign("nba_player_box_filtered", nba_player_box_filtered)


##### 5. ***END*** Boxscore Player Track V3 Data Extraction (player) ----



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Retrieve CHAN_OREB, CHAN_DREB, CHAN_REB, CFGM, CFGA, CFG_PCT, UCFGM, UCFGA, UCFG_PCT, DFGM, DFGA, DFG_PCT from nba_boxscoreplayertrackv2 ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Retrieve DFL, OLBR, DLBR, LBR, BOXREB, BOXOUTS, CHRGDRWN, SCRN_AST, SCRN_AST_PTS from nba_hustlestatsboxscore ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

nba_player_box_filtered <- nba_player_box_filtered

# 1) make sure target cols exist
hustle_cols <- c(
  "DFL",        # DEFLECTIONS
  "OLBR",       # OFF_LOOSE_BALLS_RECOVERED
  "DLBR",       # DEF_LOOSE_BALLS_RECOVERED
  "LBR",        # LOOSE_BALLS_RECOVERED
  "BOXREB",     # BOX_OUT_PLAYER_REBS
  "BOXOUTS",    # BOX_OUTS
  "CHRGDRWN",   # CHARGES_DRAWN
  "SCRN_AST",    # SCREEN_ASSISTS
  "SCRN_AST_PTS"  # SCREEN_AST_PTS
)
for (col in hustle_cols) {
  if (!col %in% names(nba_player_box_filtered)) {
    nba_player_box_filtered[[col]] <- NA_real_
  }
}

# 2) distinct games from player df
game_ids <- nba_player_box_filtered %>%
  distinct(nba_game_id) %>%
  filter(!is.na(nba_game_id), nba_game_id != "") %>%
  pull(nba_game_id)

total_games <- length(game_ids)

for (g_i in seq_along(game_ids)) {
  g <- game_ids[g_i]
  
  cat("\n------------------------------\n")
  cat("HUSTLE -> game_id:", g, "(", g_i, "of", total_games, ")\n")
  
  # 3) pull hustle for this game
  hus_res <- tryCatch({
    nba_hustlestatsboxscore(game_id = g)
  }, error = function(e) {
    message("hustle error for game_id=", g, " -> ", e$message)
    return(NULL)
  })
  
  if (is.null(hus_res)) {
    cat("  no hustle response for this game\n")
    Sys.sleep(1)
    next
  }
  
  # most seasons it’s `PlayerStats`
  if (!"PlayerStats" %in% names(hus_res)) {
    cat("  PlayerStats tibble not found for this game\n")
    Sys.sleep(1)
    next
  }
  
  player_hustle <- hus_res$PlayerStats
  
  # filter to this game if GAME_ID present
  if ("GAME_ID" %in% names(player_hustle)) {
    player_hustle <- player_hustle %>%
      dplyr::filter(GAME_ID == !!g)
  }
  
  if (nrow(player_hustle) == 0) {
    cat("  no player hustle rows for this game\n")
    Sys.sleep(1)
    next
  }
  
  # 4) rows from *our* df for THIS game
  idx_game <- which(nba_player_box_filtered$nba_game_id == g)
  num_players_this_game <- length(idx_game)
  
  for (p_i in seq_along(idx_game)) {
    row_index <- idx_game[p_i]
    
    this_player_name <- nba_player_box_filtered$player_name[row_index]
    this_player_id   <- nba_player_box_filtered$nba_player_id[row_index]
    
    print(paste(
      "Processing game_id:", g,
      "player:", this_player_name,
      "(", p_i, "of", num_players_this_game, ")"
    ))
    
    # --- match phase ---
    matching_hus <- NULL
    
    # 1) by ID
    if (!is.na(this_player_id) && this_player_id != "" &&
        "PLAYER_ID" %in% names(player_hustle)) {
      matching_hus <- player_hustle %>%
        dplyr::filter(PLAYER_ID == this_player_id)
    }
    
    # 2) fallback by name
    if ((is.null(matching_hus) || nrow(matching_hus) == 0) &&
        !is.na(this_player_name) && this_player_name != "") {
      
      name_col <- intersect(c("PLAYER_NAME", "player_name"), names(player_hustle))
      if (length(name_col) == 1) {
        nmcol <- name_col[[1]]
        matching_hus <- player_hustle %>%
          dplyr::filter(.data[[nmcol]] == this_player_name)
      }
    }
    
    # still nothing → skip
    if (is.null(matching_hus) || nrow(matching_hus) == 0) {
      next
    }
    
    # take first
    matching_hus <- matching_hus[1, ]
    
    # update player id on our df if we matched on name
    if ("PLAYER_ID" %in% names(matching_hus)) {
      nba_player_box_filtered$nba_player_id[row_index] <- as.character(matching_hus$PLAYER_ID)
    }
    
    # --- write back safely ---
    nba_player_box_filtered$DFL[row_index]        <- if ("DEFLECTIONS" %in% names(matching_hus)) matching_hus$DEFLECTIONS else NA_real_
    nba_player_box_filtered$OLBR[row_index]       <- if ("OFF_LOOSE_BALLS_RECOVERED" %in% names(matching_hus)) matching_hus$OFF_LOOSE_BALLS_RECOVERED else NA_real_
    nba_player_box_filtered$DLBR[row_index]       <- if ("DEF_LOOSE_BALLS_RECOVERED" %in% names(matching_hus)) matching_hus$DEF_LOOSE_BALLS_RECOVERED else NA_real_
    nba_player_box_filtered$LBR[row_index]        <- if ("LOOSE_BALLS_RECOVERED" %in% names(matching_hus)) matching_hus$LOOSE_BALLS_RECOVERED else NA_real_
    nba_player_box_filtered$BOXREB[row_index]     <- if ("BOX_OUT_PLAYER_REBS" %in% names(matching_hus)) matching_hus$BOX_OUT_PLAYER_REBS else NA_real_
    nba_player_box_filtered$BOXOUTS[row_index]    <- if ("BOX_OUTS" %in% names(matching_hus)) matching_hus$BOX_OUTS else NA_real_
    nba_player_box_filtered$CHRGDRWN[row_index]   <- if ("CHARGES_DRAWN" %in% names(matching_hus)) matching_hus$CHARGES_DRAWN else NA_real_
    nba_player_box_filtered$SCRN_AST[row_index]    <- if ("SCREEN_ASSISTS" %in% names(matching_hus)) matching_hus$SCREEN_ASSISTS else NA_real_
    nba_player_box_filtered$SCRN_AST_PTS[row_index] <- if ("SCREEN_AST_PTS" %in% names(matching_hus)) matching_hus$SCREEN_AST_PTS else NA_real_
  }
  
  Sys.sleep(1)
}

rm(matching_hus, hus_res, player_hustle)
cat("\n✅ Player-level Hustle metrics added to nba_player_box_filtered.\n")
assign("nba_player_box_filtered", nba_player_box_filtered)

##### 6. ***END*** ----------------------------------------------------


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Retrieve DFL, OLBR, DLBR, LBR, BOXREB, BOXOUTS, CHRGDRWN, SCRN_AST, SCRN_AST_PTS from nba_hustlestatsboxscore ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Clean up BaseStats_Player_(formatted_date) data frame and export to Datahub ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# Rename game_id.x to espn_game_id and team_id to espn_team_id
if ("game_id" %in% colnames(nba_player_box_filtered)) {
  colnames(nba_player_box_filtered)[colnames(nba_player_box_filtered) == "game_id"] <- "espn_game_id"
}

if ("player_ID" %in% colnames(nba_player_box_filtered)) {
  colnames(nba_player_box_filtered)[colnames(nba_player_box_filtered) == "player_ID"] <- "espn_player_id"
}


# 8.2 Apply column deletions safely to nba_player_box_filtered
tryCatch({
  nba_player_box_filtered <- nba_player_box_filtered %>%
    select( -athlete_id, -T_REB)
}, error = function(e) {
  message("Error during column removal: ", e$message)
})


tryCatch({
  BaseStats_Player <- BaseStats_Player %>%
    select( -athlete_id)
}, error = function(e) {
  message("Error during column removal: ", e$message)
})


# 8.3 Ensure all numeric-looking columns in nba_player_box_filtered are formatted consistently
numeric_columns_as_double <- c(
  "nba_team_id", "TS_PCT", "NET_RTG", "USG_PCT", "OFF_RTG", "DEF_RTG", "POSS","DFG_PCT", "AST_TOV", 
  "AST_PCT", "OREB_PCT", "DREB_PCT", "REB_PCT", "EFG_PCT", "3PT_PCT"
)

numeric_columns_as_integer <- c(
  "espn_game_id", "nba_game_id", "nba_player_id", "espn_player_id", "espn_team_id", "team_score", "opp_score",
  "MIN", "PTS", "FGA", "FGM", "3PTA", "3PTM",
  "REB", "OREB", "DREB", "CHAN_OREB", "CHAN_DREB", "CHAN_REB", "BOXREB", 
  "BOXOUTS", "DFL", "OLBR", "DLBR", "LBR", "SCRN_AST", "SCRN_AST_PTS", "CFGM", 
  "CFGA", "DFGM", "DFGA", "CFG_PCT", "UCFGM", "UCFGA", "UCFG_PCT", 
  "SEC_CHN_PTS","CHRGDRWN", "OFF_TOV_PTS", "FBRK_PTS", "PITP","P_M", "AST", "STL", "BLK"
)

# Convert character columns in numeric lists to appropriate types
nba_player_box_filtered[numeric_columns_as_double] <- lapply(
  nba_player_box_filtered[numeric_columns_as_double], 
  function(x) if (is.character(x)) suppressWarnings(as.double(x)) else as.double(x)
)
nba_player_box_filtered[numeric_columns_as_integer] <- lapply(
  nba_player_box_filtered[numeric_columns_as_integer], 
  function(x) if (is.character(x)) suppressWarnings(as.integer(x)) else as.integer(x)
)

# 8.4 Apply column relocations safely to nba_player_box_filtered
tryCatch({
  nba_player_box_filtered <- nba_player_box_filtered %>%
    relocate(nba_player_id, .after = espn_player_id) %>%
    relocate(espn_player_id, .after = espn_game_id) %>%
    relocate(espn_team_id, .after = espn_game_id) %>%
    relocate(nba_game_id, .after = espn_game_id) %>%
    relocate(is_home, .after = home_away) %>%
    relocate(is_away, .after = home_away) %>%
    relocate(TS_PCT, .after = `3PT_PCT`) %>%
    relocate(PITP, .after = `3PT_PCT`) %>%
    relocate(FBRK_PTS, .after = `3PT_PCT`) %>%
    relocate(OFF_TOV_PTS, .after = `3PT_PCT`) %>%
    relocate(SEC_CHN_PTS, .after = `3PT_PCT`) %>%
    relocate(AST_PCT, .after = AST) %>%
    relocate(AST_TOV, .after = AST) %>%
    relocate(STL, .after = CHAN_REB) %>%
    relocate(BLK, .after = CHAN_REB) %>%
    relocate(CHAN_REB, .after = DREB) %>%
    relocate(CHAN_OREB, .after = DREB) %>%
    relocate(CHAN_DREB, .after = DREB) %>%
    relocate(EFG_PCT, .after = TS_PCT) %>%
    relocate(REB_PCT, .after = DREB) %>%
    relocate(OREB_PCT, .after = DREB) %>%
    relocate(DREB_PCT, .after = DREB)
}, error = function(e) {
  message("Error during column relocation: ", e$message)
})



# Ensure game_date is a character before merging
BaseStats_Player$game_date <- as.character(BaseStats_Player$game_date)
nba_player_box_filtered$game_date <- as.character(nba_player_box_filtered$game_date)

# 8.6 Append aligned nba_player_box_filtered to BaseStats_Player
BaseStats_Player <- bind_rows(BaseStats_Player, nba_player_box_filtered)

# 8.7 Export the updated BaseStats_Player data frame
tryCatch({
  player_output_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_", season_token, ".csv")
  write.csv(BaseStats_Player, file = player_output_path, row.names = FALSE)
  print(paste("BaseStats_Player has been exported to:", player_output_path))
}, error = function(e) {
  message("Error during data export: ", e$message)
})

# 8.8 Debug: Print completion message
print("Data processing, aligning, merging, and exporting completed successfully.")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Clean up BaseStats_Player_(formatted_date) data frame and export to Datahub ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀