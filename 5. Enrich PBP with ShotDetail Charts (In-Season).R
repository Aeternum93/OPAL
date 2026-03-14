# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Pull NBAPBP data and enrich it with shotchartdetail for shot locations and types# ==== 
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

# Load necessary libraries
gc()
library(dplyr)
library(readr)
library(hoopR)

# Define paths
pbp_path <-  paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")

if (file.exists(pbp_path) && file.info(pbp_path)$size > 0) {
  nbapbp_df <- read.csv(pbp_path, stringsAsFactors = FALSE)
  if ("game_id" %in% names(nbapbp_df)) nbapbp_df$game_id <- as.character(nbapbp_df$game_id)
  if ("game_date"  %in% names(nbapbp_df)) nbapbp_df$game_date  <- as.Date(nbapbp_df$game_date)
} else {
  BaseStats_Team <- data.frame()
}



# 2.1 Path
base_stats_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token, ".csv"
)

# 2.2 Load BaseStats_Team (if exists)
if (file.exists(base_stats_path) && file.info(base_stats_path)$size > 0) {
  BaseStats_Team <- read.csv(base_stats_path, stringsAsFactors = FALSE)
  if ("nba_game_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_game_id <- as.character(BaseStats_Team$nba_game_id)
  if ("game_date"  %in% names(BaseStats_Team)) BaseStats_Team$game_date  <- as.Date(BaseStats_Team$game_date)
} else {
  BaseStats_Team <- data.frame()
}

# ── Bridge: map ESPN game_id → nba_game_id onto PBP ──────────────────────────
game_id_map <- BaseStats_Team %>%
  dplyr::mutate(
    espn_game_id = as.character(espn_game_id),
    nba_game_id  = as.character(nba_game_id)
  ) %>%
  dplyr::distinct(espn_game_id, nba_game_id) %>%
  dplyr::filter(!is.na(espn_game_id), !is.na(nba_game_id), nzchar(nba_game_id))

# Drop existing nba_game_id if it exists to avoid .x/.y conflict
nbapbp_df <- nbapbp_df %>%
  dplyr::select(-dplyr::any_of("nba_game_id"))

# Step 1: join first
nbapbp_df <- nbapbp_df %>%
  dplyr::mutate(game_id = as.character(game_id)) %>%
  dplyr::left_join(game_id_map, by = c("game_id" = "espn_game_id"))

# Step 2: mutate AFTER join so nba_game_id exists
nbapbp_df <- nbapbp_df %>%
  dplyr::mutate(
    nba_game_id = ifelse(
      startsWith(as.character(nba_game_id), "00"),
      as.character(nba_game_id),
      paste0("00", as.character(nba_game_id))
    )
  )

cat("nba_game_id populated:", sum(!is.na(nbapbp_df$nba_game_id)), "of", nrow(nbapbp_df), "\n")
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: SHOT ZONE BASIC EXIST Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

to_bool <- function(x) tolower(as.character(x)) %in% c("1","true","t","yes","y")
as_date_any <- function(x) { if (inherits(x,"Date")) x else suppressWarnings(as.Date(x)) }
pad00 <- function(x) ifelse(startsWith(as.character(x), "00"), as.character(x), paste0("00", as.character(x)))

if (!"SHOT_ZONE_BASIC" %in% names(nbapbp_df)) {
  message("SHOT_ZONE_BASIC not found — running for ALL games from BaseStats_Team.")
  
  # Option A: all games in BaseStats_Team
  game_ids <- BaseStats_Team %>%
    dplyr::distinct(nba_game_id) %>%
    dplyr::pull(nba_game_id) %>%
    as.character() %>%
    unique() %>%
    pad00() %>%
    sort()
  
  message(sprintf("Running logic for %d nba_game_id values (SHOT_ZONE_BASIC missing).", length(game_ids)))
  
} else {
  message("SHOT_ZONE_BASIC found — targeting only shots with missing zone info (non-Heave, non-FT).")
  
  # 1) Build df_filtered exactly as you defined
  df_filtered <- nbapbp_df %>%
    dplyr::filter(
      to_bool(shooting_play),
      !grepl("Heave", type_text, ignore.case = TRUE),
      !grepl("Free Throw", type_text, ignore.case = TRUE),
      is.na(SHOT_ZONE_BASIC) | trimws(SHOT_ZONE_BASIC) == ""
    )
  
  # 2) Use ONLY the games that actually have missing SHOT_ZONE_BASIC
  game_ids <- df_filtered %>%
    dplyr::distinct(nba_game_id) %>%
    dplyr::pull(nba_game_id) %>%
    as.character() %>%
    unique() %>%
    pad00() %>%
    sort()
  
  message(sprintf("Running logic for %d nba_game_id values (SHOT_ZONE_BASIC present, but missing for some plays).",
                  length(game_ids)))
}


# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: SHOT ZONE BASIC EXIST Section ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀


# Accumulator for all games
shotchart_full_df <- dplyr::tibble()

# Loop through each nba_game_id
for (i in seq_along(game_ids)) {
  game_id <- game_ids[i]
  
  print(paste("Processing shot data for game_id:", game_id, "(", i, "of", length(game_ids), ")"))
  flush.console()
  
  # Try pulling with Regular Season first
  df_shotchart <- tryCatch({
    nba_shotchartdetail(
      context_measure = "FGA",
      date_from = "",
      date_to = "",
      game_id = game_id,
      game_segment = "",
      last_n_games = 0,
      league_id = "00",
      location = "",
      month = 0,
      opponent_team_id = 0,
      outcome = "",
      period = 0,
      player_id = 0,
      player_position = "",
      rookie_year = "",
      season = season_token2,
      season_segment = "",
      season_type = "Regular Season",
      team_id = 0,
      vs_conference = "",
      vs_division = ""
    )$Shot_Chart_Detail
  }, error = function(e) {
    message(paste("Error fetching Regular Season shotchart for game_id:", game_id, "Error:", e$message))
    return(NULL)
  })
  
  # If NULL or no rows, retry with Playoffs
  if (is.null(df_shotchart) || nrow(df_shotchart) == 0) {
    message(paste("Retrying with Playoffs for game_id:", game_id))
    
    df_shotchart <- tryCatch({
      nba_shotchartdetail(
        context_measure = "FGA",
        date_from = "",
        date_to = "",
        game_id = game_id,
        game_segment = "",
        last_n_games = 0,
        league_id = "00",
        location = "",
        month = 0,
        opponent_team_id = 0,
        outcome = "",
        period = 0,
        player_id = 0,
        player_position = "",
        rookie_year = "",
        season = season_token2,  # Recommended dynamic season logic
        season_segment = "",
        season_type = "Playoffs",
        team_id = 0,
        vs_conference = "",
        vs_division = ""
      )$Shot_Chart_Detail
    }, error = function(e) {
      message(paste("❌ API error fetching Playoffs shotchart for game_id:", game_id, "Error:", e$message))
      return(NULL)
    })
    
    if (!is.null(df_shotchart) && nrow(df_shotchart) == 0) {
      message(paste("❌ No shotchart data found after retrying Playoffs for game_id:", game_id))
    }
  }
  
  # Append if data exists
  if (!is.null(df_shotchart) && nrow(df_shotchart) > 0) {
    df_shotchart_clean <- df_shotchart %>%
      dplyr::select(GAME_ID, GAME_EVENT_ID, PLAYER_NAME,
                    SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE,
                    SHOT_DISTANCE, LOC_X, LOC_Y) %>%
      dplyr::rename(
        nba_game_id = GAME_ID,
        sequence_number = GAME_EVENT_ID
      )
    
    # <-- append to the season accumulator (do NOT bind to itself)
    shotchart_full_df <- dplyr::bind_rows(shotchart_full_df, df_shotchart_clean)
  }
  
  Sys.sleep(1)
}


nbapbp_df$game_id <- as.character(nbapbp_df$game_id)
BaseStats_Team$espn_game_id <- as.character(BaseStats_Team$espn_game_id)


# ================== FINAL MERGE & SAVE (using shotchart_full_df) ==================

# Normalize PBP join keys
nbapbp_df <- nbapbp_df %>%
  dplyr::mutate(
    nba_game_id     = as.character(nba_game_id),
    sequence_number = as.character(sequence_number)
  ) %>%
  dplyr::mutate(
    nba_game_id = ifelse(
      startsWith(as.character(nba_game_id), "00"),
      as.character(nba_game_id),
      paste0("00", as.character(nba_game_id))
    )
  )

# Build shotchart side from the accumulator
df_sc <- shotchart_full_df %>%
  dplyr::mutate(
    nba_game_id     = as.character(nba_game_id),
    sequence_number = as.character(sequence_number)
  )

# Keep only rows that correspond to actual shot plays in PBP
to_bool <- function(x) tolower(as.character(x)) %in% c("true","t","1","yes","y")
pbp_shot_keys <- nbapbp_df %>%
  dplyr::filter(to_bool(shooting_play)) %>%
  dplyr::distinct(nba_game_id, sequence_number)

df_sc <- dplyr::semi_join(df_sc, pbp_shot_keys, by = c("nba_game_id","sequence_number"))

# Deduplicate to 1 row per (game,event)
df_sc <- df_sc %>%
  dplyr::arrange(nba_game_id, sequence_number) %>%
  dplyr::group_by(nba_game_id, sequence_number) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::coalesce(dplyr::first(.[!is.na(.)]), dplyr::first(.))
    ),
    .groups = "drop"
  )

stopifnot(!any(duplicated(df_sc[c("nba_game_id","sequence_number")])))

n_before <- nrow(nbapbp_df)

# ---------- MERGE LOGIC WITH IF BRANCH ----------

if (!"SHOT_ZONE_BASIC" %in% names(nbapbp_df)) {
  
  # First-time enrichment: just join in all shotchart fields
  nbapbp_df <- nbapbp_df %>%
    dplyr::left_join(df_sc, by = c("nba_game_id", "sequence_number"))
  
} else {
  
  # Patch existing cols instead of creating .x / .y
  patch_cols <- setdiff(
    intersect(names(df_sc), names(nbapbp_df)),
    c("nba_game_id", "sequence_number")
  )
  # patch_cols should include:
  # "PLAYER_NAME", "SHOT_ZONE_BASIC", "SHOT_ZONE_AREA",
  # "SHOT_ZONE_RANGE", "SHOT_DISTANCE", "LOC_X", "LOC_Y", etc.
  
  # --- Convert blank strings to NA *only* for character patch columns ---
  char_patch_cols <- patch_cols[
    vapply(nbapbp_df[patch_cols], is.character, logical(1))
  ]
  
  if (length(char_patch_cols) > 0) {
    nbapbp_df <- nbapbp_df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(char_patch_cols),
          ~ dplyr::na_if(., "")
        )
      )
  }
  
  # Ensure numeric columns are integers
  numeric_columns_as_integer <- c("SHOT_DISTANCE", "LOC_X", "LOC_Y")
  
  nbapbp_df <- nbapbp_df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(intersect(numeric_columns_as_integer, names(.))),
      ~ suppressWarnings(as.integer(.))
    ))
  
  df_sc <- df_sc %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(intersect(numeric_columns_as_integer, names(.))),
      ~ suppressWarnings(as.integer(.))
    ))
  
  # Rename df_sc patch columns
  df_sc_renamed <- df_sc %>%
    dplyr::rename_with(~ paste0(.x, "_sc"), dplyr::all_of(patch_cols))
  
  # Patch existing columns from *_sc
  nbapbp_df <- nbapbp_df %>%
    dplyr::left_join(df_sc_renamed, by = c("nba_game_id","sequence_number"))
  
  for (col in patch_cols) {
    sc_col <- paste0(col, "_sc")
    if (sc_col %in% names(nbapbp_df)) {
      nbapbp_df[[col]] <- dplyr::coalesce(nbapbp_df[[col]], nbapbp_df[[sc_col]])
    }
  }
  
  nbapbp_df <- nbapbp_df %>%
    dplyr::select(-dplyr::ends_with("_sc"))
}

stopifnot(nrow(nbapbp_df) == n_before)

# --- Save updated file ---
write_csv(nbapbp_df, pbp_path)

print("✅ All shot location and detail data successfully merged into nbapbp.csv.")



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Pull NBAPBP data and enrich it with shotchartdetail for shot locations and types# ==== 
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# Direct test - does the join actually produce nba_game_id?
test <- nbapbp_df %>%
  dplyr::mutate(game_id = as.character(game_id)) %>%
  dplyr::left_join(game_id_map, by = c("game_id" = "espn_game_id"))

cat("nba_game_id in test:", "nba_game_id" %in% names(test), "\n")
cat("nba_game_id non-NA:", sum(!is.na(test$nba_game_id)), "of", nrow(test), "\n")
cat("test names:", names(test)[1:10], "\n")
cat("nba_game_id already in nbapbp_df:", "nba_game_id" %in% names(nbapbp_df), "\n")