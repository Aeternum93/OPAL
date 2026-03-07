# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
#
#     oooooooooo.               o8o  oooo        .o8       ooooo      ooo oooooooooo.        .o.       ooooooooo.   oooooooooo.  ooooooooo.        oooooooooooo  o8o  oooo            
#     `888'   `Y8b              `"'  `888       "888       `888b.     `8' `888'   `Y8b      .888.      `888   `Y88. `888'   `Y8b `888   `Y88.      `888'     `8  `"'  `888            
#      888     888 oooo  oooo  oooo   888   .oooo888        8 `88b.    8   888     888     .8"888.      888   .d88'  888     888  888   .d88'       888         oooo   888   .ooooo.  
#      888oooo888' `888  `888  `888   888  d88' `888        8   `88b.  8   888oooo888'    .8' `888.     888ooo88P'   888oooo888'  888ooo88P'        888oooo8    `888   888  d88' `88b 
#      888    `88b  888   888   888   888  888   888        8     `88b.8   888    `88b   .88ooo8888.    888          888    `88b  888               888    "     888   888  888ooo888 
#      888    .88P  888   888   888   888  888   888        8       `888   888    .88P  .8'     `888.   888          888    .88P  888               888          888   888  888    .o 
#     o888bood8P'   `V88V"V8P' o888o o888o `Y8bod88P"      o8o        `8  o888bood8P'  o88o     o8888o o888o        o888bood8P'  o888o             o888o        o888o o888o `Y8bod8P' 
#                                                                                                                                                                                
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀                                                                                                                                                                                
          




# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Load nba_pbp and run missing date logic ====
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


library(hoopR)
library(dplyr)
library(data.table)
library(lubridate)



new_nbapbp_df <- load_nba_pbp(seasons = pbp_season)


# 1.1 Define the path to the nbapbp_df CSV file`
nba_pbp_path <- paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")

# 1.2 Load nbapbp_df into a data frame
if (file.exists(nba_pbp_path) && file.info(nba_pbp_path)$size > 0) {
  nbapbp_df <- read.csv(
    file = nba_pbp_path,
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
} else {
  nbapbp_df <- data.frame()  # Create an empty data frame if the file doesn't exist or is empty
}


# 2.1 Path
team_output_path <- paste0(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_",
  season_token, ".csv"
)

# 2.2 Load BaseStats_Team (if exists)
if (file.exists(team_output_path) && file.info(team_output_path)$size > 0) {
  BaseStats_Team <- read.csv(team_output_path, stringsAsFactors = FALSE)
  if ("nba_game_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_game_id <- as.character(BaseStats_Team$nba_game_id)
  if ("espn_game_id" %in% names(BaseStats_Team)) BaseStats_Team$espn_game_id <- as.character(BaseStats_Team$espn_game_id)
  if ("nba_team_id" %in% names(BaseStats_Team)) BaseStats_Team$nba_team_id <- as.character(BaseStats_Team$nba_team_id)
  if ("game_date"  %in% names(BaseStats_Team)) BaseStats_Team$game_date  <- as.Date(BaseStats_Team$game_date)
} else {
  BaseStats_Team <- data.frame()
}


# --- Append only NEW games from new_nbapbp_df to nbapbp_df (preserve new file order) ---

# ensure Date type
as_date_any <- function(x) { if (inherits(x,"Date")) x else suppressWarnings(as.Date(x)) }
if (nrow(nbapbp_df))     nbapbp_df$game_date     <- as_date_any(nbapbp_df$game_date)
if (exists("new_nbapbp_df")) new_nbapbp_df$game_date <- as_date_any(new_nbapbp_df$game_date)

# games we already have
have_ids <- if (nrow(nbapbp_df)) unique(nbapbp_df$game_id) else character(0)

# keep ONLY rows for game_ids not already present; keep original row order from new file
to_append <- if (exists("new_nbapbp_df")) dplyr::filter(new_nbapbp_df, !game_id %in% have_ids) else dplyr::slice(new_nbapbp_df, 0)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Load nba_pbp and run missing date logic ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀



# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== START: Write nbapbp to csv ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀

# --- brute-force safe append: coerce everything to character, then append ONCE ---
stack_as_char <- function(a, b) {
  a <- data.table::as.data.table(a)
  b <- data.table::as.data.table(b)
  
  # align column sets
  cols <- union(names(a), names(b))
  for (cn in setdiff(cols, names(a))) a[, (cn) := NA_character_]
  for (cn in setdiff(cols, names(b))) b[, (cn) := NA_character_]
  data.table::setcolorder(a, cols)
  data.table::setcolorder(b, cols)
  
  # coerce ALL columns to character to remove class/attr conflicts (Date, POSIXct, factor, etc.)
  for (cn in cols) {
    data.table::set(a, j = cn, value = as.character(a[[cn]]))
    data.table::set(b, j = cn, value = as.character(b[[cn]]))
  }
  
  # append once
  data.table::rbindlist(list(a, b), use.names = TRUE, fill = TRUE)
}

# ---- normalize + append ----
nbapbp_df <- stack_as_char(nbapbp_df, to_append)

# ---- (optional) cast a few key columns back after append ----
if ("game_date" %in% names(nbapbp_df)) nbapbp_df[, game_date := as.Date(game_date)]
if ("game_id"   %in% names(nbapbp_df)) nbapbp_df[, game_id   := as.character(game_id)]

# Confirm expected columns exist
stopifnot("game_id" %in% names(nbapbp_df))
stopifnot(all(c("espn_game_id","nba_game_id") %in% names(BaseStats_Team)))


BaseStats_Team <- BaseStats_Team %>%
  mutate(espn_game_id = as.character(espn_game_id))

# --- Build clean 1:1 map and add nba_game_id ---
map_ids <- BaseStats_Team %>%
  dplyr::filter(!is.na(espn_game_id), !is.na(nba_game_id)) %>%
  dplyr::distinct(espn_game_id, .keep_all = TRUE) %>%
  dplyr::transmute(
    game_id = as.character(espn_game_id),
    nba_game_id_src  = as.character(nba_game_id),
    nba_team_id_src = as.character(nba_team_id)   # <-- add this
  )


nbapbp_df <- nbapbp_df %>%
  dplyr::mutate(game_id = as.character(game_id)) %>%
  dplyr::filter(!is.na(game_id), trimws(game_id) != "") %>%
  dplyr::left_join(map_ids, by = "game_id") %>%
  dplyr::mutate(
    nba_game_id = dplyr::if_else(!is.na(nba_game_id_src) & nzchar(nba_game_id_src),
                                 paste0("00", nba_game_id_src),
                                 NA_character_),
    nba_team_id = dplyr::if_else(!is.na(nba_team_id_src) & nzchar(nba_team_id_src),
                                 nba_team_id_src,
                                 NA_character_)
  ) %>%
  dplyr::select(-nba_game_id_src, -nba_team_id_src)


# --- Normalize blanks to NA (chars only), then drop fully-empty rows ---
nbapbp_df <- nbapbp_df %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ na_if(trimws(.), ""))) %>%
  dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))



# ---- write combined file ----
fwrite(
  nbapbp_df,
  paste0("C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/nbapbp_", season_token, ".csv")
)

# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀
# ==== END: Write nbapbp to csv ====
# 🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀🏀