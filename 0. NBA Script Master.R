# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
#
#     ooooo      ooo oooooooooo.        .o.             .oooooo..o                     o8o                 .        ooo        ooooo                        .                      
#     `888b.     `8' `888'   `Y8b      .888.           d8P'    `Y8                     `"'               .o8        `88.       .888'                      .o8                      
#      8 `88b.    8   888     888     .8"888.          Y88bo.       .ooooo.  oooo d8b oooo  oo.ooooo.  .o888oo       888b     d'888   .oooo.    .oooo.o .o888oo  .ooooo.  oooo d8b 
#      8   `88b.  8   888oooo888'    .8' `888.          `"Y8888o.  d88' `"Y8 `888""8P `888   888' `88b   888         8 Y88. .P  888  `P  )88b  d88(  "8   888   d88' `88b `888""8P 
#      8     `88b.8   888    `88b   .88ooo8888.             `"Y88b 888        888      888   888   888   888         8  `888'   888   .oP"888  `"Y88b.    888   888ooo888  888     
#      8       `888   888    .88P  .8'     `888.       oo     .d8P 888   .o8  888      888   888   888   888 .       8    Y     888  d8(  888  o.  )88b   888 . 888    .o  888     
#     o8o        `8  o888bood8P'  o88o     o8888o      8""88888P'  `Y8bod8P' d888b    o888o  888bod8P'   "888"      o8o        o888o `Y888""8o 8""888P'   "888" `Y8bod8P' d888b    
#                                                                                            888                                                                                   
#                                                                                           o888o                                                                                  
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐





# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# ==== START: Date Setting and Initial Load Logic ====
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐

# ============================================================
# 0. NBA Script Master (Shiny gadget with batches)
# ============================================================

gc()
rm(list = ls())

library(hoopR)
library(dplyr)
library(data.table)
library(lubridate)
library(shiny)
library(miniUI)
library(rstudioapi)

# --- NBA logo resource path ---------------------------------
logo_dir <- "C:/Users/Austin/OneDrive/Desktop/0/Logos"
addResourcePath("logos", logo_dir)

# ------------------------------------------------------------
# Date / season logic (runs ONCE, shared by all sourced scripts)
# ------------------------------------------------------------

current_date   <- Sys.Date()
next_year_date <- current_date + lubridate::years(1)

formatted_date <- format(current_date, "%Y%m%d")
formatted_year <- as.numeric(format(current_date, "%Y"))

pbp_season    <- 2026
season_token  <- "2025_2026"
season_token2 <- "2025-26"

cat("NBA Master Runner\n")
cat("Current date:", current_date,
    "| formatted:", formatted_date,
    "| season_token:", season_token, "\n")


# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# ==== END: Date Setting and Initial Load Logic ====
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐



# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# ==== START: NBA Master Script UI Loader ====
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐

# ------------------------------------------------------------
# Script registry
# ------------------------------------------------------------

base_path <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts"

scripts <- list(
  "1. Initial Data Upload BaseStats_Team (In-Season)" =
    "1. Initial Data Upload BaseStats_Team (In-Season).R",
  "2. Initial Data Download nbapbp Load (In-Season)" =
    "2. Initial Data Download nbapbp Load (In-Season).R",
  "3. Initial Data Upload BaseStats_Player (In-Season)" =
    "3. Initial Data Upload BaseStats_Player (In-Season).R",
  "4. Player Shot Dashboard Chart Pull (In-Season)" =
    "4. Player Shot Dashboard Chart Pull (In-Season).R",
  "5. Enrich PBP with ShotDetail Charts (In-Season)" =
    "5. Enrich PBP with ShotDetail Charts (In-Season).R",
  "6. NBA API Rotation Data Scraping (In-Season)" =
    "6. NBA API Rotation Data Scraping (In-Season).R",
  "7. Enrich PBP with Popcorn Machine (In-Season)" =
    "7. Enrich PBP with Popcorn Machine (In-Season).R",
  "8. Create Team and Player Rotation Projections (In-Season)" =
    "8. Create Team and Player Rotation Projections (In-Season).R",
  "9. Odds API Pull for NBA Teams v_3 (In-Season)" =
    "9. Odds API Pull for NBA Teams v_3 (In-Season).R",
  "10. Odds API Pull for NBA Players (In-Season)" =
    "10. Odds API Pull for NBA Players (In-Season).R",
  "11. Create BaseStats_Team_MC from pm_nbapbp file" =
    "11. Create BaseStats_Team_MC from pm_nbapbp file.R",
  "12. Create BaseStats_Player_MC from pm_nbapbp file" =
    "12. Create BaseStats_Player_MC from pm_nbapbp file.R",
  "13. NBA Injury Data Aggregation (In-Season)" =
    "13. NBA API Injury Data Aggregation (In-Season).R",
  "14. AURELIUS MC (In-Season)" =
    "14. AURELIUS MC (In-Season).R",
  "15. AVITUS LOGREG (In-Season)" =
    "15. AVITUS LOGREG (In-Season).R"
)

# ------------------------------------------------------------
# Batches
# ------------------------------------------------------------

batches <- list(
  "Batch 1: BaseStats_Team, nbapbp and BaseStats_Player 1–3"  = c(
    "1. Initial Data Upload BaseStats_Team (In-Season)",
    "2. Initial Data Download nbapbp Load (In-Season)",
    "3. Initial Data Upload BaseStats_Player (In-Season)"
  ),
  "Batch 2: Player Shot Dashboard and Shot Detail Charts 4–5"  = c(
    "4. Player Shot Dashboard Chart Pull (In-Season)",
    "5. Enrich PBP with ShotDetail Charts (In-Season)"
  ),
  "Batch 3: NBA API Rotation Scraping, PM PBP enrichment and Player Rotations 6–8"  = c(
    "6. NBA API Rotation Data Scraping (In-Season)",
    "7. Enrich PBP with Popcorn Machine (In-Season)",
    "8. Create Team and Player Rotation Projections (In-Season)"
  ),
  "Batch 4: Odds API Pull for Team and Players 9–10"  = c(
    "9. Odds API Pull for NBA Teams v_3 (In-Season)",
    "10. Odds API Pull for NBA Players (In-Season)"
  ),
  "Batch 5: Monte Carlo BaseStats_Team and BaseStats_Player 11–12" = c(
    "11. Create BaseStats_Team_MC from pm_nbapbp file",
    "12. Create BaseStats_Player_MC from pm_nbapbp file"
  ),
  "Batch 6: NBA Injury Data Aggregation (In-Season)"     = c(
    "13. NBA Injury Data Aggregation (In-Season)"
  )
)

# ------------------------------------------------------------
# Essential objects that must survive environment clears
# ------------------------------------------------------------

essential_names <- c(
  # core globals
  "current_date", "formatted_date", "formatted_year", "next_year_date",
  "pbp_season", "season_token", "season_token2",
  
  # paths / config
  "base_path", "logo_dir",
  
  # runner config + helpers
  "scripts", "batches", "run_script", "run_batch"
)

# Safe IDs
script_ids <- setNames(gsub("[^A-Za-z0-9]", "_", names(scripts)), names(scripts))
batch_ids  <- setNames(gsub("[^A-Za-z0-9]", "_", names(batches)), names(batches))

# ------------------------------------------------------------
# Helper to run one script (global env so tokens are visible)
# ------------------------------------------------------------

run_script <- function(label) {
  path <- file.path(base_path, scripts[[label]])
  
  cat("\n------------------------------------------------------------\n")
  cat("▶ Running:", label, "\n")
  cat("   File:", path, "\n")
  
  if (!file.exists(path)) {
    cat("✖ File not found, skipping.\n")
    
    # Helpful debug: list candidates if you typo'd
    try({
      fls <- list.files(base_path, pattern = "\\.R$", full.names = FALSE)
      hit <- fls[grepl(gsub("\\s+", ".*", strsplit(label, "\\s+")[[1]][1]), fls, ignore.case = TRUE)]
      if (length(hit) > 0) {
        cat("Available similar files:\n")
        print(head(hit, 10))
      }
    }, silent = TRUE)
    
    showNotification(paste("File not found:", label), type = "error")
    return(invisible(FALSE))
  }
  
  ok <- tryCatch(
    {
      source(path, local = FALSE)  # FALSE -> global env
      TRUE
    },
    error = function(e) {
      cat("✖ Error in", label, ":", conditionMessage(e), "\n")
      showNotification(
        paste("Error in", label, ":", conditionMessage(e)),
        type = "error",
        duration = 10
      )
      FALSE
    }
  )
  
  if (ok) {
    cat("✔ Completed:", label, "\n")
    showNotification(paste("Finished:", label), type = "message")
  }
  
  invisible(ok)
}

run_batch <- function(batch_label) {
  lbls <- batches[[batch_label]]
  showNotification(paste("Running", batch_label, "…"), type = "message")
  for (lbl in lbls) run_script(lbl)
  showNotification(paste(batch_label, "finished."), type = "message", duration = 5)
}

# Make sure child scripts can see essentials
assign("scripts",    scripts,    envir = .GlobalEnv)
assign("batches",    batches,    envir = .GlobalEnv)
assign("base_path",  base_path,  envir = .GlobalEnv)
assign("logo_dir",   logo_dir,   envir = .GlobalEnv)
assign("run_script", run_script, envir = .GlobalEnv)
assign("run_batch",  run_batch,  envir = .GlobalEnv)
assign("essential_names", essential_names, envir = .GlobalEnv)

# ------------------------------------------------------------
# Inline SVG icons (NO image files needed)
# ------------------------------------------------------------

aurelius_icon <- function(size = 54) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = size, height = size, viewBox = "0 0 64 64",
    tags$defs(
      tags$radialGradient(
        id = "oceanG", cx = "35%", cy = "30%", r = "80%",
        tags$stop(offset = "0%",   `stop-color` = "#3C7BD6"),
        tags$stop(offset = "55%",  `stop-color` = "#1D4FA3"),
        tags$stop(offset = "100%", `stop-color` = "#0B2E6E")
      )
    ),
    
    # back earth (top)
    tags$g(
      tags$circle(cx="28", cy="20", r="16", fill="url(#oceanG)", opacity="0.98"),
      tags$path(
        d="M20 19 C18 13, 22 8, 28 9 C33 10, 34 14, 31 16 C28 18, 25 22, 22 22 C21 22, 20.5 20.5, 20 19 Z",
        fill="#2E8B57", opacity="0.95"
      ),
      tags$path(
        d="M30 18 C34 14, 40 17, 38 22 C36 27, 31 26, 29 23 C27 21, 28 20, 30 18 Z",
        fill="#2E8B57", opacity="0.90"
      )
    ),
    
    # right earth
    tags$g(
      tags$circle(cx="42", cy="36", r="16", fill="url(#oceanG)", opacity="0.98"),
      tags$path(
        d="M35 35 C33 29, 38 24, 44 26 C49 28, 49 32, 46 34 C43 36, 41 40, 37 40 C36 40, 35.5 36.5, 35 35 Z",
        fill="#2E8B57", opacity="0.95"
      ),
      tags$path(
        d="M43 33 C47 30, 52 34, 49 39 C46 44, 41 42, 40 39 C39 36, 41 35, 43 33 Z",
        fill="#2E8B57", opacity="0.90"
      )
    ),
    
    # front earth (bottom-left)
    tags$g(
      tags$circle(cx="24", cy="42", r="15", fill="url(#oceanG)", opacity="0.98"),
      tags$path(
        d="M17 41 C15 35, 20 31, 25 33 C30 35, 30 39, 27 41 C24 43, 22 47, 18 47 C17 47, 17.5 42.5, 17 41 Z",
        fill="#2E8B57", opacity="0.95"
      ),
      tags$path(
        d="M25 40 C29 37, 34 41, 31 46 C28 51, 23 49, 22 46 C21 43, 23 42, 25 40 Z",
        fill="#2E8B57", opacity="0.90"
      )
    )
  )
}

avitus_icon <- function(size = 54) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = size, height = size, viewBox = "0 0 64 64",
    tags$defs(
      tags$radialGradient(
        id = "vg1", cx = "35%", cy = "35%", r = "75%",
        tags$stop(offset = "0%",   `stop-color` = "#3A3A3A"),
        tags$stop(offset = "70%",  `stop-color` = "#0F0F0F"),
        tags$stop(offset = "100%", `stop-color` = "#000000")
      )
    ),
    tags$path(
      d="M32 6
         C18 6, 6 18, 6 32
         C6 46, 18 58, 32 58
         C46 58, 58 46, 58 32
         C58 18, 46 6, 32 6 Z",
      fill="url(#vg1)"
    ),
    tags$circle(cx="32", cy="32", r="18", fill="#000000", opacity="0.35"),
    tags$circle(cx="32", cy="32", r="14", fill="#FFFFFF")
  )
}

# ------------------------------------------------------------
# Shiny gadget UI
# ------------------------------------------------------------

ui <- miniPage(
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #1D428A !important; border-color: #1D428A !important; }
      .navbar .navbar-brand, .navbar .navbar-brand:hover { color: #FFFFFF !important; }
      .navbar .btn { background-color: #C8102E !important; border-color: #C8102E !important; color: #FFFFFF !important; }
      .navbar .btn:hover { background-color: #A60D23 !important; border-color: #A60D23 !important; }

      .mini-content, .mini-content-panel { background-color: #F5F5F5; }
      .btn { border-radius: 4px; }

      #run_all { background-color: #1D428A; color: #FFFFFF; border-color: #1D428A; font-weight: 600; }
      #run_all:hover { background-color: #C8102E; border-color: #C8102E; }

      .tileWrap { display:flex; gap:12px; align-items:stretch; }
      .tileCard {
        flex:1; border:1px solid #E5E5E5; border-radius:10px;
        padding:12px; background:#FAFAFA;
      }
      .tileHead { display:flex; align-items:center; gap:10px; }
      .tileTitle { font-weight:800; font-size:16px; line-height:1; }
      .tileSub { color:#666; font-size:12px; margin-top:2px; }
    "))
  ),
  
  gadgetTitleBar(
    title = tags$div(
      style = "width:100%; display:flex; justify-content:center; align-items:center; pointer-events:none;",
      tags$img(src = "logos/Picture1.png", height = "36px", style = "margin-right:10px;"),
      tags$span("Master Runner", style = "font-size:20px; font-weight:600; color:black;")
    )
  ),
  
  miniContentPanel(
    
    # 1) RUN ALL
    div(
      style = "margin: 10px 0;",
      actionButton("run_all", "▶ Run all NBA Scripts (1 → 15)", width = "100%")
    ),
    
    # 2) CLEAR ENV
    div(
      style = "margin: 0 0 15px 0;",
      actionButton(
        "clear_env",
        "⚠ Clear Global Environment",
        style = "background-color:#C8102E; color:white; font-weight:600;",
        width = "100%"
      )
    ),
    
    # 3) GLOBALS panel with RIGHT-side tiles
    div(
      style = "padding: 12px; background:white; border-radius:6px; margin-bottom: 20px;",
      h4("Global Variables", style = "font-weight:600; color:#1D428A; margin-bottom:8px;"),
      
      fluidRow(
        # LEFT: globals inputs
        column(
          width = 5,
          dateInput("ui_current_date", "Current Date:", value = current_date),
          numericInput("ui_pbp_season", "pbp_season:", value = pbp_season, min = 1990, max = 2050),
          textInput("ui_season_token",  "season_token:",  value = season_token),
          textInput("ui_season_token2", "season_token2:", value = season_token2),
          div(
            style="margin-top:10px;",
            actionButton("apply_globals", "Apply Global Settings", width = "100%")
          )
        ),
        
        # RIGHT: AURELIUS / AVITUS tiles
        column(
          width = 7,
          div(
            style="margin-top: 28px;",
            div(
              class = "tileWrap",
              
              div(
                class = "tileCard",
                div(
                  class = "tileHead",
                  aurelius_icon(54),
                  div(
                    tags$div("AURELIUS", class="tileTitle"),
                    tags$div("Monte Carlo", class="tileSub")
                  )
                ),
                div(
                  style="margin-top:10px;",
                  actionButton(
                    "run_aurelius",
                    "Run AURELIUS",
                    width="100%",
                    style="background-color:#1D428A; color:white; font-weight:700;"
                  )
                )
              ),
              
              div(
                class = "tileCard",
                div(
                  class = "tileHead",
                  avitus_icon(54),
                  div(
                    tags$div("AVITUS", class="tileTitle"),
                    tags$div("LogReg", class="tileSub")
                  )
                ),
                div(
                  style="margin-top:10px;",
                  actionButton(
                    "run_avitus",
                    "Run AVITUS",
                    width="100%",
                    style="background-color:#111111; color:white; font-weight:700;"
                  )
                )
              )
              
            )
          )
        )
      )
    ),
    
    # 4) BATCHES
    div(
      style = "padding: 12px; background:white; border-radius:6px; margin-bottom: 20px;",
      h4("Batches", style = "font-weight:600; color:#1D428A; margin-bottom:8px;"),
      div(
        style = "overflow-y:auto; max-height: 180px;",
        lapply(names(batches), function(bl) {
          actionButton(
            inputId = batch_ids[[bl]],
            label = bl,
            width = "100%",
            style = "margin-bottom:6px;"
          )
        })
      )
    ),
    
    # 5) INDIVIDUAL SCRIPTS
    div(
      style = "padding: 12px; background:white; border-radius:6px; margin-bottom: 20px;",
      h4("Individual Scripts", style = "font-weight:600; color:#1D428A; margin-bottom:8px;"),
      div(
        style = "overflow-y:auto; max-height: 360px;",
        lapply(names(scripts), function(lbl) {
          actionButton(
            inputId = script_ids[[lbl]],
            label = lbl,
            width = "100%",
            style = "margin-bottom:6px;"
          )
        })
      )
    )
    
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Apply globals
  observeEvent(input$apply_globals, {
    
    current_date   <<- input$ui_current_date
    formatted_date <<- format(current_date, "%Y%m%d")
    formatted_year <- as.numeric(format(current_date, "%Y"))
    
    pbp_season     <<- input$ui_pbp_season
    season_token   <<- input$ui_season_token
    season_token2  <<- input$ui_season_token2
    
    showNotification("Global settings updated.", type = "message")
    
    cat("\n=== Updated Global Variables ===\n")
    cat("current_date   :", current_date, "\n")
    cat("formatted_date :", formatted_date, "\n")
    cat("formatted_year :", formatted_year, "\n")
    cat("pbp_season     :", pbp_season, "\n")
    cat("season_token   :", season_token, "\n")
    cat("season_token2  :", season_token2, "\n")
  })
  
  # Clear env but keep essentials
  observeEvent(input$clear_env, {
    all_objs  <- ls(envir = .GlobalEnv)
    to_keep   <- c(essential_names, "essential_names")
    to_remove <- setdiff(all_objs, to_keep)
    
    if (length(to_remove) > 0) rm(list = to_remove, envir = .GlobalEnv)
    
    gc()
    showNotification("Global environment cleared (essentials preserved).", type = "warning", duration = 5)
    cat("\n=== Global Environment Cleared (kept essentials) ===\n")
    cat("Kept objects:", paste(to_keep, collapse = ", "), "\n")
  })
  
  # Run ALL
  observeEvent(input$run_all, {
    showNotification("Running ALL scripts in order...", type = "message")
    for (lbl in names(scripts)) run_script(lbl)
    showNotification("All scripts finished.", type = "message", duration = 5)
  })
  
  # AURELIUS tile button
  observeEvent(input$run_aurelius, {
    run_script("14. AURELIUS MC (In-Season)")
  })
  
  # AVITUS tile button
  observeEvent(input$run_avitus, {
    run_script("15. AVITUS LOGREG (In-Season)")
  })
  
  # Batch observers
  for (bl in names(batches)) {
    local({
      batch_label <- bl
      id <- batch_ids[[batch_label]]
      observeEvent(input[[id]], { run_batch(batch_label) })
    })
  }
  
  # Individual script observers
  for (lbl in names(scripts)) {
    local({
      lab <- lbl
      id  <- script_ids[[lab]]
      observeEvent(input[[id]], { run_script(lab) })
    })
  }
}

# ------------------------------------------------------------
# Launch
# ------------------------------------------------------------

runGadget(
  ui,
  server,
  viewer = dialogViewer("NBA Master Runner", width = 1200, height = 1000)
)

# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐
# ==== END: NBA Master Script UI Loader ====
# 🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐🌐