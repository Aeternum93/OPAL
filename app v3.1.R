library(shiny)
library(hoopR)
library(dplyr)
library(base64enc)

# ════════════════════════════════════════════════════════════════════════════════
# DATA LOADING
# ════════════════════════════════════════════════════════════════════════════════

player_data <- readRDS(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_MC_2025_2026.rds"
)

team_data <- readRDS(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_2025_2026.rds"
)
names(team_data) <- gsub("^T_", "", names(team_data))

schedule_data <- readRDS(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_2025_2026.rds"
)
schedule_data$game_date <- as.Date(schedule_data$game_date)

ROT_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/"
rot_5m  <- readRDS(paste0(ROT_PATH, "pm_nba_player_rotations_5M_2025_2026.rds"))
rot_10m <- readRDS(paste0(ROT_PATH, "pm_nba_player_rotations_10M_2025_2026.rds"))
rot_5m$game_date  <- as.Date(as.character(rot_5m$game_date))
rot_10m$GAME_DATE <- as.Date(as.character(rot_10m$GAME_DATE))

ODDS_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/"
player_odds <- readRDS(paste0(ODDS_PATH, "nba_player_odds_enrich_2025_2026.rds"))
player_odds$GAME_DATE <- dplyr::coalesce(
  as.Date(as.character(player_odds$GAME_DATE_CTR), format="%m/%d/%Y"),
  as.Date(as.character(player_odds$GAME_DATE_CTR), format="%Y-%m-%d")
)

INJ_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)/"
injury_data <- readRDS(paste0(INJ_PATH, "Injury_Database_2025_2026.rds"))
injury_data$date <- as.Date(as.character(injury_data$date))

WWW_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts/www/"
sb_logos <- list(
  DK   = base64enc::dataURI(file=paste0(WWW_PATH,"DK.png"),      mime="image/png"),
  FD   = base64enc::dataURI(file=paste0(WWW_PATH,"FD.jpg"),      mime="image/jpeg"),
  FNA  = base64enc::dataURI(file=paste0(WWW_PATH,"FAN.png"),     mime="image/png"),
  BMGM = base64enc::dataURI(file=paste0(WWW_PATH,"BETMGM.png"),  mime="image/png")
)

standings_data <- readRDS(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/8. Leauge Standings/nba_league standings.rds"
)

historical_odds <- readRDS(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/8. Historical Odds (Odds API)/nba_historical_odds_2025_2026.rds"
)
historical_odds$commence_date_est <- as.Date(as.character(historical_odds$commence_date_est))

# ── Play-by-Play data ──
PBP_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/7. MC PBP Data/"
pbp_data <- readRDS(paste0(PBP_PATH, "pm_nbapbp_2025_2026.rds"))
pbp_data$game_date <- as.Date(as.character(pbp_data$game_date))

# Classify play types for shot chart filtering
pbp_data$shot_category <- NA_character_
pbp_data$shot_category[pbp_data$shooting_play == TRUE & pbp_data$scoring_play == TRUE & pbp_data$score_value == 2] <- "2PT FG Made"
pbp_data$shot_category[pbp_data$shooting_play == TRUE & pbp_data$scoring_play == FALSE & pbp_data$points_attempted == 2] <- "2PT FG Missed"
pbp_data$shot_category[pbp_data$shooting_play == TRUE & pbp_data$scoring_play == TRUE & pbp_data$score_value == 3] <- "3PT Made"
pbp_data$shot_category[pbp_data$shooting_play == TRUE & pbp_data$scoring_play == FALSE & pbp_data$points_attempted == 3] <- "3PT Missed"


max(as.Date(player_odds$GAME_DATE), na.rm=TRUE)
max(as.Date(historical_odds$commence_date_est), na.rm=TRUE)
# ════════════════════════════════════════════════════════════════════════════════
# LOGO
# ════════════════════════════════════════════════════════════════════════════════

logo_b64 <- base64enc::dataURI(
  file = "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts/www/OPAL.png",
  mime = "image/png"
)


east_logo_b64 <- tryCatch(base64enc::dataURI(file = paste0(WWW_PATH, "Eastern_Conference_(NBA)_1993.svg"), mime = "image/svg+xml"), error = function(e) "")
west_logo_b64 <- tryCatch(base64enc::dataURI(file = paste0(WWW_PATH, "Western_Conference_(NBA)_1993.svg"), mime = "image/svg+xml"), error = function(e) "")
nba_logo_b64  <- tryCatch(base64enc::dataURI(file = paste0(WWW_PATH, "nba-logo-brandlogos.net_ipeky.png"), mime = "image/png"), error = function(e) "")


# ════════════════════════════════════════════════════════════════════════════════
# CSS
# ════════════════════════════════════════════════════════════════════════════════

OPAL_css <- "
@import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@300;400;500;600;700&family=Share+Tech+Mono&family=Inter:wght@300;400;500&display=swap');

:root {
  --bg-base:        #0a0a0b;
  --bg-sidebar:     #0e0e12;
  --bg-panel:       #0c0c0f;
  --bg-card:        #141418;
  --bg-card-hover:  #1c1c22;
  --bg-header:      #0e0e12;
  --border:         #222230;
  --border-light:   #2e2e3a;
  --text-primary:   #e8e4e0;
  --text-secondary: #8a8498;
  --text-muted:     #4a4858;
  --accent:         #d4a574;
  --accent-dim:     #a07a50;
  --accent-glow:    rgba(212, 165, 116, 0.15);
  --accent-lavender:#8a7ab8;
  --accent-coral:   #c48a6a;
  --positive:       #7aba7a;
  --negative:       #aa4a4a;
  --sidebar-width:  220px;
  --sidebar-col:    56px;
  --header-height:  48px;
}

* { margin: 0; padding: 0; box-sizing: border-box; }
html, body, .shiny-bound-input { height: 100%; }

body {
  font-family: 'Calibri', sans-serif;
  background: var(--bg-base);
  color: var(--text-primary);
  height: 100vh;
  overflow: hidden;
  display: flex;
  flex-direction: column;
}

#opal-header {
  height: var(--header-height);
  background: #1a1a1e;
  border-bottom: 1px solid var(--border);
  display: flex; align-items: center;
  padding: 0 16px; gap: 12px;
  z-index: 100; flex-shrink: 0;
}

opal-header .logo-text, #opal-header .search-placeholder, #opal-header .dark-toggle { color: #ccc; }
opal-header #hamburger span { background: #aaa; }
opal-header .header-btn svg { stroke: #aaa; }
opal-header .search-placeholder svg { stroke: #888; }

#hamburger {
  width: 28px; height: 28px;
  display: flex; flex-direction: column;
  justify-content: center; gap: 5px;
  cursor: pointer; padding: 4px;
  border-radius: 4px; border: none;
  background: transparent; transition: background .15s;
}
#hamburger:hover { background: var(--border); }
#hamburger span { display: block; height: 1.5px; background: var(--text-secondary); border-radius: 2px; }
.logo-area { display: flex; align-items: center; gap: 10px; }
.logo-text {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 28px;
  letter-spacing: .12em; text-transform: uppercase; color: var(--text-primary);
}
.header-right { margin-left: auto; display: flex; align-items: center; gap: 8px; }
.header-btn {
  width: 30px; height: 30px; background: transparent; border: none;
  cursor: pointer; border-radius: 6px;
  display: flex; align-items: center; justify-content: center; transition: background .15s;
}
.header-btn:hover { background: var(--border); }
.header-btn svg { width: 16px; height: 16px; stroke: var(--text-secondary); fill: none; stroke-width: 1.5; }
.search-placeholder {
  display: flex; align-items: center; gap: 8px;
  background: var(--bg-card); border: 1px solid var(--border);
  border-radius: 6px; padding: 0 10px; height: 30px;
  font-size: 12px; color: var(--text-muted);
  font-family: 'Calibri', sans-serif; cursor: text; min-width: 180px;
}
.search-placeholder svg { width: 13px; height: 13px; stroke: var(--text-muted); fill: none; stroke-width: 1.5; }

#app-body { display: flex; flex: 1; overflow: hidden; }

#opal-sidebar {
  width: var(--sidebar-width); background: var(--bg-sidebar);
  border-right: 1px solid var(--border);
  display: flex; flex-direction: column;
  transition: width .25s ease; overflow: hidden; flex-shrink: 0;
}
body.collapsed #opal-sidebar { width: var(--sidebar-col); }
.nav-section { padding: 8px 0; border-bottom: 1px solid var(--border); }
.nav-section:last-child { border-bottom: none; }
.nav-section-label {
  padding: 12px 12px 4px; font-size: 9px; font-weight: 500;
  letter-spacing: .14em; color: var(--text-muted);
  text-transform: uppercase; font-family: 'Rajdhani', sans-serif;
  white-space: nowrap; transition: opacity .2s;
}
body.collapsed .nav-section-label { opacity: 0; }
.nav-item {
  display: flex; align-items: center; gap: 10px;
  padding: 0 12px; height: 40px; cursor: pointer;
  transition: background .12s; white-space: nowrap;
  overflow: hidden; position: relative; border: none;
  background: transparent; width: 100%; text-align: left;
}
.nav-item:hover { background: var(--bg-card); }
.nav-item.active { background: var(--bg-card); }
.nav-item.active::before {
  content: ''; position: absolute; left: 0; top: 6px; bottom: 6px;
  width: 2px; background: var(--text-secondary); border-radius: 0 2px 2px 0;
}
.nav-icon {
  width: 28px; height: 28px; background: var(--bg-card);
  border: 1px solid var(--border); border-radius: 5px; flex-shrink: 0;
}
.nav-item.active .nav-icon { border-color: var(--border-light); background: var(--bg-card-hover); }
.nav-label {
  font-family: 'Calibri', sans-serif; font-size: 12.5px; font-weight: 400;
  color: var(--text-secondary); letter-spacing: .02em; transition: opacity .2s;
}
.nav-item.active .nav-label { color: var(--text-primary); }
body.collapsed .nav-label { opacity: 0; pointer-events: none; }

#opal-main { flex: 1; display: flex; flex-direction: column; overflow: hidden; background: var(--bg-panel); }
#main-header {
  height: 48px; border-bottom: 1px solid var(--border);
  display: flex; align-items: center; padding: 0 24px; gap: 16px; flex-shrink: 0;
}
#main-title {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 15px;
  letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary);
}
#main-subtitle {
  font-size: 11px; color: var(--text-muted);
  font-family: 'Share Tech Mono', monospace; letter-spacing: .05em;
}
#main-content { flex: 1; overflow-y: auto; padding: 28px; }
#main-content::-webkit-scrollbar { width: 4px; }
#main-content::-webkit-scrollbar-track { background: transparent; }
#main-content::-webkit-scrollbar-thumb { background: var(--border-light); border-radius: 2px; }

.home-state {
  display: flex; flex-direction: column; align-items: center; justify-content: center;
  height: 60vh; gap: 8px; opacity: .4;
}
.home-state svg { width: 40px; height: 40px; stroke: var(--text-muted); fill: none; stroke-width: 1; }
.home-state p {
  font-family: 'Rajdhani', sans-serif; font-size: 13px;
  letter-spacing: .12em; text-transform: uppercase; color: var(--text-muted);
}

.section-intro { margin-bottom: 24px; }
.section-intro h2 {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 22px;
  letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary); margin-bottom: 4px;
}
.section-intro p { font-size: 12px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; letter-spacing: .04em; }

.cards-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 12px; }
.opal-card {
  background: var(--bg-card); border: 1px solid var(--border);
  border-radius: 8px; padding: 20px 18px; cursor: pointer;
  transition: background .15s, border-color .15s, transform .12s;
  display: flex; flex-direction: column; gap: 12px; position: relative; overflow: hidden;
}
.opal-card::after {
  content: ''; position: absolute; inset: 0;
  background: linear-gradient(135deg, rgba(255,255,255,.02) 0%, transparent 60%); pointer-events: none;
}
.opal-card:hover { background: var(--bg-card-hover); border-color: var(--border-light); transform: translateY(-1px); }
.card-icon { width: 36px; height: 36px; background: var(--bg-base); border: 1px solid var(--border); border-radius: 6px; }
.card-label {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 13px;
  letter-spacing: .08em; text-transform: uppercase; color: var(--text-primary); line-height: 1.3;
}
.card-tag {
  display: inline-block; font-size: 9px; font-family: 'Share Tech Mono', monospace;
  letter-spacing: .1em; color: var(--text-muted); background: var(--bg-base);
  border: 1px solid var(--border); border-radius: 3px; padding: 2px 6px;
  text-transform: uppercase; width: fit-content;
}

.standings-placeholder { display: flex; flex-direction: column; gap: 1px; }
.standings-row {
  display: flex; align-items: center; padding: 10px 14px;
  background: var(--bg-card); border: 1px solid var(--border);
  border-radius: 4px; gap: 16px; font-size: 12px;
  color: var(--text-secondary); font-family: 'Share Tech Mono', monospace;
}
.standings-num { color: var(--text-muted); width: 20px; }
.standings-name { flex: 1; color: var(--text-primary); font-family: 'Rajdhani', sans-serif; font-weight: 600; letter-spacing: .06em; text-transform: uppercase; }
.standings-val { color: var(--text-muted); }

.fade-in { animation: fadeIn .2s ease; }
@keyframes fadeIn { from { opacity: 0; transform: translateY(4px); } to { opacity: 1; transform: translateY(0); } }

.selectize-input {
  background: var(--bg-card) !important; border: 1px solid var(--border) !important;
  border-radius: 6px !important; color: var(--text-primary) !important;
  font-family: 'Calibri', sans-serif !important; font-size: 13px !important; box-shadow: none !important;
}
.selectize-input.focus { border-color: var(--border-light) !important; }
.selectize-dropdown { background: var(--bg-card) !important; border: 1px solid var(--border-light) !important; border-radius: 6px !important; }
.selectize-dropdown-content .option { color: var(--text-secondary) !important; font-family: 'Calibri', sans-serif !important; font-size: 13px !important; padding: 8px 12px !important; }
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active { background: var(--bg-card-hover) !important; color: var(--text-primary) !important; }

/* ── PLAYER STATS LOOKUP ── */
.player-search-bar { display: flex; align-items: center; gap: 12px; margin-bottom: 24px; }
.player-search-bar .selectize-control { flex: 1; max-width: 380px; }
.player-profile { background: var(--bg-card); border: 1px solid var(--border); border-radius: 10px; overflow: hidden; }
.player-hero {
  display: flex; align-items: flex-end;
  background: linear-gradient(to right, #0d0d10 60%, #161619);
  border-bottom: 1px solid var(--border); padding: 0 28px 0 0; min-height: 140px;
}
.player-headshot { width: 130px; height: 130px; object-fit: cover; object-position: top; flex-shrink: 0; border-right: 1px solid var(--border); }
.player-headshot-placeholder {
  width: 130px; height: 130px; flex-shrink: 0; background: var(--bg-base);
  border-right: 1px solid var(--border); display: flex; align-items: center; justify-content: center;
}
.player-headshot-placeholder svg { width: 48px; height: 48px; stroke: var(--text-muted); fill: none; }
.player-identity { display: flex; flex-direction: column; justify-content: center; padding: 20px 24px; gap: 4px; flex: 1; }
.player-name-line { display: flex; align-items: baseline; gap: 10px; }
.player-firstname { font-family: 'Rajdhani', sans-serif; font-weight: 300; font-size: 26px; color: var(--text-secondary); letter-spacing: .06em; text-transform: uppercase; }
.player-lastname  { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 30px; color: var(--text-primary); letter-spacing: .06em; text-transform: uppercase; }
.player-meta { font-size: 11px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; letter-spacing: .08em; }
.player-status-dot { display: inline-block; width: 7px; height: 7px; border-radius: 50%; background: #3a8a3a; margin-right: 5px; }
.season-stats-banner { display: flex; align-items: center; padding: 0 28px 0 16px; }
.season-stats-banner .stat-block { display: flex; flex-direction: column; align-items: center; padding: 16px 24px; gap: 4px; border-right: 1px solid var(--border); }
.season-stats-banner .stat-block:last-child { border-right: none; }
.stat-val { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 26px; color: var(--text-primary); }
.stat-lbl { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; }
.season-label { margin-left: auto; font-size: 10px; font-family: 'Share Tech Mono', monospace; color: var(--text-muted); letter-spacing: .08em; background: var(--bg-base); border: 1px solid var(--border); border-radius: 4px; padding: 4px 10px; }
.bio-strip { display: flex; gap: 32px; padding: 14px 28px; border-bottom: 1px solid var(--border); background: var(--bg-base); }
.bio-item { display: flex; flex-direction: column; gap: 2px; }
.bio-lbl { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; }
.bio-val { font-size: 12px; font-family: 'Calibri', sans-serif; color: var(--text-secondary); }
.player-tabs { display: flex; border-bottom: 1px solid var(--border); padding: 0 16px; background: var(--bg-card); }
.player-tab {
  padding: 12px 16px; font-size: 12px; font-family: 'Calibri', sans-serif; font-weight: 500;
  color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent;
  transition: color .15s, border-color .15s; border: none; background: transparent; letter-spacing: .03em;
}
.player-tab:hover { color: var(--text-secondary); }
.player-tab.active { color: var(--text-primary); border-bottom-color: var(--text-secondary); }
.tab-content { padding: 20px 0 0; }
.gamelog-wrap { overflow-x: auto; }
.gamelog-wrap::-webkit-scrollbar { height: 4px; }
.gamelog-wrap::-webkit-scrollbar-thumb { background: var(--border-light); border-radius: 2px; }
table.gamelog { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 11px; }
table.gamelog thead th { padding: 8px 10px; text-align: right; color: var(--text-muted); letter-spacing: .08em; text-transform: uppercase; border-bottom: 1px solid var(--border); white-space: nowrap; font-weight: 400; }
table.gamelog thead th:first-child, table.gamelog thead th:nth-child(2), table.gamelog thead th:nth-child(3) { text-align: left; }
table.gamelog tbody tr { border-bottom: 1px solid var(--border); transition: background .1s; }
table.gamelog tbody tr:hover { background: var(--bg-card-hover); }
table.gamelog tbody td { padding: 9px 10px; color: var(--text-secondary); text-align: right; white-space: nowrap; }
table.gamelog tbody td:first-child, table.gamelog tbody td:nth-child(2), table.gamelog tbody td:nth-child(3) { text-align: left; }
.team-cell { display: flex; align-items: center; gap: 7px; }
.team-logo-img { width: 20px; height: 20px; object-fit: contain; }
.team-abv { font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 12px; color: var(--text-primary); letter-spacing: .06em; }
.winner-dot { display: inline-block; width: 6px; height: 6px; border-radius: 50%; background: #3a8a3a; margin-left: 4px; }

/* ── TEAM STATS LOOKUP ── */
.team-lookup-selectors { display: flex; gap: 12px; margin-bottom: 24px; align-items: flex-end; flex-wrap: wrap; }
.team-lookup-selectors .selectize-control { min-width: 200px; }
.team-lookup-selectors label { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; display: block; margin-bottom: 6px; }
.game-scoreboard { background: var(--bg-card); border: 1px solid var(--border); border-radius: 10px; overflow: hidden; }
.scoreboard-hero {
  display: flex; align-items: center; justify-content: center; padding: 20px 28px;
  background: linear-gradient(to right, #0d0d10, #161619, #0d0d10);
  border-bottom: 1px solid var(--border);
}
.score-team { display: flex; flex-direction: column; align-items: center; gap: 8px; flex: 1; }
.score-team-logo { width: 60px; height: 60px; object-fit: contain; }
.score-team-logo-placeholder { width: 60px; height: 60px; background: var(--bg-base); border: 1px solid var(--border); border-radius: 50%; }
.score-team-name { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 16px; letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary); }
.score-team-record { font-size: 10px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; }
.score-center { display: flex; flex-direction: column; align-items: center; gap: 8px; padding: 0 32px; }
.score-numbers { display: flex; align-items: center; gap: 16px; }
.score-num { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 48px; color: var(--text-primary); line-height: 1; }
.score-num.loser { color: var(--text-muted); }
.score-sep { font-size: 24px; color: var(--text-muted); font-family: 'Rajdhani', sans-serif; }
.score-status { font-size: 10px; font-family: 'Share Tech Mono', monospace; color: var(--text-muted); letter-spacing: .1em; text-transform: uppercase; }
.quarter-table-wrap { padding: 0; border-bottom: 1px solid var(--border); overflow-x: auto; }
table.quarter-table { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 11px; }
table.quarter-table th { padding: 8px 16px; text-align: center; color: var(--text-muted); letter-spacing: .08em; border-bottom: 1px solid var(--border); font-weight: 400; }
table.quarter-table th:first-child { text-align: left; }
table.quarter-table td { padding: 10px 16px; text-align: center; color: var(--text-secondary); }
table.quarter-table td:first-child { text-align: left; }
.qt-team-cell { display: flex; align-items: center; gap: 8px; }
.qt-total { font-weight: 700; color: var(--text-primary); }
table.quarter-table tbody tr:last-child { border-top: 1px solid var(--border); }
.game-tabs { display: flex; border-bottom: 1px solid var(--border); padding: 0 16px; background: var(--bg-card); }
.game-tab {
  padding: 12px 16px; font-size: 12px; font-family: 'Calibri', sans-serif; font-weight: 500;
  color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent;
  transition: color .15s, border-color .15s; border: none; background: transparent; letter-spacing: .03em;
}
.game-tab:hover { color: var(--text-secondary); }
.game-tab.active { color: var(--text-primary); border-bottom-color: var(--text-secondary); }
.game-tab-content { padding: 20px 0 0; }
.boxscore-wrap { overflow-x: auto; margin-bottom: 24px; }
.boxscore-team-label { display: flex; align-items: center; gap: 10px; padding: 10px 0 8px; font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 14px; letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary); }
table.boxscore { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 11px; }
table.boxscore thead th { padding: 7px 10px; text-align: right; color: var(--text-muted); letter-spacing: .06em; text-transform: uppercase; border-bottom: 1px solid var(--border); white-space: nowrap; font-weight: 400; font-size: 10px; }
table.boxscore thead th:first-child { text-align: left; min-width: 140px; }
table.boxscore tbody tr { border-bottom: 1px solid var(--border); transition: background .1s; }
table.boxscore tbody tr:hover { background: var(--bg-card-hover); }
table.boxscore tbody td { padding: 8px 10px; color: var(--text-secondary); text-align: right; white-space: nowrap; }
table.boxscore tbody td:first-child { text-align: left; }
table.boxscore tbody tr.totals-row td { color: var(--text-primary); border-top: 1px solid var(--border-light); font-weight: 600; }
.player-link { color: var(--text-primary); cursor: pointer; font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 12px; letter-spacing: .05em; text-transform: uppercase; }
.player-link:hover { color: #a0a0c0; text-decoration: underline; }
.game-leaders { margin-top: 24px; }
.game-leaders-title { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 14px; letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary); margin-bottom: 14px; }
.leaders-grid { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 12px; }
.leader-card { background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px; padding: 14px; display: flex; flex-direction: column; gap: 10px; }
.leader-cat { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; }
.leader-row { display: flex; align-items: center; gap: 10px; }
.leader-headshot { width: 36px; height: 36px; border-radius: 50%; object-fit: cover; object-position: top; border: 1px solid var(--border); }
.leader-headshot-ph { width: 36px; height: 36px; border-radius: 50%; background: var(--bg-base); border: 1px solid var(--border); flex-shrink: 0; }
.leader-info { display: flex; flex-direction: column; gap: 2px; flex: 1; }
.leader-name { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 13px; letter-spacing: .06em; text-transform: uppercase; color: var(--text-primary); cursor: pointer; }
.leader-name:hover { color: #a0a0c0; }
.leader-stat { font-size: 10px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; }
.leader-val { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 22px; color: var(--text-primary); }
.leaders-section-label { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .12em; color: var(--text-muted); text-transform: uppercase; margin-bottom: 4px; margin-top: 8px; }

/* ── SCHEDULE VIEWER ── */
.schedule-layout { display: flex; gap: 0; height: calc(100vh - 160px); position: relative; }
.calendar-wrap { flex: 1; min-width: 0; overflow: hidden; display: flex; flex-direction: column; }
.calendar-nav { display: flex; align-items: center; gap: 12px; margin-bottom: 16px; }
.cal-nav-btn {
  width: 28px; height: 28px; background: var(--bg-card); border: 1px solid var(--border);
  border-radius: 5px; cursor: pointer; color: var(--text-secondary);
  display: flex; align-items: center; justify-content: center; font-size: 14px;
  transition: background .12s, border-color .12s;
}
.cal-nav-btn:hover { background: var(--bg-card-hover); border-color: var(--border-light); }
.cal-month-label {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 16px;
  letter-spacing: .1em; text-transform: uppercase; color: var(--text-primary);
}
.cal-today-btn {
  margin-left: auto; font-size: 10px; font-family: 'Share Tech Mono', monospace;
  letter-spacing: .08em; color: var(--text-muted); background: var(--bg-card);
  border: 1px solid var(--border); border-radius: 4px; padding: 4px 10px; cursor: pointer;
  transition: background .12s;
}
.cal-today-btn:hover { background: var(--bg-card-hover); color: var(--text-secondary); }
.cal-grid { display: grid; grid-template-columns: repeat(7, 1fr); gap: 1px; background: var(--border); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; flex: 1; }
.cal-dow { background: var(--bg-header); padding: 8px 10px; font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; text-align: center; }
.cal-cell { background: var(--bg-panel); padding: 6px 8px; min-height: 90px; display: flex; flex-direction: column; gap: 3px; overflow: hidden; position: relative; }
.cal-cell.other-month { background: var(--bg-base); opacity: .5; }
.cal-cell.today { background: #0e0e18; }
.cal-date-num {
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 13px;
  color: var(--text-muted); cursor: pointer; display: inline-block;
  width: 22px; height: 22px; line-height: 22px; text-align: center;
  border-radius: 4px; transition: background .12s, color .12s; flex-shrink: 0;
}
.cal-date-num:hover { background: var(--bg-card-hover); color: var(--text-primary); }
.cal-cell.today .cal-date-num { background: var(--border-light); color: var(--text-primary); }
.cal-game {
  display: flex; align-items: center; gap: 4px; padding: 2px 5px;
  background: var(--bg-card); border: 1px solid var(--border); border-radius: 3px;
  cursor: pointer; transition: background .1s, border-color .1s; overflow: hidden;
  font-size: 10px; white-space: nowrap;
}
.cal-game:hover { background: var(--bg-card-hover); border-color: var(--border-light); }
.cal-game img { width: 14px; height: 14px; object-fit: contain; flex-shrink: 0; }
.cal-game-teams { font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 11px; color: var(--text-primary); letter-spacing: .04em; overflow: hidden; text-overflow: ellipsis; }
.cal-game-sym { color: var(--text-muted); font-size: 9px; font-family: 'Share Tech Mono', monospace; flex-shrink: 0; }
.cal-more { font-size: 9px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; padding: 1px 4px; cursor: pointer; }
.schedule-pane {
  width: 0; overflow: hidden; transition: width .25s ease;
  border-left: 0px solid var(--border); background: var(--bg-sidebar);
  display: flex; flex-direction: column;
}
.schedule-pane.open { width: 340px; border-left-width: 1px; }
.pane-header { padding: 16px 16px 12px; border-bottom: 1px solid var(--border); display: flex; align-items: center; justify-content: space-between; flex-shrink: 0; }
.pane-date-label { font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 14px; letter-spacing: .08em; text-transform: uppercase; color: var(--text-primary); }
.pane-close { background: transparent; border: none; cursor: pointer; color: var(--text-muted); font-size: 16px; padding: 2px 6px; border-radius: 4px; transition: background .12s; }
.pane-close:hover { background: var(--border); color: var(--text-primary); }
.pane-games { flex: 1; overflow-y: auto; padding: 12px; display: flex; flex-direction: column; gap: 8px; }
.pane-games::-webkit-scrollbar { width: 3px; }
.pane-games::-webkit-scrollbar-thumb { background: var(--border-light); border-radius: 2px; }
.pane-game-card {
  background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px;
  padding: 12px; cursor: pointer; transition: background .12s, border-color .12s;
  display: flex; flex-direction: column; gap: 8px;
}
.pane-game-card:hover { background: var(--bg-card-hover); border-color: var(--border-light); }
.pane-matchup { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
.pane-team { display: flex; align-items: center; gap: 6px; flex: 1; }
.pane-team img { width: 28px; height: 28px; object-fit: contain; }
.pane-team-name { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 14px; letter-spacing: .06em; color: var(--text-primary); }
.pane-team.loser .pane-team-name { color: var(--text-muted); }
.pane-score { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 20px; color: var(--text-primary); text-align: center; min-width: 50px; }
.pane-sym { font-size: 10px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; text-align: center; }
.pane-meta { font-size: 9px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; letter-spacing: .08em; }
.pane-winner-dot { display: inline-block; width: 6px; height: 6px; border-radius: 50%; background: #3a8a3a; margin-left: 4px; vertical-align: middle; }

/* ── BETTING VIEW ── */
.bv-layout { display: flex; flex-direction: column; gap: 20px; }
.bv-top-bar { display: flex; align-items: center; gap: 12px; margin-bottom: 4px; flex-wrap: wrap; }
.bv-bankroll-block { display: flex; align-items: center; gap: 0; background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; }
.bv-bankroll-btn { width: 34px; height: 44px; background: transparent; border: none; cursor: pointer; color: var(--text-muted); font-size: 18px; display: flex; align-items: center; justify-content: center; transition: background .12s, color .12s; }
.bv-bankroll-btn:hover { background: var(--bg-card-hover); color: var(--text-primary); }
.bv-bankroll-center { display: flex; flex-direction: column; align-items: center; padding: 6px 20px; border-left: 1px solid var(--border); border-right: 1px solid var(--border); }
.bv-bankroll-lbl { font-size: 8px; font-family: 'Share Tech Mono', monospace; letter-spacing: .12em; color: var(--text-muted); text-transform: uppercase; }
.bv-bankroll-val { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 22px; color: var(--text-primary); line-height: 1.1; }
.bv-action-btn { height: 44px; padding: 0 16px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px; cursor: pointer; font-size: 10px; font-family: 'Share Tech Mono', monospace; letter-spacing: .08em; color: var(--text-muted); text-transform: uppercase; transition: background .12s, border-color .12s, color .12s; }
.bv-action-btn:hover { background: var(--bg-card-hover); border-color: var(--border-light); color: var(--text-primary); }
.bv-action-btn.primary { border-color: #2a3a2a; color: #5a9a5a; }
.bv-action-btn.primary:hover { background: #0e180e; border-color: #3a6a3a; color: #7aba7a; }
.bv-stats-row { display: grid; grid-template-columns: repeat(auto-fill, minmax(140px, 1fr)); gap: 10px; }
.bv-stat-card { background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px; padding: 14px 16px; display: flex; flex-direction: column; gap: 4px; }
.bv-stat-lbl { font-size: 8px; font-family: 'Share Tech Mono', monospace; letter-spacing: .12em; color: var(--text-muted); text-transform: uppercase; }
.bv-stat-val { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 20px; color: var(--text-primary); }
.bv-stat-val.positive { color: #5a9a5a; }
.bv-stat-val.negative { color: #9a4a4a; }
.bv-charts-row { display: grid; grid-template-columns: 2fr 1fr; gap: 16px; }
.bv-chart-card { background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px; padding: 16px; }
.bv-chart-title { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .12em; color: var(--text-muted); text-transform: uppercase; margin-bottom: 12px; }
.bv-chart-area { width: 100%; height: 160px; position: relative; }
.bv-bottom-row { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }
.bv-section-title { font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 13px; letter-spacing: .08em; text-transform: uppercase; color: var(--text-primary); margin-bottom: 12px; }
.bv-bet-table-wrap { overflow-x: auto; }
table.bv-bet-table { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 11px; }
table.bv-bet-table thead th { padding: 7px 10px; text-align: left; color: var(--text-muted); letter-spacing: .08em; text-transform: uppercase; border-bottom: 1px solid var(--border); font-weight: 400; font-size: 9px; white-space: nowrap; }
table.bv-bet-table tbody tr { border-bottom: 1px solid var(--border); transition: background .1s; }
table.bv-bet-table tbody tr:hover { background: var(--bg-card-hover); }
table.bv-bet-table tbody td { padding: 8px 10px; color: var(--text-secondary); white-space: nowrap; }
.bet-result-W { color: #5a9a5a; font-weight: 600; }
.bet-result-L { color: #9a4a4a; font-weight: 600; }
.bet-result-P { color: var(--text-muted); }
.bet-result-pending { color: #7a7a3a; }
/* P&L Calendar */
.bv-cal-grid { display: grid; grid-template-columns: repeat(7, 1fr); gap: 2px; }
.bv-cal-dow { font-size: 8px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; text-align: center; padding: 4px 0; }
.bv-cal-cell { background: var(--bg-base); border-radius: 4px; padding: 6px 8px; min-height: 54px; display: flex; flex-direction: column; gap: 2px; }
.bv-cal-cell.has-bets { background: var(--bg-card); border: 1px solid var(--border); }
.bv-cal-cell.win-day { background: #0a180a; border: 1px solid #1a3a1a; }
.bv-cal-cell.loss-day { background: #180a0a; border: 1px solid #3a1a1a; }
.bv-cal-cell.today-day { border-color: var(--border-light) !important; }
.bv-cal-date { font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 12px; color: var(--text-muted); }
.bv-cal-pnl { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 13px; }
.bv-cal-pnl.pos { color: #5a9a5a; }
.bv-cal-pnl.neg { color: #9a4a4a; }
.bv-cal-pnl.zero { color: var(--text-muted); }
/* Bet entry modal */
.bv-modal-overlay { position: fixed; inset: 0; background: rgba(0,0,0,.7); z-index: 1000; display: flex; align-items: center; justify-content: center; }
.bv-modal { background: var(--bg-sidebar); border: 1px solid var(--border-light); border-radius: 12px; padding: 24px; width: 480px; max-width: 95vw; display: flex; flex-direction: column; gap: 16px; }
.bv-modal-title { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 16px; letter-spacing: .08em; text-transform: uppercase; color: var(--text-primary); }
.bv-form-row { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
.bv-form-group { display: flex; flex-direction: column; gap: 6px; }
.bv-form-group.full { grid-column: 1 / -1; }
.bv-form-label { font-size: 8px; font-family: 'Share Tech Mono', monospace; letter-spacing: .12em; color: var(--text-muted); text-transform: uppercase; }
.bv-form-input { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Calibri', sans-serif; font-size: 13px; padding: 8px 10px; width: 100%; }
.bv-form-input:focus { outline: none; border-color: var(--border-light); }
.bv-form-select { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Calibri', sans-serif; font-size: 13px; padding: 8px 10px; width: 100%; }
.bv-kelly-hint { font-size: 10px; font-family: 'Share Tech Mono', monospace; color: #5a9a5a; letter-spacing: .06em; }
.bv-modal-btns { display: flex; gap: 10px; justify-content: flex-end; }
.bv-modal-cancel { padding: 8px 18px; background: transparent; border: 1px solid var(--border); border-radius: 6px; cursor: pointer; color: var(--text-muted); font-family: 'Calibri', sans-serif; font-size: 13px; transition: background .12s; }
.bv-modal-cancel:hover { background: var(--bg-card-hover); }
.bv-modal-submit { padding: 8px 18px; background: #0e1a0e; border: 1px solid #2a4a2a; border-radius: 6px; cursor: pointer; color: #7aba7a; font-family: 'Calibri', sans-serif; font-size: 13px; font-weight: 500; transition: background .12s; }
.bv-modal-submit:hover { background: #162816; }
.bv-bankroll-input { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Rajdhani', sans-serif; font-size: 18px; font-weight: 700; padding: 6px 12px; width: 120px; text-align: center; }

/* ── PLAYER ROTATIONS ── */
.pr-layout { display: flex; flex-direction: column; height: calc(100vh - 120px); overflow: hidden; }
.pr-subnav { display: flex; gap: 0; border-bottom: 1px solid var(--border); margin-bottom: 0; flex-shrink: 0; }
.pr-tab { padding: 10px 18px; font-size: 12px; font-family: 'Calibri', sans-serif; font-weight: 500; color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent; transition: color .15s, border-color .15s; border: none; background: transparent; letter-spacing: .03em; }
.pr-tab:hover { color: var(--text-secondary); }
.pr-tab.active { color: var(--text-primary); border-bottom-color: var(--text-secondary); }
.pr-filters { padding: 12px 0; display: flex; flex-direction: column; gap: 10px; flex-shrink: 0; }
.pr-filter-row { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
.pr-filter-btn { display: flex; align-items: center; gap: 6px; padding: 6px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; cursor: pointer; font-size: 11px; font-family: 'Calibri', sans-serif; color: var(--text-secondary); transition: background .12s, border-color .12s; white-space: nowrap; }
.pr-filter-btn:hover { background: var(--bg-card-hover); border-color: var(--border-light); }
.pr-filter-btn.active { background: var(--bg-card-hover); border-color: var(--border-light); color: var(--text-primary); }
.pr-filter-btn img { width: 20px; height: 20px; object-fit: contain; }
.pr-man-btn { padding: 6px 14px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; cursor: pointer; font-size: 11px; font-family: 'Share Tech Mono', monospace; letter-spacing: .06em; color: var(--text-muted); transition: background .12s; }
.pr-man-btn:hover { background: var(--bg-card-hover); color: var(--text-secondary); }
.pr-man-btn.active { background: #0e1a0e; border-color: #2a4a2a; color: #7aba7a; }
.pr-luck-row { display: flex; gap: 20px; align-items: center; flex-wrap: wrap; }
.pr-luck-block { display: flex; flex-direction: column; gap: 4px; flex: 1; min-width: 200px; }
.pr-luck-label { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; display: flex; justify-content: space-between; }
.pr-luck-slider { width: 100%; accent-color: var(--border-light); }
.pr-content { flex: 1; overflow: hidden; display: flex; flex-direction: column; min-height: 0; }
.pr-table-wrap { flex: 1; overflow: auto; }
table.pr-table { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 14px; }
table.pr-table thead th { position: sticky; top: 0; padding: 8px 10px; text-align: right; color: var(--text-muted); letter-spacing: .07em; text-transform: uppercase; border-bottom: 1px solid var(--border); font-weight: 400; font-size: 9px; white-space: nowrap; background: var(--bg-panel); z-index: 2; }
table.pr-table thead th:first-child { text-align: left; min-width: 180px; position: sticky; left: 0; z-index: 3; background: var(--bg-panel); }
table.pr-table thead th.col-group-off { border-bottom-color: #2a4a2a; color: #5a9a5a; }
table.pr-table thead th.col-group-def { border-bottom-color: #4a2a2a; color: #9a5a5a; }
table.pr-table tbody tr { border-bottom: 1px solid var(--border); transition: background .1s; }
table.pr-table tbody tr:hover { background: var(--bg-card-hover); }
table.pr-table tbody td { padding: 8px 10px; color: var(--text-secondary); text-align: right; white-space: nowrap; }
table.pr-table tbody td:first-child { text-align: left; position: sticky; left: 0; background: var(--bg-panel); z-index: 1; }
table.pr-table tbody tr:hover td:first-child { background: var(--bg-card-hover); }
.pr-lineup-cell { display: flex; align-items: center; gap: 2px; }
.pr-hs { width: 65px; height: 65px; border-radius: 50%; object-fit: cover; object-position: top; border: 1px solid var(--border); flex-shrink: 0; }
.pr-hs-ph { width: 65px; height: 65px; border-radius: 50%; background: var(--bg-base); border: 1px solid var(--border); flex-shrink: 0; }
/* Team select modal */
.pr-team-modal-overlay { position: fixed; inset: 0; background: rgba(0,0,0,.75); z-index: 1000; display: flex; align-items: center; justify-content: center; }
.pr-team-modal { background: #1a1a20; border: 1px solid var(--border-light); border-radius: 14px; padding: 28px; width: 560px; max-width: 95vw; max-height: 85vh; overflow-y: auto; }
.pr-team-modal-header { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 20px; }
.pr-team-modal-title { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 18px; letter-spacing: .08em; text-transform: uppercase; color: var(--text-primary); }
.pr-team-modal-sub { font-size: 11px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; margin-top: 2px; }
.pr-team-modal-close { width: 28px; height: 28px; background: transparent; border: none; cursor: pointer; color: var(--text-muted); font-size: 18px; display: flex; align-items: center; justify-content: center; border-radius: 4px; }
.pr-team-modal-close:hover { background: var(--bg-card-hover); color: var(--text-primary); }
.pr-team-grid { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px; }
.pr-team-item { display: flex; flex-direction: column; align-items: center; gap: 6px; padding: 10px 6px; border-radius: 8px; cursor: pointer; transition: background .12s; border: 1px solid transparent; }
.pr-team-item:hover { background: var(--bg-card-hover); border-color: var(--border); }
.pr-team-item.selected { background: #0e180e; border-color: #2a4a2a; }
.pr-team-item img { width: 40px; height: 40px; object-fit: contain; }
.pr-team-item span { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 11px; letter-spacing: .08em; color: var(--text-secondary); }
/* Bottom tray — global fixed, injected into body */
#pr-global-tray { position: fixed; bottom: 0; left: 220px; right: 0; z-index: 999; transition: left .2s; }
body.collapsed #pr-global-tray { left: 56px; }
.pr-tray-outer { background: #16161c; border-top: 1px solid var(--border); }
.pr-tray-toggle-row { display: flex; justify-content: center; margin-top: -18px; margin-bottom: 0; }
.pr-tray-toggle { width: 36px; height: 36px; background: #c8a020; border: none; border-radius: 50%; cursor: pointer; font-size: 15px; font-weight: 900; color: #0a0a0b; box-shadow: 0 2px 10px rgba(0,0,0,.6); line-height: 1; display: flex; align-items: center; justify-content: center; }
.pr-tray-toggle:hover { background: #d4b030; }
.pr-tray-bar { display: flex; align-items: center; padding: 4px 16px; gap: 12px; min-height: 28px; }
.pr-tray-info { font-size: 10px; font-family: 'Share Tech Mono', monospace; color: #666; letter-spacing: .06em; }
.pr-tray-esc { font-size: 10px; font-family: 'Share Tech Mono', monospace; color: #666; background: #1e1e26; border: 1px solid #333; border-radius: 4px; padding: 2px 8px; cursor: pointer; }
.pr-tray-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(82px, 1fr)); gap: 4px 8px; padding: 6px 16px 10px; max-height: 160px; overflow-y: auto; }
.pr-tray-grid::-webkit-scrollbar { width: 3px; }
.pr-tray-grid::-webkit-scrollbar-thumb { background: #444; border-radius: 2px; }
.pr-tray-player { display: flex; flex-direction: column; align-items: center; gap: 3px; cursor: pointer; padding: 4px; border-radius: 6px; border: 1px solid transparent; }
.pr-tray-player:hover { background: #1e1e26; border-color: #333; }
.pr-tray-player.sel { background: #0e180e; border-color: #2a4a2a; }
.pr-tray-hs { width: 65px; height: 65px; border-radius: 50%; object-fit: cover; object-position: top; border: 1px solid #333; display: block; }
.pr-tray-hs-ph { width: 65px; height: 65px; border-radius: 50%; background: #111; border: 1px solid #333; display: block; }
.pr-tray-name { font-size: 9px; font-family: Inter, sans-serif; color: #888; text-align: center; width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.pr-opp-select { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-secondary); font-family: 'Calibri', sans-serif; font-size: 11px; padding: 5px 8px; }
.pr-placeholder { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 200px; gap: 8px; opacity: .35; }
.pr-placeholder p { font-family: 'Rajdhani', sans-serif; font-size: 13px; letter-spacing: .1em; text-transform: uppercase; color: var(--text-muted); }

/* ── INJURY DASHBOARD ── */
.inj-home-wrap { padding: 16px; overflow-y: auto; height: calc(100vh - 140px); }
.inj-team-block { margin-bottom: 28px; }
.inj-team-header { display: flex; align-items: center; gap: 6px; font-family: 'Rajdhani', sans-serif; font-size: 28px; font-weight: 700; letter-spacing: .06em; color: var(--text-primary); padding: 0 0 8px 0; border-bottom: 1px solid var(--border); margin-bottom: 6px; }
.inj-col-header { display: flex; align-items: center; padding: 5px 8px; font-family: 'Share Tech Mono', monospace; font-size: 13px; letter-spacing: .1em; color: var(--text-muted); border-bottom: 1px solid var(--border); }
.inj-player-row { display: flex; align-items: center; padding: 8px 8px; border-bottom: 1px solid rgba(255,255,255,.04); transition: background .1s; }
.inj-player-row:hover { background: var(--bg-card); }
.inj-player-name { font-family: 'Calibri', sans-serif; font-size: 16px; font-weight: 500; }

/* ── PLAYER & TEAM ODDS ── */
.po-layout { display: flex; flex-direction: column; height: calc(100vh - 96px); overflow: hidden; }
.po-subnav { display: flex; gap: 0; border-bottom: 1px solid var(--border); flex-shrink: 0; }
.po-tab { padding: 10px 18px; font-size: 12px; font-family: 'Calibri', sans-serif; font-weight: 500; color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent; border: none; background: transparent; letter-spacing: .03em; transition: color .15s; }
.po-tab:hover { color: var(--text-secondary); }
.po-tab.active { color: var(--text-primary); border-bottom: 2px solid var(--text-secondary); }
.po-filters { display: flex; align-items: center; gap: 8px; padding: 12px 0; flex-shrink: 0; flex-wrap: wrap; }
.po-filter-btn { display: flex; align-items: center; gap: 6px; padding: 6px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; cursor: pointer; font-size: 11px; font-family: 'Calibri', sans-serif; color: var(--text-secondary); transition: background .12s; white-space: nowrap; }
.po-filter-btn:hover { background: var(--bg-card-hover); border-color: var(--border-light); }
.po-filter-btn.active { background: var(--bg-card-hover); border-color: var(--border-light); color: var(--text-primary); }
.po-filter-btn img { width: 20px; height: 20px; object-fit: contain; }
.po-stat-btn { padding: 5px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 20px; cursor: pointer; font-size: 11px; font-family: 'Share Tech Mono', monospace; letter-spacing: .05em; color: var(--text-muted); transition: background .12s, color .12s; white-space: nowrap; }
.po-stat-btn:hover { background: var(--bg-card-hover); color: var(--text-secondary); }
.po-stat-btn.active { background: #0e180e; border-color: #2a4a2a; color: #7aba7a; }
.po-nba-badge { display: flex; align-items: center; gap: 6px; padding: 5px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; font-size: 11px; font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: .1em; color: var(--text-secondary); }
.po-content { flex: 1; overflow-y: auto; }
/* Player odds table */
.po-table-wrap { overflow-x: auto; }
table.po-table { width: 100%; border-collapse: collapse; font-family: 'Share Tech Mono', monospace; font-size: 11px; }
table.po-table thead tr th { position: sticky; top: 0; background: var(--bg-panel); padding: 8px 10px; text-align: left; color: var(--text-muted); font-size: 9px; letter-spacing: .08em; text-transform: uppercase; border-bottom: 1px solid var(--border); font-weight: 400; white-space: nowrap; z-index: 2; }
table.po-table thead tr.th-group th { font-size: 10px; color: var(--text-secondary); border-bottom: none; padding-bottom: 2px; }
table.po-table tbody tr { border-bottom: 1px solid var(--border); transition: background .1s; }
table.po-table tbody tr:hover { background: var(--bg-card-hover); }
table.po-table tbody td { padding: 8px 10px; color: var(--text-secondary); white-space: nowrap; vertical-align: middle; }
table.po-table tbody td.td-player { min-width: 200px; }
.po-player-cell { display: flex; flex-direction: column; gap: 2px; }
.po-player-name { font-family: 'Calibri', sans-serif; font-size: 16px; font-weight: 500; color: var(--text-primary); }
.po-player-meta { display: flex; align-items: center; gap: 4px; }
.po-team-logo { width: 16px; height: 16px; object-fit: contain; }
.po-player-team { font-size: 10px; color: var(--text-muted); }
.po-home-away { font-size: 10px; color: var(--text-muted); }
.po-prop-badge { display: inline-block; padding: 2px 8px; border-radius: 4px; font-size: 10px; font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: .06em; background: var(--bg-card); border: 1px solid var(--border); color: var(--text-secondary); }
.po-odds-cell { display: flex; flex-direction: column; gap: 2px; }
.po-odds-row { display: flex; align-items: center; gap: 4px; font-size: 10px; }
.po-ou-badge { font-size: 9px; font-family: 'Rajdhani', sans-serif; font-weight: 700; width: 14px; color: var(--text-muted); }
.po-ou-badge.O { color: #5a9a5a; }
.po-ou-badge.U { color: #9a5a5a; }
.po-sb-logo { width: 14px; height: 14px; object-fit: contain; flex-shrink: 0; }
.po-line { color: var(--text-primary); font-weight: 600; min-width: 30px; }
.po-odds-val { color: var(--text-muted); font-size: 10px; }
.po-cover-yes { color: #5a9a5a; font-weight: 700; }
.po-cover-no  { color: #9a5a5a; }
.po-cover-na  { color: var(--text-muted); }
.po-cover-amt-pos { color: #5a9a5a; }
.po-cover-amt-neg { color: #9a5a5a; }
.po-sort-btn { background: none; border: none; cursor: pointer; color: var(--text-muted); font-size: 10px; padding: 0 2px; }
.po-sort-btn:hover { color: var(--text-primary); }
th.sort-asc .po-sort-btn::after  { content: ' ▲'; color: var(--text-primary); }
th.sort-desc .po-sort-btn::after { content: ' ▼'; color: var(--text-primary); }
/* Team odds cards */
.to-banner { display: flex; gap: 0; border-bottom: 1px solid var(--border); flex-shrink: 0; }
.to-banner-stat { flex: 1; padding: 14px 20px; border-right: 1px solid var(--border); display: flex; flex-direction: column; gap: 4px; }
.to-banner-stat:last-child { border-right: none; }
.to-banner-val { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 26px; color: var(--text-primary); line-height: 1; }
.to-banner-lbl { font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .1em; color: var(--text-muted); text-transform: uppercase; }
.to-games { display: flex; flex-direction: column; gap: 10px; padding: 16px 0; }
.to-game-card { background: var(--bg-card); border: 1px solid var(--border); border-radius: 10px; overflow: hidden; }
.to-game-row { display: flex; align-items: center; padding: 10px 16px; gap: 12px; border-bottom: 1px solid var(--border); }
.to-game-row:last-child { border-bottom: none; }
.to-game-team { display: flex; align-items: center; gap: 8px; flex: 1; }
.to-game-logo { width: 28px; height: 28px; object-fit: contain; }
.to-game-name { font-family: 'Calibri', sans-serif; font-size: 13px; font-weight: 500; color: var(--text-primary); }
.to-game-score { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 18px; min-width: 32px; text-align: right; }
.to-game-score.winner { color: #7aba7a; }
.to-odds-block { display: flex; flex-direction: column; align-items: center; gap: 1px; min-width: 70px; background: var(--bg-base); border: 1px solid var(--border); border-radius: 6px; padding: 5px 8px; }
.to-odds-block.covered { background: #0a180a; border-color: #1a3a1a; }
.to-odds-line { font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 13px; color: var(--text-primary); }
.to-odds-val  { font-size: 11px; color: var(--text-muted); font-family: 'Share Tech Mono', monospace; }
.to-odds-actual { font-size: 10px; color: #7aba7a; font-family: 'Share Tech Mono', monospace; }
.to-col-header { display: flex; gap: 8px; justify-content: center; font-size: 9px; font-family: 'Share Tech Mono', monospace; letter-spacing: .08em; color: var(--text-muted); text-transform: uppercase; padding: 6px 16px; background: var(--bg-base); border-bottom: 1px solid var(--border); }
.to-game-footer { display: flex; align-items: center; padding: 6px 16px; background: var(--bg-base); font-size: 10px; font-family: 'Share Tech Mono', monospace; color: var(--text-muted); gap: 8px; }
/* ── GLOBAL SCALE-UP (all tools except Injury Dashboard) ── */
table.gamelog { font-size: 15px; }
table.gamelog thead th { font-size: 13px; }
table.gamelog td, table.gamelog th { padding: 8px 12px; }
table.quarter-table { font-size: 15px; }
table.quarter-table th, table.quarter-table td { padding: 8px 12px; }
table.boxscore { font-size: 15px; }
table.boxscore thead th { font-size: 13px; padding: 8px 12px; }
table.boxscore td { padding: 8px 12px; }
.player-link { font-size: 15px; }
.team-abv { font-size: 15px; }
.boxscore-team-label { font-size: 18px; }
.score-team-name { font-size: 22px; }
.score-team-record { font-size: 14px; }
.score-num { font-size: 64px; }
.score-sep { font-size: 32px; }
.score-status { font-size: 14px; }
.game-leaders-title { font-size: 18px; }
.leader-cat { font-size: 13px; }
.leader-name { font-size: 17px; }
.leader-stat { font-size: 14px; }
.leader-val { font-size: 28px; }
.leaders-section-label { font-size: 13px; }
.player-firstname { font-size: 36px; }
.player-lastname { font-size: 42px; }
.player-meta { font-size: 14px; }
.stat-val { font-size: 36px; }
.stat-lbl { font-size: 13px; }
.bio-lbl { font-size: 13px; }
.bio-val { font-size: 16px; }
.pane-team-name { font-size: 18px; }
.pane-score { font-size: 28px; }
.pane-sym { font-size: 14px; }
.pane-meta { font-size: 13px; }
.pane-date-label { font-size: 18px; }
.cal-game-teams { font-size: 15px; }
.cal-game-sym { font-size: 13px; }
.cal-more { font-size: 13px; }
.cal-dow { font-size: 13px; }

 .dark-toggle { display:flex;align-items:center;gap:6px;padding:4px 10px;background:var(--bg-card);border:1px solid var(--border);border-radius:6px;cursor:pointer;font-family:Calibri,sans-serif;font-size:11px;color:var(--text-secondary);transition:background .15s; }
 .dark-toggle:hover { background:var(--bg-card-hover); }
 .dark-toggle svg { width:14px;height:14px;stroke:var(--text-secondary);fill:none;stroke-width:1.5; }
 .nav-btn { width:26px;height:26px;background:transparent;border:none;cursor:pointer;border-radius:4px;display:flex;align-items:center;justify-content:center;transition:background .15s;color:var(--text-muted); }
 .nav-btn:hover { background:var(--border);color:var(--text-primary); }
 .nav-btn svg { width:14px;height:14px;stroke:currentColor;fill:none;stroke-width:2; }
 .home-section { margin-bottom:28px; }
 .home-section-title { display:flex;align-items:center;gap:10px;margin-bottom:14px;font-family:'Rajdhani',sans-serif;font-weight:700;font-size:18px;letter-spacing:.08em;text-transform:uppercase;color:var(--text-primary); }
 .home-section-title img { height:28px; }
 .slate-wrap { overflow-x:auto;border:1px solid var(--border);border-radius:8px;background:var(--bg-card); }
 table.slate-table { width:100%;border-collapse:collapse;font-family:Calibri,sans-serif;font-size:13px; }
 table.slate-table thead th { padding:6px 8px;text-align:center;font-size:10px;font-family:'Share Tech Mono',monospace;letter-spacing:.08em;color:var(--text-muted);border-bottom:2px solid var(--border);text-transform:uppercase;white-space:nowrap;font-weight:400;position:sticky;top:0;background:var(--bg-card);z-index:2; }
 table.slate-table thead th:first-child { text-align:left; }
 table.slate-table thead th.grp-m { border-bottom-color:#5a7aaa;color:#5a7aaa; }
 table.slate-table thead th.grp-p { border-bottom-color:#7a9a5a;color:#7a9a5a; }
 table.slate-table thead th.grp-v { border-bottom-color:#c8a020;color:#c8a020; }
 table.slate-table thead th.grp-d { border-bottom-color:#7a5a9a;color:#7a5a9a; }
 table.slate-table tbody tr { border-bottom:1px solid var(--border);transition:background .1s; }
 table.slate-table tbody tr:hover { background:var(--bg-card-hover); }
 table.slate-table tbody td { padding:8px;color:var(--text-secondary);text-align:center;white-space:nowrap;vertical-align:middle;font-size:13px; }
 table.slate-table tbody td:first-child { text-align:left; }
 .slate-team-cell { display:flex;align-items:center;gap:5px; }
 .slate-team-cell img { width:22px;height:22px;object-fit:contain; }
 .slate-tn { font-family:'Rajdhani',sans-serif;font-weight:700;font-size:13px;letter-spacing:.04em;color:var(--text-primary); }
 .slate-b2b { font-size:10px;color:#e57373;font-weight:700; }
 .slate-grp-hdr th { font-family:'Rajdhani',sans-serif;font-weight:700;font-size:11px;letter-spacing:.08em;text-transform:uppercase;padding:4px 8px;border-bottom:none !important; }
 .standings-conf-wrap { margin-bottom:20px;overflow-x:auto;border:1px solid var(--border);border-radius:8px;background:var(--bg-card); }
 table.conf-table { width:100%;border-collapse:collapse;font-family:Calibri,sans-serif;font-size:12px; }
 table.conf-table thead th { padding:6px 8px;text-align:center;font-size:10px;font-family:'Share Tech Mono',monospace;letter-spacing:.06em;color:var(--text-muted);border-bottom:2px solid var(--border);font-weight:400;white-space:nowrap;position:sticky;top:0;background:var(--bg-card);z-index:2; }
 table.conf-table thead th:nth-child(1),table.conf-table thead th:nth-child(2),table.conf-table thead th:nth-child(3) { text-align:left; }
 table.conf-table tbody tr { border-bottom:1px solid var(--border);transition:background .1s; }
 table.conf-table tbody tr:hover { background:var(--bg-card-hover); }
 table.conf-table tbody td { padding:6px 8px;color:var(--text-secondary);text-align:center;white-space:nowrap; }
 table.conf-table tbody td:nth-child(3) { text-align:left;font-family:'Rajdhani',sans-serif;font-weight:700;font-size:13px;color:var(--text-primary); }
 .conf-seed { font-family:'Rajdhani',sans-serif;font-weight:700;color:var(--text-primary);font-size:14px;text-align:center !important; }
 .conf-logo { width:22px;height:22px;object-fit:contain;vertical-align:middle; }
 .conf-grn { color:#4a8a4a; } .conf-red { color:#8a4a4a; }
 .leaders-wrap { display:grid;grid-template-columns:repeat(5,1fr);gap:14px; }
 @media(max-width:1200px){.leaders-wrap{grid-template-columns:repeat(3,1fr);}}
 .leader-block { background:var(--bg-card);border:1px solid var(--border);border-radius:8px;overflow:hidden; }
 .leader-block-title { padding:8px 12px;font-family:'Rajdhani',sans-serif;font-weight:700;font-size:13px;letter-spacing:.08em;text-transform:uppercase;color:var(--text-primary);border-bottom:1px solid var(--border); }
 .leader-block-row { display:flex;align-items:center;padding:6px 12px;gap:8px;border-bottom:1px solid rgba(0,0,0,.05);transition:background .1s; }
 .leader-block-row { border-bottom-color: rgba(255,255,255,.04); }
 .leader-block-row:hover { background:var(--bg-card-hover); }
 .leader-rank { font-family:'Rajdhani',sans-serif;font-weight:700;font-size:14px;width:20px;text-align:center; }
 .leader-rank-1 { color:#c8a020; } .leader-rank-2 { color:#8a8a8a; } .leader-rank-3 { color:#a06830; }
 .leader-hs { width:32px;height:32px;border-radius:50%;object-fit:cover;object-position:top;border:1px solid var(--border); }
 .leader-hs-ph { width:32px;height:32px;border-radius:50%;background:var(--bg-base);border:1px solid var(--border); }
 .leader-pname { font-family:Calibri,sans-serif;font-size:12px;font-weight:600;color:var(--text-primary); }
 .leader-pteam { font-size:10px;color:var(--text-muted); }
 .leader-pavg { font-family:'Rajdhani',sans-serif;font-weight:700;font-size:16px;color:var(--text-primary); }

/* ── Striped rows for all data tables ── */
table.slate-table tbody tr:nth-child(even),
table.conf-table tbody tr:nth-child(even),
table.gamelog tbody tr:nth-child(even),
table.boxscore tbody tr:nth-child(even),
table.quarter-table tbody tr:nth-child(even),
table.po-table tbody tr:nth-child(even),
table.pr-table tbody tr:nth-child(even),
table.bv-bet-table tbody tr:nth-child(even) {
  background: var(--bg-card);
}
table.slate-table tbody tr:nth-child(even),
table.conf-table tbody tr:nth-child(even),
table.gamelog tbody tr:nth-child(even),
table.boxscore tbody tr:nth-child(even),
table.quarter-table tbody tr:nth-child(even),
table.po-table tbody tr:nth-child(even),
table.pr-table tbody tr:nth-child(even),
table.bv-bet-table tbody tr:nth-child(even) {
  background: #1c1c22;
}

/* ── Table header rows darker bg ── */
table.slate-table thead th,
table.conf-table thead th {
  background: #1a1a1e !important; color: #aaa !important;
}
body:not(.dark-mode) table.slate-table thead th,
body:not(.dark-mode) table.conf-table thead th {
  background: #d8d8dc !important; color: #555 !important;
}

/* ── 4x Home Page text & logos ── */
.home-section-title { font-size: 28px !important; }
.home-section-title img { height: 44px !important; }
table.slate-table { font-size: 17px; }
table.slate-table thead th { font-size: 14px !important; }
table.slate-table tbody td { font-size: 17px; }
.slate-tn { font-size: 18px !important; }
.slate-team-cell img { width: 32px !important; height: 32px !important; }
table.conf-table { font-size: 16px; }
table.conf-table thead th { font-size: 13px !important; }
.conf-logo { width: 30px !important; height: 30px !important; }
.conf-seed { font-size: 18px !important; }
table.conf-table tbody td:nth-child(3) { font-size: 17px !important; }
.leader-block-title { font-size: 17px !important; }
.leader-pname { font-size: 16px !important; }
.leader-pteam { font-size: 13px !important; }
.leader-pavg { font-size: 22px !important; }
.leader-rank { font-size: 18px !important; }
.leader-hs, .leader-hs-ph { width: 44px !important; height: 44px !important; }

/* ── Pace conditional formatting (green/yellow/red, no white) ── */
.pace-1 { background: #1a472a !important; color: #7aba7a !important; font-weight: 700; }
.pace-2 { background: #1e3e22 !important; color: #6aaa5a !important; font-weight: 600; }
.pace-3 { background: #2a3a1e !important; color: #8a9a4a !important; }
.pace-4 { background: #3a3a1a !important; color: #aaaa3a !important; }
.pace-5 { background: #3a361a !important; color: #bba830 !important; }
.pace-7 { background: #3a2e1a !important; color: #bb8830 !important; }
.pace-8 { background: #3a241a !important; color: #aa6a3a !important; }
.pace-9 { background: #3a1a1a !important; color: #aa4a4a !important; font-weight: 600; }
.pace-10 { background: #471a1a !important; color: #ba4a4a !important; font-weight: 700; }
body:not(.dark-mode) .pace-1 { background: #c0ecc8 !important; color: #1a5a2a !important; }
body:not(.dark-mode) .pace-2 { background: #ccecc4 !important; color: #2a6a2a !important; }
body:not(.dark-mode) .pace-3 { background: #dce8b8 !important; color: #5a6a2a !important; }
body:not(.dark-mode) .pace-4 { background: #ece8a8 !important; color: #7a7a1a !important; }
body:not(.dark-mode) .pace-5 { background: #ece0a0 !important; color: #8a7a1a !important; }
body:not(.dark-mode) .pace-7 { background: #ecd4a0 !important; color: #8a5a1a !important; }
body:not(.dark-mode) .pace-8 { background: #ecc4a0 !important; color: #8a4a1a !important; }
body:not(.dark-mode) .pace-9 { background: #ecb4a0 !important; color: #8a2a1a !important; }
body:not(.dark-mode) .pace-10 { background: #eca4a0 !important; color: #8a1a1a !important; }

/* ── Conference table conditional formatting (green/yellow/red) ── */
.cond-green-5 { background: #1a472a !important; color: #7aba7a !important; font-weight: 700; }
.cond-green-4 { background: #1e3e22 !important; color: #6aaa5a !important; }
.cond-green-3 { background: #243a20 !important; color: #5a9a4a !important; }
.cond-green-2 { background: #2a3a1e !important; color: #6a9a3a !important; }
.cond-green-1 { background: #30381e !important; color: #8a9a3a !important; }
.cond-neutral { background: #3a3a1a !important; color: #aaaa3a !important; }
.cond-red-1 { background: #3a301a !important; color: #aa8a3a !important; }
.cond-red-2 { background: #3a2a1a !important; color: #aa7a3a !important; }
.cond-red-3 { background: #3a221a !important; color: #aa5a3a !important; }
.cond-red-4 { background: #3a1a1a !important; color: #aa4a4a !important; }
.cond-red-5 { background: #471a1a !important; color: #ba4a4a !important; font-weight: 700; }
body:not(.dark-mode) .cond-green-5 { background: #b8ecc4 !important; color: #1a5a2a !important; }
body:not(.dark-mode) .cond-green-4 { background: #c4ecc0 !important; color: #2a6a2a !important; }
body:not(.dark-mode) .cond-green-3 { background: #d0e8b8 !important; color: #3a6a2a !important; }
body:not(.dark-mode) .cond-green-2 { background: #dce8b0 !important; color: #5a7a1a !important; }
body:not(.dark-mode) .cond-green-1 { background: #e4e4a8 !important; color: #6a7a1a !important; }
body:not(.dark-mode) .cond-neutral { background: #ece0a0 !important; color: #7a7a1a !important; }
body:not(.dark-mode) .cond-red-1 { background: #ecd4a0 !important; color: #7a5a1a !important; }
body:not(.dark-mode) .cond-red-2 { background: #ecccac !important; color: #7a4a1a !important; }
body:not(.dark-mode) .cond-red-3 { background: #ecbcac !important; color: #7a3a1a !important; }
body:not(.dark-mode) .cond-red-4 { background: #ecacac !important; color: #7a2a1a !important; }
body:not(.dark-mode) .cond-red-5 { background: #ec9c9c !important; color: #7a1a1a !important; }

/* ── Tighter slate table spacing ── */
table.slate-table tbody td { padding: 5px 6px; }
table.slate-table thead th { padding: 4px 6px; }
table.conf-table tbody td { padding: 5px 6px; }
table.conf-table thead th { padding: 5px 6px; }

/* ── Leaders wrap — 3+2 stack ── */
.leaders-wrap { grid-template-columns: repeat(3, 1fr) !important; gap: 10px !important; }
.leader-block { font-size: 13px; }
@media(max-width:900px) { .leaders-wrap { grid-template-columns: repeat(2, 1fr) !important; } }
/* ── Player & Team Odds scale-up (4x) ── */
table.po-table { font-size: 17px; }
table.po-table thead tr th { font-size: 14px !important; padding: 10px 12px !important; }
table.po-table thead tr.th-group th { font-size: 15px !important; }
table.po-table tbody td { font-size: 17px; padding: 10px 12px !important; }
.po-player-name { font-size: 22px !important; }
.po-player-team, .po-home-away { font-size: 15px !important; }
.po-player-meta span { font-size: 15px !important; }
.po-team-logo { width: 22px !important; height: 22px !important; }
.po-prop-badge { font-size: 16px !important; padding: 4px 12px !important; }
.po-line { font-size: 16px !important; }
.po-odds-val { font-size: 15px !important; }
.po-ou-badge { font-size: 14px !important; }
.po-odds-row { font-size: 14px !important; }
.po-sb-logo { width: 20px !important; height: 20px !important; }
.po-tab { font-size: 16px !important; padding: 14px 22px !important; }
.po-filter-btn { font-size: 15px !important; padding: 8px 16px !important; }
.po-stat-btn { font-size: 14px !important; padding: 7px 16px !important; }
.po-nba-badge { font-size: 15px !important; }
.po-sort-btn { font-size: 14px !important; }
.to-banner-val { font-size: 36px !important; }
.to-banner-lbl { font-size: 13px !important; }
.to-banner-stat { padding: 18px 24px !important; }

/* ── Player & Team Odds scale-up (4x) ── */
table.po-table { font-size: 17px; }
table.po-table thead tr th { font-size: 14px !important; padding: 10px 12px !important; }
table.po-table thead tr.th-group th { font-size: 15px !important; }
table.po-table tbody td { font-size: 17px; padding: 10px 12px !important; }
.po-player-name { font-size: 22px !important; }
.po-player-team, .po-home-away { font-size: 15px !important; }
.po-player-meta span { font-size: 15px !important; }
.po-team-logo { width: 22px !important; height: 22px !important; }
.po-prop-badge { font-size: 16px !important; padding: 4px 12px !important; }
.po-line { font-size: 16px !important; }
.po-odds-val { font-size: 15px !important; }
.po-ou-badge { font-size: 14px !important; }
.po-odds-row { font-size: 14px !important; }
.po-sb-logo { width: 20px !important; height: 20px !important; }
.po-tab { font-size: 16px !important; padding: 14px 22px !important; }
.po-filter-btn { font-size: 15px !important; padding: 8px 16px !important; }
.po-stat-btn { font-size: 14px !important; padding: 7px 16px !important; }
.po-nba-badge { font-size: 15px !important; }
.po-sort-btn { font-size: 14px !important; }
.to-banner-val { font-size: 36px !important; }
.to-banner-lbl { font-size: 13px !important; }
.to-banner-stat { padding: 18px 24px !important; }

/* ── TOP-LEVEL TOOL TABS (Player Stats / Team Stats sub-tabs) ── */
.tool-tabs { display:flex; gap:0; border-bottom:2px solid var(--border); margin-bottom:16px; flex-shrink:0; background:var(--bg-card); border-radius:8px 8px 0 0; overflow:hidden; }
.tool-tab { padding:12px 20px; font-size:13px; font-family:'Rajdhani',sans-serif; font-weight:600; letter-spacing:.06em; text-transform:uppercase; color:var(--text-muted); cursor:pointer; border:none; background:transparent; border-bottom:2px solid transparent; margin-bottom:-2px; transition:color .15s,border-color .15s,background .15s; }
.tool-tab:hover { color:var(--text-secondary); background:var(--bg-card-hover); }
.tool-tab.active { color:var(--text-primary); border-bottom-color:var(--text-primary); background:var(--bg-card); }

/* ── SHOT DETAIL CHART ── */
.sc-layout { display:flex; flex-direction:column; gap:16px; }
.sc-top-row { display:flex; gap:16px; align-items:flex-start; }
.sc-court-col { flex:1; min-width:0; }
.sc-sidebar-col { width:320px; flex-shrink:0; display:flex; flex-direction:column; gap:12px; }
.sc-filters { display:flex; gap:8px; flex-wrap:wrap; margin-bottom:8px; }
.sc-filter-select { background:var(--bg-card); border:1px solid var(--border); border-radius:6px; color:var(--text-secondary); font-family:'Calibri',sans-serif; font-size:12px; padding:6px 10px; cursor:pointer; min-width:120px; }
.sc-filter-select:focus { outline:none; border-color:var(--border-light); }
.sc-court-wrap { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:16px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
.sc-court-wrap { box-shadow: 0 2px 12px rgba(0,0,0,.3); }
.sc-court-svg { width:100%; height:auto; display:block; }
.sc-key { display:flex; align-items:center; justify-content:center; gap:20px; padding:10px 0 0; font-family:'Share Tech Mono',monospace; font-size:11px; color:var(--text-muted); }
.sc-key-item { display:flex; align-items:center; gap:6px; }
.sc-key-dot { width:10px; height:10px; border-radius:50%; border:2px solid; }
.sc-key-dot.made { background:currentColor; }
.sc-key-dot.missed { background:transparent; }

/* Team Stats comparison bars */
.sc-stats-card { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:16px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
.sc-stats-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:15px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); margin-bottom:14px; }
.sc-stat-row { display:flex; align-items:center; gap:8px; margin-bottom:12px; }
.sc-stat-away { flex:1; text-align:right; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:15px; color:var(--text-primary); }
.sc-stat-label { width:90px; text-align:center; font-family:'Share Tech Mono',monospace; font-size:10px; letter-spacing:.06em; color:var(--text-muted); text-transform:uppercase; }
.sc-stat-home { flex:1; text-align:left; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:15px; color:var(--text-primary); }
.sc-stat-detail { font-size:11px; color:var(--text-muted); font-family:'Share Tech Mono',monospace; }
.sc-bar-wrap { display:flex; gap:2px; height:8px; border-radius:4px; overflow:hidden; margin-top:2px; }
.sc-bar-away { height:100%; border-radius:4px 0 0 4px; }
.sc-bar-home { height:100%; border-radius:0 4px 4px 0; }

/* On-court lineup */
.sc-oncourt { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:16px; box-shadow:0 2px 8px rgba(0,0,0,.08); margin-top:16px; }
.sc-oncourt-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:14px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); margin-bottom:10px; }
.sc-lineup-row { display:flex; gap:8px; align-items:center; flex-wrap:wrap; padding:8px 0; border-bottom:1px solid rgba(255,255,255,.04); }
.sc-lineup-player { display:flex; flex-direction:column; align-items:center; gap:3px; width:56px; }
.sc-lineup-hs { width:40px; height:40px; border-radius:50%; object-fit:cover; object-position:top; border:2px solid var(--border); }
.sc-lineup-hs-ph { width:40px; height:40px; border-radius:50%; background:var(--bg-base); border:2px solid var(--border); }
.sc-lineup-name { font-size:8px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); text-align:center; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; max-width:56px; }

/* ── MINI CALENDAR ── */
.mini-cal-wrap { background:var(--bg-card); border:1px solid var(--border); border-radius:8px; padding:10px; }
.mini-cal-nav { display:flex; align-items:center; gap:8px; margin-bottom:8px; }
.mini-cal-btn { width:24px; height:24px; background:var(--bg-card); border:1px solid var(--border); border-radius:4px; cursor:pointer; color:var(--text-secondary); display:flex; align-items:center; justify-content:center; font-size:12px; transition:background .12s; }
.mini-cal-btn:hover { background:var(--bg-card-hover); }
.mini-cal-month { font-family:'Rajdhani',sans-serif; font-weight:600; font-size:13px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); flex:1; text-align:center; }
.mini-cal-grid { display:grid; grid-template-columns:repeat(7,1fr); gap:1px; }
.mini-cal-dow { font-size:8px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); text-align:center; padding:2px 0; letter-spacing:.06em; }
.mini-cal-day { font-size:11px; font-family:'Rajdhani',sans-serif; font-weight:500; text-align:center; padding:4px 2px; border-radius:4px; cursor:default; color:var(--text-muted); }
.mini-cal-day.cur-month { color:var(--text-secondary); }
.mini-cal-day.has-game { color:var(--text-primary); font-weight:700; cursor:pointer; background:var(--bg-card-hover); border:1px solid var(--border); }
.mini-cal-day.has-game:hover { background:var(--border); }
.mini-cal-day.selected { background:#0e180e !important; border-color:#2a4a2a !important; color:#7aba7a !important; }
.mini-cal-day.today { box-shadow:inset 0 0 0 1px var(--border-light); }
body:not(.dark-mode) .mini-cal-day.has-game { background:#eef2f8; border:1px solid #d0d3dc; }
body:not(.dark-mode) .mini-cal-day.selected { background:#d0ecd0 !important; border-color:#4a8a4a !important; color:#1a5a2a !important; }

/* ── GAMECAST ── */
.gc-layout { display:flex; flex-direction:column; gap:16px; }
.gc-scoreboard { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:16px 24px; display:flex; align-items:center; justify-content:center; gap:24px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
.gc-team-block { display:flex; align-items:center; gap:10px; flex:1; }
.gc-team-block.away { justify-content:flex-end; }
.gc-team-block.home { justify-content:flex-start; }
.gc-team-logo { width:48px; height:48px; object-fit:contain; }
.gc-team-name { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:20px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); }
.gc-score-center { display:flex; flex-direction:column; align-items:center; gap:4px; }
.gc-score-nums { display:flex; align-items:center; gap:12px; }
.gc-score-val { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:42px; color:var(--text-primary); line-height:1; }
.gc-score-val.loser { color:var(--text-muted); }
.gc-score-sep { font-size:20px; color:var(--text-muted); }
.gc-status { font-size:11px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); letter-spacing:.08em; }
.gc-qtr-filter { display:flex; gap:4px; justify-content:center; }
.gc-qtr-btn { padding:5px 12px; background:var(--bg-card); border:1px solid var(--border); border-radius:20px; cursor:pointer; font-size:11px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); transition:background .12s,color .12s; }
.gc-qtr-btn:hover { background:var(--bg-card-hover); color:var(--text-secondary); }
.gc-qtr-btn.active { background:#0e180e; border-color:#2a4a2a; color:#7aba7a; }
body:not(.dark-mode) .gc-qtr-btn.active { background:#d0ecd0; border-color:#4a8a4a; color:#1a5a2a; }

.gc-feed { display:flex; flex-direction:column; gap:8px; max-height:calc(100vh - 380px); overflow-y:auto; padding-right:4px; }
.gc-feed::-webkit-scrollbar { width:4px; }
.gc-feed::-webkit-scrollbar-thumb { background:var(--border-light); border-radius:2px; }
.gc-play-card { background:var(--bg-card); border:1px solid var(--border); border-radius:8px; padding:12px 16px; display:flex; align-items:flex-start; gap:12px; transition:background .1s; box-shadow:0 1px 4px rgba(0,0,0,.05); }
.gc-play-card:hover { background:var(--bg-card-hover); }
.gc-play-card.scoring { border-left:3px solid #5a9a5a; }
.gc-play-hs { width:36px; height:36px; border-radius:50%; object-fit:cover; object-position:top; border:2px solid var(--border); flex-shrink:0; margin-top:2px; }
.gc-play-hs-ph { width:36px; height:36px; border-radius:50%; background:var(--bg-base); border:2px solid var(--border); flex-shrink:0; margin-top:2px; }
.gc-play-body { flex:1; min-width:0; }
.gc-play-header { display:flex; align-items:center; gap:8px; margin-bottom:4px; }
.gc-play-type { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:14px; letter-spacing:.04em; color:var(--text-primary); }
.gc-play-clock { font-size:10px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); letter-spacing:.06em; }
.gc-play-text { font-size:12px; color:var(--text-secondary); font-family:'Calibri',sans-serif; line-height:1.5; }
.gc-play-score { display:flex; align-items:center; gap:4px; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; flex-shrink:0; margin-top:2px; }
.gc-play-score .away-s { color:var(--text-secondary); }
.gc-play-score .home-s { color:var(--text-primary); font-weight:900; }
.gc-play-assist { display:flex; align-items:center; gap:6px; font-size:11px; color:var(--text-muted); font-family:'Share Tech Mono',monospace; margin-top:4px; }
.gc-play-assist img { width:20px; height:20px; border-radius:50%; object-fit:cover; object-position:top; border:1px solid var(--border); }
.gc-play-oncourt { display:flex; gap:4px; margin-top:6px; padding-top:6px; border-top:1px solid rgba(255,255,255,.04); }
.gc-play-oncourt img { width:22px; height:22px; border-radius:50%; object-fit:cover; object-position:top; border:1px solid var(--border); }
.gc-play-oncourt .oc-ph { width:22px; height:22px; border-radius:50%; background:var(--bg-base); border:1px solid var(--border); }
.gc-play-oncourt-divider { width:1px; background:var(--border); margin:0 4px; }

/* ── GAME CALENDAR ── */
.gcal-team-row { display:flex; flex-wrap:wrap; gap:6px; margin-bottom:16px; }
.gcal-team-btn { display:flex; align-items:center; justify-content:center; width:42px; height:42px; border-radius:8px; border:1px solid var(--border); background:var(--bg-card); cursor:pointer; transition:background .12s,border-color .12s; padding:4px; }
.gcal-team-btn:hover { background:var(--bg-card-hover); border-color:var(--border-light); }
.gcal-team-btn.active { background:#0e180e; border-color:#2a4a2a; }
body:not(.dark-mode) .gcal-team-btn.active { background:#d0ecd0; border-color:#4a8a4a; }
.gcal-team-btn img { width:30px; height:30px; object-fit:contain; }
/* ── 4x Scale-up: GameCast + Shot Chart ── */
.gc-play-hs, .gc-play-hs-ph { width:52px !important; height:52px !important; }
.gc-play-type { font-size:20px !important; }
.gc-play-clock { font-size:15px !important; }
.gc-play-text { font-size:17px !important; }
.gc-play-assist { font-size:15px !important; }
.gc-play-assist img { width:28px !important; height:28px !important; }
.gc-play-score { font-size:22px !important; }
.gc-play-score .away-s, .gc-play-score .home-s { font-size:22px !important; }
.gc-play-card { padding:16px 20px !important; gap:14px !important; }
.gc-play-oncourt img, .gc-play-oncourt .oc-ph { width:30px !important; height:30px !important; }
.gc-scoreboard { padding:20px 32px !important; }
.gc-team-name { font-size:28px !important; }
.gc-team-logo { width:64px !important; height:64px !important; }
.gc-score-val { font-size:56px !important; }
.gc-score-sep { font-size:28px !important; }
.gc-status { font-size:15px !important; }
.gc-qtr-btn { font-size:14px !important; padding:7px 16px !important; }
.sc-stats-title { font-size:20px !important; }
.sc-stat-away, .sc-stat-home { font-size:20px !important; }
.sc-stat-label { font-size:13px !important; }
.sc-stat-detail { font-size:14px !important; }
.sc-filter-select { font-size:15px !important; padding:8px 12px !important; }
.sc-key { font-size:14px !important; }
.sc-oncourt-title { font-size:18px !important; }
.sc-lineup-hs, .sc-lineup-hs-ph { width:52px !important; height:52px !important; }
.sc-lineup-name { font-size:11px !important; max-width:64px !important; }
.sc-lineup-player { width:64px !important; }
.mini-cal-month { font-size:16px !important; }
.mini-cal-day { font-size:14px !important; }

/* ── GameCast mini court ── */
.gc-mini-court-wrap { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:12px; box-shadow:0 2px 8px rgba(0,0,0,.08); margin-bottom:12px; }
.gc-mini-court-header { display:flex; align-items:center; justify-content:space-between; margin-bottom:8px; }
.gc-mini-court-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:18px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); }
.gc-mini-court-filter { display:flex; align-items:center; gap:8px; font-size:14px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); }
.gc-mini-court-filter img { width:28px; height:28px; border-radius:50%; object-fit:cover; object-position:top; border:2px solid #5a9a5a; }
.gc-mini-court-clear { font-size:13px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); background:var(--bg-base); border:1px solid var(--border); border-radius:4px; padding:3px 10px; cursor:pointer; transition:background .12s; }
.gc-mini-court-clear:hover { background:var(--bg-card-hover); color:var(--text-secondary); }
.gc-mini-court-svg { width:100%; max-width:500px; margin:0 auto; display:block; }

/* Clickable headshot in play cards */
.gc-play-hs-clickable { cursor:pointer; transition:border-color .15s,box-shadow .15s; }
.gc-play-hs-clickable:hover { border-color:var(--border-light); box-shadow:0 0 0 3px rgba(90,154,90,.4); }
/* ── BET SLIP TRAY ── */
#bet-slip-tray {
  position:fixed; top:var(--header-height); right:0; bottom:0;
  width:0; overflow:hidden; transition:width .25s ease;
  border-left:0px solid var(--border); background:var(--bg-sidebar);
  z-index:998; display:flex; flex-direction:column;
}
#bet-slip-tray.open { width:360px; border-left-width:1px; }
.bs-header { padding:16px; border-bottom:1px solid var(--border); display:flex; align-items:center; justify-content:space-between; flex-shrink:0; }
.bs-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:18px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); }
.bs-close { background:transparent; border:none; cursor:pointer; color:var(--text-muted); font-size:18px; padding:4px 8px; border-radius:4px; transition:background .12s; }
.bs-close:hover { background:var(--border); color:var(--text-primary); }
.bs-count { font-size:12px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); background:var(--bg-base); border:1px solid var(--border); border-radius:10px; padding:1px 8px; margin-left:8px; }

/* Bet type toggle */
.bs-type-toggle { display:flex; border-bottom:1px solid var(--border); flex-shrink:0; }
.bs-type-btn { flex:1; padding:10px; font-size:13px; font-family:'Rajdhani',sans-serif; font-weight:600; letter-spacing:.06em; text-transform:uppercase; color:var(--text-muted); cursor:pointer; border:none; background:transparent; border-bottom:2px solid transparent; transition:color .15s,border-color .15s; }
.bs-type-btn:hover { color:var(--text-secondary); }
.bs-type-btn.active { color:var(--text-primary); border-bottom-color:var(--text-primary); }

/* Selections list */
.bs-selections { flex:1; overflow-y:auto; padding:12px; display:flex; flex-direction:column; gap:8px; }
.bs-selections::-webkit-scrollbar { width:3px; }
.bs-selections::-webkit-scrollbar-thumb { background:var(--border-light); border-radius:2px; }
.bs-empty { display:flex; flex-direction:column; align-items:center; justify-content:center; height:120px; gap:8px; opacity:.4; }
.bs-empty p { font-family:'Rajdhani',sans-serif; font-size:13px; letter-spacing:.1em; text-transform:uppercase; color:var(--text-muted); }

/* Selection card */
.bs-sel-card { background:var(--bg-card); border:1px solid var(--border); border-radius:8px; padding:12px; display:flex; flex-direction:column; gap:6px; position:relative; }
.bs-sel-header { display:flex; align-items:center; justify-content:space-between; }
.bs-sel-name { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:15px; color:var(--text-primary); letter-spacing:.04em; }
.bs-sel-remove { background:transparent; border:none; cursor:pointer; color:var(--text-muted); font-size:14px; padding:2px 6px; border-radius:3px; transition:background .12s,color .12s; }
.bs-sel-remove:hover { background:var(--border); color:#9a4a4a; }
.bs-sel-type { font-size:11px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); letter-spacing:.06em; text-transform:uppercase; }
.bs-sel-odds { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; color:var(--text-primary); }
.bs-sel-odds.positive { color:#5a9a5a; }
.bs-sel-line { font-size:12px; color:var(--text-secondary); font-family:'Share Tech Mono',monospace; }
.bs-sel-game { font-size:10px; color:var(--text-muted); font-family:'Share Tech Mono',monospace; }

/* Wager section */
.bs-wager-section { padding:12px 16px; border-top:1px solid var(--border); flex-shrink:0; display:flex; flex-direction:column; gap:10px; }
.bs-wager-row { display:flex; align-items:center; gap:10px; }
.bs-wager-label { font-size:10px; font-family:'Share Tech Mono',monospace; letter-spacing:.1em; color:var(--text-muted); text-transform:uppercase; width:60px; }
.bs-wager-input { flex:1; background:var(--bg-card); border:1px solid var(--border); border-radius:6px; color:var(--text-primary); font-family:'Rajdhani',sans-serif; font-size:18px; font-weight:700; padding:8px 12px; text-align:right; }
.bs-wager-input:focus { outline:none; border-color:var(--border-light); }
.bs-payout-row { display:flex; justify-content:space-between; align-items:center; }
.bs-payout-label { font-size:10px; font-family:'Share Tech Mono',monospace; letter-spacing:.1em; color:var(--text-muted); text-transform:uppercase; }
.bs-payout-val { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:20px; color:#5a9a5a; }
.bs-parlay-odds { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; color:var(--text-primary); }
.bs-kelly-hint { font-size:11px; font-family:'Share Tech Mono',monospace; color:#5a9a5a; letter-spacing:.06em; }

/* Action buttons */
.bs-actions { padding:12px 16px; border-top:1px solid var(--border); flex-shrink:0; display:flex; flex-direction:column; gap:8px; }
.bs-fd-btn { display:flex; align-items:center; justify-content:center; gap:8px; padding:12px; background:#1a3a6a; border:1px solid #2a5a9a; border-radius:8px; cursor:pointer; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:15px; letter-spacing:.06em; color:#fff; text-transform:uppercase; transition:background .15s; }
.bs-fd-btn:hover { background:#2a4a7a; }
.bs-fd-btn img { height:20px; object-fit:contain; }
.bs-log-btn { display:flex; align-items:center; justify-content:center; gap:6px; padding:10px; background:var(--bg-card); border:1px solid var(--border); border-radius:8px; cursor:pointer; font-family:'Rajdhani',sans-serif; font-weight:600; font-size:13px; letter-spacing:.06em; color:var(--text-secondary); text-transform:uppercase; transition:background .12s,border-color .12s; }
.bs-log-btn:hover { background:var(--bg-card-hover); border-color:var(--border-light); }
.bs-log-btn.success { background:#0e1a0e; border-color:#2a4a2a; color:#7aba7a; }
.bs-clear-link { text-align:center; font-size:11px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); cursor:pointer; padding:4px; transition:color .12s; }
.bs-clear-link:hover { color:#9a4a4a; }

/* Clickable odds boxes */
.odds-clickable { cursor:pointer !important; transition:border-color .15s,box-shadow .15s,background .15s !important; }
.odds-clickable:hover { border-color:var(--border-light) !important; box-shadow:0 0 0 2px rgba(26,58,106,.3) !important; }
.odds-clickable.in-slip { border-color:#2a5a9a !important; background:rgba(26,58,106,.1) !important; box-shadow:0 0 0 2px rgba(26,58,106,.3) !important; }
/* ── Fav & Dog Analysis Table ── */
table.fd-table { width:100%; border-collapse:collapse; font-family:'Share Tech Mono',monospace; font-size:15px; }
table.fd-table thead th { padding:8px 10px; text-align:center; font-size:12px; font-family:'Share Tech Mono',monospace; letter-spacing:.06em; color:var(--text-muted); border-bottom:2px solid var(--border); font-weight:400; white-space:nowrap; position:sticky; top:0; background:var(--bg-card); z-index:2; text-transform:uppercase; }
table.fd-table thead th:nth-child(1), table.fd-table thead th:nth-child(2) { text-align:left; }
table.fd-table tbody tr { border-bottom:1px solid var(--border); transition:background .1s; }
table.fd-table tbody tr:hover { background:var(--bg-card-hover); }
table.fd-table tbody td { padding:7px 10px; color:var(--text-secondary); text-align:center; white-space:nowrap; }
table.fd-table tbody td:nth-child(2) { text-align:left; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; color:var(--text-primary); }
table.fd-table tbody tr:nth-child(even) { background:var(--bg-card); }
table.fd-table thead th { background:#1a1a1e !important; color:#aaa !important; }
body:not(.dark-mode) table.fd-table thead th { background:#d8d8dc !important; color:#555 !important; }
.fd-table .fd-logo { width:28px; height:28px; object-fit:contain; vertical-align:middle; }

/* Group headers for underdog / favorite sections */
table.fd-table thead th.fd-grp-u { border-bottom-color:#c8a020; color:#c8a020 !important; }
table.fd-table thead th.fd-grp-f { border-bottom-color:#5a7aaa; color:#5a7aaa !important; }
table.fd-table thead th.fd-grp-g { border-bottom-color:#7a7a8a; color:#7a7a8a !important; }

/* Conditional formatting for percentages */
.fd-pct-1 { background:#1a472a !important; color:#7aba7a !important; font-weight:700; }
.fd-pct-2 { background:#1e3e22 !important; color:#6aaa5a !important; }
.fd-pct-3 { background:#2a3a1e !important; color:#8a9a4a !important; }
.fd-pct-4 { background:#3a3a1a !important; color:#aaaa3a !important; }
.fd-pct-5 { background:#3a3a1a !important; color:#bba830 !important; }
.fd-pct-6 { background:#3a2e1a !important; color:#bb8830 !important; }
.fd-pct-7 { background:#3a241a !important; color:#aa6a3a !important; }
.fd-pct-8 { background:#3a1a1a !important; color:#aa4a4a !important; }
.fd-pct-9 { background:#471a1a !important; color:#ba4a4a !important; font-weight:700; }
body:not(.dark-mode) .fd-pct-1 { background:#b8ecc4 !important; color:#1a5a2a !important; }
body:not(.dark-mode) .fd-pct-2 { background:#c8ecbc !important; color:#2a6a2a !important; }
body:not(.dark-mode) .fd-pct-3 { background:#dce8b0 !important; color:#5a6a2a !important; }
body:not(.dark-mode) .fd-pct-4 { background:#ece8a8 !important; color:#7a7a1a !important; }
body:not(.dark-mode) .fd-pct-5 { background:#ece0a0 !important; color:#8a7a1a !important; }
body:not(.dark-mode) .fd-pct-6 { background:#ecd4a0 !important; color:#8a5a1a !important; }
body:not(.dark-mode) .fd-pct-7 { background:#ecc4a0 !important; color:#8a4a1a !important; }
body:not(.dark-mode) .fd-pct-8 { background:#ecacac !important; color:#7a2a1a !important; }
body:not(.dark-mode) .fd-pct-9 { background:#ec9c9c !important; color:#7a1a1a !important; }
/* ── Fav & Dog Analysis Table ── */
table.fd-table { width:100%; border-collapse:collapse; font-family:'Share Tech Mono',monospace; font-size:15px; }
table.fd-table thead th { padding:8px 10px; text-align:center; font-size:12px; font-family:'Share Tech Mono',monospace; letter-spacing:.06em; color:var(--text-muted); border-bottom:2px solid var(--border); font-weight:400; white-space:nowrap; position:sticky; top:0; background:var(--bg-card); z-index:2; text-transform:uppercase; }
table.fd-table thead th:nth-child(1), table.fd-table thead th:nth-child(2) { text-align:left; }
table.fd-table tbody tr { border-bottom:1px solid var(--border); transition:background .1s; }
table.fd-table tbody tr:hover { background:var(--bg-card-hover); }
table.fd-table tbody td { padding:7px 10px; color:var(--text-secondary); text-align:center; white-space:nowrap; }
table.fd-table tbody td:nth-child(2) { text-align:left; font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; color:var(--text-primary); }
table.fd-table tbody tr:nth-child(even) { background:var(--bg-card); }
table.fd-table thead th { background:#1a1a1e !important; color:#aaa !important; }
body:not(.dark-mode) table.fd-table thead th { background:#d8d8dc !important; color:#555 !important; }
.fd-table .fd-logo { width:28px; height:28px; object-fit:contain; vertical-align:middle; }
table.fd-table thead th.fd-grp-u { border-bottom-color:#c8a020; color:#c8a020 !important; }
table.fd-table thead th.fd-grp-f { border-bottom-color:#5a7aaa; color:#5a7aaa !important; }
table.fd-table thead th.fd-grp-g { border-bottom-color:#7a7a8a; color:#7a7a8a !important; }
.fd-pct-1 { background:#1a472a !important; color:#7aba7a !important; font-weight:700; }
.fd-pct-2 { background:#1e3e22 !important; color:#6aaa5a !important; }
.fd-pct-3 { background:#2a3a1e !important; color:#8a9a4a !important; }
.fd-pct-4 { background:#3a3a1a !important; color:#aaaa3a !important; }
.fd-pct-5 { background:#3a3a1a !important; color:#bba830 !important; }
.fd-pct-6 { background:#3a2e1a !important; color:#bb8830 !important; }
.fd-pct-7 { background:#3a241a !important; color:#aa6a3a !important; }
.fd-pct-8 { background:#3a1a1a !important; color:#aa4a4a !important; }
.fd-pct-9 { background:#471a1a !important; color:#ba4a4a !important; font-weight:700; }
body:not(.dark-mode) .fd-pct-1 { background:#b8ecc4 !important; color:#1a5a2a !important; }
body:not(.dark-mode) .fd-pct-2 { background:#c8ecbc !important; color:#2a6a2a !important; }
body:not(.dark-mode) .fd-pct-3 { background:#dce8b0 !important; color:#5a6a2a !important; }
body:not(.dark-mode) .fd-pct-4 { background:#ece8a8 !important; color:#7a7a1a !important; }
body:not(.dark-mode) .fd-pct-5 { background:#ece0a0 !important; color:#8a7a1a !important; }
body:not(.dark-mode) .fd-pct-6 { background:#ecd4a0 !important; color:#8a5a1a !important; }
body:not(.dark-mode) .fd-pct-7 { background:#ecc4a0 !important; color:#8a4a1a !important; }
body:not(.dark-mode) .fd-pct-8 { background:#ecacac !important; color:#7a2a1a !important; }
body:not(.dark-mode) .fd-pct-9 { background:#ec9c9c !important; color:#7a1a1a !important; }
.fd-slate-section { margin-top:28px; }
.fd-section-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:20px; letter-spacing:.08em; text-transform:uppercase; color:var(--text-primary); margin-bottom:14px; }
.fd-section-sub { font-size:12px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); letter-spacing:.04em; margin-bottom:16px; }
.fd-slate-cards { display:grid; grid-template-columns:repeat(auto-fill, minmax(280px, 1fr)); gap:12px; margin-bottom:20px; }
.fd-slate-card { background:var(--bg-card); border:1px solid var(--border); border-radius:10px; padding:16px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
.fd-slate-card-title { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:16px; letter-spacing:.06em; color:var(--text-primary); margin-bottom:10px; text-transform:uppercase; }
.fd-slate-card-title .fd-slate-count { font-size:12px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); font-weight:400; margin-left:6px; }
.fd-slate-stat-row { display:flex; justify-content:space-between; align-items:center; padding:6px 0; border-bottom:1px solid rgba(255,255,255,.04); }
.fd-slate-stat-row:last-child { border-bottom:none; }
.fd-slate-stat-label { font-size:12px; font-family:'Share Tech Mono',monospace; color:var(--text-muted); letter-spacing:.06em; }
.fd-slate-stat-val { font-family:'Rajdhani',sans-serif; font-weight:700; font-size:18px; color:var(--text-primary); }
.fd-slate-stat-val.dog-color { color:#c8a020; }
.fd-slate-stat-val.fav-color { color:#5a7aaa; }
.fd-slate-bar { height:6px; border-radius:3px; overflow:hidden; display:flex; gap:1px; margin-top:4px; }
.fd-slate-bar-dog { background:#c8a020; height:100%; border-radius:3px 0 0 3px; }
.fd-slate-bar-fav { background:#5a7aaa; height:100%; border-radius:0 3px 3px 0; }
table.fd-slate-table { width:100%; border-collapse:collapse; font-family:'Share Tech Mono',monospace; font-size:14px; }
table.fd-slate-table thead th { padding:8px 10px; text-align:center; font-size:11px; letter-spacing:.06em; color:var(--text-muted); border-bottom:2px solid var(--border); font-weight:400; text-transform:uppercase; background:var(--bg-card); }
table.fd-slate-table thead th:first-child { text-align:left; }
table.fd-slate-table tbody tr { border-bottom:1px solid var(--border); transition:background .1s; }
table.fd-slate-table tbody tr:hover { background:var(--bg-card-hover); }
table.fd-slate-table tbody td { padding:7px 10px; color:var(--text-secondary); text-align:center; }
table.fd-slate-table tbody td:first-child { text-align:left; font-family:'Rajdhani',sans-serif; font-weight:700; color:var(--text-primary); font-size:15px; }
table.fd-slate-table tbody tr:nth-child(even) { background:var(--bg-card); }
table.fd-slate-table thead th { background:#1a1a1e !important; color:#aaa !important; }
body:not(.dark-mode) table.fd-slate-table thead th { background:#d8d8dc !important; color:#555 !important; }
/* ── OPAL THEME ACCENTS ── */

/* Active sidebar indicator — amber instead of grey */
.nav-item.active::before { background: var(--accent) !important; }
.nav-item.active .nav-label { color: var(--accent) !important; }
.nav-item.active .nav-icon { border-color: var(--accent-dim) !important; background: rgba(212,165,116,.08) !important; }

/* Header logo text glow */
.logo-text { color: var(--accent) !important; text-shadow: 0 0 20px rgba(212,165,116,.2); }

/* Card hover glow — warm amber */
.opal-card:hover { border-color: rgba(212,165,116,.3) !important; box-shadow: 0 2px 12px var(--accent-glow) !important; }

/* Tab active states — amber underline */
.player-tab.active, .game-tab.active, .po-tab.active, .pr-tab.active, .tool-tab.active {
  color: var(--accent) !important;
  border-bottom-color: var(--accent) !important;
}

/* Stat values — warm white */
.stat-val, .bv-stat-val, .to-banner-val, .leader-pavg { color: var(--text-primary) !important; }

/* Positive values — keep green */
.bv-stat-val.positive, .bet-result-W, .bv-cal-pnl.pos { color: var(--positive) !important; }
.bv-stat-val.negative, .bet-result-L, .bv-cal-pnl.neg { color: var(--negative) !important; }

/* Buttons — amber accent for primary actions */
.bv-action-btn.primary { border-color: var(--accent-dim) !important; color: var(--accent) !important; }
.bv-action-btn.primary:hover { background: rgba(212,165,116,.08) !important; border-color: var(--accent) !important; }

/* Active filter pills — amber */
.po-stat-btn.active, .pr-man-btn.active {
  background: rgba(212,165,116,.1) !important;
  border-color: var(--accent-dim) !important;
  color: var(--accent) !important;
}

/* Bet slip tray — amber accents */
.bs-fd-btn { background: #1a2a4a !important; border-color: #2a4a7a !important; }
.bs-fd-btn:hover { background: #2a3a5a !important; }

/* Table headers — dark with warm tint */
table.slate-table thead th,
table.conf-table thead th,
table.fd-table thead th,
table.fd-slate-table thead th {
  background: #12121a !important;
  color: #7a7888 !important;
}

/* Score/winner highlights — keep green */
.to-game-score.winner { color: var(--positive) !important; }

/* Section titles — warm */
.home-section-title { color: var(--text-primary) !important; }
#main-title { color: var(--accent) !important; }

/* Scrollbar */
#main-content::-webkit-scrollbar-thumb { background: var(--accent-dim) !important; }
#main-content::-webkit-scrollbar-thumb:hover { background: var(--accent) !important; }

/* Player hero gradient — warmer */
.player-hero {
  background: linear-gradient(to right, #0a0a0e 60%, #161418) !important;
}
.scoreboard-hero {
  background: linear-gradient(to right, #0a0a0e, #141218, #0a0a0e) !important;
}

/* Subtle ambient glow on cards (like the opal shine) */
.opal-card::after {
  background: linear-gradient(135deg, rgba(212,165,116,.03) 0%, transparent 60%) !important;
}

/* Search placeholder — warm border on focus */
.search-placeholder:focus-within { border-color: var(--accent-dim) !important; }

/* Selectize inputs — warm focus */
.selectize-input.focus { border-color: var(--accent-dim) !important; }

/* Winner dots — green stays */
.player-status-dot, .winner-dot, .pane-winner-dot { background: var(--positive) !important; }

/* Calendar today highlight — amber */
.cal-cell.today { background: rgba(212,165,116,.06) !important; }
.cal-cell.today .cal-date-num { background: var(--accent-dim) !important; color: #fff !important; }

/* Mini calendar selected — amber */
.mini-cal-day.selected { background: rgba(212,165,116,.15) !important; border-color: var(--accent-dim) !important; color: var(--accent) !important; }

/* GameCast quarter buttons active — amber */
.gc-qtr-btn.active { background: rgba(212,165,116,.1) !important; border-color: var(--accent-dim) !important; color: var(--accent) !important; }

/* Game Calendar team button active — amber */
.gcal-team-btn.active { background: rgba(212,165,116,.1) !important; border-color: var(--accent-dim) !important; }

/* Scoring play card left border — amber instead of green */
.gc-play-card.scoring { border-left-color: var(--accent) !important; }

/* Injury status — keep red/orange/yellow functional colors */

/* Fav & Dog — keep gold/blue for dog/fav distinction */

/* Player Rotations tray toggle — amber */
.pr-tray-toggle { background: var(--accent) !important; color: #0a0a0b !important; }
.pr-tray-toggle:hover { background: #e0b584 !important; }

/* Bet slip odds clickable hover — amber glow */
.odds-clickable:hover { box-shadow: 0 0 0 2px rgba(212,165,116,.3) !important; }
.odds-clickable.in-slip { border-color: var(--accent-dim) !important; background: rgba(212,165,116,.08) !important; }

/* Dark toggle button — remove or restyle */
.dark-toggle { display: none !important; }
"

# ════════════════════════════════════════════════════════════════════════════════
# JAVASCRIPT
# ════════════════════════════════════════════════════════════════════════════════

OPAL_js <- "
function toggleSidebar() { document.body.classList.toggle('collapsed'); }
document.addEventListener('DOMContentLoaded', function() {
  document.body.classList.add('dark-mode');
});

var navHistory=['home'], navIndex=0;
function pushNav(id){ navHistory=navHistory.slice(0,navIndex+1); navHistory.push(id); navIndex=navHistory.length-1; updateNavBtns(); }
function goBack(){ if(navIndex>0){ navIndex--; Shiny.setInputValue('nav_history_jump',navHistory[navIndex],{priority:'event'}); } updateNavBtns(); }
function goForward(){ if(navIndex<navHistory.length-1){ navIndex++; Shiny.setInputValue('nav_history_jump',navHistory[navIndex],{priority:'event'}); } updateNavBtns(); }
function updateNavBtns(){ var b=document.getElementById('nav-back-btn'),f=document.getElementById('nav-fwd-btn'); if(b) b.style.opacity=navIndex>0?'1':'0.3'; if(f) f.style.opacity=navIndex<navHistory.length-1?'1':'0.3'; }
Shiny.addCustomMessageHandler('setActive', function(id) {
  document.querySelectorAll('.nav-item').forEach(function(el){ el.classList.remove('active'); });
  var el=document.getElementById('nav-'+id); if(el) el.classList.add('active');
  pushNav(id);
});
Shiny.addCustomMessageHandler('setPlayerTab', function(tab) { document.querySelectorAll('.player-tab').forEach(function(el){el.classList.remove('active');}); var el=document.getElementById('ptab-'+tab); if(el) el.classList.add('active'); });
Shiny.addCustomMessageHandler('setGameTab', function(tab) { document.querySelectorAll('.game-tab').forEach(function(el){el.classList.remove('active');}); var el=document.getElementById('gtab-'+tab); if(el) el.classList.add('active'); });
Shiny.addCustomMessageHandler('openSchedulePane', function(date) { var pane=document.getElementById('schedule-pane'); if(pane) pane.classList.add('open'); });
Shiny.addCustomMessageHandler('closeSchedulePane', function(x) { var pane=document.getElementById('schedule-pane'); if(pane) pane.classList.remove('open'); });
Shiny.addCustomMessageHandler('renderTray', function(data) { var existing=document.getElementById('pr-global-tray'); if(!existing){ var tray=document.createElement('div'); tray.id='pr-global-tray'; document.body.appendChild(tray); } document.getElementById('pr-global-tray').innerHTML=data.html; });
Shiny.addCustomMessageHandler('hideTray', function(x) { var t=document.getElementById('pr-global-tray'); if(t) t.innerHTML=''; });
Shiny.addCustomMessageHandler('setToolTab', function(data) {
  document.querySelectorAll('.tool-tab[data-group=\"' + data.group + '\"]').forEach(function(el){ el.classList.remove('active'); });
  var el = document.getElementById('ttab-' + data.group + '-' + data.tab);
  if(el) el.classList.add('active');
});
Shiny.addCustomMessageHandler('setToolTab', function(data) {
  document.querySelectorAll('.tool-tab[data-group=\"' + data.group + '\"]').forEach(function(el){ el.classList.remove('active'); });
  var el = document.getElementById('ttab-' + data.group + '-' + data.tab);
  if(el) el.classList.add('active');
});
document.addEventListener('keydown', function(e) {
  if(e.key === 'Escape') {
    Shiny.setInputValue('gc_player_filter_clear', Math.random(), {priority: 'event'});
  }
});
function openBetSlip() {
  var tray = document.getElementById('bet-slip-tray');
  if(tray) tray.classList.add('open');
}
function closeBetSlip() {
  var tray = document.getElementById('bet-slip-tray');
  if(tray) tray.classList.remove('open');
}
Shiny.addCustomMessageHandler('toggleBetSlip', function(data) {
  if(data.open) openBetSlip(); else closeBetSlip();
});
Shiny.addCustomMessageHandler('betSlipLogged', function(data) {
  var btn = document.getElementById('bs-log-btn');
  if(btn) { btn.classList.add('success'); btn.textContent = '\\u2713 Logged to Bet History';
    setTimeout(function(){ btn.classList.remove('success'); btn.textContent = 'Log to Bet History'; }, 2000);
  }
});
"

# ════════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════════════════════════

nav_item <- function(id, label) {
  tags$button(id = paste0("nav-", id), class = "nav-item",
              onclick = sprintf("Shiny.setInputValue('nav_click', '%s', {priority: 'event'})", id),
              tags$div(class = "nav-icon"), tags$span(class = "nav-label", label)
  )
}

opal_card <- function(label, tag, click_id = NULL) {
  onclick_attr <- if (!is.null(click_id))
    sprintf("Shiny.setInputValue('card_click', '%s', {priority: 'event'})", click_id) else NULL
  tags$div(class = "opal-card", onclick = onclick_attr,
           tags$div(class = "card-icon"), tags$div(class = "card-label", HTML(label)), tags$div(class = "card-tag", tag)
  )
}

standings_row <- function(num, name) {
  tags$div(class = "standings-row",
           tags$span(class = "standings-num", sprintf("%02d", num)),
           tags$span(class = "standings-name", name),
           tags$span(class = "standings-val", "—"), tags$span(class = "standings-val", "—"), tags$span(class = "standings-val", "—")
  )
}

player_tab_btn <- function(id, label, active = FALSE) {
  tags$button(id = paste0("ptab-", id), class = paste("player-tab", if (active) "active" else ""),
              onclick = sprintf("Shiny.setInputValue('player_tab', '%s', {priority: 'event'})", id), label)
}

game_tab_btn <- function(id, label, active = FALSE) {
  tags$button(id = paste0("gtab-", id), class = paste("game-tab", if (active) "active" else ""),
              onclick = sprintf("Shiny.setInputValue('game_tab', '%s', {priority: 'event'})", id), label)
}

fmt     <- function(x, d = 1) { if (is.na(x) || is.null(x)) "—" else round(x, d) }
pct_fmt <- function(x) { if (is.na(x) || is.null(x)) "—" else paste0(round(x * 100, 1), "%") }

# Safe numeric extraction from a single-row dataframe
sn <- function(r, col, d = 0) {
  if (!col %in% names(r)) return("—")
  val <- suppressWarnings(as.numeric(r[[col]][1]))
  if (is.na(val)) "—" else round(val, d)
}
sp <- function(r, col) {
  if (!col %in% names(r)) return("—")
  val <- suppressWarnings(as.numeric(r[[col]][1]))
  if (is.na(val)) "—" else paste0(round(val * 100, 1), "%")
}

# Safe away/home row extraction — handles NA values in IS_AWAY / IS_HOME
get_away_home <- function(t_rows) {
  away_check <- !is.na(t_rows$IS_AWAY) & t_rows$IS_AWAY == 1
  home_check <- !is.na(t_rows$IS_HOME) & t_rows$IS_HOME == 1
  away_row <- if (isTRUE(any(away_check))) t_rows[away_check, ][1, ] else t_rows[1, ]
  home_row <- if (isTRUE(any(home_check))) t_rows[home_check, ][1, ] else t_rows[min(2, nrow(t_rows)), ]
  list(away = away_row, home = home_row)
}

bs_onclick <- function(team_or_player, bet_type, line, odds, game_id="", game_info="") {
  clean <- function(x) gsub("['\"]", "", as.character(x))
  tp  <- clean(team_or_player)
  bt  <- clean(bet_type)
  ln  <- if(is.na(line) || line == "") "" else as.character(line)
  od  <- if(is.na(odds) || odds == "") "" else as.character(odds)
  gid <- clean(game_id)
  gi  <- clean(game_info)
  val <- paste(tp, bt, ln, od, gid, gi, sep="|")
  sprintf("Shiny.setInputValue('bs_add_sel','%s',{priority:'event'})", val)
}

# ════════════════════════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════════════════════════

ui <- tagList(
  tags$head(tags$style(HTML(OPAL_css)), tags$script(HTML(OPAL_js))),
  
  # ── Top header bar ──
  tags$div(id = "opal-header",
           tags$button(id = "hamburger", onclick = "toggleSidebar()", tags$span(), tags$span(), tags$span()),
           tags$button(id = "nav-back-btn", class = "nav-btn", onclick = "goBack()", style = "opacity:0.3;",
                       tags$svg(viewBox = "0 0 24 24", tags$path(d = "M15 18l-6-6 6-6"))),
           tags$button(id = "nav-fwd-btn", class = "nav-btn", onclick = "goForward()", style = "opacity:0.3;",
                       tags$svg(viewBox = "0 0 24 24", tags$path(d = "M9 18l6-6-6-6"))),
           tags$div(class = "logo-area",
                    tags$img(src = logo_b64, height = "28px", style = "border-radius: 6px;"),
                    tags$span(class = "logo-text", "OPAL")),
           tags$div(class = "header-right",
                    tags$div(class = "search-placeholder",
                             tags$svg(viewBox = "0 0 24 24", tags$circle(cx="11",cy="11",r="8"), tags$path(d="m21 21-4.35-4.35")),
                             "Search..."),
                    tags$button(class = "dark-toggle", onclick = "toggleDarkMode()",
                                tags$svg(viewBox = "0 0 24 24", tags$path(d = "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z")),
                                tags$span(id = "dark-mode-label", "Dark")),
                    tags$button(class = "header-btn",
                                tags$svg(viewBox="0 0 24 24", tags$path(d="M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9"), tags$path(d="M13.73 21a2 2 0 0 1-3.46 0"))),
                    tags$button(class = "header-btn",
                                tags$svg(viewBox="0 0 24 24", tags$path(d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"), tags$circle(cx="12",cy="7",r="4"))),
                    tags$script(HTML("document.body.classList.add('dark-mode');"))
                    
           )
  ),
  
  tags$div(id = "app-body",
           
           # ── Left sidebar ──
           tags$aside(id = "opal-sidebar",
                      tags$div(class="nav-section", nav_item("home","Home")),
                      tags$div(class="nav-section", nav_item("prediction","Prediction")),
                      tags$div(class="nav-section", nav_item("research","Research")),
                      tags$div(class="nav-section", nav_item("portfolio","Portfolio Management")),
                      tags$div(class="nav-section", nav_item("standings","Standings"))
           ),
           
           # ── Main content area ──
           tags$main(id = "opal-main",
                     tags$div(id = "main-header",
                              tags$span(id="main-title",    textOutput("main_title",    inline=TRUE)),
                              tags$span(id="main-subtitle", textOutput("main_subtitle", inline=TRUE))
                     ),
                     tags$div(id = "main-content", uiOutput("main_panel"))
           ),
           
           # ── Bet Slip Tray ──
           tags$div(id = "bet-slip-tray",
                    tags$div(class="bs-header",
                             tags$div(style="display:flex;align-items:center;",
                                      tags$span(class="bs-title", "Bet Slip"),
                                      tags$span(class="bs-count", id="bs-count-badge", "0")
                             ),
                             tags$button(class="bs-close", onclick="Shiny.setInputValue('bs_close_tray',Math.random())", "\u00D7")
                    ),
                    uiOutput("bet_slip_tray_ui")
           )
  )
)

# ════════════════════════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Reactive state ──
  active     <- reactiveVal("home")   # which sidebar section is active
  sub_active <- reactiveVal(NULL)     # which tool is open (player_stats / team_stats)
  player_tab <- reactiveVal("gamelog")
  game_tab   <- reactiveVal("boxscore")
  
  # ── Tool-level tab state ──
  ps_tool_tab   <- reactiveVal("search")
  ts_tool_tab   <- reactiveVal("search")
  
  # ── Shot Chart state ──
  sc_game_id     <- reactiveVal(NULL)
  sc_picked_game <- reactiveVal(NULL)
  sc_qtr_filter  <- reactiveVal("ALL")
  sc_type_filter <- reactiveVal("ALL")
  sc_player_filter <- reactiveVal("ALL")
  sc_cal_year    <- reactiveVal(as.integer(format(Sys.Date(), "%Y")))
  sc_cal_month   <- reactiveVal(as.integer(format(Sys.Date(), "%m")))
  
  # ── GameCast state ──
  gc_game_id     <- reactiveVal(NULL)
  gc_qtr_filter  <- reactiveVal("ALL")
  
  # ── Game Calendar state ──
  gcal_team      <- reactiveVal(NULL)
  gcal_cal_year  <- reactiveVal(as.integer(format(Sys.Date(), "%Y")))
  gcal_cal_month <- reactiveVal(as.integer(format(Sys.Date(), "%m")))
  
  
  # ── Tool tab observers ──
  observeEvent(input$ps_tool_tab_click, {
    ps_tool_tab(input$ps_tool_tab_click)
    session$sendCustomMessage("setToolTab", list(group="ps", tab=input$ps_tool_tab_click))
  })
  observeEvent(input$ts_tool_tab_click, {
    ts_tool_tab(input$ts_tool_tab_click)
    session$sendCustomMessage("setToolTab", list(group="ts", tab=input$ts_tool_tab_click))
  })
  
  # Shot Chart observers
  observeEvent(input$sc_game_select, {
    sc_game_id(input$sc_game_select)
    sc_picked_game(NULL)  # reset game picker when date changes
    sc_qtr_filter("ALL"); sc_type_filter("ALL"); sc_player_filter("ALL")
  })
  observeEvent(input$sc_game_picked,  { sc_picked_game(input$sc_game_picked) })
  observeEvent(input$sc_qtr_click,    { sc_qtr_filter(input$sc_qtr_click) })
  observeEvent(input$sc_type_sel,     { sc_type_filter(input$sc_type_sel) })
  observeEvent(input$sc_player_sel,   { sc_player_filter(input$sc_player_sel) })
  observeEvent(input$sc_cal_prev, { m<-sc_cal_month();y<-sc_cal_year(); if(m==1){sc_cal_month(12);sc_cal_year(y-1)}else sc_cal_month(m-1) })
  observeEvent(input$sc_cal_next, { m<-sc_cal_month();y<-sc_cal_year(); if(m==12){sc_cal_month(1);sc_cal_year(y+1)}else sc_cal_month(m+1) })
  
  # GameCast observers
  observeEvent(input$gc_qtr_click, { gc_qtr_filter(input$gc_qtr_click) })
  
  # Game Calendar observers
  observeEvent(input$gcal_team_click,  { gcal_team(input$gcal_team_click) })
  observeEvent(input$gcal_cal_prev, { m<-gcal_cal_month();y<-gcal_cal_year(); if(m==1){gcal_cal_month(12);gcal_cal_year(y-1)}else gcal_cal_month(m-1) })
  observeEvent(input$gcal_cal_next, { m<-gcal_cal_month();y<-gcal_cal_year(); if(m==12){gcal_cal_month(1);gcal_cal_year(y+1)}else gcal_cal_month(m+1) })
  observeEvent(input$gcal_game_click, {
    gid <- input$gcal_game_click
    gc_game_id(gid)
    if(!is.null(sub_active()) && sub_active() == "player_stats") {
      ps_tool_tab("gamecast"); session$sendCustomMessage("setToolTab", list(group="ps", tab="gamecast"))
    } else if(!is.null(sub_active()) && sub_active() == "team_stats") {
      ts_tool_tab("gamecast"); session$sendCustomMessage("setToolTab", list(group="ts", tab="gamecast"))
    }
  })
  
  
  gc_player_filter <- reactiveVal(NULL)
  
  observeEvent(input$gc_play_hs_click, {
    player <- input$gc_play_hs_click
    if(!is.null(gc_player_filter()) && gc_player_filter() == player) {
      gc_player_filter(NULL)
    } else {
      gc_player_filter(player)
    }
  })
  
  observeEvent(input$gc_player_filter_clear, {
    gc_player_filter(NULL)
  })
  
  # ── Bet Slip state ──
  bet_slip <- reactiveVal(list())      # list of selection objects
  bs_mode  <- reactiveVal("straight")  # "straight" or "parlay"
  bs_wager <- reactiveVal(0)
  bs_open  <- reactiveVal(FALSE)
  
  # Add selection to slip
  observeEvent(input$bs_add_sel, {
    sel_str <- input$bs_add_sel
    if(is.null(sel_str) || sel_str == "") return()
    
    # Parse pipe-delimited string: team_or_player|bet_type|line|odds|game_id|game_info
    parts <- strsplit(sel_str, "\\|")[[1]]
    if(length(parts) < 4) return()
    
    parsed <- list(
      team_or_player = parts[1],
      bet_type       = parts[2],
      line           = if(length(parts) >= 3) parts[3] else "",
      odds           = if(length(parts) >= 4) parts[4] else "",
      game_id        = if(length(parts) >= 5) parts[5] else "",
      game_info      = if(length(parts) >= 6) parts[6] else ""
    )
    
    current <- bet_slip()
    # Unique key for dedup/toggle
    key <- paste(parsed$team_or_player, parsed$bet_type, parsed$line, parsed$odds, sep="_")
    existing_keys <- sapply(current, function(s) paste(s$team_or_player, s$bet_type, s$line, s$odds, sep="_"))
    
    if(key %in% existing_keys) {
      # Toggle off — remove from slip
      current <- current[existing_keys != key]
    } else {
      # Add to slip
      parsed$key <- key
      current <- c(current, list(parsed))
    }
    
    bet_slip(current)
    if(length(current) > 0 && !bs_open()) {
      bs_open(TRUE)
      session$sendCustomMessage("toggleBetSlip", list(open=TRUE))
    }
    if(length(current) == 0) {
      bs_open(FALSE)
      session$sendCustomMessage("toggleBetSlip", list(open=FALSE))
    }
  })
  
  # Remove single selection
  observeEvent(input$bs_remove_sel, {
    idx <- as.integer(input$bs_remove_sel)
    current <- bet_slip()
    if(idx >= 1 && idx <= length(current)) {
      current[[idx]] <- NULL
      bet_slip(current)
      if(length(current) == 0) {
        bs_open(FALSE)
        session$sendCustomMessage("toggleBetSlip", list(open=FALSE))
      }
    }
  })
  
  # Clear all
  observeEvent(input$bs_clear_all, {
    bet_slip(list())
    bs_open(FALSE)
    session$sendCustomMessage("toggleBetSlip", list(open=FALSE))
  })
  
  # Toggle tray
  observeEvent(input$bs_toggle_tray, {
    bs_open(!bs_open())
    session$sendCustomMessage("toggleBetSlip", list(open=bs_open()))
  })
  
  # Close tray
  observeEvent(input$bs_close_tray, {
    bs_open(FALSE)
    session$sendCustomMessage("toggleBetSlip", list(open=FALSE))
  })
  
  # Mode toggle
  observeEvent(input$bs_mode_click, { bs_mode(input$bs_mode_click) })
  
  # Wager input
  observeEvent(input$bs_wager_input, {
    v <- suppressWarnings(as.numeric(input$bs_wager_input))
    if(!is.na(v)) bs_wager(v)
  })
  
  # ── Payout helpers ──
  calc_decimal_odds <- function(american) {
    v <- suppressWarnings(as.numeric(american))
    if(is.na(v)) return(1)
    if(v > 0) 1 + v/100 else 1 + 100/abs(v)
  }
  
  calc_payout <- function(selections, wager, mode) {
    if(length(selections) == 0 || wager <= 0) return(list(payout=0, profit=0, parlay_odds=NA))
    if(mode == "straight") {
      # For straight bets, show payout for first selection only
      dec <- calc_decimal_odds(selections[[1]]$odds)
      payout <- round(wager * dec, 2)
      profit <- round(payout - wager, 2)
      return(list(payout=payout, profit=profit, parlay_odds=NA))
    } else {
      # Parlay: multiply all decimal odds
      total_dec <- 1
      for(s in selections) total_dec <- total_dec * calc_decimal_odds(s$odds)
      payout <- round(wager * total_dec, 2)
      profit <- round(payout - wager, 2)
      # Convert back to American
      if(total_dec >= 2) parlay_american <- paste0("+", round((total_dec-1)*100))
      else parlay_american <- as.character(round(-100/(total_dec-1)))
      return(list(payout=payout, profit=profit, parlay_odds=parlay_american))
    }
  }
  
  # ── Log to Bet History ──
  observeEvent(input$bs_log_bets, {
    selections <- bet_slip()
    wager <- bs_wager()
    mode <- bs_mode()
    if(length(selections) == 0 || wager <= 0) return()
    
    df <- bets_rv()
    
    if(mode == "parlay") {
      # Single parlay entry
      pay <- calc_payout(selections, wager, "parlay")
      legs <- paste(sapply(selections, function(s) paste(s$team_or_player, s$bet_type, s$line)), collapse=" | ")
      new_row <- data.frame(
        bet_id=paste0("BET_", format(Sys.time(),"%Y%m%d%H%M%S")),
        date=as.character(Sys.Date()),
        session_id=as.character(Sys.Date()),
        game_id=if(!is.null(selections[[1]]$game_id)) selections[[1]]$game_id else "",
        bet_type=paste0("Parlay (",length(selections)," legs)"),
        team_or_player=legs,
        line=NA,
        odds=as.numeric(gsub("\\+","",pay$parlay_odds)),
        stake=wager,
        kelly_stake=NA,
        result="Pending",
        profit_loss=NA,
        bankroll_after=NA,
        notes=paste("Parlay:", legs),
        stringsAsFactors=FALSE
      )
      df <- dplyr::bind_rows(df, new_row)
    } else {
      # Individual straight bets
      for(s in selections) {
        new_row <- data.frame(
          bet_id=paste0("BET_", format(Sys.time(),"%Y%m%d%H%M%S"), "_", sample(1000:9999,1)),
          date=as.character(Sys.Date()),
          session_id=as.character(Sys.Date()),
          game_id=if(!is.null(s$game_id)) s$game_id else "",
          bet_type=s$bet_type,
          team_or_player=s$team_or_player,
          line=suppressWarnings(as.numeric(s$line)),
          odds=suppressWarnings(as.numeric(s$odds)),
          stake=wager,
          kelly_stake=NA,
          result="Pending",
          profit_loss=NA,
          bankroll_after=NA,
          notes=paste("From bet slip:", s$team_or_player, s$bet_type),
          stringsAsFactors=FALSE
        )
        df <- dplyr::bind_rows(df, new_row)
      }
    }
    
    # Recalculate bankroll
    df <- df %>% arrange(date, bet_id)
    running <- STARTING_BANKROLL
    for(i in seq_len(nrow(df))) {
      pl <- suppressWarnings(as.numeric(df$profit_loss[i]))
      if(!is.na(pl)) running <- running + pl
      df$bankroll_after[i] <- round(running, 2)
    }
    save_bets(df); bets_rv(df)
    
    # Confirm
    session$sendCustomMessage("betSlipLogged", list())
  })
  
  # ── FanDuel link ──
  observeEvent(input$bs_open_fd, {
    # Nothing server-side needed — JS handles the window.open
  })
  
  
  output$bet_slip_tray_ui <- renderUI({
    selections <- bet_slip()
    mode <- bs_mode()
    wager <- bs_wager()
    n <- length(selections)
    
    pay <- calc_payout(selections, wager, mode)
    
    fmt_am <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      if(is.na(v)) return("\u2014")
      if(v > 0) paste0("+", v) else as.character(v)
    }
    
    tagList(
      # Mode toggle
      tags$div(class="bs-type-toggle",
               tags$button(class=paste("bs-type-btn", if(mode=="straight") "active" else ""),
                           onclick="Shiny.setInputValue('bs_mode_click','straight',{priority:'event'})", "Straight bets"),
               tags$button(class=paste("bs-type-btn", if(mode=="parlay") "active" else ""),
                           onclick="Shiny.setInputValue('bs_mode_click','parlay',{priority:'event'})", "Parlay")
      ),
      
      # Selections
      tags$div(class="bs-selections",
               if(n == 0) {
                 tags$div(class="bs-empty", tags$p("Click any odds to add selections"))
               } else {
                 tagList(lapply(seq_len(n), function(i) {
                   s <- selections[[i]]
                   odds_val <- suppressWarnings(as.numeric(s$odds))
                   odds_cls <- if(!is.na(odds_val) && odds_val > 0) "bs-sel-odds positive" else "bs-sel-odds"
                   tags$div(class="bs-sel-card",
                            tags$div(class="bs-sel-header",
                                     tags$span(class="bs-sel-name", s$team_or_player),
                                     tags$button(class="bs-sel-remove",
                                                 onclick=sprintf("Shiny.setInputValue('bs_remove_sel','%d',{priority:'event'})", i), "\u00D7")
                            ),
                            tags$div(style="display:flex;align-items:center;justify-content:space-between;",
                                     tags$div(
                                       tags$div(class="bs-sel-type", s$bet_type),
                                       if(!is.null(s$line) && s$line != "" && !is.na(s$line))
                                         tags$div(class="bs-sel-line", s$line)
                                     ),
                                     tags$span(class=odds_cls, fmt_am(s$odds))
                            ),
                            if(!is.null(s$game_info) && s$game_info != "")
                              tags$div(class="bs-sel-game", s$game_info)
                   )
                 }))
               }
      ),
      
      # Wager section
      if(n > 0) tags$div(class="bs-wager-section",
                         # Parlay combined odds
                         if(mode == "parlay" && n >= 2 && !is.na(pay$parlay_odds)) {
                           tags$div(class="bs-payout-row",
                                    tags$span(class="bs-payout-label", sprintf("%d Leg Parlay", n)),
                                    tags$span(class="bs-parlay-odds", pay$parlay_odds)
                           )
                         },
                         # Wager input
                         tags$div(class="bs-wager-row",
                                  tags$span(class="bs-wager-label", "Wager"),
                                  tags$div(style="position:relative;flex:1;",
                                           tags$span(style="position:absolute;left:12px;top:50%;transform:translateY(-50%);font-family:'Rajdhani',sans-serif;font-size:18px;font-weight:700;color:var(--text-muted);", "$"),
                                           tags$input(type="number", class="bs-wager-input", id="bs_wager_val",
                                                      value=if(wager>0) wager else "",
                                                      placeholder="0",
                                                      style="padding-left:24px;",
                                                      oninput="Shiny.setInputValue('bs_wager_input',this.value,{priority:'event'})")
                                  )
                         ),
                         # To Win
                         tags$div(class="bs-payout-row",
                                  tags$span(class="bs-payout-label", "To Win"),
                                  tags$span(class="bs-payout-val",
                                            if(pay$profit > 0) sprintf("$%s", formatC(pay$profit, format="f", digits=2, big.mark=",")) else "\u2014")
                         )
      ),
      
      # Actions
      if(n > 0) tags$div(class="bs-actions",
                         # Place on FanDuel
                         tags$button(class="bs-fd-btn",
                                     onclick="window.open('https://sportsbook.fanduel.com/navigation/nba','_blank')",
                                     tags$img(src=sb_logos$FD),
                                     "Place on FanDuel"
                         ),
                         # Log to Bet History
                         tags$button(class="bs-log-btn", id="bs-log-btn",
                                     onclick="Shiny.setInputValue('bs_log_bets',Math.random(),{priority:'event'})",
                                     "Log to Bet History"
                         ),
                         # Clear all
                         tags$div(class="bs-clear-link",
                                  onclick="Shiny.setInputValue('bs_clear_all',Math.random(),{priority:'event'})",
                                  "\uD83D\uDDD1 Remove all selections")
      ),
      
      # Update count badge via JS
      tags$script(HTML(sprintf("document.getElementById('bs-count-badge').textContent='%d';", n)))
    )
  })
  
  # ── Navigation observers ──
  observeEvent(input$nav_click,  { active(input$nav_click); sub_active(NULL); session$sendCustomMessage("setActive", input$nav_click) })
  observeEvent(input$card_click, { if (input$card_click %in% c("player_stats","team_stats","schedule_viewer","betting_view","player_rotations","player_team_odds","injury_dashboard")) sub_active(input$card_click) })
  observeEvent(input$nav_history_jump, {
    target <- input$nav_history_jump
    active(target); sub_active(NULL)
  }, ignoreInit = TRUE)
  observeEvent(input$player_tab, { player_tab(input$player_tab); session$sendCustomMessage("setPlayerTab", input$player_tab) })
  observeEvent(input$game_tab,   { game_tab(input$game_tab);     session$sendCustomMessage("setGameTab",   input$game_tab) })
  
  # ── Jump to player (from box score / game leaders click) ──
  observeEvent(input$jump_to_player, {
    sub_active("player_stats")
    player_tab("gamelog")
    updateSelectizeInput(session, "selected_player", selected = input$jump_to_player)
  })
  
  # ── Jump to team stats from calendar game click ──
  observeEvent(input$jump_to_game, {
    sub_active("team_stats")
    gid <- as.character(input$jump_to_game)
    # Find a team for this game_id to pre-select the team dropdown
    match_row <- schedule_data[as.character(schedule_data$game_id) == gid, ]
    if (nrow(match_row) > 0) {
      updateSelectizeInput(session, "selected_team", selected = match_row$team[1])
    }
    updateSelectizeInput(session, "selected_game_id", selected = gid)
  })
  
  # ── Calendar state ──
  cal_year  <- reactiveVal(as.integer(format(Sys.Date(), "%Y")))
  cal_month <- reactiveVal(as.integer(format(Sys.Date(), "%m")))
  cal_pane_date <- reactiveVal(NULL)
  
  observeEvent(input$cal_prev,  { m <- cal_month(); y <- cal_year(); if(m==1){cal_month(12);cal_year(y-1)}else cal_month(m-1) })
  observeEvent(input$cal_next,  { m <- cal_month(); y <- cal_year(); if(m==12){cal_month(1);cal_year(y+1)}else cal_month(m+1) })
  observeEvent(input$cal_today, { cal_year(as.integer(format(Sys.Date(),"%Y"))); cal_month(as.integer(format(Sys.Date(),"%m"))); cal_pane_date(NULL); session$sendCustomMessage("closeSchedulePane", TRUE) })
  
  observeEvent(input$cal_date_click, {
    cal_pane_date(as.Date(input$cal_date_click))
    session$sendCustomMessage("openSchedulePane", input$cal_date_click)
  })
  
  observeEvent(input$close_pane, {
    cal_pane_date(NULL)
    session$sendCustomMessage("closeSchedulePane", TRUE)
  })
  
  # ── Live scoreboard polling (every 30s) ──
  live_timer <- reactiveTimer(30000)
  
  live_scores <- reactive({
    live_timer()
    tryCatch({
      url <- "https://cdn.nba.com/static/json/liveData/scoreboard/todaysScoreboard_00.json"
      raw <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      games <- raw$scoreboard$games
      if (is.null(games) || length(games) == 0) return(NULL)
      
      # Build a clean lookup: one row per game with tricodes and scores
      data.frame(
        game_id     = games$gameId,
        game_status = games$gameStatus,        # 1=pregame, 2=live, 3=final
        game_clock  = games$gameClock,          # e.g. "PT05M23.00S" or ""
        period      = games$period,
        home_tri    = games$homeTeam$teamTricode,
        away_tri    = games$awayTeam$teamTricode,
        home_score  = games$homeTeam$score,
        away_score  = games$awayTeam$score,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message("[OPAL] Live scores fetch error: ", e$message)
      NULL
    })
  })
  
  # Helper: format the game clock from ISO duration to readable
  format_game_clock <- function(clock_str, period, status) {
    if (status == 1) return("PREGAME")
    if (status == 3) return("FINAL")
    # Parse ISO duration like "PT05M23.00S"
    mins <- sub(".*PT(\\d+)M.*", "\\1", clock_str)
    secs <- sub(".*M([\\d.]+)S.*", "\\1", clock_str)
    mins <- suppressWarnings(as.integer(mins))
    secs <- suppressWarnings(as.numeric(secs))
    if (is.na(mins) || is.na(secs)) {
      q_label <- if (period <= 4) paste0("Q", period) else paste0("OT", period - 4)
      return(q_label)
    }
    q_label <- if (period <= 4) paste0("Q", period) else paste0("OT", period - 4)
    sprintf("%s %d:%02.0f", q_label, mins, secs)
  }
  
  # ── Header title / subtitle ──
  # TO CHANGE A PAGE TITLE: edit the matching switch() case below
  output$main_title <- renderText({
    if (!is.null(sub_active()) && sub_active() == "player_stats") return("Player Stats Lookup")
    if (!is.null(sub_active()) && sub_active() == "team_stats")   return("Team Stats Lookup")
    if (!is.null(sub_active()) && sub_active() == "schedule_viewer") return("Schedule Viewer")
    if (!is.null(sub_active()) && sub_active() == "betting_view") return("Betting View")
    if (!is.null(sub_active()) && sub_active() == "player_rotations") return("Player Rotations")
    if (!is.null(sub_active()) && sub_active() == "player_team_odds") return("Player & Team Odds")
    if (!is.null(sub_active()) && sub_active() == "injury_dashboard") return("Injury Dashboard")
    switch(active(), home= 'Home', prediction="Prediction", research="Research",
           portfolio="Portfolio Management", standings="Standings | Current Slate", "OPAL")
  })
  output$main_subtitle <- renderText({
    if (!is.null(sub_active()) && sub_active() %in% c("player_stats","team_stats","schedule_viewer")) return("// 2025-2026 REGULAR SEASON")
    if (!is.null(sub_active()) && sub_active() == "betting_view") return("// KELLY CRITERION · BANKROLL MANAGEMENT")
    if (!is.null(sub_active()) && sub_active() == "player_team_odds") return("// PLAYER PROPS · SPREADS · TOTALS · MONEYLINES")
    if (!is.null(sub_active()) && sub_active() == "player_rotations") return("// LINEUP ANALYTICS · WOWY · 5M / 10M")
    if (!is.null(sub_active()) && sub_active() == "injury_dashboard") return("// NBA INJURY TRACKER · ALL 30 TEAMS")
    switch(active(), home="// CURRENT SLATE | STANDINGS | LEADERS", prediction="// AVITUS · AURELIUS · DFS", research="// ANALYTICS & INTELLIGENCE TOOLS",
           portfolio="// BANKROLL & BET TRACKING", standings="// CURRENT LEAGUE DATA", "// SELECT A MODULE TO BEGIN")
  })
  
  player_choices <- sort(unique(player_data$PLAYER_NAME))
  team_choices   <- sort(unique(team_data$TEAM))
  
  # ════════════════════════════════════════════════════════════════════════════
  # MAIN PANEL ROUTER
  # Routes to the correct view based on active sidebar + sub_active tool.
  # TO ADD A NEW TOOL CARD: add an opal_card() in the research block below,
  #   then add a new if() branch above the switch() to render its UI.
  # ════════════════════════════════════════════════════════════════════════════
  
  output$main_panel <- renderUI({
    
    # ── Route: Player Stats Lookup ──
    if (!is.null(sub_active()) && sub_active() == "player_stats") {
      ps_tab <- ps_tool_tab()
      return(tags$div(class="fade-in",
                      tags$div(class="tool-tabs",
                               tags$button(id="ttab-ps-search", class=paste("tool-tab", if(ps_tab=="search") "active" else ""),
                                           `data-group`="ps", onclick="Shiny.setInputValue('ps_tool_tab_click','search',{priority:'event'})", "Player Stats Search"),
                               tags$button(id="ttab-ps-shotchart", class=paste("tool-tab", if(ps_tab=="shotchart") "active" else ""),
                                           `data-group`="ps", onclick="Shiny.setInputValue('ps_tool_tab_click','shotchart',{priority:'event'})", "Shot Detail Chart"),
                               tags$button(id="ttab-ps-gamecal", class=paste("tool-tab", if(ps_tab=="gamecal") "active" else ""),
                                           `data-group`="ps", onclick="Shiny.setInputValue('ps_tool_tab_click','gamecal',{priority:'event'})", "Game Calendar"),
                               tags$button(id="ttab-ps-gamecast", class=paste("tool-tab", if(ps_tab=="gamecast") "active" else ""),
                                           `data-group`="ps", onclick="Shiny.setInputValue('ps_tool_tab_click','gamecast',{priority:'event'})", "GameCast")
                      ),
                      if(ps_tab == "search") {
                        tagList(
                          tags$div(class="player-search-bar",
                                   selectizeInput("selected_player", label=NULL,
                                                  choices=c("Select a player..."="", player_choices), selected="",
                                                  options=list(placeholder="Search player...", onInitialize=I('function() { this.setValue(""); }')))
                          ),
                          uiOutput("player_profile_ui")
                        )
                      } else if(ps_tab == "shotchart") {
                        uiOutput("sc_ui")
                      } else if(ps_tab == "gamecal") {
                        uiOutput("gcal_ui")
                      } else if(ps_tab == "gamecast") {
                        uiOutput("gc_ui")
                      }
      ))
    }
    
    # ── Route: Team Stats Lookup ──
    if (!is.null(sub_active()) && sub_active() == "team_stats") {
      ts_tab <- ts_tool_tab()
      return(tags$div(class="fade-in",
                      tags$div(class="tool-tabs",
                               tags$button(id="ttab-ts-search", class=paste("tool-tab", if(ts_tab=="search") "active" else ""),
                                           `data-group`="ts", onclick="Shiny.setInputValue('ts_tool_tab_click','search',{priority:'event'})", "Team Stats Search"),
                               tags$button(id="ttab-ts-shotchart", class=paste("tool-tab", if(ts_tab=="shotchart") "active" else ""),
                                           `data-group`="ts", onclick="Shiny.setInputValue('ts_tool_tab_click','shotchart',{priority:'event'})", "Shot Detail Chart"),
                               tags$button(id="ttab-ts-gamecal", class=paste("tool-tab", if(ts_tab=="gamecal") "active" else ""),
                                           `data-group`="ts", onclick="Shiny.setInputValue('ts_tool_tab_click','gamecal',{priority:'event'})", "Game Calendar"),
                               tags$button(id="ttab-ts-gamecast", class=paste("tool-tab", if(ts_tab=="gamecast") "active" else ""),
                                           `data-group`="ts", onclick="Shiny.setInputValue('ts_tool_tab_click','gamecast',{priority:'event'})", "GameCast")
                      ),
                      if(ts_tab == "search") {
                        tagList(
                          tags$div(class="team-lookup-selectors",
                                   tags$div(tags$label("Select Team"),
                                            selectizeInput("selected_team", label=NULL,
                                                           choices=c("Select a team..."="", team_choices), selected="",
                                                           options=list(placeholder="Team...", onInitialize=I('function() { this.setValue(""); }')))
                                   ),
                                   uiOutput("game_selector_ui")
                          ),
                          uiOutput("game_view_ui")
                        )
                      } else if(ts_tab == "shotchart") {
                        uiOutput("sc_ui")
                      } else if(ts_tab == "gamecal") {
                        uiOutput("gcal_ui")
                      } else if(ts_tab == "gamecast") {
                        uiOutput("gc_ui")
                      }
      ))
    }
    
    
    # ── Route: Schedule Viewer ──
    if (!is.null(sub_active()) && sub_active() == "schedule_viewer") {
      return(tags$div(class="fade-in", uiOutput("schedule_viewer_ui")))
    }
    
    # ── Route: Betting View ──
    if (!is.null(sub_active()) && sub_active() == "betting_view") {
      return(tags$div(class="fade-in", uiOutput("betting_view_ui")))
    }
    
    # ── Route: Player Rotations ──
    if (!is.null(sub_active()) && sub_active() == "player_rotations") {
      return(tags$div(class="fade-in", style="display:flex;flex-direction:column;height:calc(100vh - 96px);overflow:hidden;padding:0;", uiOutput("player_rotations_ui")))
    }
    
    # ── Route: Player & Team Odds ──
    if (!is.null(sub_active()) && sub_active() == "injury_dashboard") {
      return(uiOutput("injury_dashboard_ui"))
    }
    if (!is.null(sub_active()) && sub_active() == "player_team_odds") {
      return(tags$div(class="fade-in", style="display:flex;flex-direction:column;height:calc(100vh - 96px);overflow:hidden;padding:0;", uiOutput("player_team_odds_ui")))
    }
    
    # ── Route: Sidebar sections ──
    tags$div(class="fade-in",
             switch(active(),
                    
                    # Home (no sidebar item selected)
                    home = tagList(uiOutput("home_page_ui")),
                    
                    # Prediction section
                    # Prediction section
                    prediction = tagList(
                      tags$div(class="section-intro", tags$h2("Prediction"), tags$p("// MODEL SELECTION — CHOOSE A PREDICTION ENGINE")),
                      tags$div(class="cards-grid",
                               opal_card("Run LogReg<br>AVITUS","Model 01"),
                               tags$div(class = "opal-card", style = "padding:0; overflow:hidden; position:relative; min-height:180px;",
                                        tags$img(src = base64enc::dataURI(
                                          file = paste0(WWW_PATH, "ONYX.png"), mime = "image/png"),
                                          style = "width:100%; height:100%; object-fit:cover; position:absolute; top:0; left:0; opacity:0.7;"),
                                        tags$div(style = "position:relative; z-index:1; display:flex; flex-direction:column; justify-content:flex-end; height:100%; padding:16px; background:linear-gradient(to top, rgba(0,0,0,.7) 0%, transparent 60%);",
                                                 tags$div(class = "card-label", style = "color:#fff; font-size:16px;", "ONYX }{ MC"),
                                                 tags$div(class = "card-tag", "Model 02")
                                        )
                               ),
                               opal_card("DFS Datahub","Model 03")
                      )
                    ),
                    
                    # ── Research section ──
                    # TO ADD A NEW RESEARCH CARD: add an opal_card() line here.
                    # click_id wires the card to a tool — match it in the router above.
                    research = tagList(
                      tags$div(class="section-intro", tags$h2("Research"), tags$p("// ANALYTICS TOOLS — PLAYER & TEAM INTELLIGENCE")),
                      tags$div(class="cards-grid",
                               opal_card("Player Stats Lookup", "Tool 05", click_id="player_stats"),
                               opal_card("Team Stats Lookup",   "Tool 14", click_id="team_stats"),
                               opal_card("Player Comparison",   "Tool 06"),
                               opal_card("Injury Dashboard",    "Tool 07", click_id="injury_dashboard"),
                               opal_card("Data Hub Viewer",     "Tool 10"),
                               opal_card("Schedule Viewer",     "Tool 12", click_id="schedule_viewer"),
                               opal_card("Player & Team Odds",  "Tool 13", click_id="player_team_odds"),
                               opal_card("Player Rotations",    "Tool 15", click_id="player_rotations")
                      )
                    ),
                    
                    # Portfolio section
                    portfolio = tagList(
                      tags$div(class="section-intro", tags$h2("Portfolio Management"), tags$p("// BET TRACKING & BANKROLL MANAGEMENT")),
                      tags$div(class="cards-grid", opal_card("Betting View","Tool 11", click_id="betting_view"))
                    ),
                    
                    # Standings section
                    standings = tagList(
                      tags$div(class="section-intro", tags$h2("League Standings & Slate"), tags$p("// CURRENT SLATE — PLACEHOLDER DATA")),
                      tags$div(class="standings-placeholder",
                               standings_row(1,"Team Placeholder A"), standings_row(2,"Team Placeholder B"),
                               standings_row(3,"Team Placeholder C"), standings_row(4,"Team Placeholder D"),
                               standings_row(5,"Team Placeholder E")
                      )
                    )
             )
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TEAM STATS LOOKUP — GAME SELECTOR
  # Renders the game dropdown after a team is chosen.
  # ════════════════════════════════════════════════════════════════════════════
  
  # ════════════════════════════════════════════════════════════════════════════
  # TEAM STATS LOOKUP — GAME SELECTOR
  # Renders the game dropdown after a team is chosen.
  # ════════════════════════════════════════════════════════════════════════════
  
  output$game_selector_ui <- renderUI({
    req(input$selected_team, input$selected_team != "")
    tgames <- team_data %>% filter(TEAM == input$selected_team) %>% arrange(desc(GAME_DATE))
    req(nrow(tgames) > 0)
    choices_vec <- sapply(seq_len(nrow(tgames)), function(i) {
      r      <- tgames[i,]
      gdate  <- tryCatch(format(as.Date(as.character(r$GAME_DATE)), "%m/%d/%y"), error=function(e) as.character(r$GAME_DATE))
      ha_sym <- if (isTRUE(r$is_away[1] == 1)) "@" else "vs"
      paste(gdate, r$TEAM, ha_sym, r$OPP)
    })
    ids <- as.character(tgames$ESPN_GAME_ID)
    names(ids) <- choices_vec
    tags$div(tags$label("Select Game"),
             selectizeInput("selected_game_id", label=NULL, choices=c("Select a game..."="", ids), selected="",
                            options=list(placeholder="Game...", onInitialize=I('function() { this.setValue(""); }')))
    )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TEAM STATS LOOKUP — GAME VIEW
  # Renders scoreboard header + quarter table + tab shell for a selected game.
  # ════════════════════════════════════════════════════════════════════════════
  
  output$game_view_ui <- renderUI({
    req(input$selected_game_id, input$selected_game_id != "")
    gid    <- input$selected_game_id
    t_rows <- team_data %>% filter(as.character(ESPN_GAME_ID) == gid)
    req(nrow(t_rows) >= 1)
    
    rows     <- get_away_home(t_rows)
    away_row <- rows$away; home_row <- rows$home
    
    away_team <- as.character(away_row$TEAM[1])
    home_team <- as.character(home_row$TEAM[1])
    away_logo <- if ("team_logo"%in%names(away_row) && !is.na(away_row$team_logo[1])) as.character(away_row$team_logo[1]) else ""
    home_logo <- if ("team_logo"%in%names(home_row) && !is.na(home_row$team_logo[1])) as.character(home_row$team_logo[1]) else ""
    away_pts  <- suppressWarnings(as.numeric(away_row$PTS_CGS[1]))
    home_pts  <- suppressWarnings(as.numeric(home_row$PTS_CGS[1]))
    away_won  <- isTRUE(!is.na(away_row$TEAM_WINNER[1]) && away_row$TEAM_WINNER[1] == 1)
    home_won  <- isTRUE(!is.na(home_row$TEAM_WINNER[1]) && home_row$TEAM_WINNER[1] == 1)
    
    # Dynamic quarter detection
    q_all <- c("PTS_Q1","PTS_Q2","PTS_Q3","PTS_Q4","PTS_Q5","PTS_Q6")
    q_present <- q_all[sapply(q_all, function(col) {
      if (!col %in% names(t_rows)) return(FALSE)
      vals <- suppressWarnings(as.numeric(t_rows[[col]]))
      isTRUE(any(vals > 0, na.rm = TRUE))
    })]
    q_show   <- unique(c("PTS_Q1","PTS_Q2","PTS_Q3","PTS_Q4", q_present))
    q_labels <- gsub("PTS_Q","Q", q_show)
    q_labels[q_labels=="Q5"] <- "OT1"; q_labels[q_labels=="Q6"] <- "OT2"
    
    make_qt_row <- function(row, tname, logo) {
      tc <- if (nchar(logo)>0)
        sprintf("<td><div class='qt-team-cell'><img class='team-logo-img' src='%s'/><span class='team-abv'>%s</span></div></td>", logo, tname)
      else sprintf("<td><span class='team-abv'>%s</span></td>", tname)
      qc  <- paste(sapply(q_show, function(col) {
        v <- suppressWarnings(as.numeric(row[[col]][1]))
        sprintf("<td>%s</td>", if(is.na(v)) "—" else as.integer(v))
      }), collapse="")
      tot <- suppressWarnings(as.numeric(row$PTS_CGS[1]))
      sprintf("<tr>%s%s<td class='qt-total'>%s</td></tr>", tc, qc, if(is.na(tot)) "—" else as.integer(tot))
    }
    
    qt_html <- sprintf(
      "<table class='quarter-table'><thead><tr><th></th>%s<th>T</th></tr></thead><tbody>%s%s</tbody></table>",
      paste(sprintf("<th>%s</th>", q_labels), collapse=""),
      make_qt_row(away_row, away_team, away_logo),
      make_qt_row(home_row, home_team, home_logo)
    )
    
    tagList(tags$div(class="game-scoreboard",
                     tags$div(class="scoreboard-hero",
                              tags$div(class="score-team",
                                       if(nchar(away_logo)>0) tags$img(class="score-team-logo",src=away_logo) else tags$div(class="score-team-logo-placeholder"),
                                       tags$div(class="score-team-name", away_team), tags$div(class="score-team-record","Away")
                              ),
                              tags$div(class="score-center",
                                       tags$div(class="score-numbers",
                                                tags$span(class=paste("score-num", if(away_won)"winner"else"loser"), if(is.na(away_pts))"—"else as.integer(away_pts)),
                                                tags$span(class="score-sep", "-"),
                                                tags$span(class=paste("score-num", if(home_won)"winner"else"loser"), if(is.na(home_pts))"—"else as.integer(home_pts))
                                       ),
                                       tags$div(class="score-status","FINAL")
                              ),
                              tags$div(class="score-team",
                                       if(nchar(home_logo)>0) tags$img(class="score-team-logo",src=home_logo) else tags$div(class="score-team-logo-placeholder"),
                                       tags$div(class="score-team-name", home_team), tags$div(class="score-team-record","Home")
                              )
                     ),
                     tags$div(class="quarter-table-wrap", HTML(qt_html)),
                     tags$div(class="game-tabs",
                              game_tab_btn("recap","Recap"),
                              game_tab_btn("boxscore","Box Score", active=TRUE),
                              game_tab_btn("pbp","Play-by-Play"),
                              game_tab_btn("teamstats","Team Stats")
                     ),
                     tags$div(class="game-tab-content", uiOutput("game_tab_content"))
    ))
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TEAM STATS LOOKUP — GAME TAB CONTENT
  # Renders the selected tab: Recap / Box Score / Play-by-Play / Team Stats
  # ════════════════════════════════════════════════════════════════════════════
  
  output$game_tab_content <- renderUI({
    req(input$selected_game_id, input$selected_game_id != "")
    gid <- input$selected_game_id
    tab <- game_tab()
    
    # Placeholders
    if (tab %in% c("recap","pbp")) {
      return(tags$div(style="padding:40px 28px;text-align:center;opacity:.4;",
                      tags$p(style="font-family:'Rajdhani',sans-serif;font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--text-muted);","— Placeholder —")
      ))
    }
    
    t_rows <- team_data   %>% filter(as.character(ESPN_GAME_ID) == gid)
    p_rows <- player_data %>% filter(as.character(ESPN_GAME_ID) == gid)
    
    rows      <- get_away_home(t_rows)
    away_row  <- rows$away; home_row <- rows$home
    away_team <- as.character(away_row$TEAM[1])
    home_team <- as.character(home_row$TEAM[1])
    away_logo <- if ("team_logo"%in%names(away_row) && !is.na(away_row$team_logo[1])) as.character(away_row$team_logo[1]) else ""
    home_logo <- if ("team_logo"%in%names(home_row) && !is.na(home_row$team_logo[1])) as.character(home_row$team_logo[1]) else ""
    
    # ── Team Stats tab ──
    if (tab == "teamstats") {
      if (nrow(t_rows)==0) return(tags$p(style="padding:20px;color:var(--text-muted);","No team data."))
      rows_html <- paste(sapply(seq_len(nrow(t_rows)), function(i) {
        r   <- t_rows[i,]
        fgm <- suppressWarnings(as.numeric(r$FGM_CGS[1])); fga <- suppressWarnings(as.numeric(r$FGA_CGS[1]))
        tpm <- suppressWarnings(as.numeric(r$`3PTM_CGS`[1])); tpa <- suppressWarnings(as.numeric(r$`3PTA_CGS`[1]))
        ftm <- suppressWarnings(as.numeric(r$FTM_CGS[1]));  fta <- suppressWarnings(as.numeric(r$FTA_CGS[1]))
        fg_s <- if(!is.na(fgm)&&!is.na(fga)) sprintf("%d-%d",as.integer(fgm),as.integer(fga)) else "—"
        tp_s <- if(!is.na(tpm)&&!is.na(tpa)) sprintf("%d-%d",as.integer(tpm),as.integer(tpa)) else "—"
        ft_s <- if(!is.na(ftm)&&!is.na(fta)) sprintf("%d-%d",as.integer(ftm),as.integer(fta)) else "—"
        sprintf("<tr><td class='team-abv'>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='qt-total'>%s</td></tr>",
                as.character(r$TEAM[1]), fg_s, sp(r,"FG_PCT_CGS"), tp_s, ft_s,
                sn(r,"OREB_CGS"), sn(r,"REB_CGS"), sn(r,"AST_CGS"), sn(r,"STL_CGS"), sn(r,"BLK_CGS"), sn(r,"TOV_CGS"), sn(r,"PTS_CGS"))
      }), collapse="")
      return(tags$div(class="boxscore-wrap",
                      HTML(sprintf("<table class='boxscore'><thead><tr><th>TEAM</th><th>FG</th><th>FG%%</th><th>3PT</th><th>FT</th><th>OREB</th><th>REB</th><th>AST</th><th>STL</th><th>BLK</th><th>TO</th><th>PTS</th></tr></thead><tbody>%s</tbody></table>", rows_html))
      ))
    }
    
    # ── Box Score tab ──
    if (nrow(p_rows)==0) return(tags$p(style="padding:20px;color:var(--text-muted);","No player data."))
    
    bs_header <- "<thead><tr><th>PLAYER</th><th>MIN</th><th>FG</th><th>FG%</th><th>3PT</th><th>FT</th><th>OREB</th><th>REB</th><th>AST</th><th>STL</th><th>BLK</th><th>PTS</th></tr></thead>"
    
    make_player_row <- function(r) {
      pname <- as.character(r$PLAYER_NAME[1])
      fgm <- suppressWarnings(as.numeric(r$FGM_CGS[1]))
      fga <- suppressWarnings(as.numeric(r$FGA_CGS[1]))
      tpm <- suppressWarnings(as.numeric(r$`3PTM_CGS`[1]))
      tpa <- suppressWarnings(as.numeric(r$`3PTA_CGS`[1]))
      ftm <- suppressWarnings(as.numeric(r$FTM_CGS[1]))
      fta <- suppressWarnings(as.numeric(r$FTA_CGS[1]))
      fg_s <- if(isTRUE(!is.na(fgm) && !is.na(fga))) sprintf("%d-%d", as.integer(fgm), as.integer(fga)) else "—"
      tp_s <- if(isTRUE(!is.na(tpm) && !is.na(tpa))) sprintf("%d-%d", as.integer(tpm), as.integer(tpa)) else "—"
      ft_s <- if(isTRUE(!is.na(ftm) && !is.na(fta))) sprintf("%d-%d", as.integer(ftm), as.integer(fta)) else "—"
      njs      <- gsub("'", "\\\\'", pname)
      headshot <- if ("HEADSHOT" %in% names(r) && !is.na(r$HEADSHOT[1]) && nchar(as.character(r$HEADSHOT[1])) > 0)
        sprintf("<img src='%s' style='width:24px;height:24px;border-radius:50%%;object-fit:cover;object-position:top;margin-right:6px;border:1px solid var(--border);vertical-align:middle;'/>", r$HEADSHOT[1])
      else ""
      nhtml <- sprintf("<div style='display:flex;align-items:center;'>%s<span class='player-link' onclick=\"Shiny.setInputValue('jump_to_player','%s',{priority:'event'})\">%s</span></div>", headshot, njs, pname)
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='qt-total'>%s</td></tr>",
              nhtml, sn(r,"MINS_CGS",1), fg_s, sp(r,"FG_PCT_CGS"), tp_s, ft_s,
              sn(r,"OREB_CGS"), sn(r,"REB_CGS"), sn(r,"AST_CGS"), sn(r,"STL_CGS"), sn(r,"BLK_CGS"), sn(r,"PTS_CGS"))
    }
    
    make_team_bs <- function(tname, logo) {
      tp <- p_rows %>% filter(TEAM == tname)
      if (nrow(tp)==0) return("")
      rows    <- paste(sapply(seq_len(nrow(tp)), function(i) make_player_row(tp[i,])), collapse="")
      sum_col <- function(col) sum(suppressWarnings(as.numeric(tp[[col]])), na.rm=TRUE)
      tfgm <- sum_col("FGM_CGS"); tfga <- sum_col("FGA_CGS")
      ttpm <- sum_col("3PTM_CGS"); ttpa <- sum_col("3PTA_CGS")
      tftm <- sum_col("FTM_CGS");  tfta <- sum_col("FTA_CGS")
      fg_pct_t <- if(tfga>0) paste0(round(tfgm/tfga*100,1),"%") else "—"
      totals <- sprintf("<tr class='totals-row'><td>TOTALS</td><td>—</td><td>%d-%d</td><td>%s</td><td>%d-%d</td><td>%d-%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>",
                        as.integer(tfgm), as.integer(tfga), fg_pct_t,
                        as.integer(ttpm), as.integer(ttpa), as.integer(tftm), as.integer(tfta),
                        as.integer(sum_col("OREB_CGS")), as.integer(sum_col("REB_CGS")),
                        as.integer(sum_col("AST_CGS")),  as.integer(sum_col("STL_CGS")),
                        as.integer(sum_col("BLK_CGS")),  as.integer(sum_col("PTS_CGS"))
      )
      lbl <- if(nchar(logo)>0) sprintf("<img class='team-logo-img' src='%s' style='margin-right:8px;'/>", logo) else ""
      paste0(sprintf("<div class='boxscore-team-label'>%s%s</div>", lbl, tname),
             sprintf("<table class='boxscore'>%s<tbody>%s%s</tbody></table>", bs_header, rows, totals))
    }
    
    # ── Game Leaders ──
    get_leader <- function(tname, stat_col, stat_lbl) {
      tp <- p_rows %>% filter(TEAM == tname)
      if (nrow(tp)==0) return(NULL)
      tp[[stat_col]] <- suppressWarnings(as.numeric(tp[[stat_col]]))
      best <- tp %>% filter(!is.na(.data[[stat_col]])) %>% arrange(desc(.data[[stat_col]])) %>% slice(1)
      if (nrow(best)==0) return(NULL)
      list(name=best$PLAYER_NAME[1], val=best[[stat_col]][1],
           headshot=if("HEADSHOT"%in%names(best)&&!is.na(best$HEADSHOT[1]))best$HEADSHOT[1]else"", stat=stat_lbl)
    }
    
    make_leader_card <- function(cat, away_l, home_l) {
      side <- function(l, tname) {
        if (is.null(l)) return("<div style='opacity:.4;font-family:Share Tech Mono,monospace;font-size:11px;color:var(--text-muted);'>No data</div>")
        hs  <- if(nchar(l$headshot)>0) sprintf("<img class='leader-headshot' src='%s'/>", l$headshot) else "<div class='leader-headshot-ph'></div>"
        njs <- gsub("'", "\\\\'", l$name)
        sprintf("<div class='leader-row'>%s<div class='leader-info'><div class='leader-name' onclick=\"Shiny.setInputValue('jump_to_player','%s',{priority:'event'})\">%s</div><div class='leader-stat'>%s · %s</div></div><div class='leader-val'>%s</div></div>",
                hs, njs, l$name, tname, l$stat, as.integer(l$val))
      }
      sprintf("<div class='leader-card'><div class='leader-cat'>%s</div><div class='leaders-section-label'>%s</div>%s<div class='leaders-section-label'>%s</div>%s</div>",
              cat, away_team, side(away_l, away_team), home_team, side(home_l, home_team))
    }
    
    leaders_html <- sprintf("<div class='leaders-grid'>%s%s%s</div>",
                            make_leader_card("Points",   get_leader(away_team,"PTS_CGS","PTS"), get_leader(home_team,"PTS_CGS","PTS")),
                            make_leader_card("Rebounds", get_leader(away_team,"REB_CGS","REB"), get_leader(home_team,"REB_CGS","REB")),
                            make_leader_card("Assists",  get_leader(away_team,"AST_CGS","AST"), get_leader(home_team,"AST_CGS","AST"))
    )
    
    tags$div(
      tags$div(class="boxscore-wrap", HTML(make_team_bs(away_team, away_logo))),
      tags$div(class="boxscore-wrap", HTML(make_team_bs(home_team, home_logo))),
      tags$div(class="game-leaders",
               tags$div(class="game-leaders-title","Game Leaders"),
               HTML(leaders_html)
      )
    )
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # PLAYER STATS LOOKUP — PROFILE HEADER
  # Renders headshot, name, team, season averages, bio strip, and tab bar.
  # ════════════════════════════════════════════════════════════════════════════
  
  output$player_profile_ui <- renderUI({
    req(input$selected_player, input$selected_player != "")
    p     <- input$selected_player
    pdata <- player_data %>% filter(PLAYER_NAME == p)
    req(nrow(pdata) > 0)
    
    avg_pts <- mean(pdata$PTS_CGS, na.rm=TRUE); avg_reb <- mean(pdata$REB_CGS, na.rm=TRUE)
    avg_ast <- mean(pdata$AST_CGS, na.rm=TRUE)
    tot_fgm <- sum(pdata$FGM_CGS, na.rm=TRUE);  tot_fga <- sum(pdata$FGA_CGS, na.rm=TRUE)
    fg_pct  <- if(tot_fga>0) tot_fgm/tot_fga else NA
    
    p1       <- pdata[1,]
    headshot <- if("HEADSHOT"%in%names(p1) && !is.na(p1$HEADSHOT) && p1$HEADSHOT!="") p1$HEADSHOT else NULL
    team_abv <- if("TEAM"%in%names(p1)) p1$TEAM else "—"
    nparts   <- strsplit(p," ")[[1]]
    fn <- if(length(nparts)>=1) nparts[1] else ""
    ln <- if(length(nparts)>=2) paste(nparts[-1], collapse=" ") else ""
    
    tagList(tags$div(class="player-profile",
                     tags$div(class="player-hero",
                              if(!is.null(headshot)) tags$img(class="player-headshot", src=headshot)
                              else tags$div(class="player-headshot-placeholder",
                                            tags$svg(viewBox="0 0 24 24",
                                                     tags$path(d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"),
                                                     tags$circle(cx="12", cy="7", r="4"))
                              ),
                              tags$div(class="player-identity",
                                       tags$div(class="player-name-line",
                                                tags$span(class="player-firstname", fn), tags$span(class="player-lastname", ln)
                                       ),
                                       tags$div(class="player-meta", tags$span(class="player-status-dot"), team_abv, " · Active")
                              ),
                              tags$div(class="season-stats-banner",
                                       tags$div(class="stat-block", tags$div(class="stat-val", fmt(avg_pts)), tags$div(class="stat-lbl","PTS")),
                                       tags$div(class="stat-block", tags$div(class="stat-val", fmt(avg_reb)), tags$div(class="stat-lbl","REB")),
                                       tags$div(class="stat-block", tags$div(class="stat-val", fmt(avg_ast)), tags$div(class="stat-lbl","AST")),
                                       tags$div(class="stat-block", tags$div(class="stat-val", pct_fmt(fg_pct)), tags$div(class="stat-lbl","FG%")),
                                       tags$div(class="season-label","2025-26 REGULAR SEASON STATS")
                              )
                     ),
                     tags$div(class="bio-strip",
                              tags$div(class="bio-item", tags$div(class="bio-lbl","HT/WT"),     tags$div(class="bio-val","—")),
                              tags$div(class="bio-item", tags$div(class="bio-lbl","Birthdate"), tags$div(class="bio-val","—")),
                              tags$div(class="bio-item", tags$div(class="bio-lbl","College"),   tags$div(class="bio-val","—")),
                              tags$div(class="bio-item", tags$div(class="bio-lbl","Draft Info"),tags$div(class="bio-val","—")),
                              tags$div(class="bio-item", tags$div(class="bio-lbl","Status"),    tags$div(class="bio-val", tags$span(class="player-status-dot"), "Active"))
                     ),
                     tags$div(class="player-tabs",
                              player_tab_btn("news","News"), player_tab_btn("stats","Stats"),
                              player_tab_btn("gamelog","Game Log", active=TRUE), player_tab_btn("splits","Splits")
                     ),
                     tags$div(class="tab-content", uiOutput("player_tab_content"))
    ))
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # PLAYER STATS LOOKUP — GAME LOG TAB
  # Renders the per-game stat table for the selected player.
  # ════════════════════════════════════════════════════════════════════════════
  
  output$player_tab_content <- renderUI({
    req(input$selected_player, input$selected_player != "")
    p     <- input$selected_player
    pdata <- player_data %>% filter(PLAYER_NAME == p) %>% arrange(desc(GAME_DATE)) %>%
      mutate(DATE = format(as.Date(as.character(GAME_DATE)), "%m/%d/%y"))
    tab <- player_tab()
    
    if (tab %in% c("news","stats","splits")) {
      return(tags$div(style="padding:40px 28px;text-align:center;opacity:.4;",
                      tags$p(style="font-family:'Rajdhani',sans-serif;font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--text-muted);","— Placeholder —")
      ))
    }
    
    rows_html <- lapply(seq_len(nrow(pdata)), function(i) {
      r         <- pdata[i,]
      gdate     <- if("DATE"%in%names(r) && !is.na(r[["DATE"]])) as.character(r[["DATE"]]) else "—"
      team_cell <- if("TEAM_LOGO"%in%names(r) && !is.na(r[["TEAM_LOGO"]]) && nchar(as.character(r[["TEAM_LOGO"]]))>0)
        sprintf("<div class='team-cell'><img class='team-logo-img' src='%s'/><span class='team-abv'>%s</span></div>", r[["TEAM_LOGO"]], r[["TEAM"]])
      else sprintf("<span class='team-abv'>%s</span>", r[["TEAM"]])
      opp_cell  <- if("OPP_LOGO"%in%names(r) && !is.na(r[["OPP_LOGO"]]) && nchar(as.character(r[["OPP_LOGO"]]))>0)
        sprintf("<div class='team-cell'><img class='team-logo-img' src='%s'/><span class='team-abv'>%s</span></div>", r[["OPP_LOGO"]], r[["OPP"]])
      else sprintf("<span class='team-abv'>%s</span>", r[["OPP"]])
      winner  <- suppressWarnings(as.numeric(r[["TEAM_WINNER"]]))
      win_dot <- if(!is.na(winner) && winner==1) "<span class='winner-dot'></span>" else ""
      sprintf("<tr><td>%s</td><td>%s</td><td>%s%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              gdate, team_cell, opp_cell, win_dot,
              sn(r,"MINS_CGS",1), sn(r,"PTS_CGS"),
              sn(r,"FGM_CGS"), sn(r,"FGA_CGS"), sp(r,"FG_PCT_CGS"),
              sn(r,"3PTM_CGS"), sn(r,"3PTA_CGS"),
              sn(r,"FTM_CGS"), sn(r,"FTA_CGS"),
              sn(r,"OREB_CGS"), sn(r,"DREB_CGS"), sn(r,"REB_CGS"),
              sn(r,"AST_CGS"), sn(r,"BLK_CGS"), sn(r,"STL_CGS"), sn(r,"FOULS_CGS"), sn(r,"TOV_CGS")
      )
    })
    
    tags$div(class="gamelog-wrap",
             HTML(sprintf("<table class='gamelog'><thead><tr><th>DATE</th><th>TEAM</th><th>OPP</th><th>MIN</th><th>PTS</th><th>FGM</th><th>FGA</th><th>FG%%</th><th>3PTM</th><th>3PTA</th><th>FTM</th><th>FTA</th><th>OREB</th><th>DREB</th><th>REB</th><th>AST</th><th>BLK</th><th>STL</th><th>PF</th><th>TOV</th></tr></thead><tbody>%s</tbody></table>",
                          paste(rows_html, collapse="")))
    )
  })
  # ════════════════════════════════════════════════════════════════════════════
  # SCHEDULE VIEWER — MAIN CALENDAR + SIDE PANE
  # ════════════════════════════════════════════════════════════════════════════
  
  output$schedule_viewer_ui <- renderUI({
    tags$div(class="schedule-layout",
             # ── Calendar ──
             tags$div(class="calendar-wrap",
                      tags$div(class="calendar-nav",
                               tags$button(class="cal-nav-btn", id="cal-prev-btn", onclick="Shiny.setInputValue('cal_prev', Math.random())", "‹"),
                               tags$span(class="cal-month-label", textOutput("cal_month_label", inline=TRUE)),
                               tags$button(class="cal-nav-btn", id="cal-next-btn", onclick="Shiny.setInputValue('cal_next', Math.random())", "›"),
                               tags$button(class="cal-today-btn", onclick="Shiny.setInputValue('cal_today', Math.random())", "TODAY")
                      ),
                      uiOutput("calendar_grid_ui")
             ),
             # ── Side pane ──
             tags$div(class="schedule-pane", id="schedule-pane",
                      tags$div(class="pane-header",
                               tags$span(class="pane-date-label", textOutput("pane_date_label", inline=TRUE)),
                               tags$button(class="pane-close", onclick="Shiny.setInputValue('close_pane', Math.random())", "×")
                      ),
                      tags$div(class="pane-games", uiOutput("pane_games_ui"))
             )
    )
  })
  
  output$cal_month_label <- renderText({
    format(as.Date(paste(cal_year(), cal_month(), "01", sep="-")), "%B %Y")
  })
  
  output$calendar_grid_ui <- renderUI({
    y <- cal_year(); m <- cal_month()
    first_day  <- as.Date(paste(y, m, "01", sep="-"))
    last_day   <- as.Date(paste(ifelse(m==12,y+1,y), ifelse(m==12,1,m+1), "01", sep="-")) - 1
    today      <- Sys.Date()
    
    # Days of week offset (0=Sun)
    start_dow <- as.integer(format(first_day, "%w"))
    # All cells to show
    cal_start <- first_day - start_dow
    cal_end   <- cal_start + 41  # 6 weeks
    
    # Games for this month range
    month_games <- schedule_data %>%
      filter(game_date >= cal_start & game_date <= cal_end) %>%
      arrange(game_date, game_id)
    
    # Deduplicate — one row per game_id (pick home team row for display)
    month_games <- month_games %>%
      group_by(game_id) %>%
      slice(which.max(ifelse(is_home == 1, 1, 0))) %>%
      ungroup()
    
    dow_headers <- c("SUN","MON","TUE","WED","THU","FRI","SAT")
    header_cells <- lapply(dow_headers, function(d) tags$div(class="cal-dow", d))
    
    day_cells <- lapply(0:41, function(i) {
      cell_date  <- cal_start + i
      cell_day   <- as.integer(format(cell_date, "%d"))
      is_cur_mon <- format(cell_date, "%Y-%m") == format(first_day, "%Y-%m")
      is_today   <- cell_date == today
      cell_class <- paste("cal-cell",
                          if(!is_cur_mon) "other-month" else "",
                          if(is_today) "today" else "")
      date_str <- as.character(cell_date)
      
      date_num <- tags$span(class="cal-date-num",
                            onclick=sprintf("Shiny.setInputValue('cal_date_click','%s',{priority:'event'})", date_str),
                            cell_day)
      
      # Games on this date
      day_g <- month_games[month_games$game_date == cell_date, ]
      max_show <- 3
      game_pills <- if(nrow(day_g) > 0) {
        shown <- min(nrow(day_g), max_show)
        pills <- lapply(seq_len(shown), function(gi) {
          g <- day_g[gi,]
          tl <- if(!is.na(g$team_logo) && nchar(g$team_logo)>0) sprintf("<img src='%s'/>", g$team_logo) else ""
          ol <- if(!is.na(g$opp_logo)  && nchar(g$opp_logo)>0)  sprintf("<img src='%s'/>", g$opp_logo)  else ""
          sym <- if(!is.na(g$home_away_sym)) trimws(g$home_away_sym) else "vs"
          gid <- as.character(g$game_id)
          HTML(sprintf("<div class='cal-game' onclick=\"Shiny.setInputValue('jump_to_game','%s',{priority:'event'})\">%s<span class='cal-game-teams'>%s</span><span class='cal-game-sym'>%s</span>%s</div>",
                       gid, tl, g$team, sym, ol))
        })
        if(nrow(day_g) > max_show)
          pills <- c(pills, list(tags$div(class="cal-more", sprintf("+%d more", nrow(day_g)-max_show))))
        pills
      } else list()
      
      tags$div(class=trimws(cell_class), date_num, tagList(game_pills))
    })
    
    tags$div(class="cal-grid", tagList(header_cells), tagList(day_cells))
  })
  
  output$pane_date_label <- renderText({
    d <- cal_pane_date()
    if(is.null(d)) return("")
    format(d, "%A, %B %d %Y")
  })
  
  output$pane_games_ui <- renderUI({
    d <- cal_pane_date()
    if(is.null(d)) return(NULL)
    
    day_games <- schedule_data %>%
      filter(game_date == d) %>%
      arrange(game_id)
    
    if(nrow(day_games)==0)
      return(tags$div(style="padding:20px;opacity:.4;font-family:'Share Tech Mono',monospace;font-size:11px;color:var(--text-muted);text-align:center;","No games on this date"))
    
    # ── NEW: Get live scores if viewing today ──
    is_today <- (d == Sys.Date())
    live_df  <- if (is_today) live_scores() else NULL
    
    # Build a lookup by team tricode for fast matching
    # Map your schedule_data team abbreviations to NBA tricodes
    live_lookup <- list()
    if (!is.null(live_df) && nrow(live_df) > 0) {
      for (i in seq_len(nrow(live_df))) {
        key <- paste0(sort(c(live_df$away_tri[i], live_df$home_tri[i])), collapse = "_")
        live_lookup[[key]] <- live_df[i, ]
      }
    }
    
    game_ids <- unique(day_games$game_id)
    cards <- lapply(game_ids, function(gid) {
      rows <- day_games[day_games$game_id == gid, ]
      home_r <- rows[isTRUE(rows$is_home[1]==1) | rows$home_away_sym[1]=="vs.", ][1,]
      away_r <- rows[isTRUE(rows$is_away[1]==1) | rows$home_away_sym[1]=="@", ][1,]
      if(is.na(home_r$team[1])) home_r <- rows[1,]
      if(is.na(away_r$team[1])) away_r <- rows[min(2,nrow(rows)),]
      
      # ── NEW: Try to match with live data ──
      home_abv <- toupper(trimws(as.character(home_r$team[1])))
      away_abv <- toupper(trimws(as.character(away_r$team[1])))
      match_key <- paste0(sort(c(away_abv, home_abv)), collapse = "_")
      live_game <- live_lookup[[match_key]]
      
      has_live   <- !is.null(live_game)
      
      if (has_live) {
        # Use live data for scores and status
        home_score_val <- live_game$home_score
        away_score_val <- live_game$away_score
        game_status    <- live_game$game_status
        status_text    <- format_game_clock(live_game$game_clock, live_game$period, game_status)
        has_score      <- (game_status >= 2)  # live or final
        home_won       <- (game_status == 3 && home_score_val > away_score_val)
        away_won       <- (game_status == 3 && away_score_val > home_score_val)
        is_live        <- (game_status == 2)
      } else {
        # Fall back to schedule_data
        home_score_val <- home_r$team_score[1]
        away_score_val <- away_r$team_score[1]
        has_score      <- !is.na(home_score_val) && !is.na(away_score_val)
        home_won       <- isTRUE(home_r$team_winner[1] == TRUE)
        away_won       <- isTRUE(away_r$team_winner[1] == TRUE)
        is_live        <- FALSE
        status_text    <- NULL
      }
      
      home_logo <- if(!is.na(home_r$team_logo[1]) && nchar(home_r$team_logo[1])>0) tags$img(src=home_r$team_logo[1]) else NULL
      away_logo <- if(!is.na(away_r$team_logo[1]) && nchar(away_r$team_logo[1])>0) tags$img(src=away_r$team_logo[1]) else NULL
      
      win_dot <- tags$span(class="pane-winner-dot")
      
      stype_label <- switch(as.character(rows$season_type[1]), "1"="PRESEASON","2"="REGULAR","3"="PLAYOFFS","—")
      
      # ── NEW: Build the status / meta line ──
      meta_content <- if (has_live && is_live) {
        # Live game — show pulsing dot + clock
        tags$div(class="pane-meta", style="display:flex;align-items:center;gap:6px;",
                 tags$span(style="display:inline-block;width:7px;height:7px;border-radius:50%;background:#e53935;animation:pulse 1.5s infinite;"),
                 tags$span(status_text)
        )
      } else if (has_live && !is.null(status_text)) {
        tags$div(class="pane-meta", status_text)
      } else {
        tags$div(class="pane-meta", stype_label)
      }
      
      tags$div(class="pane-game-card",
               onclick=sprintf("Shiny.setInputValue('jump_to_game','%s',{priority:'event'})", as.character(gid)),
               tags$div(class="pane-matchup",
                        tags$div(class=paste("pane-team", if(has_score && !away_won) "loser" else ""),
                                 away_logo,
                                 tags$span(class="pane-team-name", away_r$team[1]),
                                 if(away_won) win_dot else NULL
                        ),
                        if(has_score) tags$div(class="pane-score",
                                               sprintf("%s - %s", away_score_val, home_score_val))
                        else tags$div(class="pane-sym", "@"),
                        tags$div(class=paste("pane-team", if(has_score && !home_won) "loser" else ""),
                                 home_logo,
                                 tags$span(class="pane-team-name", home_r$team[1]),
                                 if(home_won) win_dot else NULL
                        )
               ),
               meta_content
      )
    })
    
    tagList(cards)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # BETTING VIEW
  # ════════════════════════════════════════════════════════════════════════════
  
  BET_HISTORY_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/14. Bet History/bet_history_2025_2026.csv"
  STARTING_BANKROLL <- 500
  
  BET_COLS <- c("bet_id","date","session_id","game_id","bet_type","team_or_player",
                "line","odds","stake","kelly_stake","result","profit_loss","bankroll_after","notes")
  
  load_bets <- function() {
    if (!file.exists(BET_HISTORY_PATH)) {
      df <- as.data.frame(matrix(ncol=length(BET_COLS), nrow=0))
      names(df) <- BET_COLS
      return(df)
    }
    df <- read.csv(BET_HISTORY_PATH, stringsAsFactors=FALSE)
    # Ensure all columns exist
    for (col in BET_COLS) if (!col %in% names(df)) df[[col]] <- NA
    df
  }
  
  save_bets <- function(df) {
    dir.create(dirname(BET_HISTORY_PATH), showWarnings=FALSE, recursive=TRUE)
    write.csv(df, BET_HISTORY_PATH, row.names=FALSE)
  }
  
  # Auto-resolve pending bets against team_data / player_data
  auto_resolve <- function(df) {
    if (nrow(df)==0) return(df)
    pending <- which(df$result == "Pending" & !is.na(df$game_id) & df$game_id != "")
    if (length(pending)==0) return(df)
    for (i in pending) {
      gid  <- as.character(df$game_id[i])
      btype <- tolower(as.character(df$bet_type[i]))
      t_game <- team_data %>% filter(as.character(ESPN_GAME_ID) == gid)
      if (nrow(t_game)==0) next  # game not yet in data
      # Moneyline
      if (grepl("moneyline|ml", btype)) {
        team <- as.character(df$team_or_player[i])
        tr <- t_game %>% filter(TEAM == team)
        if (nrow(tr)==0) next
        won <- isTRUE(tr$TEAM_WINNER[1] == 1)
        df$result[i] <- if(won) "W" else "L"
      }
      # Spread
      else if (grepl("spread", btype)) {
        team <- as.character(df$team_or_player[i])
        line_val <- suppressWarnings(as.numeric(df$line[i]))
        rows <- get_away_home(t_game)
        team_r <- if(rows$away$TEAM[1]==team) rows$away else rows$home
        opp_r  <- if(rows$away$TEAM[1]==team) rows$home else rows$away
        if(is.na(line_val)) next
        margin <- suppressWarnings(as.numeric(team_r$PTS_CGS[1])) - suppressWarnings(as.numeric(opp_r$PTS_CGS[1]))
        if(is.na(margin)) next
        covered <- (margin + line_val) > 0
        push    <- (margin + line_val) == 0
        df$result[i] <- if(push) "P" else if(covered) "W" else "L"
      }
      # Total
      else if (grepl("total|over|under", btype)) {
        line_val <- suppressWarnings(as.numeric(df$line[i]))
        if(is.na(line_val)) next
        pts <- sum(suppressWarnings(as.numeric(t_game$PTS_CGS)), na.rm=TRUE)
        is_over <- grepl("over", tolower(as.character(df$team_or_player[i])))
        push <- pts == line_val
        hit  <- if(is_over) pts > line_val else pts < line_val
        df$result[i] <- if(push) "P" else if(hit) "W" else "L"
      }
      # Player prop
      else if (grepl("player|prop|pts|reb|ast", btype)) {
        player <- as.character(df$team_or_player[i])
        line_val <- suppressWarnings(as.numeric(df$line[i]))
        if(is.na(line_val)) next
        stat_col <- if(grepl("reb",btype)) "REB_CGS" else if(grepl("ast",btype)) "AST_CGS" else "PTS_CGS"
        p_game <- player_data %>% filter(PLAYER_NAME == player, as.character(ESPN_GAME_ID) == gid)
        if(nrow(p_game)==0) next
        actual <- suppressWarnings(as.numeric(p_game[[stat_col]][1]))
        if(is.na(actual)) next
        is_over <- grepl("over", tolower(as.character(df$team_or_player[i])))
        push <- actual == line_val
        hit  <- if(is_over) actual > line_val else actual < line_val
        df$result[i] <- if(push) "P" else if(hit) "W" else "L"
      }
      # Calculate P&L
      if (df$result[i] %in% c("W","L","P")) {
        odds_val  <- suppressWarnings(as.numeric(df$odds[i]))
        stake_val <- suppressWarnings(as.numeric(df$stake[i]))
        if (!is.na(odds_val) && !is.na(stake_val)) {
          pnl <- if(df$result[i]=="P") 0
          else if(df$result[i]=="W") {
            if(odds_val>0) stake_val*(odds_val/100) else stake_val*(100/abs(odds_val))
          } else -stake_val
          df$profit_loss[i] <- round(pnl, 2)
        }
      }
    }
    # Recalculate bankroll_after sequentially
    df <- df %>% arrange(date, bet_id)
    running <- STARTING_BANKROLL
    for (i in seq_len(nrow(df))) {
      pl <- suppressWarnings(as.numeric(df$profit_loss[i]))
      if (!is.na(pl)) running <- running + pl
      df$bankroll_after[i] <- round(running, 2)
    }
    df
  }
  
  # Reactive bet data
  bets_rv       <- reactiveVal(auto_resolve(load_bets()))
  show_bet_modal  <- reactiveVal(FALSE)
  show_bankroll_modal <- reactiveVal(FALSE)
  bankroll_modal_type <- reactiveVal("add")  # "add" or "subtract"
  bv_cal_year   <- reactiveVal(as.integer(format(Sys.Date(),"%Y")))
  bv_cal_month  <- reactiveVal(as.integer(format(Sys.Date(),"%m")))
  
  # Refresh / auto-resolve
  observeEvent(input$bv_refresh, {
    df <- auto_resolve(load_bets())
    save_bets(df)
    bets_rv(df)
  })
  
  # Bankroll modal
  observeEvent(input$bv_bankroll_add,      { bankroll_modal_type("add");      show_bankroll_modal(TRUE) })
  observeEvent(input$bv_bankroll_subtract, { bankroll_modal_type("subtract"); show_bankroll_modal(TRUE) })
  observeEvent(input$bv_bankroll_cancel,   { show_bankroll_modal(FALSE) })
  observeEvent(input$bv_bankroll_confirm, {
    amt <- suppressWarnings(as.numeric(input$bv_bankroll_amount))
    if (!is.na(amt) && amt > 0) {
      df <- bets_rv()
      # Add a deposit/withdrawal row
      new_row <- data.frame(
        bet_id=paste0("ADJ_", format(Sys.time(),"%Y%m%d%H%M%S")),
        date=as.character(Sys.Date()), session_id=as.character(Sys.Date()),
        game_id="", bet_type=if(bankroll_modal_type()=="add") "Deposit" else "Withdrawal",
        team_or_player="", line=NA, odds=NA,
        stake=if(bankroll_modal_type()=="add") amt else -amt,
        kelly_stake=NA,
        result="W",
        profit_loss=if(bankroll_modal_type()=="add") amt else -amt,
        bankroll_after=NA, notes="Manual adjustment",
        stringsAsFactors=FALSE
      )
      df <- bind_rows(df, new_row)
      # Recalculate bankroll
      df <- df %>% arrange(date, bet_id)
      running <- STARTING_BANKROLL
      for (i in seq_len(nrow(df))) {
        pl <- suppressWarnings(as.numeric(df$profit_loss[i]))
        if (!is.na(pl)) running <- running + pl
        df$bankroll_after[i] <- round(running, 2)
      }
      save_bets(df); bets_rv(df)
    }
    show_bankroll_modal(FALSE)
  })
  
  # Bet entry modal
  observeEvent(input$bv_add_bet,    { show_bet_modal(TRUE) })
  observeEvent(input$bv_bet_cancel, { show_bet_modal(FALSE) })
  observeEvent(input$bv_bet_submit, {
    stake_val <- suppressWarnings(as.numeric(input$bv_stake))
    odds_val  <- suppressWarnings(as.numeric(input$bv_odds))
    if (is.na(stake_val) || is.na(odds_val)) return()
    df <- bets_rv()
    new_id <- paste0("BET_", format(Sys.time(),"%Y%m%d%H%M%S"))
    new_row <- data.frame(
      bet_id=new_id,
      date=as.character(Sys.Date()),
      session_id=as.character(Sys.Date()),
      game_id=as.character(input$bv_game_id),
      bet_type=as.character(input$bv_bet_type),
      team_or_player=as.character(input$bv_team_or_player),
      line=suppressWarnings(as.numeric(input$bv_line)),
      odds=odds_val, stake=stake_val,
      kelly_stake=suppressWarnings(as.numeric(input$bv_kelly_display)),
      result="Pending", profit_loss=NA, bankroll_after=NA,
      notes=as.character(input$bv_notes),
      stringsAsFactors=FALSE
    )
    df <- bind_rows(df, new_row)
    df <- df %>% arrange(date, bet_id)
    running <- STARTING_BANKROLL
    for (i in seq_len(nrow(df))) {
      pl <- suppressWarnings(as.numeric(df$profit_loss[i]))
      if (!is.na(pl)) running <- running + pl
      df$bankroll_after[i] <- round(running, 2)
    }
    save_bets(df); bets_rv(df)
    show_bet_modal(FALSE)
  })
  
  # Calendar nav
  observeEvent(input$bv_cal_prev, { m <- bv_cal_month(); y <- bv_cal_year(); if(m==1){bv_cal_month(12);bv_cal_year(y-1)}else bv_cal_month(m-1) })
  observeEvent(input$bv_cal_next, { m <- bv_cal_month(); y <- bv_cal_year(); if(m==12){bv_cal_month(1);bv_cal_year(y+1)}else bv_cal_month(m+1) })
  
  # Kelly calc reactive
  kelly_stake_rv <- reactive({
    prob <- suppressWarnings(as.numeric(input$bv_win_prob))
    odds <- suppressWarnings(as.numeric(input$bv_odds))
    bank <- {
      df <- bets_rv()
      if(nrow(df)>0 && !is.na(df$bankroll_after[nrow(df)])) df$bankroll_after[nrow(df)] else STARTING_BANKROLL
    }
    if (is.na(prob) || is.na(odds) || prob<=0 || prob>=1) return(NA)
    b <- if(odds>0) odds/100 else 100/abs(odds)
    q <- 1 - prob
    f <- (b*prob - q) / b
    f <- max(0, min(f, 0.25))  # cap at 25% of bankroll
    round(f * bank, 2)
  })
  
  # Main betting view UI
  output$betting_view_ui <- renderUI({
    df   <- bets_rv()
    bank <- if(nrow(df)>0 && !is.na(df$bankroll_after[nrow(df)])) df$bankroll_after[nrow(df)] else STARTING_BANKROLL
    completed <- df %>% filter(result %in% c("W","L","P"))
    earnings  <- sum(suppressWarnings(as.numeric(completed$profit_loss)), na.rm=TRUE)
    wins      <- sum(completed$result=="W")
    losses    <- sum(completed$result=="L")
    total_settled <- wins + losses
    win_rate  <- if(total_settled>0) paste0(round(wins/total_settled*100,1),"%") else "—"
    sessions  <- length(unique(df$session_id[df$session_id!=""]))
    win_sess  <- length(unique(df$session_id[df$result=="W" & df$session_id!=""]))
    loss_sess <- length(unique(df$session_id[df$result=="L" & df$session_id!=""]))
    avg_earn  <- if(sessions>0) round(earnings/sessions,2) else 0
    roi       <- if(nrow(df)>0) { total_staked <- sum(suppressWarnings(as.numeric(df$stake)),na.rm=TRUE); if(total_staked>0) paste0(round(earnings/total_staked*100,1),"%") else "—" } else "—"
    
    earn_cls  <- if(earnings>=0) "positive" else "negative"
    earn_lbl  <- if(earnings>=0) paste0("+$",round(earnings,2)) else paste0("-$",abs(round(earnings,2)))
    
    tagList(
      # ── Modals ──
      if(show_bankroll_modal()) {
        tags$div(class="bv-modal-overlay",
                 tags$div(class="bv-modal",
                          tags$div(class="bv-modal-title", if(bankroll_modal_type()=="add") "Add to Bankroll" else "Subtract from Bankroll"),
                          tags$div(class="bv-form-group",
                                   tags$div(class="bv-form-label", "Amount ($)"),
                                   tags$input(type="number", id="bv_bankroll_amount", class="bv-form-input", placeholder="0.00", min="0", step="0.01",
                                              oninput="Shiny.setInputValue('bv_bankroll_amount', this.value)")
                          ),
                          tags$div(class="bv-modal-btns",
                                   tags$button(class="bv-modal-cancel", onclick="Shiny.setInputValue('bv_bankroll_cancel', Math.random())", "Cancel"),
                                   tags$button(class="bv-modal-submit", onclick="Shiny.setInputValue('bv_bankroll_confirm', Math.random())", "Confirm")
                          )
                 )
        )
      },
      if(show_bet_modal()) {
        game_choices <- c("No Game"="", sort(unique(paste(schedule_data$game_date, schedule_data$team, "vs", schedule_data$opp))))
        game_ids     <- c("", schedule_data$game_id[order(schedule_data$game_date, schedule_data$team)])
        player_list  <- sort(unique(player_data$PLAYER_NAME))
        team_list    <- sort(unique(team_data$TEAM))
        ks <- kelly_stake_rv()
        tags$div(class="bv-modal-overlay",
                 tags$div(class="bv-modal",
                          tags$div(class="bv-modal-title", "Log New Bet"),
                          tags$div(class="bv-form-row",
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Bet Type"),
                                            tags$select(class="bv-form-select", id="bv_bet_type",
                                                        onchange="Shiny.setInputValue('bv_bet_type', this.value)",
                                                        tags$option(value="Moneyline","Moneyline"), tags$option(value="Spread","Spread"),
                                                        tags$option(value="Total Over","Total Over"), tags$option(value="Total Under","Total Under"),
                                                        tags$option(value="Player PTS","Player PTS"), tags$option(value="Player REB","Player REB"),
                                                        tags$option(value="Player AST","Player AST")
                                            )
                                   ),
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Game ID (ESPN)"),
                                            tags$input(type="text", id="bv_game_id", class="bv-form-input", placeholder="e.g. 401810696",
                                                       oninput="Shiny.setInputValue('bv_game_id', this.value)")
                                   )
                          ),
                          tags$div(class="bv-form-row",
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Team / Player"),
                                            tags$input(type="text", id="bv_team_or_player", class="bv-form-input", placeholder="e.g. BOS or LeBron James",
                                                       oninput="Shiny.setInputValue('bv_team_or_player', this.value)")
                                   ),
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Line"),
                                            tags$input(type="number", id="bv_line", class="bv-form-input", placeholder="e.g. -5.5 or 224.5",
                                                       oninput="Shiny.setInputValue('bv_line', this.value)")
                                   )
                          ),
                          tags$div(class="bv-form-row",
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Odds (American)"),
                                            tags$input(type="number", id="bv_odds", class="bv-form-input", placeholder="e.g. -110 or +150",
                                                       oninput="Shiny.setInputValue('bv_odds', this.value)")
                                   ),
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Win Probability (0-1)"),
                                            tags$input(type="number", id="bv_win_prob", class="bv-form-input", placeholder="e.g. 0.55", min="0", max="1", step="0.01",
                                                       oninput="Shiny.setInputValue('bv_win_prob', this.value)")
                                   )
                          ),
                          tags$div(class="bv-form-row",
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Stake ($)"),
                                            tags$input(type="number", id="bv_stake", class="bv-form-input", placeholder="0.00", min="0", step="0.01",
                                                       oninput="Shiny.setInputValue('bv_stake', this.value)")
                                   ),
                                   tags$div(class="bv-form-group",
                                            tags$div(class="bv-form-label","Kelly Suggested Stake"),
                                            tags$div(class="bv-kelly-hint", if(!is.na(ks)) paste0("$", ks) else "Enter odds & probability"),
                                            tags$input(type="hidden", id="bv_kelly_display", value=if(!is.na(ks)) as.character(ks) else "")
                                   )
                          ),
                          tags$div(class="bv-form-group full",
                                   tags$div(class="bv-form-label","Notes"),
                                   tags$input(type="text", id="bv_notes", class="bv-form-input", placeholder="Optional notes",
                                              oninput="Shiny.setInputValue('bv_notes', this.value)")
                          ),
                          tags$div(class="bv-modal-btns",
                                   tags$button(class="bv-modal-cancel", onclick="Shiny.setInputValue('bv_bet_cancel', Math.random())", "Cancel"),
                                   tags$button(class="bv-modal-submit", onclick="Shiny.setInputValue('bv_bet_submit', Math.random())", "Log Bet")
                          )
                 )
        )
      },
      
      # ── Main layout ──
      tags$div(class="bv-layout",
               
               # ── Top bar ──
               tags$div(class="bv-top-bar",
                        tags$div(class="bv-bankroll-block",
                                 tags$button(class="bv-bankroll-btn", onclick="Shiny.setInputValue('bv_bankroll_subtract', Math.random())", "−"),
                                 tags$div(class="bv-bankroll-center",
                                          tags$div(class="bv-bankroll-lbl", "Bankroll"),
                                          tags$div(class="bv-bankroll-val", sprintf("$%s", formatC(bank, format="f", digits=2, big.mark=",")))
                                 ),
                                 tags$button(class="bv-bankroll-btn", onclick="Shiny.setInputValue('bv_bankroll_add', Math.random())", "+")
                        ),
                        tags$button(class="bv-action-btn primary", onclick="Shiny.setInputValue('bv_add_bet', Math.random())", "+ Log Bet"),
                        tags$button(class="bv-action-btn", onclick="Shiny.setInputValue('bv_refresh', Math.random())", "↻ Resolve Pending")
               ),
               
               # ── Stats row ──
               tags$div(class="bv-stats-row",
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Earnings"), tags$div(class=paste("bv-stat-val",earn_cls), earn_lbl)),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Win Rate"),  tags$div(class="bv-stat-val", win_rate)),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","ROI"),       tags$div(class="bv-stat-val", roi)),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Sessions"),  tags$div(class="bv-stat-val", sessions)),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Avg / Session"), tags$div(class=paste("bv-stat-val", if(avg_earn>=0)"positive"else"negative"), if(avg_earn>=0) paste0("+$",avg_earn) else paste0("-$",abs(avg_earn)))),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Winning Sessions"), tags$div(class="bv-stat-val positive", win_sess)),
                        tags$div(class="bv-stat-card", tags$div(class="bv-stat-lbl","Losing Sessions"),  tags$div(class="bv-stat-val negative", loss_sess))
               ),
               
               # ── Charts row ──
               tags$div(class="bv-charts-row",
                        tags$div(class="bv-chart-card",
                                 tags$div(class="bv-chart-title", "Bankroll Over Time"),
                                 uiOutput("bv_bankroll_chart")
                        ),
                        tags$div(class="bv-chart-card",
                                 tags$div(class="bv-chart-title", "Bets by Type"),
                                 uiOutput("bv_pie_chart")
                        )
               ),
               
               # ── Bottom row: P&L Calendar + Bet History ──
               tags$div(class="bv-bottom-row",
                        # P&L Calendar
                        tags$div(class="bv-chart-card",
                                 tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:12px;",
                                          tags$div(class="bv-section-title", textOutput("bv_cal_label", inline=TRUE)),
                                          tags$button(class="cal-nav-btn", style="margin-left:auto;", onclick="Shiny.setInputValue('bv_cal_prev',Math.random())", "‹"),
                                          tags$button(class="cal-nav-btn", onclick="Shiny.setInputValue('bv_cal_next',Math.random())", "›")
                                 ),
                                 uiOutput("bv_pnl_calendar")
                        ),
                        # Bet history table
                        tags$div(class="bv-chart-card",
                                 tags$div(class="bv-section-title", "Recent Bets"),
                                 tags$div(class="bv-bet-table-wrap", uiOutput("bv_bet_table"))
                        )
               )
      )
    )
  })
  
  output$bv_cal_label <- renderText({
    format(as.Date(paste(bv_cal_year(), bv_cal_month(), "01", sep="-")), "%B %Y")
  })
  
  output$bv_bankroll_chart <- renderUI({
    df <- bets_rv()
    if (nrow(df)==0) return(tags$div(style="height:160px;display:flex;align-items:center;justify-content:center;opacity:.3;font-family:'Share Tech Mono',monospace;font-size:11px;color:var(--text-muted);","No data yet"))
    pts <- df %>% filter(!is.na(bankroll_after)) %>% arrange(date, bet_id)
    if (nrow(pts)==0) return(NULL)
    all_y <- c(STARTING_BANKROLL, pts$bankroll_after)
    mn <- min(all_y); mx <- max(all_y); rng <- max(mx-mn, 1)
    w <- 600; h <- 150; pad_l <- 50; pad_r <- 10; pad_t <- 10; pad_b <- 20
    n <- nrow(pts)
    xs <- if(n==1) pad_l else pad_l + (seq_len(n)-1) / (n-1) * (w-pad_l-pad_r)
    ys <- pad_t + (1 - (pts$bankroll_after - mn)/rng) * (h-pad_t-pad_b)
    path_d <- paste0("M ", paste(round(xs,1), round(ys,1), sep=",", collapse=" L "))
    fill_d <- paste0("M ", pad_l, ",", h-pad_b, " L ", paste(round(xs,1), round(ys,1), sep=",", collapse=" L "), " L ", round(tail(xs,1),1), ",", h-pad_b, " Z")
    y_mid <- pad_t + (h-pad_t-pad_b)/2
    tags$div(style="overflow:hidden;",
             HTML(sprintf('<svg viewBox="0 0 %d %d" style="width:100%%;height:160px;">
        <defs><linearGradient id="bvgrad" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#3a8a3a" stop-opacity="0.3"/><stop offset="1" stop-color="#3a8a3a" stop-opacity="0.02"/></linearGradient></defs>
        <text x="44" y="%s" font-family="Share Tech Mono" font-size="9" fill="#4a4a58" text-anchor="end">$%s</text>
        <text x="44" y="%s" font-family="Share Tech Mono" font-size="9" fill="#4a4a58" text-anchor="end">$%s</text>
        <line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#222228" stroke-width="0.5"/>
        <path d="%s" fill="url(#bvgrad)"/>
        <path d="%s" fill="none" stroke="#3a8a3a" stroke-width="1.5"/>
      </svg>', w, h,
                          round(pad_t+5), round(mx,0),
                          round(h-pad_b-2), round(mn,0),
                          pad_l, pad_t, pad_l, h-pad_b,
                          fill_d, path_d))
    )
  })
  
  output$bv_pie_chart <- renderUI({
    df <- bets_rv()
    if (nrow(df)==0 || !any(df$bet_type %in% c("Moneyline","Spread","Total Over","Total Under","Player PTS","Player REB","Player AST")))
      return(tags$div(style="height:160px;display:flex;align-items:center;justify-content:center;opacity:.3;font-family:'Share Tech Mono',monospace;font-size:11px;color:var(--text-muted);","No data yet"))
    type_counts <- df %>% filter(bet_type %in% c("Moneyline","Spread","Total Over","Total Under","Player PTS","Player REB","Player AST")) %>%
      group_by(bet_type) %>% summarise(n=n(), .groups="drop")
    total <- sum(type_counts$n)
    colors <- c("Moneyline"="#4a7a9a","Spread"="#7a4a9a","Total Over"="#4a9a7a","Total Under"="#3a6a5a","Player PTS"="#9a7a4a","Player REB"="#9a4a6a","Player AST"="#6a4a9a")
    cx <- 80; cy <- 75; r <- 60
    angle <- -pi/2
    slices <- ""
    for (i in seq_len(nrow(type_counts))) {
      pct   <- type_counts$n[i] / total
      sweep <- pct * 2 * pi
      x1 <- cx + r*cos(angle); y1 <- cy + r*sin(angle)
      angle2 <- angle + sweep
      x2 <- cx + r*cos(angle2); y2 <- cy + r*sin(angle2)
      large <- if(sweep > pi) 1 else 0
      col <- colors[type_counts$bet_type[i]]
      if(is.na(col)) col <- "#4a4a58"
      slices <- paste0(slices, sprintf('<path d="M %s,%s L %s,%s A %s,%s 0 %s,1 %s,%s Z" fill="%s" stroke="var(--bg-card)" stroke-width="1.5"/>',
                                       cx,cy, round(x1,2),round(y1,2), r,r, large, round(x2,2),round(y2,2), col))
      angle <- angle2
    }
    legend <- paste(sapply(seq_len(nrow(type_counts)), function(i) {
      col <- colors[type_counts$bet_type[i]]; if(is.na(col)) col <- "#4a4a58"
      sprintf('<g transform="translate(170,%s)"><rect width="8" height="8" fill="%s" rx="1"/><text x="12" y="8" font-family="Share Tech Mono" font-size="8" fill="#7a7a8a">%s (%s)</text></g>',
              20 + (i-1)*16, col, type_counts$bet_type[i], type_counts$n[i])
    }), collapse="")
    tags$div(HTML(sprintf('<svg viewBox="0 0 320 150" style="width:100%%;height:160px;">%s%s</svg>', slices, legend)))
  })
  
  output$bv_pnl_calendar <- renderUI({
    df <- bets_rv()
    y <- bv_cal_year(); m <- bv_cal_month()
    first_day <- as.Date(paste(y,m,"01",sep="-"))
    start_dow <- as.integer(format(first_day,"%w"))
    cal_start <- first_day - start_dow
    today <- Sys.Date()
    # Daily P&L
    daily <- df %>% filter(!is.na(profit_loss), result %in% c("W","L","P")) %>%
      mutate(date=as.Date(date)) %>%
      group_by(date) %>% summarise(pnl=sum(as.numeric(profit_loss), na.rm=TRUE), .groups="drop")
    dows <- lapply(c("SUN","MON","TUE","WED","THU","FRI","SAT"), function(d) tags$div(class="bv-cal-dow", d))
    cells <- lapply(0:41, function(i) {
      cd <- cal_start + i
      is_cur <- format(cd,"%Y-%m") == format(first_day,"%Y-%m")
      day_pnl <- daily$pnl[daily$date==cd]
      has_bets <- length(day_pnl)>0
      pnl_val  <- if(has_bets) day_pnl[1] else 0
      cell_cls <- paste("bv-cal-cell",
                        if(has_bets && pnl_val>0) "win-day" else if(has_bets && pnl_val<0) "loss-day" else if(has_bets) "has-bets" else "",
                        if(cd==today) "today-day" else "")
      pnl_cls  <- if(pnl_val>0) "pos" else if(pnl_val<0) "neg" else "zero"
      pnl_lbl  <- if(has_bets) {
        if(pnl_val>=0) paste0("+$",round(pnl_val,2)) else paste0("-$",abs(round(pnl_val,2)))
      } else ""
      tags$div(class=trimws(cell_cls), style=if(!is_cur)"opacity:.35;"else"",
               tags$div(class="bv-cal-date", format(cd,"%d")),
               if(nchar(pnl_lbl)>0) tags$div(class=paste("bv-cal-pnl",pnl_cls), pnl_lbl) else NULL
      )
    })
    tags$div(class="bv-cal-grid", tagList(dows), tagList(cells))
  })
  
  output$bv_bet_table <- renderUI({
    df <- bets_rv() %>% arrange(desc(date), desc(bet_id)) %>% head(20)
    if (nrow(df)==0) return(tags$div(style="padding:20px;opacity:.4;font-family:'Share Tech Mono',monospace;font-size:11px;color:var(--text-muted);","No bets logged yet"))
    rows <- paste(sapply(seq_len(nrow(df)), function(i) {
      r <- df[i,]
      res <- as.character(r$result)
      res_cls <- switch(res, W="bet-result-W", L="bet-result-L", P="bet-result-P", "bet-result-pending")
      pl <- suppressWarnings(as.numeric(r$profit_loss))
      pl_str <- if(is.na(pl)) "—" else if(pl>=0) paste0("+$",round(pl,2)) else paste0("-$",abs(round(pl,2)))
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td></tr>",
              as.character(r$date), as.character(r$bet_type), as.character(r$team_or_player),
              if(is.na(r$line)||r$line=="NA") "—" else r$line,
              as.character(r$odds), paste0("$",r$stake),
              res_cls, res, pl_str)
    }), collapse="")
    HTML(sprintf("<table class='bv-bet-table'><thead><tr><th>Date</th><th>Type</th><th>Team/Player</th><th>Line</th><th>Odds</th><th>Stake</th><th>Result</th><th>P&L</th></tr></thead><tbody>%s</tbody></table>", rows))
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # PLAYER ROTATIONS
  # ════════════════════════════════════════════════════════════════════════════
  
  # All 30 teams with ESPN CDN logos
  NBA_TEAMS <- data.frame(
    abv  = c("ATL","BKN","BOS","CHA","CHI","CLE","DAL","DEN","DET","GSW",
             "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
             "OKC","ORL","PHI","PHX","POR","SAC","SAS","TOR","UTA","WAS"),
    id   = c(1610612737,1610612751,1610612738,1610612766,1610612741,1610612739,
             1610612742,1610612743,1610612765,1610612744,1610612745,1610612754,
             1610612746,1610612747,1610612763,1610612748,1610612749,1610612750,
             1610612740,1610612752,1610612760,1610612753,1610612755,1610612756,
             1610612757,1610612758,1610612759,1610612761,1610612762,1610612764),
    stringsAsFactors=FALSE
  )
  NBA_TEAMS$logo <- paste0("https://a.espncdn.com/i/teamlogos/nba/500/", tolower(NBA_TEAMS$abv), ".png")
  
  # Reactive state
  pr_tab        <- reactiveVal("home")
  pr_team       <- reactiveVal(NULL)
  pr_opp        <- reactiveVal(NULL)
  pr_man        <- reactiveVal("5")
  pr_game_date  <- reactiveVal(NULL)
  pr_selected   <- reactiveVal(character(0))
  pr_tray_open  <- reactiveVal(TRUE)
  pr_team_modal <- reactiveVal(FALSE)   # NULL = closed, "team" or "opp"
  pr_opp_modal  <- reactiveVal(FALSE)
  
  # Team modal open/close
  observeEvent(input$pr_open_team_modal, { pr_team_modal(TRUE) })
  observeEvent(input$pr_close_team_modal, { pr_team_modal(FALSE) })
  observeEvent(input$pr_open_opp_modal,  { pr_opp_modal(TRUE) })
  observeEvent(input$pr_close_opp_modal, { pr_opp_modal(FALSE) })
  
  observeEvent(input$pr_select_team, {
    pr_team(input$pr_select_team)
    pr_game_date(NULL)
    pr_selected(character(0))
    pr_team_modal(FALSE)
  })
  observeEvent(input$pr_select_opp, {
    pr_opp(input$pr_select_opp)
    pr_opp_modal(FALSE)
  })
  
  # Man mode buttons
  observeEvent(input$pr_man_click, {
    pr_man(input$pr_man_click)
    pr_selected(character(0))
  })
  
  # Sub-tab
  observeEvent(input$pr_tab_click, { pr_tab(input$pr_tab_click) })
  
  # Game date selection
  observeEvent(input$pr_game_date_sel, { pr_game_date(input$pr_game_date_sel) })
  
  # Tray toggle
  observeEvent(input$pr_tray_toggle, { pr_tray_open(!pr_tray_open()) })
  
  # Clear all players
  observeEvent(input$pr_clear_players, { pr_selected(character(0)) })
  
  # Player tray click — add/remove from selected pool with man-mode cap
  observeEvent(input$pr_player_click, {
    player <- input$pr_player_click
    sel    <- pr_selected()
    man    <- suppressWarnings(as.integer(pr_man()))
    cap    <- if(is.na(man)) 5L else man
    if (player %in% sel) {
      pr_selected(setdiff(sel, player))
    } else {
      if (length(sel) < cap) pr_selected(c(sel, player))
      # If at cap, silently ignore (constraint enforced)
    }
  })
  
  # ── Filtered data reactive ──
  pr_data <- reactive({
    team <- pr_team()
    if (is.null(team) || team == "") return(NULL)
    man  <- pr_man()
    gd   <- pr_game_date()
    
    if (man == "10") {
      df <- rot_10m %>% filter(TEAM == team)
      if (!is.null(gd) && gd != "") df <- df %>% filter(as.character(GAME_DATE) == gd)
    } else {
      df <- rot_5m %>% filter(TEAM == team)
      if (!is.null(gd) && gd != "") df <- df %>% filter(as.character(game_date) == gd)
    }
    
    # Filter by selected players
    sel <- pr_selected()
    if (length(sel) > 0) {
      if (man == "10") {
        # Determine home/away split for team
        df <- df %>% rowwise() %>% filter({
          home_players <- c(home_P1,home_P2,home_P3,home_P4,home_P5)
          away_players <- c(away_P1,away_P2,away_P3,away_P4,away_P5)
          team_players <- if(TEAM == HOME_TEAM) home_players else away_players
          all(sel %in% team_players)
        }) %>% ungroup()
      } else {
        df <- df %>% rowwise() %>% filter({
          lp <- c(LINEUP_P1,LINEUP_P2,LINEUP_P3,LINEUP_P4,LINEUP_P5)
          all(sel %in% lp)
        }) %>% ungroup()
      }
    }
    df
  })
  
  # ── Available dates for selected team ──
  pr_dates <- reactive({
    team <- pr_team()
    if (is.null(team)) return(character(0))
    man <- pr_man()
    if (man == "10") {
      sort(unique(as.character(rot_10m$GAME_DATE[rot_10m$TEAM == team])), decreasing=TRUE)
    } else {
      sort(unique(as.character(rot_5m$game_date[rot_5m$TEAM == team])), decreasing=TRUE)
    }
  })
  
  # ── Tray players ──
  pr_tray_players <- reactive({
    team <- pr_team()
    if (is.null(team)) return(data.frame())
    man <- pr_man()
    if (man == "10") {
      df <- rot_10m %>% filter(TEAM == team)
      # Collect all players from home/away columns depending on team
      home_rows <- df %>% filter(TEAM == HOME_TEAM)
      away_rows <- df %>% filter(TEAM == AWAY_TEAM)
      p_home <- unique(c(home_rows$home_P1,home_rows$home_P2,home_rows$home_P3,home_rows$home_P4,home_rows$home_P5))
      p_away <- unique(c(away_rows$away_P1,away_rows$away_P2,away_rows$away_P3,away_rows$away_P4,away_rows$away_P5))
      players <- unique(c(p_home, p_away))
      hs_home <- if(nrow(home_rows)>0) {
        sapply(players, function(p) {
          idx <- which(home_rows$home_P1==p|home_rows$home_P2==p|home_rows$home_P3==p|home_rows$home_P4==p|home_rows$home_P5==p)[1]
          if(is.na(idx)) return("")
          r <- home_rows[idx,]
          for(j in 1:5) { if(r[[paste0("home_P",j)]]==p) return(as.character(r[[paste0("home_P",j,"_espn_id_headshot")]])) }
          ""
        })
      } else setNames(rep("",length(players)), players)
      data.frame(name=players, headshot=as.character(hs_home), stringsAsFactors=FALSE)
    } else {
      df <- rot_5m %>% filter(TEAM == team)
      players <- unique(c(df$LINEUP_P1,df$LINEUP_P2,df$LINEUP_P3,df$LINEUP_P4,df$LINEUP_P5))
      players <- players[!is.na(players) & players != ""]
      hs <- sapply(players, function(p) {
        for(j in 1:5) {
          rows <- df[df[[paste0("LINEUP_P",j)]]==p,]
          if(nrow(rows)>0) return(as.character(rows[[paste0("LINEUP_P",j,"_espn_id_headshot")]][1]))
        }
        ""
      })
      data.frame(name=players, headshot=as.character(hs), stringsAsFactors=FALSE)
    }
  })
  
  # ── Column definitions ──
  cols_5m_off <- c("LINEUP_PTS_CGS","LINEUP_FGA_CGS","LINEUP_FGM_CGS","LINEUP_FG_PCT_CGS",
                   "LINEUP_3PTA_CGS","LINEUP_3PTM_CGS","LINEUP_3PT_PCT_CGS",
                   "LINEUP_FTA_CGS","LINEUP_FTM_CGS","LINEUP_FT_PCT_CGS",
                   "T_OREB_CGS","T_DREB_CGS","T_REB_CGS",
                   "L_AST_CGS","L_STL_CGS","L_BLK_CGS","L_TOV_CGS","L_FOULS_CGS",
                   "L_OFF_RTG_CGS","L_DEF_RTG_CGS","L_NET_RTG_CGS")
  cols_5m_def <- c("OPP_LINEUP_FGA_CGS","OPP_LINEUP_PTS_CGS","OPP_LINEUP_POSS_CGS")
  cols_10m_off <- cols_5m_off
  cols_10m_def <- cols_5m_def
  
  col_labels <- c(
    LINEUP_PTS_CGS="PTS", LINEUP_FGA_CGS="FGA", LINEUP_FGM_CGS="FGM", LINEUP_FG_PCT_CGS="FG%",
    LINEUP_3PTA_CGS="3PA", LINEUP_3PTM_CGS="3PM", LINEUP_3PT_PCT_CGS="3P%",
    LINEUP_FTA_CGS="FTA", LINEUP_FTM_CGS="FTM", LINEUP_FT_PCT_CGS="FT%",
    T_OREB_CGS="OREB", T_DREB_CGS="DREB", T_REB_CGS="REB",
    L_AST_CGS="AST", L_STL_CGS="STL", L_BLK_CGS="BLK", L_TOV_CGS="TOV", L_FOULS_CGS="FOULS",
    L_OFF_RTG_CGS="OFF RTG", L_DEF_RTG_CGS="DEF RTG", L_NET_RTG_CGS="NET RTG",
    OPP_LINEUP_FGA_CGS="OPP FGA", OPP_LINEUP_PTS_CGS="OPP PTS", OPP_LINEUP_POSS_CGS="OPP POSS"
  )
  
  # Build opponent lookup from rot_5m: for each ESPN_GAME_ID + TEAM, find the other TEAM
  opp_lookup_5m <- if(nrow(rot_5m) > 0) {
    games <- unique(rot_5m[, c("ESPN_GAME_ID","TEAM")])
    do.call(rbind, lapply(unique(games$ESPN_GAME_ID), function(gid) {
      teams <- games$TEAM[games$ESPN_GAME_ID == gid]
      if(length(teams) == 2) {
        rbind(
          data.frame(ESPN_GAME_ID=gid, TEAM=teams[1], OPP=teams[2], stringsAsFactors=FALSE),
          data.frame(ESPN_GAME_ID=gid, TEAM=teams[2], OPP=teams[1], stringsAsFactors=FALSE)
        )
      } else NULL
    }))
  } else data.frame(ESPN_GAME_ID=character(), TEAM=character(), OPP=character(), stringsAsFactors=FALSE)
  
  opp_lookup_10m <- if(nrow(rot_10m) > 0) {
    games <- unique(rot_10m[, c("ESPN_GAME_ID","TEAM")])
    do.call(rbind, lapply(unique(games$ESPN_GAME_ID), function(gid) {
      teams <- games$TEAM[games$ESPN_GAME_ID == gid]
      if(length(teams) == 2) {
        rbind(
          data.frame(ESPN_GAME_ID=gid, TEAM=teams[1], OPP=teams[2], stringsAsFactors=FALSE),
          data.frame(ESPN_GAME_ID=gid, TEAM=teams[2], OPP=teams[1], stringsAsFactors=FALSE)
        )
      } else NULL
    }))
  } else data.frame(ESPN_GAME_ID=character(), TEAM=character(), OPP=character(), stringsAsFactors=FALSE)
  
  # ── Main UI ──
  output$player_rotations_ui <- renderUI({
    tab   <- pr_tab()
    team  <- pr_team()
    man   <- pr_man()
    sel   <- pr_selected()
    topen <- pr_tray_open()
    dates <- pr_dates()
    gd    <- pr_game_date()
    team_logo <- if(!is.null(team)) NBA_TEAMS$logo[NBA_TEAMS$abv==team] else NULL
    opp   <- pr_opp()
    opp_logo  <- if(!is.null(opp)) NBA_TEAMS$logo[NBA_TEAMS$abv==opp] else NULL
    cap   <- suppressWarnings(as.integer(man)); if(is.na(cap)) cap <- 5L
    
    tagList(
      # ── Team select modal ──
      if(pr_team_modal()) {
        tags$div(class="pr-team-modal-overlay",
                 tags$div(class="pr-team-modal",
                          tags$div(class="pr-team-modal-header",
                                   tags$div(
                                     tags$div(class="pr-team-modal-title","Select Team"),
                                     tags$div(class="pr-team-modal-sub","30 NBA teams")
                                   ),
                                   tags$button(class="pr-team-modal-close", onclick="Shiny.setInputValue('pr_close_team_modal',Math.random())", "×")
                          ),
                          tags$div(class="pr-team-grid",
                                   lapply(seq_len(nrow(NBA_TEAMS)), function(i) {
                                     t <- NBA_TEAMS[i,]
                                     cls <- paste("pr-team-item", if(!is.null(team) && team==t$abv) "selected" else "")
                                     tags$div(class=cls,
                                              onclick=sprintf("Shiny.setInputValue('pr_select_team','%s',{priority:'event'})", t$abv),
                                              tags$img(src=t$logo, alt=t$abv),
                                              tags$span(t$abv)
                                     )
                                   })
                          )
                 )
        )
      },
      # ── Opp select modal ──
      if(pr_opp_modal()) {
        tags$div(class="pr-team-modal-overlay",
                 tags$div(class="pr-team-modal",
                          tags$div(class="pr-team-modal-header",
                                   tags$div(
                                     tags$div(class="pr-team-modal-title","Select Opponent"),
                                     tags$div(class="pr-team-modal-sub","30 NBA teams")
                                   ),
                                   tags$button(class="pr-team-modal-close", onclick="Shiny.setInputValue('pr_close_opp_modal',Math.random())", "×")
                          ),
                          tags$div(class="pr-team-grid",
                                   lapply(seq_len(nrow(NBA_TEAMS)), function(i) {
                                     t <- NBA_TEAMS[i,]
                                     cls <- paste("pr-team-item", if(!is.null(opp) && opp==t$abv) "selected" else "")
                                     tags$div(class=cls,
                                              onclick=sprintf("Shiny.setInputValue('pr_select_opp','%s',{priority:'event'})", t$abv),
                                              tags$img(src=t$logo, alt=t$abv),
                                              tags$span(t$abv)
                                     )
                                   })
                          )
                 )
        )
      },
      
      # ── Main layout ──
      tags$div(class="pr-layout",
               
               # Sub-nav
               tags$div(class="pr-subnav",
                        lapply(list(
                          list(id="home",      lbl="Home"),
                          list(id="impact",    lbl="Impact Assessment"),
                          list(id="lineup_participation", lbl="Player Lineup Participation"),
                          list(id="matchup",   lbl="Matchup Matrix"),
                          list(id="shot",      lbl="Shot Quality View")
                        ), function(t) {
                          cls <- paste("pr-tab", if(tab==t$id) "active" else "")
                          tags$button(class=cls,
                                      onclick=sprintf("Shiny.setInputValue('pr_tab_click','%s',{priority:'event'})", t$id),
                                      t$lbl)
                        })
               ),
               
               # Content area (scrollable minus tray)
               tags$div(style="flex:1;overflow-y:auto;padding:16px 0 0;display:flex;flex-direction:column;min-height:0;",
                        
                        if (tab != "home") {
                          tags$div(class="pr-placeholder", tags$p("Coming Soon — Placeholder"))
                        } else tagList(
                          
                          # Filter row 1
                          tags$div(class="pr-filters",
                                   tags$div(class="pr-filter-row",
                                            # Year placeholder
                                            tags$button(class="pr-filter-btn",
                                                        tags$svg(viewBox="0 0 24 24",style="width:13px;height:13px;stroke:currentColor;fill:none;stroke-width:1.5;",
                                                                 tags$rect(x="3",y="4",width="18",height="18",rx="2"),tags$line(x1="16",y1="2",x2="16",y2="6"),tags$line(x1="8",y1="2",x2="8",y2="6"),tags$line(x1="3",y1="10",x2="21",y2="10")),
                                                        "2026"
                                            ),
                                            # Team button
                                            tags$button(class=paste("pr-filter-btn", if(!is.null(team)) "active" else ""),
                                                        onclick="Shiny.setInputValue('pr_open_team_modal',Math.random())",
                                                        if(!is.null(team_logo)) tags$img(src=team_logo, style="width:20px;height:20px;object-fit:contain;"),
                                                        if(!is.null(team)) team else "Team"
                                            ),
                                            # Game date dropdown
                                            if(!is.null(team) && length(dates)>0) {
                                              tags$select(class="pr-filter-btn", style="cursor:pointer;",
                                                          onchange="Shiny.setInputValue('pr_game_date_sel',this.value,{priority:'event'})",
                                                          tags$option(value="","All Dates"),
                                                          lapply(dates, function(d) {
                                                            tags$option(value=d, selected=(isTRUE(gd==d)), d)
                                                          })
                                              )
                                            } else {
                                              tags$button(class="pr-filter-btn", disabled=NA, "Game Date")
                                            },
                                            # Reg season placeholder
                                            tags$button(class="pr-filter-btn", "Reg Season ⌃"),
                                            # Med/High placeholder
                                            tags$button(class="pr-filter-btn", "Med/High ⌃")
                                   ),
                                   
                                   # Filter row 2 — man modes
                                   tags$div(class="pr-filter-row",
                                            lapply(c("2","3","4","5"), function(m) {
                                              at_cap <- (length(sel) >= cap && m == man)
                                              cls <- paste("pr-man-btn", if(man==m) "active" else "")
                                              tags$button(class=cls,
                                                          onclick=sprintf("Shiny.setInputValue('pr_man_click','%s',{priority:'event'})", m),
                                                          paste(m,"Man"))
                                            }),
                                            tags$button(class=paste("pr-man-btn", if(man=="10") "active" else ""),
                                                        onclick="Shiny.setInputValue('pr_man_click','10',{priority:'event'})",
                                                        "10 Man"),
                                            # Opp selector for 10M
                                            if(man=="10") {
                                              tags$button(class=paste("pr-filter-btn", if(!is.null(opp)) "active" else ""),
                                                          style="margin-left:8px;",
                                                          onclick="Shiny.setInputValue('pr_open_opp_modal',Math.random())",
                                                          if(!is.null(opp_logo)) tags$img(src=opp_logo, style="width:20px;height:20px;object-fit:contain;"),
                                                          if(!is.null(opp)) opp else "Opp Team"
                                              )
                                            }
                                   ),
                                   
                                   # Luck sliders
                                   tags$div(class="pr-luck-row",
                                            tags$div(class="pr-luck-block",
                                                     tags$div(class="pr-luck-label", tags$span("Off 3P Luck Adjustment"), tags$span("0%")),
                                                     tags$input(type="range", class="pr-luck-slider", min="-20", max="20", value="0", disabled=NA)
                                            ),
                                            tags$div(class="pr-luck-block",
                                                     tags$div(class="pr-luck-label", tags$span("Def 3P Luck Adjustment"), tags$span("0%")),
                                                     tags$input(type="range", class="pr-luck-slider", min="-20", max="20", value="0", disabled=NA)
                                            )
                                   )
                          ),
                          
                          # Table
                          tags$div(class="pr-content",
                                   if(is.null(team)) {
                                     tags$div(class="pr-placeholder", tags$p("Select a team to begin"))
                                   } else {
                                     tags$div(class="pr-table-wrap", uiOutput("pr_table_ui"))
                                   }
                          )
                        ) # end home tab tagList
               ), # end content scrollable area
               
               # ── Tray rendered via JS into body (see observe below) ──
               NULL
      ) # end pr-layout
    )
  })
  
  # ── Inject tray into body via JS ──
  observe({
    sa <- sub_active()
    if (!is.null(sa) && sa == "player_rotations") {
      tp    <- pr_tray_players()
      sel   <- pr_selected()
      topen <- pr_tray_open()
      team  <- pr_team()
      cap   <- suppressWarnings(as.integer(pr_man())); if(is.na(cap)) cap <- 5L
      
      info_text <- if(length(sel)>0) sprintf("%d / %d players selected", length(sel), cap) else
        if(!is.null(team)) sprintf("Click players below to filter  (max %d)", cap) else "Select a team first"
      
      esc_btn <- if(length(sel)>0)
        "<button class='pr-tray-esc' onclick=\"Shiny.setInputValue('pr_clear_players',Math.random())\">Esc — Clear All</button>"
      else ""
      
      toggle_arrow <- if(topen) "&#9660;" else "&#9650;"
      
      player_html <- if(nrow(tp) > 0) {
        tp_sorted <- tp[order(tp$name),]
        paste(sapply(seq_len(nrow(tp_sorted)), function(i) {
          p   <- tp_sorted[i,]
          cls <- if(p$name %in% sel) "pr-tray-player sel" else "pr-tray-player"
          hs  <- as.character(p$headshot)
          nm  <- gsub("'", "", p$name)
          short <- { parts <- strsplit(p$name," ")[[1]]; if(length(parts)>=2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else p$name }
          img_tag <- if(nchar(hs)>10) sprintf("<img class='pr-tray-hs' src='%s'/>", hs) else "<div class='pr-tray-hs-ph'></div>"
          sprintf("<div class='%s' onclick=\"Shiny.setInputValue('pr_player_click','%s',{priority:'event'})\">%s<div class='pr-tray-name'>%s</div></div>",
                  cls, nm, img_tag, short)
        }), collapse="")
      } else ""
      
      grid_html <- if(topen) sprintf("<div class='pr-tray-grid'>%s</div>", player_html) else ""
      bar_html  <- if(topen) sprintf("<div class='pr-tray-bar'><span class='pr-tray-info'>%s</span>%s</div>", info_text, esc_btn) else ""
      
      html <- sprintf(
        "<div class='pr-tray-outer'><div class='pr-tray-toggle-row'><button class='pr-tray-toggle' onclick=\"Shiny.setInputValue('pr_tray_toggle',Math.random())\">%s</button></div>%s%s</div>",
        toggle_arrow, bar_html, grid_html
      )
      session$sendCustomMessage("renderTray", list(html=html))
    } else {
      session$sendCustomMessage("hideTray", list())
    }
  })
  
  # ── Tray players UI (kept for legacy, not used) ──
  output$pr_tray_players_ui <- renderUI({
    tp  <- pr_tray_players()
    sel <- pr_selected()
    if(nrow(tp)==0) return(tags$div(style="color:var(--text-muted);font-size:11px;padding:8px;","No player data"))
    tp_sorted <- tp[order(tp$name),]
    lapply(seq_len(nrow(tp_sorted)), function(i) {
      p   <- tp_sorted[i,]
      cls <- paste("pr-tray-player", if(p$name %in% sel) "selected" else "")
      hs  <- as.character(p$headshot)
      short_name <- {
        parts <- strsplit(p$name, " ")[[1]]
        if(length(parts)>=2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else p$name
      }
      tags$div(class=cls,
               onclick=sprintf("Shiny.setInputValue('pr_player_click','%s',{priority:'event'})", gsub("'","",p$name)),
               if(nchar(hs)>10) tags$img(class="pr-tray-hs", src=hs) else tags$div(class="pr-tray-hs-ph"),
               tags$div(class="pr-tray-name", short_name)
      )
    })
  })
  
  # ── Lineup table UI ──
  output$pr_table_ui <- renderUI({
    df  <- pr_data()
    man <- pr_man()
    if(is.null(df) || nrow(df)==0)
      return(tags$div(class="pr-placeholder", tags$p("No lineup data for current filters")))
    
    off_cols <- if(man=="10") cols_10m_off else cols_5m_off
    def_cols <- if(man=="10") cols_10m_def else cols_5m_def
    all_cols <- c(off_cols, def_cols)
    # Keep only cols that exist in data
    all_cols <- all_cols[all_cols %in% names(df)]
    off_cols <- off_cols[off_cols %in% names(df)]
    def_cols <- def_cols[def_cols %in% names(df)]
    
    # Pick the right opp lookup
    opp_lkp <- if(man=="10") opp_lookup_10m else opp_lookup_5m
    date_col <- if(man=="10") "GAME_DATE" else "game_date"
    
    # Build header — DATE and OPP first, then stats
    th_lineup <- "<th style='min-width:200px'>Lineup</th>"
    th_meta   <- "<th>Date</th><th>Opp</th>"
    th_off <- paste(sapply(off_cols, function(c) sprintf("<th class='col-group-off'>%s</th>", col_labels[c])), collapse="")
    th_def <- paste(sapply(def_cols, function(c) sprintf("<th class='col-group-def'>%s</th>", col_labels[c])), collapse="")
    header <- sprintf("<thead><tr>%s%s%s%s</tr></thead>", th_lineup, th_meta, th_off, th_def)
    
    # Build rows (cap at 200 for performance)
    df_show <- head(df, 200)
    rows <- paste(sapply(seq_len(nrow(df_show)), function(i) {
      r <- df_show[i,]
      
      # Lineup headshots
      if(man=="10") {
        is_home <- isTRUE(r$TEAM == r$HOME_TEAM)
        px <- if(is_home) paste0("home_P",1:5) else paste0("away_P",1:5)
        hx <- if(is_home) paste0("home_P",1:5,"_espn_id_headshot") else paste0("away_P",1:5,"_espn_id_headshot")
      } else {
        px <- paste0("LINEUP_P",1:5)
        hx <- paste0("LINEUP_P",1:5,"_espn_id_headshot")
      }
      icons <- paste(sapply(1:5, function(j) {
        hs <- if(hx[j] %in% names(r)) as.character(r[[hx[j]]]) else ""
        if(nchar(hs)>10) sprintf("<img class='pr-hs' src='%s' title='%s'/>", hs, if(px[j] %in% names(r)) r[[px[j]]] else "")
        else "<div class='pr-hs-ph'></div>"
      }), collapse="")
      lineup_td <- sprintf("<td><div class='pr-lineup-cell'>%s</div></td>", icons)
      
      # Date cell
      gdate <- if(date_col %in% names(r)) as.character(r[[date_col]]) else "—"
      date_td <- sprintf("<td style='white-space:nowrap;color:var(--text-muted);font-size:11px;'>%s</td>", gdate)
      
      # Opp cell — logo + abbrev
      gid  <- as.character(r[["ESPN_GAME_ID"]])
      team <- as.character(r[["TEAM"]])
      opp_row <- opp_lkp[opp_lkp$ESPN_GAME_ID==gid & opp_lkp$TEAM==team, ]
      opp_abv <- if(nrow(opp_row)>0) opp_row$OPP[1] else "—"
      opp_logo_url <- if(opp_abv != "—") NBA_TEAMS$logo[NBA_TEAMS$abv==opp_abv] else ""
      opp_img <- if(nchar(opp_logo_url)>0) sprintf("<img src='%s' style='width:18px;height:18px;object-fit:contain;vertical-align:middle;margin-right:4px;'/>", opp_logo_url) else ""
      opp_td <- sprintf("<td style='white-space:nowrap;'>%s<span style='font-size:11px;'>%s</span></td>", opp_img, opp_abv)
      
      # Stat cells
      stat_tds <- paste(sapply(all_cols, function(col) {
        val <- suppressWarnings(as.numeric(r[[col]]))
        if(is.na(val)) "<td>—</td>"
        else if(grepl("PCT",col)) sprintf("<td>%s%%</td>", round(val*100, 1))
        else if(grepl("RTG",col)) sprintf("<td>%s</td>", round(val * 100, 1))
        else sprintf("<td>%s</td>", round(val, 1))
      }), collapse="")
      
      sprintf("<tr>%s%s%s%s</tr>", lineup_td, date_td, opp_td, stat_tds)
    }), collapse="")
    
    HTML(sprintf("<table class='pr-table'>%s<tbody>%s</tbody></table>", header, rows))
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # PLAYER & TEAM ODDS
  # ════════════════════════════════════════════════════════════════════════════
  
  po_tab        <- reactiveVal("player")   # "player" or "team"
  po_stat       <- reactiveVal("ALL")
  po_date       <- reactiveVal(NULL)
  po_sort_col   <- reactiveVal(NULL)
  po_sort_dir   <- reactiveVal("desc")
  
  observeEvent(input$po_tab_click,  { po_tab(input$po_tab_click) })
  observeEvent(input$po_stat_click, { po_stat(input$po_stat_click) })
  observeEvent(input$po_date_sel,   { po_date(input$po_date_sel) })
  po_search_val <- reactiveVal("")
  observeEvent(input$po_search_val, { po_search_val(tolower(trimws(input$po_search_val))) })
  
  observeEvent(input$po_sort, {
    col <- input$po_sort
    if(!is.null(po_sort_col()) && po_sort_col() == col) {
      po_sort_dir(if(po_sort_dir()=="asc") "desc" else "asc")
    } else {
      po_sort_col(col); po_sort_dir("desc")
    }
  })
  
  # Stat filter mapping to column prefixes
  stat_prefixes <- list(
    ALL        = c("PTS","AST","REB","3PM","STL","BLK","PTS_AST","PTS_REB","REB_AST","PTS_AST_REB"),
    PTS        = "PTS",
    REB        = "REB",
    AST        = "AST",
    `3PTS`     = "3PM",
    STL        = "STL",
    BLK        = "BLK",
    `PTS + AST`     = "PTS_AST",
    `PTS + REB`     = "PTS_REB",
    `REB + AST`     = "REB_AST",
    `PTS + AST + REB` = "PTS_AST_REB"
  )
  
  # Prop label mapping
  prop_labels <- c(
    PTS="PTS", AST="AST", REB="REB", `3PM`="3PTS", STL="STL", BLK="BLK",
    PTS_AST="PTS+AST", PTS_REB="PTS+REB", REB_AST="REB+AST", PTS_AST_REB="PTS+AST+REB"
  )
  
  # Available dates
  po_dates <- reactive({
    p_dates <- as.character(player_odds$GAME_DATE[!is.na(player_odds$GAME_DATE)])
    t_dates <- as.character(team_data$GAME_DATE[!is.na(team_data$GAME_DATE)])
    sort(unique(c(p_dates, t_dates)), decreasing=TRUE)
  })
  
  # Filtered player odds
  po_filtered <- reactive({
    df  <- player_odds
    gd  <- po_date()
    st  <- po_stat()
    
    if(!is.null(gd) && length(gd) > 0 && gd != "") df <- df[as.character(df$GAME_DATE) == as.character(gd), ]
    # Drop rows with no player name
    if("PLAYER" %in% names(df)) df <- df[!is.na(df$PLAYER) & nchar(as.character(df$PLAYER)) > 0, ]
    if(nrow(df) == 0) return(data.frame())
    
    # Expand to one row per prop type
    prefixes <- if(st == "ALL") names(prop_labels) else if(st %in% names(stat_prefixes)) as.list(stat_prefixes[[st]]) else names(prop_labels)
    
    rows <- do.call(rbind, lapply(prefixes, function(pfx) {
      # Keep row if ANY sportsbook has a line for this prop
      book_keys  <- c("DK","FD","FNA","BMGM")
      line_cols  <- paste0(pfx, "_", book_keys, "_LINE_O")
      valid_cols <- line_cols[line_cols %in% names(df)]
      if(length(valid_cols) == 0) return(NULL)
      has_any <- rowSums(!is.na(df[, valid_cols, drop=FALSE])) > 0
      sub <- df[has_any, ]
      if(nrow(sub) == 0) return(NULL)
      sub$PROP_KEY <- pfx
      sub
    }))
    if(is.null(rows) || nrow(rows) == 0) return(data.frame())
    rows
  })
  
  # Main UI output
  output$player_team_odds_ui <- renderUI({
    tab   <- po_tab()
    dates <- po_dates()
    gd    <- po_date()
    stat  <- po_stat()
    
    tagList(
      tags$div(class="po-layout",
               
               # Sub-nav tabs
               tags$div(class="po-subnav",
                        tags$button(class=paste("po-tab", if(tab=="player") "active" else ""),
                                    onclick="Shiny.setInputValue('po_tab_click','player',{priority:'event'})", "Player Odds"),
                        tags$button(class=paste("po-tab", if(tab=="team") "active" else ""),
                                    onclick="Shiny.setInputValue('po_tab_click','team',{priority:'event'})", "Team Odds")
                        ,tags$button(class=paste("po-tab", if(tab=="favdog") "active" else ""),
                                     onclick="Shiny.setInputValue('po_tab_click','favdog',{priority:'event'})", "Fav & Dog Analysis")
                        ,tags$button(class=paste("po-tab", if(tab=="favdog") "active" else ""),
                                     onclick="Shiny.setInputValue('po_tab_click','favdog',{priority:'event'})", "Fav & Dog Analysis")
               ),
               
               # Filter bar
               tags$div(class="po-filters",
                        # NBA badge
                        tags$div(class="po-nba-badge",
                                 tags$svg(viewBox="0 0 24 24", style="width:14px;height:14px;fill:none;stroke:currentColor;stroke-width:1.5;",
                                          tags$circle(cx="12",cy="12",r="10"),
                                          tags$path(d="M12 2a10 10 0 0 1 0 20M12 2a10 10 0 0 0 0 20M2 12h20")
                                 ),
                                 "NBA"
                        ),
                        # Date selector
                        if(length(dates)>0) {
                          tagList(
                            tags$select(class="po-filter-btn", style="cursor:pointer;",
                                        onchange="Shiny.setInputValue('po_date_sel',this.value,{priority:'event'})",
                                        tags$option(value="","All Dates"),
                                        lapply(dates, function(d) tags$option(value=d, selected=isTRUE(gd==d), d))
                            ),
                            if(!is.null(gd) && gd != "") {
                              tags$div(style="display:flex;align-items:center;gap:6px;padding:5px 14px;background:var(--bg-card);border:1px solid var(--border);border-radius:6px;font-family:'Rajdhani',sans-serif;font-weight:700;font-size:15px;letter-spacing:.06em;color:var(--text-primary);",
                                       format(as.Date(gd), "%A, %B %d, %Y"))
                            }
                          )
                        },
                        # Search bar + stat pills (player tab only)
                        if(tab=="player") {
                          stat_btns <- c("ALL","PTS","REB","AST","3PTS","STL","BLK",
                                         "PTS + AST","PTS + REB","REB + AST","PTS + AST + REB")
                          tagList(
                            # Search bar
                            tags$div(style="position:relative;",
                                     tags$svg(viewBox="0 0 24 24", style="position:absolute;left:8px;top:50%;transform:translateY(-50%);width:13px;height:13px;fill:none;stroke:var(--text-muted);stroke-width:2;pointer-events:none;",
                                              tags$circle(cx="11",cy="11",r="8"),
                                              tags$path(d="m21 21-4.35-4.35")
                                     ),
                                     tags$input(
                                       id="po_search",
                                       type="text",
                                       placeholder="Search player...",
                                       oninput="filterPoTable(this.value); clearTimeout(window._poSearchTimer); window._poSearchTimer = setTimeout(function(){ Shiny.setInputValue('po_search_val', document.getElementById('po_search').value, {priority:'event'}); }, 300);",
                                       style="background:var(--bg-card);border:1px solid var(--border);border-radius:6px;color:var(--text-primary);font-family:'Share Tech Mono',monospace;font-size:11px;padding:5px 10px 5px 28px;width:160px;outline:none;"
                                     )
                            ),
                            # Stat pills
                            lapply(stat_btns, function(s) {
                              tags$button(
                                class=paste("po-stat-btn", if(stat==s) "active" else ""),
                                onclick=sprintf("Shiny.setInputValue('po_stat_click','%s',{priority:'event'})", s),
                                s
                              )
                            })
                          )
                        }
               ),
               
               # Content
               tags$div(class="po-content",
                        if(tab=="player") uiOutput("po_player_table_ui")
                        else if(tab=="team") uiOutput("po_team_cards_ui")
                        else if(tab=="favdog") uiOutput("po_favdog_ui")
               )
      ),
      # JS filter function + Escape hotkey
      if(tab=="player") tags$script(HTML("
        function filterPoTable(val) {
          var q = val.toLowerCase().trim();
          var rows = document.querySelectorAll('.po-table tbody tr');
          rows.forEach(function(row) {
            var name = row.querySelector('.po-player-name');
            var team = row.querySelector('.po-player-team');
            var txt  = ((name ? name.textContent : '') + ' ' + (team ? team.textContent : '')).toLowerCase();
            row.style.display = (q === '' || txt.indexOf(q) > -1) ? '' : 'none';
          });
        }

        function clearPoFilters() {
          // Clear search box and show all rows
          var box = document.getElementById('po_search');
          if(box) { box.value = ''; filterPoTable(''); }
          // Clear server-side search
          Shiny.setInputValue('po_search_val', '', {priority: 'event'});
          // Reset stat pill to ALL
          Shiny.setInputValue('po_stat_click', 'ALL', {priority: 'event'});
        }

        document.addEventListener('keydown', function(e) {
          if(e.key === 'Escape') clearPoFilters();
        });
      "))
    )
  })
  
  # ── Player odds table ──
  output$po_player_table_ui <- renderUI({
    df  <- po_filtered()
    sc  <- po_sort_col()
    sdr <- po_sort_dir()
    
    if(is.null(df) || nrow(df)==0)
      return(tags$div(class="pr-placeholder", tags$p("No player odds data for selected filters")))
    
    # Sportsbooks config
    sbs <- list(
      list(key="DK",   label="DK",   logo=sb_logos$DK),
      list(key="FD",   label="FD",   logo=sb_logos$FD),
      list(key="FNA",  label="FNKTS",logo=sb_logos$FNA),
      list(key="BMGM", label="MGM",  logo=sb_logos$BMGM)
    )
    
    # Sort
    sort_options <- c("PLAYER","PROP","MIN","PTS","REB","AST","STL","BLK","3PTM","COVER","COVER_AMT")
    if(!is.null(sc) && sc %in% names(df)) {
      df <- df[order(df[[sc]], decreasing=(sdr=="desc")),]
    }
    
    sort_th <- function(col, label) {
      cls <- if(!is.null(sc) && sc==col) paste0("sort-",sdr) else ""
      sprintf("<th class='%s'>%s<button class='po-sort-btn' onclick=\"Shiny.setInputValue('po_sort','%s',{priority:'event'})\">⇅</button></th>",
              cls, label, col)
    }
    
    # Build header rows
    # Row 1: group headers
    th_player <- "<th rowspan='2' style='min-width:190px;'>Player</th>"
    th_prop   <- "<th rowspan='2'>Prop</th>"
    sb_group_ths <- paste(sapply(sbs, function(sb) {
      sprintf("<th colspan='4' style='text-align:center;border-left:1px solid var(--border);'><img src='%s' style='width:16px;height:16px;object-fit:contain;vertical-align:middle;'/> %s</th>",
              sb$logo, sb$label)
    }), collapse="")
    th_act    <- "<th colspan='7' style='text-align:center;border-left:1px solid var(--border);'>ACT</th>"
    th_cover  <- "<th rowspan='2' style='border-left:1px solid var(--border);'>Cover</th>"
    th_covamt <- "<th rowspan='2'>Amt</th>"
    header1   <- sprintf("<tr>%s%s%s%s%s%s</tr>", th_player, th_prop, sb_group_ths, th_act, th_cover, th_covamt)
    
    # Row 2: sub-headers per sportsbook (O line / O odds / U line / U odds)
    sb_sub_ths <- paste(sapply(sbs, function(sb) {
      "<th style='border-left:1px solid var(--border);font-size:8px;'>O Line</th><th style='font-size:8px;'>O Odds</th><th style='font-size:8px;'>U Line</th><th style='font-size:8px;'>U Odds</th>"
    }), collapse="")
    act_sub_ths <- "<th style='border-left:1px solid var(--border);font-size:8px;'>MIN</th><th style='font-size:8px;'>PTS</th><th style='font-size:8px;'>REB</th><th style='font-size:8px;'>3PM</th><th style='font-size:8px;'>STL</th><th style='font-size:8px;'>BLK</th><th style='font-size:8px;'>AST</th>"
    header2 <- sprintf("<tr>%s%s</tr>", sb_sub_ths, act_sub_ths)
    full_header <- sprintf("<thead>%s%s</thead>", header1, header2)
    
    # Build rows
    fmt_val <- function(x) { v <- suppressWarnings(as.numeric(x)); if(is.na(v)) "—" else as.character(round(v,1)) }
    fmt_odds <- function(x) { v <- suppressWarnings(as.numeric(x)); if(is.na(v)) "—" else if(v>0) paste0("+",v) else as.character(v) }
    
    safe_str <- function(r, col, default="") {
      if(!(col %in% names(r))) return(default)
      v <- r[[col]][1]
      if(is.null(v) || length(v)==0 || is.na(v)) return(default)
      as.character(v)
    }
    has_img <- function(u) { length(u)>0 && !is.na(u) && is.character(u) && nchar(u) > 10 }
    
    # Apply server-side search filter before cap so search works across full dataset
    sq <- po_search_val()
    if(nchar(sq) > 0) {
      player_col <- if("PLAYER" %in% names(df)) tolower(as.character(df$PLAYER)) else ""
      team_col   <- if("TEAM"   %in% names(df)) tolower(as.character(df$TEAM))   else ""
      opp_col    <- if("OPP"    %in% names(df)) tolower(as.character(df$OPP))    else ""
      matches <- grepl(sq, paste(player_col, team_col, opp_col), fixed=TRUE)
      df <- df[matches, ]
    }
    
    rows <- paste(sapply(seq_len(min(nrow(df), 500)), function(i) {
      r   <- df[i,]
      pfx <- safe_str(r, "PROP_KEY")
      
      # Player cell
      hs  <- safe_str(r, "HEADSHOT")
      tl  <- safe_str(r, "TEAM_LOGO")
      ol  <- safe_str(r, "OPP_LOGO")
      hs_tag  <- if(has_img(hs)) sprintf("<img src='%s' style='width:32px;height:32px;border-radius:50%%;object-fit:cover;object-position:top;flex-shrink:0;'/>", hs) else "<div style='width:32px;height:32px;border-radius:50%;background:var(--bg-base);flex-shrink:0;'></div>"
      tl_tag  <- if(has_img(tl)) sprintf("<img class='po-team-logo' src='%s'/>", tl) else ""
      ol_tag  <- if(has_img(ol)) sprintf("<img class='po-team-logo' src='%s'/>", ol) else ""
      raw_ha  <- safe_str(r, "HOME_AWAY", "")
      sym     <- if(nchar(raw_ha) > 0) trimws(raw_ha) else "vs"
      player_name <- safe_str(r, "PLAYER", "—")
      team_name   <- safe_str(r, "TEAM", "")
      opp_name    <- safe_str(r, "OPP", "")
      player_td <- sprintf(
        "<td class='td-player'><div style='display:flex;align-items:center;gap:8px;'>%s<div class='po-player-cell'><div class='po-player-name'>%s</div><div class='po-player-meta'>%s<span class='po-player-team'>%s</span><span class='po-home-away'>%s</span>%s<span class='po-player-team'>%s</span></div></div></div></td>",
        hs_tag, player_name, tl_tag, team_name, sym, ol_tag, opp_name
      )
      
      # Prop badge
      prop_lbl  <- if(pfx %in% names(prop_labels)) prop_labels[pfx] else pfx
      prop_td   <- sprintf("<td><span class='po-prop-badge'>%s</span></td>", prop_lbl)
      
      # Sportsbook cells
      sb_tds <- paste(sapply(sbs, function(sb) {
        lo_col <- paste0(pfx,"_",sb$key,"_LINE_O")
        oo_col <- paste0(pfx,"_",sb$key,"_ODDS_O")
        lu_col <- paste0(pfx,"_",sb$key,"_LINE_U")
        ou_col <- paste0(pfx,"_",sb$key,"_ODDS_U")
        get_col <- function(col) if(col %in% names(r)) r[[col]] else NA
        
        lo_v <- get_col(lo_col); oo_v <- get_col(oo_col)
        lu_v <- get_col(lu_col); ou_v <- get_col(ou_col)
        
        prop_name <- if(pfx %in% names(prop_labels)) prop_labels[pfx] else pfx
        p_name <- safe_str(r, "PLAYER", "")
        game_str <- paste(safe_str(r,"TEAM",""), "vs", safe_str(r,"OPP",""))
        
        # Over line cell — clickable
        o_onclick <- if(!is.na(lo_v) && !is.na(oo_v))
          sprintf(" class='odds-clickable' onclick=\"%s\"",
                  bs_onclick(p_name, paste("Over", prop_name), lo_v, oo_v, "", game_str)) else ""
        # Under line cell — clickable
        u_onclick <- if(!is.na(lu_v) && !is.na(ou_v))
          sprintf(" class='odds-clickable' onclick=\"%s\"",
                  bs_onclick(p_name, paste("Under", prop_name), lu_v, ou_v, "", game_str)) else ""
        
        sprintf("<td style='border-left:1px solid var(--border);'%s>%s</td><td>%s</td><td%s>%s</td><td>%s</td>",
                o_onclick, fmt_val(lo_v), fmt_odds(oo_v),
                u_onclick, fmt_val(lu_v), fmt_odds(ou_v))
      }), collapse="")
      
      # Actuals - use safe column access
      gcol <- function(col) { if(col %in% names(r)) r[[col]] else NA }
      act_min <- fmt_val(gcol("MIN")); act_pts <- fmt_val(gcol("PTS")); act_reb <- fmt_val(gcol("REB"))
      act_3pm <- fmt_val(gcol("3PTM")); act_stl <- fmt_val(gcol("STL")); act_blk <- fmt_val(gcol("BLK")); act_ast <- fmt_val(gcol("AST"))
      act_tds <- sprintf("<td style='border-left:1px solid var(--border);'>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
                         act_min, act_pts, act_reb, act_3pm, act_stl, act_blk, act_ast)
      
      # Cover
      cover_col <- paste0(pfx,"_COVER")
      cover_raw <- if(cover_col %in% names(r)) r[[cover_col]] else NA
      cover_val <- if(length(cover_raw)==0 || is.na(cover_raw[1])) NA else as.character(cover_raw[1])
      is_yes    <- isTRUE(cover_val %in% c("1","TRUE","True","Yes","yes"))
      cover_cls <- if(is.na(cover_val)) "po-cover-na" else if(is_yes) "po-cover-yes" else "po-cover-no"
      cover_lbl <- if(is.na(cover_val)) "\u2014" else if(is_yes) "Yes" else "No"
      cover_td  <- sprintf("<td style='border-left:1px solid var(--border);'><span class='%s'>%s</span></td>", cover_cls, cover_lbl)
      
      # Cover amount
      amt_col <- paste0(pfx,"_COVER_AMT")
      amt_val <- suppressWarnings(as.numeric(if(amt_col %in% names(r)) r[[amt_col]] else NA))
      amt_cls <- if(is.na(amt_val)) "po-cover-na" else if(amt_val >= 0) "po-cover-amt-pos" else "po-cover-amt-neg"
      amt_str <- if(is.na(amt_val)) "—" else if(amt_val >= 0) paste0("+",round(amt_val,1)) else as.character(round(amt_val,1))
      amt_td  <- sprintf("<td><span class='%s'>%s</span></td>", amt_cls, amt_str)
      
      sprintf("<tr>%s%s%s%s%s%s</tr>", player_td, prop_td, sb_tds, act_tds, cover_td, amt_td)
    }), collapse="")
    
    HTML(sprintf("<div class='po-table-wrap'><table class='po-table'>%s<tbody>%s</tbody></table></div>", full_header, rows))
  })
  
  # ── Team odds cards ──
  output$po_team_cards_ui <- renderUI({
    gd <- po_date()
    if(is.null(gd) || gd == "") return(tags$div(class="pr-placeholder", tags$p("Select a date to view team odds")))
    
    # Get games for this date from schedule_data
    day_games <- schedule_data %>% filter(game_date == as.Date(gd))
    if(nrow(day_games) == 0) return(tags$div(class="pr-placeholder", tags$p("No games for selected date")))
    
    # Get odds for this date
    day_odds <- historical_odds %>% filter(commence_date_est == as.Date(gd))
    
    # Get unique game_ids — dedupe to one per game
    home_games <- day_games %>% filter(is_home == 1 | is_home == "1") %>% distinct(game_id, .keep_all = TRUE)
    if(nrow(home_games) == 0) home_games <- day_games %>% distinct(game_id, .keep_all = TRUE)
    
    # Team data for actual scores
    td <- team_data[as.character(team_data$GAME_DATE) == as.character(gd), ]
    
    # Banner stats
    n_games <- nrow(home_games)
    
    # Helper to get odds
    get_odds <- function(mkt, bk, team_abv) {
      r <- day_odds[day_odds$market == mkt & tolower(day_odds$bookmaker) == tolower(bk) & day_odds$outcome_team_abv == team_abv, ]
      if(nrow(r) > 0) list(line = r$spread_or_total[1], odds = r$odds[1]) else list(line = NA, odds = NA)
    }
    
    fmt_odds <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      if(is.na(v)) return("")
      if(v > 0) paste0("+", round(v)) else as.character(round(v))
    }
    fmt_spread <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      if(is.na(v)) return("")
      if(v > 0) paste0("+", round(v, 1)) else as.character(round(v, 1))
    }
    
    # Count stats for banner
    n_fav_win <- 0; n_dog_win <- 0; n_home_win <- 0; n_away_win <- 0; n_h_dog_win <- 0; n_a_dog_win <- 0
    
    cards_html <- paste(sapply(seq_len(nrow(home_games)), function(i) {
      hg <- home_games[i, ]
      gid <- as.character(hg$game_id)
      
      # Get both teams from schedule
      g_rows <- day_games[day_games$game_id == gid, ]
      home_r <- g_rows[g_rows$is_home == 1 | g_rows$is_home == "1", ]
      away_r <- g_rows[g_rows$is_away == 1 | g_rows$is_away == "1", ]
      if(nrow(home_r) == 0) home_r <- g_rows[1, ]
      if(nrow(away_r) == 0) away_r <- g_rows[min(2, nrow(g_rows)), ]
      
      home_abv <- as.character(home_r$team[1])
      away_abv <- as.character(away_r$team[1])
      home_logo <- if(!is.na(home_r$team_logo[1]) && nchar(as.character(home_r$team_logo[1])) > 0) as.character(home_r$team_logo[1]) else ""
      away_logo <- if(!is.na(away_r$team_logo[1]) && nchar(as.character(away_r$team_logo[1])) > 0) as.character(away_r$team_logo[1]) else ""
      home_name <- if(!is.na(home_r$team_display_name[1])) as.character(home_r$team_display_name[1]) else home_abv
      away_name <- if(!is.na(away_r$team_display_name[1])) as.character(away_r$team_display_name[1]) else away_abv
      
      # Actual scores from team_data
      home_td <- td[td$TEAM == home_abv, ]
      away_td <- td[td$TEAM == away_abv, ]
      home_pts <- if(nrow(home_td) > 0) suppressWarnings(as.numeric(home_td$PTS_CGS[1])) else NA
      away_pts <- if(nrow(away_td) > 0) suppressWarnings(as.numeric(away_td$PTS_CGS[1])) else NA
      home_won <- !is.na(home_pts) && !is.na(away_pts) && home_pts > away_pts
      away_won <- !is.na(home_pts) && !is.na(away_pts) && away_pts > home_pts
      has_score <- !is.na(home_pts) && !is.na(away_pts)
      actual_total <- if(has_score) home_pts + away_pts else NA
      
      # DK odds for each team
      dk_sp_away <- get_odds("spreads", "draftkings", away_abv)
      dk_sp_home <- get_odds("spreads", "draftkings", home_abv)
      dk_ml_away <- get_odds("h2h", "draftkings", away_abv)
      dk_ml_home <- get_odds("h2h", "draftkings", home_abv)
      dk_tot     <- day_odds[day_odds$market == "totals" & tolower(day_odds$bookmaker) == "draftkings" &
                               (day_odds$espn_team_abv_home == home_abv | day_odds$espn_team_abv_away == home_abv), ]
      total_line <- if(nrow(dk_tot) > 0) suppressWarnings(as.numeric(dk_tot$spread_or_total[1])) else NA
      total_odds_o <- if(nrow(dk_tot) > 0) {
        o_row <- dk_tot[grepl("over|Over", dk_tot$outcome_team, ignore.case = TRUE) | dk_tot$outcome_team_abv == "Over", ]
        if(nrow(o_row) > 0) suppressWarnings(as.numeric(o_row$odds[1])) else suppressWarnings(as.numeric(dk_tot$odds[1]))
      } else NA
      total_odds_u <- if(nrow(dk_tot) > 1) {
        u_row <- dk_tot[grepl("under|Under", dk_tot$outcome_team, ignore.case = TRUE) | dk_tot$outcome_team_abv == "Under", ]
        if(nrow(u_row) > 0) suppressWarnings(as.numeric(u_row$odds[1])) else suppressWarnings(as.numeric(dk_tot$odds[2]))
      } else NA
      
      # Banner counting
      if(has_score) {
        if(home_won) n_home_win <<- n_home_win + 1 else if(away_won) n_away_win <<- n_away_win + 1
        # Determine favorite by spread
        home_sp <- suppressWarnings(as.numeric(dk_sp_home$line))
        if(!is.na(home_sp)) {
          home_fav <- home_sp < 0
          if(home_fav) { if(home_won) n_fav_win <<- n_fav_win + 1 else n_dog_win <<- n_dog_win + 1 }
          else { if(away_won) n_fav_win <<- n_fav_win + 1 else n_dog_win <<- n_dog_win + 1 }
          if(!home_fav && home_won) n_h_dog_win <<- n_h_dog_win + 1
          if(home_fav && away_won) n_a_dog_win <<- n_a_dog_win + 1
        }
      }
      
      
      # Build team row
      make_team_row <- function(logo, name, abv, sp, ml, pts, won, is_away, game_info_str) {
        logo_html <- if(nchar(logo) > 0) sprintf("<img src='%s' style='width:32px;height:32px;object-fit:contain;margin-right:8px;'/>", logo) else ""
        prefix <- if(is_away) "<span style='color:var(--text-muted);font-size:12px;margin-right:4px;'>@</span>" else ""
        name_html <- sprintf("<div style='display:flex;align-items:center;flex:1;min-width:180px;'>%s%s<span style='font-family:Calibri,sans-serif;font-size:17px;font-weight:600;color:var(--text-primary);'>%s</span></div>", prefix, logo_html, name)
        
        sp_line <- fmt_spread(sp$line)
        sp_odds <- fmt_odds(sp$odds)
        sp_oc <- bs_onclick(abv, "Spread", sp$line, sp$odds, "", game_info_str)
        sp_html <- if(nchar(sp_line) > 0) sprintf("<div class='odds-clickable' style='display:flex;flex-direction:column;align-items:center;min-width:80px;background:var(--bg-base);border:1px solid var(--border);border-radius:6px;padding:4px 10px;cursor:pointer;' onclick=\"%s\"><span style='font-family:Rajdhani,sans-serif;font-weight:700;font-size:16px;color:var(--text-primary);'>%s</span><span style='font-size:12px;color:var(--text-muted);font-family:Share Tech Mono,monospace;'>%s</span></div>", sp_oc, sp_line, sp_odds) else "<div style='min-width:80px;text-align:center;color:var(--text-muted);'>\u2014</div>"
        
        ml_val <- fmt_odds(ml$odds)
        ml_color <- { v <- suppressWarnings(as.numeric(ml$odds)); if(!is.na(v) && v > 0) "color:#5a9a5a;" else "color:var(--text-primary);" }
        ml_oc <- bs_onclick(abv, "Moneyline", "", ml$odds, "", game_info_str)
        ml_html <- if(nchar(ml_val) > 0) sprintf("<div class='odds-clickable' style='display:flex;flex-direction:column;align-items:center;min-width:70px;background:var(--bg-base);border:1px solid var(--border);border-radius:6px;padding:4px 10px;cursor:pointer;' onclick=\"%s\"><span style='font-family:Rajdhani,sans-serif;font-weight:700;font-size:16px;%s'>%s</span></div>", ml_oc, ml_color, ml_val) else "<div style='min-width:70px;text-align:center;color:var(--text-muted);'>\u2014</div>"
        
        pts_html <- if(!is.na(pts)) sprintf("<span style='font-family:Rajdhani,sans-serif;font-weight:700;font-size:20px;min-width:36px;text-align:right;%s'>%s</span>", if(won) "color:#7aba7a;" else "color:var(--text-secondary);", as.integer(pts)) else "<span style='min-width:36px;text-align:right;color:var(--text-muted);'>\u2014</span>"
        
        sprintf("<div style='display:flex;align-items:center;gap:10px;padding:8px 14px;border-bottom:1px solid var(--border);'>%s%s%s%s</div>", name_html, sp_html, ml_html, pts_html)
      }
      
      game_info_str <- paste(away_abv, "@", home_abv)
      away_row_html <- make_team_row(away_logo, away_name, away_abv, dk_sp_away, dk_ml_away, away_pts, away_won, TRUE, game_info_str)
      home_row_html <- make_team_row(home_logo, home_name, home_abv, dk_sp_home, dk_ml_home, home_pts, home_won, FALSE, game_info_str)
      
      # Total row
      total_str <- if(!is.na(total_line)) as.character(round(total_line, 1)) else "\u2014"
      over_hit <- isTRUE(!is.na(actual_total) && !is.na(total_line) && actual_total > total_line)
      under_hit <- isTRUE(!is.na(actual_total) && !is.na(total_line) && actual_total < total_line)
      act_str <- if(!is.na(actual_total)) sprintf("<span style='font-size:13px;color:%s;font-family:Share Tech Mono,monospace;margin-left:8px;'>Act: %s</span>", if(over_hit) "#7aba7a" else if(under_hit) "#aa5a5a" else "var(--text-muted)", as.integer(actual_total)) else ""
      
      o_odds_str <- fmt_odds(total_odds_o)
      u_odds_str <- fmt_odds(total_odds_u)
      
      total_html <- sprintf("<div style='display:flex;align-items:center;gap:12px;padding:6px 14px;background:var(--bg-base);border-top:1px solid var(--border);'>
        <span style='font-size:11px;font-family:Share Tech Mono,monospace;color:var(--text-muted);letter-spacing:.08em;min-width:40px;'>TOTAL</span>
        <div style='display:flex;align-items:center;gap:8px;'>
          <div style='background:var(--bg-card);border:1px solid var(--border);border-radius:6px;padding:3px 10px;display:flex;flex-direction:column;align-items:center;%s'>
            <span style='font-size:11px;color:var(--text-muted);'>O %s</span><span style='font-size:11px;color:var(--text-muted);'>%s</span>
          </div>
          <div style='background:var(--bg-card);border:1px solid var(--border);border-radius:6px;padding:3px 10px;display:flex;flex-direction:column;align-items:center;%s'>
            <span style='font-size:11px;color:var(--text-muted);'>U %s</span><span style='font-size:11px;color:var(--text-muted);'>%s</span>
          </div>
          %s
        </div>
      </div>",
                            if(over_hit) "background:#0a180a;border-color:#1a3a1a;" else "", total_str, o_odds_str,
                            if(under_hit) "background:#180a0a;border-color:#3a1a1a;" else "", total_str, u_odds_str,
                            act_str)
      
      # Column header
      col_hdr <- "<div style='display:flex;align-items:center;padding:4px 14px;font-size:10px;font-family:Share Tech Mono,monospace;color:var(--text-muted);letter-spacing:.08em;gap:10px;'>
        <span style='flex:1;min-width:180px;'>TEAM</span><span style='min-width:80px;text-align:center;'>SPREAD</span><span style='min-width:70px;text-align:center;'>ML</span><span style='min-width:36px;text-align:right;'>PTS</span>
      </div>"
      
      sprintf("<div style='background:var(--bg-card);border:1px solid var(--border);border-radius:10px;overflow:hidden;max-width:560px;'>%s%s%s%s</div>", col_hdr, away_row_html, home_row_html, total_html)
    }), collapse = "<div style='height:12px;'></div>")
    
    # Banner
    banner <- tags$div(class="to-banner",
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_games),     tags$div(class="to-banner-lbl","Games")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_fav_win),   tags$div(class="to-banner-lbl","Fav Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_dog_win),   tags$div(class="to-banner-lbl","Dog Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_home_win),  tags$div(class="to-banner-lbl","Home Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_away_win),  tags$div(class="to-banner-lbl","Away Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_h_dog_win), tags$div(class="to-banner-lbl","Home Dog Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_a_dog_win), tags$div(class="to-banner-lbl","Away Dog Wins"))
    )
    
    tagList(banner, HTML(cards_html))
  })
  
  
  output$po_favdog_ui <- renderUI({
    
    # SECTION 1: Per-Team Breakdown
    fd <- team_data %>%
      group_by(TEAM) %>%
      summarise(
        games   = n(),
        dog_w   = sum(as.numeric(dog_win), na.rm=TRUE),
        dog_l   = sum(as.numeric(dog_loss), na.rm=TRUE),
        fav_w   = sum(as.numeric(fav_win), na.rm=TRUE),
        fav_l   = sum(as.numeric(fav_loss), na.rm=TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        u_sum      = dog_w + dog_l,
        f_sum      = fav_w + fav_l,
        u_won_pct  = ifelse(u_sum > 0, round(dog_w / u_sum * 100, 1), NA),
        u_loss_pct = ifelse(u_sum > 0, round(dog_l / u_sum * 100, 1), NA),
        f_won_pct  = ifelse(f_sum > 0, round(fav_w / f_sum * 100, 1), NA),
        f_loss_pct = ifelse(f_sum > 0, round(fav_l / f_sum * 100, 1), NA)
      ) %>%
      arrange(TEAM)
    
    if(nrow(fd) == 0) return(tags$div(class="pr-placeholder", tags$p("No data available")))
    
    fd_pct_class <- function(val, all_vals, higher_is_better=TRUE) {
      if(is.na(val)) return("")
      vals <- all_vals[!is.na(all_vals)]
      n <- length(vals)
      if(n <= 1) return("fd-pct-5")
      rnk <- if(higher_is_better) rank(-all_vals, ties.method="average", na.last=TRUE)[match(val, all_vals)][1]
      else rank(all_vals, ties.method="average", na.last=TRUE)[match(val, all_vals)][1]
      pct <- (rnk - 1) / (n - 1)
      if(pct <= 0.11) "fd-pct-1" else if(pct <= 0.22) "fd-pct-2" else if(pct <= 0.33) "fd-pct-3"
      else if(pct <= 0.44) "fd-pct-4" else if(pct <= 0.56) "fd-pct-5"
      else if(pct <= 0.67) "fd-pct-6" else if(pct <= 0.78) "fd-pct-7"
      else if(pct <= 0.89) "fd-pct-8" else "fd-pct-9"
    }
    
    all_u_won_pct  <- fd$u_won_pct
    all_u_loss_pct <- fd$u_loss_pct
    all_f_won_pct  <- fd$f_won_pct
    all_f_loss_pct <- fd$f_loss_pct
    
    rows_html <- paste(sapply(seq_len(nrow(fd)), function(i) {
      r <- fd[i,]
      team_abv <- as.character(r$TEAM)
      logo_url <- NBA_TEAMS$logo[NBA_TEAMS$abv == team_abv]
      logo_html <- if(length(logo_url) > 0 && nchar(logo_url) > 0)
        sprintf("<img class='fd-logo' src='%s'/>", logo_url) else ""
      uw_cls <- fd_pct_class(r$u_won_pct, all_u_won_pct, TRUE)
      ul_cls <- fd_pct_class(r$u_loss_pct, all_u_loss_pct, FALSE)
      fw_cls <- fd_pct_class(r$f_won_pct, all_f_won_pct, TRUE)
      fl_cls <- fd_pct_class(r$f_loss_pct, all_f_loss_pct, FALSE)
      fmt_pct <- function(v) if(is.na(v)) "\u2014" else paste0(v, "%")
      paste0("<tr><td>", logo_html, "</td><td>", team_abv, "</td><td>", as.integer(r$games),
             "</td><td>", as.integer(r$u_sum), "</td><td>", as.integer(r$dog_w), "</td><td>", as.integer(r$dog_l),
             "</td><td class='", uw_cls, "'>", fmt_pct(r$u_won_pct), "</td><td class='", ul_cls, "'>", fmt_pct(r$u_loss_pct),
             "</td><td>", as.integer(r$fav_w), "</td><td>", as.integer(r$fav_l),
             "</td><td>", as.integer(r$f_sum),
             "</td><td class='", fw_cls, "'>", fmt_pct(r$f_won_pct), "</td><td class='", fl_cls, "'>", fmt_pct(r$f_loss_pct), "</td></tr>")
    }), collapse="")
    
    header_html <- "<thead><tr><th class='fd-grp-g' colspan='3'></th><th class='fd-grp-u' colspan='5'>UNDERDOG</th><th class='fd-grp-f' colspan='5'>FAVORITE</th></tr><tr><th></th><th>Team</th><th class='fd-grp-g'>GP</th><th class='fd-grp-u'>uSUM</th><th class='fd-grp-u'>uW</th><th class='fd-grp-u'>uL</th><th class='fd-grp-u'>uW%%</th><th class='fd-grp-u'>uL%%</th><th class='fd-grp-f'>fW</th><th class='fd-grp-f'>fL</th><th class='fd-grp-f'>fSUM</th><th class='fd-grp-f'>fW%%</th><th class='fd-grp-f'>fL%%</th></tr></thead>"
    
    total_dog_games <- sum(fd$u_sum, na.rm=TRUE)
    total_dog_wins  <- sum(fd$dog_w, na.rm=TRUE)
    total_fav_games <- sum(fd$f_sum, na.rm=TRUE)
    total_fav_wins  <- sum(fd$fav_w, na.rm=TRUE)
    dog_win_pct <- if(total_dog_games > 0) round(total_dog_wins / total_dog_games * 100, 1) else 0
    fav_win_pct <- if(total_fav_games > 0) round(total_fav_wins / total_fav_games * 100, 1) else 0
    
    # SECTION 2: Slate Size Analysis
    games_per_date <- team_data %>%
      distinct(ESPN_GAME_ID, GAME_DATE) %>%
      group_by(GAME_DATE) %>%
      summarise(slate_size = n(), .groups="drop")
    
    td_slate <- team_data %>%
      left_join(games_per_date, by="GAME_DATE")
    
    td_slate$slate_bucket <- cut(td_slate$slate_size,
                                 breaks=c(0, 4, 6, 9, 16),
                                 labels=c("1-4 Games", "5-6 Games", "7-9 Games", "10+ Games"),
                                 right=TRUE)
    
    slate_summary <- td_slate %>%
      filter(!is.na(slate_bucket)) %>%
      group_by(slate_bucket) %>%
      summarise(
        total_games = n_distinct(ESPN_GAME_ID),
        slates      = n_distinct(GAME_DATE),
        dog_wins    = sum(as.numeric(dog_win), na.rm=TRUE),
        dog_losses  = sum(as.numeric(dog_loss), na.rm=TRUE),
        fav_wins    = sum(as.numeric(fav_win), na.rm=TRUE),
        fav_losses  = sum(as.numeric(fav_loss), na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(
        dog_total   = dog_wins + dog_losses,
        fav_total   = fav_wins + fav_losses,
        dog_win_pct = ifelse(dog_total > 0, round(dog_wins / dog_total * 100, 1), 0),
        fav_win_pct = ifelse(fav_total > 0, round(fav_wins / fav_total * 100, 1), 0),
        avg_dog_wins = ifelse(slates > 0, round(dog_wins / slates, 1), 0),
        avg_fav_wins = ifelse(slates > 0, round(fav_wins / slates, 1), 0)
      )
    
    slate_detail <- td_slate %>%
      filter(!is.na(slate_bucket)) %>%
      group_by(GAME_DATE, slate_bucket) %>%
      summarise(dog_wins_tonight = sum(as.numeric(dog_win), na.rm=TRUE), .groups="drop")
    
    slate_cards_html <- paste(sapply(seq_len(nrow(slate_summary)), function(i) {
      s <- slate_summary[i,]
      bar_dog <- if(s$dog_win_pct + s$fav_win_pct > 0) round(s$dog_win_pct / (s$dog_win_pct + s$fav_win_pct) * 100) else 50
      paste0(
        "<div class='fd-slate-card'><div class='fd-slate-card-title'>", s$slate_bucket,
        "<span class='fd-slate-count'>", s$slates, " slates</span></div>",
        "<div class='fd-slate-stat-row'><span class='fd-slate-stat-label'>Total Games</span><span class='fd-slate-stat-val'>", s$total_games, "</span></div>",
        "<div class='fd-slate-stat-row'><span class='fd-slate-stat-label'>Dog Win Rate</span><span class='fd-slate-stat-val dog-color'>", s$dog_win_pct, "%</span></div>",
        "<div class='fd-slate-stat-row'><span class='fd-slate-stat-label'>Fav Win Rate</span><span class='fd-slate-stat-val fav-color'>", s$fav_win_pct, "%</span></div>",
        "<div class='fd-slate-stat-row'><span class='fd-slate-stat-label'>Avg Dog Wins / Slate</span><span class='fd-slate-stat-val dog-color'>", s$avg_dog_wins, "</span></div>",
        "<div class='fd-slate-stat-row'><span class='fd-slate-stat-label'>Avg Fav Wins / Slate</span><span class='fd-slate-stat-val fav-color'>", s$avg_fav_wins, "</span></div>",
        "<div class='fd-slate-bar'><div class='fd-slate-bar-dog' style='width:", bar_dog, "%;'></div><div class='fd-slate-bar-fav' style='width:", 100 - bar_dog, "%;'></div></div></div>")
    }), collapse="")
    
    slate_table_rows <- paste(sapply(seq_len(nrow(slate_summary)), function(i) {
      s <- slate_summary[i,]
      bucket_nights <- slate_detail %>% filter(slate_bucket == s$slate_bucket)
      if(nrow(bucket_nights) > 0) {
        mode_dog <- as.integer(names(sort(table(bucket_nights$dog_wins_tonight), decreasing=TRUE))[1])
        max_dog  <- max(bucket_nights$dog_wins_tonight)
        min_dog  <- min(bucket_nights$dog_wins_tonight)
        range_str <- paste0(min_dog, "-", max_dog, " (mode: ", mode_dog, ")")
      } else { range_str <- "\u2014" }
      paste0("<tr><td>", s$slate_bucket, "</td><td>", s$slates, "</td><td>", s$total_games,
             "</td><td>", s$dog_wins, "</td><td>", s$dog_losses,
             "</td><td style='color:#c8a020;font-weight:700;'>", s$dog_win_pct, "%</td>",
             "<td>", s$fav_wins, "</td><td>", s$fav_losses,
             "</td><td style='color:#5a7aaa;font-weight:700;'>", s$fav_win_pct, "%</td>",
             "<td>", s$avg_dog_wins, "</td><td>", range_str, "</td></tr>")
    }), collapse="")
    
    slate_table_header <- "<thead><tr><th>Slate Size</th><th>Slates</th><th>Games</th><th>Dog W</th><th>Dog L</th><th>Dog W%</th><th>Fav W</th><th>Fav L</th><th>Fav W%</th><th>Avg Dog W/Slate</th><th>Dog W Range (Mode)</th></tr></thead>"
    
    # RENDER
    tagList(
      tags$div(class="to-banner",
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", total_dog_games), tags$div(class="to-banner-lbl", "Dog Games")),
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", total_dog_wins), tags$div(class="to-banner-lbl", "Dog Wins")),
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", paste0(dog_win_pct, "%")), tags$div(class="to-banner-lbl", "Dog Win Rate")),
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", total_fav_games), tags$div(class="to-banner-lbl", "Fav Games")),
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", total_fav_wins), tags$div(class="to-banner-lbl", "Fav Wins")),
               tags$div(class="to-banner-stat", tags$div(class="to-banner-val", paste0(fav_win_pct, "%")), tags$div(class="to-banner-lbl", "Fav Win Rate"))
      ),
      tags$div(style="overflow-x:auto; border:1px solid var(--border); border-radius:8px; background:var(--bg-card); margin-top:12px;",
               HTML(paste0("<table class='fd-table'>", header_html, "<tbody>", rows_html, "</tbody></table>"))
      ),
      tags$div(class="fd-slate-section",
               tags$div(class="fd-section-title", "Slate Size Analysis"),
               tags$div(class="fd-section-sub", "Dog and favorite win rates by number of games on the slate"),
               HTML(paste0("<div class='fd-slate-cards'>", slate_cards_html, "</div>")),
               tags$div(style="overflow-x:auto; border:1px solid var(--border); border-radius:8px; background:var(--bg-card);",
                        HTML(paste0("<table class='fd-slate-table'>", slate_table_header, "<tbody>", slate_table_rows, "</tbody></table>"))
               )
      )
    )
  })
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # INJURY DASHBOARD
  # ═══════════════════════════════════════════════════════════════════════════════
  
  inj_tab <- reactiveVal("home")
  inj_search <- reactiveVal("")
  observeEvent(input$inj_tab_click,    { inj_tab(input$inj_tab_click) })
  observeEvent(input$inj_search_input, { inj_search(tolower(trimws(input$inj_search_input))) })
  
  output$injury_dashboard_ui <- renderUI({
    tab <- inj_tab()
    
    tagList(
      # Tab nav - same style as Player Rotations
      tags$div(class="po-subnav", style="margin-bottom:0;",
               tags$button(class=paste("po-tab", if(tab=="home") "active" else ""),
                           onclick="Shiny.setInputValue('inj_tab_click','home',{priority:'event'})", "Home"),
               tags$button(class=paste("po-tab", if(tab=="history") "active" else ""),
                           onclick="Shiny.setInputValue('inj_tab_click','history',{priority:'event'})", "Injury History")
      ),
      if(tab=="home")    uiOutput("inj_home_ui")
      else               uiOutput("inj_history_ui")
    )
  })
  
  # ── HOME TAB ──
  output$inj_home_ui <- renderUI({
    today      <- Sys.Date()
    all_dates  <- sort(unique(injury_data$date[!is.na(injury_data$date)]), decreasing=TRUE)
    latest_date <- if(length(all_dates)>0) all_dates[1] else today
    
    # Active statuses we care about
    active_statuses <- c("Out","Questionable","Doubtful","Day-To-Day","GTD")
    
    # Get today's report (or latest available)
    today_df <- injury_data[!is.na(injury_data$date) & injury_data$date == latest_date &
                              injury_data$status %in% active_statuses, ]
    
    # For players NOT in today's report but were recently "Out" — check last 14 days
    recent_cutoff <- today - 14
    recent_out <- injury_data[!is.na(injury_data$date) &
                                injury_data$date >= recent_cutoff &
                                injury_data$date < latest_date &
                                injury_data$status == "Out", ]
    
    # Keep only players absent from today's report
    if(nrow(today_df) > 0 && "player_clean" %in% names(today_df)) {
      already_reported <- unique(today_df$player_clean)
    } else {
      already_reported <- character(0)
    }
    if("player_clean" %in% names(recent_out)) {
      recent_out <- recent_out[!recent_out$player_clean %in% already_reported, ]
      # Find players who have ANY "Available" entry on their most recent date — exclude them
      # Sort by date desc, then prioritize "Available" status within same date so it wins dedup
      latest_per_player <- injury_data[!is.na(injury_data$date), ]
      # Get most recent date per player, then check if Available appears on that date
      latest_date_per_player <- tapply(latest_per_player$date, latest_per_player$player_clean, max)
      available_players <- names(latest_date_per_player)[sapply(names(latest_date_per_player), function(pc) {
        max_date <- latest_date_per_player[pc]
        any(trimws(injury_data$status[injury_data$player_clean == pc & !is.na(injury_data$date) & injury_data$date == max_date]) == "Available")
      })]
      available_players <- toupper(trimws(available_players))
      recent_out <- recent_out[!toupper(trimws(recent_out$player_clean)) %in% available_players, ]
      # Keep only most recent Out entry per remaining player
      recent_out <- recent_out[order(recent_out$date, decreasing=TRUE), ]
      recent_out <- recent_out[!duplicated(recent_out$player_clean), ]
      recent_out$still_out_flag <- TRUE
    }
    
    # Combine
    if(nrow(recent_out) > 0) {
      combined <- dplyr::bind_rows(today_df, recent_out)
    } else {
      combined <- today_df
    }
    
    if(nrow(combined) == 0) {
      return(tags$div(class="pr-placeholder", tags$p("No injury data available")))
    }
    
    # Status dot colors
    status_dot <- function(s) {
      col <- switch(as.character(s),
                    "Out"        = "#e53935",
                    "Doubtful"   = "#e57373",
                    "Questionable"= "#fb8c00",
                    "Day-To-Day" = "#fdd835",
                    "GTD"        = "#fdd835",
                    "#aaaaaa"
      )
      sprintf("<span style='display:inline-block;width:8px;height:8px;border-radius:50%%;background:%s;margin-right:5px;'></span>", col)
    }
    
    safe_img <- function(url, w, h, style="") {
      if(is.na(url) || nchar(as.character(url)) < 5) return("")
      sprintf("<img src='%s' style='width:%spx;height:%spx;object-fit:contain;%s'/>", url, w, h, style)
    }
    
    # All 30 NBA teams
    all_teams <- sort(unique(as.character(combined$team_long_name[!is.na(combined$team_long_name)])))
    if(length(all_teams) == 0) all_teams <- sort(unique(as.character(combined$team)))
    
    teams_html <- paste(sapply(all_teams, function(tm) {
      # Get rows for this team
      if("team_long_name" %in% names(combined)) {
        t_rows <- combined[!is.na(combined$team_long_name) & combined$team_long_name == tm, ]
      } else {
        t_rows <- combined[!is.na(combined$team) & combined$team == tm, ]
      }
      if(nrow(t_rows) == 0) return("")
      
      # Team logo + header
      logo_url <- if("team_logo" %in% names(t_rows) && !is.na(t_rows$team_logo[1])) as.character(t_rows$team_logo[1]) else ""
      logo_html <- safe_img(logo_url, 40, 40, "vertical-align:middle;margin-right:8px;")
      team_header <- sprintf("<div class='inj-team-header'>%s<span>%s</span></div>", logo_html, tm)
      
      # Column header
      col_hdr <- "<div class='inj-col-header'><span style='flex:1.8;'>NAME</span><span style='flex:0.5;'>POS</span><span style='flex:1;'>EST. RETURN</span><span style='flex:1;'>STATUS</span><span style='flex:3;'>COMMENT</span></div>"
      
      # Player rows
      player_rows <- paste(sapply(seq_len(nrow(t_rows)), function(i) {
        r       <- t_rows[i,]
        pname   <- if("player" %in% names(r)) as.character(r$player) else "—"
        pos     <- "—"  # not in data
        status  <- if("status" %in% names(r)) as.character(r$status) else "—"
        reason  <- if("reason" %in% names(r) && !is.na(r$reason)) as.character(r$reason) else "—"
        hs_url  <- if("headshot" %in% names(r) && !is.na(r$headshot)) as.character(r$headshot) else ""
        hs_html <- if(nchar(hs_url)>5) sprintf("<img src='%s' style='width:52px;height:52px;border-radius:50%%;object-fit:cover;object-position:top;margin-right:12px;flex-shrink:0;'/>", hs_url) else "<div style='width:52px;height:52px;border-radius:50%;background:var(--bg-base);margin-right:12px;flex-shrink:0;'></div>"
        
        # Return date - use reason to infer if available, otherwise blank
        ret_date <- "—"
        
        # Still out flag
        still_out <- isTRUE(r$still_out_flag)
        still_out_badge <- if(still_out) "<span style='font-size:12px;background:#1a1a1a;border:1px solid #444;border-radius:3px;padding:2px 6px;color:#888;margin-left:8px;'>still out</span>" else ""
        
        # Player name links to player lookup
        player_id <- if("espn_player_id" %in% names(r) && !is.na(r$espn_player_id)) as.character(r$espn_player_id) else ""
        name_link <- sprintf("<span class=\"inj-player-name\" style=\"cursor:pointer;color:var(--gold);\" onclick=\"Shiny.setInputValue('inj_player_click','%s',{priority:'event'})\">%s</span>%s", pname, pname, still_out_badge)
        
        sprintf("<div class='inj-player-row'><span style='flex:1.8;display:flex;align-items:center;'>%s%s</span><span style='flex:0.5;color:var(--text-muted);font-size:15px;'>%s</span><span style='flex:1;color:var(--text-muted);font-size:15px;'>%s</span><span style='flex:1;display:flex;align-items:center;font-size:15px;'>%s%s</span><span style='flex:3;color:var(--text-muted);font-size:15px;line-height:1.6;'>%s</span></div>",
                hs_html, name_link, pos, ret_date, status_dot(status), status, reason)
      }), collapse="")
      
      sprintf("<div class='inj-team-block'>%s%s%s</div>", team_header, col_hdr, player_rows)
    }), collapse="")
    
    HTML(sprintf("<div class='inj-home-wrap'>%s</div>", teams_html))
  })
  
  # ── HISTORY TAB ──
  output$inj_history_ui <- renderUI({
    tagList(
      # Static search bar — rendered once, never re-rendered so focus is preserved
      tags$div(class="po-filters", style="margin-bottom:12px;",
               tags$div(style="position:relative;",
                        tags$svg(viewBox="0 0 24 24", style="position:absolute;left:8px;top:50%;transform:translateY(-50%);width:13px;height:13px;fill:none;stroke:var(--text-muted);stroke-width:2;pointer-events:none;",
                                 tags$circle(cx="11",cy="11",r="8"),
                                 tags$path(d="m21 21-4.35-4.35")
                        ),
                        tags$input(
                          id="inj_search_input",
                          type="text",
                          placeholder="Search player or team...",
                          oninput="clearTimeout(window._injTimer); window._injTimer = setTimeout(function(){ Shiny.setInputValue('inj_search_input', document.getElementById('inj_search_input').value, {priority:'event'}); }, 300);",
                          style="background:var(--bg-card);border:1px solid var(--border);border-radius:6px;color:var(--text-primary);font-family:'Share Tech Mono',monospace;font-size:11px;padding:5px 10px 5px 28px;width:220px;outline:none;"
                        )
               )
      ),
      uiOutput("inj_history_table_ui"),
      # Escape clears search
      tags$script(HTML("
        document.addEventListener('keydown', function(e) {
          if(e.key === 'Escape') {
            var box = document.getElementById('inj_search_input');
            if(box) { box.value = ''; Shiny.setInputValue('inj_search_input', '', {priority:'event'}); }
          }
        });
      "))
    )
  })
  
  output$inj_history_table_ui <- renderUI({
    sq <- inj_search()
    df <- injury_data[!is.na(injury_data$date), ]
    df <- df[order(df$date, decreasing=TRUE), ]
    
    if(nchar(sq) > 0) {
      p_col <- tolower(as.character(df$player))
      t_col <- if("team_long_name" %in% names(df)) tolower(as.character(df$team_long_name)) else tolower(as.character(df$team))
      df <- df[grepl(sq, paste(p_col, t_col), fixed=TRUE), ]
    }
    
    if(nrow(df) == 0) return(tags$div(class="pr-placeholder", tags$p("No injury history found")))
    
    status_dot <- function(s) {
      col <- switch(as.character(s),
                    "Out"         = "#e53935",
                    "Doubtful"    = "#e57373",
                    "Questionable"= "#fb8c00",
                    "Day-To-Day"  = "#fdd835",
                    "GTD"         = "#fdd835",
                    "#aaaaaa"
      )
      sprintf("<span style='display:inline-block;width:8px;height:8px;border-radius:50%%;background:%s;margin-right:5px;'></span>", col)
    }
    
    hdr <- "<thead><tr><th style='min-width:90px;font-size:15px;'>DATE</th><th style='min-width:200px;font-size:15px;'>PLAYER</th><th style='font-size:15px;'>STATUS</th><th style='min-width:250px;font-size:15px;'>REASON</th><th style='font-size:15px;'>REPORTTIME</th></tr></thead>"
    
    rows_html <- paste(sapply(seq_len(min(nrow(df), 1000)), function(i) {
      r       <- df[i,]
      dt      <- tryCatch(format(as.Date(r$date), "%m/%d/%Y"), error=function(e) as.character(r$date))
      pname   <- if("player" %in% names(r) && !is.na(r$player)) as.character(r$player) else "—"
      status  <- if("status" %in% names(r) && !is.na(r$status)) as.character(r$status) else "—"
      reason  <- if("reason" %in% names(r) && !is.na(r$reason)) as.character(r$reason) else "—"
      rtime   <- if("reportTime" %in% names(r) && !is.na(r$reportTime)) as.character(r$reportTime) else "—"
      hs_url  <- if("headshot" %in% names(r) && !is.na(r$headshot) && nchar(as.character(r$headshot))>5) as.character(r$headshot) else ""
      tl_url  <- if("team_logo" %in% names(r) && !is.na(r$team_logo) && nchar(as.character(r$team_logo))>5) as.character(r$team_logo) else ""
      hs_html <- if(nchar(hs_url)>0) sprintf("<img src='%s' style='width:52px;height:52px;border-radius:50%%;object-fit:cover;object-position:top;margin-right:10px;flex-shrink:0;vertical-align:middle;'/>", hs_url) else ""
      tl_html <- if(nchar(tl_url)>0) sprintf("<img src='%s' style='width:36px;height:36px;object-fit:contain;margin-right:8px;vertical-align:middle;'/>", tl_url) else ""
      
      sprintf("<tr><td style='color:var(--text-muted);font-size:15px;'>%s</td><td><div style='display:flex;align-items:center;'>%s%s<span style='color:var(--gold);font-size:16px;'>%s</span></div></td><td style='white-space:nowrap;'><div style='display:flex;align-items:center;'>%s<span style='font-size:15px;'>%s</span></div></td><td style='font-size:15px;color:var(--text-muted);max-width:300px;'>%s</td><td style='font-size:15px;color:var(--text-muted);white-space:nowrap;'>%s</td></tr>",
              dt, hs_html, tl_html, pname, status_dot(status), status, reason, rtime)
    }), collapse="")
    
    HTML(sprintf("<div class='po-table-wrap'><table class='po-table'><thead>%s</thead><tbody>%s</tbody></table></div>", hdr, rows_html))
  })
  
  # Wire player name click -> jump to player stats lookup
  observeEvent(input$inj_player_click, {
    pname <- input$inj_player_click
    updateTextInput(session, "selected_player", value=pname)
    sub_active("player_stats")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # SHOT DETAIL CHART
  # ════════════════════════════════════════════════════════════════════════════
  
  # Helper: mini calendar HTML
  render_mini_cal <- function(y, m, game_dates, selected_date, input_prefix) {
    first_day <- as.Date(paste(y, m, "01", sep="-"))
    start_dow <- as.integer(format(first_day, "%w"))
    cal_start <- first_day - start_dow
    today <- Sys.Date()
    nav_html <- sprintf(
      "<div class='mini-cal-nav'><button class='mini-cal-btn' onclick=\"Shiny.setInputValue('%s_cal_prev',Math.random())\">&#8249;</button><span class='mini-cal-month'>%s</span><button class='mini-cal-btn' onclick=\"Shiny.setInputValue('%s_cal_next',Math.random())\">&#8250;</button></div>",
      input_prefix, format(first_day, "%B %Y"), input_prefix)
    dow_html <- paste(sapply(c("S","M","T","W","T","F","S"), function(d) sprintf("<div class='mini-cal-dow'>%s</div>", d)), collapse="")
    days_html <- paste(sapply(0:41, function(i) {
      cd <- cal_start + i
      is_cur <- format(cd, "%Y-%m") == format(first_day, "%Y-%m")
      has_game <- cd %in% game_dates
      is_sel <- !is.null(selected_date) && cd == selected_date
      is_today <- cd == today
      cls <- paste("mini-cal-day", if(is_cur) "cur-month" else "", if(has_game) "has-game" else "",
                   if(is_sel) "selected" else "", if(is_today) "today" else "")
      onclick <- if(has_game) sprintf(" onclick=\"Shiny.setInputValue('%s_game_select','%s',{priority:'event'})\"", input_prefix, as.character(cd)) else ""
      sprintf("<div class='%s'%s>%d</div>", trimws(cls), onclick, as.integer(format(cd, "%%d")))
    }), collapse="")
    sprintf("<div class='mini-cal-wrap'>%s<div class='mini-cal-grid'>%s%s</div></div>", nav_html, dow_html, days_html)
  }
  
  # Reactive: filtered PBP for shot chart
  sc_pbp <- reactive({
    gid <- sc_picked_game()
    if(is.null(gid) || gid == "") {
      sel_date <- tryCatch(as.Date(sc_game_id()), error=function(e) NULL)
      if(is.null(sel_date)) return(NULL)
      day_games <- pbp_data %>% filter(game_date == sel_date)
      if(nrow(day_games) == 0) return(NULL)
      gid <- as.character(day_games$game_id[1])
      sc_picked_game(gid)
    }
    pbp_data %>% filter(as.character(game_id) == as.character(gid))
  })
  
  # ── Main Shot Chart UI ──
  output$sc_ui <- renderUI({
    all_dates <- sort(unique(pbp_data$game_date))
    sel_date_str <- sc_game_id()
    sel_date <- if(!is.null(sel_date_str)) tryCatch(as.Date(sel_date_str), error=function(e) NULL) else NULL
    has_data <- !is.null(sel_date) && !is.null(sc_pbp()) && nrow(sc_pbp()) > 0
    
    tags$div(class="sc-layout",
             tags$div(class="sc-top-row",
                      tags$div(class="sc-court-col",
                               HTML(render_mini_cal(sc_cal_year(), sc_cal_month(), all_dates, sel_date, "sc")),
                               uiOutput("sc_game_picker_ui"),
                               if(has_data) uiOutput("sc_filters_ui") else NULL,
                               if(has_data) uiOutput("sc_court_ui")
                               else tags$div(class="pr-placeholder", style="margin-top:40px;", tags$p("Select a date with games to view shot chart"))
                      ),
                      if(has_data) tags$div(class="sc-sidebar-col",
                                            uiOutput("sc_team_stats_ui"),
                                            uiOutput("sc_game_leaders_ui")
                      ) else NULL
             ),
             if(has_data) uiOutput("sc_oncourt_ui") else NULL
    )
  })
  
  
  # ── Game picker dropdown ──
  output$sc_game_picker_ui <- renderUI({
    sel_date <- tryCatch(as.Date(sc_game_id()), error=function(e) NULL)
    if(is.null(sel_date)) return(NULL)
    day_games <- pbp_data %>% filter(game_date == sel_date) %>%
      distinct(game_id, away_team_abbrev, home_team_abbrev)
    if(nrow(day_games) == 0) return(tags$div(style="padding:8px;color:var(--text-muted);font-size:12px;","No PBP data for this date"))
    tags$div(style="margin:8px 0;",
             tags$select(class="sc-filter-select", style="width:100%;",
                         onchange="Shiny.setInputValue('sc_game_picked',this.value,{priority:'event'})",
                         lapply(seq_len(nrow(day_games)), function(i)
                           tags$option(value=as.character(day_games$game_id[i]),
                                       paste(day_games$away_team_abbrev[i], "@", day_games$home_team_abbrev[i])))
             ))
  })
  
  # ── Filters ──
  output$sc_filters_ui <- renderUI({
    df <- sc_pbp(); if(is.null(df)) return(NULL)
    qtrs <- sort(unique(df$qtr[!is.na(df$qtr)]))
    qtr_labels <- sapply(qtrs, function(q) if(q<=4) paste0("Q",q) else paste0("OT",q-4))
    players <- df %>% filter(PLAYER_NAME != "" & !is.na(PLAYER_NAME)) %>% distinct(PLAYER_NAME, team_id)
    away_players <- players %>% filter(team_id == df$away_team_id[1])
    home_players <- players %>% filter(team_id == df$home_team_id[1])
    tags$div(class="sc-filters",
             tags$select(class="sc-filter-select", onchange="Shiny.setInputValue('sc_qtr_click',this.value,{priority:'event'})",
                         tags$option(value="ALL","All Quarters"), lapply(seq_along(qtrs), function(i) tags$option(value=qtrs[i], qtr_labels[i]))),
             tags$select(class="sc-filter-select", onchange="Shiny.setInputValue('sc_type_sel',this.value,{priority:'event'})",
                         tags$option(value="ALL","All Play Types"), tags$option(value="2PT FG Made"), tags$option(value="2PT FG Missed"),
                         tags$option(value="3PT Made"), tags$option(value="3PT Missed")),
             tags$select(class="sc-filter-select", onchange="Shiny.setInputValue('sc_player_sel',this.value,{priority:'event'})",
                         tags$option(value="ALL","All Players"),
                         tags$optgroup(label=df$away_team_abbrev[1], lapply(away_players$PLAYER_NAME, function(p) tags$option(value=p, p))),
                         tags$optgroup(label=df$home_team_abbrev[1], lapply(home_players$PLAYER_NAME, function(p) tags$option(value=p, p))))
    )
  })
  
  # ── Court SVG with headshot dots ──
  output$sc_court_ui <- renderUI({
    df <- sc_pbp(); if(is.null(df)) return(NULL)
    
    away_abv <- df$away_team_abbrev[1]; home_abv <- df$home_team_abbrev[1]
    
    # Get team colors from data
    away_color <- if("opp_color" %in% names(df) && !is.na(df$team_color[1])) {
      # The away team's color: find a row where the away team is the acting team
      away_rows <- df[df$team_id == df$away_team_id[1] & !is.na(df$team_color), ]
      if(nrow(away_rows) > 0) paste0("#", gsub("^#","", away_rows$team_color[1])) else "#e53935"
    } else "#e53935"
    home_color <- {
      home_rows <- df[df$team_id == df$home_team_id[1] & !is.na(df$team_color), ]
      if(nrow(home_rows) > 0) paste0("#", gsub("^#","", home_rows$team_color[1])) else "#1565c0"
    }
    
    # Get logos from data
    home_logo_url <- {
      hr <- df[!is.na(df$team_logo) & df$team_id == df$home_team_id[1], ]
      if(nrow(hr)>0 && nchar(as.character(hr$team_logo[1]))>5) as.character(hr$team_logo[1])
      else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(home_abv), ".png")
    }
    away_logo_url <- {
      ar <- df[!is.na(df$opp_logo) & df$team_id == df$home_team_id[1], ]
      if(nrow(ar)>0 && nchar(as.character(ar$opp_logo[1]))>5) as.character(ar$opp_logo[1])
      else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(away_abv), ".png")
    }
    
    # Filter shots
    shots <- df %>% filter(shooting_play == TRUE, !is.na(LOC_X), !is.na(LOC_Y))
    qf <- sc_qtr_filter(); if(qf != "ALL") shots <- shots %>% filter(qtr == as.integer(qf))
    tf <- sc_type_filter(); if(tf != "ALL") shots <- shots %>% filter(shot_category == tf)
    pf <- sc_player_filter(); if(pf != "ALL") shots <- shots %>% filter(PLAYER_NAME == pf)
    
    # Court lines
    court_lines <- '
    <rect x="-250" y="-52" width="500" height="472" fill="#e8d5b5" rx="0"/>
    <rect x="-80" y="-52" width="160" height="190" fill="none" stroke="#333" stroke-width="1.5"/>
    <circle cx="0" cy="138" r="60" fill="none" stroke="#333" stroke-width="1.5"/>
    <path d="M -40 -52 A 40 40 0 0 0 40 -52" fill="none" stroke="#333" stroke-width="1.5"/>
    <path d="M -220 -52 L -220 88 A 237.5 237.5 0 0 0 220 88 L 220 -52" fill="none" stroke="#333" stroke-width="1.5"/>
    <circle cx="0" cy="0" r="7.5" fill="none" stroke="#333" stroke-width="1.5"/>
    <line x1="-30" y1="-8" x2="30" y2="-8" stroke="#333" stroke-width="2"/>
    <line x1="-250" y1="418" x2="250" y2="418" stroke="#333" stroke-width="1.5"/>
    <path d="M -60 418 A 60 60 0 0 1 60 418" fill="none" stroke="#333" stroke-width="1.5"/>
    <circle cx="0" cy="418" r="36" fill="rgba(255,255,255,.3)" stroke="none"/>'
    
    # Defs for clipping headshots
    defs_svg <- "<defs>"
    dots_svg <- ""
    
    for(i in seq_len(nrow(shots))) {
      s <- shots[i,]
      x <- s$LOC_X; y <- s$LOC_Y
      made <- isTRUE(s$scoring_play)
      is_home <- isTRUE(s$team_id == s$home_team_id)
      col <- if(is_home) home_color else away_color
      hs_url <- if("athlete_id_1_headshot" %in% names(s) && !is.na(s$athlete_id_1_headshot) && nchar(as.character(s$athlete_id_1_headshot)) > 10) as.character(s$athlete_id_1_headshot) else ""
      
      clip_id <- paste0("sc", i)
      defs_svg <- paste0(defs_svg, sprintf("<clipPath id='%s'><circle cx='%s' cy='%s' r='7'/></clipPath>", clip_id, round(x,1), round(y,1)))
      
      if(nchar(hs_url) > 10) {
        # Headshot dot
        if(made) {
          dots_svg <- paste0(dots_svg, sprintf(
            "<circle cx='%s' cy='%s' r='8' fill='%s' opacity='0.9'/><image href='%s' x='%s' y='%s' width='14' height='14' clip-path='url(#%s)' preserveAspectRatio='xMidYMid slice'/>",
            round(x,1), round(y,1), col, hs_url, round(x-7,1), round(y-7,1), clip_id))
        } else {
          dots_svg <- paste0(dots_svg, sprintf(
            "<circle cx='%s' cy='%s' r='8' fill='none' stroke='%s' stroke-width='2' opacity='0.7'/><image href='%s' x='%s' y='%s' width='14' height='14' clip-path='url(#%s)' preserveAspectRatio='xMidYMid slice' opacity='0.35'/>",
            round(x,1), round(y,1), col, hs_url, round(x-7,1), round(y-7,1), clip_id))
        }
      } else {
        # Fallback: plain dot
        if(made) {
          dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='5' fill='%s' stroke='%s' stroke-width='1' opacity='0.85'/>", round(x,1), round(y,1), col, col))
        } else {
          dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='5' fill='none' stroke='%s' stroke-width='2' opacity='0.7'/>", round(x,1), round(y,1), col))
        }
      }
    }
    defs_svg <- paste0(defs_svg, "</defs>")
    
    logo_svg <- sprintf("<image href='%s' x='-28' y='390' width='56' height='56' opacity='0.6'/>", home_logo_url)
    
    full_svg <- sprintf("<svg class='sc-court-svg' viewBox='-260 -60 520 490' xmlns='http://www.w3.org/2000/svg'>%s%s%s%s</svg>",
                        defs_svg, court_lines, dots_svg, logo_svg)
    
    tags$div(class="sc-court-wrap",
             HTML(full_svg),
             tags$div(class="sc-key",
                      tags$div(class="sc-key-item",
                               tags$img(src=away_logo_url, style="width:20px;height:20px;object-fit:contain;"),
                               tags$span(away_abv),
                               tags$span(class="sc-key-dot made", style=sprintf("border-color:%s;background:%s;",away_color,away_color)), "Made",
                               tags$span(class="sc-key-dot missed", style=sprintf("border-color:%s;",away_color)), "Missed"),
                      tags$div(class="sc-key-item",
                               tags$img(src=home_logo_url, style="width:20px;height:20px;object-fit:contain;"),
                               tags$span(home_abv),
                               tags$span(class="sc-key-dot made", style=sprintf("border-color:%s;background:%s;",home_color,home_color)), "Made",
                               tags$span(class="sc-key-dot missed", style=sprintf("border-color:%s;",home_color)), "Missed")
             )
    )
  })
  
  # ── Team Stats Bars ──
  output$sc_team_stats_ui <- renderUI({
    df <- sc_pbp(); if(is.null(df)) return(NULL)
    away_abv <- df$away_team_abbrev[1]; home_abv <- df$home_team_abbrev[1]
    away_tid <- df$away_team_id[1]; home_tid <- df$home_team_id[1]
    
    away_color <- { ar <- df[df$team_id==away_tid & !is.na(df$team_color),]; if(nrow(ar)>0) paste0("#",gsub("^#","",ar$team_color[1])) else "#e53935" }
    home_color <- { hr <- df[df$team_id==home_tid & !is.na(df$team_color),]; if(nrow(hr)>0) paste0("#",gsub("^#","",hr$team_color[1])) else "#1565c0" }
    
    shots <- df %>% filter(shooting_play == TRUE)
    a_sh <- shots %>% filter(team_id==away_tid); h_sh <- shots %>% filter(team_id==home_tid)
    a_fgm <- sum(a_sh$scoring_play,na.rm=T); a_fga <- nrow(a_sh); h_fgm <- sum(h_sh$scoring_play,na.rm=T); h_fga <- nrow(h_sh)
    a_fg <- if(a_fga>0) round(a_fgm/a_fga*100) else 0; h_fg <- if(h_fga>0) round(h_fgm/h_fga*100) else 0
    a3 <- a_sh %>% filter(points_attempted==3); h3 <- h_sh %>% filter(points_attempted==3)
    a3m <- sum(a3$scoring_play,na.rm=T); a3a <- nrow(a3); h3m <- sum(h3$scoring_play,na.rm=T); h3a <- nrow(h3)
    a3p <- if(a3a>0) round(a3m/a3a*100) else 0; h3p <- if(h3a>0) round(h3m/h3a*100) else 0
    a_tov <- nrow(df %>% filter(team_id==away_tid, grepl("turnover|Turnover",type_text,ignore.case=T)))
    h_tov <- nrow(df %>% filter(team_id==home_tid, grepl("turnover|Turnover",type_text,ignore.case=T)))
    a_reb <- nrow(df %>% filter(team_id==away_tid, grepl("rebound|Rebound",type_text,ignore.case=T)))
    h_reb <- nrow(df %>% filter(team_id==home_tid, grepl("rebound|Rebound",type_text,ignore.case=T)))
    
    bar <- function(av,hv,al,hl,nm,ad="",hd="") {
      tot<-av+hv; ap<-if(tot>0) round(av/tot*100) else 50; hp<-100-ap
      sprintf("<div class='sc-stat-row'><div class='sc-stat-away'>%s <span class='sc-stat-detail'>%s</span></div><div class='sc-stat-label'>%s</div><div class='sc-stat-home'><span class='sc-stat-detail'>%s</span> %s</div></div><div style='padding:0 8px;'><div class='sc-bar-wrap'><div class='sc-bar-away' style='width:%s%%;background:%s;'></div><div class='sc-bar-home' style='width:%s%%;background:%s;'></div></div></div>",
              al,ad,nm,hd,hl,ap,away_color,hp,home_color)
    }
    
    away_logo <- { ar<-df[!is.na(df$team_logo)&df$team_id==away_tid,]; if(nrow(ar)>0) as.character(ar$team_logo[1]) else "" }
    home_logo <- { hr<-df[!is.na(df$team_logo)&df$team_id==home_tid,]; if(nrow(hr)>0) as.character(hr$team_logo[1]) else "" }
    
    tags$div(class="sc-stats-card",
             tags$div(class="sc-stats-title","Team Stats"),
             HTML(sprintf("<div style='display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;'><div style='display:flex;align-items:center;gap:6px;'><img src='%s' style='width:24px;height:24px;'/><span style='font-family:Rajdhani;font-weight:700;font-size:14px;color:var(--text-primary);'>%s</span></div><div style='display:flex;align-items:center;gap:6px;'><span style='font-family:Rajdhani;font-weight:700;font-size:14px;color:var(--text-primary);'>%s</span><img src='%s' style='width:24px;height:24px;'/></div></div>",
                          away_logo, away_abv, home_abv, home_logo)),
             HTML(bar(a_fg,h_fg,paste0(a_fg,"%"),paste0(h_fg,"%"),"Field Goal %",sprintf("(%s-%s)",a_fgm,a_fga),sprintf("(%s-%s)",h_fgm,h_fga))),
             HTML(bar(a3p,h3p,paste0(a3p,"%"),paste0(h3p,"%"),"Three Point %",sprintf("(%s-%s)",a3m,a3a),sprintf("(%s-%s)",h3m,h3a))),
             HTML(bar(a_tov,h_tov,a_tov,h_tov,"Turnovers")),
             HTML(bar(a_reb,h_reb,a_reb,h_reb,"Rebounds"))
    )
  })
  
  # ── Game Leaders ──
  output$sc_game_leaders_ui <- renderUI({
    df <- sc_pbp(); if(is.null(df)) return(NULL)
    scoring <- df %>% filter(scoring_play==TRUE, PLAYER_NAME!="") %>%
      group_by(PLAYER_NAME, team_id) %>% summarise(pts=sum(score_value,na.rm=T), .groups="drop")
    rebs <- df %>% filter(grepl("rebound|Rebound",type_text,ignore.case=T), PLAYER_NAME!="") %>%
      group_by(PLAYER_NAME, team_id) %>% summarise(reb=n(), .groups="drop")
    id_to_name <- df %>% filter(PLAYER_NAME!="", !is.na(athlete_id_1)) %>% distinct(athlete_id_1, PLAYER_NAME, team_id)
    assists <- df %>% filter(scoring_play==TRUE, !is.na(athlete_id_2)) %>%
      group_by(athlete_id_2) %>% summarise(ast=n(), .groups="drop") %>%
      left_join(id_to_name, by=c("athlete_id_2"="athlete_id_1")) %>% filter(!is.na(PLAYER_NAME))
    
    mk <- function(sdf, col, cat) {
      if(is.null(sdf)||nrow(sdf)==0) return("")
      ta <- sdf %>% filter(team_id==df$away_team_id[1]) %>% arrange(desc(.data[[col]])) %>% slice(1)
      th <- sdf %>% filter(team_id==df$home_team_id[1]) %>% arrange(desc(.data[[col]])) %>% slice(1)
      s <- function(r,c) { if(nrow(r)==0) return("<span style='color:var(--text-muted);'>—</span>")
        sprintf("<div style='display:flex;align-items:center;gap:8px;'><span style='font-family:Rajdhani;font-weight:700;font-size:20px;color:var(--text-primary);'>%s</span><span style='font-size:12px;color:var(--text-secondary);'>%s</span></div>",r[[c]][1],r$PLAYER_NAME[1]) }
      sprintf("<div style='margin-bottom:10px;'><div style='font-size:10px;font-family:Share Tech Mono;color:var(--text-muted);letter-spacing:.08em;margin-bottom:4px;'>%s</div><div style='display:flex;justify-content:space-between;'>%s%s</div></div>",cat,s(ta,col),s(th,col))
    }
    tags$div(class="sc-stats-card", tags$div(class="sc-stats-title","Game Leaders"),
             HTML(mk(scoring,"pts","POINTS")), HTML(mk(rebs,"reb","REBOUNDS")), HTML(mk(assists,"ast","ASSISTS")))
  })
  
  # ── On-Court Lineup with Headshots ──
  output$sc_oncourt_ui <- renderUI({
    df <- sc_pbp(); if(is.null(df)) return(NULL)
    pf <- sc_player_filter(); tf <- sc_type_filter()
    if(pf == "ALL" && tf == "ALL") return(NULL)
    
    shots <- df %>% filter(shooting_play == TRUE)
    if(pf != "ALL") shots <- shots %>% filter(PLAYER_NAME == pf)
    if(tf != "ALL") shots <- shots %>% filter(shot_category == tf)
    if(nrow(shots) == 0) return(NULL)
    
    hs_cols <- c(paste0("home_P",1:5,"_headshot"), paste0("away_P",1:5,"_headshot"))
    name_cols <- c(paste0("home_P",1:5), paste0("away_P",1:5))
    existing_hs <- hs_cols[hs_cols %in% names(shots)]
    existing_nm <- name_cols[name_cols %in% names(shots)]
    
    lineups <- shots %>% select(any_of(c(existing_nm, existing_hs))) %>% distinct()
    if(nrow(lineups) == 0) return(NULL)
    
    make_hs <- function(url, name) {
      short <- { parts <- strsplit(name," ")[[1]]; if(length(parts)>=2) paste0(substr(parts[1],1,1),".",parts[length(parts)]) else name }
      if(!is.na(url) && nchar(as.character(url)) > 10)
        sprintf("<div class='sc-lineup-player'><img class='sc-lineup-hs' src='%s' title='%s'/><span class='sc-lineup-name'>%s</span></div>", url, name, short)
      else
        sprintf("<div class='sc-lineup-player'><div class='sc-lineup-hs-ph'></div><span class='sc-lineup-name'>%s</span></div>", short)
    }
    
    rows_html <- paste(sapply(seq_len(min(nrow(lineups), 30)), function(i) {
      r <- lineups[i,]
      away_html <- paste(sapply(1:5, function(j) {
        nc <- paste0("away_P",j); hc <- paste0("away_P",j,"_headshot")
        nm <- if(nc %in% names(r)) as.character(r[[nc]]) else ""
        hs <- if(hc %in% names(r)) as.character(r[[hc]]) else ""
        make_hs(hs, nm)
      }), collapse="")
      home_html <- paste(sapply(1:5, function(j) {
        nc <- paste0("home_P",j); hc <- paste0("home_P",j,"_headshot")
        nm <- if(nc %in% names(r)) as.character(r[[nc]]) else ""
        hs <- if(hc %in% names(r)) as.character(r[[hc]]) else ""
        make_hs(hs, nm)
      }), collapse="")
      sprintf("<div class='sc-lineup-row'><span style='font-size:10px;font-family:Share Tech Mono;color:var(--text-muted);width:36px;'>%s</span>%s<span style='width:1px;height:30px;background:var(--border);margin:0 6px;'></span>%s</div>",
              df$away_team_abbrev[1], away_html, home_html)
    }), collapse="")
    
    tags$div(class="sc-oncourt",
             tags$div(class="sc-oncourt-title", sprintf("Players On Court (%s unique lineups)", nrow(lineups))),
             HTML(rows_html))
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # GAME CALENDAR — Team-filtered
  # ════════════════════════════════════════════════════════════════════════════
  
  output$gcal_ui <- renderUI({
    team <- gcal_team()
    tagList(
      tags$div(class="gcal-team-row",
               lapply(seq_len(nrow(NBA_TEAMS)), function(i) {
                 t <- NBA_TEAMS[i,]
                 cls <- paste("gcal-team-btn", if(!is.null(team) && team==t$abv) "active" else "")
                 tags$button(class=cls, onclick=sprintf("Shiny.setInputValue('gcal_team_click','%s',{priority:'event'})", t$abv),
                             tags$img(src=t$logo))
               })
      ),
      uiOutput("gcal_calendar_ui")
    )
  })
  
  output$gcal_calendar_ui <- renderUI({
    team <- gcal_team(); y <- gcal_cal_year(); m <- gcal_cal_month()
    first_day <- as.Date(paste(y,m,"01",sep="-"))
    last_day <- as.Date(paste(ifelse(m==12,y+1,y), ifelse(m==12,1,m+1), "01", sep="-"))-1
    start_dow <- as.integer(format(first_day,"%w")); cal_start <- first_day-start_dow; today <- Sys.Date()
    team_sched <- if(!is.null(team)) schedule_data %>% filter(team==!!team) else schedule_data
    team_sched <- team_sched %>% filter(game_date >= cal_start, game_date <= cal_start+41) %>% group_by(game_id) %>% slice(1) %>% ungroup()
    nav_html <- tags$div(class="calendar-nav",
                         tags$button(class="cal-nav-btn", onclick="Shiny.setInputValue('gcal_cal_prev',Math.random())", "\u2039"),
                         tags$span(class="cal-month-label", format(first_day, "%B %Y")),
                         tags$button(class="cal-nav-btn", onclick="Shiny.setInputValue('gcal_cal_next',Math.random())", "\u203A"))
    if(is.null(team)) return(tagList(nav_html, tags$div(class="pr-placeholder", tags$p("Select a team to view their schedule"))))
    header_cells <- lapply(c("SUN","MON","TUE","WED","THU","FRI","SAT"), function(d) tags$div(class="cal-dow", d))
    day_cells <- lapply(0:41, function(i) {
      cd <- cal_start+i; cell_day <- as.integer(format(cd,"%d"))
      is_cur <- format(cd,"%Y-%m")==format(first_day,"%Y-%m"); is_today <- cd==today
      cell_class <- paste("cal-cell", if(!is_cur)"other-month"else"", if(is_today)"today"else"")
      day_g <- team_sched[team_sched$game_date==cd,]
      pills <- if(nrow(day_g)>0) lapply(seq_len(min(nrow(day_g),3)), function(gi) {
        g <- day_g[gi,]; tl <- if(!is.na(g$team_logo)&&nchar(g$team_logo)>0) sprintf("<img src='%s'/>",g$team_logo) else ""
        ol <- if(!is.na(g$opp_logo)&&nchar(g$opp_logo)>0) sprintf("<img src='%s'/>",g$opp_logo) else ""
        sym <- if(!is.na(g$home_away_sym)) trimws(g$home_away_sym) else "vs"
        HTML(sprintf("<div class='cal-game' onclick=\"Shiny.setInputValue('gcal_game_click','%s',{priority:'event'})\">%s<span class='cal-game-teams'>%s</span><span class='cal-game-sym'>%s</span>%s</div>", as.character(g$game_id), tl, g$team, sym, ol))
      }) else list()
      tags$div(class=trimws(cell_class), tags$span(class="cal-date-num",cell_day), tagList(pills))
    })
    tagList(nav_html, tags$div(class="cal-grid", tagList(header_cells), tagList(day_cells)))
  })
  
  
  # ════════════════════════════════════════════════════════════════════════════
  # GAMECAST — Play-by-Play with Headshots
  # ════════════════════════════════════════════════════════════════════════════
  
  output$gc_ui <- renderUI({
    gid <- gc_game_id()
    if(is.null(gid) || gid == "") {
      return(tags$div(class="pr-placeholder", style="margin-top:60px;",
                      tags$p("Select a game from the Game Calendar to view GameCast")))
    }
    
    df <- pbp_data %>% filter(as.character(game_id) == as.character(gid))
    if(nrow(df) == 0) return(tags$div(class="pr-placeholder", tags$p("No play-by-play data for this game")))
    
    away_abv <- df$away_team_abbrev[1]; home_abv <- df$home_team_abbrev[1]
    away_tid <- df$away_team_id[1]; home_tid <- df$home_team_id[1]
    
    home_rows <- df[df$team_id == home_tid & !is.na(df$team_logo), ]
    away_rows <- df[df$team_id == away_tid & !is.na(df$team_logo), ]
    home_logo <- if(nrow(home_rows) > 0) as.character(home_rows$team_logo[1]) else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(home_abv), ".png")
    away_logo <- if(nrow(away_rows) > 0) as.character(away_rows$team_logo[1]) else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(away_abv), ".png")
    
    last_play <- df %>% arrange(desc(game_play_number)) %>% slice(1)
    final_away <- last_play$away_score; final_home <- last_play$home_score
    away_won <- final_away > final_home
    qtrs <- sort(unique(df$qtr[!is.na(df$qtr)]))
    qtr_labels <- sapply(qtrs, function(q) if(q <= 4) paste0("Q", q) else paste0("OT", q - 4))
    
    tagList(tags$div(class = "gc-layout",
                     # Scoreboard
                     tags$div(class = "gc-scoreboard",
                              tags$div(class = "gc-team-block away", tags$span(class = "gc-team-name", away_abv), tags$img(class = "gc-team-logo", src = away_logo)),
                              tags$div(class = "gc-score-center",
                                       tags$div(class = "gc-score-nums",
                                                tags$span(class = paste("gc-score-val", if(!away_won) "loser" else ""), final_away),
                                                tags$span(class = "gc-score-sep", "-"),
                                                tags$span(class = paste("gc-score-val", if(away_won) "loser" else ""), final_home)),
                                       tags$div(class = "gc-status", "FINAL")),
                              tags$div(class = "gc-team-block home", tags$img(class = "gc-team-logo", src = home_logo), tags$span(class = "gc-team-name", home_abv))
                     ),
                     # Mini shot chart
                     uiOutput("gc_mini_court_ui"),
                     # Quarter filter
                     tags$div(class = "gc-qtr-filter",
                              tags$button(class = paste("gc-qtr-btn", if(gc_qtr_filter() == "ALL") "active" else ""),
                                          onclick = "Shiny.setInputValue('gc_qtr_click','ALL',{priority:'event'})", "ALL"),
                              lapply(seq_along(qtrs), function(i) {
                                tags$button(class = paste("gc-qtr-btn", if(gc_qtr_filter() == as.character(qtrs[i])) "active" else ""),
                                            onclick = sprintf("Shiny.setInputValue('gc_qtr_click','%s',{priority:'event'})", qtrs[i]),
                                            qtr_labels[i])
                              })
                     ),
                     # Play feed
                     uiOutput("gc_feed_ui")
    ))
  })
  
  output$gc_mini_court_ui <- renderUI({
    gid <- gc_game_id()
    if(is.null(gid)) return(NULL)
    
    df <- pbp_data %>% filter(as.character(game_id) == as.character(gid))
    if(nrow(df) == 0) return(NULL)
    
    away_abv <- df$away_team_abbrev[1]; home_abv <- df$home_team_abbrev[1]
    away_tid <- df$away_team_id[1]; home_tid <- df$home_team_id[1]
    
    away_color <- { ar <- df[df$team_id == away_tid & !is.na(df$team_color), ]; if(nrow(ar) > 0) paste0("#", gsub("^#", "", ar$team_color[1])) else "#e53935" }
    home_color <- { hr <- df[df$team_id == home_tid & !is.na(df$team_color), ]; if(nrow(hr) > 0) paste0("#", gsub("^#", "", hr$team_color[1])) else "#1565c0" }
    
    home_logo_url <- { hr <- df[!is.na(df$team_logo) & df$team_id == home_tid, ]; if(nrow(hr) > 0 && nchar(as.character(hr$team_logo[1])) > 5) as.character(hr$team_logo[1]) else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(home_abv), ".png") }
    away_logo_url <- { ar <- df[!is.na(df$opp_logo) & df$team_id == home_tid, ]; if(nrow(ar) > 0 && nchar(as.character(ar$opp_logo[1])) > 5) as.character(ar$opp_logo[1]) else paste0("https://a.espncdn.com/i/teamlogos/nba/500/scoreboard/", tolower(away_abv), ".png") }
    
    shots <- df %>% filter(shooting_play == TRUE, !is.na(LOC_X), !is.na(LOC_Y))
    
    pf <- gc_player_filter(); pf_name <- NULL; pf_hs <- NULL
    if(!is.null(pf)) {
      ps <- shots %>% filter(PLAYER_NAME == pf)
      if(nrow(ps) > 0) { shots <- ps; pf_name <- pf
      pf_hs <- if("athlete_id_1_headshot" %in% names(ps) && !is.na(ps$athlete_id_1_headshot[1])) as.character(ps$athlete_id_1_headshot[1]) else NULL
      }
    }
    
    court_lines <- '<rect x="-250" y="-52" width="500" height="472" fill="#e8d5b5" rx="0"/><rect x="-80" y="-52" width="160" height="190" fill="none" stroke="#333" stroke-width="1.5"/><circle cx="0" cy="138" r="60" fill="none" stroke="#333" stroke-width="1.5"/><path d="M -40 -52 A 40 40 0 0 0 40 -52" fill="none" stroke="#333" stroke-width="1.5"/><path d="M -220 -52 L -220 88 A 237.5 237.5 0 0 0 220 88 L 220 -52" fill="none" stroke="#333" stroke-width="1.5"/><circle cx="0" cy="0" r="7.5" fill="none" stroke="#333" stroke-width="1.5"/><line x1="-30" y1="-8" x2="30" y2="-8" stroke="#333" stroke-width="2"/><line x1="-250" y1="418" x2="250" y2="418" stroke="#333" stroke-width="1.5"/><path d="M -60 418 A 60 60 0 0 1 60 418" fill="none" stroke="#333" stroke-width="1.5"/><circle cx="0" cy="418" r="36" fill="rgba(255,255,255,.3)" stroke="none"/>'
    
    defs_svg <- "<defs>"; dots_svg <- ""
    for(i in seq_len(nrow(shots))) {
      s <- shots[i, ]; x <- s$LOC_X; y <- s$LOC_Y; made <- isTRUE(s$scoring_play)
      is_home <- isTRUE(s$team_id == home_tid); col <- if(is_home) home_color else away_color
      hs_url <- if("athlete_id_1_headshot" %in% names(s) && !is.na(s$athlete_id_1_headshot) && nchar(as.character(s$athlete_id_1_headshot)) > 10) as.character(s$athlete_id_1_headshot) else ""
      cid <- paste0("gc", i)
      defs_svg <- paste0(defs_svg, sprintf("<clipPath id='%s'><circle cx='%s' cy='%s' r='7'/></clipPath>", cid, round(x,1), round(y,1)))
      if(nchar(hs_url) > 10) {
        if(made) dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='8' fill='%s' opacity='0.9'/><image href='%s' x='%s' y='%s' width='14' height='14' clip-path='url(#%s)' preserveAspectRatio='xMidYMid slice'/>", round(x,1),round(y,1),col,hs_url,round(x-7,1),round(y-7,1),cid))
        else dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='8' fill='none' stroke='%s' stroke-width='2' opacity='0.7'/><image href='%s' x='%s' y='%s' width='14' height='14' clip-path='url(#%s)' preserveAspectRatio='xMidYMid slice' opacity='0.35'/>", round(x,1),round(y,1),col,hs_url,round(x-7,1),round(y-7,1),cid))
      } else {
        if(made) dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='5' fill='%s' opacity='0.85'/>", round(x,1),round(y,1),col))
        else dots_svg <- paste0(dots_svg, sprintf("<circle cx='%s' cy='%s' r='5' fill='none' stroke='%s' stroke-width='2' opacity='0.7'/>", round(x,1),round(y,1),col))
      }
    }
    defs_svg <- paste0(defs_svg, "</defs>")
    logo_svg <- sprintf("<image href='%s' x='-28' y='390' width='56' height='56' opacity='0.6'/>", home_logo_url)
    full_svg <- sprintf("<svg class='gc-mini-court-svg' viewBox='-260 -60 520 490' xmlns='http://www.w3.org/2000/svg'>%s%s%s%s</svg>", defs_svg, court_lines, dots_svg, logo_svg)
    
    filter_html <- if(!is.null(pf_name)) {
      hs_img <- if(!is.null(pf_hs) && nchar(pf_hs) > 10) sprintf("<img src='%s'/>", pf_hs) else ""
      tags$div(class="gc-mini-court-filter", HTML(hs_img), tags$span(pf_name),
               tags$button(class="gc-mini-court-clear", onclick="Shiny.setInputValue('gc_player_filter_clear',Math.random(),{priority:'event'})", "\u00D7 Clear (Esc)"))
    } else NULL
    
    key_html <- tags$div(class="sc-key",
                         tags$div(class="sc-key-item", tags$img(src=away_logo_url,style="width:16px;height:16px;object-fit:contain;"), tags$span(away_abv),
                                  tags$span(class="sc-key-dot made",style=sprintf("border-color:%s;background:%s;",away_color,away_color)),"Made",
                                  tags$span(class="sc-key-dot missed",style=sprintf("border-color:%s;",away_color)),"Missed"),
                         tags$div(class="sc-key-item", tags$img(src=home_logo_url,style="width:16px;height:16px;object-fit:contain;"), tags$span(home_abv),
                                  tags$span(class="sc-key-dot made",style=sprintf("border-color:%s;background:%s;",home_color,home_color)),"Made",
                                  tags$span(class="sc-key-dot missed",style=sprintf("border-color:%s;",home_color)),"Missed"))
    
    tags$div(class="gc-mini-court-wrap",
             tags$div(class="gc-mini-court-header", tags$span(class="gc-mini-court-title","SHOT CHART"), filter_html),
             HTML(full_svg), key_html)
  })
  
  
  output$gc_feed_ui <- renderUI({
    gid <- gc_game_id(); if(is.null(gid)) return(NULL)
    df <- pbp_data %>% filter(as.character(game_id)==as.character(gid)) %>% arrange(game_play_number)
    qf <- gc_qtr_filter(); if(qf!="ALL") df <- df %>% filter(qtr==as.integer(qf))
    if(nrow(df)==0) return(tags$div(style="padding:20px;color:var(--text-muted);","No plays"))
    
    away_tid <- df$away_team_id[1]; home_tid <- df$home_team_id[1]
    id_to_name <- df %>% filter(PLAYER_NAME!="",!is.na(athlete_id_1)) %>% distinct(athlete_id_1,PLAYER_NAME)
    id_map <- setNames(id_to_name$PLAYER_NAME, as.character(id_to_name$athlete_id_1))
    active_pf <- gc_player_filter()
    
    df_rev <- df %>% arrange(desc(game_play_number)) %>% head(300)
    
    cards_html <- paste(sapply(seq_len(nrow(df_rev)), function(i) {
      r <- df_rev[i,]; is_scoring <- isTRUE(r$scoring_play)
      player_name <- if(!is.na(r$PLAYER_NAME) && r$PLAYER_NAME!="") r$PLAYER_NAME else ""
      
      hs_url <- if("athlete_id_1_headshot" %in% names(r) && !is.na(r$athlete_id_1_headshot) && nchar(as.character(r$athlete_id_1_headshot))>10) as.character(r$athlete_id_1_headshot) else ""
      is_active <- !is.null(active_pf) && player_name==active_pf
      active_style <- if(is_active) "border-color:#5a9a5a;box-shadow:0 0 0 3px rgba(90,154,90,.4);" else ""
      
      if(nchar(hs_url)>10 && nchar(player_name)>0) {
        pn_esc <- gsub("'","\\\\'",player_name)
        hs_tag <- sprintf("<img class='gc-play-hs gc-play-hs-clickable' src='%s' style='%s' onclick=\"Shiny.setInputValue('gc_play_hs_click','%s',{priority:'event'})\" title='Click to filter: %s'/>", hs_url, active_style, pn_esc, player_name)
      } else if(nchar(hs_url)>10) {
        hs_tag <- sprintf("<img class='gc-play-hs' src='%s'/>", hs_url)
      } else { hs_tag <- "<div class='gc-play-hs-ph'></div>" }
      
      qtr_label <- if(r$qtr<=4) paste0("Q",r$qtr) else paste0("OT",r$qtr-4)
      clock <- if(!is.na(r$clock_display_value)) r$clock_display_value else ""
      time_str <- sprintf("%s - %s",clock,qtr_label)
      play_type <- if(!is.na(r$short_description)&&r$short_description!="") r$short_description else r$type_text
      play_text <- if(!is.na(r$text)) r$text else ""
      
      assist_html <- ""
      if(is_scoring && !is.na(r$athlete_id_2)) {
        ast_name <- if(as.character(r$athlete_id_2) %in% names(id_map)) id_map[as.character(r$athlete_id_2)] else ""
        ast_hs <- if("athlete_id_2_headshot" %in% names(r) && !is.na(r$athlete_id_2_headshot) && nchar(as.character(r$athlete_id_2_headshot))>10) sprintf("<img src='%s'/>",r$athlete_id_2_headshot) else ""
        if(nchar(ast_name)>0) assist_html <- sprintf("<div class='gc-play-assist'>%s AST: %s</div>",ast_hs,ast_name)
      }
      
      score_html <- if(is_scoring) sprintf("<div class='gc-play-score'><span class='away-s'>%s</span><span style='color:var(--text-muted);font-size:16px;margin:0 3px;'>-</span><span class='home-s'>%s</span></div>",r$away_score,r$home_score) else ""
      
      oncourt_html <- ""
      if("home_P1_headshot" %in% names(r)) {
        away_oc <- paste(sapply(1:5, function(j) { hc<-paste0("away_P",j,"_headshot"); url<-if(hc %in% names(r)&&!is.na(r[[hc]])&&nchar(as.character(r[[hc]]))>10) as.character(r[[hc]]) else ""; if(nchar(url)>10) sprintf("<img src='%s'/>",url) else "<div class='oc-ph'></div>" }), collapse="")
        home_oc <- paste(sapply(1:5, function(j) { hc<-paste0("home_P",j,"_headshot"); url<-if(hc %in% names(r)&&!is.na(r[[hc]])&&nchar(as.character(r[[hc]]))>10) as.character(r[[hc]]) else ""; if(nchar(url)>10) sprintf("<img src='%s'/>",url) else "<div class='oc-ph'></div>" }), collapse="")
        oncourt_html <- sprintf("<div class='gc-play-oncourt'>%s<div class='gc-play-oncourt-divider'></div>%s</div>",away_oc,home_oc)
      }
      
      card_cls <- paste("gc-play-card",if(is_scoring)"scoring"else"")
      sprintf("<div class='%s'>%s<div class='gc-play-body'><div class='gc-play-header'><span class='gc-play-type'>%s</span><span class='gc-play-clock'>%s</span></div><div class='gc-play-text'>%s</div>%s%s</div>%s</div>",
              card_cls,hs_tag,play_type,time_str,play_text,assist_html,oncourt_html,score_html)
    }), collapse="")
    
    tags$div(class="gc-feed", HTML(cards_html))
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # HOME PAGE OUTPUTS
  # ════════════════════════════════════════════════════════════════════════════
  
  output$home_page_ui <- renderUI({
    tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:16px;align-items:start;",
             # Left column
             tags$div(style="display:flex;flex-direction:column;gap:16px;",
                      tags$div(class="home-section", style="margin-bottom:0;",
                               tags$div(class="home-section-title",
                                        if(nchar(nba_logo_b64) > 0) tags$img(src = nba_logo_b64) else NULL,
                                        "Current Slate"),
                               uiOutput("home_slate_ui")),
                      tags$div(class="home-section", style="margin-bottom:0;",
                               tags$div(class="home-section-title", "League Leaders"),
                               uiOutput("home_leaders_ui"))
             ),
             # Right column
             tags$div(style="display:flex;flex-direction:column;gap:16px;",
                      uiOutput("home_standings_ui")
             )
    )
  })
  
  output$home_slate_ui <- renderUI({
    today <- Sys.Date()
    today_all <- schedule_data %>% filter(game_date == today) %>% arrange(game_id)
    if (nrow(today_all) == 0) return(tags$div(style="padding:20px;text-align:center;color:var(--text-muted);font-family:'Share Tech Mono',monospace;font-size:14px;", "No games scheduled for today"))
    
    live_df <- live_scores()
    live_lookup <- list()
    if (!is.null(live_df) && nrow(live_df) > 0) {
      for (i in seq_len(nrow(live_df))) {
        key <- paste0(sort(c(live_df$away_tri[i], live_df$home_tri[i])), collapse = "_")
        live_lookup[[key]] <- live_df[i, ]
      }
    }
    
    today_odds <- historical_odds %>% filter(commence_date_est == today)
    
    pace_by_team <- team_data %>%
      group_by(TEAM) %>%
      summarise(avg_poss = mean(as.numeric(POSS_CGS), na.rm = TRUE), .groups = "drop")
    
    # Compute game pace per game_id (shared by both teams in the game)
    game_ids <- unique(today_all$game_id)
    game_pace_map <- setNames(rep(NA_real_, length(game_ids)), as.character(game_ids))
    for (gid in game_ids) {
      g_rows <- today_all[today_all$game_id == gid, ]
      teams_in_game <- unique(as.character(g_rows$team))
      paces <- sapply(teams_in_game, function(t) {
        v <- pace_by_team$avg_poss[pace_by_team$TEAM == t]
        if(length(v)>0 && !is.na(v[1])) v[1] else NA
      })
      if(all(!is.na(paces)) && length(paces) >= 2) {
        game_pace_map[as.character(gid)] <- round(mean(paces), 1)
      }
    }
    
    # Rank games by pace (fastest = rank 1)
    gp_vec <- unlist(game_pace_map)
    gp_ranks <- rank(-gp_vec, ties.method = "min", na.last = TRUE)
    names(gp_ranks) <- names(game_pace_map)
    n_games <- length(game_ids)
    
    # Pace conditional class (green=fast, yellow=mid, red=slow)
    pace_class <- function(rnk, n) {
      if(is.na(rnk) || n <= 1) return("pace-5")
      pct <- (rnk - 1) / max(n - 1, 1)
      if(pct <= 0.15) "pace-1" else if(pct <= 0.3) "pace-2" else if(pct <= 0.45) "pace-3"
      else if(pct <= 0.55) "pace-4" else if(pct <= 0.7) "pace-7"
      else if(pct <= 0.85) "pace-8" else "pace-9"
    }
    
    # Add pace & rank to each row, then sort by rank
    today_all$gp_pace <- sapply(as.character(today_all$game_id), function(g) game_pace_map[g])
    today_all$gp_rank <- sapply(as.character(today_all$game_id), function(g) gp_ranks[g])
    today_all <- today_all[order(today_all$gp_rank, today_all$game_id), ]
    
    yesterday <- today - 1
    
    rows_html <- paste(sapply(seq_len(nrow(today_all)), function(i) {
      r <- today_all[i,]
      team_abv <- as.character(r$team)
      opp_abv  <- as.character(r$opp)
      team_logo <- if(!is.na(r$team_logo) && nchar(as.character(r$team_logo))>0) sprintf("<img src='%s'/>", r$team_logo) else ""
      opp_logo  <- if(!is.na(r$opp_logo) && nchar(as.character(r$opp_logo))>0) sprintf("<img src='%s' style='width:24px;height:24px;object-fit:contain;vertical-align:middle;margin-right:4px;'/>", r$opp_logo) else ""
      sym <- trimws(as.character(r$home_away_sym))
      
      match_key <- paste0(sort(c(toupper(team_abv), toupper(opp_abv))), collapse="_")
      live_game <- live_lookup[[match_key]]
      has_live <- !is.null(live_game)
      score_str <- ""; live_html <- ""
      if (has_live && live_game$game_status >= 2) {
        # Match this row's team to the correct side of the live score
        my_team_upper <- toupper(trimws(team_abv))
        if(my_team_upper == live_game$home_tri) {
          my_score <- live_game$home_score; opp_score <- live_game$away_score
        } else {
          my_score <- live_game$away_score; opp_score <- live_game$home_score
        }
        score_str <- sprintf("%s - %s", my_score, opp_score)
        if (live_game$game_status == 2) {
          st <- format_game_clock(live_game$game_clock, live_game$period, live_game$game_status)
          live_html <- sprintf("<span style='display:inline-block;width:7px;height:7px;border-radius:50%%;background:#e53935;margin-right:4px;'></span>%s", st)
        } else live_html <- "FINAL"
      }
      
      b2b <- if(nrow(schedule_data %>% filter(game_date==yesterday, team==team_abv))>0) "<span class='slate-b2b'>B2B</span>" else ""
      
      gp <- r$gp_pace
      gr <- r$gp_rank
      pace_str <- if(!is.na(gp)) as.character(gp) else "\u2014"
      rank_str <- if(!is.na(gr)) as.character(as.integer(gr)) else "\u2014"
      p_cls <- pace_class(gr, n_games)
      
      get_line <- function(mkt,bk,tm){ rr<-today_odds[today_odds$market==mkt & tolower(today_odds$bookmaker)==tolower(bk) & today_odds$outcome_team_abv==tm,]; if(nrow(rr)>0) list(line=rr$spread_or_total[1],odds=rr$odds[1]) else list(line=NA,odds=NA) }
      dk_sp <- get_line("spreads","draftkings",team_abv)
      spread_str <- if(!is.na(dk_sp$line)) as.character(round(dk_sp$line,1)) else "\u2014"
      dk_ml <- get_line("h2h","draftkings",team_abv)
      ml_str <- if(!is.na(dk_ml$odds)){ v<-round(dk_ml$odds); if(v>0) paste0("+",v) else as.character(v) } else "\u2014"
      dk_tot <- today_odds[today_odds$market=="totals" & tolower(today_odds$bookmaker)=="draftkings" & (today_odds$espn_team_abv_home==team_abv | today_odds$espn_team_abv_away==team_abv),]
      total_str <- if(nrow(dk_tot)>0 && !is.na(dk_tot$spread_or_total[1])) as.character(round(dk_tot$spread_or_total[1],1)) else "\u2014"
      
      dvp <- paste(rep("<td>\u2014</td>",5),collapse="")
      sprintf("<tr><td><div class='slate-team-cell'>%s<span class='slate-tn'>%s</span>%s</div></td><<td style='color:var(--text-muted);font-size:17px;white-space:nowrap;'>%s %s<span style='font-family:Rajdhani,sans-serif;font-weight:700;letter-spacing:.04em;'>%s</span></td><td>%s</td><td>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td><td>%s</td>%s</tr>",
              team_logo, team_abv, b2b,
              sym, opp_logo, opp_abv,
              score_str, live_html,
              p_cls, rank_str, p_cls, pace_str,
              spread_str, ml_str, total_str, dvp)
    }), collapse="")
    
    tags$div(class="slate-wrap", HTML(sprintf(
      "<table class='slate-table'><thead><tr class='slate-grp-hdr'><th colspan='2' class='grp-m'>Matchup</th><th class='grp-m'>Score</th><th class='grp-m'>Live</th><th colspan='2' class='grp-p'>Pace</th><th colspan='3' class='grp-v'>Vegas (DK)</th><th colspan='5' class='grp-d'>DVP</th></tr><tr><th class='grp-m'>Team</th><th class='grp-m'>Opp</th><th class='grp-m'>Score</th><th>Status</th><th class='grp-p'>#</th><th class='grp-p'>pPace</th><th class='grp-v'>Spread</th><th class='grp-v'>ML</th><th class='grp-v'>Total</th><th class='grp-d'>PG</th><th class='grp-d'>SG</th><th class='grp-d'>SF</th><th class='grp-d'>PF</th><th class='grp-d'>C</th></tr></thead><tbody>%s</tbody></table>", rows_html)))
  })
  
  output$home_standings_ui <- renderUI({
    # Compute pace for all teams from team_data
    pace_all <- team_data %>%
      group_by(TEAM) %>%
      summarise(avg_poss = round(mean(as.numeric(POSS_CGS), na.rm=TRUE), 1), .groups="drop")
    
    # Compute FT% / FTA / 3PT% / 3PTA from team_data
    shooting_all <- team_data %>%
      group_by(TEAM) %>%
      summarise(
        avg_ft_pct  = round(mean(as.numeric(FT_PCT_CGS), na.rm=TRUE) * 100, 1),
        avg_fta     = round(mean(as.numeric(FTA_CGS), na.rm=TRUE), 0),
        avg_3pt_pct = round(mean(as.numeric(`3PT_PCT_CGS`), na.rm=TRUE) * 100, 1),
        avg_3pta    = round(mean(as.numeric(`3PTA_CGS`), na.rm=TRUE), 0),
        .groups="drop"
      )
    
    # Conditional formatting helper: assign class based on rank within a vector (lower rank = better)
    # higher_is_better=TRUE means highest value gets green
    cond_class <- function(val, all_vals, higher_is_better=TRUE) {
      if(is.na(val)) return("cond-neutral")
      sorted <- sort(unique(all_vals[!is.na(all_vals)]))
      n <- length(sorted)
      if(n <= 1) return("cond-neutral")
      rnk <- if(higher_is_better) rank(-all_vals, ties.method="average", na.last=TRUE)[match(val, all_vals)][1]
      else rank(all_vals, ties.method="average", na.last=TRUE)[match(val, all_vals)][1]
      # Use percentile
      pct <- (rnk - 1) / (n - 1)
      if(pct <= 0.1) "cond-green-5" else if(pct <= 0.2) "cond-green-4" else if(pct <= 0.3) "cond-green-3"
      else if(pct <= 0.4) "cond-green-2" else if(pct <= 0.45) "cond-green-1"
      else if(pct <= 0.55) "cond-neutral"
      else if(pct <= 0.6) "cond-red-1" else if(pct <= 0.7) "cond-red-2"
      else if(pct <= 0.8) "cond-red-3" else if(pct <= 0.9) "cond-red-4"
      else "cond-red-5"
    }
    
    # Build per-value rank classes for each stat column across ALL 30 teams
    all_standings <- standings_data
    all_ortg <- suppressWarnings(as.numeric(all_standings$PointsPG))
    all_drtg <- suppressWarnings(as.numeric(all_standings$OppPointsPG))
    all_nrtg <- suppressWarnings(as.numeric(all_standings$DiffPointsPG))
    all_sos  <- suppressWarnings(as.numeric(all_standings$SOS))
    all_osos <- suppressWarnings(as.numeric(all_standings$oSOS))
    all_dsos <- suppressWarnings(as.numeric(all_standings$dSOS))
    all_wins <- suppressWarnings(as.numeric(all_standings$WINS))
    all_loss <- suppressWarnings(as.numeric(all_standings$LOSSES))
    all_pace_vals <- pace_all$avg_poss
    all_ft_pct <- shooting_all$avg_ft_pct
    all_fta    <- shooting_all$avg_fta
    all_3pt_pct <- shooting_all$avg_3pt_pct
    all_3pta   <- shooting_all$avg_3pta
    
    make_conf_table <- function(conf_name, conf_logo_src) {
      conf <- standings_data %>% filter(Conference == conf_name) %>% arrange(PlayoffRank)
      if (nrow(conf) == 0) return(NULL)
      
      rows_html <- paste(sapply(seq_len(nrow(conf)), function(i) {
        r <- conf[i,]
        team_name <- paste(r$TeamCity, r$TeamName)
        team_abv_match <- ""
        logo_html <- ""
        # Match using TeamName's last word against known team nicknames
        abv_map <- c("ATL"="Hawks","BKN"="Nets","BOS"="Celtics","CHA"="Hornets","CHI"="Bulls",
                     "CLE"="Cavaliers","DAL"="Mavericks","DEN"="Nuggets","DET"="Pistons","GSW"="Warriors",
                     "HOU"="Rockets","IND"="Pacers","LAC"="Clippers","LAL"="Lakers","MEM"="Grizzlies",
                     "MIA"="Heat","MIL"="Bucks","MIN"="Timberwolves","NOP"="Pelicans","NYK"="Knicks",
                     "OKC"="Thunder","ORL"="Magic","PHI"="76ers","PHX"="Suns","POR"="Trail Blazers",
                     "SAC"="Kings","SAS"="Spurs","TOR"="Raptors","UTA"="Jazz","WAS"="Wizards")
        for(j in seq_len(nrow(NBA_TEAMS))) {
          nickname <- abv_map[NBA_TEAMS$abv[j]]
          if(!is.na(nickname) && grepl(nickname, team_name, ignore.case=TRUE)) {
            logo_html <- sprintf("<img class='conf-logo' src='%s'/>", NBA_TEAMS$logo[j])
            team_abv_match <- NBA_TEAMS$abv[j]
            break
          }
        }
        wins <- suppressWarnings(as.numeric(r$WINS))
        losses <- suppressWarnings(as.numeric(r$LOSSES))
        ortg <- suppressWarnings(as.numeric(r$PointsPG))
        drtg <- suppressWarnings(as.numeric(r$OppPointsPG))
        nrtg_raw <- suppressWarnings(as.numeric(r$DiffPointsPG))
        sos_raw  <- if("SOS" %in% names(r)) suppressWarnings(as.numeric(r$SOS)) else NA
        osos_raw <- if("oSOS" %in% names(r)) suppressWarnings(as.numeric(r$oSOS)) else NA
        dsos_raw <- if("dSOS" %in% names(r)) suppressWarnings(as.numeric(r$dSOS)) else NA
        
        # Lookup pace and shooting
        tp <- pace_all[pace_all$TEAM == team_abv_match, ]
        pace_val <- if(nrow(tp)>0) tp$avg_poss[1] else NA
        ts <- shooting_all[shooting_all$TEAM == team_abv_match, ]
        ft_pct_val  <- if(nrow(ts)>0) ts$avg_ft_pct[1] else NA
        fta_val     <- if(nrow(ts)>0) ts$avg_fta[1] else NA
        tpt_pct_val <- if(nrow(ts)>0) ts$avg_3pt_pct[1] else NA
        tpta_val    <- if(nrow(ts)>0) ts$avg_3pta[1] else NA
        
        nrtg_str <- if(!is.na(nrtg_raw)){ if(nrtg_raw>0) paste0("+",round(nrtg_raw,1)) else as.character(round(nrtg_raw,1)) } else "\u2014"
        sos_str  <- if(!is.na(sos_raw)) as.character(round(sos_raw,1)) else "\u2014"
        osos_str <- if(!is.na(osos_raw)) as.character(round(osos_raw,1)) else "\u2014"
        dsos_str <- if(!is.na(dsos_raw)) as.character(round(dsos_raw,1)) else "\u2014"
        
        fmt_v <- function(v) if(is.na(v)) "\u2014" else as.character(round(v,1))
        fmt_i <- function(v) if(is.na(v)) "\u2014" else as.character(as.integer(round(v)))
        
        # Conditional classes
        w_cls    <- cond_class(wins, all_wins, higher_is_better=TRUE)
        l_cls    <- cond_class(losses, all_loss, higher_is_better=FALSE)
        o_cls    <- cond_class(ortg, all_ortg, higher_is_better=TRUE)
        d_cls    <- cond_class(drtg, all_drtg, higher_is_better=FALSE)
        n_cls    <- cond_class(nrtg_raw, all_nrtg, higher_is_better=TRUE)
        sos_cls  <- cond_class(sos_raw, all_sos, higher_is_better=FALSE)
        osos_cls <- cond_class(osos_raw, all_osos, higher_is_better=FALSE)
        dsos_cls <- cond_class(dsos_raw, all_dsos, higher_is_better=FALSE)
        pace_cls <- cond_class(pace_val, all_pace_vals, higher_is_better=TRUE)
        ft_cls   <- cond_class(ft_pct_val, all_ft_pct, higher_is_better=TRUE)
        fta_cls  <- cond_class(fta_val, all_fta, higher_is_better=TRUE)
        tpt_cls  <- cond_class(tpt_pct_val, all_3pt_pct, higher_is_better=TRUE)
        tpta_cls <- cond_class(tpta_val, all_3pta, higher_is_better=TRUE)
        
        sprintf("<tr><td class='conf-seed'>%s</td><td>%s</td><td>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td><td class='%s'>%s</td></tr>",
                r$PlayoffRank, logo_html, team_name,
                w_cls, if(!is.na(wins)) wins else "\u2014",
                l_cls, if(!is.na(losses)) losses else "\u2014",
                o_cls, if(!is.na(ortg)) round(ortg,0) else "\u2014",
                d_cls, if(!is.na(drtg)) round(drtg,0) else "\u2014",
                n_cls, nrtg_str,
                sos_cls, sos_str, osos_cls, osos_str, dsos_cls, dsos_str,
                pace_cls, fmt_v(pace_val),
                ft_cls, fmt_v(ft_pct_val), fta_cls, fmt_i(fta_val),
                tpt_cls, fmt_v(tpt_pct_val), tpta_cls, fmt_i(tpta_val))
      }), collapse="")
      
      tags$div(class="standings-conf-wrap",
               tags$div(class="home-section-title", style="padding:10px 12px;margin-bottom:0;",
                        if(nchar(conf_logo_src)>0) tags$img(src=conf_logo_src) else NULL,
                        paste(conf_name,"Conference")),
               HTML(sprintf("<table class='conf-table'><thead><tr><th>#</th><th></th><th>Team</th><th>W</th><th>L</th><th>ORtg</th><th>DRtg</th><th>NRtg</th><th>SOS</th><th>oSOS</th><th>dSOS</th><th>Pace</th><th>FT%%</th><th>FTA</th><th>3PT%%</th><th>3PTA</th></tr></thead><tbody>%s</tbody></table>", rows_html)))
    }
    
    tagList(
      make_conf_table("East", east_logo_b64),
      make_conf_table("West", west_logo_b64)
    )
  })
  
  output$home_leaders_ui <- renderUI({
    avgs <- player_data %>%
      group_by(PLAYER_NAME) %>%
      summarise(TEAM=first(TEAM), HEADSHOT=first(HEADSHOT),
                PPG=round(mean(PTS_CGS,na.rm=TRUE),1), RPG=round(mean(REB_CGS,na.rm=TRUE),1),
                APG=round(mean(AST_CGS,na.rm=TRUE),1), SPG=round(mean(STL_CGS,na.rm=TRUE),1),
                BPG=round(mean(BLK_CGS,na.rm=TRUE),1),
                FPG=round(mean(FOULS_CGS,na.rm=TRUE),1),
                GP=n(), .groups="drop") %>%
      filter(GP >= 10)
    
    make_leader_block <- function(title, stat_col) {
      top5 <- avgs %>% arrange(desc(.data[[stat_col]])) %>% head(5)
      rows_html <- paste(sapply(seq_len(nrow(top5)), function(i) {
        r <- top5[i,]; rank_cls <- switch(as.character(i),"1"="leader-rank-1","2"="leader-rank-2","3"="leader-rank-3","")
        hs <- if(!is.na(r$HEADSHOT) && nchar(as.character(r$HEADSHOT))>5) sprintf("<img class='leader-hs' src='%s'/>", r$HEADSHOT) else "<div class='leader-hs-ph'></div>"
        sprintf("<div class='leader-block-row'><span class='leader-rank %s'>%d</span>%s<div style='flex:1;display:flex;flex-direction:column;gap:1px;'><span class='leader-pname'>%s</span><span class='leader-pteam'>%s</span></div><span class='leader-pavg'>%s</span></div>",
                rank_cls, i, hs, r$PLAYER_NAME, r$TEAM, r[[stat_col]])
      }), collapse="")
      sprintf("<div class='leader-block'><div class='leader-block-title'>%s</div>%s</div>", title, rows_html)
    }
    
    HTML(sprintf("<div class='leaders-wrap'>%s%s%s%s%s%s</div>",
                 make_leader_block("Average Points","PPG"), make_leader_block("Average Rebounds","RPG"),
                 make_leader_block("Average Assists","APG"), make_leader_block("Average Steals","SPG"),
                 make_leader_block("Average Blocks","BPG"), make_leader_block("Average Fouls","FPG")))
  })
  
}  # ← this closes the server function
#==============================================================================
shinyApp(ui, server)