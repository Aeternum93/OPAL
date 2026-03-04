library(shiny)
library(hoopR)
library(dplyr)
library(base64enc)

# ════════════════════════════════════════════════════════════════════════════════
# DATA LOADING
# ════════════════════════════════════════════════════════════════════════════════

player_data <- read.csv(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/2. BaseStats_Player/BaseStats_Player_MC_2025_2026.csv",
  stringsAsFactors = FALSE
)

team_data <- read.csv(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/1. BaseStats_Team/BaseStats_Team_MC_2025_2026.csv",
  stringsAsFactors = FALSE
)
names(team_data) <- gsub("^T_", "", names(team_data))

schedule_data <- read.csv(
  "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/10. NBA Schedule/nba_schedule_2025_2026.csv",
  stringsAsFactors = FALSE
)
schedule_data$game_date <- as.Date(schedule_data$game_date)

ROT_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/12. Player Rotations/"

rot_5m  <- read.csv(paste0(ROT_PATH, "pm_nba_player_rotations_5M_2025_2026.csv"),  stringsAsFactors=FALSE)
rot_10m <- read.csv(paste0(ROT_PATH, "pm_nba_player_rotations_10M_2025_2026.csv"), stringsAsFactors=FALSE)

rot_5m$game_date  <- as.Date(as.character(rot_5m$game_date))
rot_10m$GAME_DATE <- as.Date(as.character(rot_10m$GAME_DATE))

ODDS_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/1. hoopR/11. Player Enriched Odds/"
player_odds <- read.csv(paste0(ODDS_PATH, "nba_player_odds_enrich_2025_2026.csv"), stringsAsFactors=FALSE)
player_odds$GAME_DATE <- as.Date(as.character(player_odds$GAME_DATE_CTR), format="%m/%d/%Y")

INJ_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/0. Datahub (Temp)/9. Historical Injuries (RapidAPI)/"
injury_data <- read.csv(paste0(INJ_PATH, "Injury_Database_2025_2026.csv"), stringsAsFactors=FALSE)
injury_data$date <- as.Date(as.character(injury_data$date))

WWW_PATH <- "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts/www/"
sb_logos <- list(
  DK   = base64enc::dataURI(file=paste0(WWW_PATH,"DK.png"),      mime="image/png"),
  FD   = base64enc::dataURI(file=paste0(WWW_PATH,"FD.jpg"),      mime="image/jpeg"),
  FNA  = base64enc::dataURI(file=paste0(WWW_PATH,"FAN.png"),     mime="image/png"),
  BMGM = base64enc::dataURI(file=paste0(WWW_PATH,"BETMGM.png"),  mime="image/png")
)

# ════════════════════════════════════════════════════════════════════════════════
# LOGO
# ════════════════════════════════════════════════════════════════════════════════

logo_b64 <- base64enc::dataURI(
  file = "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts/www/OPAL.png",
  mime = "image/png"
)

# ════════════════════════════════════════════════════════════════════════════════
# CSS
# ════════════════════════════════════════════════════════════════════════════════

OPAL_css <- "
@import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@300;400;500;600;700&family=Share+Tech+Mono&family=Inter:wght@300;400;500&display=swap');

:root {
  --bg-base:        #0a0a0b;
  --bg-sidebar:     #111114;
  --bg-panel:       #0e0e10;
  --bg-card:        #161619;
  --bg-card-hover:  #1c1c20;
  --bg-header:      #111114;
  --border:         #222228;
  --border-light:   #2a2a32;
  --text-primary:   #e8e8ec;
  --text-secondary: #7a7a8a;
  --text-muted:     #4a4a58;
  --sidebar-width:  220px;
  --sidebar-col:    56px;
  --header-height:  48px;
}

* { margin: 0; padding: 0; box-sizing: border-box; }
html, body, .shiny-bound-input { height: 100%; }

body {
  font-family: 'Inter', sans-serif;
  background: var(--bg-base);
  color: var(--text-primary);
  height: 100vh;
  overflow: hidden;
  display: flex;
  flex-direction: column;
}

#opal-header {
  height: var(--header-height);
  background: var(--bg-header);
  border-bottom: 1px solid var(--border);
  display: flex; align-items: center;
  padding: 0 16px; gap: 12px;
  z-index: 100; flex-shrink: 0;
}
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
  font-family: 'Inter', sans-serif; cursor: text; min-width: 180px;
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
  font-family: 'Inter', sans-serif; font-size: 12.5px; font-weight: 400;
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
  font-family: 'Inter', sans-serif !important; font-size: 13px !important; box-shadow: none !important;
}
.selectize-input.focus { border-color: var(--border-light) !important; }
.selectize-dropdown { background: var(--bg-card) !important; border: 1px solid var(--border-light) !important; border-radius: 6px !important; }
.selectize-dropdown-content .option { color: var(--text-secondary) !important; font-family: 'Inter', sans-serif !important; font-size: 13px !important; padding: 8px 12px !important; }
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
.bio-val { font-size: 12px; font-family: 'Inter', sans-serif; color: var(--text-secondary); }
.player-tabs { display: flex; border-bottom: 1px solid var(--border); padding: 0 16px; background: var(--bg-card); }
.player-tab {
  padding: 12px 16px; font-size: 12px; font-family: 'Inter', sans-serif; font-weight: 500;
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
  padding: 12px 16px; font-size: 12px; font-family: 'Inter', sans-serif; font-weight: 500;
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
.bv-form-input { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Inter', sans-serif; font-size: 13px; padding: 8px 10px; width: 100%; }
.bv-form-input:focus { outline: none; border-color: var(--border-light); }
.bv-form-select { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Inter', sans-serif; font-size: 13px; padding: 8px 10px; width: 100%; }
.bv-kelly-hint { font-size: 10px; font-family: 'Share Tech Mono', monospace; color: #5a9a5a; letter-spacing: .06em; }
.bv-modal-btns { display: flex; gap: 10px; justify-content: flex-end; }
.bv-modal-cancel { padding: 8px 18px; background: transparent; border: 1px solid var(--border); border-radius: 6px; cursor: pointer; color: var(--text-muted); font-family: 'Inter', sans-serif; font-size: 13px; transition: background .12s; }
.bv-modal-cancel:hover { background: var(--bg-card-hover); }
.bv-modal-submit { padding: 8px 18px; background: #0e1a0e; border: 1px solid #2a4a2a; border-radius: 6px; cursor: pointer; color: #7aba7a; font-family: 'Inter', sans-serif; font-size: 13px; font-weight: 500; transition: background .12s; }
.bv-modal-submit:hover { background: #162816; }
.bv-bankroll-input { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-primary); font-family: 'Rajdhani', sans-serif; font-size: 18px; font-weight: 700; padding: 6px 12px; width: 120px; text-align: center; }

/* ── PLAYER ROTATIONS ── */
.pr-layout { display: flex; flex-direction: column; height: calc(100vh - 120px); overflow: hidden; }
.pr-subnav { display: flex; gap: 0; border-bottom: 1px solid var(--border); margin-bottom: 0; flex-shrink: 0; }
.pr-tab { padding: 10px 18px; font-size: 12px; font-family: 'Inter', sans-serif; font-weight: 500; color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent; transition: color .15s, border-color .15s; border: none; background: transparent; letter-spacing: .03em; }
.pr-tab:hover { color: var(--text-secondary); }
.pr-tab.active { color: var(--text-primary); border-bottom-color: var(--text-secondary); }
.pr-filters { padding: 12px 0; display: flex; flex-direction: column; gap: 10px; flex-shrink: 0; }
.pr-filter-row { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
.pr-filter-btn { display: flex; align-items: center; gap: 6px; padding: 6px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; cursor: pointer; font-size: 11px; font-family: 'Inter', sans-serif; color: var(--text-secondary); transition: background .12s, border-color .12s; white-space: nowrap; }
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
.pr-opp-select { background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; color: var(--text-secondary); font-family: 'Inter', sans-serif; font-size: 11px; padding: 5px 8px; }
.pr-placeholder { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 200px; gap: 8px; opacity: .35; }
.pr-placeholder p { font-family: 'Rajdhani', sans-serif; font-size: 13px; letter-spacing: .1em; text-transform: uppercase; color: var(--text-muted); }

/* ── INJURY DASHBOARD ── */
.inj-home-wrap { padding: 16px; overflow-y: auto; height: calc(100vh - 140px); }
.inj-team-block { margin-bottom: 28px; }
.inj-team-header { display: flex; align-items: center; gap: 6px; font-family: 'Rajdhani', sans-serif; font-size: 28px; font-weight: 700; letter-spacing: .06em; color: var(--text-primary); padding: 0 0 8px 0; border-bottom: 1px solid var(--border); margin-bottom: 6px; }
.inj-col-header { display: flex; align-items: center; padding: 5px 8px; font-family: 'Share Tech Mono', monospace; font-size: 13px; letter-spacing: .1em; color: var(--text-muted); border-bottom: 1px solid var(--border); }
.inj-player-row { display: flex; align-items: center; padding: 8px 8px; border-bottom: 1px solid rgba(255,255,255,.04); transition: background .1s; }
.inj-player-row:hover { background: var(--bg-card); }
.inj-player-name { font-family: 'Inter', sans-serif; font-size: 16px; font-weight: 500; }

/* ── PLAYER & TEAM ODDS ── */
.po-layout { display: flex; flex-direction: column; height: calc(100vh - 96px); overflow: hidden; }
.po-subnav { display: flex; gap: 0; border-bottom: 1px solid var(--border); flex-shrink: 0; }
.po-tab { padding: 10px 18px; font-size: 12px; font-family: 'Inter', sans-serif; font-weight: 500; color: var(--text-muted); cursor: pointer; border-bottom: 2px solid transparent; border: none; background: transparent; letter-spacing: .03em; transition: color .15s; }
.po-tab:hover { color: var(--text-secondary); }
.po-tab.active { color: var(--text-primary); border-bottom: 2px solid var(--text-secondary); }
.po-filters { display: flex; align-items: center; gap: 8px; padding: 12px 0; flex-shrink: 0; flex-wrap: wrap; }
.po-filter-btn { display: flex; align-items: center; gap: 6px; padding: 6px 12px; background: var(--bg-card); border: 1px solid var(--border); border-radius: 6px; cursor: pointer; font-size: 11px; font-family: 'Inter', sans-serif; color: var(--text-secondary); transition: background .12s; white-space: nowrap; }
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
.po-player-name { font-family: 'Inter', sans-serif; font-size: 16px; font-weight: 500; color: var(--text-primary); }
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
.to-game-name { font-family: 'Inter', sans-serif; font-size: 13px; font-weight: 500; color: var(--text-primary); }
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
"

# ════════════════════════════════════════════════════════════════════════════════
# JAVASCRIPT
# ════════════════════════════════════════════════════════════════════════════════

OPAL_js <- "
function toggleSidebar() { document.body.classList.toggle('collapsed'); }
Shiny.addCustomMessageHandler('setActive', function(id) {
  document.querySelectorAll('.nav-item').forEach(function(el) { el.classList.remove('active'); });
  var el = document.getElementById('nav-' + id);
  if (el) el.classList.add('active');
});
Shiny.addCustomMessageHandler('setPlayerTab', function(tab) {
  document.querySelectorAll('.player-tab').forEach(function(el) { el.classList.remove('active'); });
  var el = document.getElementById('ptab-' + tab);
  if (el) el.classList.add('active');
});
Shiny.addCustomMessageHandler('setGameTab', function(tab) {
  document.querySelectorAll('.game-tab').forEach(function(el) { el.classList.remove('active'); });
  var el = document.getElementById('gtab-' + tab);
  if (el) el.classList.add('active');
});
Shiny.addCustomMessageHandler('openSchedulePane', function(date) {
  var pane = document.getElementById('schedule-pane');
  if (pane) { pane.classList.add('open'); }
});
Shiny.addCustomMessageHandler('closeSchedulePane', function(x) {
  var pane = document.getElementById('schedule-pane');
  if (pane) { pane.classList.remove('open'); }
});
Shiny.addCustomMessageHandler('renderTray', function(data) {
  var existing = document.getElementById('pr-global-tray');
  if (!existing) {
    var tray = document.createElement('div');
    tray.id = 'pr-global-tray';
    document.body.appendChild(tray);
  }
  document.getElementById('pr-global-tray').innerHTML = data.html;
});
Shiny.addCustomMessageHandler('hideTray', function(x) {
  var t = document.getElementById('pr-global-tray');
  if (t) t.innerHTML = '';
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

# ════════════════════════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════════════════════════

ui <- tagList(
  tags$head(tags$style(HTML(OPAL_css)), tags$script(HTML(OPAL_js))),
  
  # ── Top header bar ──
  tags$div(id = "opal-header",
           tags$button(id = "hamburger", onclick = "toggleSidebar()", tags$span(), tags$span(), tags$span()),
           tags$div(class = "logo-area",
                    tags$img(src = logo_b64, height = "28px", style = "border-radius: 6px;"),
                    tags$span(class = "logo-text", "OPAL")
           ),
           tags$div(class = "header-right",
                    tags$div(class = "search-placeholder",
                             tags$svg(viewBox = "0 0 24 24", tags$circle(cx="11",cy="11",r="8"), tags$path(d="m21 21-4.35-4.35")),
                             "Search..."
                    ),
                    tags$button(class = "header-btn",
                                tags$svg(viewBox="0 0 24 24", tags$path(d="M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9"), tags$path(d="M13.73 21a2 2 0 0 1-3.46 0"))
                    ),
                    tags$button(class = "header-btn",
                                tags$svg(viewBox="0 0 24 24", tags$path(d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"), tags$circle(cx="12",cy="7",r="4"))
                    )
           )
  ),
  
  tags$div(id = "app-body",
           
           # ── Left sidebar — nav items ──
           # TO ADD A NEW SIDEBAR ITEM: add a new nav_section block with nav_item()
           tags$aside(id = "opal-sidebar",
                      tags$div(class="nav-section", tags$div(class="nav-section-label","Prediction"), nav_item("prediction","Prediction")),
                      tags$div(class="nav-section", tags$div(class="nav-section-label","Research"),   nav_item("research","Research")),
                      tags$div(class="nav-section", tags$div(class="nav-section-label","Portfolio"),  nav_item("portfolio","Portfolio Management")),
                      tags$div(class="nav-section", tags$div(class="nav-section-label","League"),     nav_item("standings","Standings | Current Slate"))
           ),
           
           # ── Main content area ──
           tags$main(id = "opal-main",
                     tags$div(id = "main-header",
                              tags$span(id="main-title",    textOutput("main_title",    inline=TRUE)),
                              tags$span(id="main-subtitle", textOutput("main_subtitle", inline=TRUE))
                     ),
                     tags$div(id = "main-content", uiOutput("main_panel"))
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
  
  # ── Navigation observers ──
  observeEvent(input$nav_click,  { active(input$nav_click); sub_active(NULL); session$sendCustomMessage("setActive", input$nav_click) })
  observeEvent(input$card_click, { if (input$card_click %in% c("player_stats","team_stats","schedule_viewer","betting_view","player_rotations","player_team_odds","injury_dashboard")) sub_active(input$card_click) })
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
    switch(active(), prediction="Prediction", research="Research",
           portfolio="Portfolio Management", standings="Standings | Current Slate", "OPAL")
  })
  output$main_subtitle <- renderText({
    if (!is.null(sub_active()) && sub_active() %in% c("player_stats","team_stats","schedule_viewer")) return("// 2025-2026 REGULAR SEASON")
    if (!is.null(sub_active()) && sub_active() == "betting_view") return("// KELLY CRITERION · BANKROLL MANAGEMENT")
    if (!is.null(sub_active()) && sub_active() == "player_team_odds") return("// PLAYER PROPS · SPREADS · TOTALS · MONEYLINES")
    if (!is.null(sub_active()) && sub_active() == "player_rotations") return("// LINEUP ANALYTICS · WOWY · 5M / 10M")
    if (!is.null(sub_active()) && sub_active() == "injury_dashboard") return("// NBA INJURY TRACKER · ALL 30 TEAMS")
    switch(active(), prediction="// AVITUS · AURELIUS · DFS", research="// ANALYTICS & INTELLIGENCE TOOLS",
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
      return(tags$div(class="fade-in",
                      tags$div(class="player-search-bar",
                               selectizeInput("selected_player", label=NULL,
                                              choices=c("Select a player..."="", player_choices), selected="",
                                              options=list(placeholder="Search player...", onInitialize=I('function() { this.setValue(""); }')))
                      ),
                      uiOutput("player_profile_ui")
      ))
    }
    
    # ── Route: Team Stats Lookup ──
    if (!is.null(sub_active()) && sub_active() == "team_stats") {
      return(tags$div(class="fade-in",
                      tags$div(class="team-lookup-selectors",
                               tags$div(tags$label("Select Team"),
                                        selectizeInput("selected_team", label=NULL,
                                                       choices=c("Select a team..."="", team_choices), selected="",
                                                       options=list(placeholder="Team...", onInitialize=I('function() { this.setValue(""); }')))
                               ),
                               uiOutput("game_selector_ui")
                      ),
                      uiOutput("game_view_ui")
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
                    home = tags$div(class="home-state",
                                    tags$svg(viewBox="0 0 24 24", tags$polygon(points="12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2")),
                                    tags$p("Select a module from the sidebar")
                    ),
                    
                    # Prediction section
                    prediction = tagList(
                      tags$div(class="section-intro", tags$h2("Prediction"), tags$p("// MODEL SELECTION — CHOOSE A PREDICTION ENGINE")),
                      tags$div(class="cards-grid",
                               opal_card("Run LogReg<br>AVITUS","Model 01"),
                               opal_card("Run Monte Carlo<br>AURELIUS","Model 02"),
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
                          tags$select(class="po-filter-btn", style="cursor:pointer;",
                                      onchange="Shiny.setInputValue('po_date_sel',this.value,{priority:'event'})",
                                      tags$option(value="","All Dates"),
                                      lapply(dates, function(d) tags$option(value=d, selected=isTRUE(gd==d), d))
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
                        else              uiOutput("po_team_cards_ui")
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
        lo_col  <- paste0(pfx,"_",sb$key,"_LINE_O")
        oo_col  <- paste0(pfx,"_",sb$key,"_ODDS_O")
        lu_col  <- paste0(pfx,"_",sb$key,"_LINE_U")
        ou_col  <- paste0(pfx,"_",sb$key,"_ODDS_U")
        get_col <- function(col) if(col %in% names(r)) r[[col]] else NA
        sprintf(
          "<td style='border-left:1px solid var(--border);'>%s</td><td>%s</td><td>%s</td><td>%s</td>",
          fmt_val(get_col(lo_col)), fmt_odds(get_col(oo_col)),
          fmt_val(get_col(lu_col)), fmt_odds(get_col(ou_col))
        )
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
    # Filter team_data for the selected date
    td <- team_data
    if(!is.null(gd) && length(gd) > 0 && gd != "") {
      td <- td[as.character(td$GAME_DATE) == as.character(gd), ]
    }
    if(nrow(td)==0) return(tags$div(class="pr-placeholder", tags$p("No team data for selected date")))
    
    get_val <- function(r, col) {
      if(!(col %in% names(r))) return(NA_real_)
      suppressWarnings(as.numeric(r[[col]][1]))
    }
    get_chr <- function(r, col) {
      if(!(col %in% names(r))) return("")
      v <- r[[col]][1]; if(is.na(v)) "" else as.character(v)
    }
    
    # Determine home/away safely
    split_home_away <- function(g) {
      is_h <- sapply(seq_len(nrow(g)), function(k) {
        v <- g$IS_HOME[k]
        isTRUE(v == 1) || isTRUE(v == "1") || isTRUE(v == TRUE)
      })
      home_idx <- which(is_h)[1]; away_idx <- which(!is_h)[1]
      if(is.na(home_idx) || is.na(away_idx)) {
        list(home=g[1,], away=g[2,])
      } else {
        list(home=g[home_idx,], away=g[away_idx,])
      }
    }
    
    game_ids <- unique(td$ESPN_GAME_ID)
    
    # Banner: use pre-computed columns directly — sum across all rows for the day
    sum_col <- function(col) if(col %in% names(td)) sum(as.numeric(td[[col]]), na.rm=TRUE) else 0
    n_games    <- length(game_ids)
    n_fav_win  <- sum_col("fav_win")
    n_dog_win  <- sum_col("dog_win")
    n_fav_loss <- sum_col("fav_loss")
    n_dog_loss <- sum_col("dog_loss")
    n_home_win <- sum_col("home_win")
    n_away_win <- sum_col("away_win")
    n_h_dog_win <- sum_col("home_dog_win")
    n_a_dog_win <- sum_col("away_dog_win")
    
    banner <- tags$div(class="to-banner",
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_games),    tags$div(class="to-banner-lbl","Games")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_fav_win),  tags$div(class="to-banner-lbl","Fav Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_dog_win),  tags$div(class="to-banner-lbl","Dog Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_home_win), tags$div(class="to-banner-lbl","Home Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_away_win), tags$div(class="to-banner-lbl","Away Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_h_dog_win),tags$div(class="to-banner-lbl","Home Dog Wins")),
                       tags$div(class="to-banner-stat", tags$div(class="to-banner-val", n_a_dog_win),tags$div(class="to-banner-lbl","Away Dog Wins"))
    )
    
    fmt_sc  <- function(v) if(is.na(v) || length(v)==0) "—" else as.character(round(as.numeric(v)))
    fmt_tot <- function(v) if(is.na(v) || length(v)==0) "—" else as.character(round(as.numeric(v), 1))
    safe_logo <- function(u) if(length(u)>0 && !is.na(u) && nchar(u)>5) sprintf("<img style='width:28px;height:28px;object-fit:contain;' src='%s'/>", u) else ""
    
    # Build all game cards as a single HTML string — much faster than tag objects
    cards_html <- paste(sapply(game_ids, function(gid) {
      g <- td[td$ESPN_GAME_ID == gid, ]
      if(nrow(g) < 2) return("")
      ha   <- split_home_away(g)
      home <- ha$home; away <- ha$away
      
      h_score <- get_val(home, "T_PTS_CGS")
      a_score <- get_val(away, "T_PTS_CGS")
      h_won   <- isTRUE(home$TEAM_WINNER == 1)
      a_won   <- isTRUE(away$TEAM_WINNER == 1)
      
      dk_o <- get_val(home, "DK_total_O"); if(is.na(dk_o)) dk_o <- get_val(away, "DK_total_O")
      fd_o <- get_val(home, "FD_total_O"); if(is.na(fd_o)) fd_o <- get_val(away, "FD_total_O")
      actual_total <- if(!is.na(h_score) && !is.na(a_score)) h_score + a_score else NA_real_
      over_hit_dk  <- isTRUE(!is.na(dk_o) && !is.na(actual_total) && actual_total > dk_o)
      over_hit_fd  <- isTRUE(!is.na(fd_o) && !is.na(actual_total) && actual_total > fd_o)
      
      tl_h <- safe_logo(get_chr(home, "team_logo"))
      tl_a <- safe_logo(get_chr(away, "team_logo"))
      
      team_row <- function(logo, team_name, score, won) {
        sc_cls <- if(won) "to-game-score winner" else "to-game-score"
        sprintf("<div class='to-game-row'><div class='to-game-team'>%s<span class='to-game-name'>%s</span></div><div class='%s'>%s</div></div>",
                logo, team_name, sc_cls, fmt_sc(score))
      }
      
      tot_block <- if(!is.na(dk_o) || !is.na(fd_o)) {
        act_str <- if(!is.na(actual_total)) sprintf("<span class='to-odds-actual'>Act: %s</span>", fmt_tot(actual_total)) else ""
        dk_part <- if(!is.na(dk_o)) sprintf("<div style='display:flex;flex-direction:column;align-items:center;gap:2px;'><span style='font-size:8px;color:var(--text-muted);'>DK</span><div class='to-odds-block%s' style='display:inline-flex;gap:6px;align-items:center;padding:3px 8px;'><span class='to-odds-line'>O/U %s</span>%s</div></div>", if(over_hit_dk) " covered" else "", fmt_tot(dk_o), act_str) else ""
        fd_part <- if(!is.na(fd_o)) sprintf("<div style='display:flex;flex-direction:column;align-items:center;gap:2px;'><span style='font-size:8px;color:var(--text-muted);'>FD</span><div class='to-odds-block%s' style='display:inline-flex;gap:6px;align-items:center;padding:3px 8px;'><span class='to-odds-line'>O/U %s</span>%s</div></div>", if(over_hit_fd) " covered" else "", fmt_tot(fd_o), act_str) else ""
        sprintf("<div style='display:flex;align-items:center;gap:16px;padding:8px 16px;background:var(--bg-base);border-top:1px solid var(--border);'><span style='font-size:9px;font-family:Share Tech Mono,monospace;color:var(--text-muted);letter-spacing:.08em;min-width:28px;'>TOTAL</span>%s%s</div>", dk_part, fd_part)
      } else ""
      
      hdr <- "<div class='to-col-header'><span style='flex:1;'></span><span style='min-width:44px;text-align:center;'>PTS</span></div>"
      sprintf("<div class='to-game-card'>%s%s%s%s</div>",
              hdr,
              team_row(tl_a, get_chr(away,"TEAM"), a_score, a_won),
              team_row(tl_h, get_chr(home,"TEAM"), h_score, h_won),
              tot_block)
    }), collapse="")
    
    tagList(
      banner,
      HTML(sprintf("<div class='to-games'>%s</div>", cards_html))
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
  
}

# ═══════════════════════════════════════════════════════════════════════════════=
shinyApp(ui, server)