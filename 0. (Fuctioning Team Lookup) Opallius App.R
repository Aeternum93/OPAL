library(shiny)
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

# ════════════════════════════════════════════════════════════════════════════════
# LOGO
# ════════════════════════════════════════════════════════════════════════════════

logo_b64 <- base64enc::dataURI(
  file = "C:/Users/Austin/OneDrive/Desktop/1/Data Analytics/NBA Data/NBA R Scripts/NBA Scripts/www/Opallius.png",
  mime = "image/png"
)

# ════════════════════════════════════════════════════════════════════════════════
# CSS
# ════════════════════════════════════════════════════════════════════════════════

opallius_css <- "
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
  font-family: 'Rajdhani', sans-serif; font-weight: 600; font-size: 17px;
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
"

# ════════════════════════════════════════════════════════════════════════════════
# JAVASCRIPT
# ════════════════════════════════════════════════════════════════════════════════

opallius_js <- "
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
  away_check <- !is.na(t_rows$is_away) & t_rows$is_away == 1
  home_check <- !is.na(t_rows$is_home) & t_rows$is_home == 1
  away_row <- if (isTRUE(any(away_check))) t_rows[away_check, ][1, ] else t_rows[1, ]
  home_row <- if (isTRUE(any(home_check))) t_rows[home_check, ][1, ] else t_rows[min(2, nrow(t_rows)), ]
  list(away = away_row, home = home_row)
}

# ════════════════════════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════════════════════════

ui <- tagList(
  tags$head(tags$style(HTML(opallius_css)), tags$script(HTML(opallius_js))),
  
  # ── Top header bar ──
  tags$div(id = "opal-header",
           tags$button(id = "hamburger", onclick = "toggleSidebar()", tags$span(), tags$span(), tags$span()),
           tags$div(class = "logo-area",
                    tags$img(src = logo_b64, height = "28px", style = "border-radius: 6px;"),
                    tags$span(class = "logo-text", "Opallius")
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
  observeEvent(input$card_click, { if (input$card_click %in% c("player_stats","team_stats")) sub_active(input$card_click) })
  observeEvent(input$player_tab, { player_tab(input$player_tab); session$sendCustomMessage("setPlayerTab", input$player_tab) })
  observeEvent(input$game_tab,   { game_tab(input$game_tab);     session$sendCustomMessage("setGameTab",   input$game_tab) })
  
  # ── Jump to player (from box score / game leaders click) ──
  observeEvent(input$jump_to_player, {
    sub_active("player_stats")
    player_tab("gamelog")
    updateSelectizeInput(session, "selected_player", selected = input$jump_to_player)
  })
  
  # ── Header title / subtitle ──
  # TO CHANGE A PAGE TITLE: edit the matching switch() case below
  output$main_title <- renderText({
    if (!is.null(sub_active()) && sub_active() == "player_stats") return("Player Stats Lookup")
    if (!is.null(sub_active()) && sub_active() == "team_stats")   return("Team Stats Lookup")
    switch(active(), prediction="Prediction", research="Research",
           portfolio="Portfolio Management", standings="Standings | Current Slate", "Opallius")
  })
  output$main_subtitle <- renderText({
    if (!is.null(sub_active()) && sub_active() %in% c("player_stats","team_stats")) return("// 2025-2026 REGULAR SEASON")
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
                               opal_card("Player Rotations",    "Tool 07"),
                               opal_card("5 Man vs 5 Man Viewer","Tool 08"),
                               opal_card("On/Off Lineup Viewer","Tool 09"),
                               opal_card("Data Hub Viewer",     "Tool 10"),
                               opal_card("Schedule Viewer",     "Tool 12"),
                               opal_card("Player & Team Odds",  "Tool 13")
                      )
                    ),
                    
                    # Portfolio section
                    portfolio = tagList(
                      tags$div(class="section-intro", tags$h2("Portfolio Management"), tags$p("// BET TRACKING & BANKROLL MANAGEMENT")),
                      tags$div(class="cards-grid", opal_card("Betting View","Tool 11"))
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
    
    rows   <- get_away_home(t_rows)
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
    
    # Use shared helper — same safe logic as game_view_ui
    rows     <- get_away_home(t_rows)
    away_row <- rows$away; home_row <- rows$home
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
      njs   <- gsub("'","\\\\'", pname)
      headshot <- if("HEADSHOT" %in% names(r) && !is.na(r$HEADSHOT[1]) && nchar(as.character(r$HEADSHOT[1])) > 0)
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
        njs <- gsub("'","\\\\'", l$name)
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
}

shinyApp(ui, server)