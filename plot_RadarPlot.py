"""
Radar (pizza) plots from the Wyscout data saved by load_wyscout.R
=================================================================

Reads the CSV files that load_wyscout.R writes to data/wyscout/tidy/<competition>/
and makes a percentile pizza plot for one or more players, using the same nine
attacking stats as the Soccermatics radar lesson:

    non-penalty xG, non-penalty goals, assists, key passes, smart passes,
    passes ending in the final third, passes received in the final third,
    attacking ground duels won, air duels won

Everything is per 90 minutes, and each slice shows the player's percentile among
players in the same role (e.g. Forwards) with enough minutes. With --adjusted,
stats are also adjusted for the possession his team had while he was on the pitch.

Differences from the lesson code:
  * Minutes played are calculated from the lineups and substitutions (no extra
    download needed). Red cards end a player's minutes at the card.
  * The xG model is a plain logistic regression on distance and angle (separate
    models for headers and other shots), coded so the probabilities can't flip.
  * A "reception" in the final third only counts if the next event is by a
    teammate (the lesson also counted interceptions by the other team).

Usage (from the folder that contains data/):
    python radar_plots.py --player "Mohamed Salah"
    python radar_plots.py --player "Mohamed Salah" "Harry Kane" --adjusted
    python radar_plots.py --list salah              # search player names
    python radar_plots.py --competition Spain --position Midfielder --player "Isco"

Or edit the SETTINGS below and run the file without arguments (e.g. in Spyder).

Needs: pandas, numpy, matplotlib, mplsoccer   (python -m pip install mplsoccer)
"""

import argparse
import os
import re
import sys
import unicodedata

import numpy as np
import pandas as pd
import matplotlib

matplotlib.use("Agg")  # save files without needing a screen; remove to show windows
import matplotlib.pyplot as plt
from mplsoccer import PyPizza

# ---- SETTINGS (used when you run the file without command-line arguments) ----
COMPETITION = "England"
DATA_DIR    = os.path.join("example_data", "wyscout", "tidy")
PLAYERS     = ["Mohamed Salah"]
POSITION    = "Forward"        # Goalkeeper, Defender, Midfielder or Forward
MIN_MINUTES = 400              # players need more than this to be compared
ADJUSTED    = False            # True = possession-adjusted per 90
OUT_DIR     = "radar_output"

STATS = [  # (column, label, group)
    ("npxG",                   "Non-penalty\nExpected Goals",   "Shooting"),
    ("goals",                  "Non-penalty\nGoals",            "Shooting"),
    ("assists",                "Assists",                       "Passing"),
    ("key_passes",             "Key Passes",                    "Passing"),
    ("smart_passes",           "Smart Passes",                  "Passing"),
    ("final_third_passes",     "Passes Ending in\nFinal Third", "Passing"),
    ("final_third_receptions", "Passes Received\nin Final Third", "Passing"),
    ("ground_duels_won",       "Attacking Ground\nDuels Won",   "Duels"),
    ("air_duels_won",          "Air Duels Won",                 "Duels"),
]
GROUP_COLORS = {"Shooting": "#1A78CF", "Passing": "#FF9300", "Duels": "#D70232"}

# Wyscout tag ids
GOAL, ASSIST, KEY_PASS, HEADER, LOST, WON, ACCURATE = 101, 301, 302, 403, 701, 703, 1801


# ---- Loading -----------------------------------------------------------------

def read_table(folder, name):
    path = os.path.join(folder, name + ".csv.gz")         # compressed (default in load_wyscout.R)
    if not os.path.exists(path):
        path = os.path.join(folder, name + ".csv")
    if not os.path.exists(path):
        sys.exit(f"Missing {path}. Run load_wyscout.R first (with SAVE_CSV <- TRUE).")
    try:
        return pd.read_csv(path, low_memory=False)
    except pd.errors.EmptyDataError:   # e.g. a competition with no substitutions saved
        return pd.DataFrame()


def load(folder):
    t = {n: read_table(folder, n) for n in
         ["events", "players", "teams", "matches", "lineups", "substitutions"]}
    if t["substitutions"].empty:
        t["substitutions"] = pd.DataFrame(columns=["match_id", "team_id", "player_in", "player_out", "minute"])
    ev = t["events"]
    ev["tags"] = ev["tags"].fillna("").astype(str)
    ev["sub_event_name"] = ev["sub_event_name"].fillna("")
    return t


def has_tag(tags, tag_id):
    """True where the comma-separated tag list contains tag_id."""
    return ("," + tags + ",").str.contains(f",{tag_id},", regex=False)


def plain(s):
    """Lower-case, accent-free text for name matching."""
    s = unicodedata.normalize("NFKD", str(s))
    return "".join(c for c in s if not unicodedata.combining(c)).lower().strip()


# ---- Minutes played ------------------------------------------------------------

def minutes_played(matches, lineups, subs):
    """One row per player per match with the minute he came on and went off."""
    length = matches.set_index("match_id")["duration"].map(
        {"Regular": 90, "ExtraTime": 120, "Penalties": 120}).fillna(90)
    lu = lineups.copy()
    lu["end"] = lu["match_id"].map(length).fillna(90)

    came_on = subs.groupby(["match_id", "player_in"])["minute"].min()
    went_off = subs.groupby(["match_id", "player_out"])["minute"].min()
    key = list(zip(lu["match_id"], lu["player_id"]))

    lu["min_in"] = np.where(lu["role"] == "starter", 0.0,
                            pd.Series(key).map(came_on).to_numpy(dtype=float))
    off = pd.Series(key).map(went_off).to_numpy(dtype=float)
    lu["min_out"] = np.where(np.isnan(off), lu["end"], off)
    red = pd.to_numeric(lu["red_minute"], errors="coerce").fillna(0)
    lu["min_out"] = np.where(red > 0, np.minimum(lu["min_out"], red), lu["min_out"])

    lu = lu[lu["min_in"].notna()].copy()                    # unused substitutes
    lu["minutes"] = (lu["min_out"] - lu["min_in"]).clip(lower=0)
    return lu[["match_id", "team_id", "player_id", "min_in", "min_out", "minutes"]]


# ---- xG ------------------------------------------------------------------------

def fit_logit(X, y, iters=50):
    """Logistic regression by iteratively reweighted least squares (no extra packages)."""
    X = np.column_stack([np.ones(len(X)), X])
    b = np.zeros(X.shape[1])
    for _ in range(iters):
        p = 1 / (1 + np.exp(-(X @ b)))
        w = np.clip(p * (1 - p), 1e-9, None)
        step = np.linalg.solve(X.T @ (X * w[:, None]) + 1e-8 * np.eye(len(b)), X.T @ (y - p))
        b += step
        if np.max(np.abs(step)) < 1e-8:
            break
    return b


def npxg_by_player(ev):
    """Non-penalty xG from open-play shots (eventName 'Shot'), as in the lesson."""
    shots = ev[ev["event_name"] == "Shot"].copy()
    X = (100 - shots["start_x"]) * 105 / 100            # metres from goal line
    C = (shots["start_y"] - 50).abs() * 68 / 100         # metres from centre line
    shots["distance"] = np.sqrt(X ** 2 + C ** 2)
    a = np.arctan(7.32 * X / (X ** 2 + C ** 2 - (7.32 / 2) ** 2))
    shots["angle"] = np.where(a > 0, a, a + np.pi)       # angle the goal mouth covers
    shots["goal"] = has_tag(shots["tags"], GOAL).astype(float)
    shots["header"] = has_tag(shots["tags"], HEADER)

    shots["xG"] = np.nan
    groups = [shots["header"], ~shots["header"]]
    if shots["header"].sum() < 30:                       # too few headers for their own model
        print("Note: fewer than 30 headed shots, so one xG model is used for all shots.")
        groups = [pd.Series(True, index=shots.index)]
    for g in groups:
        sub = shots[g]
        b = fit_logit(sub[["distance", "angle"]].to_numpy(), sub["goal"].to_numpy())
        lin = b[0] + sub[["distance", "angle"]].to_numpy() @ b[1:]
        shots.loc[g, "xG"] = 1 / (1 + np.exp(-lin))
    return shots.groupby("player_id")["xG"].sum().rename("npxG")


# ---- Counting stats --------------------------------------------------------------

def count_stats(ev):
    passes = ev[ev["event_name"] == "Pass"]
    accurate = passes[has_tag(passes["tags"], ACCURATE)]
    shots = ev[ev["sub_event_name"] == "Shot"]

    def per_player(df, name):
        return df.groupby("player_id").size().rename(name)

    out = [
        per_player(shots[has_tag(shots["tags"], GOAL)], "goals"),
        per_player(passes[has_tag(passes["tags"], ASSIST)], "assists"),
        per_player(passes[has_tag(passes["tags"], KEY_PASS)], "key_passes"),
        per_player(accurate[accurate["sub_event_name"] == "Smart pass"], "smart_passes"),
    ]

    # final third: accurate passes ending beyond 2/3 of the pitch, and who received them
    ft = accurate[accurate["end_x"] > 100 * 2 / 3]
    out.append(per_player(ft, "final_third_passes"))
    nxt_player = ev.groupby("match_id")["player_id"].shift(-1)
    nxt_team = ev.groupby("match_id")["team_id"].shift(-1)
    rec = ft.assign(receiver=nxt_player.loc[ft.index], next_team=nxt_team.loc[ft.index])
    rec = rec[(rec["next_team"] == rec["team_id"]) & (rec["receiver"] > 0)]
    rec["receiver"] = rec["receiver"].astype("int64")
    out.append(rec.groupby("receiver").size().rename("final_third_receptions")
               .rename_axis("player_id"))

    won = has_tag(ev["tags"], WON)
    out.append(per_player(ev[(ev["sub_event_name"] == "Ground attacking duel") & won], "ground_duels_won"))
    out.append(per_player(ev[(ev["sub_event_name"] == "Air duel") & won], "air_duels_won"))
    return pd.concat(out, axis=1)


# ---- Possession while on the pitch -------------------------------------------------

def possession_on_pitch(ev, mins):
    """Share of touches (passes + duels where the ball was won/contested) by the
    player's team while he was on the pitch, over the whole season."""
    touch = ev[ev["event_name"].isin(["Pass", "Duel"])].copy()
    lost_no_touch = (touch["sub_event_name"].isin(["Air duel", "Ground defending duel", "Ground loose ball duel"])
                     & has_tag(touch["tags"], LOST))
    touch = touch[~lost_no_touch]

    # one running clock per match: 2nd half (and extra time) continue from where the previous period ended
    period_end = ev.groupby(["match_id", "match_period"])["event_sec"].max().unstack()
    order = [p for p in ["1H", "2H", "E1", "E2"] if p in period_end.columns]
    offsets = period_end[order].fillna(0).cumsum(axis=1).shift(1, axis=1).fillna(0)
    touch = touch[touch["match_period"].isin(order)]
    touch["t"] = touch["event_sec"] + [offsets.at[m, p] for m, p in zip(touch["match_id"], touch["match_period"])]

    h1 = period_end["1H"] if "1H" in period_end else pd.Series(dtype=float)
    h2 = period_end["2H"] if "2H" in period_end else pd.Series(dtype=float)

    def to_clock(match_id, minute):        # match minute -> seconds on the running clock
        a, b = h1.get(match_id, 45 * 60), h2.get(match_id, 45 * 60)
        if minute <= 45:
            return minute * 60
        if minute <= 90:
            return a + (minute - 45) * 60
        return a + b + (minute - 90) * 60

    team_touch, all_touch = {}, {}
    by_match = {m: g.sort_values("t") for m, g in touch.groupby("match_id")}
    for r in mins.itertuples(index=False):
        g = by_match.get(r.match_id)
        if g is None or r.minutes <= 0:
            continue
        t = g["t"].to_numpy()
        lo, hi = to_clock(r.match_id, r.min_in), to_clock(r.match_id, r.min_out)
        i, j = np.searchsorted(t, lo, "right"), np.searchsorted(t, hi, "right")
        teams = g["team_id"].to_numpy()[i:j]
        team_touch[r.player_id] = team_touch.get(r.player_id, 0) + int((teams == r.team_id).sum())
        all_touch[r.player_id] = all_touch.get(r.player_id, 0) + len(teams)
    return pd.Series({p: team_touch[p] / all_touch[p] for p in all_touch if all_touch[p] > 0},
                     name="possession")


# ---- Summary table -----------------------------------------------------------------

def build_summary(t, position, min_minutes, adjusted):
    ev = t["events"]
    ev = ev[ev["end_x"].notna()]          # like the lesson: keep events with a start and an end
    mins = minutes_played(t["matches"], t["lineups"], t["substitutions"])

    totals = pd.concat([npxg_by_player(ev), count_stats(ev)], axis=1)
    season_mins = mins.groupby("player_id")["minutes"].sum().rename("minutes")
    main_team = (mins.groupby(["player_id", "team_id"])["minutes"].sum()
                 .reset_index().sort_values("minutes").drop_duplicates("player_id", keep="last")
                 .set_index("player_id")["team_id"])

    s = pd.DataFrame(season_mins).join(totals, how="left").fillna(0)
    s["team_id"] = main_team
    s = s.join(t["players"].set_index("player_id")[["player_name", "role.name"]], how="left")
    s = s.join(t["teams"].set_index("team_id")["team_name"], on="team_id")

    cols = [c for c, _, _ in STATS]
    if adjusted:
        s = s.join(possession_on_pitch(t["events"], mins), how="left")
        s["possession"] = s["possession"].replace(0, np.nan)
        for c in cols:
            s[c] = s[c] / s["possession"]
    for c in cols:
        s[c + "_p90"] = s[c] * 90 / s["minutes"]

    s["eligible"] = (s["minutes"] > min_minutes) & (s["role.name"] == position)
    return s


def percentile(ref, x):
    """Percent of the comparison group at or below x (ties count half), 0-100."""
    ref = np.asarray(ref, dtype=float)
    return 100 * ((ref < x).sum() + 0.5 * (ref == x).sum()) / len(ref)


def find_player(s, query):
    q = plain(query)
    names = s["player_name"].map(plain)
    hit = s[names == q]
    if hit.empty:
        hit = s[names.str.contains(re.escape(q), na=False)]
    if hit.empty:
        return None, []
    hit = hit.sort_values("minutes", ascending=False)
    return hit.index[0], hit["player_name"].tolist()[1:]


# ---- Plot ----------------------------------------------------------------------------

def plot_player(s, pid, position, min_minutes, adjusted, competition, season, out_dir):
    row = s.loc[pid]
    group = s[s["eligible"]]
    cols = [c + "_p90" for c, _, _ in STATS]
    values = [row[c] for c in cols]
    pcts = [int(percentile(group[c], row[c])) for c in cols]
    colors = [GROUP_COLORS[g] for _, _, g in STATS]

    baker = PyPizza(
        params=[label for _, label, _ in STATS],
        background_color="#fcfcfb",
        straight_line_color="#d4d2cc", straight_line_lw=1,
        last_circle_color="#52514e", last_circle_lw=1,
        other_circle_color="#d4d2cc", other_circle_lw=1, other_circle_ls="-.",
    )
    fig, ax = baker.make_pizza(
        pcts, figsize=(9, 9.6), param_location=112,
        slice_colors=colors, value_colors=["white"] * len(STATS), value_bck_colors=colors,
        kwargs_slices=dict(edgecolor="#fcfcfb", zorder=2, linewidth=1.5),
        kwargs_params=dict(color="#0b0b0b", fontsize=11, va="center"),
        kwargs_values=dict(color="white", fontsize=10.5, zorder=3,
                           bbox=dict(edgecolor="none", boxstyle="round,pad=0.25", lw=0)),
    )
    for txt, v, p in zip(baker.get_value_texts(), values, pcts):
        txt.set_text(f"{v:.2f}")            # slice size = percentile, label = per-90 value

    kind = "per 90, possession-adjusted" if adjusted else "per 90"
    team = row["team_name"] if isinstance(row["team_name"], str) else ""
    fig.text(0.5, 0.975, f"{row['player_name']} - {team}", size=18, ha="center",
             weight="bold", color="#0b0b0b")
    fig.text(0.5, 0.948,
             f"{kind} | slice size = percentile vs {competition} {position.lower()}s "
             f"with {min_minutes}+ minutes (n = {len(group)}) | {season}",
             size=11, ha="center", color="#52514e")
    for k, (g, c) in enumerate(GROUP_COLORS.items()):
        fig.text(0.38 + 0.12 * k, 0.05, g, size=11, ha="center", color="white",
                 bbox=dict(facecolor=c, edgecolor="none", boxstyle="round,pad=0.3"))
    fig.text(0.5, 0.015, f"{int(row['minutes'])} minutes played. Labels show the per-90 value.",
             size=9.5, ha="center", color="#52514e")

    os.makedirs(out_dir, exist_ok=True)
    safe = re.sub(r"[^A-Za-z0-9]+", "_", plain(row["player_name"])).strip("_")
    path = os.path.join(out_dir, f"{safe}_{competition}{'_adjusted' if adjusted else ''}.png")
    fig.savefig(path, dpi=150, facecolor=fig.get_facecolor())
    plt.close(fig)
    return path, row["minutes"]


# ---- Main ------------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description="Wyscout radar plots from load_wyscout.R output")
    ap.add_argument("--player", nargs="+", default=PLAYERS, help="player name(s)")
    ap.add_argument("--competition", default=COMPETITION)
    ap.add_argument("--data-dir", default=DATA_DIR, help="folder that holds <competition>/events.csv etc.")
    ap.add_argument("--position", default=POSITION, choices=["Goalkeeper", "Defender", "Midfielder", "Forward"])
    ap.add_argument("--min-minutes", type=int, default=MIN_MINUTES)
    ap.add_argument("--adjusted", action="store_true", default=ADJUSTED, help="possession-adjust the stats")
    ap.add_argument("--out-dir", default=OUT_DIR)
    ap.add_argument("--list", metavar="TEXT", help="list players whose name contains TEXT, then stop")
    a = ap.parse_args()

    folder = os.path.join(a.data_dir, a.competition)
    print(f"Loading {folder} ...")
    t = load(folder)
    s = build_summary(t, a.position, a.min_minutes, a.adjusted)

    dates = pd.to_datetime(t["matches"]["date_utc"], errors="coerce")
    season = ""
    if dates.notna().any():
        y0, y1 = dates.min().year, dates.max().year
        season = f"{y0}-{str(y1)[-2:]}" if y1 > y0 else str(y0)

    if a.list:
        m = s[s["player_name"].map(plain).str.contains(re.escape(plain(a.list)), na=False)]
        print(m.sort_values("minutes", ascending=False)[["player_name", "team_name", "role.name", "minutes"]]
              .to_string())
        return

    if s["eligible"].sum() < 5:
        sys.exit(f"Only {s['eligible'].sum()} {a.position.lower()}s have {a.min_minutes}+ minutes; "
                 "lower --min-minutes or pick another position.")

    os.makedirs(a.out_dir, exist_ok=True)
    s.reset_index().rename(columns={"index": "player_id"}).to_csv(
        os.path.join(a.out_dir, f"summary_{a.competition}{'_adjusted' if a.adjusted else ''}.csv"), index=False)

    for name in a.player:
        pid, others = find_player(s, name)
        if pid is None:
            print(f"'{name}': no player found. Try --list {name.split()[-1]}")
            continue
        row = s.loc[pid]
        if row["role.name"] != a.position:
            print(f"Note: {row['player_name']} is listed as a {row['role.name']}, "
                  f"but is being compared with {a.position.lower()}s.")
        if row["minutes"] <= a.min_minutes:
            print(f"Note: {row['player_name']} played only {int(row['minutes'])} minutes.")
        path, mins = plot_player(s, pid, a.position, a.min_minutes, a.adjusted, a.competition, season, a.out_dir)
        extra = f" (other matches: {', '.join(others[:3])})" if others else ""
        print(f"Saved {path}  [{row['player_name']}, {int(mins)} min]{extra}")


if __name__ == "__main__":
    main()