"""
Simulated men's college soccer player metrics (Wyscout-style), one row per player-season,
for every team-season in rpi_standings_all_fixed.csv.

- Rosters persist across seasons (Fr -> So -> Jr -> Sr -> Gr), with players developing over time.
- Team strength comes from RPI rank (within season) and win %, and shifts every metric
  (better teams: more possession, more shots/xG, better passing, fewer defensive actions).
- Games played come from the team's actual W-L-T record in the standings file.
- Derived metrics are computed from their components so the data is internally consistent
  (e.g. goals <= shots, head goals <= goals, xG+xA = xG + xA, successful attacking actions =
  SoT + accurate crosses + successful dribbles).

Usage: python simulate_player_metrics.py <standings.csv> <out.csv> [seed]
"""
import sys
import numpy as np
import pandas as pd

SRC = sys.argv[1] if len(sys.argv) > 1 else "rpi_standings_all_fixed.csv"
OUT = sys.argv[2] if len(sys.argv) > 2 else "simulated_player_metrics.csv"
SEED = int(sys.argv[3]) if len(sys.argv) > 3 else 42
rng = np.random.default_rng(SEED)

FIRST = """Aidan Alex Andres Ben Blake Brandon Caleb Cameron Carlos Chris Cole Connor Daniel David Diego Dylan Eli
Ethan Evan Felipe Finn Gabriel Gavin Hugo Ian Isaac Jack Jacob Jake Jalen Javier Joao Jonah Jordan Jose Josh Julian
Kai Kevin Kofi Leo Liam Logan Lucas Luis Malik Marco Mateo Matt Max Miguel Nathan Nico Noah Oliver Omar Owen Pablo
Patrick Rafael Riley Ryan Sam Santiago Sean Seth Simon Thomas Tyler Victor Will Xavier Yusuf Zach Emeka Tomas Anders
Lukas Jonas Kwame Sergio Ivan Mason Henry Charlie Theo Callum Rhys Declan Oscar Elias Kenji Andre Bryce Aaron""".split()
LAST = """Adams Alvarez Anderson Baker Barnes Bell Bennett Brooks Brown Campbell Carter Castillo Chen Clark Collins
Cooper Cruz Davis Diaz Edwards Evans Fischer Flores Foster Garcia Gomez Gonzalez Gray Green Hall Harris Hernandez
Hill Hughes Jackson James Jensen Johnson Jones Kelly Kim King Kowalski Lee Lewis Lopez Martin Martinez Meyer Miller
Mitchell Moore Morales Morgan Murphy Nelson Nguyen Okafor Ortiz Owens Parker Perez Peterson Phillips Price Ramirez
Reed Reyes Richardson Rivera Roberts Robinson Rodriguez Rogers Ross Russell Sanchez Santos Schmidt Scott Silva Smith
Stewart Sullivan Taylor Thompson Torres Turner Walker Ward Watson White Williams Wilson Wright Young Mensah Lindqvist
Okoro Becker Dubois Rossi Novak Tanaka Petrov Larsen Murray Doyle Byrne Mwangi Haaland Mueller Costa Ferreira""".split()

# ---------------- position baselines (per 90 / %) -------------------------------------------------
# Roughly calibrated to Wyscout distributions for lower-tier pro / NCAA D1 outfield players.
POS = ["GK", "CB", "FB", "DM", "CM", "AM", "W", "CF"]
ROSTER_TEMPLATE = {"GK": 3, "CB": 5, "FB": 4, "DM": 2, "CM": 4, "AM": 2, "W": 4, "CF": 3}  # 27 players
STARTERS = {"GK": 1, "CB": 2, "FB": 2, "DM": 1, "CM": 2, "AM": 1, "W": 2, "CF": 1}  # 4-2-3-1ish, 12 -> trimmed

B = {  # metric: {pos: mean}
 "shots":        dict(GK=0.0, CB=0.45, FB=0.45, DM=0.65, CM=0.95, AM=1.7, W=1.9, CF=2.7),
 "xg_per_shot":  dict(GK=0.0, CB=0.11, FB=0.06, DM=0.06, CM=0.07, AM=0.085, W=0.085, CF=0.115),
 "sot_pct":      dict(GK=0.0, CB=30, FB=28, DM=28, CM=31, AM=34, W=34, CF=38),
 "head_share":   dict(GK=0.0, CB=0.60, FB=0.15, DM=0.15, CM=0.12, AM=0.08, W=0.08, CF=0.22),
 "passes":       dict(GK=25, CB=42, FB=38, DM=45, CM=42, AM=33, W=26, CF=19),
 "pass_acc":     dict(GK=62, CB=82, FB=76, DM=81, CM=79, AM=75, W=72, CF=68),
 "long_share":   dict(GK=0.50, CB=0.16, FB=0.11, DM=0.10, CM=0.08, AM=0.05, W=0.04, CF=0.03),
 "long_acc":     dict(GK=42, CB=50, FB=45, DM=55, CM=55, AM=50, W=45, CF=40),
 "prog_passes":  dict(GK=1.5, CB=6.5, FB=6.5, DM=7.5, CM=7.0, AM=5.5, W=4.0, CF=1.8),
 "prog_acc":     dict(GK=55, CB=72, FB=65, DM=70, CM=68, AM=62, W=58, CF=55),
 "final3":       dict(GK=0.8, CB=4.5, FB=5.0, DM=6.0, CM=6.0, AM=4.5, W=3.0, CF=1.5),
 "final3_acc":   dict(GK=45, CB=70, FB=65, DM=72, CM=70, AM=66, W=62, CF=58),
 "pen_area":     dict(GK=0.0, CB=0.3, FB=1.5, DM=0.9, CM=1.5, AM=2.6, W=2.5, CF=1.3),
 "pen_area_acc": dict(GK=0, CB=40, FB=38, DM=45, CM=45, AM=45, W=40, CF=42),
 "smart":        dict(GK=0.0, CB=0.10, FB=0.30, DM=0.40, CM=0.70, AM=1.30, W=0.80, CF=0.50),
 "smart_acc":    dict(GK=0, CB=35, FB=33, DM=40, CM=40, AM=42, W=38, CF=38),
 "through":      dict(GK=0.0, CB=0.15, FB=0.35, DM=0.50, CM=0.75, AM=1.20, W=0.80, CF=0.55),
 "through_acc":  dict(GK=0, CB=35, FB=33, DM=40, CM=40, AM=42, W=38, CF=38),
 "key_passes":   dict(GK=0.0, CB=0.10, FB=0.35, DM=0.25, CM=0.45, AM=0.95, W=0.80, CF=0.55),
 "shot_assists": dict(GK=0.02, CB=0.25, FB=0.75, DM=0.55, CM=0.95, AM=1.70, W=1.55, CF=1.00),
 "xa_per_sa":    dict(GK=0.05, CB=0.07, FB=0.09, DM=0.08, CM=0.09, AM=0.10, W=0.10, CF=0.10),
 "deep_compl":   dict(GK=0.0, CB=0.2, FB=0.8, DM=0.6, CM=1.0, AM=1.8, W=1.6, CF=1.0),
 "crosses":      dict(GK=0.0, CB=0.10, FB=2.4, DM=0.25, CM=0.6, AM=1.1, W=3.0, CF=0.6),
 "cross_acc":    dict(GK=0, CB=25, FB=30, DM=28, CM=30, AM=30, W=29, CF=27),
 "dribbles":     dict(GK=0.05, CB=0.7, FB=2.4, DM=1.8, CM=2.8, AM=5.2, W=6.5, CF=4.0),
 "dribble_pct":  dict(GK=60, CB=62, FB=53, DM=58, CM=55, AM=50, W=47, CF=44),
 "prog_runs":    dict(GK=0.0, CB=0.40, FB=1.6, DM=0.9, CM=1.5, AM=2.5, W=3.3, CF=1.8),
 "accels":       dict(GK=0.0, CB=0.30, FB=1.2, DM=0.6, CM=1.0, AM=1.8, W=2.6, CF=1.5),
 "off_duels":    dict(GK=0.3, CB=2.2, FB=6.0, DM=5.0, CM=6.5, AM=10.0, W=12.5, CF=12.0),
 "off_duel_pct": dict(GK=55, CB=45, FB=42, DM=43, CM=40, AM=38, W=37, CF=33),
 "def_duels":    dict(GK=0.3, CB=7.0, FB=8.5, DM=8.5, CM=7.0, AM=5.0, W=5.0, CF=3.5),
 "def_duel_pct": dict(GK=50, CB=66, FB=62, DM=62, CM=59, AM=55, W=55, CF=52),
 "aerials":      dict(GK=0.7, CB=6.5, FB=2.5, DM=3.5, CM=2.6, AM=1.8, W=1.4, CF=7.0),
 "aerial_pct":   dict(GK=80, CB=58, FB=46, DM=50, CM=45, AM=38, W=33, CF=40),
 "loose":        dict(GK=0.2, CB=2.0, FB=2.4, DM=3.0, CM=3.0, AM=2.6, W=2.4, CF=2.4),
 "interceptions":dict(GK=0.3, CB=5.2, FB=4.5, DM=5.0, CM=4.0, AM=2.8, W=2.6, CF=1.6),
 "sliding_90":   dict(GK=0.0, CB=0.45, FB=0.50, DM=0.55, CM=0.40, AM=0.25, W=0.25, CF=0.20),
 "clearances":   dict(GK=0.6, CB=5.0, FB=2.2, DM=1.3, CM=0.6, AM=0.3, W=0.4, CF=0.6),
 "recoveries":   dict(GK=5.5, CB=7.0, FB=6.8, DM=7.5, CM=6.5, AM=4.8, W=4.8, CF=3.2),
 "opp_half_rec_share": dict(GK=0.0, CB=0.10, FB=0.25, DM=0.25, CM=0.32, AM=0.45, W=0.45, CF=0.55),
 "losses":       dict(GK=4.0, CB=5.5, FB=9.0, DM=8.5, CM=10.0, AM=13.5, W=15.0, CF=15.5),
 "own_half_loss_share": dict(GK=0.95, CB=0.80, FB=0.40, DM=0.45, CM=0.30, AM=0.15, W=0.12, CF=0.08),
 "missed_balls": dict(GK=0.1, CB=0.9, FB=1.0, DM=0.9, CM=0.7, AM=0.5, W=0.6, CF=0.5),
 "pressing":     dict(GK=0.0, CB=1.2, FB=3.0, DM=4.0, CM=4.5, AM=5.5, W=5.0, CF=6.0),
 "fouls":        dict(GK=0.05, CB=1.2, FB=1.0, DM=1.6, CM=1.3, AM=1.1, W=1.0, CF=1.6),
 "yellow_90":    dict(GK=0.03, CB=0.17, FB=0.14, DM=0.20, CM=0.15, AM=0.11, W=0.10, CF=0.12),
 "red_90":       dict(GK=0.006, CB=0.012, FB=0.008, DM=0.010, CM=0.007, AM=0.005, W=0.004, CF=0.006),
}

# strength elasticity: metric multiplier = exp(k * strength_z); strength_z ~ N(0,1) (good teams > 0)
K = {"shots": .18, "xg_per_shot": .05, "passes": .14, "prog_passes": .10, "final3": .15, "pen_area": .18,
     "smart": .12, "through": .12, "key_passes": .18, "shot_assists": .18, "deep_compl": .20, "crosses": .08,
     "dribbles": .06, "prog_runs": .10, "accels": .08, "off_duels": .03, "def_duels": -.10, "aerials": -.06,
     "loose": -.03, "interceptions": -.10, "sliding_90": -.10, "clearances": -.20, "recoveries": -.05,
     "losses": -.02, "missed_balls": -.05, "pressing": .08, "fouls": -.05}
# additive (percentage points) per strength z
KP = {"sot_pct": 1.5, "pass_acc": 2.2, "long_acc": 2.0, "prog_acc": 2.0, "final3_acc": 2.5, "pen_area_acc": 2.0,
      "smart_acc": 2.0, "through_acc": 2.0, "cross_acc": 1.5, "dribble_pct": 1.5, "off_duel_pct": 1.5,
      "def_duel_pct": 1.5, "aerial_pct": 1.0}

# ---------------- teams --------------------------------------------------------------------------
st = pd.read_csv(SRC)
st["gp"] = st[["overall_w", "overall_l", "overall_t"]].sum(axis=1).clip(lower=1)
st["win_pct"] = (st.overall_w + 0.5 * st.overall_t) / st.gp
st["n_in_season"] = st.groupby("season_year").team.transform("count")
st["rank_pct"] = 1 - (st.rpi_rank - 1) / (st.n_in_season - 1)      # 1 = best
from scipy.stats import norm
st["strength_z"] = 0.75 * norm.ppf(st.rank_pct.clip(.01, .99)) + 0.25 * norm.ppf(st.win_pct.clip(.02, .98))
st["strength_z"] = (st.strength_z - st.strength_z.mean()) / st.strength_z.std()
st["team_possession"] = (50 + 4.5 * st.strength_z + rng.normal(0, 2, len(st))).clip(36, 64)
# team-level style noise so teams of the same strength still differ
st["style_direct"] = rng.normal(0, 1, len(st))   # + = more long balls / crosses, fewer short passes
st["style_press"] = rng.normal(0, 1, len(st))    # + = more pressing / opp-half recoveries

classes = ["Fr", "So", "Jr", "Sr", "Gr"]
next_class = {"Fr": "So", "So": "Jr", "Jr": "Sr", "Sr": "Gr", "Gr": None}
base_age = {"Fr": 18.6, "So": 19.6, "Jr": 20.6, "Sr": 21.6, "Gr": 22.8}

_pid = [0]
def new_player(pos, cls, strength_z):
    _pid[0] += 1
    return dict(player_id=f"P{_pid[0]:06d}",
                player_name=f"{rng.choice(FIRST)} {rng.choice(LAST)}",
                position=pos,
                foot=rng.choice(["Right", "Left", "Both"], p=[.72, .24, .04]),
                class_year=cls,
                height_cm=int(round(rng.normal({"GK": 189, "CB": 187, "CF": 183}.get(pos, 178), 5))),
                talent=rng.normal(0.35 * strength_z, 0.85),  # recruits track program strength
                age=round(base_age[cls] + rng.uniform(-.4, .6), 1),
                # persistent individual tendencies (per-metric multipliers)
                tend={m: rng.normal(0, .22) for m in B})

rosters = {}  # team -> list of player dicts (persist across seasons)
rows = []

for season in sorted(st.season_year.unique()):
    for _, t in st[st.season_year == season].iterrows():
        team = t.team
        prev = rosters.get(team, [])
        roster = []
        # carry over: advance class, some attrition (transfers/pros); grad students rarely stay
        for p in prev:
            nc = next_class[p["class_year"]]
            keep = nc is not None and rng.random() < {"So": .85, "Jr": .85, "Sr": .88, "Gr": .30}[nc]
            if keep:
                p = dict(p); p["class_year"] = nc; p["age"] = round(p["age"] + 1, 1)
                p["talent"] += rng.normal(.18, .25)  # development
                roster.append(p)
        # fill to template by position with recruits (mostly Fr, some transfers)
        for pos, n in ROSTER_TEMPLATE.items():
            have = sum(1 for p in roster if p["position"] == pos)
            for _ in range(max(0, n - have)):
                cls = rng.choice(classes, p=[.62, .10, .10, .08, .10])
                roster.append(new_player(pos, cls, t.strength_z))
        rosters[team] = roster

        gp = int(t.gp)
        z = t.strength_z
        # depth chart: by talent within position
        for pos in POS:
            group = sorted([p for p in roster if p["position"] == pos], key=lambda p: -p["talent"])
            n_start = STARTERS[pos]
            for depth, p in enumerate(group):
                if pos == "GK":
                    share = [0.92, 0.10, 0.0][min(depth, 2)]
                elif depth < n_start:
                    share = rng.uniform(.70, .95)
                elif depth == n_start:
                    share = rng.uniform(.30, .60)
                else:
                    share = rng.uniform(.0, .30)
                share *= rng.uniform(.85, 1.0) if rng.random() > .08 else rng.uniform(.2, .6)  # injuries
                apps = int(np.clip(round(gp * min(1, share * 1.35 + rng.uniform(0, .25))), 0, gp))
                minutes = int(round(gp * 90 * share))
                minutes = min(minutes, apps * 90) if apps else 0
                if pos == "GK" and depth == 2 and rng.random() < .7:
                    apps, minutes = 0, 0
                starts = int(min(apps, round(minutes / 82)))
                rows.append(dict(season_year=season, conference=t.conference, team=team,
                                 team_rpi_rank=int(t.rpi_rank), team_strength_z=round(z, 3),
                                 team_possession_pct=round(t.team_possession, 1),
                                 style_direct=t.style_direct, style_press=t.style_press,
                                 player_id=p["player_id"], player_name=p["player_name"], position=p["position"],
                                 class_year=p["class_year"], age=p["age"], height_cm=p["height_cm"],
                                 foot=p["foot"], team_games=gp, matches_played=apps, starts=starts,
                                 minutes_played=minutes, _talent=p["talent"], _tend=p["tend"]))

df = pd.DataFrame(rows)
# scale minutes so each team plays exactly 11 x 90 per game, capped at full availability
tot = df.groupby(["season_year", "team"]).minutes_played.transform("sum")
df["minutes_played"] = np.minimum((df.minutes_played * 990 * df.team_games / tot).round().astype(int),
                                  df.matches_played * 90)
df["starts"] = np.minimum(df.matches_played, (df.minutes_played / 82).round().astype(int))
n = len(df)
print(f"{n:,} player-season rows")

# ---------------- generate metrics ---------------------------------------------------------------
pos = df.position.values
z = df.team_strength_z.values
tal = df._talent.values
mins = df.minutes_played.values.astype(float)
direct = df.style_direct.values
press = df.style_press.values
# small-sample noise shrinks with minutes
noise_scale = 0.12 + 0.55 / np.sqrt(np.maximum(mins, 45) / 90)

def rate(m, talent_k=0.10, extra=0.0):
    base = np.array([B[m][p] for p in pos])
    tend = np.array([d[m] for d in df._tend])
    mult = np.exp(K.get(m, 0) * z + talent_k * tal + tend + extra + rng.normal(0, 1, n) * noise_scale * .5)
    return base * mult

def pct(m, talent_k=1.5, lo=0, hi=100, vol=None):
    base = np.array([B[m][p] for p in pos], dtype=float)
    tend = np.array([d[m] for d in df._tend]) * 15
    v = base + KP.get(m, 0) * z + talent_k * tal + tend
    sd = 3 + 6 * noise_scale
    if vol is not None:  # binomial-ish noise from attempt volume
        attempts = np.maximum(vol * mins / 90, 1)
        sd = np.sqrt(np.clip(v, 5, 95) * (100 - np.clip(v, 5, 95)) / attempts)
    return np.clip(v + rng.normal(0, 1, n) * sd, lo, hi)

M = {}
p90 = mins / 90.0
safe = np.where(p90 > 0, p90, np.nan)

# Shooting
shots90 = rate("shots", .18)
shots_tot = rng.poisson(shots90 * p90)
M["Shots, total"] = shots_tot
M["Shots / 90"] = shots_tot / safe
xg_per_shot = np.array([B["xg_per_shot"][p] for p in pos]) * np.exp(.05 * z + .06 * tal + rng.normal(0, .15, n))
xg_tot = np.round(shots_tot * xg_per_shot * np.exp(rng.normal(0, .12, n)), 2)
M["xG, Total"] = xg_tot
M["xG / 90"] = xg_tot / safe
goals = rng.poisson(xg_tot * np.exp(.05 * tal - .05))
goals = np.minimum(goals, shots_tot)
M["Goals"] = goals
M["Goals / 90"] = goals / safe
pens = np.minimum(goals, rng.binomial(goals, np.where(np.isin(pos, ["CF", "AM", "W"]), .12, .05)))
npg = goals - pens
M["Non-penalty goals, total"] = npg
M["Non-penalty goals, per 90 mins"] = npg / safe
head_share = np.array([B["head_share"][p] for p in pos])
heads = rng.binomial(npg, np.clip(head_share * np.exp(rng.normal(0, .3, n)), 0, .9))
M["Head goals, total"] = heads
M["Head goals, avg per 90 mins"] = heads / safe
M["Goal conversion (%)"] = np.where(shots_tot > 0, 100 * goals / np.maximum(shots_tot, 1), np.nan)
sot_pct = pct("sot_pct", 1.5, vol=shots90)
sot = rng.binomial(shots_tot, np.clip(sot_pct / 100, 0, 1))
sot = np.maximum(sot, goals - pens * 0)  # every goal is on target
sot = np.minimum(np.maximum(sot, goals), shots_tot)
M["Shots on target %"] = np.where(shots_tot > 0, 100 * sot / np.maximum(shots_tot, 1), np.nan)
M["Touches in box, per 90"] = shots90 * rng.uniform(1.8, 2.6, n) + rate("pen_area", .05) * .3

# Passing
passes90 = rate("passes", .06, extra=-.12 * direct)
M["Passes / 90"] = passes90
M["Pass accuracy"] = pct("pass_acc", 1.8, 40, 97)
long_share = np.array([B["long_share"][p] for p in pos]) * np.exp(.25 * direct - .12 * z + rng.normal(0, .2, n))
long90 = passes90 * long_share
M["Long passes / 90 minutes"] = long90
M["Accurate long passes (%)"] = pct("long_acc", 1.5, 10, 90, vol=long90)
short90 = passes90 - long90
M["Short/medium passes / 90"] = short90
# short accuracy consistent with overall: overall = (short*sa + long*la)/passes
sa = (M["Pass accuracy"] * passes90 - M["Accurate long passes (%)"] * long90) / np.maximum(short90, .01)
M["Short/medium passes %"] = np.clip(sa, 30, 99)
M["Progressive passes / 90"] = rate("prog_passes", .12, extra=.08 * direct)
M["Accurate progressive passes (%)"] = pct("prog_acc", 1.5, 20, 95, vol=M["Progressive passes / 90"])
M["Passes to final third / 90"] = rate("final3", .12)
M["Passes to final third %"] = pct("final3_acc", 1.5, 20, 95, vol=M["Passes to final third / 90"])
M["Passes to penalty area / 90"] = rate("pen_area", .15)
M["Passes to penalty area %"] = pct("pen_area_acc", 1.5, 0, 100, vol=M["Passes to penalty area / 90"])
M["Smart passes / 90"] = rate("smart", .25)
M["Accurate smart passes (%)"] = pct("smart_acc", 2, 0, 100, vol=M["Smart passes / 90"])
M["Through passes / 90"] = rate("through", .22)
M["Accurate through passes (%)"] = pct("through_acc", 2, 0, 100, vol=M["Through passes / 90"])
M["Deep completions passes / 90"] = rate("deep_compl", .18)

# Chance creation
sa90 = rate("shot_assists", .20)
sa_tot = rng.poisson(sa90 * p90)
M["Shot assists / 90"] = sa_tot / safe
xa_tot = np.round(sa_tot * np.array([B["xa_per_sa"][p] for p in pos]) * np.exp(.05 * z + rng.normal(0, .15, n)), 2)
M["xA / 90"] = xa_tot / safe
assists = np.minimum(rng.poisson(xa_tot * np.exp(.08 * tal)), sa_tot)
M["Assists / 90"] = assists / safe
kp_extra = rng.poisson(rate("key_passes", .2) * p90 * .6)
M["Key passes / 90"] = (assists + np.minimum(kp_extra, sa_tot - assists)) / safe  # includes assists, <= shot assists
M["Second assists / 90"] = rng.poisson(assists * .55 + rate("key_passes", .1) * p90 * .08) / safe
M["Third assists / 90"] = rng.poisson(assists * .3 + rate("key_passes", .1) * p90 * .04) / safe
M["xG + xA / 90"] = M["xG / 90"] + M["xA / 90"]

# Crossing
cross90 = rate("crosses", .06, extra=.2 * direct)
M["Crosses / 90"] = cross90
foot = df.foot.values
left_share = np.where(foot == "Left", .72, np.where(foot == "Right", .32, .5)) + rng.normal(0, .08, n)
left_share = np.clip(left_share, .02, .98)
M["Crosses from left flank, per 90"] = cross90 * left_share * .88
M["Crosses from right flank, per 90"] = cross90 * (1 - left_share) * .88  # ~12% from central areas
cacc = pct("cross_acc", 1.5, 0, 75, vol=cross90)
M["Cross accuracy"] = cacc
M["Cross accuracy from left flank (%)"] = np.clip(cacc + rng.normal(0, 4, n), 0, 80)
M["Cross accuracy from right flank (%)"] = np.clip(cacc + rng.normal(0, 4, n), 0, 80)
M["Crosses to six-yard box, per 90 mins"] = cross90 * rng.uniform(.10, .22, n)
M["Cross accuracy to six-yard box (%)"] = np.clip(cacc * rng.uniform(.6, 1.0, n), 0, 70)
M["Deep completed crosses / 90"] = np.minimum(cross90 * cacc / 100 * rng.uniform(.4, .75, n),
                                             M["Deep completions passes / 90"] + cross90 * cacc / 100)

# Carrying / duels
drib90 = rate("dribbles", .15)
M["Dribbles / 90"] = drib90
M["Dribbles %"] = pct("dribble_pct", 1.5, 15, 90, vol=drib90)
M["Progressive runs / 90"] = rate("prog_runs", .15)
M["Accelerations / 90"] = rate("accels", .12)
M["Offensive Duels / 90"] = rate("off_duels", .05)
M["Offensive Duels %"] = pct("off_duel_pct", 1.5, 10, 80, vol=M["Offensive Duels / 90"])
M["Defensive duels / 90 minutes"] = rate("def_duels", .03)
M["Defensive duels won (%)"] = pct("def_duel_pct", 1.5, 25, 92, vol=M["Defensive duels / 90 minutes"])
M["Aerial duels / 90 minutes"] = rate("aerials", .02)
M["Aerial duels won (%)"] = pct("aerial_pct", 1.2, 0, 100, vol=M["Aerial duels / 90 minutes"])
M["Loose ball duels / 90 minutes"] = rate("loose", .02)
M["Pressing duels, per 90"] = rate("pressing", .03, extra=.25 * press)

# Defending
opp_poss_min = 55 * (1 - df.team_possession_pct.values / 100)  # ~55 min ball-in-play
int90 = rate("interceptions", .06)
M["Interceptions / 90 minutes"] = int90
M["Interceptions / per 30 minutes of opponent possession"] = int90 * 30 / opp_poss_min
slide90 = rate("sliding_90", .0)
M["Sliding tackles / per 30 minutes of opponent possession"] = slide90 * 30 / opp_poss_min
M["Clearances / 90"] = rate("clearances", .0)
rec90 = rate("recoveries", .06, extra=.08 * press)
M["Recoveries / 90"] = rec90
ohr_share = np.clip(np.array([B["opp_half_rec_share"][p] for p in pos]) * np.exp(.15 * z + .15 * press
                    + rng.normal(0, .15, n)), 0, .95)
M["Opponent half recoveries / 90"] = rec90 * ohr_share
M["Successful defensive actions / 90"] = (M["Defensive duels / 90 minutes"] * M["Defensive duels won (%)"] / 100
                                          + int90 + slide90)
M["Missed balls / 90 minutes"] = rate("missed_balls", -.1)
M["Fouls / 90"] = rate("fouls", -.03)

# Ball losses
is_gk = pos == "GK"
loss90 = rate("losses", -.05, extra=.10 * direct)
M["Losses / 90"] = loss90
M["Own half losses / 90"] = loss90 * np.clip(np.array([B["own_half_loss_share"][p] for p in pos])
                                             * np.exp(rng.normal(0, .15, n)), 0, 1)
saa = (M["Shots / 90"] * np.nan_to_num(M["Shots on target %"]) / 100
       + cross90 * cacc / 100 + drib90 * M["Dribbles %"] / 100)
M["Successful attacking actions / 90"] = saa
M["Loss index"] = np.where((saa > 0.05) & ~is_gk, loss90 / np.maximum(saa, 1e-9), np.nan)

# Discipline
yel = rng.poisson(rate("yellow_90", .0) * p90)
red = rng.poisson(rate("red_90", .0) * p90)
M["Yellow cards, total"] = yel
M["Yellow cards / 90 minutes"] = yel / safe
M["Red cards, total"] = red
M["Red cards / 90 minutes"] = red / safe

# Goalkeeping & zone defending
is_gk = pos == "GK"
shots_against90 = np.exp(np.log(12.5) - .30 * z + rng.normal(0, .12, n))
M["Shots against / 90"] = np.where(is_gk, shots_against90, np.nan)
M["Exits, per 90 mins"] = np.where(is_gk, rng.gamma(6, .14, n) * np.exp(.1 * tal), np.nan)
zone_def = np.isin(pos, ["CB", "FB", "DM"])
M["Progressive passes allowed"] = np.where(zone_def, np.round(
    rng.poisson(np.exp(np.log(4.2) - .25 * z - .10 * tal) * p90)), np.nan)

# GK outfield-only metrics -> 0 where it makes no sense
for col in ["Crosses / 90", "Crosses from left flank, per 90", "Crosses from right flank, per 90",
            "Crosses to six-yard box, per 90 mins", "Deep completed crosses / 90", "Smart passes / 90",
            "Through passes / 90", "Passes to penalty area / 90", "Progressive runs / 90", "Accelerations / 90",
            "Pressing duels, per 90", "Touches in box, per 90"]:
    M[col] = np.where(is_gk, 0.0, M[col])
for col in ["Cross accuracy", "Cross accuracy from left flank (%)", "Cross accuracy from right flank (%)",
            "Cross accuracy to six-yard box (%)", "Accurate smart passes (%)", "Accurate through passes (%)",
            "Passes to penalty area %"]:
    M[col] = np.where(is_gk, np.nan, M[col])

# ---------------- assemble ---------------------------------------------------------------------
metric_order = [l.split("\t")[0] for l in open(sys.argv[4]).read().splitlines()[2:]] if len(sys.argv) > 4 \
    else list(M.keys())
id_cols = ["season_year", "conference", "team", "team_rpi_rank", "team_strength_z", "team_possession_pct",
           "player_id", "player_name", "position", "class_year", "age", "height_cm", "foot",
           "team_games", "matches_played", "starts", "minutes_played"]
out = df[id_cols].copy()
for m in metric_order:
    v = np.asarray(M[m], dtype=float)
    out[m] = np.round(v, 1 if ("%" in m or m in ("Pass accuracy", "Cross accuracy")) else 2)
# players with 0 minutes: rate metrics undefined
out.loc[out.minutes_played == 0, metric_order] = np.nan
for m in ["Goals", "Shots, total", "Head goals, total", "Non-penalty goals, total", "Yellow cards, total",
          "Red cards, total"]:
    out.loc[out.minutes_played == 0, m] = 0
out.to_csv(OUT, index=False)
print("wrote", OUT, out.shape)
