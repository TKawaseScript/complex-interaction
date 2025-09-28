# -*- coding: utf-8 -*-
# run_bc_paper_py2.py  (Python 2.7)
# Purpose: Single-network analysis aligned with Broido & Clauset goals,
#          with CSN(2009) rigorous GoF p-value using multiprocessing.
# Inputs:
#   - histogram CSVs: ./histcsvs/*.csv  with columns (xvalue,counts)
#   - OR degree sequences: ./degseqs/*.txt  one integer per line
# Outputs:
#   - paper_analysis_results.csv       (n, xmin, ntail, alpha, KS, p_gof, log-likelihoods, best_model)
#   - paper_lrtests_vs_PL.csv          (PL vs alternatives: LR, Z, p)
#   - figs/*.pdf                       (CCDF overlays for PL/TPL)
#
# Notes:
#   - This script uses a pure-Python discrete likelihood implementation
#     and multiprocessing to parallelize CSN GoF resampling.

import os
import sys
import glob
import math
import time
import random
import numpy as np
import pandas as pd

# plotting (optional)
HAVE_MPL = True
try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
except Exception:
    HAVE_MPL = False

# import patched sfanalysis (must be patched beforehand)
sys.path.append('./code')
import sfanalysis as sf

# ---------------- CONFIG ----------------
USE_HIST_CSV = True        # True: read ./histcsvs/*.csv, False: read ./degseqs/*.txt
CSV_DIR      = './histcsvs/'
DEG_DIR      = './degseqs/'

OUT_ANALYSIS = './paper_analysis_results.csv'
OUT_LR       = './paper_lrtests_vs_PL.csv'
FIG_DIR      = './figs'

# CSN GoF settings
BOOT_REPS = 1000            # increase to 1000-2000 for publication
RNG_SEED  = 123
N_JOBS    = 12             # number of processes for parallel bootstrap

MIN_TAIL  = 10             # minimum tail size to fit

# -------------- utils --------------
def ensure_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)

def load_hist_csv(path):
    df = pd.read_csv(path)
    lc = [c.lower().strip() for c in df.columns]
    if 'xvalue' not in lc or 'counts' not in lc:
        raise ValueError("CSV must have columns: xvalue, counts -> %s" % path)
    x_col = df.columns[lc.index('xvalue')]
    c_col = df.columns[lc.index('counts')]
    k = df[x_col].astype(int).values
    c = df[c_col].astype(int).values
    deg = np.repeat(k, c)
    return np.asarray(deg, dtype=np.int64)

def load_deg_txt(path):
    arr = np.loadtxt(path, dtype=np.int64)
    return arr

# ---------- discrete zetas and pmfs ----------
def zeta_hurwitz_approx(alpha, xmin, kcap, tol=1e-10):
    s = 0.0
    k = int(xmin)
    cap = int(kcap) + 4000
    while k <= cap:
        term = k ** (-alpha)
        s += term
        if term < tol and k > kcap:
            break
        k += 1
    return s

def pmf_pl(k, alpha, xmin, Z):
    return (k ** (-alpha)) / Z

def Z_tpl(alpha, lam, xmin, kcap, tol=1e-12):
    s = 0.0
    k = int(xmin)
    cap = int(kcap) + 4000
    while k <= cap:
        term = (k ** (-alpha)) * math.exp(-lam * k)
        s += term
        if term < tol and k > kcap:
            break
        k += 1
    return s

def pmf_tpl(k, alpha, lam, xmin, Z):
    return (k ** (-alpha)) * math.exp(-lam * k) / Z

def pmf_exp(k, lam, xmin):
    q = math.exp(-lam)
    return (1.0 - q) * (q ** (k - xmin))

def Z_logn(mu, sigma, xmin, kcap, tol=1e-12):
    s = 0.0
    k = int(xmin)
    cap = int(kcap) + 4000
    invs = 1.0 / (sigma * math.sqrt(2.0 * math.pi))
    while k <= cap:
        dens = invs * (1.0 / k) * math.exp(-(math.log(k) - mu) ** 2 / (2.0 * sigma * sigma))
        s += dens
        if dens < tol and k > kcap:
            break
        k += 1
    return s

def pmf_logn(k, mu, sigma, xmin, Z):
    invs = 1.0 / (sigma * math.sqrt(2.0 * math.pi))
    dens = invs * (1.0 / k) * math.exp(-(math.log(k) - mu) ** 2 / (2.0 * sigma * sigma))
    return dens / Z

def Z_stex(lam, beta, xmin, kcap, tol=1e-12):
    s = 0.0
    k = int(xmin)
    cap = int(kcap) + 4000
    while k <= cap:
        term = math.exp(-lam * (k ** beta))
        s += term
        if term < tol and k > kcap:
            break
        k += 1
    return s

def pmf_stex(k, lam, beta, xmin, Z):
    return math.exp(-lam * (k ** beta)) / Z

# ---------- KS and loglik ----------
def ks_discrete(tail, pmf_func):
    n = len(tail)
    counts = np.bincount(tail)
    sup = np.arange(len(counts))
    mask = (sup >= int(np.min(tail)))
    sup = sup[mask]
    counts = counts[mask]
    emp_pdf = counts.astype(float) / float(n)
    emp_ccdf = emp_pdf[::-1].cumsum()[::-1]
    model_pdf = np.array([pmf_func(int(k)) for k in sup])
    model_ccdf = model_pdf[::-1].cumsum()[::-1]
    return float(np.max(np.abs(emp_ccdf - model_ccdf)))

def loglik(tail, pmf_func):
    s = 0.0
    for k in tail:
        p = pmf_func(int(k))
        if p <= 0.0 or (not np.isfinite(p)):
            return -np.inf
        s += math.log(p)
    return float(s)

# ---------- PL / alternatives fits ----------
def fit_pl_tail(tail, xmin):
    logs = np.log(tail).sum()
    n = len(tail)
    kmax = int(np.max(tail))
    def nll(a):
        if a <= 1.01 or a > 6.0:
            return np.inf
        Z = zeta_hurwitz_approx(a, xmin, kmax)
        if (not np.isfinite(Z)) or Z <= 0.0:
            return np.inf
        return a * logs + n * math.log(Z)
    grid = np.linspace(1.05, 5.5, 70)
    best_a, best_v = None, np.inf
    for a in grid:
        v = nll(a)
        if v < best_v:
            best_v, best_a = v, a
    a = best_a
    step = 0.3
    for _ in range(35):
        improved = False
        for aa in (a - step, a + step):
            v = nll(aa)
            if v < best_v:
                best_v, a = v, aa
                improved = True
        step *= 0.5
        if (not improved) and step < 1e-4:
            break
    alpha = float(a)
    Zpl = zeta_hurwitz_approx(alpha, xmin, kmax)
    pmf = lambda kk: pmf_pl(kk, alpha, xmin, Zpl)
    ks = ks_discrete(tail, pmf)
    ll = loglik(tail, pmf)
    return {'alpha': alpha, 'ks': ks, 'll': ll, 'pmf': pmf}

def choose_xmin_by_ks(deg):
    k = np.asarray(deg, dtype=np.int64)
    k = k[k >= 1]
    if k.size == 0:
        return None
    k.sort()
    uniques = np.unique(k)
    best = None
    for xmin in uniques:
        tail = k[k >= xmin]
        if len(tail) < MIN_TAIL:
            continue
        res = fit_pl_tail(tail, xmin)
        ks = res['ks']
        if (best is None) or (ks < best[1]):
            best = (int(xmin), float(ks), float(res['alpha']))
    if best is None:
        return None
    return {'xmin': best[0], 'alpha0': best[2]}

def fit_tpl_tail(tail, xmin):
    from scipy.optimize import minimize
    kmax = int(np.max(tail))
    def nll(params):
        a, lam = params
        if a <= 1.01 or a > 6.0 or lam <= 1e-6 or lam > 5.0:
            return np.inf
        Zt = Z_tpl(a, lam, xmin, kmax)
        if (not np.isfinite(Zt)) or Zt <= 0.0:
            return np.inf
        s = 0.0
        for k in tail:
            p = (k ** (-a)) * math.exp(-lam * k) / Zt
            if p <= 0.0 or (not np.isfinite(p)):
                return np.inf
            s -= math.log(p)
        return s
    res = minimize(nll, x0=[2.4, 0.1], bounds=[(1.02, 6.0), (1e-5, 5.0)], method='L-BFGS-B')
    a, lam = float(res.x[0]), float(res.x[1])
    Zt = Z_tpl(a, lam, xmin, kmax)
    pmf = lambda kk: pmf_tpl(kk, a, lam, xmin, Zt)
    ks = ks_discrete(tail, pmf)
    ll = loglik(tail, pmf)
    return {'alpha': a, 'lam': lam, 'ks': ks, 'll': ll, 'pmf': pmf}

def fit_exp_tail(tail, xmin):
    kp = tail - xmin
    mean_kp = np.mean(kp)
    if mean_kp <= 0.0:
        lam = 5.0
    else:
        q = mean_kp / (1.0 + mean_kp)
        q = min(max(q, 1e-6), 1.0 - 1e-6)
        lam = -math.log(q)
    pmf = lambda kk: pmf_exp(kk, lam, xmin)
    ks = ks_discrete(tail, pmf)
    ll = loglik(tail, pmf)
    return {'lam': float(lam), 'ks': ks, 'll': ll, 'pmf': pmf}

def fit_logn_tail(tail, xmin):
    from scipy.optimize import minimize
    logs = np.log(tail)
    mu0, sig0 = float(np.mean(logs)), float(np.std(logs) + 1e-6)
    kmax = int(np.max(tail))
    def nll(params):
        mu, sig = params
        if sig <= 1e-3 or sig > 5.0:
            return np.inf
        ZN = Z_logn(mu, sig, xmin, kmax)
        if (not np.isfinite(ZN)) or ZN <= 0.0:
            return np.inf
        s = 0.0
        inv = 1.0 / (sig * math.sqrt(2.0 * math.pi))
        for k in tail:
            dens = inv * (1.0 / k) * math.exp(-(math.log(k) - mu) ** 2 / (2.0 * sig * sig))
            p = dens / ZN
            if p <= 0.0 or (not np.isfinite(p)):
                return np.inf
            s -= math.log(p)
        return s
    res = minimize(nll, x0=[mu0, max(sig0, 0.2)], bounds=[(0.0, 20.0), (1e-3, 5.0)], method='L-BFGS-B')
    mu, sig = float(res.x[0]), float(res.x[1])
    ZN = Z_logn(mu, sig, xmin, kmax)
    inv = 1.0 / (sig * math.sqrt(2.0 * math.pi))
    pmf = lambda kk: (inv * (1.0 / kk) * math.exp(-(math.log(kk) - mu) ** 2 / (2.0 * sig * sig))) / ZN
    ks = ks_discrete(tail, pmf)
    ll = loglik(tail, pmf)
    return {'mu': mu, 'sigma': sig, 'ks': ks, 'll': ll, 'pmf': pmf}

def fit_stex_tail(tail, xmin):
    from scipy.optimize import minimize
    kmax = int(np.max(tail))
    def nll(params):
        lam, beta = params
        if lam <= 1e-6 or lam > 10.0 or beta <= 0.2 or beta > 3.0:
            return np.inf
        ZS = Z_stex(lam, beta, xmin, kmax)
        if (not np.isfinite(ZS)) or ZS <= 0.0:
            return np.inf
        s = 0.0
        for k in tail:
            p = math.exp(-lam * (k ** beta)) / ZS
            if p <= 0.0 or (not np.isfinite(p)):
                return np.inf
            s -= math.log(p)
        return s
    res = minimize(nll, x0=[0.1, 0.8], bounds=[(1e-5, 10.0), (0.2, 3.0)], method='L-BFGS-B')
    lam, beta = float(res.x[0]), float(res.x[1])
    ZS = Z_stex(lam, beta, xmin, kmax)
    pmf = lambda kk: math.exp(-lam * (kk ** beta)) / ZS
    ks = ks_discrete(tail, pmf)
    ll = loglik(tail, pmf)
    return {'lam': lam, 'beta': beta, 'ks': ks, 'll': ll, 'pmf': pmf}

# ---------- Vuong test ----------
def vuong_pl_vs(model_pl, model_alt, tail):
    diffs = []
    for k in tail:
        p1 = model_pl(int(k)); p2 = model_alt(int(k))
        if p1 <= 0.0 or p2 <= 0.0 or (not np.isfinite(p1)) or (not np.isfinite(p2)):
            continue
        diffs.append(math.log(p1) - math.log(p2))
    diffs = np.asarray(diffs, dtype=np.float64)
    n = len(diffs)
    if n < 10:
        return (np.nan, np.nan, np.nan)
    m = float(np.mean(diffs))
    sd = float(np.std(diffs, ddof=1))
    if sd <= 0.0:
        return (np.nan, np.nan, np.nan)
    z = m / (sd / math.sqrt(n))
    from math import erf, sqrt
    p = 2.0 * (1.0 - 0.5 * (1.0 + erf(abs(z) / sqrt(2.0))))
    LR = float(np.sum(diffs))
    return (LR, z, p)

# ---------- CSN GoF with multiprocessing ----------
from multiprocessing import Pool

def _one_rep(args):
    # worker function for a single bootstrap replicate
    seed, n_tail, xmin_obs, alpha_obs, kcap = args
    rs = np.random.RandomState(seed)
    # build PL CDF (finite) once per worker
    Z = zeta_hurwitz_approx(alpha_obs, xmin_obs, kcap)
    cdf = []
    acc = 0.0
    k = int(xmin_obs)
    cap = int(kcap) + 4000
    while True:
        p = (k ** (-alpha_obs)) / Z
        acc += p
        cdf.append(acc)
        if 1.0 - acc < 1e-8 or k >= cap:
            break
        k += 1
    sup = np.arange(int(xmin_obs), int(xmin_obs) + len(cdf))
    u = rs.rand(n_tail)
    idx = np.searchsorted(cdf, u)
    syn = sup[idx]
    # re-estimate xmin on synthetic
    xm = choose_xmin_by_ks(syn)
    if xm is None:
        return None
    xmin_syn = xm['xmin']
    tail_syn = syn[syn >= xmin_syn]
    fit_syn  = fit_pl_tail(tail_syn, xmin_syn)
    return float(fit_syn['ks'])

def csn_gof_pvalue_parallel(deg, xmin_obs, alpha_obs, reps, rng_seed, n_jobs):
    k = np.asarray(deg, dtype=np.int64)
    k = k[k >= 1]
    kcap = int(np.max(k))
    tail_obs = k[k >= xmin_obs]
    n_tail = len(tail_obs)
    Zobs = zeta_hurwitz_approx(alpha_obs, xmin_obs, kcap)
    pmf_obs = lambda kk: pmf_pl(kk, alpha_obs, xmin_obs, Zobs)
    ks_obs = ks_discrete(tail_obs, pmf_obs)
    # seed schedule per replicate
    base = int(rng_seed)
    arglist = [(base + i, n_tail, int(xmin_obs), float(alpha_obs), kcap) for i in range(reps)]
    pool = Pool(processes=int(n_jobs))
    ks_list = pool.map(_one_rep, arglist)
    pool.close(); pool.join()
    cnt = 0
    valid = 0
    for ks in ks_list:
        if ks is None or (not np.isfinite(ks)):
            continue
        valid += 1
        if ks >= ks_obs - 1e-12:
            cnt += 1
    if valid == 0:
        return (np.nan, ks_obs)
    # scale by valid replicates (standard practice when a few replicates fail to fit)
    pval = float(cnt) / float(valid)
    return (pval, ks_obs)

# ---------- CCDF plot ----------
def plot_ccdf_with_fits(out_pdf, deg, xmin, pl_fit, tpl_fit):
    """
    Produce a publication-friendly CCDF figure:
      - Empirical CCDF as points (discrete)
      - Discrete model CCDFs (PL/TPL) as small points
      - Smooth guide curves for models via log-log interpolation
    Notes:
      - Underlying models are discrete; smooth curves are for visualization only.
      - Requires matplotlib (skips silently if not available).
    """
    if not HAVE_MPL:
        return

    import numpy as np
    import matplotlib.pyplot as plt

    # 1) prepare empirical CCDF (discrete)
    k = np.asarray(deg, dtype=np.int64)
    k = k[k >= 1]
    if k.size == 0:
        return
    k.sort()
    vals, counts = np.unique(k, return_counts=True)
    pdf_emp = counts.astype(float) / float(k.size)
    ccdf_emp = pdf_emp[::-1].cumsum()[::-1]

    # 2) build integer support for model CCDFs on tail
    sup_min = int(max(xmin, vals.min()))
    sup_max = int(vals.max())
    sup = np.arange(sup_min, sup_max + 1, dtype=int)

    # 3) discrete model CCDFs (PL, TPL) on integer grid
    #    reuse discrete normalizers from this script
    try:
        Zpl = zeta_hurwitz_approx(pl_fit['alpha'], xmin, sup_max)
        pl_pdf = np.array([pmf_pl(int(t), pl_fit['alpha'], xmin, Zpl) if t >= xmin else 0.0 for t in sup])
        pl_ccdf = pl_pdf[::-1].cumsum()[::-1]
    except Exception:
        pl_ccdf = None

    try:
        a2 = tpl_fit['alpha']; lam = tpl_fit['lam']
        Zt = Z_tpl(a2, lam, xmin, sup_max)
        tpl_pdf = np.array([pmf_tpl(int(t), a2, lam, xmin, Zt) if t >= xmin else 0.0 for t in sup])
        tpl_ccdf = tpl_pdf[::-1].cumsum()[::-1]
    except Exception:
        tpl_ccdf = None

    # 4) smooth guide curves via log-log interpolation of discrete CCDFs
    #    (visualization only; does not change the discrete model)
    def smooth_from_discrete(x_support, y_ccdf, npts=400):
        x_support = np.asarray(x_support, dtype=float)
        y_ccdf = np.asarray(y_ccdf, dtype=float)
        # keep only positive y values for log interpolation
        m = (x_support > 0) & (y_ccdf > 0)
        xs = x_support[m]
        ys = y_ccdf[m]
        if xs.size < 3:
            return None, None
        xq = np.exp(np.linspace(np.log(xs.min()), np.log(xs.max()), npts))
        # log-log linear interpolation
        yq = np.exp(np.interp(np.log(xq), np.log(xs), np.log(ys)))
        return xq, yq

    xq_pl, yq_pl = (None, None)
    xq_tp, yq_tp = (None, None)
    if pl_ccdf is not None:
        xq_pl, yq_pl = smooth_from_discrete(sup, pl_ccdf, npts=500)
    if tpl_ccdf is not None:
        xq_tp, yq_tp = smooth_from_discrete(sup, tpl_ccdf, npts=500)

    # 5) plot
    ensure_dir(os.path.dirname(out_pdf))
    plt.figure()

    # empirical CCDF: points only
    plt.loglog(vals, ccdf_emp, marker='o', linestyle='None', label='Empirical CCDF')

    # discrete model points (small markers)
    if pl_ccdf is not None:
        plt.loglog(sup, pl_ccdf, marker='.', linestyle='None', label='PL (discrete)')
    if tpl_ccdf is not None:
        plt.loglog(sup, tpl_ccdf, marker='.', linestyle='None', label='TPL (discrete)')

    # smooth guide curves
    if (xq_pl is not None) and (yq_pl is not None):
        plt.loglog(xq_pl, yq_pl, linewidth=2.0, label='PL (smooth guide)')
    if (xq_tp is not None) and (yq_tp is not None):
        plt.loglog(xq_tp, yq_tp, linewidth=2.0, linestyle='--', label='TPL (smooth guide)')

    plt.xlabel('k')
    plt.ylabel('CCDF')
    plt.legend(loc='best')
    plt.grid(True, which='both', linestyle=':', alpha=0.3)
    plt.tight_layout()
    plt.savefig(out_pdf, bbox_inches='tight')
    plt.close()

# ---------- main ----------
def main():
    t0 = time.time()
    # build catalog for sfanalysis (reproducibility)
    if USE_HIST_CSV:
        files = sorted([os.path.basename(p) for p in glob.glob(os.path.join(CSV_DIR, '*.csv'))])
        if not files:
            raise SystemExit("No CSV in %s" % CSV_DIR)
        deg_dir = CSV_DIR if CSV_DIR.endswith(os.sep) else (CSV_DIR + os.sep)
    else:
        files = sorted([os.path.basename(p) for p in glob.glob(os.path.join(DEG_DIR, '*.txt'))])
        if not files:
            raise SystemExit("No TXT in %s" % DEG_DIR)
        deg_dir = DEG_DIR if DEG_DIR.endswith(os.sep) else (DEG_DIR + os.sep)

    deg_df = pd.DataFrame({'degseq_file': [str(f) for f in files]}).set_index('degseq_file')
    for c in ['ppl','tpl','lnorm','exp','dexp','pl','tpl_xmin','lnorm_xmin','exp_xmin','dexp_xmin']:
        deg_df[c] = ''
    deg_df['xmin'] = 1

    # run sfanalysis baseline
    analysis_df = sf.analyze_degree_sequences(deg_dir, deg_df, overwrite=True)
    analysis_df['fp_gml'] = analysis_df.index.astype(str)

    rows = []
    lr_rows = []

    for fname in files:
        if USE_HIST_CSV:
            deg = load_hist_csv(os.path.join(CSV_DIR, fname))
        else:
            deg = load_deg_txt(os.path.join(DEG_DIR, fname))
        n_all = int(deg.size)

        xm = choose_xmin_by_ks(deg)
        if xm is None:
            rows.append({'file': fname, 'n': n_all, 'xmin': np.nan, 'ntail': 0,
                         'alpha': np.nan, 'KS': np.nan, 'p_gof': np.nan,
                         'll_PL': np.nan, 'll_TPL': np.nan, 'll_LOGN': np.nan,
                         'll_EXP': np.nan, 'll_STEX': np.nan, 'best_model': 'NA'})
            continue

        xmin = int(xm['xmin'])
        tail = deg[deg >= xmin]
        ntail = int(tail.size)

        # PL fit
        pl = fit_pl_tail(tail, xmin)

        # rigorous CSN GoF p-value with parallel bootstrap
        p_gof, ks_obs = csn_gof_pvalue_parallel(deg, xmin, pl['alpha'], BOOT_REPS, RNG_SEED, N_JOBS)

        # alternatives
        tpl  = fit_tpl_tail(tail, xmin)
        logn = fit_logn_tail(tail, xmin)
        expm = fit_exp_tail(tail, xmin)
        stex = fit_stex_tail(tail, xmin)

        # log-likelihoods
        Ls = {'PL': pl['ll'], 'TPL': tpl['ll'], 'LOGN': logn['ll'], 'EXP': expm['ll'], 'STEX': stex['ll']}
        best_model = max(Ls.items(), key=lambda kv: kv[1])[0]

        # Vuong vs PL
        for mname, mpmf in [('TPL', tpl['pmf']), ('LOGN', logn['pmf']), ('EXP', expm['pmf']), ('STEX', stex['pmf'])]:
            LR, Z, p = vuong_pl_vs(pl['pmf'], mpmf, tail)
            lr_rows.append({'file': fname, 'xmin': xmin, 'ntail': ntail,
                            'alpha_PL': round(pl['alpha'], 4), 'compare_to': mname,
                            'LR': None if (LR!=LR) else round(LR, 3),
                            'Z': None if (Z!=Z) else round(Z, 3),
                            'p_Vuong': None if (p!=p) else round(p, 3)})

        rows.append({'file': fname, 'n': n_all, 'xmin': xmin, 'ntail': ntail,
                     'alpha': round(pl['alpha'], 4), 'KS': round(ks_obs, 5), 'p_gof': None if (p_gof!=p_gof) else round(p_gof, 3),
                     'll_PL': round(pl['ll'], 3), 'll_TPL': round(tpl['ll'], 3),
                     'll_LOGN': round(logn['ll'], 3), 'll_EXP': round(expm['ll'], 3),
                     'll_STEX': round(stex['ll'], 3), 'best_model': best_model})

        # CCDF figure
        out_pdf = os.path.join(FIG_DIR, os.path.splitext(fname)[0] + '_ccdf.pdf')
        plot_ccdf_with_fits(out_pdf, deg, xmin, pl, tpl)

    pd.DataFrame(rows).set_index('file').sort_index().to_csv(OUT_ANALYSIS)
    pd.DataFrame(lr_rows).to_csv(OUT_LR, index=False)

    print("Saved:")
    print("  %s" % OUT_ANALYSIS)
    print("  %s" % OUT_LR)
    if HAVE_MPL:
        print("  figs/*.pdf (CCDF overlays)")
    else:
        print("  (matplotlib not installed: skipped figures)")

    print("NOTE:")
    print(" - p_gof uses rigorous CSN MC with xmin re-estimated per synthetic.")
    print(" - Increase BOOT_REPS for publication; current = %d" % BOOT_REPS)
    print(" - Interpret cautiously if ntail < 50.")
    print("Elapsed [s]: %.2f" % (time.time() - t0))

if __name__ == '__main__':
    main()

