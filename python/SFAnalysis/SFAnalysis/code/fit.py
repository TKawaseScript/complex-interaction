import numpy as np
import scipy.optimize as op
import scipy.special as sp
import time
import integration_constants as ic

"""
Python 3 互換 SFAnalysis fit.py
- パワーロー、指数、対数正規、指数カットオフ、ストレッチ指数にフィット
- fit_distribution(x) を通して sfanalysis.py から呼び出せる
"""

def pl(x):
    """ パワーロー分布にフィット """
    xminV = np.trim_zeros(np.unique(x))
    fitV = np.zeros([len(xminV), 2])
    xminprev = min(xminV) - 1
    alstart = 1.01
    shift = 9.50
    alphaV = np.arange(alstart, alstart+shift, 0.01)
    zetaV = sp.zeta(alphaV)
    constV = zetaV
    for j in range(xminprev):
        constV += -(1+j)**(-alphaV)

    for i, xmin in enumerate(xminV):
        xtail = x[x>=xmin]
        ntail = len(xtail)
        Ls = -alphaV*np.sum(np.log(xtail)) - ntail*np.log(constV)
        aind = Ls.argmax()
        alpha = alphaV[aind]

        cdf = np.cumsum(np.arange(np.min(xtail), np.max(xtail)+1)**(-alpha)/constV[aind])
        xhist = np.histogram(xtail, bins=np.arange(np.min(xtail), np.max(xtail)+2))
        edf = np.cumsum(xhist[0])/float(ntail)
        ks = np.max(np.abs(cdf - edf))
        fitV[i] = np.array([ks, alpha])

        for j in range(xmin-xminprev):
            constV += -(xmin+j)**(-alphaV)
        xminprev = xmin

    ksind = fitV[:,0].argmin()
    ks = fitV[ksind,0]
    xmin = xminV[ksind]
    alpha = fitV[ksind,1]
    xtail = x[x>=xmin]
    ntail = len(xtail)
    const = sp.zeta(alpha) - np.sum(np.arange(1, xmin)**(-alpha))
    L = -alpha * np.sum(np.log(xtail)) - ntail*np.log(const)
    return [alpha, xmin, ntail, L, ks]

def exp(x):
    """ 指数分布にフィット """
    xmin = np.min(x)
    ntail = len(x)
    if len(np.unique(x)) < 2:
        lam, LV, convstatus = 0, 0, False
    else:
        lam0 = np.log(1 + float(ntail)/np.sum(x - xmin))
        negloglike = lambda lam: -np.sum(np.log(1-np.exp(-lam)) + lam*xmin - lam*x)
        res = op.minimize(negloglike, lam0, bounds=[(1e-9, None)], method='L-BFGS-B')
        lam = float(res.x)
        convstatus = res.success
        LV = np.log(1-np.exp(-lam)) + lam*xmin - lam*x
    return [lam, LV, convstatus]

def ln(x):
    """ 対数正規分布にフィット """
    xmin = np.min(x)
    ntail = len(x)
    def logpdf(x, mu, sigma):
        F = lambda x: sp.erfc((np.log(x)-mu)/(np.sqrt(2)*sigma))/2
        g = lambda x: F(x)-F(x+1)
        h = -np.log(F(xmin))+np.log(g(x))
        return h
    theta0 = np.array([0,1])
    negloglike = lambda theta: -np.sum(logpdf(x,theta[0],theta[1]))
    bnds = [(-ntail/5,None),(1e-1,None)]
    res = op.minimize(negloglike, theta0, bounds=bnds, method='L-BFGS-B')
    theta = res.x
    convstatus = res.success
    LV = logpdf(x, theta[0], theta[1])
    return [theta, LV, convstatus]

def plwc(x, alpha0=None):
    """ パワーロー指数カットオフ分布にフィット """
    xmin = np.min(x)
    ntail = len(x)
    if alpha0 is None:
        alpha0 = pl(x)[0]
    lam0 = exp(x)[0]
    theta0 = np.array([alpha0, lam0])
    negloglike = lambda theta: -np.sum(-np.log(ic.plwcconst(theta[0], theta[1], xmin)) - theta[0]*np.log(x) - theta[1]*x)
    res = op.minimize(negloglike, theta0, bounds=[(-1+1e-5,None),(1e-5,None)])
    theta = res.x
    convstatus = res.success
    alpha, lam = theta
    LV = -np.log(ic.plwcconst(alpha, lam, xmin)) - alpha*np.log(x) - lam*x
    return [alpha, lam, LV, convstatus]

def strexp(x):
    """ ストレッチ指数分布にフィット """
    xmin = np.min(x)
    ntail = len(x)
    def initialguessweib(x):
        shape = (np.sqrt(6)/np.pi)*np.std(np.log(x))
        scale = (np.sum(x**shape)/ntail)**(1/shape)
        return np.array([shape, scale])
    def logpdf(x, a, b, tol_pad=1e-8):
        F = lambda x: np.exp(-(x/b)**a)
        g = lambda x: F(x)-F(x+1)
        h = -np.log(F(xmin)+tol_pad)+np.log(g(x)+tol_pad)
        return h
    theta0 = initialguessweib(x)
    negloglike = lambda theta: -np.sum(logpdf(x,theta[0],theta[1]))
    res = op.minimize(negloglike, theta0, bounds=[(1e-5,1),(1e-2,None)], method='L-BFGS-B')
    theta = res.x
    convstatus = res.success
    LV = logpdf(x, theta[0], theta[1])
    return [theta, LV, convstatus]

# -----------------------------
# ラッパー関数：sfanalysis.py から呼び出す
# -----------------------------
def fit(x):
    """ 元の fit 関数ラッパー """
    alpha, xmin, ntail, L, ks = pl(x)
    lam, LV_exp, conv_exp = exp(x)
    theta_ln, LV_ln, conv_ln = ln(x)
    alpha_wc, lam_wc, LV_wc, conv_wc = plwc(x)
    theta_se, LV_se, conv_se = strexp(x)

    return {
        'pl': {'alpha': alpha, 'xmin': xmin, 'ntail': ntail, 'L': L, 'ks': ks},
        'exp': {'lam': lam, 'LV': LV_exp, 'conv': conv_exp},
        'ln': {'theta': theta_ln, 'LV': LV_ln, 'conv': conv_ln},
        'plwc': {'alpha': alpha_wc, 'lam': lam_wc, 'LV': LV_wc, 'conv': conv_wc},
        'strexp': {'theta': theta_se, 'LV': LV_se, 'conv': conv_se},
        'gamma': alpha  # スケールフリー判定用
    }

def fit_distribution(x):
    """ sfanalysis.py から呼ぶための関数 """
    return fit(x)
