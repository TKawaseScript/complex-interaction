import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import sys
import os

# --- パスを通す ---
code_path = "/Users/kawase/Desktop/complex-interaction/python/SFAnalysis/SFAnalysis/code"
sys.path.append(code_path)

# --- 必要な関数をインポート ---
from sfanalysis import analyze_degree_sequences, categorize_networks

# --- CSV のあるディレクトリ ---
deg_dir = "/Users/kawase/Desktop/complex-interaction/python/"

# --- 次数列ファイルの一覧 ---
deg_df = pd.DataFrame({
    'file': ['python_csv_all.csv', 'python_csv_out.csv', 'python_csv_in.csv']
})
deg_df['file'] = deg_df['file'].astype(str)

# --- 次数列の解析 ---
analysis_df = analyze_degree_sequences(deg_dir, deg_df)

# --- スケールフリー性の分類 ---
hyps_df = categorize_networks(analysis_df)

# --- 1枚の図に上段1つ、下段2つ ---
fig = plt.figure(figsize=(14, 10))
gs = gridspec.GridSpec(2, 2, height_ratios=[1, 1])

axes = [
    fig.add_subplot(gs[0, :]),  # 上段は2列分
    fig.add_subplot(gs[1, 0]),  # 下段左
    fig.add_subplot(gs[1, 1]),  # 下段右
]

# --- 描画ループ ---
for ax, (_, row) in zip(axes, analysis_df.iterrows()):
    fname = row['file']

    try:
        alpha = row['fit']['pl']['alpha']
        xmin = row['fit']['pl']['xmin']
    except KeyError:
        print(f"{fname} の fit に 'pl' または 'alpha' が存在しません。")
        continue

    # --- CSV 読み込み ---
    data = pd.read_csv(os.path.join(deg_dir, fname))
    data.columns = ["xvalue", "counts"]

    # 0度はlog軸で描けないので除外（必要なら）
    data = data[data["xvalue"] > 0].copy()

    # --- PMF & CCDF（全データで計算）---
    data = data.sort_values("xvalue").reset_index(drop=True)
    data["prob"] = data["counts"] / data["counts"].sum()
    ccdf = data.copy()
    ccdf["ccdf"] = ccdf["prob"][::-1].cumsum()[::-1]

    # --- プロット（経験CCDFは全範囲表示）---
    ax.scatter(ccdf["xvalue"], ccdf["ccdf"], s=30, color="black", label="Empirical CCDF")

    # --- 理論べき乗CCDF（xmin から引く、再正規化なし）---
    kmin = int(np.ceil(xmin))
    if kmin > ccdf["xvalue"].max():
        print(f"{fname}: xmin({kmin}) がデータ範囲外です。")
        continue

    x = np.arange(kmin, int(ccdf["xvalue"].max()) + 1)

    # 理論近似: P(X ≥ k) ∝ k^(-(alpha - 1))
    y = x**(-(alpha - 1))

    # データの CCDF とスケールを合わせる（xmin の点で揃える）
    emp_at_kmin = ccdf.loc[ccdf["xvalue"] >= kmin, "ccdf"].iloc[0]
    y = y * (emp_at_kmin / y[0])

    ax.plot(x, y, color="red", lw=2,
            label=f"Power-law fit (α={alpha:.2f}, xmin={kmin})")

    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set_xlabel("Degree (log)")
    ax.set_ylabel("CCDF P(X ≥ k) (log)")
    ax.set_title(f"{fname}")
    ax.legend()

plt.tight_layout()

# --- PDF保存 ---
pdf_path = os.path.join(deg_dir, "degree_distributions_ccdf.pdf")
plt.savefig(pdf_path, format='pdf')
print(f"PDFを保存しました: {pdf_path}")

plt.show()
