import pandas as pd
import sys
import os

# code ディレクトリをパスに追加
code_path = "/Users/kawase/Desktop/complex-interaction/python/SFAnalysis/SFAnalysis/code"
sys.path.append(code_path)

# 必要な関数をインポート
from sfanalysis import analyze_degree_sequences, categorize_networks

# CSV ファイルのあるディレクトリ
deg_dir = "/Users/kawase/Desktop/complex-interaction/python/"

# 次数列ファイルの一覧
deg_df = pd.DataFrame({
    'file': ['python_csv_all.csv', 'python_csv_out.csv', 'python_csv_in.csv']
})

deg_df['file'] = deg_df['file'].astype(str)

# 次数列の解析
analysis_df = analyze_degree_sequences(deg_dir, deg_df)

# 上段：解析結果（category 列は削除して fit のみ表示）
analysis_display = analysis_df.drop(columns=['category'], errors='ignore')
print("分析結果 (fit のみ):")
print(analysis_display)

# スケールフリー性の分類
hyps_df = categorize_networks(analysis_df)
print("\nスケールフリー性分類:")
print(hyps_df[['file', 'category']])

# --- 強弱を抽出して別の列を作る ---
def extract_strength(cat):
    if cat in ["Strongest", "Strong"]:
        return "Strong"
    elif cat in ["Weak", "Weakest"]:
        return "Weak"
    elif cat == "Super-Weak":
        return "Super-Weak"
    else:
        return "Not Scale-Free"

hyps_df["strength"] = hyps_df["category"].apply(extract_strength)

print("\n強弱まとめ:")
print(hyps_df[['file', 'category', 'strength']])
