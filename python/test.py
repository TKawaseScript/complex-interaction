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
