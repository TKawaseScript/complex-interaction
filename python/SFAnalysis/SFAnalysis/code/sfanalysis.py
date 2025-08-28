# sfanalysis.py (Python 3 対応版)
import os
import pandas as pd
from importfiles import readdata
from fit import fit_distribution
import integration_constants as ic

def analyze_degree_sequences(deg_dir, file_list):
    results = []

    for fn in file_list['file']:  # DataFrame の 'file' 列を利用
        fp = os.path.join(deg_dir, fn)
        if not os.path.exists(fp):
            print(f"Warning: file {fp} not found")
            continue

        x = readdata(fp)
        fit_result = fit_distribution(x)

        results.append({
            'file': fn,
            'fit': fit_result
        })

    return pd.DataFrame(results)


def categorize_networks(df):
    categories = []
    for _, row in df.iterrows():
        gamma = row['fit'].get('gamma', None)
        try:
            gamma_val = float(gamma)
            if 2 < gamma_val < 3:
                categories.append('scale-free')
            else:
                categories.append('non-scale-free')
        except:
            categories.append('unknown')

    df['category'] = categories
    return df
