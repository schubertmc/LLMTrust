#define_groups.py

import pandas as pd
import numpy as np
import os
import ast
os.getcwd() 

version = ""
path = f"../data/evaluation_{version}/evaluation_all_bound_{version}.csv"
saving_path = f"../data/evaluation_{version}/"
data = pd.read_csv(path)

# extract lists from tone column
data['tone'] = data['tone'].apply(lambda x: ast.literal_eval(x))
tones = [item.lower() for sublist in data['tone'] for item in sublist]

# counts
tone_counts = pd.Series(tones).value_counts()
# write csv
tone_counts.to_csv(os.path.join(saving_path, "tone_counts.csv"))
# Select the top 10 tones
top_tones = tone_counts.head(10).index.tolist()
for tone in top_tones:
    print(tone)



