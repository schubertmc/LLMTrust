import sys
import pandas as pd
import re
import orjson
sys.path.append("/Users/fabianvonbubnoff/Library/Mobile Documents/com~apple~CloudDocs/redonc/arctic_shift/scripts")
from fileStreams import getFileJsonStream
from utils import FileProgressLog

subreddit_data = r"/Users/fabianvonbubnoff/Library/Mobile Documents/com~apple~CloudDocs/redonc/raw subreddit metadata/subreddits_2024-01.zst"
subreddit_output = r"/Users/fabianvonbubnoff/Library/Mobile Documents/com~apple~CloudDocs/redonc/Completed tasks/Extract cancer-related subreddits/cancer_subs.csv"
cancer_keywords = [
    "cancer", "carcinoma", "melanoma", "sarcoma", "lymphoma", 
    "oncology", "tumor", "neoplasm", "metastasis", "leukemia",
    "glioma", "mesothelioma", "adenoma", "myeloma", "colorectal"
]

subs = {}
print(f"Processing file {subreddit_data}")
with open(subreddit_data, "rb") as f:
    jsonStream = getFileJsonStream(subreddit_data, f)
    if jsonStream is None:
        print(f"Skipping unknown file {subreddit_data}")
    progressLog = FileProgressLog(subreddit_data, f)
    for row in jsonStream:
        progressLog.onRow()
        for cancertype in cancer_keywords:
            if re.match(rf"r/[a-z0-9_]*{cancertype}[a-z0-9_]*", row["display_name_prefixed"], re.IGNORECASE):
                if (row["_meta"]["num_posts"] > 0):
                    subs[row["display_name_prefixed"]] = row["_meta"]["num_posts"]
    progressLog.logProgress("\n")
subs_df = pd.DataFrame(subs.items())
subs_df.columns = ["subreddit", "post number"]
subs_df = subs_df.sort_values(by="post number", ascending=False)
subs_df.to_csv(subreddit_output, index=False, header=False)