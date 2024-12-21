# filter dataset

import os
import json
from datetime import datetime

os.getcwd()


files = os.listdir("../data/RD")


Ns = []
for file in files:
    if not file.endswith("posts.jsonl"):
        continue
    print(file)
    file_path = os.path.join("../data/RD", file)
    with open(file_path, "r") as f:
        data = [json.loads(line) for line in f]
        print(len(data))
        n_start = len(data)
        
        # remove distinguished posts
        filtered_data = []
        for post in data:
            if post["distinguished"] is None:
                filtered_data.append(post)
            else:
                pass
                #print("distinguished:", post["distinguished"],  " - filtering out.")
        print(str(len(filtered_data)), "of", str(n_start), "remaining.")
        # Filtering out deleted
        data = filtered_data
        filtered_data = [] 

        for post in data:
            if post["selftext"] not in {'[deleted]', '[removed]', ''}:
                filtered_data.append(post)
            else:
                pass
                #print("selftext :", post["selftext"],  " - filtering out.")
        print(str(len(filtered_data)), "of", str(n_start), "remaining.")

        # lenghts
        lengths = [len(post["selftext"]) for post in filtered_data]
        data=filtered_data
        filtered_data = []
        for post in data:
            if len(post["selftext"]) < 50:
                pass
                #print("selftext :", post["selftext"],  " - short post.")
            else:
                filtered_data.append(post)
        print(str(len(filtered_data)), "of", str(n_start), "remaining., percent: ", str(len(filtered_data)/n_start))

        # filter out date
        data = filtered_data
        filtered_data = []
        print("time filter:")
        # Create UTC time for the date
        utc_cutoff = datetime(2024, 9, 30, 23, 59, 59).timestamp()
        for post in data:
            if post["created_utc"] < utc_cutoff:
                filtered_data.append(post)
            else:
                #print("created_utc :", post["created_utc"],  " - too young.")
                #print("Day: ", datetime.utcfromtimestamp(post["created_utc"]).strftime('%Y-%m-%d %H:%M:%S'))
                pass

        print(str(len(filtered_data)), "of", str(n_start), "remaining., percent: ", str(len(filtered_data)/n_start))
        Ns.append(len(filtered_data))


        # write to file 
        file_path = file_path.replace("posts.jsonl", "filtered_posts.jsonl")
        print(file_path)
        with open(file_path, "w") as f:
            for post in filtered_data:
                f.write(json.dumps(post) + "\n")

    

for x in Ns:
        print(x)
print("Total: ", sum(Ns))

