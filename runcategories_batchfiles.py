#runopen.py
# 

import os
#import config
import dotenv # type: ignore
#import jinja2
import json 
import pandas as pd
import random

import response_formats
import engine as eng
from datetime import datetime


from openai import OpenAI # type: ignore
dotenv.load_dotenv(".env")

client = OpenAI()



################################################################################
os.getcwd()
prompt_dir = '../code/prompts'

files = os.listdir("../data/RD")

# def read_posts(file_path):
#     with open(file_path, "r") as f:
#         for line in f:
#             yield json.loads(line)


for file in files:
    if not file.endswith("filtered_posts.jsonl"):
        continue

    print(file)
    file_path = os.path.join("../data/RD", file)
    
    # read in data 
    basic_data = []
    with open(file_path, "r") as f:
        posts = [json.loads(line) for line in f]

    for idx, post_obj in enumerate(posts):
        print(idx, "of", len(posts), "posts - ", file)
    
        subreddit = post_obj["subreddit"]
        post_id = post_obj["id"]
        title = post_obj.get("title", "")
        text = post_obj.get("selftext", "")

        input_prompt = eng.createPromptWithTemplate(
            prompt_dir=prompt_dir,
            TEMPLATEFILE="categories_post_analysis_trust_v2.j2",
            params={"title": title, "text": text}
        )

        custom_id = f"{file}_{post_id}_{idx}"
        request_line = eng.createBatchLine(
            custom_id=custom_id,
            input_prompt=input_prompt,
            response_format=response_formats.FinalPostTrustEvaluation
        )

        basic_data.append({"subreddit":subreddit,
                           "post_id": post_id,
                           "title": title,
                           "text": text,
                           "custom_id": custom_id
                           })
        
        #Write each line immediately to minimize memory usage
        output_file = f"../batchtemp/BatchRequests_{file}.jsonl"
        with open(output_file, "a") as f:
           f.write(json.dumps(request_line) + "\n")

    pd.DataFrame(basic_data).to_csv(f"../batchtemp/BasicData_{file}.csv", index=False)




os.chdir("../batchtemp")

### Batchrequestins 
files = os.listdir(".")
for file in files: 
    if not file.startswith("BatchRequests"):
        continue
    print(file)

    batch_input_file = client.files.create(
        file=open(file, "rb"),
        purpose="batch"
    )
    batch_input_file_id = batch_input_file.id

    print(batch_input_file_id)
    bx = client.batches.create(
        input_file_id=batch_input_file_id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
        metadata={
            "description": "BR_V3" + file
        }
    )
    print(bx.id)
    status = client.batches.retrieve(bx.id)
    print(status)
    print(status.output_file_id)




##############
# Check status of batches: 

batches = client.batches.list(limit=10)
for batch in batches:
    print(batch.id, batch.status, batch.metadata)




### Write Data
for batch in client.batches.list(limit=10):
    if batch.metadata["description"].startswith("BR_V3"): 
        print(batch.id, batch.status, batch.metadata)
        file = batch.metadata["description"].replace("BatchRequests", "BatchResponses")
  
        status = client.batches.retrieve(batch.id)
        print(status)
        print(status.output_file_id)
        # Write output file if done 
        # if done: 
        output_file = status.output_file_id
        file_response = client.files.content(output_file)
        txt = file_response.text
        # write jsonl 
        file_path = f"{file}raw.jsonl"
        with open(file_path, "w") as f:
            f.write(txt)




############## READ Data and write structured ############################
# read jsonl file

files = os.listdir(".")

for file_path in files:
    if not file_path.startswith("BR_V3BatchResponses"):
        continue
    print(file_path)

    with open(file_path, "r") as f:
        data = f.readlines()

    version = "vBatch_1"
    responses = []
    for idx, response in enumerate(data):
        print(idx)
        response_dict = eng.extractDictFromBatchResponse(response)    
        responses.append(response_dict)

    df = pd.DataFrame(responses)
    df_eval = pd.json_normalize(df["evaluation"])
    df_combined = pd.concat([df, df_eval], axis=1)
    df_combined.to_csv(f"B_eval_{file_path}_{version}.csv", index=False)


# combine all 
files = os.listdir()
dfs = []
for file in files:
    if file.startswith("B_eval"):
        print(file)
        df = pd.read_csv(f"../batchtemp/{file}")
        dfs.append(df)

pd.concat(dfs).to_csv(f"../batchtemp/B_eval_all_{version}.csv", index=False)
