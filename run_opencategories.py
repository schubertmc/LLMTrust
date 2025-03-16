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


from openai import OpenAI # type: ignore
dotenv.load_dotenv(".env")

client = OpenAI()


################################################################################
os.getcwd()
prompt_dir = '../code/prompts'


version = ""
n_samples_per_subreddit = 100
files = os.listdir("../data/RD")
# drop subredditcancer


for file in files:
    if not file.endswith("posts.jsonl"):
        continue
    print(file)

    file_path = os.path.join("../data/RD", file)
    with open(file_path, "r") as f:
        data = [json.loads(line) for line in f]

    len(data)

    # sample 100 samples randomly
    samples = [data[i] for i in random.sample(list(range(0, len(data))), n_samples_per_subreddit)]
    #samples = [data[i] for i in range(0, n_samples_per_subreddit)]

    list_of_evaluations = []
    for idx, post_obj in enumerate(samples):
        print(idx, "of", len(samples), "posts")

        # input: post_obj
        subreddit = post_obj["subreddit"]
        post_id = post_obj["id"]
        title = post_obj.get("title", "")   # else none
        text = post_obj.get("selftext", "")

        input_prompt = eng.createPromptWithTemplate(prompt_dir=prompt_dir,
                                                TEMPLATEFILE="open_post_analysis_trust.j2", 
                                                params = {"title": title, "text": text})


        output = eng.promptModel(input_prompt, response_format=response_formats.OpenPostTrustEvaluation)
        output_dict = output.choices[0].message.parsed.dict()
        print(output_dict)
        post_analysis = {"subreddit": subreddit, 
                        "post_id": post_id,
                        "title": title,
                        "text": text}

        post_analysis.update({"evaluation": output_dict})

        eng.saveEvaluationJSON(post_analysis, output_dir=os.path.join(f"../data/evaluation_{version}/",subreddit))

        # write to data folder
        list_of_evaluations.append(post_analysis)

    # create a dataframe from the list of evaluations
    df = pd.DataFrame(list_of_evaluations)
    df_eval = pd.json_normalize(df["evaluation"])
    df_combined = pd.concat([df, df_eval], axis=1)
    df_combined.to_csv(f"../data/evaluation_{version}/evaluation_all_{version}{subreddit}.csv", index=False)



# bind all csvs from the evaluation folder 
import os
import pandas as pd
import glob
# list files 
files = glob.glob(f"../data/evaluation_{version}/*.csv")

dfs = []
for file in files:
    print(file)
    df = pd.read_csv(file)
    dfs.append(df)

bound = pd.concat(dfs)
bound.to_csv(f"../data/evaluation_{version}/evaluation_all_bound_{version}.csv", index=False)
