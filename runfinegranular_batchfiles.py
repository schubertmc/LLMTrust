#run_finegranular_categories.py

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


map_response_format = {
        "Communication": response_formats.CommunicationEvaluation,
        "Perceived Incompetence of Medical Management": response_formats.PerceivedIncompetenceEvaluation,
        "Disregard for Patient Concerns": response_formats.DisregardEvaluation,
        "Profit-Driven Medical Management": response_formats.ProfitDrivenEvaluation,
        "Lack of Trust in Medical Procedures": response_formats.LackOfTrustEvaluation,
}
map_finegranular_prompts = {
        "Communication": "categories_post_analysis_communication.j2",
        "Perceived Incompetence of Medical Management": "categories_post_analysis_perceived_incompetence.j2",
        "Disregard for Patient Concerns": "categories_post_analysis_disregard.j2",
        "Profit-Driven Medical Management": "categories_post_analysis_profit_driven.j2",
        "Lack of Trust in Medical Procedures": "categories_post_analysis_medical_procedures.j2",


}



################################################################################
os.getcwd()
prompt_dir = '../code/prompts'
os.chdir("../batchtemp/")
# for subreddit in subreddits 
files = os.listdir("../batchtemp/")
files = [file for file in files if file.count("B_eval_BR_")>0]

basic_data_files = [file for file in os.listdir() if file.count("BasicData")>0]


for idx, file in enumerate(files):
    print(idx, file)

    data = pd.read_csv(file)

    # filter for mistrust in mistrust_trust_NA column
    data = data[data["mistrust_trust_NA"] == "mistrust"]
    data = data[data["mistrust_reason_category"] != "Other"]
    # get the post_id whihc is after the 2. last _ in the custom_id
    data["post_id"] = data["custom_id"].apply(lambda x: x.split("_")[-2])
    sub_id = file[file.find("BatchResponses_")+len("BatchResponses_"):file.find("_filtered_posts")]

    # filter for the files
    # for batch file in files:
    # filter only for mistrust
    # load in also the basic data
    # merge them by the post id, only keep the ones that are in the mistrust data

    basic_data_file = [file for file in basic_data_files if file.count(sub_id)>0][0]
    basic_data = pd.read_csv(basic_data_file)
    
    print(data.shape)
    print(basic_data.shape)
    data = pd.merge(data, basic_data, on="post_id", how="inner")
    print(data.shape)


    for idx, row in data.iterrows():
        print(idx, row["mistrust_reason_category"], " - ", row["object_of_mistrust_category"])
        post_id = row["post_id"]
        title = row["title"]
        text = row["text"]
        object_of_mistrust = row["object_of_mistrust"]
        mistrust_reason = row["mistrust_reason"]
        mistrust_reason_category = row["mistrust_reason_category"]
        object_of_mistrust_category = row["object_of_mistrust_category"]


        mapped_response_format = map_response_format[mistrust_reason_category]
        mapped_prompt_file = map_finegranular_prompts[mistrust_reason_category]

        # finegranular analysis
        input_prompt = eng.createPromptWithTemplate(prompt_dir=prompt_dir,
                                                    TEMPLATEFILE="finegranular_categories_post_analysis_trust_v2.j2", 
                                                    params = {"title": title, "text": text, 
                                                            "finegranlular_categories_filename": mapped_prompt_file,
                                                                "object_of_mistrust": object_of_mistrust,
                                                                "mistrust_reason": mistrust_reason,
                                                                "object_of_mistrust_category": object_of_mistrust_category,
                                                                "mistrust_reason_category": mistrust_reason_category
                                                            })
        
        #output = eng.promptModel(input_prompt, response_format=mapped_response_format)

        custom_id = f"Fine{file}_{post_id}_{idx}"
        request_line = eng.createBatchLine(
            custom_id=custom_id,
            input_prompt=input_prompt,
            response_format=mapped_response_format
        )


        #Write each line immediately to minimize memory usage
        output_file = f"../batchtemp_finegranular/FineBRequests_{file}.jsonl"
        with open(output_file, "a") as f:
            f.write(json.dumps(request_line) + "\n")




### Batchrequests 
os.chdir("../batchtemp_finegranular/")
files = os.listdir(".")

for file in files: 
    if not file.startswith("FineBRequests"):
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
            "description": "FineReqs" + file
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
    if batch.metadata["description"].startswith("FineReqs"): 
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
    if not file_path.startswith("FineReqsFineBRequests"):
        continue
    print(file_path)

    with open(file_path, "r") as f:
        data = f.readlines()

    version = "vBatch_Fine"
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
    if file.startswith("B_eval_FineReqsFineBRequests"):
        print(file)
        df = pd.read_csv(f"../batchtemp_finegranular/{file}")
        dfs.append(df)

pd.concat(dfs).to_csv(f"../batchtemp/FINE_B_eval_all_{version}.csv", index=False)