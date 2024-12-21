#functions
import os
import config
import dotenv # type: ignore
import jinja2
import json 

from openai import OpenAI # type: ignore
from openai.lib._pydantic import to_strict_json_schema # type: ignore
from openai.lib._parsing._completions import type_to_response_format_param # type: ignore

dotenv.load_dotenv(".env")

client = OpenAI()




def clean_string(string_input):
    first_bracket_index = string_input.find('{')
    last_bracket_index = string_input.rfind('}')
    return string_input[first_bracket_index:last_bracket_index+1]

def createPromptWithTemplate(prompt_dir,TEMPLATEFILE, params):
    templateLoader = jinja2.FileSystemLoader(searchpath=prompt_dir)
    templateEnv = jinja2.Environment(loader=templateLoader)
    template = templateEnv.get_template(TEMPLATEFILE)
    outputText = template.render(**params)
    return outputText

def promptModel(input_prompt, response_format, model=config.modelID):
    output = client.beta.chat.completions.parse(
        model=config.modelID,
        messages=[
        {
            "role": "user",
            "content": input_prompt            
        }
        ],
        response_format=response_format
    )
    return output


def saveEvaluationJSON(post_analysis, output_dir="../data/evaluation"):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    output_file = f"postanalysis_{post_analysis['subreddit']}_{post_analysis['post_id']}.json"
    output_path = os.path.join(output_dir, output_file)
    with open(output_path, "w") as f:
        json.dump(post_analysis, f)





def createBatchLine(custom_id, input_prompt, response_format, model="gpt-4o-mini"):
    
    batch_line = {"custom_id": custom_id, 
              "method": "POST", 
              "url": "/v1/chat/completions", 
              "body": {"model": model, 
                       "messages": [
        {
            "role": "user",
            "content": input_prompt            
        }, 
    
        ], 
        "response_format": type_to_response_format_param(response_format)
    }}
    return batch_line
    
def extractDictFromBatchResponse(batch_response):
    output = json.loads(batch_response)
    custom_id = output["custom_id"]
    extracted= json.loads(output["response"]["body"]["choices"][0]["message"]["content"])
    output_dict = {"custom_id": custom_id, "evaluation": extracted}
    return output_dict
