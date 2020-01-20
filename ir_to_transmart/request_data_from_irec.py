#!/usr/bin/env python3

import requests 
from typing import List
import json

def download_from_ireceptor(url:str, filename:str):
    """[summary]
    
    Arguments:
        url {str} -- [url to download datafrom ireceptor]
        filename {str} -- [file name]
    """
    response = requests.get(url)
    print("response")
    with open(filename, 'w') as outfile:
        json.dump(response.json(), outfile)
        print(response.json())
    return filename

