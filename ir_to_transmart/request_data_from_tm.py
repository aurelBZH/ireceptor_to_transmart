#!/usr/bin/env python3

import requests, json
import subprocess
import sys
from config_file import config_data
import typing
from loguru import logger

def request_access_token(authorize_url : str ,callback_uri: str, token_url: str, client_id : str, client_secret : str ):
    """[summary]
    
    Arguments:
        authorize_url {str} -- [description]
        callback_uri {str} -- [description]
        token_url {str} -- [description]
        client_id {str} -- [description]
        client_secret {str} -- [description]
    
    Returns:
        [type] -- [description]
    """
    authorization_redirect_url = authorize_url + '?response_type=code&client_id=' + client_id +'&client_secret='+ client_secret + '&redirect_uri=' + callback_uri
    authorization_redirect_url = authorize_url + '?response_type=code&client_id=' + client_id +'&client_secret='+ client_secret + '&redirect_uri=' + callback_uri
    logger.info("go to the following url on the browser and enter the code from the returned url: ")
    logger.info ("---  " + authorization_redirect_url + "  ---")
    authorization_code = input('code: ')
    data = {'grant_type': 'authorization_code', 'code': authorization_code, 'redirect_uri': callback_uri}
    #step A - simulate a request from a browser on the authorize_url - will return an authorization code after the user is
    # prompted for credentials.

    # step I, J - turn the authorization code into a access token, etc
    logger.info ("requesting access token")

    access_token_response = requests.post(token_url, data=data, verify=False, allow_redirects=True, auth=(config_data.client_id, config_data.client_secret))
    logger.debug ("content"+ str(access_token_response.content))
    logger.debug ('body: ' + access_token_response.text)
    # we can now use the access_token as much as we want to access protected resources.
    tokens = json.loads(access_token_response.text)
    access_token = tokens['access_token']
    refresh_token = tokens['refresh_token']
    expires_in = type(tokens['expires_in'])
    logger.debug ("access token: " + access_token)

    return access_token, refresh_token, expires_in

def request_data(test_api_url : str):
    """[summary]
    
    Arguments:
        test_api_url {str} -- [description]
    
    Returns:
        [type] -- [description]
    """
    if  not config_data.access_token or not config_data.refresh_token or not config_data.expires_in:
        
        access_token, refresh_token, expires_in = request_access_token(config_data.authorize_url ,config_data.callback_uri, config_data.token_url, config_data.client_id , config_data.client_secret )
        config_data.access_token = access_token
        config_data.refresh_token = refresh_token
        config_data.expires_in = expires_in
        api_call_headers = {'Authorization': 'Bearer ' + access_token}
    else:
        api_call_headers = {'Authorization': 'Bearer ' + config_data.access_token}
    
    api_call_response = requests.get(test_api_url, headers=api_call_headers, verify=False)
    return api_call_response, api_call_response.text


def refresh_expired_access_token(refresh_token:str):
    pass
    return access_token, refresh_token, expires_in 



if __name__ == "__main__":
    print(request_data("https://transmart.i3lab.cloud:8445/transmart/studies"))
    print(request_data("https://transmart.i3lab.cloud:8445/transmart/studies/MSY012/subjects"))