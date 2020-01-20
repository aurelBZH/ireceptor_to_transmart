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
        authorize_url {str} -- url for authorisation
        callback_uri {str} -- url for calback after authorisation
        token_url {str} -- url for token download
        client_id {str} -- id of the client
        client_secret {str} -- password for the client
    
    Returns:
        [type] -- tokens and expiration date
    """
    authorization_redirect_url = f'{authorize_url}?response_type=code&client_id={client_id}&client_secret={client_secret}&redirect_uri={callback_uri}'
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
    logger.debug(refresh_token)
    expires_in = tokens['expires_in']
    logger.debug ("access token: " + access_token)

    return access_token, refresh_token, expires_in

def request_data(api_url : str):
    """[summary]
    
    Arguments:
        api_url {str} -- the url the data the user want to download  
    
    Returns:
        [type] -- [description]
    """
    if  not config_data.access_token or not config_data.refresh_token :
        access_token, refresh_token, expires_in = request_access_token(config_data.authorize_url ,config_data.callback_uri, config_data.token_url, config_data.client_id , config_data.client_secret )
        config_data.access_token = access_token
        config_data.refresh_token = refresh_token
        config_data.expires_in = expires_in
        api_call_headers = {'Authorization': 'Bearer ' + access_token}
    else:
        api_call_headers = {'Authorization': 'Bearer ' + config_data.access_token}
    try:
        api_call_response = requests.get(config_data.domain_name + api_url, headers=api_call_headers, verify=False)
        if 'error' in api_call_response.json() and api_call_response.json()["error"]=="invalid_token":
            access_token, refresh_token, expires_in = request_access_token(config_data.authorize_url ,config_data.callback_uri, config_data.token_url, config_data.client_id , config_data.client_secret )

    except requests.exceptions.RequestException as e:
        logger.exception(e)

    logger.debug(api_call_response)
    logger.debug(type(api_call_response.json()))
    file_name = "_".join(api_url.split("/"))
    file_url = f"download/{file_name}"
    with open(file_url, 'w+') as outfile:
        json.dump(api_call_response.json(),outfile)
    return api_call_response, outfile


def refresh_expired_access_token(refresh_token:str, token_url:str, callback_uri: str):
    """refresh access token
    
    Arguments:
        refresh_token {str} -- refresh token
        token_url {str} -- url to refresh token
        callback_uri {str} -- callback uri
    
    Returns:
        [type] -- return new tokens
    """

    data= {'grant_type': 'refresh_token',
    'code': refresh_token,
    'redirect_uri': callback_uri}
    access_token_response = requests.post(token_url, data=data, verify=False, allow_redirects=True, auth=(config_data.client_id, config_data.client_secret))
    tokens = json.loads(access_token_response.text)
    print(tokens)
    access_token = tokens['access_token']
    refresh_token = tokens['refresh_token']
    expires_in = tokens['expires_in']
    return access_token, refresh_token, expires_in 



if __name__ == "__main__":
    # print(request_data("https://transmart.i3lab.cloud:8445/transmart/studies"))
    # print(request_data("https://transmart.i3lab.cloud:8445/transmart/studies/MSY012/subjects"))
    print(refresh_expired_access_token("0a07d7a1-3c9b-4545-9142-999e32d7a041",config_data.token_url))