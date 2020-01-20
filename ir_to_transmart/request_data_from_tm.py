#!/usr/bin/env python3

import requests, json
import subprocess
import sys
from config_file import config_data
import typing
from loguru import logger
import datetime

data_config = config_data("config.db")

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

    access_token_response = requests.post(token_url, data=data, verify=False, allow_redirects=True, auth=(data_config.client_id, data_config.client_secret))
    logger.debug ("content "+ str(access_token_response.content))
    logger.debug ('body: ' + access_token_response.text)
    # we can now use the access_token as much as we want to access protected resources.
    tokens = json.loads(access_token_response.text)
    access_token = tokens['access_token']
    refresh_token = tokens['refresh_token']
    logger.debug(refresh_token)
    expires_in = datetime.datetime.now() + datetime.timedelta(seconds=tokens['expires_in']) 
    logger.debug(expires_in)
    logger.debug ("access token: " + access_token)

    return access_token, refresh_token, expires_in

def request_data(api_url : str):
    """[summary]
    
    Arguments:
        api_url {str} -- the url the data the user want to download  
    
    Returns:
        [type] -- a file containing the result of the request done by the user 
    """

    if  not data_config.access_token or not data_config.refresh_token or not data_config.expires_in :
        if (data_config.expires_in - datetime.datetime).seconds < 60:
            data_config.access_token, data_config.refresh_token,data_config.expires_in = refresh_expired_access_token(data_config.refresh_token,data_config.token_url, data_config.callback_uri)
      
        access_token, refresh_token, expires_in = request_access_token(data_config.authorize_url ,data_config.callback_uri, data_config.token_url, data_config.client_id , data_config.client_secret )
        logger.debug("bing")
        data_config.access_token = access_token
        data_config.refresh_token = refresh_token
        data_config.expires_in = expires_in
        api_call_headers = {'Authorization': 'Bearer ' + access_token}
    else:
        logger.debug("bong")
        api_call_headers = {'Authorization': 'Bearer ' + data_config.access_token}
    try:
        logger.debug("bang")
        api_call_response = requests.get(data_config.domain_name + api_url, headers=api_call_headers, verify=False)
        if 'error' in api_call_response.json() and api_call_response.json()["error"]=="invalid_token":
            access_token, refresh_token, expires_in = request_access_token(data_config.authorize_url ,data_config.callback_uri, data_config.token_url, data_config.client_id , data_config.client_secret )

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
    access_token_response = requests.post(token_url, data=data, verify=False, allow_redirects=True, auth=(data_config.client_id, data_config.client_secret))
    tokens = json.loads(access_token_response.text)
    print(tokens)
    access_token = tokens['access_token']
    refresh_token = tokens['refresh_token']
    expires_in = tokens['expires_in']
    return access_token, refresh_token, expires_in 


def upload_data_to_transmart():
    pass


if __name__ == "__main__":
    # request_access_token(data_config.authorize_url ,data_config.callback_uri, data_config.token_url, data_config.client_id , data_config.client_secret)
    print(request_data("studies"))
    # print(request_data("studies/GSE22138/subjects"))
    # print(refresh_expired_access_token("0a07d7a1-3c9b-4545-9142-999e32d7a041",data_config.token_url, config.callback_uri))