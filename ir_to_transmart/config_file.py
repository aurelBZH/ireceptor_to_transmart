#!/usr/bin/env python3

import datetime
import shelve
import os
from loguru import logger

class config_data:
    def __init__ (self, db):
        self.domain_name = "https://transmart.i3lab.cloud:8444/transmart/"
        self.authorize_url = f"{self.domain_name}oauth/authorize"
        self.token_url = f"{self.domain_name}oauth/token"

        #callback url specified when the application was defined
        self.callback_uri = f"{self.domain_name}oauth/verify"

        self.client_id = 'api-client'
        self.client_secret = 'api-client'
        if os.path.isfile(db):
            config_db = shelve.open(db)
     
            try:
                logger.debug("ihihihi")
                self.access_token = config_db["access_token"]
                self.refresh_token = config_db["refresh_token"]
                self.expires_in = config_db["expires_in"]
            except KeyError as identifier:
                logger.debug("ohohoh")
                self.access_token = "c9667330-4d33-4510-9b7f-814d9c3c3fab"
                self.refresh_token = "2925861b-98fd-4f21-9adc-41060fd0ac11"
                self.expires_in = datetime.datetime(2019,11,26,22,36,57,696187)              
        else:
            self.access_token = "c9667330-4d33-4510-9b7f-814d9c3c3fab"
            self.refresh_token = "2925861b-98fd-4f21-9adc-41060fd0ac11"
            self.expires_in = datetime.datetime(2019,11,26,22,36,57,696187)

    def store(self, name, value):
        config_db= shelve.open("config.db")
        config_db[name]=value
        config_db.close()

