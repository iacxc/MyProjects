#!/usr/bin/python -O

import json

def print_r(cap, obj):
    print cap
    print json.dumps(obj, indent=4)
    print


CONSUMER_KEY = '75zjpyvyxag513'     # This is api_key
CONSUMER_SECRET = 'pzgPKWCGWpDoFXvS'   # This is secret_key

USER_TOKEN = 'daaac107-0a6d-485e-aea3-12f1a0810717'   # This is oauth_token
USER_SECRET = 'b1a750ba-c3e7-49d1-8dc1-2a691cc527af'   # This is oauth_secret

RETURN_URL = ''

from linkedin import linkedin
from linkedin.linkedin import PERMISSIONS, NETWORK_UPDATES

# Define CONSUMER_KEY, CONSUMER_SECRET,
# USER_TOKEN, and USER_SECRET from the credentials
# provided in your LinkedIn application

# Instantiate the developer authentication class

authentication = linkedin.LinkedInDeveloperAuthentication(CONSUMER_KEY, CONSUMER_SECRET,
                                                      USER_TOKEN, USER_SECRET,
                                                      RETURN_URL, PERMISSIONS.enums.values())

# Pass it in to the app...

app = linkedin.LinkedInApplication(authentication)

# Use the app....

print_r ('Profile:', app.get_profile())
print_r ('Connections:', app.get_connections(
                 selectors=['first-name', 'last-name', 'current-share',
                            'public-profile-url']))
#print app.comment_on_update()

#print_r('Company:', app.get_companies(universal_names=['linkedin']))
#print_r('Company:', app.get_company_by_email_domain('apple.com'))
#print_r('Company Products:', app.get_company_products(162479))
#print_r('Company Updates:',
#            app.get_company_updates(162479, params={'count' : 100}))

#print_r ('Job Bookmarks:', app.get_job_bookmarks())

#members = app.get_memberships()
#print_r ('Memberships:', members)

#for v in members['values']:
#    print_r('Group:', app.get_group(v['_key']))
#    print_r('Post:', app.get_posts(v['_key'], params={'count' : 250}))

#print_r ('Network status:', app.get_network_status())

#print_r('Network updates',
#        app.get_network_updates(NETWORK_UPDATES.enums.values(),
#                                params={'count': 250}) )
