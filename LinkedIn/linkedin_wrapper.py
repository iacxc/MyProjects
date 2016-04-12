
from linkedin import linkedin, exceptions

api_key    = '75zjpyvyxag513'
secret_key = 'pzgPKWCGWpDoFXvS'

user_token  = 'daaac107-0a6d-485e-aea3-12f1a0810717'
user_secret = 'b1a750ba-c3e7-49d1-8dc1-2a691cc527af'


BASIC_PROFILE_FIELDS = ('id', 'first-name', 'last-name', 'maiden-name',
        'formatted-name', 'headline', 'location',
        'industry', 'distance',
        'current-share', 'num-connections', 'num-connections-capped',
        'summary', 'specialties', 'positions', 'picture-url',
        'site-standard-profile-request',
        'api-standard-profile-request',
        'public-profile-url')
FULL_PROFILE_FIELDS = ('last-modified-timestamp',
        'proposal-comments', 'associations', 'interests',
        'publications', 'patents', 'languages', 'skills',
        'certifications', 'educations', 'courses',
        'volunteer',
        'three-current-positions ', 'three-past-positions ',
        'num-recommenders ', 'recommendations-received ',
        'mfeed-rss-url ', 'following ', 'job-bookmarks ',
        'suggestions ', 'date-of-birth ', 'member-url-resources ',
        'related-profile-views ', 'honors-awards ')
CONTACT_INFO_FIELDS = ('phone-numbers', 'bound-account-types',
        'im-accounts', 'main-address', 'primary-twitter-account')

PROFILE_FIELDS = BASIC_PROFILE_FIELDS + FULL_PROFILE_FIELDS \
                + CONTACT_INFO_FIELDS

PROFILE_FIELDS_2 = ('id', 'first-name', 'last-name', 'maiden-name',
              'formatted-name', 'headline', 'location',
              'industry', 'num-connections',
              'summary', 'specialties', 'positions', 'picture-url',
              'site-standard-profile-request',
              'api-standard-profile-request',
              'public-profile-url', 'twitter-accounts',
              'last-modified-timestamp', 'educations')
SHARE_FIELDS = ('id', 'current-share')


def getLinkedInApp():
    auth= linkedin.LinkedInDeveloperAuthentication(api_key, secret_key,
                          user_token, user_secret,
                          '', linkedin.PERMISSIONS.enums.values())

    return linkedin.LinkedInApplication(auth)


def get_profile(uid):
    try:
        if __debug__:
            for field in PROFILE_FIELDS:
                print field, app.get_profile(uid, selectors=[field])

        return app.get_profile(uid, selectors=PROFILE_FIELDS_2)

    except exceptions.LinkedInError, e:
        return { 'id' : uid, 'error' : e.message }

    except exceptions.LinkedInHTTPError, e:
        return { 'url' : uid, 'error' : e.message }


def get_profile_by_url(url):
    try:
        return app.get_profile(member_url=url, selectors=PROFILE_FIELDS_2)

    except exceptions.LinkedInError, e:
        return { 'url' : url, 'error' : e.message }

    except exceptions.LinkedInHTTPError, e:
        return { 'url' : url, 'error' : e.message }


def get_people_shares_by_url(url):
    try:
        profile = app.get_profile(member_url=url, selectors=SHARE_FIELDS)
        if profile.get('currentShare'):
            return { 'id' : profile['id'],
                     'timestamp' : profile['currentShare']['timestamp'],
                     'currentShare' : profile['currentShare'] }
        else:
            return { 'url': url, 'error' : 'No shares' }

    except exceptions.LinkedInError, e:
        return { 'url' : url, 'error' : e.message }

    except exceptions.LinkedInHTTPError, e:
        return { 'url' : url, 'error' : e.message }


def get_people_updates(uid):
    try:
        updates = app.get_network_updates(
                          linkedin.NETWORK_UPDATES.enums.values(),
                          member_id=uid,
                          params={'count' : 250})
        if updates['_total'] > 0:
            return updates['values']
        else:
            return [{'id': uid, 'error' : 'No updates'}]

    except exceptions.LinkedInError, e:
        return [{'id': uid, 'error' : e.message}]

    except exceptions.LinkedInHTTPError, e:
        return { 'url' : uid, 'error' : e.message }


def get_people_updates_by_url(url):
    try:
        profile = app.get_profile(member_url=url, selectors=['id'])
        return get_people_updates(profile['id'])

    except exceptions.LinkedInError, e:
        return [{'url': url, 'error' : e.message}]

    except exceptions.LinkedInHTTPError, e:
        return { 'url' : url, 'error' : e.message }



def get_company(cid):
    sels = ['id', 'name', 'universal-name', 'email-domains',
             'company-type', 'ticker', 'website-url',
             'industries', 'status', 'logo-url',
             'square-logo-url', 'blog-rss-url', 'twitter-id',
             'employee-count-range', 'specialties', 'locations',
             'description', 'stock-exchange',
             'founded-year', 'end-year',
             'num-followers' ]

    try:
        company = app.get_companies(company_ids=[cid],
                              selectors=sels,
                              params={'count' : 250})
        return company['values']

    except exceptions.LinkedInError, e:
        return [{ 'id' : cid, 'error' : e.message }]


def get_company_by_name(name):
    sels = ['id', 'name', 'universal-name', 'email-domains',
             'company-type', 'ticker', 'website-url',
             'industries', 'status', 'logo-url',
             'square-logo-url', 'blog-rss-url', 'twitter-id',
             'employee-count-range', 'specialties', 'locations',
             'description', 'stock-exchange',
             'founded-year', 'end-year',
             'num-followers' ]

    try:
        profile_c = app.get_companies(universal_names=[name], selectors=['id'])

        if profile_c['_total'] > 0:
            return get_company(profile_c['values'][0]['id'])
        else:
            return [{ 'name'  : name,
                      'error' : 'Request Error: Did not find company '}]

    except exceptions.LinkedInError, e:
        return [{ 'name'  : name, 'error' : e.message }]


def get_companies_by_domain(domain):
    try:
        companies = app.get_company_by_email_domain(domain,
                                           params={'count' : 250})


        company_ids =[ c['id'] for c in companies['values'] ]

        return [ get_company(cid)[0] for cid in company_ids ]

    except exceptions.LinkedInError, e:
        return [{ 'domain' : domain, 'error' : e.message}]


def get_company_updates(cid):
    try:
        updates = app.get_company_updates(cid, params={'count' : 250})

        if updates['_total'] > 0:
            return updates['values']
        else:
            return [{ 'id' : cid, 'error' : 'No updates'}]

    except exceptions.LinkedInError, e:
        return [{'error'  : e.message}]

# global variable
app = getLinkedInApp()
