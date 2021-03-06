#!/usr/bin/python -O

TITLE_MAINWIN = 'Database Viewer'
TITLE_DATAVIEW = 'Data Viewer'
TITLE_FAILURE = 'Failure'

MENU_TOOL = '&Tool'
MENU_DATAVIEW = '&DataView'
MENU_EXIT = 'E&xit'

CONNSTR_LABEL = 'Connection String:'
QUERY_LABEL = 'Query Text:'
DATA_LABEL = 'Query Data'

SAVE = '&Save'
OPEN = '&Open'
RUN = '&Run'
EXPORT = '&Export'

DEF_CONNSTR = 'DSN=bba-5;UID=seapilot;PWD=seapilot1'
DEF_QUERY = """at epoch latest
select * from metric_topn_cpu
order by gen_utc_ts desc
limit 50
"""
