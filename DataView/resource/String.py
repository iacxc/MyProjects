#!/usr/bin/python -O

MENU_TOOL = '&Tool'
MENU_DATAVIEW = '&DataView'
MENU_EXIT = 'E&xit'

TITLE_FAILURE = 'Failure'
TITLE_DATAVIEW = 'Data Viewer'

CONNSTR_LABEL = 'Connection String:'
QUERY_LABEL = 'Query Text:'
EXECUTE = 'Execute'
EXPORT = 'Export'

DEF_QUERY = """at epoch latest
select * from metric_topn_cpu
order by gen_utc_ts desc
limit 50
"""
