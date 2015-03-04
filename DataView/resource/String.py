#!/usr/bin/python -O

TITLE_FAILURE = 'Failure'

CONNSTR_LABEL = 'Connection String:'
QUERY_LABEL = 'Query Text:'
EXECUTE = 'Execute'
EXPORT = 'Export'

DEF_QUERY = """at epoch latest
select * from metric_hadoop_job_progress
order by gen_utc_ts desc
limit 5
"""
