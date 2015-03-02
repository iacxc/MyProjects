#!/usr/bin/python -O


CONNSTR_LABEL = 'Connection String:'
QUERY_LABEL = 'Query Text:'
EXECUTE = 'Execute'
DEF_QUERY = """at epoch latest
select * from metric_hadoop_job_progress
order by gen_utc_ts desc
limit 5
"""
