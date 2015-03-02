#!/usr/bin/python -O

import wx

class Value(object):
    FONTSIZE = 13
    BORDER = 5
    def __init__(self):
        pass


class String(object):
    CONNSTR_LABEL = 'Connection String:'
    QUERY_LABEL = 'Query Text:'
    EXECUTE = 'Execute'
    DEF_QUERY = """at epoch latest
select * from metric_hadoop_job_progress
order by gen_utc_ts desc
limit 5
"""
    def __init__(self):
        pass


class Id(object):
    ID_EXECUTE = wx.NewId()
    def __init__(self):
        pass
