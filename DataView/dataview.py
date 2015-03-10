#!/usr/bin/python -O


import time
import wx
import threading

import resource as R
from Controls import SqlEditor, DataListCtrl, ProgressStatusBar
import DBUtil


class DataThread(threading.Thread):
    def __init__(self, conn_str, query_str, window):
        super(DataThread, self).__init__()

        self.__conn_str = conn_str
        self.__query_str = query_str
        self.window = window


    def run(self):

        start_ts = time.time()
        wx.CallAfter(self.window.UpdateStatus, 'Connecting...')

        conn = DBUtil.connect(self.__conn_str)
        wx.CallAfter(self.window.UpdateStatus, 'Querying...')

        desc, rows = DBUtil.fetch_table(conn, self.__query_str)
        wx.CallAfter(self.window.UpdateStatus, 'Done')

        end_ts = time.time()
        wx.CallAfter(self.window.StopQuery, desc, rows, end_ts - start_ts)


class DataFrame(wx.MDIChildFrame):
    def __init__(self, parent, title):
        wx.MDIChildFrame.__init__(self, parent, title=title)

        self.initUI()

        self.Bind(wx.EVT_BUTTON, self.OnExecute, self.btnExecute)
        self.Bind(wx.EVT_BUTTON, self.OnExport, self.btnExport)


    def initUI(self):
        panel = wx.Panel(self)

        self.txtOdbc = wx.TextCtrl(panel, value=R.String.DEF_CONNSTR)
        self.txtQuery = SqlEditor(panel, value=R.String.DEF_QUERY)
        self.btnExecute = wx.Button(panel, label=R.String.EXECUTE)
        self.btnExport = wx.Button(panel, label=R.String.EXPORT)
        self.lstData = DataListCtrl(panel)

        #main sizer
        msizer = wx.BoxSizer(wx.VERTICAL)

        # connection string
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)

        hbox1.Add(wx.StaticText(panel, label=R.String.CONNSTR_LABEL),
                  flag=wx.ALL|wx.ALIGN_CENTER_VERTICAL,
                  border=R.Value.BORDER)
        hbox1.Add(self.txtOdbc, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)

        # query text
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        hbox2.Add(wx.StaticText(panel, label=R.String.QUERY_LABEL),
            flag=wx.ALL, border=R.Value.BORDER)
        hbox2.Add(self.txtQuery, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        #buttons
        btnsizer = wx.BoxSizer(wx.VERTICAL)

        btnsizer.Add(self.btnExecute, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(self.btnExport,  0, wx.ALL, border=R.Value.BORDER)

        hbox2.Add(btnsizer)

        msizer.Add(hbox1, 0, flag=wx.EXPAND|wx.ALL, border=R.Value.BORDER)
        msizer.Add(hbox2, 0, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        msizer.Add(self.lstData, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER*2)

        panel.SetSizer(msizer)

        # status bar
        self.statusbar = ProgressStatusBar(self)
        self.SetStatusBar(self.statusbar)


    def OnExecute(self, event):
        self.StartQuery()
        task = DataThread(self.txtOdbc.GetValue(),
                          self.txtQuery.GetText(),
                          self)
        task.start()


    def OnExport(self, event):
        dlg = wx.FileDialog(self, message="Export",
                            wildcard="*.csv",
                            style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)

        if dlg.ShowModal() ==  wx.ID_OK:
            path = dlg.GetPath()
            try:
                self.lstData.SaveTo(path)
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nExport failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


    def UpdateStatus(self, msg):
        self.PushStatusText(msg, 0)


    def StartQuery(self):
#        self.PushStatusText('Connecting...', 0)
        self.statusbar.StartBusy()
        self.btnExecute.Disable()
        self.btnExport.Disable()


    def StopQuery(self, desc, rows, elapsed):
        self.lstData.RefreshData(desc, rows)
        self.PushStatusText('{0} rows got, {1:.0f} seconds elapsed'.format(
                                     len(rows), elapsed),
                        1)

        self.statusbar.StopBusy()
        self.btnExecute.Enable()
        self.btnExport.Enable()


class MainFrame(wx.MDIParentFrame):
    def __init__(self, parent=None, title="", size=(1200,800)):
        super(MainFrame, self).__init__(parent, title=title, size=size)

        self.__index = 0
        self.initUI()
        self.Center()

        self.Bind(wx.EVT_MENU, self.OnMenuClick)


    def initUI(self):
        menu = wx.Menu()
        menu.Append(R.Id.ID_DATAVIEW, R.String.MENU_DATAVIEW)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, R.String.MENU_EXIT)

        menubar = wx.MenuBar()
        menubar.Append(menu, R.String.MENU_TOOL)

        self.SetMenuBar(menubar)


    def OnMenuClick(self, event):
        event_id = event.GetId()
        if event_id == wx.ID_EXIT:
            self.Close(True)
        elif event_id == R.Id.ID_DATAVIEW:
            frame = DataFrame(self,
                              R.String.TITLE_DATAVIEW + ' {0}'.format(
                                   self.__index))
            self.__index += 1
            frame.Show()


class MainApp(wx.App):

    def OnInit(self):
        self.frame = MainFrame(title=R.String.TITLE_DATAVIEW)
        self.frame.Show()

        return True


if __name__ == '__main__':
    app = MainApp()
    app.MainLoop()
