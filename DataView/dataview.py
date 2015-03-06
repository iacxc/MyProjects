#!/usr/bin/python -O


import time
import pypyodbc

import wx

import resource as R

from Controls import SqlEditor, DataListCtrl

class DataFrame(wx.MDIChildFrame):
    default_connstr = 'DSN=bronto10;UID=seapilot;PWD=seapilot1'
    def __init__(self, parent, title):
        wx.MDIChildFrame.__init__(self, parent, title=title)

        self.initUI()

        self.SetInitialSize()

        self.Bind(wx.EVT_BUTTON, self.OnExecute, id=R.Id.ID_EXECUTE)
        self.Bind(wx.EVT_BUTTON, self.OnExport, id=R.Id.ID_EXPORT)


    def initUI(self):
        font = wx.Font(R.Value.FONTSIZE, 
                       wx.FONTFAMILY_MODERN, 
                       wx.FONTSTYLE_NORMAL, 
                       wx.FONTWEIGHT_NORMAL)

        panel = wx.Panel(self)
        #main sizer
        msizer = wx.BoxSizer(wx.VERTICAL)

        # connection string
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)

        stOdbc = wx.StaticText(panel, label=R.String.CONNSTR_LABEL)
        stOdbc.SetFont(font)

        self.txtOdbc = wx.TextCtrl(panel, value=self.default_connstr)
        self.txtOdbc.SetFont(font)

        hbox1.Add(stOdbc, flag=wx.ALL|wx.ALIGN_CENTER_VERTICAL,
                  border=R.Value.BORDER)
        hbox1.Add(self.txtOdbc, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)

        # query text
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)

        stQuery = wx.StaticText(panel, label=R.String.QUERY_LABEL)
        stQuery.SetFont(font)

        self.txtQuery = SqlEditor(panel, font, value=R.String.DEF_QUERY)

        #buttons
        btnsizer = wx.BoxSizer(wx.VERTICAL)
        btnExecute = wx.Button(panel, R.Id.ID_EXECUTE, label=R.String.EXECUTE)
        btnExecute.SetFont(font)

        btnExport = wx.Button(panel, R.Id.ID_EXPORT, label=R.String.EXPORT)
        btnExport.SetFont(font)

        btnsizer.Add(btnExecute, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(btnExport,  0, wx.ALL, border=R.Value.BORDER)

        hbox2.Add(stQuery, flag=wx.ALL, border=R.Value.BORDER)
        hbox2.Add(self.txtQuery, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)
        hbox2.Add(btnsizer)

        # data list
        self.lstData = DataListCtrl(panel)

        msizer.Add(hbox1, 0, flag=wx.EXPAND|wx.ALL, border=R.Value.BORDER)
        msizer.Add(hbox2, 0, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        msizer.Add(self.lstData, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER*2)

        panel.SetSizer(msizer)

        # status bar
        statusbar = self.CreateStatusBar()
        statusbar.SetFont(font)
        statusbar.SetFieldsCount(2)
        statusbar.SetStatusWidths([-1, 300])


    def OnExecute(self, event):
        connstr = self.txtOdbc.GetValue()
        query = self.txtQuery.GetText()

        start_ts = time.time()
        self.PushStatusText('Connecting...', 0)
        conn = pypyodbc.connect(connstr)

        self.PushStatusText('Querying...', 0)
        cursor = conn.cursor()
        cursor.execute(query)

        now = time.time()
        self.PushStatusText('Done', 0)
        self.PushStatusText('{0} rows got, {1:.0f} seconds elapsed'.format(
                                     cursor.rowcount, now - start_ts), 
                        1) 

        self.lstData.RefreshData([ fd[0] for fd in cursor.description ],
                                  cursor.fetchall(),)

        conn.close() 


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
