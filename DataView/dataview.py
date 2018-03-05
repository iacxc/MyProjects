#!/usr/bin/python -O


import threading
import time
import wx

from Controls import SqlEditor, DataGrid, ProgressStatusBar
import DBUtil
import resource as R


# utilities
def CreateButton(parent, label, handler):
    button = wx.Button(parent, label=label)
    button.Bind(wx.EVT_BUTTON, handler)
    return button


class DataThread(threading.Thread):
    def __init__(self, conn_str, query_str, window):
        super(DataThread, self).__init__()

        self.__conn_str = conn_str
        self.__query_str = query_str
        self.window = window


    def run(self):

        start_ts = time.time()
        wx.CallAfter(self.window.UpdateStatus, "Connecting...")

        conn = DBUtil.connect(self.__conn_str)
        wx.CallAfter(self.window.UpdateStatus, "Querying...")

        desc, rows = DBUtil.fetch_table(conn, self.__query_str)
        wx.CallAfter(self.window.UpdateStatus, "Done")

        end_ts = time.time()
        wx.CallAfter(self.window.StopQuery, desc, rows, end_ts - start_ts)


class DataFrame(wx.MDIChildFrame):
    def __init__(self, parent, title):
        wx.MDIChildFrame.__init__(self, parent, title=title)

        self.initUI()


    def initUI(self):
        panel = wx.Panel(self)

        self.txtOdbc = wx.TextCtrl(panel, value=R.String.DEF_CONNSTR)
        self.txtQuery = SqlEditor(panel, value=R.String.DEF_QUERY)
        btnOpen = CreateButton(panel, R.String.OPEN, self.OnBtnOpen)
        btnSave = CreateButton(panel, R.String.SAVE, self.OnBtnSave)
        self.btnRun = CreateButton(panel, R.String.RUN,  self.OnBtnRun)
        self.btnExport = CreateButton(panel, R.String.EXPORT, self.OnBtnExport)
        self.gridData = DataGrid(panel)

        #main sizer
        msizer = wx.BoxSizer(wx.VERTICAL)

        # connection string
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)

        hbox1.Add(wx.StaticText(panel, label=R.String.CONNSTR_LABEL),
                  flag=wx.ALL|wx.ALIGN_CENTER_VERTICAL,
                  border=R.Value.BORDER)
        hbox1.Add(self.txtOdbc, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        # query text
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        hbox2.Add(wx.StaticText(panel, label=R.String.QUERY_LABEL),
            flag=wx.ALL, border=R.Value.BORDER)
        hbox2.Add(self.txtQuery, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        #buttons
        btnsizer = wx.BoxSizer(wx.VERTICAL)

        btnsizer.Add(btnOpen, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(btnSave, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(self.btnRun, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(self.btnExport, 0, wx.ALL, border=R.Value.BORDER)

        hbox2.Add(btnsizer)

        gridbox = wx.StaticBoxSizer(wx.StaticBox(panel,
                                                 label=R.String.DATA_LABEL))
        gridbox.Add(self.gridData, 1, wx.EXPAND)

        msizer.Add(hbox1, 0, flag=wx.EXPAND|wx.TOP|wx.LEFT|wx.RIGHT,
                   border=R.Value.BORDER)
        msizer.Add(hbox2, 0, wx.EXPAND|wx.TOP|wx.LEFT|wx.RIGHT,
                   border=R.Value.BORDER)
        msizer.Add(gridbox, 1, wx.EXPAND|wx.ALL,
                   border=R.Value.BORDER)

        panel.SetSizer(msizer)

        # status bar
        self.statusbar = ProgressStatusBar(self)
        self.SetStatusBar(self.statusbar)


    def OnBtnOpen(self, event):
        dlg = wx.FileDialog(self, message=R.String.OPEN,
                            wildcard="*.sql",
                            style=wx.FD_OPEN)

        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            with file(path, "r") as sqlfile:
                self.txtQuery.SetText(sqlfile.read())

        dlg.Destroy()


    def OnBtnSave(self, event):
        dlg = wx.FileDialog(self, message=R.String.SAVE,
                            wildcard="*.sql",
                            style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)

        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            with file(path, "w") as sqlfile:
                sqlfile.write(self.txtQuery.GetText())

        dlg.Destroy()


    def OnBtnRun(self, event):
        self.StartQuery()
        task = DataThread(self.txtOdbc.GetValue(), self.txtQuery.GetText(),
                          self)
        task.start()


    def OnBtnExport(self, event):
        dlg = wx.FileDialog(self, message=R.String.EXPORT,
                            wildcard="*.csv",
                            style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)

        if dlg.ShowModal() ==  wx.ID_OK:
            path = dlg.GetPath()
            try:
                self.gridData.SaveTo(path)
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nExport failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


    def UpdateStatus(self, msg):
        self.statusbar.SetStatusText(msg)


    def StartQuery(self):
        self.statusbar.StartBusy()
        self.btnRun.Disable()
        self.btnExport.Disable()


    def StopQuery(self, desc, rows, elapsed):
        self.gridData.RefreshData(desc, rows)

        msg = "%d rows got, %.0f seconds elapsed" % (len(rows), elapsed)
        self.statusbar.SetStatusText(msg, 1)

        self.statusbar.StopBusy()
        self.btnRun.Enable()
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
                              R.String.TITLE_DATAVIEW + " {0}".format(
                                   self.__index))
            self.__index += 1
            frame.Show()


class MainApp(wx.App):

    def OnInit(self):
        self.frame = MainFrame(title=R.String.TITLE_MAINWIN)
        self.frame.Show()

        return True


if __name__ == "__main__":
    app = MainApp()
    app.MainLoop()
