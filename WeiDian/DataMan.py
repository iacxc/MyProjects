

import wx

import DBUtil
import resource as R
from controls import DataListCtrl


class DataManFrame(wx.Frame):
    def __init__(self, parent, title, tablename, dialog):
        super(DataManFrame, self).__init__(parent, title='DataMan - ' + title)
        self.tablename = tablename
        self.dialog = dialog

        self.SetupWindow()


    def SetupWindow(self):
        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        self.lstData = DataListCtrl(panel)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.AddMany([(wx.Button(panel, R.Id.ID_ADD,    R.String.BTN_ADD),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_DELETE, R.String.BTN_DELETE),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_IMPORT, R.String.BTN_IMPORT),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_EXPORT, R.String.BTN_EXPORT),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_DUMP, R.String.BTN_DUMP),
                            1, wx.EXPAND),
                      ])

        vbox.Add(self.lstData, 1, wx.EXPAND, border=R.Value.BORDER)
        vbox.Add(hbox, 0, wx.EXPAND, border=R.Value.BORDER)
        panel.SetSizer(vbox)

        self.Bind(wx.EVT_BUTTON, self.OnAdd, id=R.Id.ID_ADD)
        self.Bind(wx.EVT_BUTTON, self.OnDelete, id=R.Id.ID_DELETE)
        self.Bind(wx.EVT_BUTTON, self.OnImport, id=R.Id.ID_IMPORT)
        self.Bind(wx.EVT_BUTTON, self.OnExport, id=R.Id.ID_EXPORT)
        self.Bind(wx.EVT_BUTTON, self.OnDump, id=R.Id.ID_DUMP)

        self.SetSize((800, 600))
        self.refreshData()


    def refreshData(self):
        self.lstData.RefreshData(DBUtil.get_table_data(self.tablename))


    def OnAdd(self, event):
        dlg = self.dialog(self)
        if dlg.ShowModal() == wx.ID_OK:
            try:
                DBUtil.insert(self.tablename, dlg.get_data())

                self.refreshData()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nAdd failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


    def OnDelete(self, event):
        row = self.lstData.GetRow()
        result = wx.MessageBox(
                     "Are you sure to delete {0}".format(row),
                     R.String.TITLE_DELETE,
                     wx.CENTER|wx.YES_NO|wx.ICON_QUESTION,
                     self)

        if result == wx.YES:
            try:
                DBUtil.delete(self.tablename, row)

                self.lstData.Delete()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nDelete failed",
                              R.String.TITLE_FAILURE)


    def OnImport(self, event):
        dlg = wx.FileDialog(self, message="Import",
                            wildcard="*.csv",
                            style=wx.FD_OPEN)

        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()

            try:
                DBUtil.set_table_data(self.tablename,
                                  self.lstData.LoadFrom(path))
                self.refreshData()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nImport failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


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


    def OnDump(self, event):
        dlg = wx.FileDialog(self, message="Dump",
                            wildcard="*.sql",
                            style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)

        if dlg.ShowModal() ==  wx.ID_OK:
            path = dlg.GetPath()
            try:
                DBUtil.dump(path, self.tablename, self.lstData.Dump())
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nDump failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()



