#!/usr/bin/python -O

import wx
import DBUtil
import csv

ID_CATALOG =  wx.NewId()

class MainFrame(wx.Frame):
    def __init__(self, title):
        super(MainFrame, self).__init__(None, -1, title, size=(600, 300))

        self.__frmcatalog = None

        menu = wx.Menu()
        menu.Append(ID_CATALOG, '&Catalog')
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, 'E&xit')

        menubar = wx.MenuBar()
        menubar.Append(menu, '&Tool')

        self.SetMenuBar(menubar)

        self.Bind(wx.EVT_MENU, self.OnCatalog, id=ID_CATALOG)
        self.Bind(wx.EVT_MENU, self.OnExit, id=wx.ID_EXIT)


    def OnCatalog(self, event):
        if self.__frmcatalog is None:
            self.__frmcatalog = CatalogFrame(self)

        self.__frmcatalog.Show()


    def OnExit(self, event):
        self.Close(True)


ID_NEW    = wx.NewId()
ID_DELETE = wx.NewId()
ID_MODIFY = wx.NewId()
ID_IMPORT = wx.NewId()
ID_EXPORT = wx.NewId()

class CatalogFrame(wx.Frame):
    tablename = 'T_CATALOG'
    def __init__(self, parent, title='Catalog'):
        super(CatalogFrame, self).__init__(parent, title='Catalog',
                                           size=(600, 400))
        self.initUI()
        self.refreshData()


    def initUI(self):
        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        self.lstData = DataListCtrl(panel)
        vbox.Add(self.lstData, 1, wx.EXPAND, border=5)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(wx.Button(panel, ID_NEW,    'New'),    1, wx.EXPAND)
        hbox.Add(wx.Button(panel, ID_DELETE, 'Delete'), 1, wx.EXPAND)
        hbox.Add(wx.Button(panel, ID_MODIFY, 'Modify'), 1, wx.EXPAND)
        hbox.Add(wx.Button(panel, ID_IMPORT, 'Import'), 1, wx.EXPAND)
        hbox.Add(wx.Button(panel, ID_EXPORT, 'Export'), 1, wx.EXPAND)

        vbox.Add(hbox, 0, wx.EXPAND, border=5)
        panel.SetSizer(vbox)

        self.Bind(wx.EVT_BUTTON, self.OnNew, id=ID_NEW)
        self.Bind(wx.EVT_BUTTON, self.OnImport, id=ID_IMPORT)
        self.Bind(wx.EVT_BUTTON, self.OnExport, id=ID_EXPORT)


    def refreshData(self):
        self.lstData.RefreshData(
            DBUtil.get_table_data(self.tablename))


    def OnNew(self, event):
        pass


    def OnImport(self, event):
        dlg = wx.FileDialog(self, message="Import",
                            wildcard="*.csv",
                            style=wx.FD_OPEN)

        if dlg.ShowModal() ==  wx.ID_OK:
            path = dlg.GetPath()

            DBUtil.set_table_data(self.tablename,
                                  self.lstData.LoadFrom(path))

        dlg.Destroy()

        self.refreshData()


    def OnExport(self, event):
        dlg = wx.FileDialog(self, message="Export",
                            wildcard="*.csv",
                            style=wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT)

        if dlg.ShowModal() ==  wx.ID_OK:
            path = dlg.GetPath()
            self.lstData.SaveTo(path)

        dlg.Destroy()


class DataListCtrl(wx.ListCtrl):
    def __init__(self, parent):
        super(DataListCtrl, self).__init__(parent, style=wx.LC_REPORT)


    def RefreshData(self, (titles, rows)):
        self.ClearAll()

        for index, title in enumerate(titles):
            self.InsertColumn(index, title)
            self.SetColumnWidth(index, len(title)*30)

        for row in rows:
            self.Append(row)


    def LoadFrom(self, path):
        with file(path, "rb") as csvfile:
            reader = csv.reader(csvfile)

            rows = list(reader)

        return rows


    def SaveTo(self, path):
        with file(path, "wb") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

            colcount = self.GetColumnCount()

            for rindex in range(self.GetItemCount()):
                writer.writerow([self.GetItemText(rindex, cindex)
                                      for cindex in range(colcount)])


class MainApp(wx.App):
    def OnInit(self):
        frame = CatalogFrame(None, 'WeiDian')
        frame.Show()

        return True


if __name__ == '__main__':
    app = MainApp()
    app.MainLoop()
