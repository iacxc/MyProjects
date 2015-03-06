#!/usr/bin/python -O

import os, sys
import wx

import DBUtil
import resource as R
import Singleton
from controls import DataListCtrl, ProdDlg


class CatalogFrame(wx.Frame):
    __metaclass__ = Singleton.Singleton
    tablename = 'T_CATALOG'

    def SetupWindow(self):
        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        self.lstData = DataListCtrl(panel)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.AddMany([(wx.Button(panel, R.Id.ID_ADD,    R.String.BTN_ADD),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_DELETE, R.String.BTN_DELETE),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_MODIFY, R.String.BTN_MODIFY),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_IMPORT, R.String.BTN_IMPORT),
                            1, wx.EXPAND),
                      (wx.Button(panel, R.Id.ID_EXPORT, R.String.BTN_EXPORT),
                            1, wx.EXPAND)])

        vbox.Add(self.lstData, 1, wx.EXPAND, border=R.Value.BORDER)
        vbox.Add(hbox, 0, wx.EXPAND, border=R.Value.BORDER)
        panel.SetSizer(vbox)

        self.Bind(wx.EVT_BUTTON, self.OnAdd, id=R.Id.ID_ADD)
        self.Bind(wx.EVT_BUTTON, self.OnDelete, id=R.Id.ID_DELETE)
        self.Bind(wx.EVT_BUTTON, self.OnModify, id=R.Id.ID_MODIFY)
        self.Bind(wx.EVT_BUTTON, self.OnImport, id=R.Id.ID_IMPORT)
        self.Bind(wx.EVT_BUTTON, self.OnExport, id=R.Id.ID_EXPORT)

        self.refreshData()


    def refreshData(self):
        self.lstData.RefreshData(DBUtil.get_table_data(self.tablename))


    def OnAdd(self, event):
        dlg = ProdDlg(self)
        if dlg.ShowModal() == wx.ID_OK:
            try:
                DBUtil.insert(self.tablename,
                          (dlg.prodid, dlg.proddesc, dlg.barcode))

                self.refreshData()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nAdd failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


    def OnDelete(self, event):
        prodid, proddesc, barcode = self.lstData.GetRow()
        result = wx.MessageBox(
                     "Are you sure to delete product {0}".format(prodid),
                     R.String.TITLE_DELETE,
                     wx.CENTER|wx.YES_NO|wx.ICON_QUESTION, 
                     self)

        if result == wx.YES:
            try:
                DBUtil.delete(self.tablename, (prodid,))

                self.lstData.Delete()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nDelete failed",
                              R.String.TITLE_FAILURE)


    def OnModify(self, event):
        dlg = ProdDlg(self, R.Value.MODE_MODIFY)

        dlg.prodid, dlg.proddesc, dlg.barcode = self.lstData.GetRow()

        if dlg.ShowModal() == wx.ID_OK:
            try:
                DBUtil.update(self.tablename,
                         (dlg.prodid,), (dlg.proddesc, dlg.barcode))

                self.refreshData()
            except Exception as exp:
                wx.MessageBox(exp.message + ",\nModify failed",
                              R.String.TITLE_FAILURE)

        dlg.Destroy()


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


class MainFrame(wx.Frame):
    def __init__(self, parent=None, title=""):
        super(MainFrame, self).__init__(parent, -1, title, size=(600, 300))

        menu = wx.Menu()
        menu.Append(R.Id.ID_CATALOG, R.String.MENU_CATALOG)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, R.String.MENU_EXIT)

        menubar = wx.MenuBar()
        menubar.Append(menu, R.String.MENU_TOOL)

        self.SetMenuBar(menubar)

        self.__frmcatalog = CatalogFrame(self, title=R.String.TITLE_CATALOG)

        self.Bind(wx.EVT_MENU, self.OnCatalog, id=R.Id.ID_CATALOG)
        self.Bind(wx.EVT_MENU, self.OnExit, id=wx.ID_EXIT)


    def OnCatalog(self, event):
        self.__frmcatalog.Show()


    def OnExit(self, event):
        self.Close(True)


class MainApp(wx.App):
    def OnInit(self):
        if not os.path.exists(R.Value.DBFILE):
            wx.MessageBox('Failed to start, please run init.py first',
                      'Failed to start',
                      style=wx.OK|wx.ICON_ERROR)
            sys.exit(1)

        frame = MainFrame(title=R.String.TITLE_MAIN)
        frame.Show()

        return True


if __name__ == '__main__':
    app = MainApp()
    app.MainLoop()
