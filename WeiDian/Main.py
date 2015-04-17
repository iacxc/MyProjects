#!/usr/bin/python -O

import os, sys
import wx

import resource as R

from DataMan import DataManFrame
from dialogs import ProdDlg, CustomerDlg

class MainFrame(wx.Frame):
    def __init__(self, parent=None, title=""):
        super(MainFrame, self).__init__(parent, -1, title, size=(600, 300))

        menu = wx.Menu()
        menu.Append(R.Id.ID_CATALOG, R.String.MENU_CATALOG)
        menu.Append(R.Id.ID_CUSTOMER, R.String.MENU_CUSTOMER)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, R.String.MENU_EXIT)

        menubar = wx.MenuBar()
        menubar.Append(menu, R.String.MENU_DATA)

        self.SetMenuBar(menubar)

        self.Bind(wx.EVT_MENU, self.OnCatalog, id=R.Id.ID_CATALOG)
        self.Bind(wx.EVT_MENU, self.OnCustomer, id=R.Id.ID_CUSTOMER)
        self.Bind(wx.EVT_MENU, self.OnExit, id=wx.ID_EXIT)


    def OnCatalog(self, event):
        DataManFrame(self,
                     R.String.TITLE_CATALOG,
                     R.String.TABLE_CATALOG,
                     ProdDlg).Show()


    def OnCustomer(self, event):
        DataManFrame(self,
                     R.String.TITLE_CUSTOMER,
                     R.String.TABLE_CUSTOMER,
                     CustomerDlg).Show()


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
