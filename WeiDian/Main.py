#!/usr/bin/python -O

# -*- coding: utf-8 -*-

import os, sys
import wx

import resource as R

from DataMan import DataManFrame
from dialogs import ProdDlg, CustomerDlg

class MainFrame(wx.Frame):
    def __init__(self, parent=None, title=""):
        super(MainFrame, self).__init__(parent, -1, title, size=(600, 300))

        menudata = wx.Menu()
        menudata.Append(R.Id.ID_CATALOG, R.String.MENU_CATALOG)
        menudata.Append(R.Id.ID_CUSTOMER, R.String.MENU_CUSTOMER)
        menudata.AppendSeparator()
        menudata.Append(wx.ID_EXIT, R.String.MENU_EXIT)

        menuprod = wx.Menu()
        menuprod.Append(R.Id.ID_BUY, R.String.MENU_BUY)
        menuprod.Append(R.Id.ID_SELL, R.String.MENU_SELL)

        menubar = wx.MenuBar()
        menubar.Append(menudata, R.String.MENU_DATA)
        menubar.Append(menuprod, R.String.MENU_PROD)

        self.SetMenuBar(menubar)

        self.Bind(wx.EVT_MENU, self.OnCatalog, id=R.Id.ID_CATALOG)
        self.Bind(wx.EVT_MENU, self.OnCustomer, id=R.Id.ID_CUSTOMER)
        self.Bind(wx.EVT_MENU, self.OnExit, id=wx.ID_EXIT)


    def OnCatalog(self, event):
        frm = DataManFrame(self,
                     R.String.TITLE_CATALOG,
                     R.Value.BASE_CATALOG,
                     ProdDlg)
        frm.Show()


    def OnCustomer(self, event):
        frm = DataManFrame(self,
                     R.String.TITLE_CUSTOMER,
                     R.Value.BASE_CUSTOMER,
                     CustomerDlg)
        frm.Show()


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
