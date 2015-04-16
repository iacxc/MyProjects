#!/usr/bin/python -O

import os, sys
import wx

import resource as R

from Catalog import CatalogFrame


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
