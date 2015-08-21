
# -*- coding: utf-8 -*-

import csv
import wx


class DataListCtrl(wx.ListView):
    def __init__(self, parent):
        super(DataListCtrl, self).__init__(parent, style=wx.LC_REPORT)

        self.__cur_row = -1
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)


    def RefreshData(self, (titles, rows)):
        self.ClearAll()

        for index, title in enumerate(titles):
            self.InsertColumn(index, unicode(title, "utf-8"))
            self.SetColumnWidth(index, len(title)*30)

        for row in rows:
            self.Append(row)


    def OnItemSelected(self, event):
        self.__cur_row = event.GetIndex()


    def GetRow(self):
        assert self.__cur_row >= 0 and self.__cur_row < self.GetItemCount()

        colcount = self.GetColumnCount()
        return [ self.GetItemText(self.__cur_row, cindex)
                               for cindex in range(colcount) ]

