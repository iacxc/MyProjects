#!/usr/bin/python -O

import csv
import wx

import resource as R


class DataListCtrl(wx.ListCtrl):
    def __init__(self, parent):
        super(DataListCtrl, self).__init__(parent, style=wx.LC_REPORT)

        self.__cur_row = -1
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)


    def RefreshData(self, (titles, rows)):
        self.ClearAll()

        for index, title in enumerate(titles):
            self.InsertColumn(index, title)
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


    def Delete(self):
        assert self.__cur_row >= 0 and self.__cur_row < self.GetItemCount()
        self.DeleteItem(self.__cur_row)
        self.__cur_row = -1


    def LoadFrom(self, path):
        with file(path, "rb") as csvfile:
            reader = csv.reader(csvfile)

            rows = [ [unicode(item, "utf-8") for item in row]
                                            for row in reader ]

        return rows


    def SaveTo(self, path):
        with file(path, "wb") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

            colcount = self.GetColumnCount()

            for rindex in range(self.GetItemCount()):
                row = [self.GetItemText(rindex, cindex).encode("utf-8")
                                      for cindex in range(colcount)]

                writer.writerow(row)


class ProdDlg(wx.Dialog):
    def __init__(self, parent, mode=R.Value.MODE_ADD):
        super(ProdDlg, self).__init__(parent, title=R.String.TITLE_PROD)

        self.initUI(mode)


    def initUI(self, mode):
        msizer = wx.BoxSizer(wx.VERTICAL)

        box1 = wx.FlexGridSizer(3, 2, R.Value.BORDER, R.Value.BORDER)

        box1.AddGrowableCol(1, 1)

        self.txtProdId = wx.TextCtrl(self)
        self.txtProdId.SetEditable(mode == R.Value.MODE_ADD)

        self.txtProdDesc = wx.TextCtrl(self)
        self.txtBarcode = wx.TextCtrl(self)

        box1.Add(wx.StaticText(self, label=R.String.ST_PRODID),
                0, wx.ALIGN_CENTER_VERTICAL)
        box1.Add(self.txtProdId, 1, wx.EXPAND|wx.ALIGN_CENTER_VERTICAL)
        box1.Add(wx.StaticText(self, label=R.String.ST_PRODDESC),
                0, wx.ALIGN_CENTER_VERTICAL)
        box1.Add(self.txtProdDesc, 1, wx.EXPAND|wx.ALIGN_CENTER_VERTICAL)
        box1.Add(wx.StaticText(self, label=R.String.ST_BARCODE),
                0, wx.ALIGN_CENTER_VERTICAL)
        box1.Add(self.txtBarcode, 1, wx.EXPAND|wx.ALIGN_CENTER_VERTICAL)

        msizer.Add(box1, 1, wx.EXPAND|wx.ALL, R.Value.BORDER)

        btnszr = wx.StdDialogButtonSizer()
        btnszr.AddButton(wx.Button(self, wx.ID_OK))
        btnszr.AddButton(wx.Button(self, wx.ID_CANCEL))

        msizer.Add(btnszr, 0, wx.ALIGN_CENTER|wx.ALL, R.Value.BORDER)
        btnszr.Realize()

        self.SetSizer(msizer)


    @property
    def prodid(self):
        return self.txtProdId.GetValue()

    @prodid.setter
    def prodid(self, value):
        self.txtProdId.SetValue(value)


    @property
    def proddesc(self):
        return self.txtProdDesc.GetValue()

    @proddesc.setter
    def proddesc(self, value):
        self.txtProdDesc.SetValue(value)

    @property
    def barcode(self):
        return self.txtBarcode.GetValue()

    @barcode.setter
    def barcode(self, value):
        self.txtBarcode.SetValue(value)
