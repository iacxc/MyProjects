
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


    def Dump(self):

        colcount = self.GetColumnCount()
        rows = []
        for rindex in range(self.GetItemCount()):
            rows.append([self.GetItemText(rindex, cindex).encode("utf-8")
                              for cindex in range(colcount)])
        return rows

