

import csv

import wx
import wx.grid
import wx.stc


class SqlEditor(wx.stc.StyledTextCtrl):
    def __init__(self, parent, value=None):
        super(SqlEditor, self).__init__(parent, size=(300, 180))

        self.SetLexer(wx.stc.STC_LEX_SQL)
        self.SetKeyWords(0,
            " ".join(['at', 'epoch', 'latest',
                      'select', 'from', 'insert', 'where',
                      'limit', 'order', 'group', 'by']))

        self.SetupStyles()
        self.EnableLineNumber()

        if value:
            self.SetText(value)


    def EnableLineNumber(self):
        #enable line number margin
        self.SetMarginType(1, wx.stc.STC_MARGIN_NUMBER)
        self.SetMarginMask(1,0)
        self.SetMarginWidth(1, 25)


    def SetupStyles(self):

        default = "fore:#000000"
        self.StyleSetSpec(wx.stc.STC_STYLE_DEFAULT, default)

        line = "back:#C0C0C0"
        self.StyleSetSpec(wx.stc.STC_STYLE_LINENUMBER, line)

        self.StyleSetSpec(wx.stc.STC_SQL_DEFAULT, default)

        self.StyleSetSpec(wx.stc.STC_SQL_COMMENT, "fore:#007F00")
        self.StyleSetSpec(wx.stc.STC_SQL_NUMBER, "fore:#007F7F")
        self.StyleSetSpec(wx.stc.STC_SQL_STRING, "fore:#7F007F")
        self.StyleSetSpec(wx.stc.STC_SQL_WORD, "fore:#7F0000,bold")
        self.StyleSetSpec(wx.stc.STC_SQL_OPERATOR, "bold")


class DataListCtrl(wx.ListCtrl):
    def __init__(self, parent):
        super(DataListCtrl, self).__init__(parent,
                             style=wx.LC_REPORT|wx.LC_HRULES|wx.LC_VRULES)


    def RefreshData(self, titles, rows):
        self.ClearAll()

        for index, title in enumerate(titles):
            self.InsertColumn(index, title)
            self.SetColumnWidth(index, wx.LIST_AUTOSIZE_USEHEADER)
        self.SetColumnWidth(0, 150)

        for row in rows:
            self.Append(row)


    def SaveTo(self, path):
        with file(path, "wb") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

            colcount = self.GetColumnCount()

            for rindex in range(self.GetItemCount()):
                row = [self.GetItemText(rindex, cindex).encode("utf-8")
                                      for cindex in range(colcount)]

                writer.writerow(row)


class DataGrid(wx.grid.Grid):
    def __init__(self, parent):
        super(DataGrid, self).__init__(parent)
        self.CreateGrid(0,0)


    def RefreshData(self, titles, rows):
        self.BeginBatch()

        self.ClearGrid()
        self.AppendCols(len(titles))
        for index, title in enumerate(titles):
            self.SetColLabelValue(index, title)

        self.AppendRows(len(rows))
        for rindex, row in enumerate(rows):
            for cindex, value in enumerate(row):
                self.SetCellValue(rindex, cindex, str(value))

        self.EndBatch()
        self.ShowRow(0)


    def SaveTo(self, path):
        with file(path, "wb") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

            colcount = self.GetNumberCols()

            for rindex in range(self.GetNumberRows()):
                row = [self.GetCellValue(rindex, cindex).encode("utf-8")
                                      for cindex in range(colcount)]

                writer.writerow(row)


class  ProgressStatusBar(wx.StatusBar):
    def __init__(self, parent, style=wx.SB_FLAT, name="ProgressStatusBar"):
        super(ProgressStatusBar, self).__init__(parent,
                                                wx.ID_ANY, style, name)

        self.__changed = False
        self.busy = False
        self.timer = wx.Timer(self)
        self.prog = wx.Gauge(self, style=wx.GA_HORIZONTAL)
        self.prog.Hide()

        self.SetFieldsCount(2)
        self.SetStatusWidths([-1, 300])

        self.Bind(wx.EVT_IDLE, lambda evt: self._Reposition())
        self.Bind(wx.EVT_TIMER, self.OnTimer)
        self.Bind(wx.EVT_SIZE, self.OnSize)


    def __del__(self):
        if self.timer.IsRunning():
            self.timer.Stop()


    def _Reposition(self):
        if self.__changed:
            lfield = self.GetFieldsCount() - 1
            rect =  self.GetFieldRect(lfield)
            prog_pos = (rect.x+2, rect.y+2)
            prog_size = (rect.width - 8, rect.height - 4)
            self.prog.SetPosition(prog_pos)
            self.prog.SetSize(prog_size)
            self.__changed = False


    def OnSize(self, event):
        self.__changed = True
        self._Reposition()


    def OnTimer(self, event):
        if not self.prog.IsShown():
            self.timer.Stop()

        if  self.busy:
            self.prog.Pulse()


    def Run(self, rate=100):
        if not self.timer.IsRunning():
            self.timer.Start(rate)


    def GetProgress(self):
        return self.prog.GetValue()


    def SetProgress(self, val):
        if not self.prog.IsShown():
            self.ShowProgress(True)

        if val == self.prog.GetRange():
            self.prog.SetValue(0)
            self.ShowProgress(False)
        else:
            self.prog.SetValue(val)


    def ShowProgress(self, show=True):
        self._Reposition()
        self.prog.Show(show)


    def SetRange(self, val):
        if val != self.prog.GetRange():
            self.prog.SetRange(val)


    def StartBusy(self, rate=100):
        self.busy = True
        self.ShowProgress(True)
        if not self.timer.IsRunning():
            self.timer.Start(rate)


    def StopBusy(self):
        self.timer.Stop()
        self.ShowProgress(False)
        self.prog.SetValue(0)
        self.busy = False


    def IsBusy(self):
        return self.busy

