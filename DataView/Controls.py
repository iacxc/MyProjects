

import csv

import wx
import wx.stc as stc


class SqlEditor(stc.StyledTextCtrl):
    def __init__(self, parent, font, value=None):
        super(SqlEditor, self).__init__(parent, size=(300, 180))

        self.__font = font

        self.SetLexer(stc.STC_LEX_SQL)
        self.SetKeyWords(0,
            " ".join(['select', 'from', 'insert', 'at', 'epoch', 'latest',
                       'limit', 'order', 'group', 'by']))

        self.SetupStyles()
        self.EnableLineNumber()

        if value:
            self.SetText(value)


    def EnableLineNumber(self):
        #enable line number margin
        self.SetMarginType(1, stc.STC_MARGIN_NUMBER)
        self.SetMarginMask(1,0)
        self.SetMarginWidth(1, 25)


    def SetupStyles(self):
        face = self.__font.GetFaceName()
        size = self.__font.GetPointSize()

        fonts = "face:%s,size:%d" % (face, size)
        default = "fore:#000000," + fonts
        self.StyleSetSpec(stc.STC_STYLE_DEFAULT, default)

        line = "back:#C0C0C0," + fonts
        self.StyleSetSpec(stc.STC_STYLE_LINENUMBER, line)
        self.StyleSetSpec(stc.STC_STYLE_CONTROLCHAR, "face:%s" % face)

        self.StyleSetSpec(stc.STC_SQL_DEFAULT, default)

        self.StyleSetSpec(stc.STC_SQL_COMMENT, "fore:#007F00," + fonts)
        self.StyleSetSpec(stc.STC_SQL_NUMBER, "fore:#007F7F," + fonts)
        self.StyleSetSpec(stc.STC_SQL_STRING, "fore:#7F007F," + fonts)
        self.StyleSetSpec(stc.STC_SQL_WORD, "fore:#7F0000,bold," + fonts)
        self.StyleSetSpec(stc.STC_SQL_OPERATOR, "bold," + fonts)


class DataListCtrl(wx.ListCtrl):
    def __init__(self, parent):
        wx.ListCtrl.__init__(self, parent, style=wx.LC_REPORT)


    def RefreshData(self, titles, rows):
        self.ClearAll()

        for index, title in enumerate(titles):
            self.InsertColumn(index, title)

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

