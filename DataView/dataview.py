#!/usr/bin/python -O


import time
import csv
import pypyodbc

import wx
import wx.stc as stc

import resource as R


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
        super(DataListCtrl, self).__init__(parent, style=wx.LC_REPORT)


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


class DataFrame(wx.Frame):
    default_connstr = 'DSN=bronto10;UID=seapilot;PWD=seapilot1'
    def __init__(self, parent, ID, title, size=(1200,800)):
        super(DataFrame, self).__init__(parent, ID, title, size=size) 

        self.initUI()
        self.SetInitialSize()

        self.Bind(wx.EVT_BUTTON, self.OnExecute, id=R.Id.ID_EXECUTE)
        self.Bind(wx.EVT_BUTTON, self.OnExport, id=R.Id.ID_EXPORT)


    def initUI(self):
        font = wx.Font(R.Value.FONTSIZE, 
                       wx.FONTFAMILY_MODERN, 
                       wx.FONTSTYLE_NORMAL, 
                       wx.FONTWEIGHT_NORMAL)

        panel = wx.Panel(self)
        #main sizer
        msizer = wx.BoxSizer(wx.VERTICAL)

        # connection string
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)

        stOdbc = wx.StaticText(panel, label=R.String.CONNSTR_LABEL)
        stOdbc.SetFont(font)

        self.txtOdbc = wx.TextCtrl(panel, value=self.default_connstr)
        self.txtOdbc.SetFont(font)

        hbox1.Add(stOdbc, flag=wx.ALL|wx.ALIGN_CENTER_VERTICAL,
                  border=R.Value.BORDER)
        hbox1.Add(self.txtOdbc, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)

        # query text
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)

        stQuery = wx.StaticText(panel, label=R.String.QUERY_LABEL)
        stQuery.SetFont(font)

        self.txtQuery = SqlEditor(panel, font, value=R.String.DEF_QUERY)

        #buttons
        btnsizer = wx.BoxSizer(wx.VERTICAL)
        btnExecute = wx.Button(panel, R.Id.ID_EXECUTE, label=R.String.EXECUTE)
        btnExecute.SetFont(font)

        btnExport = wx.Button(panel, R.Id.ID_EXPORT, label=R.String.EXPORT)
        btnExport.SetFont(font)

        btnsizer.Add(btnExecute, 0, wx.ALL, border=R.Value.BORDER)
        btnsizer.Add(btnExport,  0, wx.ALL, border=R.Value.BORDER)

        hbox2.Add(stQuery, flag=wx.ALL, border=R.Value.BORDER)
        hbox2.Add(self.txtQuery, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)
        hbox2.Add(btnsizer)

        # grid
        self.lstData = DataListCtrl(panel)

        msizer.Add(hbox1, 0, flag=wx.EXPAND|wx.ALL, border=R.Value.BORDER)
        msizer.Add(hbox2, 0, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        msizer.Add(self.lstData, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER*2)

        panel.SetSizer(msizer)

        # status bar
        statusbar = self.CreateStatusBar()
        statusbar.SetFont(font)
        statusbar.SetFieldsCount(2)
        statusbar.SetStatusWidths([-4, -1])


    def OnExecute(self, event):
        connstr = self.txtOdbc.GetValue()
        query = self.txtQuery.GetText()

        start_ts = time.time()
        self.PushStatusText('Connecting...', 0)
        conn = pypyodbc.connect(connstr)

        self.PushStatusText('Querying...', 0)
        cursor = conn.cursor()
        cursor.execute(query)

        now = time.time()
        self.PushStatusText('Done', 0)
        self.PushStatusText('{0} rows got, {1:.0f} seconds elapsed'.format(
                                     cursor.rowcount, now - start_ts), 
                        1) 

        self.lstData.RefreshData([ fd[0] for fd in cursor.description ],
                                  cursor.fetchall(),)

        conn.close() 


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

class MainApp(wx.App):

    def OnInit(self):
        self.frame = DataFrame(parent=None, ID=-1, title='Data Viewer')
        self.frame.Show()

        return True


if __name__ == '__main__':
    app = MainApp()
    app.MainLoop()
