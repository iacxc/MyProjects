#!/usr/bin/python -O


import pypyodbc
import time

import wx
import wx.stc as stc
import wx.grid

import resource as R

class DataTable(wx.grid.PyGridTableBase):
    def __init__(self, data, colLabels=None, rowLabels=None):
        super(DataTable, self).__init__()

        self.data = data
        self.colLabels = colLabels


    def GetNumberRows(self):
        return len(self.data) 


    def GetNumberCols(self):
        return len(self.data[0])


    def GetColLabelValue(self, col):
        return self.colLabels[col] if self.colLabels else col
            

    def GetRowLabelValue(self, row):
        return row


    def IsEmptyCell(self, row, col):
        return False


    def GetValue(self, row, col):
        return self.data[row][col]


    def SetValue(self, row, col, value):
        pass


class DataGrid(wx.grid.Grid):
    def __init__(self, parent):
        super(DataGrid, self).__init__(parent)

        tableBase = DataTable(((1,2,3,4,5),))
        self.SetTable(tableBase)

    def set_value(self, data, colLabels):
        tableBase = DataTable(data, colLabels)
        self.SetTable(tableBase)


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


class DataFrame(wx.Frame):
    default_connstr = 'DSN=bronto10;UID=seapilot;PWD=seapilot1'
    def __init__(self, parent, ID, title, size=(1200,800)):
        super(DataFrame, self).__init__(parent, ID, title, size=size) 

        self.initUI()
        self.SetInitialSize()


    def initUI(self):
        font = wx.Font(R.Value.FONTSIZE, 
                       wx.FONTFAMILY_MODERN, 
                       wx.FONTSTYLE_NORMAL, 
                       wx.FONTWEIGHT_NORMAL)

        panel = wx.Panel(self)
        vbox = wx.BoxSizer(wx.VERTICAL)

        hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        # connection string
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

        btnExecute = wx.Button(panel, R.Id.ID_EXECUTE, label=R.String.EXECUTE)
        btnExecute.SetFont(font)

        hbox2.Add(stQuery, flag=wx.ALL, border=R.Value.BORDER)
        hbox2.Add(self.txtQuery, 1, wx.EXPAND|wx.ALL,
                  border=R.Value.BORDER)
        hbox2.Add(btnExecute, 0, wx.ALL, border=R.Value.BORDER)

        # grid
        self.grid = DataGrid(panel)
        self.grid.SetFont(font)

        vbox.Add(hbox1, 0, flag=wx.EXPAND|wx.ALL, border=R.Value.BORDER)
        vbox.Add(hbox2, 0, wx.EXPAND|wx.ALL, border=R.Value.BORDER)

        vbox.Add(self.grid, 1, wx.EXPAND|wx.ALL, border=R.Value.BORDER*2)

        panel.SetSizer(vbox)

        # status bar
        statusbar = self.CreateStatusBar()
        statusbar.SetFont(font)
        statusbar.SetFieldsCount(2)
        statusbar.SetStatusWidths([-4, -1])

        self.Bind(wx.EVT_BUTTON, self.OnExecute, id=R.Id.ID_EXECUTE)


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

        self.grid.set_value(cursor.fetchall(), 
                            [ fd[0] for fd in cursor.description ])

        conn.close() 

class MainApp(wx.App):

    def OnInit(self):
        self.frame = DataFrame(parent=None, ID=-1, title='Data Viewer')
        self.frame.Show()

        return True


if __name__ == '__main__':
    app = MainApp()
    app.MainLoop()
