
import wx

import resource as R


class DataDialog(wx.Dialog):
    def __init__(self, parent, title):
        super(DataDialog, self).__init__(parent, title=title)

        self.initUI()

    def initUI(self):
        raise NotImplementedError


    def get_data(self):
        raise NotImplementedError


class ProdDlg(DataDialog):
    def __init__(self, parent):
        super(ProdDlg, self).__init__(parent, title=R.String.TITLE_PROD)


    def initUI(self):
        msizer = wx.BoxSizer(wx.VERTICAL)

        inputszr = wx.FlexGridSizer(3, 2, R.Value.BORDER, R.Value.BORDER)

        inputszr.AddGrowableCol(1)

        self.txtId = wx.TextCtrl(self)
        self.txtDesc = wx.TextCtrl(self, size=(300, -1))
        self.txtBarcode = wx.TextCtrl(self)

        inputszr.AddMany([(wx.StaticText(self, -1, R.String.ST_PRODID),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtId, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_PRODDESC),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtDesc, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_BARCODE),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtBarcode, 1, wx.EXPAND)])

        btnszr = wx.StdDialogButtonSizer()
        btnszr.AddButton(wx.Button(self, wx.ID_OK))
        btnszr.AddButton(wx.Button(self, wx.ID_CANCEL))
        btnszr.Realize()

        msizer.AddMany([(inputszr, 0, wx.EXPAND|wx.ALL, R.Value.BORDER),
                        (btnszr, 0, wx.ALIGN_CENTER|wx.ALL, R.Value.BORDER)])

        self.SetSizer(msizer)
        msizer.Fit(self)


    def get_data(self):
        return (self.txtId.GetValue(),
                self.txtDesc.GetValue(),
                self.txtBarcode.GetValue())



class CustomerDlg(DataDialog):
    def __init__(self, parent):
        super(CustomerDlg, self).__init__(parent, title=R.String.TITLE_CUSTOMER)


    def initUI(self):
        msizer = wx.BoxSizer(wx.VERTICAL)

        inputszr = wx.FlexGridSizer(7, 2, R.Value.BORDER, R.Value.BORDER)

        inputszr.AddGrowableCol(1)

        self.txtId = wx.TextCtrl(self, )
        self.txtName = wx.TextCtrl(self)
        self.txtCellphone = wx.TextCtrl(self)
        self.txtWeixin = wx.TextCtrl(self)
        self.txtQQ = wx.TextCtrl(self)
        self.txtAddress = wx.TextCtrl(self, size=(300, 60), style=wx.TE_MULTILINE)
        self.txtOther = wx.TextCtrl(self, size=(300, 40), style=wx.TE_MULTILINE)

        inputszr.AddMany([(wx.StaticText(self, -1, R.String.ST_CUSTID),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtId, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTNAME),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtName, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTCELL),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtCellphone, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTWEIXIN),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtWeixin, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTQQ),
                               0, wx.ALIGN_CENTER_VERTICAL),
                          (self.txtQQ, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTADDRESS),
                               0, wx.ALIGN_TOP),
                          (self.txtAddress, 1, wx.EXPAND),
                          (wx.StaticText(self, -1, R.String.ST_CUSTOTHER),
                               0, wx.ALIGN_TOP),
                          (self.txtOther,  1, wx.EXPAND)])

        btnszr = wx.StdDialogButtonSizer()
        btnszr.AddButton(wx.Button(self, wx.ID_OK, R.String.BTN_OK))
        btnszr.AddButton(wx.Button(self, wx.ID_CANCEL, R.String.BTN_CANCEL))
        btnszr.Realize()

        msizer.AddMany([(inputszr, 0, wx.EXPAND|wx.ALL, R.Value.BORDER),
                        (btnszr, 0, wx.ALIGN_CENTER|wx.ALL, R.Value.BORDER)])

        self.SetSizer(msizer)
        msizer.Fit(self)



    def get_data(self):
        return (self.txtId.GetValue(),
                self.txtName.GetValue(),
                self.txtCellphone.GetValue(),
                self.txtWeixin.GetValue(),
                self.txtQQ.GetValue(),
                self.txtAddress.GetValue(),
                self.txtOther.GetValue())
