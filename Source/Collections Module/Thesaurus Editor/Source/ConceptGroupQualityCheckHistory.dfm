object dlgConceptGroupQualityCheckHistory: TdlgConceptGroupQualityCheckHistory
  Left = 613
  Top = 537
  BorderStyle = bsSingle
  Caption = 'Quality Check History'
  ClientHeight = 204
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnGrid: TPanel
    Left = 0
    Top = 0
    Width = 275
    Height = 161
    Align = alTop
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object sgHistory: TDSSStringGrid
      Left = 1
      Top = 1
      Width = 273
      Height = 159
      Align = alClient
      ColCount = 2
      Ctl3D = True
      FixedCols = 0
      ParentCtl3D = False
      TabOrder = 0
      ReadOnly = True
      ColWidths = (
        119
        125)
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 163
    Width = 275
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnClose: TButton
      Left = 107
      Top = 9
      Width = 55
      Height = 21
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
end
