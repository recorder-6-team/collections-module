object dlgFind: TdlgFind
  Left = 491
  Top = 340
  Width = 341
  Height = 260
  BorderIcons = [biSystemMenu]
  Caption = 'Find'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF008888
    888888838888888888888888888F888888888882282328888888882288FF8877
    77777777223332722377222222FF8877777777777233222222222222222F8877
    77777777772333222222222277FF887777777777777777722222222222FF8877
    777777777777722222222222222F887777777777777223333323222222228877
    7777777772223333323322222222222772777777777222333333222222220332
    322777777777723332332222722F223323222227777772332333332277FF8232
    22233327777773332233322227FF883322223332777723220233322277FF8873
    22223332777777772223332277FF887332222332277777772233322777FF8877
    32222332777777777233333277FF882232222222777777773233233777FF8872
    22322222277777773333227777FF882022332223322777773333277777FF8872
    22223233332777277233277777FF887227772332027772222233377777FF8877
    77722332227772333233277777FF887777772222277772333332777777FF8877
    77772727777772233222777777FF887777777777777777232777777777FF8877
    77777777777722222227777777FF887777777777777723333222777777FF8877
    77777777777772333332777777FF887777777777777230333333277777FF8FFF
    FFFFFFFFFFFF2323333332FFFFFFFFFFFFFFFFFFFFFF2332333332FFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000008000000000000000000000000000080000000000000000000000
    0000000000000000000010000000000000000008000000000000000000000000
    000000000000000000000000000000000000000040000000000000000000}
  OldCreateOrder = True
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  DesignSize = (
    327
    223)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlFrame: TBevel
    Left = 4
    Top = 8
    Width = 317
    Height = 171
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 61
    Height = 13
    Caption = 'Search &Text:'
  end
  object Label2: TLabel
    Left = 12
    Top = 68
    Width = 44
    Height = 13
    Caption = '&Matches:'
  end
  object btnOk: TImageListButton
    Left = 169
    Top = 187
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 247
    Top = 187
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object eSearchText: TEdit
    Left = 12
    Top = 32
    Width = 301
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = eSearchTextChange
    OnKeyDown = eSearchTextKeyDown
  end
  object lbMatches: TIDListBox
    Left = 12
    Top = 84
    Width = 301
    Height = 86
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    Style = lbOwnerDrawFixed
    TabOrder = 1
    OnClick = lbMatchesClick
    OnDblClick = lbMatchesDblClick
    OnDrawItem = lbMatchesDrawItem
    OnKeyDown = lbMatchesKeyDown
  end
  object Animation: TAnimate
    Left = 8
    Top = 191
    Width = 16
    Height = 16
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Center = False
    CommonAVI = aviFindFile
    StopFrame = 8
    Visible = False
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrSearchTimer
    Left = 88
    Top = 180
  end
end
