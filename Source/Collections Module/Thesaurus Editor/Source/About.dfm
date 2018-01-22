object dlgAboutThesaurusEditor: TdlgAboutThesaurusEditor
  Left = 438
  Top = 256
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About Thesaurus Editor'
  ClientHeight = 200
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Lucida Sans Unicode'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 379
    Height = 153
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 119
    Top = 40
    Width = 159
    Height = 23
    Caption = 'Thesaurus Editor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Lucida Sans Unicode'
    Font.Style = []
    ParentFont = False
  end
  object lblVersion: TLabel
    Left = 167
    Top = 88
    Width = 41
    Height = 15
    Caption = 'Version'
  end
  object btnClose: TButton
    Left = 312
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
end
