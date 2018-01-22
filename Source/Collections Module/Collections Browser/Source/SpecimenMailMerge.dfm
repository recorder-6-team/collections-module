object dlgSpecimenMailMerge: TdlgSpecimenMailMerge
  Left = 388
  Top = 352
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Specimen Mail Merge Output'
  ClientHeight = 258
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 421
    Height = 217
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 131
    Height = 14
    Caption = 'Rendering XSL style sheet:'
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 66
    Height = 14
    Caption = 'Output folder:'
  end
  object Label3: TLabel
    Left = 16
    Top = 152
    Width = 105
    Height = 14
    Caption = 'XML Report file name:'
  end
  object Label4: TLabel
    Left = 20
    Top = 112
    Width = 366
    Height = 28
    Caption = 
      'Note: If the folder you specify does not already exist, it will ' +
      'be automatically created for you.'
    WordWrap = True
  end
  object btnCancel: TImageListButton
    Left = 350
    Top = 228
    Width = 74
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOK: TImageListButton
    Left = 264
    Top = 228
    Width = 74
    Height = 25
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 5
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object eStyleSheet: TEdit
    Left = 16
    Top = 32
    Width = 377
    Height = 22
    TabOrder = 0
    OnChange = TextChanged
    OnExit = eStyleSheetExit
  end
  object btnGetStyleSheet: TButton
    Left = 392
    Top = 32
    Width = 22
    Height = 22
    Caption = '...'
    TabOrder = 1
    OnClick = btnGetStyleSheetClick
  end
  object eOutputFolder: TEdit
    Left = 16
    Top = 88
    Width = 377
    Height = 22
    TabOrder = 2
    OnChange = TextChanged
  end
  object btnGetOutputFolder: TImageListButton
    Left = 392
    Top = 88
    Width = 22
    Height = 22
    TabOrder = 3
    OnClick = btnGetOutputFolderClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 11
  end
  object eReportFileName: TEdit
    Left = 16
    Top = 168
    Width = 281
    Height = 22
    TabOrder = 4
    OnChange = TextChanged
  end
  object chkGenerateLabelRecords: TCheckBox
    Left = 16
    Top = 197
    Width = 137
    Height = 17
    Caption = 'Generate label records'
    TabOrder = 7
  end
  object dlgOpen: TOpenDialog
    Filter = 'Style Sheets (*.xsl)|*.xsl|All Files (*.*)|*.*'
    Left = 320
    Top = 152
  end
  object dlgFolder: TFolderBrowser
    BrowseFlags = [bfStatusText]
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 372
    Top = 152
  end
  object Barcode: TBarcode
    Height = 70
    Top = 32
    Left = 264
    Modul = 2
    Ratio = 2.000000000000000000
    Typ = bcCode_2_5_industrial
    ShowTextFont.Charset = DEFAULT_CHARSET
    ShowTextFont.Color = clWindowText
    ShowTextFont.Height = -11
    ShowTextFont.Name = 'MS Sans Serif'
    ShowTextFont.Style = []
  end
end
