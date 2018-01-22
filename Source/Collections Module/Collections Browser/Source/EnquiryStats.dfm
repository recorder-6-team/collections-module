object frmEnquiryStats: TfrmEnquiryStats
  Left = 356
  Top = 262
  ActiveControl = eDateFrom
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Enquiry Statistics'
  ClientHeight = 127
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
  object btnCancel: TImageListButton
    Left = 350
    Top = 96
    Width = 74
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOK: TImageListButton
    Left = 264
    Top = 96
    Width = 74
    Height = 25
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object gpMain: TGroupBox
    Left = 4
    Top = 4
    Width = 420
    Height = 85
    Caption = ' Report Parameters: '
    TabOrder = 0
    object lblOutput: TLabel
      Left = 8
      Top = 56
      Width = 54
      Height = 14
      Caption = 'Output File:'
    end
    object lblDateFrom: TLabel
      Left = 8
      Top = 24
      Width = 52
      Height = 14
      Caption = 'Date From:'
    end
    object lblDateTo: TLabel
      Left = 228
      Top = 24
      Width = 40
      Height = 14
      Caption = 'Date To:'
    end
    object eDateFrom: TVagueDateEdit
      Left = 72
      Top = 20
      Width = 131
      Height = 21
      Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object eOutputPath: TEdit
      Left = 72
      Top = 52
      Width = 318
      Height = 22
      TabOrder = 2
      OnChange = eOutputPathChange
    end
    object btnFindFile: TButton
      Left = 392
      Top = 52
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btnFindFileClick
    end
    object eDateTo: TVagueDateEdit
      Left = 280
      Top = 20
      Width = 131
      Height = 21
      Hint = '"VAGUEDATEEDIT2" is not a valid month or season name.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
end
