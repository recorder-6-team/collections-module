object dlgSpecimenRelocation: TdlgSpecimenRelocation
  Left = 483
  Top = 336
  BorderStyle = bsDialog
  Caption = 'Specimen Relocation'
  ClientHeight = 240
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 52
    Width = 325
    Height = 149
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 36
    Top = 88
    Width = 277
    Height = 26
    AutoSize = False
    Caption = 
      'Only the specimen'#39's current location will be updated to the new ' +
      'chosen location.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 36
    Top = 156
    Width = 278
    Height = 26
    Caption = 
      'Both the current and usual location of the specimen will be upda' +
      'ted to the new chosen location.'
    WordWrap = True
  end
  object lblRelocationInfo: TLabel
    Left = 8
    Top = 12
    Width = 317
    Height = 37
    AutoSize = False
    Caption = 
      'Select the type of relocation you wish to apply to the specimen ' +
      #39'%'#39':'
    WordWrap = True
  end
  object btnOK: TImageListButton
    Left = 41
    Top = 208
    Width = 88
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 241
    Top = 208
    Width = 88
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object chkTemporary: TRadioButton
    Left = 18
    Top = 68
    Width = 299
    Height = 17
    Caption = '&Temporary relocation'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object chkPermanent: TRadioButton
    Left = 18
    Top = 136
    Width = 299
    Height = 17
    Caption = '&Permanent relocation'
    TabOrder = 1
  end
  object btnApplyToAll: TImageListButton
    Left = 141
    Top = 208
    Width = 88
    Height = 25
    Caption = 'Apply To All'
    ModalResult = 1
    TabOrder = 4
    OnClick = btnApplyToAllClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 25
  end
end
