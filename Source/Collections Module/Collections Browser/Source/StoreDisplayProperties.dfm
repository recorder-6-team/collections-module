object dlgStoreDisplayProperties: TdlgStoreDisplayProperties
  Left = 435
  Top = 216
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Store Display Properties'
  ClientHeight = 349
  ClientWidth = 341
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
  object Bevel9: TBevel
    Left = 4
    Top = 4
    Width = 333
    Height = 305
    Shape = bsFrame
  end
  object Label11: TLabel
    Left = 12
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Shape'
  end
  object Bevel5: TBevel
    Left = 44
    Top = 20
    Width = 281
    Height = 5
    Shape = bsTopLine
  end
  object Label12: TLabel
    Left = 20
    Top = 36
    Width = 56
    Height = 13
    Caption = 'Line Colour:'
  end
  object lblWeight: TLabel
    Left = 148
    Top = 36
    Width = 40
    Height = 13
    Caption = ' Weight:'
  end
  object Label15: TLabel
    Left = 12
    Top = 108
    Width = 21
    Height = 13
    Caption = 'Font'
  end
  object Bevel7: TBevel
    Left = 36
    Top = 116
    Width = 289
    Height = 5
    Shape = bsTopLine
  end
  object Bevel8: TBevel
    Left = 56
    Top = 188
    Width = 269
    Height = 5
    Shape = bsTopLine
  end
  object Label16: TLabel
    Left = 12
    Top = 180
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object Label1: TLabel
    Left = 20
    Top = 68
    Width = 48
    Height = 13
    Caption = 'Fill Colour:'
  end
  object cbtnLineColour: TColorButton
    Left = 80
    Top = 32
    Width = 41
    Height = 20
    ActiveColor = clBlack
    TabOrder = 0
    TabStop = True
    OnChange = cbtnLineColourChange
  end
  object udLineWeight: TUpDown
    Left = 223
    Top = 32
    Width = 15
    Height = 21
    Associate = eLineWeight
    Max = 10
    Position = 1
    TabOrder = 1
  end
  object pnlFont: TPanel
    Left = 24
    Top = 132
    Width = 169
    Height = 33
    BevelOuter = bvLowered
    Caption = 'AaBbYyZz'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object pnlPreview: TPanel
    Left = 12
    Top = 200
    Width = 317
    Height = 97
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 3
    object shpConceptPreview: TShape
      Left = 91
      Top = 15
      Width = 133
      Height = 65
      Brush.Color = clCream
    end
    object lblConceptLabelPreview: TLabel
      Left = 132
      Top = 40
      Width = 51
      Height = 16
      Caption = 'My Term'
      Transparent = True
    end
  end
  object btnChangeFont: TBitBtn
    Left = 200
    Top = 132
    Width = 81
    Height = 25
    Caption = 'Change...'
    TabOrder = 4
    OnClick = btnChangeFontClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000008400
      0000FF00000000008400848484000000FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00666666666660
      6006000066000644044045366635464064066350005366406406645555546640
      6406663565366664040666354536666644666663536000661100666353612466
      6214666434661200021666663666612222166666666666126216666666666661
      2216666666666666121666666666666661166666666666666616}
  end
  object eLineWeight: TNumberEdit
    Left = 192
    Top = 32
    Width = 31
    Height = 21
    TabOrder = 5
    Text = '1'
    OnChange = eLineWeightChange
    Maximum = 10
  end
  object cbtnFillColour: TColorButton
    Left = 80
    Top = 64
    Width = 41
    Height = 20
    ActiveColor = clCream
    TabOrder = 6
    TabStop = True
    OnChange = cbtnFillColourChange
  end
  object btnApply: TImageListButton
    Left = 260
    Top = 316
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 7
    OnClick = btnApplyClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 17
  end
  object btnCancel: TImageListButton
    Left = 176
    Top = 316
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOk: TImageListButton
    Left = 92
    Top = 316
    Width = 75
    Height = 25
    Caption = '&Ok'
    TabOrder = 9
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
end
