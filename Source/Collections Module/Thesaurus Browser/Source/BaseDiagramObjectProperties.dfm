object fraBaseDiagramObjectProperties: TfraBaseDiagramObjectProperties
  Left = 0
  Top = 0
  Width = 343
  Height = 394
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Bevel9: TBevel
    Left = 4
    Top = 4
    Width = 333
    Height = 385
    Shape = bsFrame
  end
  object Label11: TLabel
    Left = 12
    Top = 92
    Width = 26
    Height = 14
    Caption = 'Lines'
  end
  object Bevel5: TBevel
    Left = 40
    Top = 100
    Width = 285
    Height = 5
    Shape = bsTopLine
  end
  object Label12: TLabel
    Left = 20
    Top = 116
    Width = 34
    Height = 14
    Caption = 'Colour:'
  end
  object lblWeight: TLabel
    Left = 132
    Top = 116
    Width = 39
    Height = 14
    Caption = ' Weight:'
  end
  object Label15: TLabel
    Left = 12
    Top = 240
    Width = 21
    Height = 14
    Caption = 'Font'
  end
  object Bevel7: TBevel
    Left = 36
    Top = 248
    Width = 289
    Height = 5
    Shape = bsTopLine
  end
  object Bevel8: TBevel
    Left = 56
    Top = 316
    Width = 269
    Height = 5
    Shape = bsTopLine
  end
  object Label16: TLabel
    Left = 12
    Top = 308
    Width = 40
    Height = 14
    Caption = 'Preview'
  end
  object lblInstructions: TLabel
    Left = 12
    Top = 12
    Width = 317
    Height = 45
    AutoSize = False
    Caption = 'Instructions'
    WordWrap = True
  end
  object cbtnLineColour: TColorButton
    Left = 60
    Top = 112
    Width = 41
    Height = 20
    ActiveColor = clBlack
    TabOrder = 0
    TabStop = True
    OnChange = cbtnLineColourChange
  end
  object udLineWeight: TUpDown
    Left = 214
    Top = 112
    Width = 15
    Height = 22
    Associate = eLineWeight
    Max = 10
    Position = 1
    TabOrder = 2
  end
  object pnlFont: TPanel
    Left = 24
    Top = 264
    Width = 169
    Height = 33
    BevelOuter = bvLowered
    Caption = 'AaBbYyZz'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 3
  end
  object pnlPreview: TPanel
    Left = 12
    Top = 328
    Width = 317
    Height = 49
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
  end
  object btnChangeFont: TBitBtn
    Left = 200
    Top = 272
    Width = 81
    Height = 25
    Caption = 'Change...'
    TabOrder = 5
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
    Left = 183
    Top = 112
    Width = 31
    Height = 22
    TabOrder = 1
    Text = '1'
    OnChange = eLineWeightChange
    Maximum = 10
  end
end
