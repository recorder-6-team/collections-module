object fraInstall: TfraInstall
  Left = 0
  Top = 0
  Width = 332
  Height = 297
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -15
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  object Label2: TLabel
    Left = 26
    Top = 60
    Width = 287
    Height = 36
    Caption = 'Installing Collections Module...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object lblInstallPhase: TLabel
    Left = 26
    Top = 104
    Width = 287
    Height = 14
    AutoSize = False
    Caption = 'Installing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object ProgressBar: TProgressBar
    Left = 24
    Top = 124
    Width = 289
    Height = 16
    TabOrder = 0
  end
end
