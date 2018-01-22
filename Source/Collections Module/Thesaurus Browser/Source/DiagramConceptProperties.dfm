inherited fraDiagramConceptProperties: TfraDiagramConceptProperties
  inherited Label11: TLabel
    Width = 31
    Caption = 'Shape'
  end
  inherited Bevel5: TBevel
    Left = 44
    Width = 281
  end
  inherited Label12: TLabel
    Width = 57
    Caption = 'Line Colour:'
  end
  inherited lblWeight: TLabel
    Left = 148
  end
  inherited Label15: TLabel
    Top = 188
  end
  inherited Bevel7: TBevel
    Top = 196
  end
  inherited Bevel8: TBevel
    Top = 268
  end
  inherited Label16: TLabel
    Top = 260
  end
  object Label1: TLabel [10]
    Left = 20
    Top = 148
    Width = 49
    Height = 14
    Caption = 'Fill Colour:'
  end
  inherited pnlPreview: TPanel [11]
    Top = 280
    Height = 97
    Font.Color = clBlue
    Font.Height = -13
    ParentFont = False
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
  inherited cbtnLineColour: TColorButton [12]
    Left = 80
  end
  inherited eLineWeight: TNumberEdit [13]
    Left = 192
  end
  inherited udLineWeight: TUpDown [14]
    Left = 229
  end
  inherited pnlFont: TPanel [15]
    Top = 212
    Font.Height = -13
    Font.Style = []
  end
  inherited btnChangeFont: TBitBtn [16]
    Top = 220
  end
  object cbtnFillColour: TColorButton
    Left = 80
    Top = 144
    Width = 41
    Height = 20
    ActiveColor = clCream
    TabOrder = 6
    TabStop = True
    OnChange = cbtnFillColourChange
  end
end
