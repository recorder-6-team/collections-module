inherited fraDiagramRelationshipProperties: TfraDiagramRelationshipProperties
  object Label14: TLabel [10]
    Left = 12
    Top = 148
    Width = 32
    Height = 14
    Caption = 'Labels'
  end
  object Bevel6: TBevel [11]
    Left = 44
    Top = 156
    Width = 281
    Height = 5
    Shape = bsTopLine
  end
  inherited pnlPreview: TPanel [12]
    Font.Color = clBlue
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    object shpBoxLeft: TShape
      Left = 8
      Top = 4
      Width = 21
      Height = 37
      Brush.Color = clCream
    end
    object shpBoxRight: TShape
      Left = 284
      Top = 4
      Width = 21
      Height = 37
      Brush.Color = clCream
    end
    object shpRelLine: TShape
      Left = 28
      Top = 24
      Width = 257
      Height = 1
      Brush.Color = clBlack
    end
    object lblForwardTerm: TLabel
      Left = 32
      Top = 8
      Width = 60
      Height = 13
      Caption = 'Parasite of'
    end
    object lblMainTerm: TLabel
      Left = 136
      Top = 8
      Width = 46
      Height = 13
      Caption = 'Parasite'
    end
    object lblReverseTerm: TLabel
      Left = 240
      Top = 28
      Width = 38
      Height = 13
      Caption = 'Host of'
    end
  end
  inherited cbtnLineColour: TColorButton [13]
  end
  inherited eLineWeight: TNumberEdit [14]
  end
  inherited udLineWeight: TUpDown [15]
  end
  inherited pnlFont: TPanel [16]
  end
  inherited btnChangeFont: TBitBtn [17]
  end
  object chkForwardTermVisible: TCheckBox
    Left = 20
    Top = 188
    Width = 189
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show Forward Relationship Term:'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = chkForwardTermVisibleClick
  end
  object chkReverseTermVisible: TCheckBox
    Left = 20
    Top = 208
    Width = 189
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show Reverse Relationship Term:'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = chkReverseTermVisibleClick
  end
  object chkMainTermVisible: TCheckBox
    Left = 20
    Top = 168
    Width = 189
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show Main Relationship Term:'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chkMainTermVisibleClick
  end
end
