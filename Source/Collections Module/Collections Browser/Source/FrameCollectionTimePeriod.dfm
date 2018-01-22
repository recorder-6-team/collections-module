inherited fraCollectionTimePeriod: TfraCollectionTimePeriod
  object gbCollation: TGroupBox
    Left = 24
    Top = 24
    Width = 317
    Height = 61
    Caption = 'Collation Period'
    TabOrder = 0
    object lblCollationFrom: TLabel
      Left = 32
      Top = 27
      Width = 26
      Height = 13
      Caption = 'From:'
      WordWrap = True
    end
    object lblCollationTo: TLabel
      Left = 176
      Top = 27
      Width = 16
      Height = 13
      Caption = 'To:'
      WordWrap = True
    end
    object eCollationFrom: TVagueDateEdit
      Left = 64
      Top = 24
      Width = 85
      Height = 21
      Hint = 'Dates range from 01/01/1905 to 31/12/1905'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '1905'
    end
    object eCollationTo: TVagueDateEdit
      Left = 200
      Top = 24
      Width = 85
      Height = 21
      Hint = 'Dates range from 01/01/1936 to 31/12/1936'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '1936'
    end
  end
  object gbFieldGatheringPeriod: TGroupBox
    Left = 24
    Top = 96
    Width = 317
    Height = 61
    Caption = 'Field Gathering Period'
    TabOrder = 1
    object lblFieldFrom: TLabel
      Left = 32
      Top = 27
      Width = 26
      Height = 13
      Caption = 'From:'
      WordWrap = True
    end
    object lblFieldTo: TLabel
      Left = 176
      Top = 27
      Width = 16
      Height = 13
      Caption = 'To:'
      WordWrap = True
    end
    object eFieldFrom: TVagueDateEdit
      Left = 64
      Top = 24
      Width = 85
      Height = 21
      Hint = 'Dates range from 01/01/1905 to 31/12/1905'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '1905'
    end
    object eFieldTo: TVagueDateEdit
      Left = 200
      Top = 24
      Width = 85
      Height = 21
      Hint = 'Dates range from 01/01/1930 to 31/12/1930'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '1930'
    end
  end
  object gbHistoricalPeriod: TGroupBox
    Left = 24
    Top = 168
    Width = 317
    Height = 64
    Caption = 'Historical or Geological Period'
    TabOrder = 2
    object lblGeologicalFrom: TLabel
      Left = 32
      Top = 28
      Width = 26
      Height = 13
      Caption = 'From:'
      WordWrap = True
    end
    object lblGeologicalTo: TLabel
      Left = 176
      Top = 28
      Width = 16
      Height = 13
      Caption = 'To:'
      WordWrap = True
    end
    object eGeologicalFrom: TEdit
      Left = 64
      Top = 24
      Width = 85
      Height = 21
      TabOrder = 0
      Text = '500000 B.C.'
    end
    object eGeologicalTo: TEdit
      Left = 200
      Top = 24
      Width = 85
      Height = 21
      TabOrder = 1
      Text = '250000 B.C.'
    end
  end
end
