inherited fraServerDetails: TfraServerDetails
  object Label1: TLabel
    Left = 20
    Top = 40
    Width = 293
    Height = 33
    Caption = 
      'Please select the SQL Server instance which has the database on ' +
      'it:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 20
    Top = 112
    Width = 222
    Height = 14
    Caption = 'Enter the name of the database on the server:'
  end
  object Label3: TLabel
    Left = 20
    Top = 8
    Width = 137
    Height = 18
    Caption = 'Database Selection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cmbInstances: TComboBox
    Left = 20
    Top = 72
    Width = 293
    Height = 22
    ItemHeight = 14
    TabOrder = 0
  end
  object eDBName: TEdit
    Left = 20
    Top = 128
    Width = 293
    Height = 22
    TabOrder = 1
    Text = 'NBNData'
  end
end
