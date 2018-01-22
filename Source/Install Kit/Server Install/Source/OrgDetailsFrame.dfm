inherited fraOrgDetails: TfraOrgDetails
  object Label3: TLabel
    Left = 20
    Top = 8
    Width = 144
    Height = 18
    Caption = 'Organisation Details'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 20
    Top = 48
    Width = 277
    Height = 28
    Caption = 
      'Please enter the name of the organisation which is linked to thi' +
      's installation.'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 20
    Top = 96
    Width = 30
    Height = 14
    Caption = 'Name:'
  end
  object Label4: TLabel
    Left = 20
    Top = 128
    Width = 47
    Height = 14
    Caption = 'Acronym:'
  end
  object eOrgName: TEdit
    Left = 76
    Top = 92
    Width = 229
    Height = 22
    TabOrder = 0
    OnChange = eOrgNameChange
  end
  object eAcronym: TEdit
    Left = 76
    Top = 124
    Width = 121
    Height = 22
    TabOrder = 1
  end
end
