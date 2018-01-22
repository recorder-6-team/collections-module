inherited fraDatabase: TfraDatabase
  object Label3: TLabel
    Left = 20
    Top = 8
    Width = 207
    Height = 18
    Caption = 'Database Installation Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label14: TLabel
    Left = 20
    Top = 36
    Width = 287
    Height = 42
    Caption = 
      'Before transferring data to the SQL Server, please specify the s' +
      'erver login details.  The login requires system administrator pr' +
      'ivileges on the server.'
    WordWrap = True
  end
  object Label15: TLabel
    Left = 20
    Top = 88
    Width = 284
    Height = 42
    Caption = 
      'Note that this tool must be run on the same machine as the SQL S' +
      'erver or MSDE installation itself so that the database Server ca' +
      'n find all the required data files.'
    WordWrap = True
  end
  object Label16: TLabel
    Left = 80
    Top = 176
    Width = 52
    Height = 14
    Caption = '&Username:'
    FocusControl = eUsername
  end
  object Label17: TLabel
    Left = 80
    Top = 200
    Width = 53
    Height = 14
    Caption = '&Password:'
    FocusControl = ePassword
  end
  object Label1: TLabel
    Left = 40
    Top = 136
    Width = 77
    Height = 14
    Caption = ' Server Login '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSALogin: TLabel
    Left = 74
    Top = 154
    Width = 131
    Height = 14
    Caption = 'Use the following '#39'&sa'#39' login:'
    FocusControl = rbSQLLogin
    OnClick = lblSALoginClick
  end
  object lblNTLogin: TLabel
    Left = 74
    Top = 228
    Width = 150
    Height = 14
    Caption = 'Use my &NT Account to connect'
    FocusControl = rbTrustedLogin
    OnClick = lblNTLoginClick
  end
  object rbSQLLogin: TRadioButton
    Left = 56
    Top = 154
    Width = 14
    Height = 17
    Checked = True
    Color = clWhite
    ParentColor = False
    TabOrder = 0
    TabStop = True
    OnClick = ContentChange
    OnEnter = rbSQLLoginEnter
  end
  object eUsername: TEdit
    Left = 152
    Top = 172
    Width = 137
    Height = 22
    TabOrder = 1
    Text = 'sa'
    OnChange = ContentChange
  end
  object ePassword: TEdit
    Left = 152
    Top = 196
    Width = 137
    Height = 22
    PasswordChar = '*'
    TabOrder = 2
    OnChange = ContentChange
  end
  object rbTrustedLogin: TRadioButton
    Left = 56
    Top = 228
    Width = 14
    Height = 17
    Color = clWhite
    ParentColor = False
    TabOrder = 3
    OnClick = ContentChange
  end
end
