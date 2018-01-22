inherited fraLogin: TfraLogin
  object Label1: TLabel
    Left = 20
    Top = 52
    Width = 302
    Height = 16
    Caption = 'Please select the system administrator login to use:'
  end
  object Label2: TLabel
    Left = 52
    Top = 128
    Width = 63
    Height = 16
    Caption = 'Username:'
  end
  object Label3: TLabel
    Left = 52
    Top = 160
    Width = 61
    Height = 16
    Caption = 'Password:'
  end
  object Label5: TLabel
    Left = 20
    Top = 20
    Width = 99
    Height = 16
    Caption = 'Database Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object rbTrusted: TRadioButton
    Left = 28
    Top = 76
    Width = 217
    Height = 17
    Caption = 'Use my NT Account'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbSQLClick
  end
  object rbSQL: TRadioButton
    Left = 28
    Top = 100
    Width = 245
    Height = 17
    Caption = 'Use the following SQL Server account:'
    TabOrder = 1
    OnClick = rbSQLClick
  end
  object eUsername: TEdit
    Left = 124
    Top = 124
    Width = 121
    Height = 24
    TabOrder = 2
    Text = 'sa'
  end
  object ePassword: TEdit
    Left = 124
    Top = 156
    Width = 121
    Height = 24
    PasswordChar = '*'
    TabOrder = 3
  end
end
