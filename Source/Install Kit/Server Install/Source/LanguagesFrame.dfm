inherited fraLanguages: TfraLanguages
  object Label3: TLabel
    Left = 20
    Top = 8
    Width = 162
    Height = 18
    Caption = 'Language Preferences'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 20
    Top = 40
    Width = 285
    Height = 56
    Caption = 
      'Select the preferred languages you want to use for the common na' +
      'mes.  You can select more than one language.  Move the languages' +
      ' up or down to change their order of priority.'
    WordWrap = True
  end
  object clbLanguages: TCheckListBox
    Left = 24
    Top = 100
    Width = 185
    Height = 158
    ItemHeight = 14
    Items.Strings = (
      'Latin'
      'English'
      'French'
      'Gaelic (Scottish)'
      'Welsh'
      'German'
      'Italian'
      'Spanish'
      'Hungarian'
      'Polish'
      'Russian')
    TabOrder = 0
    OnClick = clbLanguagesClick
  end
  object btnMoveUp: TBitBtn
    Left = 220
    Top = 197
    Width = 97
    Height = 25
    Hint = 'Increase language priority.'
    Caption = 'Move &Up'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnMoveUpClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000520073006318
      84006B298C0073318C007B429400844A9C00945AA500FF00FF009C6BAD00A573
      B500AD84BD00C09ECF00C9AED000DFCCE300F3ECF400FFFFFF007FFFFFFFFFFF
      FFF7FA0000000000008FF03444444443300FF34444444444420FF34444444444
      430FF44AAAAAAAAAA30FF44AFFFFFFFFA40FF484AFFFFFFA442FF4884AFFFFA4
      442FF48844AFFA44442FF488844AA444442FF8A888844444442FF8AA88888444
      442FF8CAA8888844440FFC888444444443AF7FFFFFFFFFFFFFF7}
    Margin = 6
  end
  object btnMoveDown: TBitBtn
    Left = 220
    Top = 233
    Width = 97
    Height = 25
    Hint = 'Decrease language priority.'
    Caption = 'Move &Down'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnMoveDownClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000520073006318
      84006B298C0073318C007B429400844A9C00945AA500FF00FF009C6BAD00A573
      B500AD84BD00C09ECF00C9AED000DFCCE300F3ECF400FFFFFF007FFFFFFFFFFF
      FFF7FA0000000000008FF03444444443300FF34444444444420FF34444444444
      430FF444444AA444430FF48444AFFA44440FF4884AFFFFA4442FF488AFFFFFFA
      442FF48AFFFFFFFFA42FF48AAAAAAAAAA42FF8A888844444442FF8AA88888444
      442FF8CAA8888844440FFC888444444443AF7FFFFFFFFFFFFFF7}
    Margin = 6
  end
end
