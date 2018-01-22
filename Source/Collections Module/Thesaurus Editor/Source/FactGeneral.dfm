inherited fraFactGeneral: TfraFactGeneral
  Width = 414
  Height = 256
  inherited bvlBorder: TBevel
    Width = 406
    Height = 248
    Anchors = [akLeft, akTop, akRight]
  end
  object Label1: TLabel
    Left = 12
    Top = 19
    Width = 22
    Height = 14
    Caption = 'Title:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblDataCaption: TLabel
    Left = 12
    Top = 76
    Width = 38
    Height = 28
    Caption = 'Data (HTML):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 12
    Top = 47
    Width = 27
    Height = 14
    Caption = 'Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 12
    Top = 171
    Width = 51
    Height = 14
    Caption = 'Language:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 12
    Top = 201
    Width = 51
    Height = 14
    Caption = 'Applies to:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 223
    Top = 171
    Width = 25
    Height = 14
    Caption = 'Date:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object eTitle: TEdit
    Left = 68
    Top = 16
    Width = 331
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object mmData: TMemo
    Left = 68
    Top = 72
    Width = 331
    Height = 88
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Lines.Strings = (
      'This is a very interesting wasp indeed.')
    ParentFont = False
    TabOrder = 2
  end
  object cmbAppliesTo: TComboBox
    Left = 68
    Top = 197
    Width = 332
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 5
    Items.Strings = (
      'All concepts that have the same meaning'
      'This concept only'
      'This version of this term'
      'All related versions of this term')
  end
  object chkInherited: TCheckBox
    Left = 9
    Top = 222
    Width = 390
    Height = 25
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This fact applies to all descendants of the selected concept in ' +
      'the hierarchy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    WordWrap = True
  end
  object eDataFile: TEdit
    Left = 68
    Top = 72
    Width = 310
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Visible = False
  end
  object cmbType: TLuxIDComboBox
    Left = 68
    Top = 44
    Width = 332
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 1
    OnChange = cmbTypeChange
    OnPopulate = cmbTypePopulate
  end
  object cmbLanguage: TLuxIDComboBox
    Left = 68
    Top = 168
    Width = 144
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 4
    OnPopulate = cmbLanguagePopulate
  end
  object eDate: TVagueDateEdit
    Left = 257
    Top = 168
    Width = 142
    Height = 22
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object btnDataFileDialog: TImageListButton
    Left = 374
    Top = 72
    Width = 25
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 8
    Visible = False
    OnClick = btnDataFileDialogClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 11
  end
  object dlgGetFilename: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp;*.gif)|*.jpg;*.jpeg;*.bmp;*.gif|JPEG Ima' +
      'ge File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*' +
      '.bmp)|*.bmp|GIF Image File (*.gif)|*.gif'
    Left = 308
    Top = 104
  end
end
