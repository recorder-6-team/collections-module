inherited fraConceptHistoryDetails: TfraConceptHistoryDetails
  Width = 382
  Height = 286
  inherited bvlBorder: TBevel
    Top = 8
    Width = 374
    Height = 274
    Visible = False
  end
  object gbFrom: TGroupBox
    Left = 4
    Top = 5
    Width = 374
    Height = 104
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Concept was introduced or re-introduced'
    TabOrder = 0
    DesignSize = (
      374
      104)
    object Label2: TLabel
      Left = 8
      Top = 34
      Width = 38
      Height = 13
      Caption = 'Version:'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 67
      Width = 26
      Height = 13
      Caption = 'Date:'
    end
    object eFromDate: TVagueDateEdit
      Left = 88
      Top = 63
      Width = 276
      Height = 21
      Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cmbFromVersion: TLuxIDComboBox
      Left = 88
      Top = 30
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbFromVersionChange
      OnPopulate = cmbVersionPopulate
    end
  end
  object gbTo: TGroupBox
    Left = 4
    Top = 120
    Width = 374
    Height = 104
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Concept expired'
    TabOrder = 1
    DesignSize = (
      374
      104)
    object Label3: TLabel
      Left = 8
      Top = 34
      Width = 38
      Height = 13
      Caption = 'Version:'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 8
      Top = 67
      Width = 26
      Height = 13
      Caption = 'Date:'
    end
    object eToDate: TVagueDateEdit
      Left = 88
      Top = 63
      Width = 276
      Height = 21
      Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cmbToVersion: TLuxIDComboBox
      Left = 88
      Top = 30
      Width = 275
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbToVersionChange
      OnPopulate = cmbVersionPopulate
    end
  end
end
