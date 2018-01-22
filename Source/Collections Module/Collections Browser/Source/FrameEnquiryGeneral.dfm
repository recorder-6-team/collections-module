inherited fraEnquiryGeneral: TfraEnquiryGeneral
  inherited bvlBorder: TBevel
    Visible = False
  end
  object gbEnquiry: TGroupBox
    Left = 4
    Top = 0
    Width = 357
    Height = 184
    TabOrder = 0
    object lblLinkedName: TLabel
      Left = 8
      Top = 15
      Width = 66
      Height = 13
      Caption = 'Linked Name:'
    end
    object Label8: TLabel
      Left = 8
      Top = 63
      Width = 65
      Height = 13
      Caption = 'Enquiry Type:'
    end
    object lblDate: TLabel
      Left = 8
      Top = 81
      Width = 45
      Height = 26
      Caption = 'Enquired Date:'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 8
      Top = 132
      Width = 56
      Height = 26
      Caption = 'Enquiry Description:'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 174
      Top = 63
      Width = 77
      Height = 13
      Caption = 'Enquiry Method:'
    end
    object lblOtherName: TLabel
      Left = 8
      Top = 39
      Width = 60
      Height = 13
      Caption = 'Other Name:'
    end
    object cmbType: TConceptGroupComboBox
      Left = 76
      Top = 60
      Width = 93
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbTypeChange
    end
    object eDate: TVagueDateEdit
      Left = 76
      Top = 84
      Width = 93
      Height = 21
      TabOrder = 4
    end
    object mmDescription: TMemo
      Left = 76
      Top = 132
      Width = 271
      Height = 45
      ScrollBars = ssVertical
      TabOrder = 7
    end
    object cmbMethod: TConceptGroupComboBox
      Left = 262
      Top = 60
      Width = 86
      Height = 21
      ItemHeight = 13
      TabOrder = 3
    end
    object eOtherName: TEdit
      Left = 76
      Top = 36
      Width = 247
      Height = 21
      TabOrder = 1
    end
    object chkMaterialLeft: TCheckBox
      Left = 6
      Top = 110
      Width = 83
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Material Left:'
      TabOrder = 5
    end
    object chkObservationRequired: TCheckBox
      Left = 172
      Top = 105
      Width = 102
      Height = 27
      Alignment = taLeftJustify
      Caption = 'Plan to create an Observation:'
      TabOrder = 6
      WordWrap = True
    end
    object eName: TUserEdit
      Tag = 1
      Left = 76
      Top = 11
      Width = 270
      Height = 23
      TabOrder = 0
      BorderStyle = bsSingle
      ImageIndex = 12
      ImageList = dmInterface.ilButtons
    end
  end
  object gbAnswered: TGroupBox
    Left = 4
    Top = 184
    Width = 357
    Height = 92
    TabOrder = 1
    object lblAnsweredBy: TLabel
      Left = 8
      Top = 16
      Width = 65
      Height = 13
      Caption = 'Answered By:'
    end
    object Label4: TLabel
      Left = 8
      Top = 41
      Width = 58
      Height = 13
      Caption = 'Department:'
    end
    object lblAnsweredDate: TLabel
      Left = 165
      Top = 67
      Width = 76
      Height = 13
      Caption = 'Date Answered:'
    end
    object lblDept: TLabel
      Left = 76
      Top = 41
      Width = 3
      Height = 13
      Constraints.MaxWidth = 269
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object chkAnswered: TCheckBox
      Left = 7
      Top = 65
      Width = 82
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Answered:'
      TabOrder = 1
    end
    object eDateAnswered: TVagueDateEdit
      Left = 253
      Top = 64
      Width = 93
      Height = 21
      TabOrder = 2
    end
    object eAnsweredBy: TUserEdit
      Tag = 1
      Left = 76
      Top = 12
      Width = 269
      Height = 23
      TabOrder = 0
      BorderStyle = bsSingle
      ImageIndex = 12
      ImageList = dmInterface.ilButtons
    end
  end
end
