object fraContainer: TfraContainer
  Left = 0
  Top = 0
  Width = 384
  Height = 483
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblHeading: TLabel
      Left = 4
      Top = 0
      Width = 67
      Height = 14
      Caption = 'Selected Item:'
      Color = clBtnFace
      ParentColor = False
    end
    object lblName: TTermLabel
      Left = 72
      Top = 0
      Width = 38
      Height = 17
      Caption = 'lblName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 454
    Width = 384
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      384
      29)
    object btnSave: TImageListButton
      Left = 230
      Top = 4
      Width = 75
      Height = 25
      Hint = 'Save'
      Anchors = [akRight, akBottom]
      Caption = '&Save'
      TabOrder = 0
      OnClick = btnSaveClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object btnCancel: TImageListButton
      Left = 309
      Top = 4
      Width = 75
      Height = 25
      Hint = 'Cancel'
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
    end
  end
end
