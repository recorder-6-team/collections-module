inherited fraSubjectAreaGeneral: TfraSubjectAreaGeneral
  Font.Name = 'Arial'
  ParentFont = False
  object Label1: TLabel
    Left = 12
    Top = 20
    Width = 66
    Height = 14
    Caption = 'Subject Area:'
  end
  object Label2: TLabel
    Left = 12
    Top = 52
    Width = 47
    Height = 14
    Caption = 'Comment:'
  end
  object eItemName: TEdit
    Left = 96
    Top = 16
    Width = 253
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object mmComment: TMemo
    Left = 96
    Top = 48
    Width = 253
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
