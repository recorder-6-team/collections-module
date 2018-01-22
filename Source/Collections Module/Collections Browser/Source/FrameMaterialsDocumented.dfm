inherited fraMaterialsDocumented: TfraMaterialsDocumented
  inherited shpList: TShape
    Height = 187
  end
  object Label3: TLabel [2]
    Left = 12
    Top = 239
    Width = 52
    Height = 13
    Caption = 'Document:'
  end
  inherited sgMaterials: TDSSStringGrid
    Height = 185
  end
  inherited btnAdd: TImageListButton
    Top = 200
  end
  inherited btnRemove: TImageListButton
    Top = 200
  end
  inherited eMaterial: TLinkedEdit
    TabOrder = 4
  end
  inherited cmbUnit: TConceptGroupComboBox
    TabOrder = 3
  end
  object eDocuments: TLinkedEdit
    Tag = 1
    Left = 68
    Top = 235
    Width = 284
    Height = 23
    TabOrder = 5
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnGetData = eDocumentsGetData
  end
end
