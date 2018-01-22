inherited fraDiagramRelationshipPropertiesForRel: TfraDiagramRelationshipPropertiesForRel
  inherited lblInstructions: TLabel
    Caption = 
      'For this relationship type, either use the properties for the re' +
      'lationship type, or check the boxes to specify settings unique t' +
      'o the relationship.'
  end
  inherited Label1: TLabel
    Width = 61
    Caption = 'Relationship:'
  end
  inherited cmbItem: TComboBox
    Left = 80
    Width = 249
    OnChange = cmbItemChange
  end
end
