inherited fraDiagramRelationshipPropertiesForType: TfraDiagramRelationshipPropertiesForType
  inherited lblInstructions: TLabel
    Caption = 
      'For this relationship type, either use the default properties fo' +
      'r the diagram, or check the boxes to specify settings unique to ' +
      'the relationship type.'
  end
  inherited Label1: TLabel
    Width = 88
    Caption = 'Relationship Type:'
  end
  inherited cmbItem: TComboBox
    Left = 112
    Width = 217
    OnChange = cmbItemChange
  end
end
