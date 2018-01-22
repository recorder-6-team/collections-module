inherited fraDiagramConceptPropertiesForConcept: TfraDiagramConceptPropertiesForConcept
  inherited lblInstructions: TLabel
    Caption = 
      'For this concept, either use the default properties for the conc' +
      'ept group, or check the boxes to specify settings unique to the ' +
      'concept.'
  end
  inherited lblItem: TLabel
    Width = 43
    Caption = 'Concept:'
  end
  inherited cmbItem: TComboBox
    OnChange = cmbItemChange
  end
end
