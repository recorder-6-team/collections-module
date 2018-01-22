inherited fraDiagramConceptPropertiesForCG: TfraDiagramConceptPropertiesForCG
  inherited lblInstructions: TLabel
    Caption = 
      'For this concept group, either use the default properties for th' +
      'e diagram, or check the boxes to specify settings unique to the ' +
      'concept group.'
  end
  inherited lblItem: TLabel
    Width = 76
    Caption = 'Concept Group:'
  end
  inherited cmbItem: TComboBox
    OnChange = cmbItemChange
  end
end
