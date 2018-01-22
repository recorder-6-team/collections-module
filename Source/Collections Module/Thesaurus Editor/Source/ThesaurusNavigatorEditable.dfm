inherited fraThesaurusNavigatorEditable: TfraThesaurusNavigatorEditable
  inherited pnlTree: TPanel
    inherited tvHierarchy: TRapidTree
      RowCount = 0
      Data = {0400000000000000}
    end
  end
  inherited pmSearchMode: TPopupMenu
    object pmSearchModeUniqueConcepts: TMenuItem
      Caption = 'Unique Concepts'
      OnClick = pmSearchModeUniqueConceptsClick
    end
    object pmSearchModeNonSynchronisedConcepts: TMenuItem
      Caption = 'Non-Synchronised Concepts'
      OnClick = pmSearchModeNonSynchronisedConceptsClick
    end
  end
end
