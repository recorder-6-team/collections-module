inherited fraListOrganiserTree: TfraListOrganiserTree
  Width = 365
  Height = 297
  inherited pnlTree: TPanel
    Width = 365
    Height = 297
    inherited tvHierarchy: TRapidTree
      Width = 357
      Height = 289
      RightClickSelect = True
      PopupMenu = pmCutPaste
      OnExpanding = tvHierarchyExpanding
      Data = {0400000000000000}
    end
  end
  object pmCutPaste: TPopupMenu
    OnPopup = pmCutPasteOnPopup
    Left = 124
    Top = 96
    object pmCutPasteCut: TMenuItem
      Caption = 'Cut'
      OnClick = pmCutPasteCutOnClick
    end
    object pmCutPastePaste: TMenuItem
      Caption = 'Paste'
      OnClick = pmCutPastePasteOnClick
    end
  end
end
