inherited frmConceptOrganiser: TfrmConceptOrganiser
  Left = 271
  Top = 174
  Width = 734
  Height = 531
  Caption = 'Concept Organiser'
  Font.Name = 'Arial'
  PixelsPerInch = 96
  TextHeight = 14
  inherited BrowserDetailsSplitter: TSplitter
    Left = 400
    Width = 4
    Height = 473
  end
  inherited pnlButtons: TPanel
    Top = 473
    Width = 726
    object Bevel1: TBevel [0]
      Left = 0
      Top = 0
      Width = 726
      Height = 5
      Align = alTop
      Shape = bsTopLine
    end
    inherited btnEdit: TImageListButton
      Visible = False
    end
    inherited btnDelete: TImageListButton
      Left = 88
    end
  end
  inherited pnlBrowser: TPanel
    Width = 400
    Height = 473
    inline fraThesaurusNavigator: TfraThesaurusNavigatorEditable
      Left = 0
      Top = 0
      Width = 400
      Height = 473
      Align = alClient
      Constraints.MinWidth = 220
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      inherited pnlTree: TPanel
        Width = 400
        Height = 400
        inherited tvHierarchy: TRapidTree
          Width = 392
          Height = 392
          OnDblClick = fraThesaurusNavigatortvHierarchyDblClick
          OnChanging = fraThesaurusNavigatortvHierarchyChanging
          OnChange = fraThesaurusNavigatortvHierarchyChange
          Data = {0400000000000000}
        end
      end
      inherited pnlTop: TPanel
        Width = 400
        inherited eSearch: TEdit
          Width = 253
        end
        inherited btnGo: TButton
          Left = 351
        end
        inherited btnSearchMode: TBitBtn
          Left = 375
        end
        inherited fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
          Width = 389
          inherited cmbDomains: TComboBox
            Width = 301
          end
          inherited btnHistory: TImageListButton
            Left = 367
          end
          inherited cmbConceptGroups: TLuxIDComboBox
            Width = 279
            OnChange = fraDomainConceptGroupsSelectorcmbConceptGroupsChange
          end
        end
      end
      inherited pmSearchMode: TPopupMenu
        inherited pmSearchModePopulateTopLevel: TMenuItem
          OnClick = fraThesaurusNavigatorpmSearchModePopulateTopLevelClick
        end
      end
      inherited pmNode: TPopupMenu
        OnPopup = fraThesaurusNavigatorpmNodePopup
        object pmNodeCut: TMenuItem [0]
          Caption = 'Cut'
          Enabled = False
          ImageIndex = 1
          ShortCut = 16472
          OnClick = pmNodeCutClick
        end
        inherited pmNodeCopy: TMenuItem
          Caption = 'Copy'
          Enabled = False
          OnClick = pmNodeCopyClick
        end
        object pmNodePaste: TMenuItem [2]
          Caption = 'Paste'
          Enabled = False
          ImageIndex = 3
          ShortCut = 16470
          OnClick = pmNodePasteClick
        end
        object pmNodePasteAsTopLevel: TMenuItem [3]
          Caption = 'Paste as Top Level'
          OnClick = pmNodePasteAsTopLevelClick
        end
        object pmNodePasteAsSynonym: TMenuItem [4]
          Caption = 'Paste as Synonym'
          OnClick = pmNodePasteAsSynonymClick
        end
        object pmNodeDelete: TMenuItem [5]
          Caption = 'Delete'
          OnClick = pmNodeDeleteClick
        end
        object N1: TMenuItem [6]
          Caption = '-'
        end
      end
    end
  end
  inherited pnlDetails: TPanel
    Left = 404
    Width = 322
    Height = 473
    inherited pnlSaveButtons: TPanel
      Top = 438
      Width = 322
      inherited pnlSaveBtnsAlign: TPanel
        Left = 147
      end
    end
  end
  object pmAdd: TPopupMenu
    Left = 32
    Top = 176
    object pmAddConcept: TMenuItem
      Caption = 'Add Concept'
      OnClick = pmAddConceptClick
    end
    object pmAddChildConcept: TMenuItem
      Caption = 'Add Child Concept'
      OnClick = pmAddChildConceptClick
    end
  end
end
