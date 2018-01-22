inherited frmListOrganiser: TfrmListOrganiser
  Left = 402
  Top = 226
  Width = 699
  Height = 384
  Caption = 'List Organiser'
  Font.Name = 'Arial'
  PixelsPerInch = 96
  TextHeight = 14
  inherited BrowserDetailsSplitter: TSplitter
    Height = 319
  end
  inherited pnlButtons: TPanel
    Top = 319
    Width = 691
    object Bevel1: TBevel [0]
      Left = 0
      Top = 0
      Width = 691
      Height = 17
      Align = alTop
      Shape = bsTopLine
    end
  end
  inherited pnlBrowser: TPanel
    Height = 319
    inline fraListOrganiserTree: TfraListOrganiserTree
      Left = 0
      Top = 0
      Width = 289
      Height = 319
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      inherited pnlTree: TPanel
        Width = 289
        Height = 319
        inherited tvHierarchy: TRapidTree
          Width = 281
          Height = 311
          OnChange = tvHierarchyChange
          Images = dmMain.ilHierarchy
          Data = {0400000000000000}
        end
      end
    end
  end
  inherited pnlDetails: TPanel
    Width = 400
    Height = 319
    inherited pnlSaveButtons: TPanel
      Top = 284
      Width = 400
      inherited pnlSaveBtnsAlign: TPanel
        Left = 225
      end
    end
  end
  object pmAdd: TPopupMenu
    Left = 20
    Top = 28
    object pmAddSubjectArea: TMenuItem
      Action = actAddSubjectArea
    end
    object pmAddDomain: TMenuItem
      Action = actAddDomain
    end
    object pmAddLocalDomain: TMenuItem
      Action = actAddLocalDomain
    end
    object pmAddConceptGroup: TMenuItem
      Action = actAddConceptGroup
    end
    object pmAddConceptGroupVersion: TMenuItem
      Action = actAddConceptGroupVersion
    end
  end
  object alDomainsAndConceptGroups: TActionList
    Left = 112
    Top = 28
    object actAddSubjectArea: TAction
      Category = 'AddActions'
      Caption = 'Add Subject Area'
      ImageIndex = 0
      OnExecute = actAddSubjectAreaExecute
    end
    object actAddDomain: TAction
      Category = 'AddActions'
      Caption = 'Add Domain'
      ImageIndex = 1
      OnExecute = actAddDomainExecute
    end
    object actAddLocalDomain: TAction
      Category = 'AddActions'
      Caption = 'Add Local Domain'
      ImageIndex = 2
      OnExecute = actAddLocalDomainExecute
    end
    object actAddConceptGroup: TAction
      Category = 'AddActions'
      Caption = 'Add Concept Group'
      ImageIndex = 3
      OnExecute = actAddConceptGroupExecute
    end
    object actAddConceptGroupVersion: TAction
      Category = 'AddActions'
      Caption = 'Add Concept Group Version'
      ImageIndex = 4
      OnExecute = actAddConceptGroupVersionExecute
    end
    object actAddChildDomain: TAction
      Category = 'AddActions'
      Caption = 'Add Child Domain'
      ImageIndex = 1
    end
  end
end
