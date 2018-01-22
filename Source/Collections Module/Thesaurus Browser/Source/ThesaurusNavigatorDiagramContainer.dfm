inherited fraThesaurusNavigatorDiagramContainer: TfraThesaurusNavigatorDiagramContainer
  Width = 251
  Height = 472
  object pcNavigatorAndDiagrams: TPageControl
    Left = 0
    Top = 0
    Width = 251
    Height = 472
    ActivePage = tsNavigator
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChange = pcNavigatorAndDiagramsChange
    object tsNavigator: TTabSheet
      Caption = 'Browse'
      inline fraThesaurusNavigator: TfraThesaurusNavigator
        Left = 0
        Top = 0
        Width = 243
        Height = 446
        Align = alClient
        Constraints.MinHeight = 176
        Constraints.MinWidth = 220
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        inherited tvHierarchy: TRapidTree
          Width = 235
          Height = 363
          Data = {0400000000000000}
        end
        inherited eSearch: TEdit
          Width = 94
        end
        inherited btnGo: TButton
          Left = 192
        end
        inherited btnSearchMode: TBitBtn
          Left = 216
        end
        inherited fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
          Width = 230
          inherited cmbDomains: TComboBox
            Width = 142
          end
          inherited btnHistory: TImageListButton
            Left = 208
          end
          inherited cmbConceptGroups: TLuxIDComboBox
            Width = 120
          end
        end
        inherited pmNode: TPopupMenu
          Left = 68
          Top = 128
        end
      end
    end
  end
end
