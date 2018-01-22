inherited dlgDiagramRelationshipPropertiesManager: TdlgDiagramRelationshipPropertiesManager
  Caption = 'Relationship Properties'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcTabs: TPageControl
    ActivePage = tsItem
    inherited tsDiagram: TTabSheet
      inline fraDiagramRelationshipPropertiesForDiagram: TfraDiagramRelationshipPropertiesForDiagram
        Left = -1
        Top = -1
        Width = 343
        Height = 394
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    inherited tsGroup: TTabSheet
      Caption = 'Rel. Type Properties'
      inline fraDiagramRelationshipPropertiesForType: TfraDiagramRelationshipPropertiesForType
        Left = -1
        Top = -1
        Width = 343
        Height = 394
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    inherited tsItem: TTabSheet
      Caption = 'Relationship Properties'
      inline fraDiagramRelationshipPropertiesForRel: TfraDiagramRelationshipPropertiesForRel
        Left = -1
        Top = -1
        Width = 343
        Height = 394
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        inherited lblInstructions: TLabel
          Caption = 
            'For this relationship, either use the properties for the relatio' +
            'nship type, or check the boxes to specify settings unique to the' +
            ' relationship.'
        end
      end
    end
  end
end
