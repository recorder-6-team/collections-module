inherited dlgDiagramConceptPropertiesManager: TdlgDiagramConceptPropertiesManager
  Left = 488
  Top = 280
  Caption = 'Concept Properties'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcTabs: TPageControl
    ActivePage = tsItem
    inherited tsDiagram: TTabSheet
      inline fraDiagramConceptPropertiesForDiagram: TfraDiagramConceptPropertiesForDiagram
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
      inline fraDiagramConceptPropertiesForCG: TfraDiagramConceptPropertiesForCG
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
      inline fraDiagramConceptPropertiesForConcept: TfraDiagramConceptPropertiesForConcept
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
  end
end
