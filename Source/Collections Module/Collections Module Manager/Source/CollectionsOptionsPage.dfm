object CollectionsOptionsPage: TCollectionsOptionsPage
  Left = 637
  Top = 277
  Width = 498
  Height = 288
  AxBorderStyle = afbNone
  Caption = 'CollectionsOptionsPage'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object pcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 490
    Height = 261
    ActivePage = tsDomainFilters
    Align = alClient
    TabOrder = 0
    object tsDomainFilters: TTabSheet
      Caption = 'Domain Filters'
      inline fraFilterOptions: TfraFilterOptions
        Left = 0
        Top = 0
        Width = 482
        Height = 232
        Align = alClient
        TabOrder = 0
        inherited Label14: TLabel
          Width = 127
          Height = 14
        end
        inherited chklbDomains: TCheckListBox
          ItemHeight = 14
        end
      end
    end
    object tsNumberGenerationMacros: TTabSheet
      Caption = 'Number Generation Macros'
      ImageIndex = 1
      inline fraNumberMacroOptions: TfraNumberMacroOptions
        Left = 0
        Top = -1
        Width = 482
        Height = 233
        AutoScroll = False
        TabOrder = 0
        inherited Label15: TLabel
          Width = 129
          Height = 14
        end
        inherited Label16: TLabel
          Height = 14
        end
        inherited Label17: TLabel
          Width = 178
          Height = 14
        end
        inherited Label1: TLabel
          Width = 320
          Height = 14
        end
        inherited cmbNumberType: TComboBox
          Height = 22
          ItemHeight = 14
        end
        inherited eNumberSequenceStart: TEdit
          Height = 22
        end
      end
    end
    object tsGeneralOptions: TTabSheet
      Caption = 'General Options'
      ImageIndex = 2
      inline fraGeneralOptions: TfraGeneralOptions
        Left = 0
        Top = -1
        Width = 482
        Height = 233
        TabOrder = 0
        inherited Label1: TLabel
          Width = 152
          Height = 14
        end
        inherited Label2: TLabel
          Width = 105
          Height = 14
        end
        inherited eStandardReportTemplatePath: TEdit
          Height = 22
        end
        inherited eSpecimentImagePath: TEdit
          Height = 22
        end
      end
    end
  end
end
