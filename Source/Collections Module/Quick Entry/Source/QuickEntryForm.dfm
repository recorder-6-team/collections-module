object QuickEntryForm: TQuickEntryForm
  Left = 255
  Top = 288
  Width = 676
  Height = 400
  AxBorderStyle = afbNone
  Caption = 'QuickEntryForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  inline fraQuickEntry: TfraQuickEntry
    Left = 0
    Top = 0
    Width = 668
    Height = 373
    Align = alClient
    Constraints.MinHeight = 230
    Constraints.MinWidth = 500
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    inherited pcMain: TPageControl
      Width = 668
      Height = 306
      inherited tsSpecimen: TTabSheet
        inherited sgQuickEntry: TStringGrid
          Width = 660
          Height = 227
        end
        inherited sbSpecimenControls: TScrollBox
          Width = 660
          Height = 225
        end
        inherited pnlSpecimenControls: TPanel
          Top = 227
          Width = 660
        end
      end
    end
    inherited pnlButtons: TPanel
      Top = 338
      Width = 668
      inherited btnDiscardSession: TImageListButton
        OnClick = fraQuickEntrybtnDiscardSessionClick
      end
      inherited pnlAdd: TPanel
        Left = 548
      end
    end
    inherited Panel1: TPanel
      Width = 668
    end
  end
end
