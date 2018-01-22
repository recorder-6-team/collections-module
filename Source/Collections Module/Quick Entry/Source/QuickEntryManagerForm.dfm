object frmQuickEntryManager: TfrmQuickEntryManager
  Left = 327
  Top = 113
  Width = 804
  Height = 390
  AxBorderStyle = afbNone
  Caption = 'frmQuickEntryManager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  inline fraQuickEntryManager: TfraQuickEntryManager
    Left = 0
    Top = 0
    Width = 796
    Height = 363
    Align = alClient
    Constraints.MinHeight = 230
    Constraints.MinWidth = 600
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    inherited Splitter1: TSplitter
      Height = 363
    end
    inherited pnlDetails: TPanel
      Width = 532
      Height = 363
      DesignSize = (
        532
        363)
      inherited sgFields: TStringGrid
        Width = 517
        Height = 208
      end
      inherited btnSave: TImageListButton
        Left = 444
        Top = 10
        Width = 78
        Anchors = [akTop, akRight]
      end
      inherited btnCancel: TImageListButton
        Left = 443
        Top = 38
        Anchors = [akTop, akRight]
      end
      inherited btnAddMeasurement: TImageListButton
        Top = 331
      end
      inherited btnMoveUp: TImageListButton
        Top = 331
        Width = 76
      end
      inherited btnMoveDown: TImageListButton
        Left = 432
        Top = 331
        Width = 90
      end
      inherited btnAddNumber: TBitBtn
        Top = 331
      end
      inherited btnAddMetadata: TBitBtn
        Top = 331
      end
    end
    inherited pnlSelector: TPanel
      Height = 363
      inherited lbTemplates: TListBox
        Height = 300
      end
      inherited btnAdd: TImageListButton
        Top = 331
      end
      inherited btnEdit: TImageListButton
        Top = 331
      end
      inherited btnDelete: TImageListButton
        Top = 331
      end
    end
  end
end
