inherited frmMergeData: TfrmMergeData
  Caption = 'Merge Data Items'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inline fraMergeData: TfraMergeData
    Left = 0
    Top = 0
    Width = 616
    Height = 285
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    inherited pnlInstruct: TPanel
      Width = 616
    end
    inherited pnlSource: TPanel
      Height = 127
      inherited tvSourceItem: TTreeView
        Height = 125
      end
    end
    inherited pnlDest: TPanel
      Width = 437
      Height = 127
      inherited tvTargetItem: TTreeView
        Width = 435
        Height = 125
      end
    end
    inherited pnlButtons: TPanel
      Top = 248
      Width = 616
    end
  end
end
