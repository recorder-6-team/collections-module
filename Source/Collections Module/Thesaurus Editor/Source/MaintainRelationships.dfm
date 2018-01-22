inherited frmMaintainRelationships: TfrmMaintainRelationships
  Left = 432
  Top = 360
  Caption = 'Maintain Relationships'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBrowser: TPanel
    object lbRelationships: TIDListBox
      Left = 0
      Top = 20
      Width = 289
      Height = 229
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbRelationshipsClick
    end
    object pnlListTop: TPanel
      Left = 0
      Top = 0
      Width = 289
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 4
        Width = 66
        Height = 13
        Caption = 'Relationships:'
      end
    end
  end
end
