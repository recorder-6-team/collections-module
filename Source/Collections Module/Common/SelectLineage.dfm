object dlgSelectLineage: TdlgSelectLineage
  Left = 338
  Top = 315
  Width = 577
  Height = 320
  BorderIcons = [biSystemMenu]
  Caption = 'Select Lineage'
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    569
    293)
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 524
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'The item you have selected to view ancestors for has more than o' +
      'ne lineage.  Select the lineage you wish to view:'
    WordWrap = True
  end
  object sgLineage: TStringGrid
    Left = 7
    Top = 40
    Width = 554
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultColWidth = 162
    DefaultRowHeight = 21
    RowCount = 2
    GridLineWidth = 0
    TabOrder = 0
    OnDrawCell = sgLineageDrawCell
    OnSelectCell = sgLineageSelectCell
    ColWidths = (
      43
      162
      162
      162)
  end
  object btnCancel: TImageListButton
    Left = 488
    Top = 260
    Width = 71
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOk: TImageListButton
    Left = 403
    Top = 260
    Width = 71
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object chkIncludeSynonyms: TCheckBox
    Left = 7
    Top = 264
    Width = 120
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Include Synonyms'
    TabOrder = 3
    OnClick = chkIncludeSynonymsClick
  end
end
