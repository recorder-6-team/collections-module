object dlgBaseDiagramPropertiesManager: TdlgBaseDiagramPropertiesManager
  Left = 414
  Top = 206
  Width = 358
  Height = 487
  Caption = 'Display Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TImageListButton
    Left = 92
    Top = 428
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 178
    Top = 428
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnApply: TImageListButton
    Left = 264
    Top = 428
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 2
    OnClick = btnApplyClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 17
  end
  object pcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 350
    Height = 421
    ActivePage = tsDiagram
    Align = alTop
    TabOrder = 3
    object tsDiagram: TTabSheet
      Caption = 'Diagram Properties'
    end
    object tsGroup: TTabSheet
      Caption = 'Group Properties'
      ImageIndex = 1
    end
    object tsItem: TTabSheet
      Caption = 'Item Properties'
      ImageIndex = 2
    end
  end
end
