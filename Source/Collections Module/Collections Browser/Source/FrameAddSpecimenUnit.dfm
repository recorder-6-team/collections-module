object FrameAddSpecimen: TFrameAddSpecimen
  Left = 948
  Top = 766
  Width = 173
  Height = 135
  BorderIcons = [biSystemMenu]
  Caption = 'New Specimen'
  Color = clBtnFace
  Constraints.MaxHeight = 135
  Constraints.MaxWidth = 173
  Constraints.MinHeight = 135
  Constraints.MinWidth = 173
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnUseTaxon: TButton
    Left = 25
    Top = 25
    Width = 113
    Height = 25
    Caption = 'Use Taxon Dictionary'
    ModalResult = 6
    TabOrder = 0
  end
  object btnUseThesaurus: TButton
    Left = 25
    Top = 65
    Width = 113
    Height = 25
    Caption = 'Use Thesaurus'
    ModalResult = 7
    TabOrder = 1
  end
end
