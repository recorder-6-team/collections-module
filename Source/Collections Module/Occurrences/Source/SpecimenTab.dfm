object SpecimenTab: TSpecimenTab
  Left = 420
  Top = 278
  Width = 373
  Height = 305
  AxBorderStyle = afbNone
  Caption = 'SpecimenTab'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline fraSpecimens: TfraSpecimens
    Left = 0
    Top = 0
    Width = 365
    Height = 277
    TabOrder = 0
    inherited bvlBorder: TBevel
      Width = 355
      Height = 265
    end
    inherited shpGrid: TShape
      Width = 337
      Height = 223
    end
    inherited btnAdd: TImageListButton
      Left = 304
      Top = 238
    end
    inherited btnDel: TImageListButton
      Left = 327
      Top = 238
    end
    inherited sgSpecimens: TDSSStringGrid
      Width = 335
      Height = 221
      ColWidths = (
        330)
    end
  end
end
