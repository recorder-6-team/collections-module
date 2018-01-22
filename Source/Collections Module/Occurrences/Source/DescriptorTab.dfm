object DescriptorTab: TDescriptorTab
  Left = 263
  Top = 107
  Width = 373
  Height = 305
  AxBorderStyle = afbNone
  Caption = 'DescriptorTab'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline fraDescriptors: TfraDescriptors
    Left = 0
    Top = 0
    Width = 365
    Height = 277
    TabOrder = 0
    inherited bvlBorder: TBevel
      Width = 355
      Height = 265
    end
    inherited shpList: TShape
      Width = 338
      Height = 226
    end
    inherited btnAdd: TImageListButton
      Left = 304
      Top = 238
    end
    inherited btnDel: TImageListButton
      Left = 327
      Top = 238
    end
    inherited sgDescriptors: TDSSStringGrid
      Width = 336
      Height = 224
    end
  end
end
