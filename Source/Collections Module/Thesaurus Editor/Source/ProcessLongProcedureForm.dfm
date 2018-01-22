object frmProcessLongProcedure: TfrmProcessLongProcedure
  Left = 593
  Top = 256
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Processing'
  ClientHeight = 84
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    252
    84)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 236
    Height = 68
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lblMessage: TLabel
    Left = 20
    Top = 20
    Width = 54
    Height = 13
    Caption = 'Moving %s.'
  end
  object lblTime: TLabel
    Left = 20
    Top = 48
    Width = 198
    Height = 13
    Caption = 'Please wait, this may take several minutes'
  end
  object tmrShowDots: TTimer
    OnTimer = tmrShowDotsTimer
    Left = 192
    Top = 8
  end
end
