object dlgDiagramSettings: TdlgDiagramSettings
  Left = 381
  Top = 455
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Diagram Settings'
  ClientHeight = 186
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object btnOk: TImageListButton
    Left = 156
    Top = 154
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
    Left = 242
    Top = 154
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
    Left = 328
    Top = 154
    Width = 75
    Height = 25
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = btnApplyClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 17
  end
  object pnlAll: TPanel
    Left = 4
    Top = 6
    Width = 409
    Height = 141
    AutoSize = True
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 3
    object pnlBgrndAndGrid: TPanel
      Left = 2
      Top = 72
      Width = 405
      Height = 67
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 95
        Height = 14
        Caption = 'Background Colour:'
      end
      object Label1: TLabel
        Left = 276
        Top = 40
        Width = 47
        Height = 14
        Caption = 'Grid Size:'
      end
      object cbtnBackground: TColorButton
        Left = 108
        Top = 6
        Width = 41
        Height = 20
        ActiveColor = clWhite
        TabOrder = 0
        TabStop = True
        OnChange = cbtnBackgroundChange
      end
      object chkDisplayGrid: TCheckBox
        Left = 8
        Top = 40
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Display Grid:'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = chkDisplayGridClick
      end
      object chkSnapToGrid: TCheckBox
        Left = 156
        Top = 40
        Width = 89
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Snap To Grid:'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chkSnapToGridClick
      end
      object eGridSize: TNumberEdit
        Left = 328
        Top = 36
        Width = 49
        Height = 22
        TabOrder = 3
        Text = '4'
        OnExit = eGridSizeExit
        Maximum = 40
        DecimalPlaces = 0
      end
      object udGridSize: TUpDown
        Left = 377
        Top = 36
        Width = 15
        Height = 22
        Associate = eGridSize
        Min = 4
        Max = 40
        Position = 4
        TabOrder = 4
      end
    end
    object pnlTitle: TPanel
      Left = 2
      Top = 2
      Width = 405
      Height = 70
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 44
        Width = 64
        Height = 14
        Caption = 'Diagram Title:'
      end
      object chkDisplayTitle: TCheckBox
        Left = 8
        Top = 8
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Display Title'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkDisplayTitleClick
      end
      object eTitle: TEdit
        Left = 108
        Top = 40
        Width = 285
        Height = 22
        MaxLength = 100
        TabOrder = 1
        Text = 'New Diagram'
        OnChange = eTitleChange
      end
    end
  end
end
