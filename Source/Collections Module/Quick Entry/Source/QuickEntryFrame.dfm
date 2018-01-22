inherited fraQuickEntry: TfraQuickEntry
  Width = 664
  Height = 370
  Font.Name = 'Arial'
  OnResize = FrameResize
  object pcMain: TPageControl
    Left = 0
    Top = 32
    Width = 664
    Height = 303
    ActivePage = tsSpecimen
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = '&General'
      DesignSize = (
        656
        274)
      object sbGeneralControls: TScrollBox
        Left = 0
        Top = 0
        Width = 655
        Height = 270
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelKind = bkTile
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          651
          266)
        object sbHiddenControls: TScrollBox
          Left = -21
          Top = -21
          Width = 471
          Height = 86
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelKind = bkTile
          BorderStyle = bsNone
          TabOrder = 0
          Visible = False
        end
      end
    end
    object tsSpecimen: TTabSheet
      Caption = 'S&pecimen'
      ImageIndex = 1
      DesignSize = (
        656
        274)
      object sgQuickEntry: TStringGrid
        Left = 0
        Top = 0
        Width = 656
        Height = 224
        Align = alClient
        ColCount = 7
        DefaultColWidth = 12
        DefaultRowHeight = 18
        RowCount = 13
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing]
        PopupMenu = pmGrid
        TabOrder = 0
        Visible = False
        OnExit = sgQuickEntryExit
        ColWidths = (
          12
          101
          84
          85
          104
          108
          97)
      end
      object sbSpecimenControls: TScrollBox
        Left = 0
        Top = 0
        Width = 656
        Height = 224
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelKind = bkTile
        BorderStyle = bsNone
        Color = clBtnFace
        ParentColor = False
        TabOrder = 1
        object lblValidated: TLabel
          Left = 8
          Top = 9
          Width = 45
          Height = 14
          Caption = 'Validated'
        end
        object chkValidated: TCheckBox
          Left = 177
          Top = 8
          Width = 15
          Height = 15
          Alignment = taLeftJustify
          TabOrder = 0
          OnClick = chkValidatedClick
        end
      end
      object pnlSpecimenControls: TPanel
        Left = 0
        Top = 224
        Width = 656
        Height = 50
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          656
          50)
        object chkAsForm: TCheckBox
          Left = 5
          Top = 2
          Width = 97
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = '&View As Form'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = chkAsFormClick
        end
        object pnlRecButtons: TPanel
          Left = 388
          Top = 16
          Width = 269
          Height = 33
          Hint = 'Control Records'
          Anchors = [akRight, akBottom]
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            269
            33)
          object lblRecordNumber: TLabel
            Left = 65
            Top = 9
            Width = 41
            Height = 13
            Alignment = taRightJustify
            Anchors = [akTop]
            Caption = 'Rec.  of '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object btnRecDel: TImageListButton
            Left = 239
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 5
            OnClick = btnRecDelClick
            ImageList = ilRecords
            ImageIndex = 5
          end
          object btnRecAdd: TImageListButton
            Left = 214
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 4
            OnClick = btnRecAddClick
            ImageList = ilRecords
            ImageIndex = 4
          end
          object btnRecLast: TImageListButton
            Left = 189
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 3
            OnClick = btnRecLastClick
            ImageList = ilRecords
            ImageIndex = 3
          end
          object btnRecNext: TImageListButton
            Left = 164
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 2
            OnClick = btnRecNextClick
            ImageList = ilRecords
            ImageIndex = 2
          end
          object btnRecPrev: TImageListButton
            Left = 139
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 1
            OnClick = btnRecPrevClick
            ImageList = ilRecords
            ImageIndex = 1
          end
          object btnRecFirst: TImageListButton
            Left = 114
            Top = 3
            Width = 25
            Height = 25
            TabOrder = 0
            OnClick = btnRecFirstClick
            ImageList = ilRecords
            ImageIndex = 0
          end
        end
        object btnValidateAll: TImageListButton
          Left = 4
          Top = 21
          Width = 116
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Validate All'
          TabOrder = 1
          OnClick = btnValidateAllClick
          ImageList = dmInterface.ilBrowserNodes
          ImageIndex = 25
        end
        object btnProcessSelected: TImageListButton
          Left = 130
          Top = 20
          Width = 116
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Pr&ocess Selected'
          TabOrder = 2
          OnClick = btnProcessSelectedClick
          ImageList = dmInterface.ilButtons
          ImageIndex = 17
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 335
    Width = 664
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      664
      35)
    object btnDiscardSession: TImageListButton
      Left = 8
      Top = 6
      Width = 116
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Discard Session'
      TabOrder = 0
      OnClick = btnDiscardSessionClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
    end
    object btnProcessSession: TImageListButton
      Left = 134
      Top = 6
      Width = 116
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'P&rocess Session'
      TabOrder = 1
      OnClick = btnProcessSessionClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object pnlAdd: TPanel
      Left = 544
      Top = 0
      Width = 120
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object btnAdd: TImageListButton
        Left = 2
        Top = 6
        Width = 110
        Height = 25
        Caption = '&Add'
        TabOrder = 0
        OnClick = btnAddClick
        ImageList = dmInterface.ilButtons
        ImageIndex = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 664
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 10
      Width = 72
      Height = 14
      Caption = 'Session Name:'
    end
    object lblLastUser: TLabel
      Left = 360
      Top = 10
      Width = 50
      Height = 14
      Caption = 'Last User:'
    end
    object eLastUser: TLabel
      Left = 416
      Top = 10
      Width = 3
      Height = 14
    end
    object eSessionName: TEdit
      Left = 84
      Top = 6
      Width = 237
      Height = 22
      MaxLength = 50
      TabOrder = 0
      OnChange = eSessionNameExit
    end
  end
  object ilRecords: TImageList
    Left = 328
    Top = 5
    Bitmap = {
      494C01010A000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7FFF7F0000
      0000000010421042000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000000000
      0000104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000000000
      104200000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042000000000000FF7F
      0000000000000000000000000000000000000000000010420000000000000000
      000000000000000000000000FF7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042000000000000FF7F
      0000FF7FFF7FFF7FFF7FFF7FFF7F000000000000000000001042000000000000
      00000000000000000000FF7F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042000000000000FF7F
      104200000000000000000000FF7F000000000000000000000000104200000000
      0000000000000000FF7F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042000000000000FF7F
      786310420000000000000000FF7F000000000000000000000000000010420000
      000000000000FF7F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010420000000000000000
      FF7F00000000000000000000FF7F000000000000000000000000000000001042
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000000000
      000000000000000010420000FF7F000000000000000000000000000000000000
      1042104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000104200000000
      000000000000104210420000FF7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010421042
      1042104210420000000010420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF7F
      FF7FFF7FFF7F0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF7F
      000000000000000000000000000000000000000000000000FF7FFF7F00000000
      000000000000FF7FFF7F00000000000000000000000000000000000010420000
      00000000FF7F0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF7F0000
      FF7F0000000000000000000000000000000000000000104200000000FF7F0000
      00000000104200000000FF7F0000000000000000000000000000000010420000
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7F00000000
      0000FF7F0000000000000000000000000000000000001042000000000000FF7F
      00001042000000000000FF7F000000000000000000000000FF7FFF7F00000000
      00000000FF7FFF7FFF7FFF7F000000000000000000001042FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000FF7F000000000000
      00000000FF7F0000000000000000000000000000000000001042000000000000
      1042000000000000FF7F00000000000000000000000010420000000000000000
      00000000000000000000FF7F0000000000000000000010420000000000000000
      000000000000000000000000FF7F000000000000000010420000000000000000
      000000000000FF7F000000000000000000000000000000000000104200000000
      000000000000FF7F000000000000000000000000000010420000000000000000
      00000000000000000000FF7F0000000000000000000010420000000000000000
      000000000000000000000000FF7F00000000000000001042000000000000FF7F
      1042000000000000FF7F00000000000000000000000000000000000010420000
      00000000FF7F0000000000000000000000000000000010420000000000000000
      00000000000000000000FF7F0000000000000000000010420000000000000000
      000000000000000000000000FF7F000000000000000000001042104210420000
      00001042000000000000FF7F0000000000000000000000000000104200000000
      000000000000FF7F000000000000000000000000000010421042104210420000
      0000000000001042104200000000000000000000000010421042104210421042
      104210421042104210421042FF7F000000000000000000000000000000000000
      000000001042000000000000FF7F000000000000000000001042000000000000
      1042000000000000FF7F00000000000000000000000000000000000010420000
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000104200000000000000000000000000001042000000000000FF7F
      00001042000000000000FF7F0000000000000000000000000000000010420000
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000001042000000000000000000000000104200000000FF7F0000
      00000000104200000000FF7F0000000000000000000000000000000010421042
      1042104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010421042000000000000000000001042104200000000
      0000000000001042104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF7FFF7FFF7F0000
      0000000000000000FF7FFF7FFF7F000000000000000000000000000000000000
      000000000000FF7FFF7FFF7F0000000000000000000000000000FF7FFF7F0000
      000000000000000000000000000000000000000000000000FF7FFF7F00000000
      0000000000000000FF7FFF7FFF7F0000000000000000104200000000FF7F0000
      00000000FF7FFF7F00000000FF7F000000000000000000000000000000000000
      0000FF7FFF7F00000000FF7F000000000000000000000000104200000000FF7F
      FF7F0000000000000000000000000000000000000000104200000000FF7FFF7F
      000000000000104200000000FF7F0000000000000000104200000000FF7F0000
      FF7FFF7F0000000000000000FF7F00000000000000000000000000000000FF7F
      FF7F0000000000000000FF7F0000000000000000000000001042000000000000
      0000FF7FFF7F0000000000000000000000000000000010420000000000000000
      FF7FFF7F0000104200000000FF7F0000000000000000104200000000FF7FFF7F
      000000000000000000000000FF7F000000000000000000000000FF7FFF7F0000
      00000000000000000000FF7F0000000000000000000000001042000000000000
      000000000000FF7FFF7F00000000000000000000000010420000000000000000
      00000000FF7F000000000000FF7F000000000000000010420000000000000000
      000000000000000000000000FF7F000000000000000000001042000000000000
      00000000000000000000FF7F0000000000000000000000001042000000000000
      00000000000000000000FF7F0000000000000000000010420000000000000000
      000000000000000000000000FF7F000000000000000010420000000000001042
      000000000000000000000000FF7F000000000000000000000000104210420000
      00000000000000000000FF7F0000000000000000000000001042000000000000
      0000000000001042104200000000000000000000000010420000000000000000
      000000001042104200000000FF7F0000000000000000104200000000FF7F0000
      104210420000000000000000FF7F000000000000000000000000000000001042
      10420000000000000000FF7F0000000000000000000000001042000000000000
      0000104210420000000000000000000000000000000010420000000000000000
      104210420000104200000000FF7F0000000000000000104200000000FF7F0000
      000000001042104200000000FF7F000000000000000000000000000000000000
      00001042104200000000FF7F0000000000000000000000001042000000001042
      1042000000000000000000000000000000000000000010420000000010421042
      000000000000104200000000FF7F000000000000000010421042104200000000
      0000000000000000104210420000000000000000000000000000000000000000
      0000000000001042104200000000000000000000000000001042104210420000
      0000000000000000000000000000000000000000000010421042104200000000
      0000000000001042104210420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFF00000000FFFFFFFF00000000
      FC1FFFFF00000000F01FFFFF00000000E07FFFFF00000000E0FFE00300000000
      C1FFC00300000000C103E00700000000C003F00F00000000C003F81F00000000
      C003FC3F00000000E003FE7F00000000F003FFFF00000000F837FFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FC3FFFFFFDFFE7CFF83FFFFFF8FFC387F83FFFFFF07FC107E007C003E03FE00F
      C007C003C01FF01FC007C003C00FF83FC007C003E307F01FC00FC003FF83E00F
      F83FFFFFFFC3C107F83FFFFFFFE3C387F87FFFFFFFF3E7CFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFE3E3FFC7F3FFE7E3C383FF07E0FFC1C3C203FC07E03FC043
      C003F007E00FC003C003E007E007C003C003F007E00FC003C203FC07E03FC043
      C383FF07E0FFC1C3C7E7FFCFE3FFC7C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object pmGrid: TPopupMenu
    Left = 440
    Top = 212
    object pmGridFillUp: TMenuItem
      Caption = 'Fill &Up'
      OnClick = pmGridFillUpClick
    end
    object pmGridFillDown: TMenuItem
      Caption = 'Fill &Down'
      OnClick = pmGridFillDownClick
    end
  end
  object pmAdd: TPopupMenu
    Left = 492
    Top = 212
    object pmAddMultiple: TMenuItem
      Caption = 'Add &Multiple...'
      OnClick = pmAddMultipleClick
    end
    object pmAddFromFile: TMenuItem
      Caption = 'Import from File...'
      OnClick = pmAddFromFileClick
    end
  end
end
