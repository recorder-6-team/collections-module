inherited fraListViewer: TfraListViewer
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 301
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lvListViewer: TListView
      Left = 0
      Top = 0
      Width = 369
      Height = 301
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Specimens'
        end>
      MultiSelect = True
      OwnerDraw = True
      SmallImages = dmInterface.ilBrowserNodes
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = lvListViewerDblClick
      OnDrawItem = lvListViewerDrawItem
    end
  end
end
