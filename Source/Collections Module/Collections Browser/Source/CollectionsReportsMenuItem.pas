{===============================================================================
  Unit:        CollectionsReportsMenuItem

  Defines:     TCollectionsReportsMenuItem

  Description: IDynamicMenu implementation for the menu items that appear under
               the Reports menu

  Model:       CollectionBrowserGeneral.mpb

  Created:

  Last revision information:
    $Revision: 7 $
    $Date: 13/03/14 17:02 $
    $Author: Christopherknight $

===============================================================================}
unit CollectionsReportsMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsBrowser_TLB, StdVcl, Recorder2000_TLB,
  StandardReports_TLB, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for a single item on the Standard Reports sub-menu.
  }
  TCollectionsReportsMenuItem = class(TAutoObject, ICollectionsReportsMenuItem, IDynamicMenu,
      IExecuteAction)
    function IDynamicMenu.Execute = Execute;
    procedure IExecuteAction.Execute = ExecuteReport;
  private
    FCaption: String;
    FEnquiryStats: Boolean;
    FItemKey: String;
    FItemsProvider: IReportItemsProvider;
    FReportSource: Integer;
    procedure GenerateEnquiryStatsReport;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure ExecuteReport; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_Caption: WideString; safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_EnquiryStats: WordBool; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_ItemKey: WideString; safecall;
    function Get_ItemsProvider: IReportItemsProvider; safecall;
    function Get_ReportSource: Integer; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_EnquiryStats(Value: WordBool); safecall;
    procedure Set_ItemKey(const Value: WideString); safecall;
    procedure Set_ItemsProvider(const Value: IReportItemsProvider); safecall;
    procedure Set_ReportSource(Value: integer); safecall;
  end;
  
//==============================================================================
implementation

uses
  ComServ, ResourceStrings, EnquiryStats, SpecimenMailMerge, SpecimenLabel;

{-==============================================================================
    TCollectionsReportsMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Item has no children.
}
function TCollectionsReportsMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TCollectionsReportsMenuItem.Child 

{-------------------------------------------------------------------------------
  Return an IExecuteAction pointer allowing Recorder to execute the report.
}
function TCollectionsReportsMenuItem.Execute: IUnknown;
begin
  Result := Self as IExecuteAction;
end;  // TCollectionsReportsMenuItem.Execute 

{-------------------------------------------------------------------------------
  Actual report execution.  IExecuteAction.Execute implementation.
}
procedure TCollectionsReportsMenuItem.ExecuteReport;
var
  i: Integer;
begin
  // Special case #1
  if FEnquiryStats then
    GenerateEnquiryStatsReport
  else
    case FReportSource of
      // Special case #2
      TOP_LEVEL_SPECIMEN_MAIL_MERGE_OUTPUT,
      FOLDER_SPECIMEN_MAIL_MERGE_OUTPUT:
        with TdlgSpecimenMailMerge.Create(nil) do
          try
            if FReportSource = TOP_LEVEL_SPECIMEN_MAIL_MERGE_OUTPUT then
              for i := 0 to FItemsProvider.TopLevelListCount - 1 do
                AddItemKey(FItemsProvider.TopLevelListKey[i])
            else
              for i := 0 to FItemsProvider.FolderListCount - 1 do
                AddItemKey(FItemsProvider.FolderListKey[i]);
            ShowModal;
          finally
            Free;
          end;

      // Special case #3
      TOP_LEVEL_SPECIMEN_LABEL_OUTPUT,
      FOLDER_SPECIMEN_LABEL_OUTPUT:
        with TdlgSpecimenLabel.Create(nil) do
          try
            if FReportSource = TOP_LEVEL_SPECIMEN_LABEL_OUTPUT then
              for i := 0 to FItemsProvider.TopLevelListCount -1 do
                AddItemKey(FItemsProvider.TopLevelListKey[i])
            else
              for i := 0 to FItemsProvider.FolderListCount - 1 do
                AddItemKey(FItemsProvider.FolderListKey[i]);
            ShowModal;
          finally
            Free;
          end;  //Try

      ITEM_REPORT:
        // Details reports
        with CreateCOMObject(CLASS_DetailsReportGenerator) as IDetailsReportGenerator do begin
          ReportKey := FItemKey;
          ItemKey := FItemsProvider.SelectedItemKey;
          GenerateReport;
        end; // with
  
    else
      // List reports
      with CreateCOMObject(CLASS_ListReportGenerator) as IListReportGenerator do begin
        ReportKey := FItemKey;
        if FReportSource = TOP_LEVEL_REPORT then
          for i := 0 to FItemsProvider.TopLevelListCount - 1 do
            AddItemKey(FItemsProvider.TopLevelListKey[i])
        else
          // Folder list report
          for i := 0 to FItemsProvider.FolderListCount - 1 do
            AddItemKey(FItemsProvider.FolderListKey[i]);
        GenerateReport;
      end; // with
  end;
end;  // TCollectionsReportsMenuItem.ExecuteReport 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReportsMenuItem.GenerateEnquiryStatsReport;
begin
  with TfrmEnquiryStats.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // TCollectionsReportsMenuItem.GenerateEnquiryStatsReport 

{-------------------------------------------------------------------------------
  Returns data required to set up the action to appear on the menu.
}
procedure TCollectionsReportsMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    if FEnquiryStats then
      Caption := ResStr_EnquiryStats
    else
      Caption := FCaption;
    Enabled := True;
    Hint := Caption;
    ImageIndex := 0;
    ShortCut := 0;
  end;
end;  // TCollectionsReportsMenuItem.GetItemData 

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsReportsMenuItem.Get_Caption: WideString;
begin
  Result := FCaption;
end;  // TCollectionsReportsMenuItem.Get_Caption 

{-------------------------------------------------------------------------------
  Item has no children.
}
function TCollectionsReportsMenuItem.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TCollectionsReportsMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
}
function TCollectionsReportsMenuItem.Get_EnquiryStats: WordBool;
begin
  Result := FEnquiryStats;
end;  // TCollectionsReportsMenuItem.Get_EnquiryStats 

{-------------------------------------------------------------------------------
  Item has no children.
}
function TCollectionsReportsMenuItem.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TCollectionsReportsMenuItem.Get_HasSubmenu 

{-------------------------------------------------------------------------------
  Accessor method
}
function TCollectionsReportsMenuItem.Get_ItemKey: WideString;
begin
  Result := FItemKey;
end;  // TCollectionsReportsMenuItem.Get_ItemKey 

{-------------------------------------------------------------------------------
  COM Accessor method
}
function TCollectionsReportsMenuItem.Get_ItemsProvider: IReportItemsProvider;
begin
  Result := FItemsProvider;
end;  // TCollectionsReportsMenuItem.Get_ItemsProvider 

{-------------------------------------------------------------------------------
  COM Accessor method
}
function TCollectionsReportsMenuItem.Get_ReportSource: Integer;
begin
  Result := FReportSource;
end;  // TCollectionsReportsMenuItem.Get_ReportSource 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsReportsMenuItem.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;  // TCollectionsReportsMenuItem.Set_Caption 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReportsMenuItem.Set_EnquiryStats(Value: WordBool);
begin
  FEnquiryStats := Value;
end;  // TCollectionsReportsMenuItem.Set_EnquiryStats 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TCollectionsReportsMenuItem.Set_ItemKey(const Value: WideString);
begin
  FItemKey := Value;
end;  // TCollectionsReportsMenuItem.Set_ItemKey 

{-------------------------------------------------------------------------------
  COM Accessor method
}
procedure TCollectionsReportsMenuItem.Set_ItemsProvider(const Value: IReportItemsProvider);
begin
  FItemsProvider := Value;
end;  // TCollectionsReportsMenuItem.Set_ItemsProvider 

{-------------------------------------------------------------------------------
  COM Accessor method.  Sets the report type to Item (0), top level (1) or folder (2).
}
procedure TCollectionsReportsMenuItem.Set_ReportSource(Value: integer);
begin
  FReportSource := Value;
end;  // TCollectionsReportsMenuItem.Set_ReportSource 

initialization
  TAutoObjectFactory.Create(ComServer, TCollectionsReportsMenuItem,
      Class_CollectionsReportsMenuItem,
    ciMultiInstance, tmApartment);
end.
