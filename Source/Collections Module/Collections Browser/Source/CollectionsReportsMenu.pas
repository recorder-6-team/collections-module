{===============================================================================
  Unit:        CollectionsReportsMenu

  Defines:     TCollectionsReportsMenu

  Description: IDynamicMenu implementation for the top level item for the
               Standard Reports sub-menu.

  Model:       CollectionBrowserGeneral.mpb

  Created:

  Last revision information:
    $Revision: 10 $
    $Date: 13/03/14 17:02 $
    $Author: Christopherknight $

===============================================================================}
unit CollectionsReportsMenu;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsBrowser_TLB, StdVcl, Recorder2000_TLB, AdoDb, SysUtils,
  LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the top level item for the Standard Reports sub-menu.
  }
  TCollectionsReportsMenu = class(TAutoObject, ICollectionsReportsMenu, IDynamicMenu)
  private
    FFolderListCount: Integer;
    FFolderListTable: String;
    FItemsProvider: IReportItemsProvider;
    FRecPos: Integer;
    FReportList: _Recordset;
    FSelectedItemKey: String;
    FSelectedItemTable: String;
    FTopLevelListCount: Integer;
    FTopLevelListTable: String;
    procedure CheckQuery;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_ItemsProvider: IReportItemsProvider; safecall;
    procedure Set_ItemsProvider(const Value: IReportItemsProvider); safecall;
  end;
  
//==============================================================================
implementation

uses 
  ComServ, ResourceStrings, GeneralData;

{-==============================================================================
    TCollectionsReportsMenu
===============================================================================}
{-------------------------------------------------------------------------------
  Checks that the current report list query is up to date (i.e. the last used parameters are
      correct).  If not, then refreshes it or loads it for the first time.
}
procedure TCollectionsReportsMenu.CheckQuery;
begin
  // If report list query has been closed, then force it to be rebuilt
  if Assigned(FReportList) then
    if FReportList.State=0 then
      FReportList := nil;
  if not Assigned(FReportList) or
     (FSelectedItemTable<>FItemsProvider.SelectedItemTable) or
     (FSelectedItemKey<>FItemsProvider.SelectedItemKey) or
     (FTopLevelListTable<>FItemsProvider.TopLevelListTable) or
     (FTopLevelListCount<>FItemsProvider.TopLevelListCount) or
     (FFolderListTable<>FItemsProvider.FolderListTable) or
     (FFolderListCount<>FItemsProvider.FolderListCount) then
  begin
    FReportList := dmGeneral.GetRecordset('usp_AvailableReports_Select', [
                       '@SelectedItemKey', FItemsProvider.SelectedItemKey,
                       '@SelectedItemTable', FItemsProvider.SelectedItemTable,
                       '@TopLevelTable', FItemsProvider.TopLevelListTable,
                       '@TopLevelListCount', FItemsProvider.TopLevelListCount,
                       '@FolderTable', FItemsProvider.FolderListTable,
                       '@FolderListCount', FItemsProvider.FolderListCount
                       ]);
    FRecPos := 0;

    // It's all very well to check if these have changed, but you also need to UPDATE THEM
    // for next time arround!!!!!
    FSelectedItemTable := FItemsProvider.SelectedItemTable;
    FSelectedItemKey   := FItemsProvider.SelectedItemKey;
    FTopLevelListTable := FItemsProvider.TopLevelListTable;
    FTopLevelListCount := FItemsProvider.TopLevelListCount;
    FFolderListTable   := FItemsProvider.FolderListTable;
    FFolderListCount   := FItemsProvider.FolderListCount;
  end;
end;  // TCollectionsReportsMenu.CheckQuery 

{-------------------------------------------------------------------------------
  Return the children, i.e. the reports that are appropriate to the selection in the
      Collections Browser.
}
function TCollectionsReportsMenu.Child(Index: Integer): IUnknown;
var
  lstReportType: String;
begin
  CheckQuery;
  Result := CreateComObject(CLASS_CollectionsReportsMenuItem);
  // last report in list is Enquiry Stats
  if Index >= FReportList.RecordCount then
    // Specimen is top level
    if (Index = FReportList.RecordCount) and
       SameText(FItemsProvider.TopLevelListTable, TN_SPECIMEN_UNIT) then
    begin
      with Result as ICollectionsReportsMenuItem do begin
        Caption       := ResStr_SpecimensMailMergeOutput + ResStr_ForTopLevel;
        ItemsProvider := FItemsProvider;
        ReportSource  := TOP_LEVEL_SPECIMEN_MAIL_MERGE_OUTPUT;
      end;
    end
    else
    // Specimen is folder or leaf
    if (Index = FReportList.RecordCount) and
       SameText(FItemsProvider.FolderListTable, TN_SPECIMEN_UNIT) then
    begin
      with Result as ICollectionsReportsMenuItem do begin
        Caption       := ResStr_SpecimensMailMergeOutput + ResStr_ForFolder;
        ItemsProvider := FItemsProvider;
        ReportSource  := FOLDER_SPECIMEN_MAIL_MERGE_OUTPUT;
      end;
    end
    else
    // Specimen is top level
    if (Index = FReportList.RecordCount+1) and
       SameText(FItemsProvider.TopLevelListTable, TN_SPECIMEN_UNIT) then
    begin
      with Result as ICollectionsReportsMenuItem do
        begin
          Caption := ResStr_SpecimensLabelOutput + ResStr_ForTopLevel;
          ItemsProvider := FItemsProvider;
          ReportSource := TOP_LEVEL_SPECIMEN_LABEL_OUTPUT;
        end;
    end
    else
    // Specimen is folder or leaf
    if (Index = FReportList.RecordCount+1) and
       SameText(FItemsProvider.FolderListTable, TN_SPECIMEN_UNIT) then
    begin
      with Result as ICollectionsReportsMenuItem do
        begin
          Caption := ResStr_SpecimensLabelOutPut + ResStr_ForFolder;
          ItemsProvider := FItemsProvider;
          ReportSource := FOLDER_SPECIMEN_LABEL_OUTPUT;
        end;
    end
    else
      // No specimen, or Index beyond RecordCount, always set EnquiryStats
      (Result as ICollectionsReportsMenuItem).EnquiryStats := True
  else begin
    // Ensure the recordset is on correct row
    if FRecPos > Index then begin
      FReportList.MoveFirst;
      FRecPos := 0;
    end;
    while FRecPos < Index do begin
      FReportList.MoveNext;
      Inc(FRecPos);
    end;
    with Result as ICollectionsReportsMenuItem do begin
      if SameText(FReportList.Fields['ReportType'].Value, 'TopLevelList') then
        lstReportType := ResStr_ForTopLevel
      else if SameText(FReportList.Fields['ReportType'].Value, 'FolderList') then
        lstReportType := ResStr_ForFolder
      else
        lstReportType := '';
  
      ItemKey       := FReportList.Fields['Item_Key'].Value;
      Caption       := FReportList.Fields['Item_Name'].Value + lstReportType;
      ItemsProvider := FItemsProvider;
      ReportSource  := FReportList.Fields['Report_Source'].Value;
    end;
  end;
end;  // TCollectionsReportsMenu.Child 

{-------------------------------------------------------------------------------
  Menu contains a sub-menu so no execution required.
}
function TCollectionsReportsMenu.Execute: IUnknown;
begin
  Result := nil;
end;  // TCollectionsReportsMenu.Execute 

{-------------------------------------------------------------------------------
  Initialise the action to be created.
}
procedure TCollectionsReportsMenu.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := ResStr_MnuCollectionsReports;
    Enabled := True;
    Hint := Caption;
    ImageIndex := 0;
  end;
end;  // TCollectionsReportsMenu.GetItemData 

{-------------------------------------------------------------------------------
  Return the number of available reports according to the selection in the Collections Browser.
}
function TCollectionsReportsMenu.Get_ChildCount: Integer;
begin
  CheckQuery;
  // Extra one for Enquiry Stats which is always present (so long as the
  // ReportList is open)
  if (FReportList.State <> 0) then begin
    Result := FReportList.RecordCount + 1;
    if SameText(FItemsProvider.TopLevelListTable, TN_SPECIMEN_UNIT) or
       SameText(FItemsProvider.FolderListTable, TN_SPECIMEN_UNIT) then
      Inc(Result, 2);
  end else
    Result := 0;
end;  // TCollectionsReportsMenu.Get_ChildCount 

{-------------------------------------------------------------------------------
  The reports menu has a sub-menu.
}
function TCollectionsReportsMenu.Get_HasSubmenu: WordBool;
begin
  Result := True;
end;  // TCollectionsReportsMenu.Get_HasSubmenu 

{-------------------------------------------------------------------------------
  Return the interface responsible for providing details on the items that are currently
      reportable.
}
function TCollectionsReportsMenu.Get_ItemsProvider: IReportItemsProvider;
begin
  Result := FItemsProvider;
end;  // TCollectionsReportsMenu.Get_ItemsProvider 

{-------------------------------------------------------------------------------
  COM Accessor method
}
procedure TCollectionsReportsMenu.Set_ItemsProvider(const Value: IReportItemsProvider);
begin
  FItemsProvider := Value;
end;  // TCollectionsReportsMenu.Set_ItemsProvider 

initialization
  TAutoObjectFactory.Create(ComServer, TCollectionsReportsMenu,
      Class_CollectionsReportsMenu,
    ciMultiInstance, tmApartment);
end.
