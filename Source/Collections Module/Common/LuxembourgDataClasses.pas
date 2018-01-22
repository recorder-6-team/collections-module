{===============================================================================
  Unit:        LuxembourgDataClasses

  Defines:

  Description:

  Model:       CollectionBrowserFramework.mpb

  Created:     September 2003

  Last revision information:
    $Revision: 26 $
    $Date: 24/05/11 16:45 $
    $Author: Jamesbichard $

===============================================================================}
unit LuxembourgDataClasses;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Contnrs,
  DataClasses, Grids, ADODB, ADOInt, ResourceStrings, DataTypes, Variants, DssStringGrid,
  ComboListID, LinkedControls, StdCtrls;

type
  ELuxCachedDataError = class (Exception)
  end;
  
  { Forward declaration so TDataItem can hold its owner }
  TLuxCachedDataList = class;

  TCustomGetData = procedure (WinControl: TWinControl; var Text, Key: String) of object;
  TCustomSetData = procedure (WinControl: TWinControl; const Text, Key: String) of object;
  {-----------------------------------------------------------------------------
    Abstract base class for data items used with the TLuxCachedDataList class.  This is used 
    when a list of items is populated on a form, which can be edited but must be cached until 
    the 'Save' or OK button is clicked.
  }
  TLuxCachedDataItem = class (TObject)
  private
    FAdded: Boolean;
    FCustodian: String;
    FDeleted: Boolean;
    FIgnore: Boolean;
    FItemKey: TKeyString;
    FModified: Boolean;
    FOwnerList: TLuxCachedDataList;
    FTimestamp: TSQLSvrTimestamp;
    FHidden: Boolean;
  protected
    procedure InitFromRecord(AFields: Fields); virtual; abstract;
    procedure SetDeleted(const Value: Boolean);
    procedure SetItemKey(const Value: TKeyString); virtual;
    procedure SetModified; virtual;
    procedure SetHidden(const Value: Boolean); virtual;
    procedure ValidateData; virtual;
    property Ignore: Boolean read FIgnore write FIgnore;
  public
    constructor CreateFromRecord(AOwner: TLuxCachedDataList; AFields: Fields); virtual;
    constructor CreateNew(AOwner: TLuxCachedDataList); virtual;
    property Added: Boolean read FAdded;
    property Custodian: String read FCustodian;
    property Deleted: Boolean read FDeleted write SetDeleted;
    property ItemKey: TKeyString read FItemKey;
    property Modified: Boolean read FModified;
    property OwnerList: TLuxCachedDataList read FOwnerList;
    property Timestamp: TSQLSvrTimestamp read FTimestamp;
    property Hidden: Boolean read FHidden write SetHidden;
  end;
  
  { Metaclass to identify the items so that the list class can create them }
  TItemClass = class of TLuxCachedDataItem;

  {-----------------------------------------------------------------------------
    Abstract base class for managing a list of TLuxCachedDataItems
  }
  TLuxCachedDataList = class (TObjectList)
  private
    FChanged: Boolean;
    FItemClass: TItemClass;
    FItemsToAdd: TList;
    FItemsToDelete: TList;
    FItemsToModify: TList;
    FMasterKey: TKeyString;
    function GetDataItems(const Key: TKeyString): TLuxCachedDataItem;
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TLuxCachedDataItem;
    procedure PrepareUpdateLists;
    procedure SetLists;
  protected
    procedure AddToList(AItem: TLuxCachedDataItem; AIndex: integer); virtual;
    function CanAddItemToList(AItem: TLuxCachedDataItem): Boolean; virtual;
    procedure DoAddition(AItem: TLuxCachedDataItem); virtual;
    procedure DoDeletion(AItem: TLuxCachedDataItem); virtual;
    procedure DoModification(AItem: TLuxCachedDataItem); virtual;
    function GetRecordset: _Recordset; virtual; abstract;
    procedure PopulateFromRecordset;
    property ItemsToAdd: TList read FItemsToAdd;
    property ItemsToDelete: TList read FItemsToDelete;
    property ItemsToModify: TList read FItemsToModify;
  public
    constructor Create(AItemClass: TItemClass); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure AddFromRecordset(AFields: Fields);
    procedure AddNew(AItem: TLuxCachedDataItem); virtual;
    procedure DeleteItem(const AIndex: integer); virtual;
    procedure Refresh; virtual;
    procedure Update; virtual;
    procedure ValidateContent; virtual;
    property Changed: Boolean read FChanged;
    property DataItems[const Key: TKeyString]: TLuxCachedDataItem read GetDataItems;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TLuxCachedDataItem read GetItems;
    property MasterKey: TKeyString read FMasterKey write FMasterKey;
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStrings instance (eg on a listbox 
    component).
  }
  TLuxStringDataItem = class (TLuxCachedDataItem)
  protected
    procedure SetModified; override;
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStrings instance (eg on a listbox 
    component). Also partly abstract.
  }
  TLuxStringDataList = class (TLuxCachedDataList)
  private
    FStrings: TStrings;
  protected
    procedure AddToList(AItem: TLuxCachedDataItem; AIndex: Integer); override;
    function GetText(AItem: TLuxCachedDataItem): String; virtual; abstract;
  public
    constructor Create(AItemClass: TItemClass; AStrings: TStrings); reintroduce; virtual;
    procedure DeleteItem(const AIndex: Integer); override;
    procedure Refresh; override;
    procedure RefreshItemDisplay(AItem: TLuxStringDataItem);
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStringGrid component. Also partly 
    abstract.
  }
  TLuxGridDataItem = class (TLuxCachedDataItem)
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); virtual;
        abstract;
    function GetDisplayText(Column: Integer): String; virtual;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); 
        virtual;
    procedure SetModified; override;
  public
    property DisplayText[Column: Integer]: String read GetDisplayText;
  end;
  
  {-----------------------------------------------------------------------------
    Sub-class which keep the list in synch with an existing TStringGrid component.
  }
  TLuxGridDataList = class (TLuxCachedDataList)
  private
    FGrid: TDSSStringGrid;
    FGridKeyDown: TKeyEvent;
    FGridRowMoved: TMovedEvent;
    FOnCustomGetData: TCustomGetData;
    FOnCustomSetData: TCustomSetData;
    procedure DoCustomGetData(AWinControl: TWinControl; var Text, Key: String);
    procedure DoCustomSetData(AWinControl: TWinControl; const Text, Key: String);
    function GetItemAtRow(const ARow: Integer): TLuxGridDataItem;
    procedure GridCellCustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState; 
        AWinControl: TWinControl);
    procedure GridCellLeaveCustom(Sender: TObject; ACol, ARow: Integer; WinControl:
        TWinControl);
    procedure GridCellLeaveDefault(Sender: TObject; ACol, ARow: Integer; var Options: 
        TGridOptions);
    procedure GridCellSelectedCustom(Sender: TObject; ACol, ARow: Integer; WinControl: 
        TWinControl);
    procedure GridCellSelectedDefault(Sender: TObject; ACol, ARow: Integer; var Options: 
        TGridOptions);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
  protected
    procedure AddToList(AItem: TLuxCachedDataItem; AIndex: Integer); override;
    function AllowedAddOnKeyDown: Boolean; virtual;
    property Grid: TDSSStringGrid read FGrid;
  public
    constructor Create(AItemClass: TItemClass; AStringGrid: TDSSStringGrid); reintroduce; 
        virtual;
    procedure AddNew(AItem: TLuxCachedDataItem); override;
    procedure DeleteItem(const AIndex: integer); override;
    procedure Refresh; override;
    procedure RefreshItemDisplay(AItem: TLuxGridDataItem);
    procedure ValidateContent; override;
    procedure UpdateGrid;
    property OnCustomGetData: TCustomGetData read FOnCustomGetData write FOnCustomGetData;
    property OnCustomSetData: TCustomSetData read FOnCustomSetData write FOnCustomSetData;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TLuxCachedDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLuxCachedDataItem.CreateFromRecord(AOwner: TLuxCachedDataList; AFields: Fields);
begin
  inherited Create;
  FOwnerList := AOwner;
  // Not a newly created record
  FAdded := False;
  // Initialise it
  InitFromRecord(AFields);
  // It may happen this field is null (see metadata).
  FCustodian := VarToStr(AFields['Custodian'].Value);
  FTimestamp := AFields['Timestamp'].Value;
  FModified := False;
  FOwnerList.FChanged := True;
  FHidden := False;
end;  // TLuxCachedDataItem.CreateFromRecord 

{-------------------------------------------------------------------------------
}
constructor TLuxCachedDataItem.CreateNew(AOwner: TLuxCachedDataList);
begin
  inherited Create;
  FOwnerList := AOwner;
  // A newly created record which we need to post into the dataset
  FAdded := True;
  FOwnerList.FChanged := True;
end;  // TLuxCachedDataItem.CreateNew 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataItem.SetDeleted(const Value: Boolean);
begin
  FDeleted := Value;
  // As both classes are in same unit, private fields can be accessed.
  FOwnerList.FChanged := True;
end;  // TLuxCachedDataItem.SetDeleted 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataItem.SetItemKey(const Value: TKeyString);
begin
  if FItemKey <> Value then begin
    FItemKey := Value;
    SetModified;
  end;
end;  // TLuxCachedDataItem.SetItemKey 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataItem.SetModified;
begin
  FModified := True;
  FOwnerList.FChanged := True;
end;  // TLuxCachedDataItem.SetModified

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataItem.SetHidden(const Value: Boolean);
begin
  FHidden := Value;
end;

{-------------------------------------------------------------------------------
  Override when and where necessary.
}
procedure TLuxCachedDataItem.ValidateData;
begin
end;  // TLuxCachedDataItem.ValidateData 

{-==============================================================================
    TLuxCachedDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLuxCachedDataList.Create(AItemClass: TItemClass);
begin
  inherited Create;
  
  FItemClass      := AItemClass;
  FChanged        := False;
end;  // TLuxCachedDataList.Create 

{-------------------------------------------------------------------------------
}
destructor TLuxCachedDataList.Destroy;
begin
  Clear;
  FItemsToAdd.Free;
  FItemsToModify.Free;
  FItemsToDelete.Free;
  inherited Destroy;
end;  // TLuxCachedDataList.Destroy 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.AddFromRecordset(AFields: Fields);
var
  lDataItem: TLuxCachedDataItem;
begin
  lDataItem := FItemClass.CreateFromRecord(Self, AFields);
  AddNew(lDataItem);
end;  // TLuxCachedDataList.AddFromRecordset 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.AddNew(AItem: TLuxCachedDataItem);
begin
  if AItem <> nil then
    AddToList(AItem, -1)
  else
    raise EDataListError.Create(ResStr_CannotAddNilItem);
end;  // TLuxCachedDataList.AddNew 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.AddToList(AItem: TLuxCachedDataItem; AIndex: integer);
begin
  if CanAddItemToList(AItem) then
    if AIndex <> -1 then
      Insert(AIndex, AItem)
    else
      Add(AItem);
end;  // TLuxCachedDataList.AddToList

{-------------------------------------------------------------------------------
}
function TLuxCachedDataList.CanAddItemToList(AItem: TLuxCachedDataItem): Boolean;
begin
  Result := True;
end; // TLuxCachedDataList.CanAddItemToList

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.DeleteItem(const AIndex: integer);
begin
  TLuxCachedDataItem(Items[AIndex]).Deleted := True;
end;  // TLuxCachedDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.DoAddition(AItem: TLuxCachedDataItem);
begin
  // If sub-class need to save items, override this method.
end;  // TLuxCachedDataList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  // If sub-class need to delete items, override this method.
end;  // TLuxCachedDataList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.DoModification(AItem: TLuxCachedDataItem);
begin
  // If sub-class need to modify items, override this method.
end;  // TLuxCachedDataList.DoModification 

{-------------------------------------------------------------------------------
}
function TLuxCachedDataList.GetDataItems(const Key: TKeyString): TLuxCachedDataItem;
var
  i: Integer;
begin
  Result := nil; // default value is not found
  for i := 0 to Count - 1 do
    if TLuxCachedDataItem(Items[i]).ItemKey = Key then begin
      Result := TLuxCachedDataItem(Items[i]);
      Exit; // from the loop
    end;
end;  // TLuxCachedDataList.GetDataItems 

{-------------------------------------------------------------------------------
}
function TLuxCachedDataList.GetItemCount: Integer;
var
  i, lTotal: Integer;
begin
  lTotal := 0;
  for i := 0 to Count - 1 do
    if not TLuxCachedDataItem(Items[i]).Deleted then Inc(lTotal);
  Result := lTotal;
end;  // TLuxCachedDataList.GetItemCount 

{-------------------------------------------------------------------------------
}
function TLuxCachedDataList.GetItems(Index: Integer): TLuxCachedDataItem;
begin
  Result := TLuxCachedDataItem(inherited Items[Index]);
end;  // TLuxCachedDataList.GetItems 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.PopulateFromRecordset;
var
  lDataItem: TLuxCachedDataItem;
begin
  { Get the list of items from the dataset }
  with GetRecordset do begin
    while not Eof do Begin
      lDataItem := FItemClass.CreateFromRecord(Self, Fields);
      AddToList(lDataItem, -1);
      MoveNext;
    end;
    Close;
  end;
end;  // TLuxCachedDataList.PopulateFromRecordset 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.PrepareUpdateLists;
begin
  if FItemsToAdd = nil then FItemsToAdd := TList.Create
                       else FItemsToAdd.Clear;
  
  if FItemsToModify = nil then FItemsToModify := TList.Create
                          else FItemsToModify.Clear;
  
  if FItemsToDelete = nil then FItemsToDelete := TList.Create
                          else FItemsToDelete.Clear;
end;  // TLuxCachedDataList.PrepareUpdateLists 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.Refresh;
begin
  //Free existing items
  Clear;
  FChanged := False;
  //Get new items
  PopulateFromRecordset;
end;  // TLuxCachedDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.SetLists;
var
  i: Integer;
  lItem: TLuxCachedDataItem;
begin
  PrepareUpdateLists;
  // Iterate through each item checking the state - what do we have to do?
  for i := 0 to Count - 1 do
  begin
    lItem := TLuxCachedDataItem(Items[i]);
    if (lItem.Added and lItem.Deleted) or (lItem.Ignore) then
      Continue;  // next iteration - this item never existed in the first place
  
    // Deletion has the highest priority!
    if lItem.Deleted then FItemsToDelete.Add(lItem) else
    // Addition before modification - as long as we add the most recent copy
    if lItem.Added then FItemsToAdd.Add(lItem) else
    // Finally modifications
    if lItem.Modified then FItemsToModify.Add(lItem);
  end; // for
end;  // TLuxCachedDataList.SetLists 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.Update;
var
  i: Integer;
  lCursor: TCursor;
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    // ValidateContent sets the lists.
    ValidateContent;
  
    // If we get here, all should be fine.
    for i := 0 to FItemsToAdd.Count -1 do
      DoAddition(FItemsToAdd[i]);
  
    for i := 0 to FItemsToModify.Count - 1 do
      DoModification(FItemsToModify[i]);
  
    for i := 0 to FItemsToDelete.Count - 1 do
      DoDeletion(FItemsToDelete[i]);
  
    // All items properly added/modified/deleted. So now we can get rid of the
    // deleted ones from the main list too. IndexOf uses objects in ObjectList.
    for i := FItemsToDelete.Count - 1 downto 0 do
      Delete(IndexOf(FItemsToDelete[i]));
  finally
    Screen.Cursor := lCursor;
  end; // finally
end;  // TLuxCachedDataList.Update 

{-------------------------------------------------------------------------------
}
procedure TLuxCachedDataList.ValidateContent;
var
  i: Integer;
begin
  SetLists;
  // First validate data for all items to save to the database.
  // Raise a TExceptionPath exception for invalid data.
  for i := 0 to FItemsToAdd.Count -1 do
    TLuxCachedDataItem(FItemsToAdd[i]).ValidateData;
  
  for i := 0 to FItemsToModify.Count - 1 do
    TLuxCachedDataItem(FItemsToModify[i]).ValidateData;
end;  // TLuxCachedDataList.ValidateContent 

{-==============================================================================
    TLuxStringDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLuxStringDataList.Create(AItemClass: TItemClass; AStrings: TStrings);
begin
  FStrings := AStrings;
  inherited Create(AItemClass);
end;  // TLuxStringDataList.Create 

{-------------------------------------------------------------------------------
}
procedure TLuxStringDataList.AddToList(AItem: TLuxCachedDataItem; AIndex: Integer);
var
  lIndex, lNewIndex: Integer;
  lNonDeletedItemsIndex, i: Integer;
begin
  if not CanAddItemToList(AItem) then Exit;

  { Add to the string list first so we can find out where it should go }
  lIndex := FStrings.AddObject(GetText(AItem), AItem);
  { Since FStrings.Count could be less than Items.Count we must check the Items array for
    items marked for deletion and ignore them. i.e deleting item 3 from an initial list of 5
    items will set FStrings.Count = 4 and Items.Count = 5 where TDataItem(Items[3]).Delete =
        true }
  lNonDeletedItemsIndex := -1;
  lNewIndex := -1;
  for i := 0 to Count - 1 do
    if not TLuxCachedDataItem(Items[i]).Deleted then
    begin
      Inc(lNonDeletedItemsIndex);
      if lNonDeletedItemsIndex = lIndex then lNewIndex := i;
    end;
  
  inherited AddToList(AItem, lNewIndex);
end;  // TLuxStringDataList.AddToList 

{-------------------------------------------------------------------------------
}
procedure TLuxStringDataList.DeleteItem(const AIndex: Integer);
var
  lDataItemIndex: Integer;
begin
  if AIndex > FStrings.Count - 1 then
    raise EDataListError.Create(Format(ResStr_ListIndexOutOfBound, [AIndex]));
  { Translate the string list item index to our object list item index }
  lDataItemIndex := IndexOf(FStrings.Objects[AIndex]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ObjectForItemNotFound);
  FStrings.Delete(AIndex);
  inherited DeleteItem(lDataItemIndex);
end;  // TLuxStringDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TLuxStringDataList.Refresh;
begin
  FStrings.Clear;
  inherited;
end;  // TLuxStringDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TLuxStringDataList.RefreshItemDisplay(AItem: TLuxStringDataItem);
var
  lIdx: Integer;
begin
  lIdx :=  FStrings.IndexOfObject(AItem);
  if lIdx <> -1 then
    FStrings[lIdx] := GetText(AItem);
end;  // TLuxStringDataList.RefreshItemDisplay 

{-==============================================================================
    TLuxStringDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TLuxStringDataItem.SetModified;
begin
  if not (FOwnerList is TLuxStringDataList) then
    raise EDataListError.Create(ResStr_OwnerNotStringDataList);
  inherited SetModified;
  TLuxStringDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TLuxStringDataItem.SetModified 

{-==============================================================================
    TLuxGridDataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TLuxGridDataItem.GetDisplayText(Column: Integer): String;
var
  lText: String;
  lKey: TKeyString;
begin
  GetData(Column, lText, lKey);
  Result := lText;
end;  // TLuxGridDataItem.GetDisplayText 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataItem.SetData(const Column: Integer; const AText: String; const AKey: 
    TKeyString);
begin
  // Some grids can be always readonly, therefore removing the need for this method.
  // Override when appropriate.
end;  // TLuxGridDataItem.SetData 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataItem.SetModified;
begin
  if not (FOwnerList is TLuxGridDataList) then
    raise EDataListError.Create(ResStr_OwnerNotGridDataList);
  inherited SetModified;
  TLuxGridDataList(FOwnerList).RefreshItemDisplay(Self);
end;  // TLuxGridDataItem.SetModified 

{-==============================================================================
    TLuxGridDataList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLuxGridDataList.Create(AItemClass: TItemClass; AStringGrid: TDSSStringGrid);
begin
  FGrid := AStringGrid;
  if not Assigned(FGrid) then
    raise ELuxCachedDataError.Create(
        'GridDataList component requires reference to existing DssStringGrid control.');
  
  with FGrid do begin
    // Save user-defined event, before hijacking it.
    FGridKeyDown := OnKeyDown;
    FGridRowMoved := OnRowMoved;
    OnKeyDown := GridKeyDown;
    OnRowMoved := GridRowMoved;
    OnCellCustomKeyDown := GridCellCustomKeyDown;
    OnCellLeaveCustom := GridCellLeaveCustom;
    OnCellLeaveDefault := GridCellLeaveDefault;
    OnCellSelectedCustom := GridCellSelectedCustom;
    OnCellSelectedDefault := GridCellSelectedDefault;
  end;
  inherited Create(AItemClass);
end;  // TLuxGridDataList.Create 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.AddNew(AItem: TLuxCachedDataItem);
begin
  inherited AddNew(AItem);
  RefreshItemDisplay(TLuxGridDataItem(AItem));
end;  // TLuxGridDataList.AddNew 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.AddToList(AItem: TLuxCachedDataItem; AIndex: Integer);
var
  i, lDisplayed: Integer;
begin
  inherited AddToList(AItem, AIndex);

  if IndexOf(AItem) = -1 then Exit;

  { Add another one except for when top blank row is kept - we can't lose the
      very top row otherwise our fixed rows go wrong }
  // Get count of displayed items, so we can get the count of rows needed in the grid
  lDisplayed := Count;
  for i := 0 to Count - 1 do
    if TLuxCachedDataItem(Items[i]).Deleted or TLuxCachedDataItem(Items[i]).Hidden
      then Dec(lDisplayed);

  with FGrid do begin
    if lDisplayed >= RowCount - FixedRows then RowCount := lDisplayed + FixedRows
                                          else RowCount := FixedRows + 1;
    { Populate last row }
    for i := 0 to ColCount - 1 do
      Cells[i, RowCount - 1] := TLuxGridDataItem(AItem).DisplayText[i];
    Rows[RowCount - 1].Objects[0] := AItem;
  end;
end;  // TLuxGridDataList.AddToList

{-------------------------------------------------------------------------------
  Method to update the grid whenever items in the list have been altered but
  without making any changes to the database.
}
procedure TLuxGridDataList.UpdateGrid;
var
  i, j, lCurrentRow, lDisplayed: Integer;
begin
  lDisplayed := Count;
  for i := 0 to Count - 1 do
  begin
    if TLuxCachedDataItem(Items[i]).Deleted or TLuxCachedDataItem(Items[i]).Hidden
      then Dec(lDisplayed)
    else if TLuxCachedDataItem(Items[i]).ItemKey = '' then
    begin
      Remove(Items[i]);
      Dec(lDisplayed);
    end;
  end;

  with FGrid do begin
    if lDisplayed > 0 then
    begin
      RowCount := lDisplayed + FixedRows;
      lCurrentRow := FixedRows;
      for i := 0 to Count - 1 do
      begin
        if (not TLuxCachedDataItem(Items[i]).Deleted)
           and (not TLuxCachedDataItem(Items[i]).Hidden) then
        begin
          for j := 0 to ColCount - 1 do
          begin
            Cells[j, lCurrentRow] := TLuxGridDataItem(Items[i]).DisplayText[j];
            end;
          Rows[lCurrentRow].Objects[0] := Items[i];
          Inc(lCurrentRow);
      end;
    end;
    end else
    //If there are no rows to display, ensure that one empty row is displayed
    //(Setting RowCount = FixedRows will not work as this automatically makes
    //the last fixed row unfixed.)
    begin
      RowCount := FixedRows + 1;
      for j := 0 to ColCount - 1 do
        begin
          Cells[j, RowCount - 1] := '';
        end;
      Rows[FixedRows].Objects[0] := nil;
    end;


  end;
end;

{-------------------------------------------------------------------------------
  Allow user to override default behaviour which is to add rows to linked grid when the Down 
      key is pressed.
}
function TLuxGridDataList.AllowedAddOnKeyDown: Boolean;
begin
  Result := True;
end;  // TLuxGridDataList.AllowedAddOnKeyDown 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.DeleteItem(const AIndex: integer);
var
  lDataItemIndex: Integer;
begin
  if AIndex > FGrid.RowCount - 1 then
    raise EDataListError.Create(Format(ResStr_ListIndexOutOfBound, [AIndex]));
  { Translate the string list item index to our object list item index - note
    we always store the object on the left most cell of the row (item 0) }
  lDataItemIndex := IndexOf(FGrid.Rows[AIndex].Objects[0]);
  if lDataItemIndex = -1 then
    raise EDataListError.Create(ResStr_ObjectForItemNotFound);
  { Select the row to delete }
  FGrid.RemoveRow(AIndex);
  inherited DeleteItem(lDataItemIndex);
end;  // TLuxGridDataList.DeleteItem 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.DoCustomGetData(AWinControl: TWinControl; var Text, Key: String);
begin
  if Assigned(FOnCustomGetData) then FOnCustomGetData(AWinControl, Text, Key);
end;  // TLuxGridDataList.DoCustomGetData 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.DoCustomSetData(AWinControl: TWinControl; const Text, Key: String);
begin
  if Assigned(FOnCustomSetData) then FOnCustomSetData(AWinControl, Text, Key);
end;  // TLuxGridDataList.DoCustomSetData 

{-------------------------------------------------------------------------------
  Ensure there is always an item returned. Create one if needed, but only if grid not in 
      ReadOnly mode.
}
function TLuxGridDataList.GetItemAtRow(const ARow: Integer): TLuxGridDataItem;
begin
  Result := TLuxGridDataItem(FGrid.Rows[ARow].Objects[0]);
  if (Result = nil) and not FGrid.ReadOnly then begin
    // Create new item
    Result := TLuxGridDataItem(FItemClass.CreateNew(Self));
    // Add it to list
    Add(Result);
    // Link it to grid.
    FGrid.Rows[ARow].Objects[0] := Result;
  end;
end;  // TLuxGridDataList.GetItemAtRow 

{-------------------------------------------------------------------------------
  Trap the Return key for Linked Edit controls if the key is empty, which means that the 
      content 
      hasn't been validated, by simply setting the Key parameter to zero to tell the grid it's 
      been handled.
}
procedure TLuxGridDataList.GridCellCustomKeyDown(Sender: TObject; var Key: Word; Shift: 
    TShiftState; AWinControl: TWinControl);
begin
  if not FGrid.ReadOnly then
    if AWinControl is TLinkedEdit then
      if (Key = VK_RETURN) and (TLinkedEdit(AWinControl).Key = '') then Key := 0;
end;  // TLuxGridDataList.GridCellCustomKeyDown 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.GridCellLeaveCustom(Sender: TObject; ACol, ARow: Integer; 
    WinControl: TWinControl);
var
  lItem: TLuxGridDataItem;
  lText, lKey: String;
begin
  if not FGrid.ReadOnly then begin
    lItem := GetItemAtRow(ARow);
    if not Assigned(lItem) then Exit;
  
    if WinControl is TLinkedEdit then
      lItem.SetData(ACol, TLinkedEdit(WinControl).Text, TLinkedEdit(WinControl).Key)
    else
    if WinControl is TIDComboBox then
      lItem.SetData(ACol, TIDComboBox(WinControl).CurrentItem,
                          TIDComboBox(WinControl).CurrentStrID)
    else begin
      // Grab data from custom control
      DoCustomGetData(WinControl, lText, lKey);
      //  Shove it in the grid/object/list.
      lItem.SetData(ACol, lText, lKey);
    end;
  end;
end;  // TLuxGridDataList.GridCellLeaveCustom 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.GridCellLeaveDefault(Sender: TObject; ACol, ARow: Integer; var 
    Options: TGridOptions);
begin
  if not FGrid.ReadOnly then
    if Assigned(GetItemAtRow(ARow)) then
      GetItemAtRow(ARow).SetData(ACol, FGrid.Cells[ACol, ARow], '');
end;  // TLuxGridDataList.GridCellLeaveDefault 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.GridCellSelectedCustom(Sender: TObject; ACol, ARow: Integer;
    WinControl: TWinControl);
var
  lText: String;
  lKey: TKeyString;
  lItemIndex: Integer;
begin
  if not Assigned(GetItemAtRow(ARow)) then Exit;
  GetItemAtRow(ARow).GetData(ACol, lText, lKey);
  
  if WinControl is TLinkedEdit then begin
    TLinkedEdit(WinControl).Key := lKey;
    TLinkedEdit(WinControl).Text := lText;
  end else
  if WinControl is TIDComboBox then
    with TIDComboBox(WinControl) do
      // DropDownList 'should' have the item, unless it hasn't yet been populated!
      // Or not DropDownList or DropDown, but item has a key, so look for it.
      if (Style in [csDropDownList, csDropDown]) or (lKey <> '') then
        if not Populated then begin
          // Not populated yet, so clear previous single item (if any) and
          // add the new one instead, and select it.
          Clear;
          ItemIndex := Add(lText, lKey);
        end else begin
          // Combo already populated, item may be in the list if it was one
          // of the 10 most recently used terms.
          lItemIndex := IndexOf(lText);
          // Item in the combo box list so can use the text and the key.
          if lItemIndex <> -1 then
            ItemIndex := lItemIndex;

          // Make sure Text is correct. For DropDownList, setting ItemIndex is enough.
          if Style = csDropDown then
            Text := lText;
        end
      else
        // Not DropDownList or DropDown and no key, set Text property directly.
        Text := lText
  else
    // Send data to custom control
    DoCustomSetData(WinControl, lText, lKey);
end;  // TLuxGridDataList.GridCellSelectedCustom 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.GridCellSelectedDefault(Sender: TObject; ACol, ARow: Integer; var 
    Options: TGridOptions);
var
  lText: String;
  lKey: TKeyString;
begin
  if not Assigned(GetItemAtRow(ARow)) then Exit;
  // Key not used in this case, but it is a VAR param in method.
  GetItemAtRow(ARow).GetData(ACol, lText, lKey);
  FGrid.Cells[ACol, ARow] := lText;
end;  // TLuxGridDataList.GridCellSelectedDefault 

{-------------------------------------------------------------------------------
  Allow the list to react to key presses happening on the grid.
}
procedure TLuxGridDataList.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lOptions: TGridOptions;
begin
  // List hijacked grid event, so call user-defined one, if any.
  if Assigned(FGridKeyDown) then FGridKeyDown(Sender, Key, Shift);
  
  if not FGrid.ReadOnly then
    case Key of
      VK_DOWN:
          // On last row, therefore, add new one.
          if (FGrid.Row = FGrid.RowCount - 1) and AllowedAddOnKeyDown then
            AddNew(FItemClass.CreateNew(Self));
  
      VK_ESCAPE:
          with FGrid do
            if ColumnsInfo[Col].ColumnType = ctDefault then
              GridCellSelectedDefault(nil, Col, Row, lOptions)
            else begin
              GridCellSelectedCustom(nil, Col, Row, ColumnsInfo[Col].WinControl);
              ColumnsInfo[Col].WinControl.SetFocus;
              Key := 0;
            end;
    end;
end;  // TLuxGridDataList.GridKeyDown 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.GridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
var
  Obj: TObject;
  OwnedObjects: Boolean;
begin
  // List hijacked grid event, so call user-defined one, if any.
  if Assigned(FGridRowMoved) then FGridRowMoved(Sender, FromIndex, ToIndex);
  
  Obj := Items[FromIndex - Grid.FixedRows];
  OwnedObjects := OwnsObjects;
  
  OwnsObjects := False;
  try
    Delete(FromIndex - Grid.FixedRows);
    Insert(ToIndex - Grid.FixedRows, Obj);
  finally
    OwnsObjects := OwnedObjects;
  end;
end;  // TLuxGridDataList.GridRowMoved 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.Refresh;
begin
  //Clear grid
  with FGrid do begin
    RowCount := FixedRows + 1;
    Rows[RowCount - 1].CommaText := '';
  end;
  inherited;
end;  // TLuxGridDataList.Refresh 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.RefreshItemDisplay(AItem: TLuxGridDataItem);
var
  i, lIndex, lRow: Integer;
begin
  lIndex := IndexOf(AItem);
  if lIndex <> -1 then begin
    // Set row index as if nothing had been deleted
    lRow := lIndex + FGrid.FixedRows;
    // Decrease by one for each deleted item before current item. As the deleted
    // items don't appear in the grid, there is a misalignment to deal with
    // But a row might be emptied without it being removed from the grid though.
    for i := lIndex downto 0 do
      if (TLuxGridDataItem(Items[i]).Deleted or TLuxGridDataItem(Items[i]).Hidden) and
         (FGrid.Objects[0, lRow] <> Items[i]) then
        Dec(lRow);
    // Now proceed with the proper row for the item
    for i := 0 to FGrid.ColCount - 1 do
      FGrid.Cells[i, lRow] := AItem.DisplayText[i];
  end;
end;  // TLuxGridDataList.RefreshItemDisplay 

{-------------------------------------------------------------------------------
}
procedure TLuxGridDataList.ValidateContent;
var
  lRow: Integer;
  lCol: Integer;
  lItem: TLuxGridDataItem;
begin
  // Do this to trick the grid. That's for when custom controls are still visible
  // and list is going to be saved, and value in control not updated in list item.
  FGrid.ReadOnly := FGrid.ReadOnly;
  
  // And find out if some items should be completely ignored.
  // Especially for first row in empty grid...
  for lRow := FGrid.FixedRows to FGrid.RowCount - 1 do begin
    lItem := GetItemAtRow(lRow);
    if not Assigned(lItem) then Continue;
    // Assume empty row
    lItem.Ignore := True;
    // Now work out if it is really empty
    for lCol := FGrid.FixedCols to FGrid.ColCount - 1 do
      if FGrid.Cells[lCol, lRow] <> '' then begin
        // Wasn't empty after all...
        lItem.Ignore := False;
        // Stop inner loop to move on to next row.
        Break;
      end;
    // If item was added and cleared, ignoring it is enough.
    // If item came from database, clearing equals deleting it, so flip the switches.
    if lItem.Ignore and not lItem.Added then begin
      lItem.Ignore := False;
      lItem.Deleted := True;
    end;
  end;
  
  inherited ValidateContent;
end;  // TLuxGridDataList.ValidateContent 

end.
