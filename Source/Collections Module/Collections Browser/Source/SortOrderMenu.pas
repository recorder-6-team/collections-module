{===============================================================================
  Unit:        SortOrderMenu

  Defines:     TSortOrderMenu

  Description: IDynamicMenu implementation for the top level item for the
               sort order sub-menu.

  Model:       CollectionBrowserGeneral.mpb

  Created:

  Last revision information:
    $Revision: 5 $
    $Date: 18/03/04 14:04 $
    $Author: Anthonysimpson $

===============================================================================}
unit SortOrderMenu;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsBrowser_TLB, StdVcl, Recorder2000_TLB,
  ExceptionForm, Sysutils;

type
  ESortOrderMenuException = class (TExceptionPath)
  end;
  
  TSortOrderMenu = class (TAutoObject, ISortOrderMenu, IDynamicMenu)
  private
    FIndex: SmallInt;
    FSortableScreen: ISortableScreen;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_Index: Integer; safecall;
    function Get_SortableScreen: ISortableScreen; safecall;
    procedure Set_Index(Value: Integer); safecall;
    procedure Set_SortableScreen(const Value: ISortableScreen); safecall;
  end;
  
implementation

uses ComServ, DataTypes, ResourceStrings, BrowserViewTypes, BrowserNodeFramework;
// BrowserNodeFramework included because ISortOrderProvider has been moved there.

{-==============================================================================
    TSortOrderMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSortOrderMenu.Child(Index: Integer): IUnknown;
var
  lNewMenu: IDynamicMenu;
begin
  if FIndex=-1 then begin
    lNewMenu := CreateCOMObject(CLASS_SortOrderMenu) as IDynamicMenu;
    (lNewMenu As ISortOrderMenu).SortableScreen := FSortableScreen;
    // Set index to indicate which sub menu item it refers to
    (lNewMenu As ISortOrderMenu).Index := Index;
  end else
    lNewMenu := nil;  // individual sort items have no children
  Result := lNewMenu;
end;  // TSortOrderMenu.Child 

{-------------------------------------------------------------------------------
  Top level sort menu item does not execute.
}
function TSortOrderMenu.Execute: IUnknown;
begin
  if FIndex>-1 then
    if Assigned(FSortableScreen.SortProvider) then
      (FSortableScreen.SortProvider as ISortOrderProvider).SelectSort(FIndex);
  Result := nil;
end;  // TSortOrderMenu.Execute 

{-------------------------------------------------------------------------------
}
procedure TSortOrderMenu.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    if FIndex=-1 then
      Caption := ResStr_MnuSortBy  // -1 means top level Sort By... option
    else
      Caption := (FSortableScreen.SortProvider as ISortOrderProvider).SortOrderCaption[FIndex];
    Enabled    := True;
    Hint       := Caption;
    ShortCut   := 0;
    Visible    := True;
    ImageIndex := -1;
  end;
end;  // TSortOrderMenu.GetItemData 

{-------------------------------------------------------------------------------
}
function TSortOrderMenu.Get_ChildCount: Integer;
begin
  if Assigned(FSortableScreen.SortProvider) and (FIndex=-1) then
    Result := (FSortableScreen.SortProvider as ISortOrderProvider).SortOrderCount
  else
    Result := 0;
end;  // TSortOrderMenu.Get_ChildCount 

{-------------------------------------------------------------------------------
}
function TSortOrderMenu.Get_HasSubmenu: WordBool;
begin
  Result := FIndex = -1;
end;  // TSortOrderMenu.Get_HasSubmenu 

{-------------------------------------------------------------------------------
}
function TSortOrderMenu.Get_Index: Integer;
begin
  Result := FIndex;
end;  // TSortOrderMenu.Get_Index 

{-------------------------------------------------------------------------------
  COM Accessor method
}
function TSortOrderMenu.Get_SortableScreen: ISortableScreen;
begin
  Result := FSortableScreen;
end;  // TSortOrderMenu.Get_SortableScreen 

{-------------------------------------------------------------------------------
}
procedure TSortOrderMenu.Set_Index(Value: Integer);
begin
  FIndex := Value;
end;  // TSortOrderMenu.Set_Index 

{-------------------------------------------------------------------------------
  COM Accessor method
}
procedure TSortOrderMenu.Set_SortableScreen(const Value: ISortableScreen);
begin
  FSortableScreen := Value;
end;  // TSortOrderMenu.Set_SortableScreen 


initialization

  TAutoObjectFactory.Create(ComServer, TSortOrderMenu, Class_SortOrderMenu,
    ciMultiInstance, tmApartment);
end.
