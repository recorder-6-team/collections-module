{===============================================================================
  Unit:        CollectionsBrowserMenuItem

  Defines:     TCollectionsBrowserMenuItem

  Description: Class that implements a single IDynamicMenu instance for the
               Collections Browser module.

  Created:     Oct 2003

  Last revision information:
    $Revision: 3 $
    $Date: 16/01/04 10:58 $
    $Author: Bencollier $

===============================================================================}
unit CollectionsBrowserMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB,
  CollectionsBrowser_TLB;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the Collections Browser screen.
  }
  TCollectionsBrowserMenuItem = class (TAutoObject, ICollectionsBrowserMenuItem, IDynamicMenu)
  private
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
  end;
  
implementation

uses ComServ, ResourceStrings;

{-==============================================================================
    TCollectionsBrowserMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Item has no children. 
}
function TCollectionsBrowserMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TCollectionsBrowserMenuItem.Child 

{-------------------------------------------------------------------------------
}
function TCollectionsBrowserMenuItem.Execute: IUnknown;
begin
  Result := CreateCOMObject(CLASS_frmCBMain);
end;  // TCollectionsBrowserMenuItem.Execute 

{-------------------------------------------------------------------------------
}
procedure TCollectionsBrowserMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := ResStr_MnuCollectionsBrowser;
    Enabled := True;
    Hint := Caption;
    ImageIndex := 0;
    Shortcut := 0;
  end;
end;  // TCollectionsBrowserMenuItem.GetItemData 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TCollectionsBrowserMenuItem.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TCollectionsBrowserMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TCollectionsBrowserMenuItem.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TCollectionsBrowserMenuItem.Get_HasSubmenu 

initialization
  TAutoObjectFactory.Create(ComServer, TCollectionsBrowserMenuItem, 
      Class_CollectionsBrowserMenuItem,
    ciMultiInstance, tmApartment);
end.
