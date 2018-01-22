{===============================================================================
  Unit:        ExtendedUserConfigMenuItem

  Defines:     TExtendedUserConfigMenuItem

  Description: Class that implements a single IDynamicMenu instance for the
               Extended User Configuration screen.

  Model:       CollectionsModuleManager.mpb

  Created:     Oct 2003

  Last revision information:
    $Revision: 5 $
    $Date: 6/01/04 11:34 $
    $Author: Johnvanbreda $

===============================================================================}
unit ExtendedUserConfigMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB,
  UserConfigExtended_TLB;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the Extended User Configration screen.
  }
  TExtendedUserConfigMenuItem = class (TAutoObject, IExtendedUserConfigMenuItem, IDynamicMenu)
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
  end;
  
//==============================================================================
implementation

uses
  ComServ, ResourceStrings;

{-==============================================================================
    TExtendedUserConfigMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Item has no children. 
}
function TExtendedUserConfigMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TExtendedUserConfigMenuItem.Child 

{-------------------------------------------------------------------------------
  Return an instance of the Extended User Config screen. 
}
function TExtendedUserConfigMenuItem.Execute: IUnknown;
begin
  Result := CreateCOMObject(CLASS_frmUCEMain);
end;  // TExtendedUserConfigMenuItem.Execute 

{-------------------------------------------------------------------------------
  Initialise the action to be created. 
}
procedure TExtendedUserConfigMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := ResStr_MnuExtendedUserConfiguration;
    Enabled := True;
    Hint := Caption;
    ImageIndex := -1;
    Shortcut := 0;
  end;
end;  // TExtendedUserConfigMenuItem.GetItemData 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TExtendedUserConfigMenuItem.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TExtendedUserConfigMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TExtendedUserConfigMenuItem.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TExtendedUserConfigMenuItem.Get_HasSubmenu 

initialization
  TAutoObjectFactory.Create(ComServer, TExtendedUserConfigMenuItem,
      Class_ExtendedUserConfigMenuItem,
    ciMultiInstance, tmApartment);
end.
