{===============================================================================
  Unit:        SpecimenFinderMenuItem

  Defines:     TSpecimenFinderMenuItem

  Description: Class that implements a single IDynamicMenu instance for the
               Specimen Finder module.

  Created:     Oct 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/01/04 11:34 $
    $Author: Johnvanbreda $

===============================================================================}
unit SpecimenFinderMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB,
  SpecimenFinderModule_TLB;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the Specimen Finder screen.
  }
  TSpecimenFinderMenuItem = class (TAutoObject, ISpecimenFinderMenuItem, IDynamicMenu)
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
    TSpecimenFinderMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Menu item has no children. 
}
function TSpecimenFinderMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TSpecimenFinderMenuItem.Child 

{-------------------------------------------------------------------------------
  Return an instance of the specimen finder form. 
}
function TSpecimenFinderMenuItem.Execute: IUnknown;
begin
  Result := CreateComObject(CLASS_frmSpecimenFinder);
end;  // TSpecimenFinderMenuItem.Execute 

{-------------------------------------------------------------------------------
  Initialise the data describing the action to create. 
}
procedure TSpecimenFinderMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := ResStr_MnuSpecimenFinder;
    Enabled := True;
    Hint := Caption;
    ImageIndex := 5;
    Shortcut := 0;
  end;
end;  // TSpecimenFinderMenuItem.GetItemData 

{-------------------------------------------------------------------------------
  Menu item has no children. 
}
function TSpecimenFinderMenuItem.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TSpecimenFinderMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
  Menu item has no children so no sub-menu. 
}
function TSpecimenFinderMenuItem.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TSpecimenFinderMenuItem.Get_HasSubmenu 

initialization
  TAutoObjectFactory.Create(ComServer, TSpecimenFinderMenuItem, Class_SpecimenFinderMenuItem,
    ciMultiInstance, tmApartment);
end.
