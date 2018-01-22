{===============================================================================
  Unit:        ThesaurusBrowserMenuItem

  Defines:     TThesaurusBrowserMenuItem

  Description: Class that implements a single IDynamicMenu instance for the
               Thesaurus Browser screen.

  Created:     Oct 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/01/04 11:34 $
    $Author: Johnvanbreda $

===============================================================================}
unit ThesaurusBrowserMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB,
  ThesaurusBrowser_TLB;

type
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the Thesaurus Browser screen.
  }
  TThesaurusBrowserMenuItem = class (TAutoObject, IThesaurusBrowserMenuItem, IDynamicMenu)
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
    TThesaurusBrowserMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
  Item has no children. 
}
function TThesaurusBrowserMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TThesaurusBrowserMenuItem.Child 

{-------------------------------------------------------------------------------
}
function TThesaurusBrowserMenuItem.Execute: IUnknown;
begin
  Result := CreateComObject(CLASS_frmThesaurusBrowser);
end;  // TThesaurusBrowserMenuItem.Execute 

{-------------------------------------------------------------------------------
}
procedure TThesaurusBrowserMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := ResStr_MnuThesaurusBrowser;
    Enabled := True;
    Hint := Caption;
    ImageIndex := 1;
    Shortcut := 0;
  end;
end;  // TThesaurusBrowserMenuItem.GetItemData 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TThesaurusBrowserMenuItem.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TThesaurusBrowserMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
  Item has no children. 
}
function TThesaurusBrowserMenuItem.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TThesaurusBrowserMenuItem.Get_HasSubmenu 

initialization
  TAutoObjectFactory.Create(ComServer, TThesaurusBrowserMenuItem, 
      Class_ThesaurusBrowserMenuItem,
    ciMultiInstance, tmApartment);
end.
