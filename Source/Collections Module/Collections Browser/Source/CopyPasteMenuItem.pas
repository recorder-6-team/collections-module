{===============================================================================
  Unit:        CopyPasteMenuItem.pas

  Defines:     TCopyPasteMenu

  Description: IDynamicMenu implementation for Cop and Paste Menu Items.

  Model:       CollectionBrowserGeneral.mpb

  Created:     Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 10/08/04 17:55 $
    $Author: Ericsalmon $

===============================================================================}

unit CopyPasteMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsBrowser_TLB, StdVcl, Recorder2000_TLB,
  DataTypes;

type
  TCopyPasteMenu = class(TAutoObject, ICopyPasteMenu, IDynamicMenu)
  private
    FMenuType: TMenuType;
    FCopyPasteActions: ICopyPasteActions;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_MenuTypeIndex: Integer; safecall;
    procedure Set_MenuTypeIndex(Value: Integer); safecall;
    function Get_CopyPasteActions: ICopyPasteActions; safecall;
    procedure Set_CopyPasteActions(const Value: ICopyPasteActions); safecall;
  end;

//==============================================================================
implementation

uses
  ComServ, ResourceStrings, Classes, Menus;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Execute: IUnknown;
begin
  Result := nil;
  case FMenuType of
    mtCopy: FCopyPasteActions.Copy;
    mtPaste: FCopyPasteActions.Paste;
  end;
end;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Get_ChildCount: Integer;
begin
  Result := 0;
end;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Get_HasSubmenu: WordBool;
begin
  Result := false;
end;

{-------------------------------------------------------------------------------
}
procedure TCopyPasteMenu.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    case FMenuType of
      mtCopy:
        begin
          Caption := ResStr_MnuCopy;
          ImageIndex := 2;
          ShortCut := Menus.ShortCut(Word('C'), [ssCtrl]);
        end;
      mtPaste:
        begin
          Caption :=  ResStr_MnuPaste;
          ImageIndex := 3;
          ShortCut := Menus.ShortCut(Word('V'), [ssCtrl]);
        end;
    end;
    Enabled := True;
    Hint := Caption;
    Visible    := True;
  end;
end;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Get_MenuTypeIndex: Integer;
begin
  Result := integer(FMenuType);
end;

procedure TCopyPasteMenu.Set_MenuTypeIndex(Value: Integer);
begin
  FMenuType := TMenuType(Value);
end;

{-------------------------------------------------------------------------------
}
function TCopyPasteMenu.Get_CopyPasteActions: ICopyPasteActions;
begin
  Result := FCopyPasteActions;
end;

{-------------------------------------------------------------------------------
}
procedure TCopyPasteMenu.Set_CopyPasteActions(const Value: ICopyPasteActions);
begin
  FCopyPasteActions := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCopyPasteMenu, Class_CopyPasteMenu,
    ciMultiInstance, tmApartment);
end.
