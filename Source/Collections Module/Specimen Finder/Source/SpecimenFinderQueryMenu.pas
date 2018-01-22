{===============================================================================
  Unit:        SpecimenFinderQueryMenu

  Defines:     TSpecimenFinderQueryMenu

  Description: IDynamicMenu implementations for the File\Specimen Finder submenu

  Created:     Dec 2010


===============================================================================}

unit SpecimenFinderQueryMenu;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, SpecimenFinderModule_TLB, StdVcl, Recorder2000_TLB,
  SpecimenFinder;

const
  // ItemType values
  MNU_SPECIMEN_FINDER   = 0;
  MNU_OPEN              = 1;
  MNU_SAVE              = 2;

type
  TSpecimenFinderQueryMenu = class (TAutoObject, ISpecimenFinderQueryMenu,
                                    IDynamicMenu)
  private
    FItemType: Integer;
    FSpecimenFinder: TfrmSpecimenFinder;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_ItemType: Integer; safecall;
    function Get_SpecimenFinder: Integer; safecall;
    procedure Set_ItemType(Value: Integer); safecall;
    procedure Set_SpecimenFinder(Value: Integer); safecall;
  end;
  
implementation

uses ComServ, ResourceStrings;

{-==============================================================================
    TSpecimenFinder
===============================================================================}
{-------------------------------------------------------------------------------
  Get children of the Specimen Finder item, otherwise no children available.
}
function TSpecimenFinderQueryMenu.Child(Index: Integer): IUnknown;
begin
  if FItemType=MNU_SPECIMEN_FINDER then begin
    Result := CreateCOMObject(CLASS_SpecimenFinderQueryMenu);
    (Result as ISpecimenFinderQueryMenu).ItemType := Index+1;
    // pass a pointer to the object so it can find the diagram
    (Result as ISpecimenFinderQueryMenu).SpecimenFinder := Integer(FSpecimenFinder);
  end
  else
    Result := nil;
end;  // TSpecimenFinderQueryMenu.Child

{-------------------------------------------------------------------------------
  Executes the appropriate behaviour depending on which type of menu item this 
      is. 
}
function TSpecimenFinderQueryMenu.Execute: IUnknown;
begin
  case FItemType of
    MNU_OPEN: FSpecimenFinder.OpenQuery;
    MNU_SAVE: FSpecimenFinder.SaveQuery;
  end;
end;  // TSpecimenFinderQuery.Execute

{-------------------------------------------------------------------------------
  Return data required to set up the caption. 
}
procedure TSpecimenFinderQueryMenu.GetItemData(var ActionData: TActionData);
  
  procedure InitActionData(const ACaption: string; AEnabled: boolean;
      AImageIndex: integer=-1);
  begin
    with ActionData do begin
      Caption := ACaption;
      Enabled := AEnabled;
      ImageIndex := AImageIndex;
    end; // with
  end;
  
begin
  case FItemType of
    MNU_SPECIMEN_FINDER :
      InitActionData(ResStr_MnuSpecimenFinderQuery, True, 4);
    MNU_OPEN :
      InitActionData(ResStr_MnuOpenQuery, True, 5);
    MNU_SAVE : begin
      InitActionData(ResStr_MnuSaveQuery, True, 6);
      // Only enabled if specimen finder contains some search criterion
      if FSpecimenFinder.fraSpecimenFinderDragFrame.lbFinder.Items.Count <> 0 then
        ActionData.Enabled := True
      else
        ActionData.Enabled := False;
    end;
  end; // case
  ActionData.Hint := ActionData.Caption;
  ActionData.ShortCut := 0;
end;  // TSpecimenFinderQueryMenu.GetItemData

{-------------------------------------------------------------------------------
  Specimen Finder menu has 2 sub-items.
}
function TSpecimenFinderQueryMenu.Get_ChildCount: Integer;
begin
  case FItemType of
    MNU_SPECIMEN_FINDER: Result := 2;
  else
    Result := 0;
  end;
 // case
end;  // TSpecimenFinderQueryMenu.Get_ChildCount

{-------------------------------------------------------------------------------
  Specimen Finder item only has a sub menu. 
}
function TSpecimenFinderQueryMenu.Get_HasSubmenu: WordBool;
begin
  Result := FItemType=MNU_SPECIMEN_FINDER;
end;  // TSpecimenFinderQueryMenu.Get_HasSubmenu

{-------------------------------------------------------------------------------
}
function TSpecimenFinderQueryMenu.Get_ItemType: Integer;
begin
  Result := FItemType;
end;  // TSpecimenFinderQueryMenu.Get_ItemType

{-------------------------------------------------------------------------------
}
function TSpecimenFinderQueryMenu.Get_SpecimenFinder: Integer;
begin
  Result := Integer(FSpecimenFinder);
end;  // TSpecimenFinderQueryMenu.Get_SpecimenFinder

{-------------------------------------------------------------------------------
}
procedure TSpecimenFinderQueryMenu.Set_ItemType(Value: Integer);
begin
  FItemType := Value;
end;  // TSpecimenFinderQueryMenu.Set_ItemType

{-------------------------------------------------------------------------------
}
procedure TSpecimenFinderQueryMenu.Set_SpecimenFinder(Value: Integer);
begin
  FSpecimenFinder := TfrmSpecimenFinder(Value);
end;  // TSpecimenFinderQueryMenu.Set_SpecimenFinder

initialization
  TAutoObjectFactory.Create(ComServer, TSpecimenFinderQueryMenu, CLASS_SpecimenFinderQueryMenu,
    ciMultiInstance, tmApartment);
end.
