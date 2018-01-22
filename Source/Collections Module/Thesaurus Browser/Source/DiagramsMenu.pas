{===============================================================================
  Unit:        DiagramsMenu

  Defines:     TDiagramsMenu

  Description: IDynamicMenu implementations for the File\Diagrams submenu

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 5 $
    $Date: 7/01/04 16:46 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramsMenu;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, ThesaurusBrowser_TLB, StdVcl, Recorder2000_TLB,
  ThesaurusDiagram, ThesaurusBrowserImpl;

const
  // ItemType values
  MNU_DIAGRAM         = 0;
  MNU_DIAGRAM_OPEN    = 1;
  MNU_DIAGRAM_SAVE    = 2;
  MNU_DIAGRAM_SAVEAS  = 3;
  MNU_DIAGRAM_SAVEIMG = 4;
  MNU_DIAGRAM_PRINT   = 5;


type
  TDiagramsMenu = class (TAutoObject, IDiagramsMenu, IDynamicMenu)
  private
    FItemType: Integer;
    FThesaurusBrowser: TfrmThesaurusBrowser;
  protected
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    function Get_ItemType: Integer; safecall;
    function Get_ThesaurusBrowser: Integer; safecall;
    procedure Set_ItemType(Value: Integer); safecall;
    procedure Set_ThesaurusBrowser(Value: Integer); safecall;
  end;
  
implementation

uses ComServ, ResourceStrings;

{-==============================================================================
    TDiagramsMenu
===============================================================================}
{-------------------------------------------------------------------------------
  Get children of the Diagram item, otherwise no children available. 
}
function TDiagramsMenu.Child(Index: Integer): IUnknown;
begin
  if FItemType=MNU_DIAGRAM then begin
    Result := CreateCOMObject(CLASS_DiagramsMenu);
    (Result as IDiagramsMenu).ItemType := Index+1;
    // pass a pointer to the object so it can find the diagram
    (Result as IDiagramsMenu).ThesaurusBrowser := Integer(FThesaurusBrowser);
  end
  else
    Result := nil;
end;  // TDiagramsMenu.Child 

{-------------------------------------------------------------------------------
  Executes the appropriate behaviour depending on which type of menu item this 
      is. 
}
function TDiagramsMenu.Execute: IUnknown;
begin
  case FItemType of
    MNU_DIAGRAM_OPEN: FThesaurusBrowser.OpenDiagram;
    MNU_DIAGRAM_SAVE: FThesaurusBrowser.CurrentDiagram.Save;
    MNU_DIAGRAM_SAVEAS: FThesaurusBrowser.CurrentDiagram.SaveAs;
    MNU_DIAGRAM_SAVEIMG: FThesaurusBrowser.CurrentDiagram.SaveImg;
    MNU_DIAGRAM_PRINT: FThesaurusBrowser.CurrentDiagram.Print;
  end;
end;  // TDiagramsMenu.Execute 

{-------------------------------------------------------------------------------
  Return data required to set up the caption. 
}
procedure TDiagramsMenu.GetItemData(var ActionData: TActionData);
  
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
    MNU_DIAGRAM :
      InitActionData(ResStr_MnuDiagrams, True, 0);
    MNU_DIAGRAM_OPEN :
      InitActionData(ResStr_MnuOpen, True, 1);
    MNU_DIAGRAM_SAVE : begin
      InitActionData(ResStr_MnuSave, True, 2);
      // Only enabled if diagram has known filename
      if FThesaurusBrowser.CurrentDiagram <> nil then
        ActionData.Enabled := not (FThesaurusBrowser.CurrentDiagram.FileName='')
      else
        ActionData.Enabled := False;
    end;
    MNU_DIAGRAM_SAVEAS :
      InitActionData(ResStr_MnuSaveAs, FThesaurusBrowser.CurrentDiagram <> nil);
    MNU_DIAGRAM_SAVEIMG :
      InitActionData(ResStr_MnuSaveAsImage, FThesaurusBrowser.CurrentDiagram <> nil);
    MNU_DIAGRAM_PRINT :
      InitActionData(ResStr_MnuPrint, FThesaurusBrowser.CurrentDiagram <> nil);
  end; // case
  ActionData.Hint := ActionData.Caption;
  ActionData.ShortCut := 0;
end;  // TDiagramsMenu.GetItemData 

{-------------------------------------------------------------------------------
  Diagram menu has 4 sub-items. 
}
function TDiagramsMenu.Get_ChildCount: Integer;
begin
  case FItemType of
    MNU_DIAGRAM: Result := 5;
  else
    Result := 0;
  end; // case
end;  // TDiagramsMenu.Get_ChildCount 

{-------------------------------------------------------------------------------
  Diagram item only has a sub menu. 
}
function TDiagramsMenu.Get_HasSubmenu: WordBool;
begin
  Result := FItemType=MNU_DIAGRAM;
end;  // TDiagramsMenu.Get_HasSubmenu 

{-------------------------------------------------------------------------------
}
function TDiagramsMenu.Get_ItemType: Integer;
begin
  Result := FItemType;
end;  // TDiagramsMenu.Get_ItemType 

{-------------------------------------------------------------------------------
}
function TDiagramsMenu.Get_ThesaurusBrowser: Integer;
begin
  Result := Integer(FThesaurusBrowser);
end;  // TDiagramsMenu.Get_ThesaurusBrowser 

{-------------------------------------------------------------------------------
}
procedure TDiagramsMenu.Set_ItemType(Value: Integer);
begin
  FItemType := Value;
end;  // TDiagramsMenu.Set_ItemType 

{-------------------------------------------------------------------------------
}
procedure TDiagramsMenu.Set_ThesaurusBrowser(Value: Integer);
begin
  FThesaurusBrowser := TfrmThesaurusBrowser(Value);
end;  // TDiagramsMenu.Set_ThesaurusBrowser 

initialization
  TAutoObjectFactory.Create(ComServer, TDiagramsMenu, Class_DiagramsMenu,
    ciMultiInstance, tmApartment);
end.
