{===============================================================================
  Unit:        QuickEntryMenuItem

  Defines:     TQuickEntryMenuItem

  Description: Class that implements a single IDynamicMenu instance for the
               Quick Entry module.

  Created:     Oct 2003

  Last revision information:
    $Revision: 11 $
    $Date: 21/06/16 12:56 $
    $Author: Christopherknight $

===============================================================================}
unit QuickEntryMenuItem;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB, ADODB,
  Variants, Sysutils, GeneralData, QuickEntry_TLB, Classes;

type
  TMenuItemType = (mtQuickEntry, mtNewSessionMenu, mtExistingSessionMenu,
                   mtSeparator, mtQuickEntryManager, mtNewSession, mtExistingSession);

  {-----------------------------------------------------------------------------
    Base class for the specific menu item classes that are used to populate the Quick Entry 
    menu structure.
  }
  TInternalQuickEntryMenuItem = class (TObject)
  protected
    function DoExecute: IUnknown; virtual;
    function DoGetChild(AIndex: integer): IUnknown; virtual;
    function DoGetChildCount: Integer; virtual;
    function GetImageIndex: Integer; virtual;
    function GetItemCaption: String; virtual; abstract;
  end;
  
  {-----------------------------------------------------------------------------
    IDynamicMenu implementation for the Quick Entry menu structure and screens.  This class is 
    a wrapper for all the menu item types, the actual handling of the menu item is achieved by 
    assigning a TInternalQuickEntryMenuItem sub-class to the InternalMenuItem property.
  }
  TQuickEntryMenuItem = class (TAutoObject, IQuickEntryMenuItem, IDynamicMenu)
  private
    FInternalMenuItem: TInternalQuickEntryMenuItem;
    FInternalQuickEntryMenuItem: TInternalQuickEntryMenuItem;
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
    procedure SetInternalMenuItem(Value: TInternalQuickEntryMenuItem);
  public
    destructor Destroy; override;
    property InternalMenuItem: TInternalQuickEntryMenuItem read FInternalMenuItem write 
        SetInternalMenuItem;
    property InternalQuickEntryMenuItem: TInternalQuickEntryMenuItem read 
        FInternalQuickEntryMenuItem write FInternalQuickEntryMenuItem;
  end;
  
  TQuickEntryTopLevelMenuItem = class (TInternalQuickEntryMenuItem)
  private
    function GetExistingSessionCount: Integer;
    function GetTemplateCount: Integer;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  end;
  
  TSessionMenu = class (TInternalQuickEntryMenuItem)
  end;
  
  TSessionMenuItem = class (TInternalQuickEntryMenuItem)
  private
    FDataCaption: String;
    procedure SetDataCaption(const Value: String);
  protected
    function DoGetChildCount: Integer; override;
    function GetItemCaption: String; override;
  public
    property DataCaption: String read FDataCaption write SetDataCaption;
  end;
  
  TExistingSessionMenu = class (TSessionMenu)
  private
    FQETemplates: _Recordset;
    FQETemplatesRecordsetPosition: Integer;
    function GetQETemplates: _Recordset;
    property QETemplates: _Recordset read GetQETemplates;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  end;

  TLoadInToExistingSessionMenu = class(TSessionMenu)
  private
    FTemplates: _Recordset;
    FTemplatesRecordsetPosition: Integer;
    FSelectedNodeInfo: TNodeInfo;
    FCountStoredProcedureName: String;
    FStoredProcedureName: String;
    procedure SetTemplates;
    procedure SetStoredProcedureNames;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo);
  end;

  TLoadIntoQEFormMenu = class(TSessionMenuItem)
  private
    FTemplateKey: String;
    FSessions: _Recordset;
    FSelectedNodeInfo: TNodeInfo;
    FStoredProcedureName: String;
    FCountStoredProcedureName: String;
    FSessionsRecordsetPosition: Integer;
    FChildCount: Integer;
    procedure SetSessions;
    procedure SetStoredProcedureNames;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo; ATemplateKey: String);
  end;

  TQEFormNameMenu = class(TSessionMenuItem)
  private
    FQESessions: _Recordset;
    FQESessionsRecordsetPosition: Integer;
    FTemplateKey: String;
    function GetQESessions: _Recordset;
    property QESessions: _Recordset read GetQESessions;
    procedure SetTemplateKey(const Value: String);
  protected
    function DoGetChild(AIndex: Integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex : Integer; override;
    function GetItemCaption : String; override;
  public
    property TemplateKey: String read FTemplateKey write SetTemplateKey;
  end;

  TExistingSessionMenuItem = class (TSessionMenuItem)
  private
    FSessionKey: Integer;
    procedure SetSessionKey(Value: Integer);
  protected
    function DoExecute: IUnknown; override;
  public
    property SessionKey: Integer read FSessionKey write SetSessionKey;
  end;

  TNewSessionMenu = class (TSessionMenu)
  private
    FQETemplates: _Recordset;
    FQETemplatesRecordsetPosition: Integer;
    function GetQETemplates: _Recordset;
    property QETemplates: _Recordset read GetQETemplates;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  end;

  TLoadIntoNewSessionMenu = class(TSessionMenu)
  private
    FTemplates: _Recordset;
    FSelectedNodeInfo: TNodeInfo;
    FStoredProcedureName: String;
    FChildren: IInterfaceList;
    procedure SetTemplates;
    procedure SetStoredProcedureName;
    procedure CreateChildren;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo);
  end;

  TNewSessionMenuItem = class (TSessionMenuItem)
  private
    FTemplateKey: String;
    function CreateNewSession(const ATemplateKey: string): Integer;
    procedure SetTemplateKey(const Value: String);
  protected
    function DoExecute: IUnknown; override;
  public
    property TemplateKey: String read FTemplateKey write SetTemplateKey;
  end;
  
  TSeparatorMenuItem = class (TInternalQuickEntryMenuItem)
  protected
    function DoGetChildCount: Integer; override;
    function GetItemCaption: String; override;
  end;
  
  TQuickEntryManagerMenuItem = class (TInternalQuickEntryMenuItem)
  protected
    function DoExecute: IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetImageIndex: Integer; override;
    function GetItemCaption: String; override;
  end;

  {-----------------------------------------------------------------------------
    Wrapper for the 'Load Into Quick Item' menu.
  }
  TLoadIntoQuickEntryMenu = class (TAutoObject, IDynamicMenuList)
  private
    FQuickEntryMenuItem: IDynamicMenu;
    function Get_Count: Integer; safecall;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo);
    function Get_ImageListHandle: Integer; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Items(AIndex: Integer): IDynamicMenu; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
  end;

  {-----------------------------------------------------------------------------
    Top level item for the 'Load Into Quick Item' menu.
  }
  TLoadIntoQuickEntryTopLevelMenuItem = class (TSessionMenu)
  private
    FSelectedNodeInfo: TNodeInfo;
  protected
    function DoGetChild(AIndex: integer): IUnknown; override;
    function DoGetChildCount: Integer; override;
    function GetItemCaption: String; override;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo);
  end;

  {-----------------------------------------------------------------------------
    A quick entry template item for the 'Load Into Quick Item' menu.
  }
  TLoadIntoQuickEntryTemplateItem = class (TSessionMenuItem)
  private
    FTemplateKey: String;
    FSelectedNodeInfo: TNodeInfo;
    FOccurrenceKey: String;
    FTaxonOccurrenceKey: String;
    FSessionKey: Integer;
    FIsExistingSession: Boolean;
    function CreateNewSession(const ATemplateKey: string): Integer;
    procedure AddDataRowsToSession(
      const ASessionKey: Integer;
      const ANumberOfRowsToAdd: Integer);
    procedure AddDataItemsToSession(const ASessionKey: Integer);
    procedure AddDataItemsToExistingSession(
      const ASessionKey: Integer;
      const ANumberOfRowsToAdd: Integer);
    procedure SetTemplateKey(const Value: String);
    procedure SetItemKeys;
    procedure SetSessionKey(const Value: Integer);
    function GetSessionKey: Integer;
    function GetNumberOfSpecimens: Integer;
  protected
    function DoExecute: IUnknown; override;
  public
    constructor Create(ASelectedNodeInfo: TNodeInfo);
    property TemplateKey: String read FTemplateKey write SetTemplateKey;
    property SessionKey: Integer read GetSessionKey write SetSessionKey;
  end;

//==============================================================================
implementation

uses
  ComServ, ResourceStrings, QENumberOfSpecimens, Controls;

{-==============================================================================
    TInternalQuickEntryMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TInternalQuickEntryMenuItem.DoExecute: IUnknown;
begin
  Result := nil;
end;  // TInternalQuickEntryMenuItem.DoExecute 

{-------------------------------------------------------------------------------
}
function TInternalQuickEntryMenuItem.DoGetChild(AIndex: integer): IUnknown;
begin
  Result := nil;
end;  // TInternalQuickEntryMenuItem.DoGetChild 

{-------------------------------------------------------------------------------
}
function TInternalQuickEntryMenuItem.DoGetChildCount: Integer;
begin
  Result := 0;
end;  // TInternalQuickEntryMenuItem.DoGetChildCount 

{-------------------------------------------------------------------------------
  Virtual method, overriden to return the appropriate image index. 
}
function TInternalQuickEntryMenuItem.GetImageIndex: Integer;
begin
  Result := -1;
end;  // TInternalQuickEntryMenuItem.GetImageIndex 

{-==============================================================================
    TQuickEntryMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TQuickEntryMenuItem.Destroy;
begin
  FInternalMenuItem.Free;
  inherited Destroy;
end;  // TQuickEntryMenuItem.Destroy 

{-------------------------------------------------------------------------------
}
function TQuickEntryMenuItem.Child(Index: Integer): IUnknown;
begin
  Result := FInternalMenuItem.DoGetChild(Index);
end;  // TQuickEntryMenuItem.Child 

{-------------------------------------------------------------------------------
}
function TQuickEntryMenuItem.Execute: IUnknown;
begin
  Result := FInternalMenuItem.DoExecute;
end;  // TQuickEntryMenuItem.Execute 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryMenuItem.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := FInternalMenuItem.GetItemCaption;
    Enabled := True;
    Hint := Caption;
    ImageIndex := FInternalMenuItem.GetImageIndex;
    Shortcut := 0;
  end;
end;  // TQuickEntryMenuItem.GetItemData 

{-------------------------------------------------------------------------------
}
function TQuickEntryMenuItem.Get_ChildCount: Integer;
begin
  Result := FInternalMenuItem.DoGetChildCount;
end;  // TQuickEntryMenuItem.Get_ChildCount 

{-------------------------------------------------------------------------------
}
function TQuickEntryMenuItem.Get_HasSubmenu: WordBool;
begin
  Result :=FInternalMenuItem.DoGetChildCount>0;
end;  // TQuickEntryMenuItem.Get_HasSubmenu 

{-------------------------------------------------------------------------------
}
procedure TQuickEntryMenuItem.SetInternalMenuItem(Value: TInternalQuickEntryMenuItem);
begin
  FInternalMenuItem := Value;
end;  // TQuickEntryMenuItem.SetInternalMenuItem 

{-==============================================================================
    TQuickEntryTopLevelMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TQuickEntryTopLevelMenuItem.DoGetChild(AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
  lCount: Integer;
begin
  lItem := TQuickEntryMenuItem.Create;
  lCount := DoGetChildCount;
  if AIndex = lCount - 1 then
    // last item must be Manager
    lItem.InternalMenuItem := TQuickEntryManagerMenuItem.Create
  else
  if AIndex = lCount - 2 then
    // 2nd to last item must be separator, otherwise there'd be only one item in menu.
    lItem.InternalMenuItem := TSeparatorMenuItem.Create
  else
  if (AIndex = 0) and (GetTemplateCount > 0) then
    // If there are any templates, they go under first item (0)
    lItem.InternalMenuItem := TNewSessionMenu.Create
  else
    // All the rest should be sessions going in the Existing Session sub menu.
    lItem.InternalMenuItem := TExistingSessionMenu.Create;
  Result := lItem as IUnknown;
end;  // TQuickEntryTopLevelMenuItem.DoGetChild 

{-------------------------------------------------------------------------------
}
function TQuickEntryTopLevelMenuItem.DoGetChildCount: Integer;
begin
  // Because it is "dynamic", count needs to be calculated everytime.
  Result := 1;
  if GetTemplateCount > 0 then
    Inc(Result);
  if GetExistingSessionCount > 0 then
    Inc(Result);
  if Result > 1 then
    Inc(Result); // for separator
end;  // TQuickEntryTopLevelMenuItem.DoGetChildCount

{-------------------------------------------------------------------------------
}
function TQuickEntryTopLevelMenuItem.GetExistingSessionCount: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QESessionCount_Get', [],
      '@Count');
end;  // TQuickEntryTopLevelMenuItem.GetExistingSessionCount 

{-------------------------------------------------------------------------------
  Return the correct image index for the item. 
}
function TQuickEntryTopLevelMenuItem.GetImageIndex: Integer;
begin
  Result := 0;
end;  // TQuickEntryTopLevelMenuItem.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TQuickEntryTopLevelMenuItem.GetItemCaption: String;
begin
  Result := ResStr_MnuQuickEntry;
end;  // TQuickEntryTopLevelMenuItem.GetItemCaption 

{-------------------------------------------------------------------------------
}
function TQuickEntryTopLevelMenuItem.GetTemplateCount: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QETemplateCount_Get', [],
      '@Count');
end;  // TQuickEntryTopLevelMenuItem.GetTemplateCount 

{-==============================================================================
    TSessionMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSessionMenuItem.DoGetChildCount: Integer;
begin
  Result := 0;
end;  // TSessionMenuItem.DoGetChildCount 

{-------------------------------------------------------------------------------
}
function TSessionMenuItem.GetItemCaption: String;
begin
  Result := FDataCaption;
end;  // TSessionMenuItem.GetItemCaption 

{-------------------------------------------------------------------------------
}
procedure TSessionMenuItem.SetDataCaption(const Value: String);
begin
  FDataCaption := Value;
end;  // TSessionMenuItem.SetDataCaption 

{-==============================================================================
    TQEFormNameMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TQEFormNameMenu.SetTemplateKey(const Value: String);
begin
  FTemplateKey := Value;
end;  // TQEFormNameMenu.SetTemplateKey

{-------------------------------------------------------------------------------
}
function TQEFormNameMenu.DoGetChild(AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
begin
  lItem := TQuickEntryMenuItem.Create;
  lItem.InternalMenuItem := TExistingSessionMenuItem.Create;
  // If the recordset has overshot, then go back to beginning.  This shouldn't
  // normally happen
  if FQESessionsRecordsetPosition>AIndex then begin
    QESessions.MoveFirst;
    FQESessionsRecordsetPosition := 0;
  end;
  // Wind the recordset forward to locate the correct requested item
  while FQESessionsRecordsetPosition<AIndex do begin
    QESessions.MoveNext;
    Inc(FQESessionsRecordsetPosition);
  end;
  with TExistingSessionMenuItem(lItem.InternalMenuItem) do begin
    SessionKey := QESessions.Fields['QE_Session_Key'].Value;
    DataCaption := VarToStr(QESessions.Fields['Item_Name'].Value);
  end; // with
  Result := lItem As IUnknown;
end;

{-------------------------------------------------------------------------------
}
function TQEFormNameMenu.DoGetChildCount: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QESessionFormCount_Get', ['@QE_Template_Key', FTemplateKey],
      '@Count');
end;  // TQEFormNameMenu.DoGetChildCount 

{-------------------------------------------------------------------------------
  Return the correct image index for the item. 
}
function TQEFormNameMenu.GetImageIndex: Integer;
begin
  Result := 3;
end;  // TQEFormNameMenu.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TQEFormNameMenu.GetItemCaption: String;
begin
  Result := DataCaption;
end;  // TQEFormNameMenu.GetItemCaption 

{-------------------------------------------------------------------------------
}
function TQEFormNameMenu.GetQESessions: _Recordset;
begin
  if not assigned(FQESessions) then begin
    FQESessions := dmGeneral.GetRecordset('usp_QESessionsForm_Select', ['@QE_Template_Key', FTemplateKey]);
    FQESessions.MoveFirst;
    // Keep manual track of the record position
    FQESessionsRecordsetPosition := 0;
  end;
  Result := FQESessions;
end;  // TQEFormNameMenu.GetQESessions

{-==============================================================================
    TExistingSessionMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
function TExistingSessionMenu.DoGetChild(AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
begin
  lItem := TQuickEntryMenuItem.Create;
  lItem.InternalMenuItem := TQEFormNameMenu.Create;
  // If the recordset has overshot, then go back to beginning.  This shouldn't
  // normally happen
  if FQETemplatesRecordsetPosition>AIndex then begin
    QETemplates.MoveFirst;
    FQETemplatesRecordsetPosition := 0;
  end;
  // Wind the recordset forward to locate the correct requested item
  while FQETemplatesRecordsetPosition<AIndex do begin
    QETemplates.MoveNext;
    Inc(FQETemplatesRecordsetPosition);
  end;
  with TQEFormNameMenu(lItem.InternalMenuItem) do begin
    TemplateKey := VarToStr(QETemplates.Fields['QE_Template_Key'].Value);
    DataCaption := VarToStr(QETemplates.Fields['Item_Name'].Value);
  end; // with
  Result := lItem As IUnknown;
end;  // TExistingSessionMenu.DoGetChild

{-------------------------------------------------------------------------------
}
function TExistingSessionMenu.DoGetChildCount: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QESessionFormNamesCount_Get', [],
      '@Count');
end;  // TExistingSessionMenu.DoGetChildCount 

{-------------------------------------------------------------------------------
  Return the correct image index for the item.
}
function TExistingSessionMenu.GetImageIndex: Integer;
begin
  Result := 3;
end;  // TExistingSessionMenu.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TExistingSessionMenu.GetItemCaption: String;
begin
  Result := ResStr_MnuExistingEntrySession;
end;  // TExistingSessionMenu.GetItemCaption 

{-------------------------------------------------------------------------------
}
function TExistingSessionMenu.GetQETemplates: _Recordset;
begin
  if not assigned(FQETemplates) then begin
    FQETemplates := dmGeneral.GetRecordset('usp_QESessionsFormNames_Select', []);
    FQETemplates.MoveFirst;
    // Keep manual track of the record position
    FQETemplatesRecordsetPosition := 0;
  end;
  Result := FQETemplates;
end;  // TExistingSessionMenu.GetQESessions 

{-==============================================================================
    TExistingSessionMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TExistingSessionMenuItem.DoExecute: IUnknown;
begin
  Result := CreateComObject(CLASS_QuickEntryForm);
  (Result as IQuickEntryForm).SessionKey := FSessionKey;
end;  // TExistingSessionMenuItem.DoExecute 

{-------------------------------------------------------------------------------
}
procedure TExistingSessionMenuItem.SetSessionKey(Value: Integer);
begin
  FSessionKey := Value;
end;  // TExistingSessionMenuItem.SetSessionKey

{-==============================================================================
    TNewSessionMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
function TNewSessionMenu.DoGetChild(AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
begin
  lItem := TQuickEntryMenuItem.Create;
  lItem.InternalMenuItem := TNewSessionMenuItem.Create;
  // If the recordset has overshot, then go back to beginning.  This shouldn't
  // normally happen
  if FQETemplatesRecordsetPosition>AIndex then begin
    QETemplates.MoveFirst;
    FQETemplatesRecordsetPosition := 0;
  end;
  // Wind the recordset forward to locate the correct requested item
  while FQETemplatesRecordsetPosition<AIndex do begin
    QETemplates.MoveNext;
    Inc(FQETemplatesRecordsetPosition);
  end;
  with TNewSessionMenuItem(lItem.InternalMenuItem) do begin
    TemplateKey := VarToStr(QETemplates.Fields['QE_Template_Key'].Value);
    DataCaption := VarToStr(QETemplates.Fields['Item_Name'].Value);
  end; // with
  Result := lItem As IUnknown;
end;  // TNewSessionMenu.DoGetChild

{-------------------------------------------------------------------------------
}
function TNewSessionMenu.DoGetChildCount: Integer;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QETemplateCount_Get', [],
      '@Count');
end;  // TNewSessionMenu.DoGetChildCount

{-------------------------------------------------------------------------------
  Return the correct image index for the item.
}
function TNewSessionMenu.GetImageIndex: Integer;
begin
  Result := 2;
end;  // TNewSessionMenu.GetImageIndex

{-------------------------------------------------------------------------------
}
function TNewSessionMenu.GetItemCaption: String;
begin
  Result := ResStr_MnuNewEntrySession;
end;  // TNewSessionMenu.GetItemCaption 

{-------------------------------------------------------------------------------
}
function TNewSessionMenu.GetQETemplates: _Recordset;
begin
  if not assigned(FQETemplates) then begin
    FQETemplates := dmGeneral.GetRecordset('usp_QETemplates_Select_Available', []);
    FQETemplates.MoveFirst;
    // Keep manual track of the record position
    FQETemplatesRecordsetPosition := 0;
  end;
  Result := FQETemplates;
end;  // TNewSessionMenu.GetQETemplates

{-==============================================================================
    TNewSessionMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TNewSessionMenuItem.CreateNewSession(const ATemplateKey: string): Integer;
begin
  Result := dmGeneral.RunInsertStoredProc('QE_Session', 'usp_QESession_Insert',
                ['@QE_Template_Key', ATemplateKey], '@Key');
end;  // TNewSessionMenuItem.CreateNewSession 

{-------------------------------------------------------------------------------
}
function TNewSessionMenuItem.DoExecute: IUnknown;
begin
  Result := CreateComObject(CLASS_QuickEntryForm);
  (Result as IQuickEntryForm).SessionKey := CreateNewSession(FTemplateKey);
  (Result as IQuickEntryForm).IsNewSession := true;
end;  // TNewSessionMenuItem.DoExecute 

{-------------------------------------------------------------------------------
}
procedure TNewSessionMenuItem.SetTemplateKey(const Value: String);
begin
  FTemplateKey := Value;
end;  // TNewSessionMenuItem.SetTemplateKey

{-==============================================================================
    TSeparatorMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSeparatorMenuItem.DoGetChildCount: Integer;
begin
  Result := 0;
end;  // TSeparatorMenuItem.DoGetChildCount 

{-------------------------------------------------------------------------------
}
function TSeparatorMenuItem.GetItemCaption: String;
begin
  Result := '-';
end;  // TSeparatorMenuItem.GetItemCaption 

{-==============================================================================
    TQuickEntryManagerMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
function TQuickEntryManagerMenuItem.DoExecute: IUnknown;
begin
  Result := CreateComObject(CLASS_frmQuickEntryManager);
end;  // TQuickEntryManagerMenuItem.DoExecute 

{-------------------------------------------------------------------------------
}
function TQuickEntryManagerMenuItem.DoGetChildCount: Integer;
begin
  Result := 0;
end;  // TQuickEntryManagerMenuItem.DoGetChildCount 

{-------------------------------------------------------------------------------
  Return the correct image index for the item. 
}
function TQuickEntryManagerMenuItem.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TQuickEntryManagerMenuItem.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TQuickEntryManagerMenuItem.GetItemCaption: String;
begin
  Result := ResStr_MnuQuickEntryManager;
end;  // TQuickEntryManagerMenuItem.GetItemCaption

{-==============================================================================
    TLoadIntoQuickEntryMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadIntoQuickEntryMenu.Create(ASelectedNodeInfo: TNodeInfo);
var
  lQuickEntryMenuItem: TQuickEntryMenuItem;
begin
  // Create a COM wrapper class for the menu item
  lQuickEntryMenuItem := TQuickEntryMenuItem.Create;
  // Assign the appropriate class to manage the menu item
  lQuickEntryMenuItem.InternalMenuItem
      := TLoadIntoQuickEntryTopLevelMenuItem.Create(ASelectedNodeInfo);
  FQuickEntryMenuItem := lQuickEntryMenuItem;
end;

function TLoadIntoQuickEntryMenu.Get_Count: Integer;
begin
  Result := 1;
end;

{-------------------------------------------------------------------------------
  Handle to image list that exposes images for the menu items.
}
function TLoadIntoQuickEntryMenu.Get_ImageListHandle: Integer;
begin
  Result := 0;
end;  // TLoadIntoQuickEntryMenu.Get_ImageListHandle

function TLoadIntoQuickEntryMenu.Items(AIndex: Integer): IDynamicMenu;
begin
  if AIndex >= Get_Count then Result := nil
  else
  begin
    Result := FQuickEntryMenuItem;
  end;
end;  // TCollectionsMenus.Items

{-------------------------------------------------------------------------------
  Path to the menu that the item identified by the index is created on. 
}
function TLoadIntoQuickEntryMenu.Get_MenuPath(AIndex: Integer): WideString;
begin
    if AIndex >= Get_Count then Result := ''
    else
    begin
      Result := ResStr_MnuQuickEntry;
    end;
end;  // TLoadIntoQuickEntryMenu.Get_MenuPath

{-------------------------------------------------------------------------------
  Menu item to insert the item after, for the item identified by the index
}
function TLoadIntoQuickEntryMenu.Get_InsertAfterMenu(AIndex: Integer): WideString;
begin
    if AIndex >= Get_Count then Result := ''
    else
    begin
      Result := 'Load into Record Card';
    end;
end;  // TLoadIntoQuickEntryMenu.Get_InsertAfterMenu

{-------------------------------------------------------------------------------
  Not used
}
function TLoadIntoQuickEntryMenu.Get_InsertBeforeMenu(AIndex: Integer): WideString;
begin
  Result := '';
end;  // TLoadIntoQuickEntryMenu.Get_InsertBeforeMenu

{-==============================================================================
    TLoadIntoQuickEntryTopLevelMenuItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadIntoQuickEntryTopLevelMenuItem.Create(
    ASelectedNodeInfo: TNodeInfo);
begin
  inherited Create;

  FSelectedNodeInfo := ASelectedNodeInfo;
end;

function TLoadIntoQuickEntryTopLevelMenuItem.DoGetChild(AIndex: integer): IUnknown;
  var lItem: TQuickEntryMenuItem;
begin
  lItem := TQuickEntryMenuItem.Create;
  if AIndex = 0 then
    lItem.InternalMenuItem := TLoadIntoNewSessionMenu.Create(FSelectedNodeInfo)
  else
  if AIndex = 1 then
     lItem.InternalMenuItem := TLoadIntoExistingSessionMenu.Create(FSelectedNodeInfo);

  Result := lItem as IUnknown;
end;  // TLoadIntoQuickEntryTopLevelMenuItem.DoGetChild 

{-------------------------------------------------------------------------------
}
function TLoadIntoQuickEntryTopLevelMenuItem.DoGetChildCount: Integer;
begin
  Result := 2;
end;  // TLoadIntoQuickEntryTopLevelMenuItem.DoGetChildCount

{-------------------------------------------------------------------------------
}
function TLoadIntoQuickEntryTopLevelMenuItem.GetItemCaption: String;
begin
  Result := ResStr_MnuLoadIntoQuickEntry;
end;  // TLoadIntoQuickEntryTopLevelMenuItem.GetItemCaption

{-==============================================================================
    TLoadIntoQuickEntryTemplateItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadIntoQuickEntryTemplateItem.Create(ASelectedNodeInfo: TNodeInfo);
begin
  inherited Create;
  FSelectedNodeInfo := ASelectedNodeInfo;
  FIsExistingSession := true;
  SetItemKeys;
end;

{-------------------------------------------------------------------------------
  Sets the values of FTaxonOccurrenceKey & FOccurrenceKey according the
  table name of ASelectedNodeInfo.
}
procedure TLoadIntoQuickEntryTemplateItem.SetItemKeys;
begin
  if CompareText(FSelectedNodeInfo.TableName, 'Taxon_Occurrence') = 0 then
    FTaxonOccurrenceKey := FSelectedNodeInfo.ItemKey
  else
    FOccurrenceKey := FSelectedNodeInfo.ItemKey
end;

{-------------------------------------------------------------------------------
   Sets the session key.
}
procedure TLoadIntoQuickEntryTemplateItem.SetSessionKey(const Value:Integer);
begin
  FSessionKey := Value;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoQuickEntryTemplateItem.DoExecute: IUnknown;
var
  lNumberOfRowsToAdd, lSessionKey: Integer;
begin
  Result := nil;
  lNumberOfRowsToAdd := GetNumberOfSpecimens;

  if lNumberOfRowsToAdd <> 0 then
  begin
    lSessionKey := GetSessionKey;
    if FIsExistingSession then
      AddDataItemsToExistingSession(lSessionKey, lNumberOfRowsToAdd)
    else
    begin
      AddDataRowsToSession(lSessionKey, lNumberOfRowsToAdd);
      AddDataItemsToSession(lSessionKey);
    end;

    Result := CreateComObject(CLASS_QuickEntryForm);
    (Result as IQuickEntryForm).SessionKey := lSessionKey;
  end;
end;  // TLoadIntoQuickEntryTemplateItem.DoExecute

{-------------------------------------------------------------------------------
  Prompts the user for the number of specimens to enter and returns this value.
}
function TLoadIntoQuickEntryTemplateItem.GetNumberOfSpecimens: Integer;
var
  lDlgQENumberOfSpecimens: TdlgQENumberOfSpecimens;
begin
  Result := 0;
  lDlgQENumberOfSpecimens := TdlgQENumberOfSpecimens.Create(nil);
  try
    if lDlgQENumberOfSpecimens.ShowModal = mrOk then
        Result := lDlgQENumberOfSpecimens.NumberOfSpecimens;
  finally
    lDlgQENumberOfSpecimens.Release;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the session key, if the session has not yet been set then a new
  one is created.
}
function TLoadIntoQuickEntryTemplateItem.GetSessionKey: Integer;
begin
  if FSessionKey = 0 then
    FSessionKey := CreateNewSession(FTemplateKey);

  Result := FSessionKey;
end;

{-------------------------------------------------------------------------------
  Inserts a new QE session into the database using the given template.
}
function TLoadIntoQuickEntryTemplateItem.CreateNewSession(
  const ATemplateKey: string): Integer;
begin
  FIsExistingSession := false;

  Result := dmGeneral.RunInsertStoredProc(
      'QE_Session',
      'usp_QESession_Insert',
      ['@QE_Template_Key', ATemplateKey,
          '@Occurrence_Key', FOccurrenceKey,
          '@Taxon_Occurrence_Key', FTaxonOccurrenceKey],
      '@Key');
end;  // TLoadIntoQuickEntryTemplateItem.CreateNewSession

{-------------------------------------------------------------------------------
  Adds the specified number of rows to the given Quick Entry session.
}
procedure TLoadIntoQuickEntryTemplateItem.AddDataRowsToSession(
  const ASessionKey: Integer;
  const ANumberOfRowsToAdd:Integer);
var
  i: Integer;
begin
  for i := 0 to ANumberOfRowsToAdd - 1 do
  begin
    dmGeneral.GetRecordset(
        'usp_QEDataRow_Insert',
        ['@QESessionKey', ASessionKey]);
  end;
end;

{-------------------------------------------------------------------------------
  Adds data items to the given session using the data of the session's
  taxon occurrence or thesaurus occurrence
}
procedure TLoadIntoQuickEntryTemplateItem.AddDataItemsToSession(
  const ASessionKey: Integer);
var
  lStoredProcedureName: String;
begin
  if FTaxonOccurrenceKey <> '' then
      lStoredProcedureName := 'usp_QEDataItem_Insert_ForTaxonOccurrence'
  else
      lStoredProcedureName := 'usp_QEDataItem_Insert_ForOccurrence';

  dmGeneral.RunStoredProc(
      lStoredProcedureName,
      ['@QE_Session_Key', ASessionKey])
end;

{-------------------------------------------------------------------------------
  Calls stored procedures to add an occurrence AnumberOfRowsToAdd times to an
  existing session.
}
procedure TLoadIntoQuickEntryTemplateItem.AddDataItemsToExistingSession(
  const ASessionKey: Integer;
  const ANumberOfRowsToAdd:Integer);
var
  lStoredProcedureName: String;
begin
  if FTaxonOccurrenceKey <> '' then
  begin
      lStoredProcedureName := 'usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence';
      dmGeneral.RunStoredProc(
      lStoredProcedureName,
      ['@QE_Session_Key', ASessionKey,
          '@Taxon_Occurrence_Key', FTaxonOccurrenceKey,
          '@NumberOfRows', ANumberOfRowsToAdd]);
  end
  else
  begin
      lStoredProcedureName := 'usp_QEDataItem_Insert_ForSecondaryOccurrence';
      dmGeneral.RunStoredProc(
        lStoredProcedureName,
        ['@QE_Session_Key', ASessionKey,
            '@Occurrence_Key', FOccurrenceKey,
            '@NumberOfRows', ANumberOfRowsToAdd]) ;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoQuickEntryTemplateItem.SetTemplateKey(const Value: String);
begin
  FTemplateKey := Value;
end;  // TLoadIntoQuickEntryTemplateItem.SetTemplateKey

{-==============================================================================
    TLoadIntoNewSessionMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadIntoNewSessionMenu.Create(ASelectedNodeInfo: TNodeInfo);
begin
  inherited Create;

  FSelectedNodeInfo := ASelectedNodeInfo;
  SetStoredProcedureName;
  SetTemplates;
  CreateChildren;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoNewSessionMenu.SetTemplates;
begin
  if not assigned(FTemplates) then begin
    FTemplates := dmGeneral.GetRecordset(
        FStoredProcedureName,
        ['@Item_Key', FSelectedNodeInfo.ItemKey]);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoNewSessionMenu.SetStoredProcedureName;
begin
  if CompareText(FSelectedNodeInfo.TableName, 'Taxon_Occurrence') = 0 then
  begin
    FStoredProcedureName := 'usp_QETemplates_Select_ForTaxonOccurrence';
  end
  else
  begin
    FStoredProcedureName := 'usp_QETemplates_Select_ForOccurrence';
  end
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoNewSessionMenu.CreateChildren;
var
  lItem: TQuickEntryMenuItem;
begin
  FChildren := TInterfaceList.Create;

  while not FTemplates.EOF do begin
    lItem := TQuickEntryMenuItem.Create;
    lItem.InternalMenuItem
        := TLoadIntoQuickEntryTemplateItem.Create(FSelectedNodeInfo);

    with TLoadIntoQuickEntryTemplateItem(lItem.InternalMenuItem) do begin
      TemplateKey := VarToStr(FTemplates.Fields['QE_Template_Key'].Value);
      DataCaption := VarToStr(FTemplates.Fields['Item_Name'].Value);
    end; // with

    FChildren.Add(lItem);
    FTemplates.MoveNext;
  end;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoNewSessionMenu.DoGetChild(AIndex: integer): IUnknown;
begin
  Result := FChildren[AIndex];
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoNewSessionMenu.DoGetChildCount: Integer;
begin
  Result := FTemplates.RecordCount;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoNewSessionMenu.GetImageIndex: Integer;
begin
  Result := 2;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoNewSessionMenu.GetItemCaption: String;
begin
  Result := ResStr_MnuNewEntrySession;
end;

{-==============================================================================
    TLoadInToExistingSessionMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadInToExistingSessionMenu.Create(
  ASelectedNodeInfo: TNodeInfo);
begin
  inherited Create;
  FSelectedNodeInfo := ASelectedNodeInfo;
  SetStoredProcedureNames;
end;



{-------------------------------------------------------------------------------
}
function TLoadInToExistingSessionMenu.DoGetChild(
  AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
begin
    SetTemplates;
    lItem := TQuickEntryMenuItem.Create;

    if FTemplatesRecordsetPosition>AIndex then begin
      FTemplates.MoveFirst;
      FTemplatesRecordsetPosition := 0;
    end;

    while FTemplatesRecordsetPosition<AIndex do begin
      FTemplates.MoveNext;
      Inc(FTemplatesRecordsetPosition);
    end;

    lItem.InternalMenuItem
        := TLoadIntoQEFormMenu.Create(
              FSelectedNodeInfo,
              VarToStr(FTemplates.Fields['QE_Template_Key'].Value));

    with TLoadIntoQEFormMenu(lItem.InternalMenuItem) do begin
      DataCaption := VarToStr(FTemplates.Fields['Item_Name'].Value);
    end;

  Result := lItem as IUnknown; 
end;

{-------------------------------------------------------------------------------
}
function TLoadInToExistingSessionMenu.DoGetChildCount: Integer;
begin
  //Result := FTemplates.RecordCount;
  Result := dmGeneral.GetStoredProcOutputParam(FCountStoredProcedureName, ['@Item_Key', FSelectedNodeInfo.ItemKey],
      '@Count');
  //Result := 1;
end;

{-------------------------------------------------------------------------------
}
function TLoadInToExistingSessionMenu.GetImageIndex: Integer;
begin
  Result := 3;
end;

{-------------------------------------------------------------------------------
}
function TLoadInToExistingSessionMenu.GetItemCaption: String;
begin
  Result := ResStr_MnuExistingEntrySession;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadInToExistingSessionMenu.SetStoredProcedureNames;
begin
  if CompareText(FSelectedNodeInfo.TableName, 'Taxon_Occurrence') = 0 then
  begin
    FStoredProcedureName := 'usp_QESessionsFormNames_Select_ForTaxonOccurence';
    FCountStoredProcedureName := 'usp_QESessionsFormNames_Count_ForTaxonOccurence';
  end
  else
  begin
    FStoredProcedureName := 'usp_QESessionsFormNames_Select_ForOccurence';
    FCountStoredProcedureName := 'usp_QESessionsFormNames_Count_ForOccurence';
  end
end;

{-------------------------------------------------------------------------------
}
procedure TLoadInToExistingSessionMenu.SetTemplates;
begin
  if not assigned(FTemplates) then begin
    FTemplates := dmGeneral.GetRecordset(
        FStoredProcedureName,
        ['@Item_Key', FSelectedNodeInfo.ItemKey]);
  end;
end;


{-==============================================================================
    TLoadIntoQEFormMenu
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLoadIntoQEFormMenu.Create(ASelectedNodeInfo: TNodeInfo; ATemplateKey: String);
begin
  inherited Create;
  FChildCount := 0;
  FSelectedNodeInfo := ASelectedNodeInfo;
  FTemplateKey := ATemplateKey;
  SetStoredProcedureNames;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoQEFormMenu.DoGetChild(AIndex: integer): IUnknown;
var
  lItem: TQuickEntryMenuItem;
begin
    SetSessions;
    lItem := TQuickEntryMenuItem.Create;
    lItem.InternalMenuItem
        := TLoadIntoQuickEntryTemplateItem.Create(FSelectedNodeInfo);

    if FSessionsRecordsetPosition>AIndex then begin
      FSessions.MoveFirst;
      FSessionsRecordsetPosition := 0;
    end;

    while FSessionsRecordsetPosition<AIndex do begin
      FSessions.MoveNext;
      Inc(FSessionsRecordsetPosition);
    end;

    with TLoadIntoQuickEntryTemplateItem(lItem.InternalMenuItem) do begin
      TemplateKey := FTemplateKey;
      SessionKey :=  FSessions.Fields['QE_Session_Key'].Value;
      DataCaption := VarToStr(FSessions.Fields['Item_Name'].Value);
    end;
  Result := lItem as IUnknown;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoQEFormMenu.DoGetChildCount: Integer;
begin
  //Result := FSessions.RecordCount;
  FChildCount := dmGeneral.GetStoredProcOutputParam(FCountStoredProcedureName, ['@QE_Template_Key', FTemplateKey, '@Item_Key', FSelectedNodeInfo.ItemKey],
      '@Count');
      //Result := 0;
  Result := FChildCount;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoQEFormMenu.GetImageIndex: Integer;
begin
  Result := 3;
end;

{-------------------------------------------------------------------------------
}
function TLoadIntoQEFormMenu.GetItemCaption: String;
begin
  Result := DataCaption;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoQEFormMenu.SetSessions;
begin
  if not assigned(FSessions) then begin
    FSessions := dmGeneral.GetRecordset(
        FStoredProcedureName,
        ['@QE_Template_Key', FTemplateKey , '@Item_Key', FSelectedNodeInfo.ItemKey]);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TLoadIntoQEFormMenu.SetStoredProcedureNames;
begin
  if CompareText(FSelectedNodeInfo.TableName, 'Taxon_Occurrence') = 0 then
  begin
    FStoredProcedureName := 'usp_QESessionsForm_Select_ForTaxonOccurence';
    FCountStoredProcedureName := 'usp_QESessionsForm_Count_ForTaxonOccurence';
  end
  else
  begin
    FStoredProcedureName := 'usp_QESessionsForm_Select_ForOccurence';
    FCountStoredProcedureName := 'usp_QESessionsForm_Count_ForOccurence';
  end
end;

initialization
  TAutoObjectFactory.Create(ComServer, TQuickEntryMenuItem,
      Class_QuickEntryMenuItem,
    ciMultiInstance, tmApartment);
end.
