{===============================================================================
  Unit:        DropControlAssistor

  Defines:     EDropControlAssistor
               TDropControlAssistor

  Description: Drag/Drop interface layer for the SMART addin

  Created:     Sept 2006

  Last revision information:
    $Revision: 1 $
    $Date: 12/09/07 13:33 $
    $Author: Davidkelly $

===============================================================================}
unit DropControlAssistor;

interface

uses classes, Controls, Sysutils, DropTarget, DropStruct, DropSource, ActiveX,
    AddinLinkedControls, ExceptionForm, Messages, AxCtrls, Windows, Grids,
    StdCtrls, Clipbrd;

resourcestring
  ResStr_DragDropRegFailed = 'Registration for Drop capability failed for control ';

const
  WM_SETDRAGDROP = WM_APP + 1001;
  EST_NOT_COMPONENT = 'Internal Error - Registered drop control is not a component';

type
  EDropControlAssistor = class(TExceptionPath);

  TDropControlAssistor = class(TObject)
  private
    FDropComponentList: TStringList;
    FParent: TActiveForm;
    FRegisteredDragDropComponents: TList;
  protected
    procedure DoStandardPaste(iWinControl: TWinControl);
    function GetDropComponentIndex(iComponent: TObject): Integer;
  public
    constructor Create(AParent: TActiveForm); reintroduce; overload;
    destructor Destroy; override;
    procedure ExecutePaste(Sender: TObject);
    procedure RegisterDropComponent(AWinControl: TWinControl; ADropEvent:
        TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent;
        ATableList: array of String; AFormatList: array of Integer; ADragOverCheck:
        TDragOverCheckEvent);
  end;

implementation

type
  { allow access to the in place editor which is protected }
  TDummyCustomGrid = class(TCustomGrid);

{-------------------------------------------------------------------------------
  Initialise registration lists
}
constructor TDropControlAssistor.Create(AParent: TActiveForm);
begin
  inherited Create;
  FParent := AParent;
  FRegisteredDragDropComponents := TList.Create;
  FDropComponentList := TStringList.Create;
  PostMessage(FParent.Handle, WM_SETDRAGDROP, 0, 0);
end;

{-------------------------------------------------------------------------------
  Revoke all drop objects
}
destructor TDropControlAssistor.Destroy;
var
  i: integer;
begin
  if Assigned(FDropComponentList) then
    for i := 0 to FDropComponentList.Count - 1 do
      FDropComponentList.Objects[i].Free;
  FreeAndNil(FDropComponentList);
  if Assigned(FRegisteredDragDropComponents) then
    for i := 0 to FRegisteredDragDropComponents.Count - 1 do
      try
        RevokeDragDrop(TWinControl(FRegisteredDragDropComponents[i]).Handle);
      except
        on Exception do ; // will cause a memory leak but nothing else
      end;
  FreeAndNil(FRegisteredDragDropComponents);
  inherited;
end;

//==============================================================================
{ Paste a control which does not feature the CF_JNCCDATA format }
procedure TDropControlAssistor.DoStandardPaste(iWinControl: TWinControl);
var
  editor: TInplaceEdit;
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if iWinControl is TCustomEdit then
    TCustomEdit(iWinControl).PasteFromClipboard
  else
  if (iWinControl is TComboBox) then
    SendMessage(iWinControl.Handle, WM_PASTE, 0, 0)
  else
  if (iWinControl is TCustomGrid) then begin
    editor := TDummyCustomGrid(iWinControl).InPlaceEditor;
    if Assigned(editor) then SendMessage(editor.Handle, WM_PASTE, 0, 0)
  end
  {$IFDEF DEBUG}
  else
    raise TExceptionPath.CreateNonCritical(EST_COMPONENT_CANT_PASTE + iWinControl.Name)
  {$ENDIF};
end;  // DoStandardPaste

//==============================================================================
procedure TDropControlAssistor.ExecutePaste(Sender: TObject);
var
  i, lIndex: integer; // index in the component list
  lCapability: TClipboardCapability;
begin
  if Sender is TWinControl then begin
    if TWinControl(Sender).Parent is TAddinLinkedEdit then
      lIndex := GetDropComponentIndex(TWinControl(Sender).Parent)
    else
      lIndex := GetDropComponentIndex(Sender);
    if lIndex = -1 then
      DoStandardPaste(TWinControl(Sender))
    else begin
      lCapability := TClipboardCapability(FDropComponentList.Objects[lIndex]);
      for i := 0 to Clipboard.FormatCount - 1 do
        if lCapability.IsFormatSupported(Clipboard.Formats[i]) then
          lCapability.ActOnGlobalMemory(Clipboard.GetAsHandle(Clipboard.Formats[i]),
                                        Clipboard.Formats[i],
                                        TControl(Sender));
    end;
  end;
end;  // ExecutePaste

//==============================================================================
{ Find the component's position in the list of drop components. }
function TDropControlAssistor.GetDropComponentIndex(iComponent: TObject):
    Integer;
begin
  Result := -1;
  if Assigned(FDropComponentList) then begin
    if not (iComponent is TComponent) then
      raise EDropControlAssistor.Create(EST_NOT_COMPONENT);
    Result := FDropComponentList.IndexOf(TComponent(iComponent).Name);
  end;
end;  // GetDropComponentIndex

{-------------------------------------------------------------------------------
}
procedure TDropControlAssistor.RegisterDropComponent(AWinControl: TWinControl;
    ADropEvent: TDataDroppedEvent; AAdvancedDropEvent:
    TAdvancedDataDroppedEvent; ATableList: array of String; AFormatList: array
    of Integer; ADragOverCheck: TDragOverCheckEvent);
var
  lResult: HResult;
  lDropTarget: TJNCCDropTarget;
  lClipboardCapability: TClipboardCapability;
  lDropControl: TWinControl;
begin
  if Assigned(AWinControl) then begin
    // Create a drop target object
    if Assigned(ADropEvent) then
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList,
          ATableList,  ADropEvent, ADragOverCheck)
    else
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList,
          ATableList,  AAdvancedDropEvent, ADragOverCheck);
    // API call to set up OLE drag drop for the control
    lDropControl := AWinControl;
    if AWinControl is TAddinLinkedEdit then
      lDropControl := TAddinLinkedEdit(AWinControl).EditBox;
    lResult := RegisterDragDrop(lDropControl.Handle, lDropTarget as IDropTarget);
    // Remember so we can revoke drag and drop. RevokeDragDrop works on registered control.
    FRegisteredDragDropComponents.Add(lDropControl);
    if lResult <> S_OK then
      raise EDropControlAssistor.Create(ResStr_DragDropRegFailed + AWinControl.Name +
                                       #13'Result: ' + IntToStr(lResult));
  
    // NB The drop target object is reference counted so we don't need to free it
    // Setup the clipboard capability list
    if Assigned(ADropEvent) then
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList,
          ADropEvent)
    else
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList,
          AAdvancedDropEvent);
    FDropComponentList.AddObject(AWinControl.Name, lClipboardCapability);
  end;
end;  // TBaseDragFrame.RegisterDropComponent 

end.
