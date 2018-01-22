{===============================================================================
  Unit:        DragDropControlAssistor

  Defines:     EDragDropControlAssistor
               TDragDropControlAssistor

  Description: Drag/Drop interface layer for addins

  Created:     Oct 2007

  Last revision information:
    $Revision: 1 $
    $Date: 29/10/07 10:25 $
    $Author: Davidkelly $

===============================================================================}
unit DragDropControlAssistor;

interface

uses Classes, Controls, Sysutils, DropTarget, DropStruct, DropSource, ActiveX,
    AddinLinkedControls, ExceptionForm, Messages, AxCtrls, Windows, Grids,
    StdCtrls, Clipbrd;

resourcestring
  ResStr_DragDropRegFailed = 'Registration for Drop capability failed for control ';

const
  WM_SETDRAGDROP = WM_APP + 1001;
  EST_NOT_COMPONENT = 'Internal Error - Registered drop control is not a component';

type
  EDragDropControlAssistor = class(TExceptionPath);
  
  {-----------------------------------------------------------------------------
    To allow access to the protected OnStartDrag and Text properties in TControl.
  }
  TControlAccessor = class (TControl)
  public
    property Text;
  end;

  {-----------------------------------------------------------------------------
    To allow access to the in-place editor in grids.
  }
  TCustomGridAccessor = class (TCustomGrid)
  public
    property InplaceEditor;
  end;

  TDragDropControlAssistor = class(TObject)
  private
    FDragComponentList: TStringList;
    FDropComponentList: TStringList; 
    FEventList: TStringList;
    FParent: TActiveForm;
    FRegisteredDragDropComponents: TList; 
    FDragStartPos: TPoint;
  protected
    procedure DoStandardPaste(iWinControl: TWinControl);
    procedure DragControlMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
        Integer);
    procedure DragControlMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    function GetDropComponentIndex(iComponent: TObject): Integer;
    procedure StartDragDrop(Sender: TWinControl);
  public
    constructor Create(AParent: TActiveForm); reintroduce; overload;
    destructor Destroy; override;
    procedure ExecutePaste(Sender: TObject);
    procedure RegisterDropComponent(AWinControl: TWinControl; ADropEvent:
        TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent;
        ATableList: array of String; AFormatList: array of Integer; ADragOverCheck:
        TDragOverCheckEvent);
    procedure RegisterDragComponent(AWinControl: TWinControl; ADragEvent:
        TDataDraggedEvent; AAdvancedDragEvent: TAdvancedDataDraggedEvent);
  end;

implementation

const
  DRAG_THRESHOLD = 5;
  
resourcestring
  { TODO : Move to correct unit }
  ResStr_NotComponent       = 'Object is not component';
  ResStr_NotRegistered      = 'Component not registered to drag from';  

  type
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TDragEventContainer = class (TObject)
  private
    FEvent: TDataDraggedEvent;
  public
    constructor Create(AEvent: TDataDraggedEvent); overload;
    property Event: TDataDraggedEvent read FEvent;
  end;

  TAdvancedDragEventContainer = class (TObject)
  private
    FEvent: TAdvancedDataDraggedEvent;
  public
    constructor Create(AEvent: TAdvancedDataDraggedEvent); overload;
    property Event: TAdvancedDataDraggedEvent read FEvent;
  end;
  
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TMouseEventContainer = class (TObject)
  private
    FEvent: TMouseEvent;
  public
    constructor Create(AEvent: TMouseEvent);
    property Event: TMouseEvent read FEvent;
  end;   
  
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TMouseMoveEventContainer = class (TObject)
  private
    FEvent: TMouseMoveEvent;
  public
    constructor Create(AEvent: TMouseMoveEvent);
    property Event: TMouseMoveEvent read FEvent;
  end;

type
  { allow access to the in place editor which is protected }
  TDummyCustomGrid = class(TCustomGrid);

{-------------------------------------------------------------------------------
  Initialise registration lists
}
constructor TDragDropControlAssistor.Create(AParent: TActiveForm);
begin
  inherited Create;
  FParent := AParent;
  FRegisteredDragDropComponents := TList.Create;
  FDropComponentList := TStringList.Create;
  FDragComponentList := TStringList.Create;
  FEventList := TStringList.Create;
  PostMessage(FParent.Handle, WM_SETDRAGDROP, 0, 0);

end;

{-------------------------------------------------------------------------------
  Revoke all drop objects
}
destructor TDragDropControlAssistor.Destroy;
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
procedure TDragDropControlAssistor.DoStandardPaste(iWinControl: TWinControl);
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
procedure TDragDropControlAssistor.ExecutePaste(Sender: TObject);
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
function TDragDropControlAssistor.GetDropComponentIndex(iComponent: TObject):
    Integer;
begin
  Result := -1;
  if Assigned(FDropComponentList) then begin
    if not (iComponent is TComponent) then
      raise EDragDropControlAssistor.Create(EST_NOT_COMPONENT);
    Result := FDropComponentList.IndexOf(TComponent(iComponent).Name);
  end;
end;  // GetDropComponentIndex

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.RegisterDropComponent(AWinControl: TWinControl;
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
      raise EDragDropControlAssistor.Create(ResStr_DragDropRegFailed + AWinControl.Name +
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
end;  // TDragDropControlAssistor.RegisterDropComponent

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.RegisterDragComponent(AWinControl: TWinControl; ADragEvent:
    TDataDraggedEvent; AAdvancedDragEvent: TAdvancedDataDraggedEvent);
begin
  if Assigned(AWinControl) then begin
    if Assigned(ADragEvent) then
      FDragComponentList.AddObject(AWinControl.Name,
                                   TDragEventContainer.Create(ADragEvent))
    else
      FDragComponentList.AddObject(AWinControl.Name,
                      TAdvancedDragEventContainer.Create(AAdvancedDragEvent));
  
    //Remember the old events
    if Assigned(TControlAccessor(AWinControl).OnMouseDown) then
      FEventList.AddObject(TControlAccessor(AWinControl).Name + 'MouseDown',
              TMouseEventContainer.Create(TControlAccessor(AWinControl).OnMouseDown));
  
    if Assigned(TControlAccessor(AWinControl).OnMouseUp) then
      FEventList.AddObject(TControlAccessor(AWinControl).Name + 'MouseUp',
              TMouseEventContainer.Create(TControlAccessor(AWinControl).OnMouseUp));
  
    TControlAccessor(AWinControl).DragMode    := dmManual;
    TControlAccessor(AWinControl).OnMouseDown := DragControlMouseDown;
    TControlAccessor(AWinControl).OnMouseUp   := DragControlMouseUp;
  end;
  // TODO -cMM: TBaseDragFrame.InternalDragComponent default body inserted
end;  // TDragDropControlAssistor.RegisterDragComponent

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.DragControlMouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  lCanDrag: Boolean;
  lOldEvent: TMouseEvent;
  lEventIdx: Integer;
begin
  // If double-click, DON'T initiate drag/drop!
  lCanDrag := not (ssDouble in Shift);
  // Don't want to start dragging if resizing columns only!!!
  if (Sender is TCustomGrid) then
    with TCustomGridAccessor(Sender) do
      if (FixedRows > 0) and (MouseCoord(X, Y).Y < FixedRows) then lCanDrag := false;
  
  // Set drag values only if left mouse button down
  if lCanDrag and (Button=mbLeft) then begin
    //Set drag start position
    FDragStartPos := Mouse.CursorPos;
    //Set mouse move cursor to check for drag
    if Assigned(TControlAccessor(Sender).OnMouseMove) then
      FEventList.AddObject(TControlAccessor(Sender).name + 'MouseMove',
          TMouseMoveEventContainer.Create(TControlAccessor(Sender).OnMouseMove));
    TControlAccessor(Sender).OnMouseMove:= DragControlMouseMove;
  end;
  lEventIdx := FEventList.IndexOf((sender as TControl).name + 'MouseDown');
  if lEventIdx >=0 then
  begin
    lOldEvent := TMouseEventContainer(FEventList.Objects[lEventIdx]).Event;
    lOldEvent(Sender, Button, Shift, X,Y);
  end;
end;  // TDragDropControlAssistor.DragControlMouseDown

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
begin
  //Check to see if mouse has moved far enough to start dragging
  if ((Abs(FDragStartPos.X - Mouse.CursorPos.X) >= DRAG_THRESHOLD) or
      (Abs(FDragStartPos.Y - Mouse.CursorPos.Y) >= DRAG_THRESHOLD)) or
     (Sender is TListbox) and (ssLeft in Shift) then begin
    //Initiate drag
    if Sender is TWinControl then
      StartDragDrop(TWinControl(Sender));
  end;
end;  // TDragDropControlAssistor.DragControlMouseMove

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.DragControlMouseUp(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  lOldEvent: TMouseEvent;
  lEventIdx: Integer;
begin
  // Remove MouseMove handler
  lEventIdx := FEventList.IndexOf((Sender as TControl).Name + 'MouseMove');
  if lEventIdx >=0 then
    TControlAccessor(Sender).OnMouseMove := TMouseMoveEventContainer(
        FEventList.Objects[lEventIdx]).Event
  else
    TControlAccessor(Sender).OnMouseMove := nil;

  //Execute the user's event
  lEventIdx := FEventList.IndexOf((Sender as TControl).Name + 'MouseUp');
  if lEventIdx >=0 then
  begin
    lOldEvent := TMouseEventContainer(FEventList.Objects[lEventIdx]).Event;
    lOldEvent(Sender, Button, Shift, X,Y);
  end;
end;  // TDragDropControlAssistor.DragControlMouseUp

{-------------------------------------------------------------------------------
}
procedure TDragDropControlAssistor.StartDragDrop(Sender: TWinControl);
var
  lDropSource: TJNCCDropSource;
  lEffect: Integer;
  i: Integer;
  lEventIdx: Integer;
begin
  inherited;
  try
    with Sender do begin
      EndDrag(False);
      Invalidate;  // make sure Delphi drag cursor cleared from screen
      i := FDragComponentList.IndexOf(Name);
      if i = -1 then
        raise EDragDropControlAssistor.Create(ResStr_NotRegistered);
      lDropSource := TJNCCDropSource.Create(TWinControl(Sender));
      if Assigned(FDragComponentList.Objects[i]) then
        TAdvancedDragEventContainer(FDragComponentList.Objects[i]).Event
                                                (Sender, lDropSource, False);
    end;
    if lDropSource.DropData.Header.ItemCount > 0 then begin
      DoDragDrop(lDropSource as IDataObject,
                 lDropSource as IDropSource,
                 DROPEFFECT_COPY,
                 lEffect);
    end;
  finally
    // Remove MouseMove handler
    lEventIdx := FEventList.IndexOf(Sender.Name + 'MouseMove');
    if lEventIdx >= 0 then
      TControlAccessor(Sender).OnMouseMove :=
          TMouseMoveEventContainer(FEventList.Objects[lEventIdx]).Event
    else
      TControlAccessor(Sender).OnMouseMove :=nil;
  end;
end;  // TDragDropControlAssistor.StartDragDrop

{-==============================================================================
    TDragEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDragEventContainer.Create(AEvent: TDataDraggedEvent);
begin
  FEvent := AEvent;
end;  // TDragEventContainer.Create 


{-==============================================================================
    TMouseEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMouseEventContainer.Create(AEvent: TMouseEvent);
begin
  FEvent := AEvent;
end;  // TMouseEventContainer.Create 

{-==============================================================================
    TMouseMoveEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMouseMoveEventContainer.Create(AEvent: TMouseMoveEvent);
begin
  FEvent := AEvent;
end;  // TMouseMoveEventContainer.Create 

{-==============================================================================
    TAdvancedDragEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAdvancedDragEventContainer.Create(AEvent: TAdvancedDataDraggedEvent);
begin
  FEvent := AEvent;
end;  // TAdvancedDragEventContainer.Create

end.
