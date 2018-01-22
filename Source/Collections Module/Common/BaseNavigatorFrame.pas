{===============================================================================
  Unit:        BaseNavigatorFrame

  Defines:     TfraBaseNavigator

  Description: A frame containing a general tree view with drag and drop
               functionality.

  Created:     December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/12/08 14:41 $
    $Author: Qingsun $

===============================================================================}

unit BaseNavigatorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDragFrameUnit, exgrid, RapTree, ExtCtrls, DropSource,
  DataClasses, DropTarget, Types, DataTypes, Treecoll;

type
  TNavigatorNode = class(TFlyNode)
  private
    FPopulated: Boolean;
    procedure SetPopulated(Value: Boolean);
  public   
    constructor Create(AOwner: TTreeCollection); override;
    destructor Destroy; override;
    property Populated: Boolean read FPopulated write SetPopulated;
  end;

  TfraBaseNavigator = class(TBaseDragFrame)
    tvHierarchy: TRapidTree;
    pnlTree: TPanel;    
    procedure pmNodeRefreshNodeChildrenClick(Sender: TObject);
  private
    FMousePointer: TPoint;
    FOnFrameNotification: TFrameNotificationEvent;
  protected
    FDraggedNode: TFlyNode;
    FIsDrag: Boolean;
    FTargetNode: TFlyNode;
    procedure PopulateNode(ANode: TNavigatorNode); virtual; abstract;
    procedure CheckNodeCanDrop(APoint: TPoint; const ATable, AKeyField: string;
        var Accept: boolean); virtual; abstract;
    procedure DragTerm(const Sender: TObject; var oDropSource: TJNCCDropSource;
        const oIsCopyOperation: Boolean);
    procedure DropTerm(const Sender: TObject; const iFormat : integer; const
        iSourceData: TKeyList; const iTextStrings : TstringList; const
        iIsPasteOperation: boolean; var ioHandled : boolean); virtual;
    procedure RegisterDragDropComponents; override;
    procedure SetDraggedNode(Value: TFlyNode);
    function CanDrag : Boolean; virtual; abstract;
    function CanDrop : Boolean; virtual; abstract;
    function GetTableList : TStringDynArray; virtual; abstract;
    procedure SetDropSourceData(var oDropSource: TJNCCDropSource;
        var node : TFlyNode); virtual; abstract;
  public                                                               
    procedure DeleteNode(ANode: TFlyNode);
    property MousePointer: TPoint read FMousePointer write FMousePointer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;   
    property DraggedNode: TFlyNode read FDraggedNode write SetDraggedNode; 
    property OnFrameNotification: TFrameNotificationEvent
        read FOnFrameNotification write FOnFrameNotification;
  end;

var
  fraBaseNavigator: TfraBaseNavigator;

implementation  

{$R *.dfm}
{-==============================================================================
    TNavigatorNode
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor sets default property values.
}
constructor TNavigatorNode.Create(AOwner: TTreeCollection);
begin
  inherited Create(AOwner);
  FPopulated := False;
end;  // TNavigatorNode.Create 

{-------------------------------------------------------------------------------
}
destructor TNavigatorNode.Destroy;
begin
  inherited;
end;  // TNavigatorNode.Destroy

{-------------------------------------------------------------------------------
}
procedure TNavigatorNode.SetPopulated(Value: Boolean);
begin
  FPopulated := Value;
end;  // TTermNode.SetPopulated


{-==============================================================================
    TfraBaseNavigator
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraBaseNavigator.Create(AOwner: TComponent);
begin
  inherited;
end;  // TfraBaseNavigator.Create 

{-------------------------------------------------------------------------------
  Does exactly what it says on the tin.
}
procedure TfraBaseNavigator.DeleteNode(ANode: TFlyNode);
var
  lDelNode, lSelNode: TFlyNode;
begin
  with tvHierarchy do begin
    if FDraggedNode = ANode then
      FDraggedNode := nil;
    lDelNode := ANode;
    lSelNode := lDelNode.GetPrevVisible;
    lDelNode.Delete;
    if not Assigned(lSelNode) then
      Selected := Items.GetFirstNode
    else
      Selected := lSelNode;
    // clear the locked node
    if Assigned(OnFrameNotification) then
      OnFrameNotification(Self, etNodeCleared, nil);
    if Items.Count = 0 then
      // We need to fire an event at this point to get the Delete button
      // correctly disabled.
      if Assigned(OnFrameNotification) then
        OnFrameNotification(Self, etSetBrowseMode, nil);
  end;
end;  // TfraBaseNavigator.DeleteNode

{-------------------------------------------------------------------------------
  Reloads the child nodes of the currently selected node.
}
procedure TfraBaseNavigator.pmNodeRefreshNodeChildrenClick(Sender: TObject);
var
  lWasExpanded: boolean;
begin
  if Assigned(tvHierarchy.Selected) then begin
    // if the recorded dragged node is about to be destroyed, then clean it up
    if assigned(DraggedNode) then
      if DraggedNode.Parent=tvHierarchy.Selected then
        DraggedNode := nil;
    with TNavigatorNode(tvHierarchy.Selected) do begin
      lWasExpanded := Expanded;
      DeleteChildren;
      Populated := False;
      // set HasChildren so that it will at least try to expand
      HasChildren := True;
      PopulateNode(TNavigatorNode(tvHierarchy.Selected));
      HasChildren := Count>0;
      if lWasExpanded then
        Expand(false);
    end;
  end;
end;  // TfraBaseNavigator.pmNodeRefreshNodeChildrenClick

{-------------------------------------------------------------------------------
  Procedure which deals with operations to take place when a node is dropped.
  Should be overridden with this part inherited at the start of the procedure in
  child forms, with the actual actions to take continued afterwards.
}
procedure TfraBaseNavigator.DropTerm(const Sender: TObject; const
    iFormat : integer; const iSourceData: TKeyList; const iTextStrings :
    TstringList; const iIsPasteOperation: boolean; var ioHandled : boolean);
var
  lClickPoint: TPoint;
begin
  FIsDrag := not iIsPasteOperation;

  if FIsDrag then begin
    lClickPoint := tvHierarchy.ScreenToClient(Mouse.CursorPos);
    FTargetNode := tvHierarchy.GetNodeAt(lClickPoint.X, lClickPoint.Y);
  end else
    FTargetNode := tvHierarchy.Selected;
end;

{-------------------------------------------------------------------------------
}
destructor TfraBaseNavigator.Destroy;
begin
  inherited Destroy;
end;  // TfraBaseNavigator.Destroy

{-------------------------------------------------------------------------------
  Register the hierarchy for drag and drop operations, where applicable.
}
procedure TfraBaseNavigator.RegisterDragDropComponents;
begin
  if CanDrag then
    RegisterDragComponent(tvHierarchy, DragTerm);
  RegisterDropComponent(tvHierarchy, DropTerm, GetTableList, [CF_JNCCDATA],CheckNodeCanDrop);
end;  // TfraBaseNavigator.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraBaseNavigator.SetDraggedNode(Value: TFlyNode);
begin
  FDraggedNode := Value;
end;  // TfraBaseNavigator.SetDraggedNode       

{-------------------------------------------------------------------------------
  When a drag operation starts, generate the drop source object that refers to
  the current selected term.
}
procedure TfraBaseNavigator.DragTerm(const Sender: TObject; var oDropSource:
    TJNCCDropSource; const oIsCopyOperation: Boolean);
var
  lCol, lRow: Integer;
begin
  // The problem here is that clicking on white space does not deselect the item
  // in the treeview. We need to check that the mouse was over a node when
  // the drag was initiated, or the pop-up menu first appeared. We cannot rely
  // on the mouse position when cut/copy was clicked, because the mouse will
  // have moved around in the pop-up menu by that point.
  if not oIsCopyOperation then
    MousePointer := tvHierarchy.ScreenToClient(Mouse.CursorPos);

  tvHierarchy.MouseToCell(MousePointer.X, MousePointer.Y, lCol, lRow);

  FDraggedNode := tvHierarchy.Selected;
  if Assigned(tvHierarchy.Selected) and (lRow>-1) then begin
    SetDropSourceData(oDropSource, FDraggedNode);
  end;
end;  // TfraBaseNavigator.DragTerm

end.
