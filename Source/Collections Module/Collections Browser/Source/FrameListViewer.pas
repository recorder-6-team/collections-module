{===============================================================================
  Unit:        FrameListViewer.pas

  Defines:     TfraListViewer

  Description: Displays children of currently selected tvNav node and allows
               double-click navigation.

  Model:       CollectionsCommonAndGeneral

  Created:     Sept 2003

  Last revision information:
    $Revision: 15 $
    $Date: 19/05/04 14:17 $
    $Author: Anthonysimpson $

===============================================================================}
unit FrameListViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, ExtCtrls, ComCtrls, ImgList, RapTree,
  UserMessages, StdCtrls, DropSource, DataClasses;

type
  {-----------------------------------------------------------------------------
    When nodes are selected in the Collections Browser that do not link to a specific detail 
    screen, TfraListViewer is displayed in the details panel.  This contains a list view with 
    an item for each child node of the current selected node.  The user can browse items in 
    the list by double clicking them (this performs the same action as double clicking the 
    same item in the browser hierarchy).
  }
  TfraListViewer = class (TBaseFullScreenFrame)
    lvListViewer: TListView;
    pnlMain: TPanel;
    procedure lvListViewerDblClick(Sender: TObject);
    procedure lvListViewerDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; 
        State: TOwnerDrawState);
  private
    procedure DragList(const Sender: TObject; var oDropSource: TJNCCDropSource);
  protected
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
  public
    function GetKeyList: TKeyList;
    property KeyList: TKeyList read GetKeyList;
  end;
  
//==============================================================================
implementation

uses
  InterfaceDataModule, BaseDragFrameUnit, BrowserNodeFramework,
  BrowserNodeCollectionUnits, BaseDetailFrameUnit;

{$R *.dfm}

{-==============================================================================
    TfraListViewer
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraListViewer.DragList(const Sender: TObject; var oDropSource: TJNCCDropSource);
begin
  oDropSource.AssignKeyList(KeyList);
end;  // TfraListViewer.DragList 

{-------------------------------------------------------------------------------
  Retrieve a key list of the selected items only. 
}
function TfraListViewer.GetKeyList: TKeyList;
var
  i: Integer;
  lMixed: Boolean;
  lNode: TBrowserNode;
  lSelectAll: boolean;
begin
  Result := TEditableKeyList.Create;
  lMixed := False;
  //If no nodes are selected then assume all nodes should be included
  lSelectAll := (lvListViewer.SelCount = 0);

  with TEditableKeyList(Result) do begin
    for i := 0 to lvListViewer.Items.Count - 1 do
      if lvListViewer.Items[i].Selected or lSelectAll then begin
        lNode := TBrowserNode(AssociatedContainerNode.Items[i]);
        // Set the key list table name
        if Header.TableName = '' then
          SetTable(lNode.TableName)
        else
        if Header.TableName <> lNode.TableName then begin
          // more than one table in the list, so use mixed data list type
          ConvertToMixedData;
          lMixed := True;
        end;
        if lMixed then AddItem(lNode.Key, lNode.TableName)
                  else AddItem(lNode.Key, '');
      end;
  end;
end;  // TfraListViewer.GetKeyList

{-------------------------------------------------------------------------------
  Load nodes into listbox. 
}
procedure TfraListViewer.LoadData;
var
  lTreeNode: TFlyNode;
begin
  lvListViewer.Clear;
  lvListViewer.ReadOnly := True;
  if AssociatedContainerNode <> nil then begin
    lTreeNode := AssociatedContainerNode.GetFirstChild;
    while lTreeNode <> nil do begin
      with lvListViewer.Items.Add do begin
        Caption := lTreeNode.Text;
        ImageIndex := lTreeNode.ImageIndex;
        //NB do not store node pointer as the AssociatedContainerNode may be
        //refreshed without the listviewer updating its node pointers.
        //Data := pointer(lTreeNode);
      end; // with
      lTreeNode := lTreeNode.GetNextSibling;
    end;
    lvListViewer.Column[0].Caption := AssociatedContainerNode.Caption;
  end;
end;  // TfraListViewer.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraListViewer.lvListViewerDblClick(Sender: TObject);
begin
  inherited;
  { Navigate by passing a pointer to the node that requires to be navigated
      to the CBNavigation unit }
  with AssociatedContainerNode do
    PostMessage(TWinControl(Tree.Owner).Handle,
        WM_NAVIGATE, Integer(Item[lvListViewer.ItemIndex]), 0);
end;  // TfraListViewer.lvListViewerDblClick

{-------------------------------------------------------------------------------
}
procedure TfraListViewer.lvListViewerDrawItem(Sender: TCustomListView; Item: TListItem; Rect: 
    TRect; State: TOwnerDrawState);
var
  lTreeNode: TFlyNode;
begin
  with Sender.Canvas do begin
    if odSelected in State then begin
      Brush.Color := clHighlight;
      Font.Color  := clHighlightText;
    end else begin
      Brush.Color := clWindow;
      Font.Color  := clWindowText;
    end;
    FillRect(Rect);
  end;

  lTreeNode := AssociatedContainerNode.Item[item.Index];

  if lTreeNode is THyperlinkLeafNode then
    dmInterface.ilBrowserNodes.DrawOverlay(Sender.Canvas, Rect.Left, Rect.Top,
        Item.ImageIndex, 0)
  else
    dmInterface.ilBrowserNodes.Draw(Sender.Canvas, Rect.Left, Rect.Top, Item.ImageIndex);

  Rect.Left := Rect.Left + dmInterface.ilBrowserNodes.Width + 4;

  if lTreeNode is TStoreLeafNode then
    if TStoreLeafNode(lTreeNode).DrawHierarchically then begin
      //Clear the canvas underneath where the icon would normally be drawn
      Sender.Canvas.FillRect(Rect);
      //Indent each node according to its DrawSpacer
      Rect.Left := Rect.Left + TStoreLeafNode(lTreeNode).HierarchyDrawSpacer;
      dmInterface.ilBrowserNodes.DrawOverlay(Sender.Canvas, Rect.Left - 20,
          Rect.Top, 4, 0); //4 is the imageindex for a Store
    end;

  dmInterface.DrawTerm(Sender.Canvas, Rect, Item.Caption, odSelected in State);
end;  // TfraListViewer.lvListViewerDrawItem

{-------------------------------------------------------------------------------
  Set up list view as a drag source. 
}
procedure TfraListViewer.RegisterDragDropComponents;
begin
  RegisterDragComponent(lvListViewer, DragList);
end;  // TfraListViewer.RegisterDragDropComponents 

end.
