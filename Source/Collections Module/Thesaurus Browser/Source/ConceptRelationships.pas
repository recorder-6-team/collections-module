{===============================================================================
  Unit:        ConceptRelationships

  Defines:     TfraConceptRelationships

  Description: Details tab for viewing relationships between concepts

  Created:     September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 2/09/11 16:08 $
    $Author: Jamesbichard $

===============================================================================}
unit ConceptRelationships;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, BaseDetailFrameUnit, StdCtrls, exgrid,
  RapTree, ExtCtrls, GeneralData, GeneralFunctions, DebuggerTracker,
  ResourceStrings, InterfaceDataModule, DataTypes, DropSource;

resourcestring
  ResStr_Unspecified = 'Unspecified';

type
  {-----------------------------------------------------------------------------
    Fly node descendant allowing details required for a relationship to be
    stored on the tree.
  }
  TRelationshipNode = class(TFlyNode)
  private
    FAuthorAndDate: string;
    FConceptGroup: string;
    FConceptKey: string;
    FDetails: string;
    FItemName: string;
  public
    procedure Initialise(const AConceptKey, AItemName, AAuthorAndDate,
        AConceptGroup, ADetails: string);
    property AuthorAndDate: string read FAuthorAndDate write FAuthorAndDate;
    property ConceptGroup: string read FConceptGroup;
    property ConceptKey: string read FConceptKey;
    property Details: string read FDetails;
    property ItemName: string read FItemName;
  end;
  
  {-----------------------------------------------------------------------------
    Frame control that displays all relationships of a selected concept in the
    thesaurus.
  }
  TfraConceptRelationships = class(TBaseTabSheetFrame)
    chkIncludeSynonyms: TCheckBox;
    lblRelationships: TLabel;
    lblSelectedNode: TLabel;
    tvRelationships: TRapidTree;
    procedure chkIncludeSynonymsClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure tvRelationshipsDblClick(Sender: TObject);
    procedure tvRelationshipsDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
        ARow: Integer; Rect: TRect; State: TExGridDrawState);
  private
    procedure DrawRelationshipDetails(aCanvas: TCanvas; ARect: TRect; State:
        TExGridDrawState; ANode: TRelationshipNode);
    procedure DrawRelationshipHeading(aCanvas: TCanvas; Rect: TRect; State:
        TExGridDrawState; ANode: TFlyNode);
    procedure DrawRelationshipName(aCanvas: TCanvas; Rect: TRect; State:
        TExGridDrawState; ANode: TRelationshipNode);
    procedure ExpandHierarchy;
    procedure GetDraggedRelationship(const Sender: TObject; var oDropSource:
        TJNCCDropSource);
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    procedure RegisterDragDropComponents; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CheckNodeHeight(ANode: TRelationshipNode; ARect: TRect);
    procedure LoadData; override;
  end;
  
implementation

uses
  DataClasses, LuxembourgConstants, ApplicationSettings,
  CollectionsModuleManager_TLB;

{$R *.dfm}

{-==============================================================================
    TRelationshipNode
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise the member data for the node. 
}
procedure TRelationshipNode.Initialise(const AConceptKey, AItemName,
    AAuthorAndDate, AConceptGroup, ADetails: string);
begin
  FConceptKey := AConceptKey;
  FItemName := AItemName;
  FAuthorAndDate := AAuthorAndDate;
  FConceptGroup := AConceptGroup;
  FDetails := ADetails;
end;  // TRelationshipNode.Initialise 

{-==============================================================================
    TfraConceptRelationships
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  Load user's settings from registry. 
}
constructor TfraConceptRelationships.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  chkIncludeSynonyms.Checked := AppSettings.IncludeHierarchySynonyms;
end;  // TfraConceptRelationships.Create 

{-------------------------------------------------------------------------------
  Calculates the height of a row in the treeview.  Changes the row height only
      if necessary.
}
procedure TfraConceptRelationships.CheckNodeHeight(ANode: TRelationshipNode;
    ARect: TRect);
var
  lRect: TRect;
begin
  if (ANode.Details = '') and
      (tvRelationships.RowHeights[ANode.GetRow]<>tvRelationships.DefaultRowHeight) then
    tvRelationships.RowHeights[ANode.GetRow]:=tvRelationships.DefaultRowHeight
  else if ANode.Details <> '' then begin
    lRect := ARect;
    // Draw text with CalcRect option, so heigth is obtained but nothing drawn
    DrawText(tvRelationships.Canvas.Handle, PChar(ANode.Details), Length(
        ANode.Details),
        lRect, DT_CALCRECT + DT_WORDBREAK);
    if lRect.Bottom-lRect.Top+tvRelationships.DefaultRowHeight <>
        tvRelationships.RowHeights[ANode.GetRow] then
      tvRelationships.RowHeights[ANode.GetRow] := lRect.Bottom-lRect.Top+
          tvRelationships.DefaultRowHeight;
  end;
end;  // TfraConceptRelationships.CheckNodeHeight 

{-------------------------------------------------------------------------------
  Change the include synonyms checkbox causes the data to reload and stores the
      setting in AppSettings.
}
procedure TfraConceptRelationships.chkIncludeSynonymsClick(Sender: TObject);
begin
  inherited;
  LoadData;
  AppSettings.IncludeHierarchySynonyms := chkIncludeSynonyms.Checked;
end;  // TfraConceptRelationships.chkIncludeSynonymsClick 

{-------------------------------------------------------------------------------
  Draw the details section of a node, if there is one.  This is drawn as a
      multiline block of text - the node row must already be set to the correct
      height.
}
procedure TfraConceptRelationships.DrawRelationshipDetails(aCanvas: TCanvas;
    ARect: TRect; State: TExGridDrawState; ANode: TRelationshipNode);
begin
  if ANode.Details <> '' then begin
    if gdSelected in State then begin
      aCanvas.Brush.Color := clHighlight;
      aCanvas.Font.Color := clHighlightText;
    end
    else begin
      aCanvas.Brush.Color := clWindow;
      aCanvas.Font.Color := clWindowText;
    end;
    // Indent the details
    ARect.Left := ARect.Left + (ANode.Level+2) * tvRelationships.Indent;
    // Move down to allow for node name
    ARect.Top := ARect.Top + tvRelationships.DefaultRowHeight;
    aCanvas.Font.Style := [];
    DrawText(aCanvas.Handle, PChar(ANode.Details),
        Length(ANode.Details), ARect, DT_WORDBREAK);
  end;
end;  // TfraConceptRelationships.DrawRelationshipDetails 

{-------------------------------------------------------------------------------
  Draw each top level node, which represents a relationship heading, as a
      highlighted row.
}
procedure TfraConceptRelationships.DrawRelationshipHeading(aCanvas: TCanvas;
    Rect: TRect; State: TExGridDrawState; ANode: TFlyNode);
var
  lTextHeight: Integer;
  lText: string;
begin
  if gdSelected in State then
    aCanvas.Brush.Color := clHighlight
  else
    aCanvas.Brush.Color := MergeColours(clHighlight, clWindow, 20);
  aCanvas.Font.Color := GetContrastColour(aCanvas.Brush.Color);
  Rect.Left := Rect.Left + (ANode.Level+1) * tvRelationships.Indent;
  aCanvas.FillRect(Rect);
  // vertically centre the text
  lTextHeight := aCanvas.TextHeight('A');
  lText := ANode.Text;
  // Translate tagged headings into the correct version
  if CompareText(lText, '#Synonyms')=0 then
    lText := ResStr_AllKnownSynonyms
  else if CompareText(lText, '#TermVersions')=0 then
    lText := ResStr_OtherTermVersions;
  aCanvas.TextOut(Rect.Left + 4, (Rect.Top + Rect.Bottom) div 2 - lTextHeight
      div 2,
      lText);
  aCanvas.Brush.Color := clWindow;
  aCanvas.Font.Color := clWindowText;
end;  // TfraConceptRelationships.DrawRelationshipHeading 

{-------------------------------------------------------------------------------
  Draw the name part of a listed relationship in the treeview. 
}
procedure TfraConceptRelationships.DrawRelationshipName(aCanvas: TCanvas; Rect:
    TRect; State: TExGridDrawState; ANode: TRelationshipNode);
var
  lText: string;
begin
  // Get the node text
  lText := ANode.Text;
  if ANode.ConceptGroup <> '' then
    lText := lText + ' - ' + ANode.ConceptGroup;
  // Position the text
  Rect.Left := Rect.Left + (ANode.Level+1) * tvRelationships.Indent;
  Rect.Top := Rect.Top + (tvRelationships.DefaultRowHeight - aCanvas.TextHeight(
      'A')) div 2;
  // Draw the text
  dmInterface.DrawTerm(aCanvas, Rect, lText, gdSelected in State);
end;  // TfraConceptRelationships.DrawRelationshipName 

{-------------------------------------------------------------------------------
  Edit/Browse mode not relevant here so ensure controls stay enabled. 
}
procedure TfraConceptRelationships.EnableControls(AEnabled: Boolean);
begin
  inherited;
  chkIncludeSynonyms.Enabled := True;
end;  // TfraConceptRelationships.EnableControls 

{-------------------------------------------------------------------------------
    Display the relationships tree view already expanded. 
}
procedure TfraConceptRelationships.ExpandHierarchy;
var
  lIdx: Integer;
begin
  for lIdx := 0 to tvRelationships.Items.Count-1 do
    tvRelationships.Items[lIdx].Expand(false);
end;  // TfraConceptRelationships.ExpandHierarchy 

{-------------------------------------------------------------------------------
}
procedure TfraConceptRelationships.FrameResize(Sender: TObject);
begin
  inherited;
  tvRelationships.ColWidths[0] := tvRelationships.Width - 17;
end;  // TfraConceptRelationships.FrameResize 

{-------------------------------------------------------------------------------
  Picks up the selected item for drag operations. 
}
procedure TfraConceptRelationships.GetDraggedRelationship(const Sender: TObject;
    var oDropSource: TJNCCDropSource);
var
  lKeyList: TEditableKeyList;
  lIdx: Integer;
begin
  if Assigned(tvRelationships.Selected) then begin
    lKeyList := TEditableKeyList.Create;
    with lKeyList do begin
      SetTable(TN_CONCEPT);
      if tvRelationships.Selected is TRelationshipNode then
        AddItem(TRelationshipNode(tvRelationships.Selected).ConceptKey, '')
      else begin
        // Top level node selected, so drag the children
        for lIdx := 0 to tvRelationships.Selected.Count-1 do
          if tvRelationships.Selected.Items[lIdx] is TRelationshipNode then
            AddItem(TRelationshipNode(
                tvRelationships.Selected.Items[lIdx]).ConceptKey, '');
      end;
    end;
    oDropSource.AssignKeyList(lKeyList);
  end;
end;  // TfraConceptRelationships.GetDraggedRelationship 

{-------------------------------------------------------------------------------
  Populates the tree view with the relationships list. 
}
procedure TfraConceptRelationships.LoadData;
var
  lCurrentHeadingNode: TFlyNode;
  lCursor: TCursor;
  lNewNode: TRelationshipNode;
  
  function GetNextHeadingNode(const AText: string): TFlyNode;
  begin
    Result := tvRelationships.Items.Add(nil, AText);
  end;
  
begin
  tvRelationships.Items.Clear;
  if Key<>'' then begin
    lCursor := HourglassCursor;
    try
      with dmGeneral.GetRecordset('usp_ConceptRelations_Select', [
          '@ConceptKey', Key,
          '@IncludeSynonyms', chkIncludeSynonyms.Checked]) do begin
        lCurrentHeadingNode := GetNextHeadingNode(VarToStr(
            Fields['Relationship_Name'].Value));
        while not EOF do begin
          if VarToStr(
              Fields['Relationship_Name'].Value)<>lCurrentHeadingNode.Text then
            lCurrentHeadingNode := GetNextHeadingNode(VarToStr(
                Fields['Relationship_Name'].Value));
          lNewNode := TRelationshipNode(
              tvRelationships.Items.AddTypedChild(lCurrentHeadingNode,
                  TRelationshipNode));
          lNewNode.Text := VarToStr(Fields['Item_Name'].Value);
          lNewNode.Initialise(Fields['Concept_Key'].Value,
              lNewNode.Text,
              VarToStr(Fields['Author_And_Date'].Value),
              VarToStr(Fields['Concept_Group_Name'].Value),
              VarToStr(Fields['Details'].Value));
          MoveNext;
        end;
      end;
      ExpandHierarchy;
      // Ensure top node is visibe
      if tvRelationships.Items.Count>0 then
        tvRelationships.Selected := tvRelationships.Items.GetFirstNode;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // TfraConceptRelationships.LoadData 

{-------------------------------------------------------------------------------
  Register the tree view as a drag control. 
}
procedure TfraConceptRelationships.RegisterDragDropComponents;
begin
  RegisterDragComponent(tvRelationships, GetDraggedRelationship);
end;  // TfraConceptRelationships.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TfraConceptRelationships.tvRelationshipsDblClick(Sender: TObject);
var
  lKey: string;
begin
  inherited;
  if Assigned(OnFrameNotification) then
    if tvRelationships.Selected is TRelationshipNode then begin
      lKey := TRelationshipNode(tvRelationships.Selected).ConceptKey;
      OnFrameNotification(Self, etNavigation,
          VarArrayOf(['CONCEPT', lKey]));
    end;
end;  // TfraConceptRelationships.tvRelationshipsDblClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptRelationships.tvRelationshipsDrawCell(Sender: TObject;
    aCanvas: TCanvas; ACol, ARow: Integer; Rect: TRect; State:
    TExGridDrawState);
var
  lNode: TFlyNode;
  lRelationshipNode: TRelationshipNode;
begin
  if ACol>0 then Exit;
  lNode := tvRelationships.GetNodeAtRow(ARow);
  // shade the background of each section
  if lNode.Level = 0 then
    DrawRelationshipHeading(aCanvas, Rect, State, lNode)
  else begin
    lRelationshipNode := TRelationshipNode(lNode);
    CheckNodeHeight(lRelationshipNode, Rect);
    DrawRelationshipName(aCanvas, Rect, State, lRelationshipNode);
    DrawRelationshipDetails(aCanvas, Rect, State, lRelationshipNode);
  end;
end;  // TfraConceptRelationships.tvRelationshipsDrawCell 






end.



















