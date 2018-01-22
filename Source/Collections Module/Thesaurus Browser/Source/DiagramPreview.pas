{===============================================================================
  Unit:        DiagramPreview

  Defines:     TDiagramsMenu

  Description: Preview popup window for the Thesaurus' diagrams

  Created:     Dec 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 4 $
    $Date: 6/02/04 9:33 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ExceptionForm;

type
  EDiagramPreviewException = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Popup toolwindow that shows a preview of the diagram layout at a small 
    scale.
  }
  TfrmDiagramPreview = class (TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure DrawConcepts;
    procedure DrawRelationships;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
  end;
  
var
  frmDiagramPreview: TfrmDiagramPreview;

implementation

uses
  ThesaurusDiagramObjects, ThesaurusDiagram, ResourceStrings;

{$R *.dfm}

const ZOOM=0.2;

{ TfrmDiagramPreview }

{-==============================================================================
    TfrmDiagramPreview
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TfrmDiagramPreview.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  if not (AOwner is TThesaurusDiagram) then
    raise EDiagramPreviewException.Create(Format(ResStr_InvalidMethodCall,
        ['TfrmDiagramPreview.Create']));
end;  // TfrmDiagramPreview.Create 

{-------------------------------------------------------------------------------
  Draw shrunk concepts onto the form. 
}
procedure TfrmDiagramPreview.DrawConcepts;
var
  lIdx: Integer;
  lConcept: TDiagramConcept;
begin
  with TThesaurusDiagram(Owner).DiagramConceptList do
    for lIdx := 0 to Count-1 do begin
      lConcept := Items[lIdx];
      Canvas.Brush.Assign(lConcept.ConceptDisplayProperties.Brush);
      Canvas.Pen.Assign(lConcept.ConceptDisplayProperties.Pen);
      Canvas.Rectangle(
          Round(lConcept.Left * ZOOM),
          Round(lConcept.Top * ZOOM),
          Round((lConcept.Left + lConcept.Width) * ZOOM),
          Round((lConcept.Top + lConcept.Height) * ZOOM));
    end;
end;  // TfrmDiagramPreview.DrawConcepts 

{-------------------------------------------------------------------------------
}
procedure TfrmDiagramPreview.DrawRelationships;
var
  lRelation: TDiagramRelationship;
  lIdx: Integer;
begin
  with TThesaurusDiagram(Owner).DiagramRelationshipList do
    for lIdx := 0 to Count-1 do begin
      lRelation := Items[lIdx];
      Canvas.Pen.Assign(lRelation.RelationshipDisplayProperties.Pen);
      Canvas.MoveTo(Round(lRelation.XPosStart * ZOOM),
          Round(lRelation.YPosStart * ZOOM));
      Canvas.LineTo(Round(lRelation.XPosEnd * ZOOM),
          Round(lRelation.YPosEnd * ZOOM));
    end;
end;  // TfrmDiagramPreview.DrawRelationships 

{-------------------------------------------------------------------------------
}
procedure TfrmDiagramPreview.FormClose(Sender: TObject; var Action: 
    TCloseAction);
begin
  Action := caHide;
  // Uncheck the menu item
  TThesaurusDiagram(Owner).PreviewPopupItem.Checked := False;
end;  // TfrmDiagramPreview.FormClose

{-------------------------------------------------------------------------------
}
procedure TfrmDiagramPreview.Paint;
begin
  inherited;
  Canvas.Brush.Color := TThesaurusDiagram(Owner).DiagramDisplayProperties.Color;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  DrawRelationships;
  DrawConcepts;
end;  // TfrmDiagramPreview.Paint 

end.



