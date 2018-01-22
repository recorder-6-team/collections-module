{===============================================================================
  Unit:        DiagramConceptPropertiesForCG

  Defines:     TfraDiagramConceptPropertiesForCG

  Description: Display properties frame for a concept group

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 4 $
    $Date: 6/02/04 9:33 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptPropertiesForCG;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramConceptPropertiesInherited, StdCtrls, Buttons, ComCtrls,
  ColorBtn, ExtCtrls, ThesaurusDiagramObjects, ThesaurusDiagramObjectLists,
  ThesaurusDiagram, RestrictedEdits;

type
  {-----------------------------------------------------------------------------
    Frame that allows the properties of a specific concept group to be 
    controlled.  The properties apply to all concepts of this group, unless 
    specified otherwise.
    Inherits the controls from TfraDiagramConceptPropertiesInherited and sets 
    the instructions label to appropriate text for the task.
  }
  TfraDiagramConceptPropertiesForCG = class (
      TfraDiagramConceptPropertiesInherited)
    procedure cmbItemChange(Sender: TObject);
  private
    FConceptGroup: TDiagramConceptGroup;
    procedure SetConceptGroup(Value: TDiagramConceptGroup);
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  public
    property ConceptGroup: TDiagramConceptGroup read FConceptGroup write 
        SetConceptGroup;
  end;
  
var
  fraDiagramConceptPropertiesForCG: TfraDiagramConceptPropertiesForCG;

implementation

{$R *.dfm}

{-==============================================================================
    TfraDiagramConceptPropertiesForCG
===============================================================================}
{-------------------------------------------------------------------------------
  When the selected item changes, apply changes to the existing properties 
      object, then select the new one. 
}
procedure TfraDiagramConceptPropertiesForCG.cmbItemChange(Sender: TObject);
begin
  inherited;
  if cmbItem.ItemIndex>-1 then
    Properties := TDiagramConceptGroup(
        cmbItem.Items.Objects[cmbItem.ItemIndex]).
        DisplayProperties
  else
    Properties := nil;
end;  // TfraDiagramConceptPropertiesForCG.cmbItemChange 

{-------------------------------------------------------------------------------
  Accessor method.  Selects the concept group associated with the frame. 
}
procedure TfraDiagramConceptPropertiesForCG.SetConceptGroup(Value: 
    TDiagramConceptGroup);
begin
  FConceptGroup := Value;
  
  Properties := Value.DisplayProperties;
  // Select the item in the list
  cmbItem.ItemIndex := cmbItem.Items.IndexOfObject(Value);
end;  // TfraDiagramConceptPropertiesForCG.SetConceptGroup 

{-------------------------------------------------------------------------------
}
procedure TfraDiagramConceptPropertiesForCG.SetDiagram(Value: 
    TThesaurusDiagram);
var
  lIdx: Integer;
begin
  inherited SetDiagram(Value);
  
  cmbItem.Clear;
  with Diagram.DiagramConceptGroupList do
    for lIdx := 0 to Count-1 do
      cmbItem.Items.AddObject(Items[lIdx].Caption, Items[lIdx]);
  ConceptGroup := TDiagramConceptGroup(Value.SelectedConcept.ConceptGroup);
end;  // TfraDiagramConceptPropertiesForCG.SetDiagram 



end.





