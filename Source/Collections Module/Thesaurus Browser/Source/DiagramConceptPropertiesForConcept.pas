{===============================================================================
  Unit:        DiagramConceptPropertiesForConcept

  Defines:     TfraDiagramConceptPropertiesForConcept

  Description: Display properties frame for an individual concept

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 3 $
    $Date: 7/01/04 16:46 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptPropertiesForConcept;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramConceptPropertiesInherited, StdCtrls, Buttons, ComCtrls,
  ColorBtn, ExtCtrls, ThesaurusDiagramObjects, ComboListID, LuxIDComboBox,
  ThesaurusDiagram;

type
  {-----------------------------------------------------------------------------
    Frame that allows the properties of a specific concept to be controlled.
    Inherits the controls from TfraDiagramConceptPropertiesInherited and sets 
    the instructions label to appropriate text for the task.
  }
  TfraDiagramConceptPropertiesForConcept = class (
          TfraDiagramConceptPropertiesInherited)
    procedure cmbItemChange(Sender: TObject);
  private
    function GetConcept: TDiagramConcept;
    procedure SetConcept(Value: TDiagramConcept);
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  public
    property Concept: TDiagramConcept read GetConcept write SetConcept;
  end;
  

implementation

{$R *.dfm}

{-==============================================================================
    TfraDiagramConceptPropertiesForConcept
===============================================================================}
{-------------------------------------------------------------------------------
  When the selected item changes, apply changes to the existing properties 
          object, then select the new one. 
}
procedure TfraDiagramConceptPropertiesForConcept.cmbItemChange(Sender: TObject);
begin
  inherited;
  if cmbItem.ItemIndex>-1 then
    Properties := TDiagramConcept(cmbItem.Items.Objects[cmbItem.ItemIndex]).ConceptDisplayProperties
  else
    Properties := nil;
end;  // TfraDiagramConceptPropertiesForConcept.cmbItemChange 

{-------------------------------------------------------------------------------
}
function TfraDiagramConceptPropertiesForConcept.GetConcept: TDiagramConcept;
begin
  Result := TDiagramConcept(cmbItem.Items.Objects[cmbItem.ItemIndex]);
end;  // TfraDiagramConceptPropertiesForConcept.GetConcept 

{-------------------------------------------------------------------------------
  Accessor method.  Selects the correct concept. 
}
procedure TfraDiagramConceptPropertiesForConcept.SetConcept(Value: 
        TDiagramConcept);
begin
  cmbItem.ItemIndex := cmbItem.Items.IndexOfObject(Value);
  cmbItemChange(nil);
end;  // TfraDiagramConceptPropertiesForConcept.SetConcept 

{-------------------------------------------------------------------------------
  When the diagram is associated with the frame, load the list of concepts into 
          the combo box. 
}
procedure TfraDiagramConceptPropertiesForConcept.SetDiagram(Value: 
        TThesaurusDiagram);
var
  lIdx: Integer;
begin
  inherited SetDiagram(Value);
  
  cmbItem.Clear;
  with Diagram.DiagramConceptList do
    for lIdx := 0 to Count-1 do
      cmbItem.Items.AddObject(Items[lIdx].Caption, Items[lIdx]);
  Concept := Value.SelectedConcept;
end;  // TfraDiagramConceptPropertiesForConcept.SetDiagram 

end.






