{===============================================================================
  Unit:        DiagramRelationshipPropertiesForRel

  Defines:     TfraDiagramRelationshipPropertiesForRel

  Description: A frame for editing the properties of a specific relationship
               on the diagram

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 7/01/04 16:46 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipPropertiesForRel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramRelationshipPropertiesInherited, StdCtrls, Buttons,
  ComCtrls, ColorBtn, ExtCtrls, ThesaurusDiagramObjects, ThesaurusDiagram;

type
  {-----------------------------------------------------------------------------
    Frame that allows the properties of a specific relationship to be 
    controlled.
    Inherits the controls from TfraDiagramRelationshipPropertiesInherited and 
    sets the instructions label to appropriate text for the task.
  }
  TfraDiagramRelationshipPropertiesForRel = class (
      TfraDiagramRelationshipPropertiesInherited)
    procedure cmbItemChange(Sender: TObject);
  private
    function GetRelationship: TDiagramRelationship;
    procedure SetRelationship(Value: TDiagramRelationship);
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  public
    property Relationship: TDiagramRelationship read GetRelationship write 
        SetRelationship;
  end;
  

implementation

{$R *.dfm}

{-==============================================================================
    TfraDiagramRelationshipPropertiesForRel
===============================================================================}
{-------------------------------------------------------------------------------
  When the selected item changes, apply changes to the existing properties 
      object, then select the new one. 
}
procedure TfraDiagramRelationshipPropertiesForRel.cmbItemChange(Sender: 
    TObject);
begin
  inherited;
  if cmbItem.ItemIndex>-1 then
    Properties := TDiagramRelationship(
        cmbItem.Items.Objects[cmbItem.ItemIndex]).RelationshipDisplayProperties
  else
    Properties := nil;
end;  // TfraDiagramRelationshipPropertiesForRel.cmbItemChange 

{-------------------------------------------------------------------------------
  Retrieve the selected relationship from the combo box. 
}
function TfraDiagramRelationshipPropertiesForRel.GetRelationship: 
    TDiagramRelationship;
begin
  Result := TDiagramRelationship(cmbItem.Items.Objects[cmbItem.ItemIndex]);
end;  // TfraDiagramRelationshipPropertiesForRel.GetRelationship 

{-------------------------------------------------------------------------------
  Accessor method override.  Links the frame to the relationship's properties. 
}
procedure TfraDiagramRelationshipPropertiesForRel.SetDiagram(Value: 
    TThesaurusDiagram);
var
  lIdx: Integer;
begin
  inherited SetDiagram(Value);
  
  cmbItem.Clear;
  with Diagram.DiagramRelationshipList do
    for lIdx := 0 to Count-1 do
      cmbItem.Items.AddObject(Items[lIdx].Caption, Items[lIdx]);
  Relationship := TDiagramRelationship(Diagram.SelectedRelationship);
end;  // TfraDiagramRelationshipPropertiesForRel.SetDiagram 

{-------------------------------------------------------------------------------
  Accessor method.  Selects the correct relationship in the combo box. 
}
procedure TfraDiagramRelationshipPropertiesForRel.SetRelationship(Value: 
    TDiagramRelationship);
begin
  cmbItem.ItemIndex := cmbItem.Items.IndexOfObject(Value);
  cmbItemChange(nil);
end;  // TfraDiagramRelationshipPropertiesForRel.SetRelationship 



end.

