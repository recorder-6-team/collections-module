{===============================================================================
  Unit:        DiagramRelationshipPropertiesForType

  Defines:     TfraDiagramRelationshipPropertiesForType

  Description: A frame for editing the properties of a specific relationship
               type on the diagram

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 7/01/04 16:46 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipPropertiesForType;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramRelationshipPropertiesInherited, StdCtrls, Buttons,
  ComCtrls, ColorBtn, ExtCtrls, ThesaurusDiagramObjects, ThesaurusDiagram,
  ThesaurusDiagramObjectLists;

type
  {-----------------------------------------------------------------------------
    Frame that allows the properties of a specific relationship type to be 
    controlled.  The properties apply to all relationships of this type, unless 
    specified otherwise.
    Inherits the controls from TfraDiagramRelationshipPropertiesInherited and 
    sets the instructions label to appropriate text for the task.
  }
  TfraDiagramRelationshipPropertiesForType = class (
      TfraDiagramRelationshipPropertiesInherited)
    procedure cmbItemChange(Sender: TObject);
  private
    function GetRelationType: TDiagramRelationType;
    procedure SetRelationType(Value: TDiagramRelationType);
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  public
    property RelationType: TDiagramRelationType read GetRelationType write 
        SetRelationType;
  end;
  

implementation

{$R *.dfm}

{-==============================================================================
    TfraDiagramRelationshipPropertiesForType
===============================================================================}
{-------------------------------------------------------------------------------
  When the selected item changes, apply changes to the existing properties 
      object, then select the new one. 
}
procedure TfraDiagramRelationshipPropertiesForType.cmbItemChange(Sender: 
    TObject);
begin
  inherited;
  if cmbItem.ItemIndex>-1 then
    Properties := RelationType.DisplayProperties
  else
    Properties := nil;
end;  // TfraDiagramRelationshipPropertiesForType.cmbItemChange 

{-------------------------------------------------------------------------------
  Retrieve the relation type from the selected item in the combo box. 
}
function TfraDiagramRelationshipPropertiesForType.GetRelationType: 
    TDiagramRelationType;
begin
  Result := TDiagramRelationType(cmbItem.Items.Objects[cmbItem.ItemIndex]);
end;  // TfraDiagramRelationshipPropertiesForType.GetRelationType 

{-------------------------------------------------------------------------------
  Accessor method override.  Links the frame to the relationship type 
      properties. 
}
procedure TfraDiagramRelationshipPropertiesForType.SetDiagram(Value: 
    TThesaurusDiagram);
var
  lIdx: Integer;
begin
  inherited SetDiagram(Value);
  
  cmbItem.Clear;
  with Diagram.DiagramRelationTypeList do
    for lIdx := 0 to Count-1 do
      cmbItem.Items.AddObject(Items[lIdx].Caption, Items[lIdx]);
  RelationType := TDiagramRelationType(
      Diagram.SelectedRelationship.RelationType);
end;  // TfraDiagramRelationshipPropertiesForType.SetDiagram 

{-------------------------------------------------------------------------------
  Accessor method.  Selects the correct relation type. 
}
procedure TfraDiagramRelationshipPropertiesForType.SetRelationType(Value: 
    TDiagramRelationType);
begin
  cmbItem.ItemIndex := cmbItem.Items.IndexOfObject(Value);
  cmbItemChange(nil);
end;  // TfraDiagramRelationshipPropertiesForType.SetRelationType 



end.

