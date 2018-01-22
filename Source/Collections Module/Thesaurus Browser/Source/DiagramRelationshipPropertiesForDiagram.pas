{===============================================================================
  Unit:        DiagramRelationshipPropertiesForDiagram

  Defines:     TfraDiagramRelationshipPropertiesForDiagram

  Description: A frame for editing the properties of relationships on the
               diagrams that apply at the default, entire diagram level.

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/09/04 16:25 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipPropertiesForDiagram;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramRelationshipProperties, StdCtrls, RestrictedEdits,
  ColorBtn, ExtCtrls, Buttons, ComCtrls, ThesaurusDiagram;

type
  {-----------------------------------------------------------------------------
    Specialisation of TfraDiagramRelationshipProperties in which the properties
    are set that apply to all relationships on the diagram.  The instructions 
    label is set to display appropriate instructions for the task.
  }
  TfraDiagramRelationshipPropertiesForDiagram = class (
      TfraDiagramRelationshipProperties)
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  end;
  
implementation

{$R *.dfm}

uses
  ThesaurusDiagramObjects;

{-==============================================================================
    TfraDiagramRelationshipPropertiesForDiagram
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method override.  Links the frame to the diagram's properties. 
}
procedure TfraDiagramRelationshipPropertiesForDiagram.SetDiagram(Value: 
    TThesaurusDiagram);
begin
  inherited SetDiagram(Value);
  
  Properties := TThesaurusDiagramDisplayProperties(
      Value.DiagramDisplayProperties).RelationshipDisplayProperties;
end;  // TfraDiagramRelationshipPropertiesForDiagram.SetDiagram 


end.

