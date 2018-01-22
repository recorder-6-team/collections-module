{===============================================================================
  Unit:        DiagramConceptPropertiesForDiagram

  Defines:     TfraDiagramConceptPropertiesForDiagram

  Description: Display properties frame for all concepts on a diagram

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 4 $
    $Date: 3/09/04 16:25 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptPropertiesForDiagram;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramConceptProperties, StdCtrls, Buttons, ComCtrls, ColorBtn,
  ExtCtrls, ThesaurusDiagram, RestrictedEdits;

type
  {-----------------------------------------------------------------------------
    Specialisation of TfraDiagramConceptProperties in which the properties are 
    set that apply to all concepts on the diagram.  The instructions label is
    set to display appropriate instructions for the task.
  }
  TfraDiagramConceptPropertiesForDiagram = class (TfraDiagramConceptProperties)
  protected
    procedure SetDiagram(Value: TThesaurusDiagram); override;
  end;
  
var
  fraDiagramConceptPropertiesForDiagram: TfraDiagramConceptPropertiesForDiagram;

implementation

uses
  ThesaurusDiagramObjects;

{-==============================================================================
    TfraDiagramConceptPropertiesForDiagram
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDiagramConceptPropertiesForDiagram.SetDiagram(Value: 
        TThesaurusDiagram);
begin
  inherited SetDiagram(Value);
  
  Properties := TThesaurusDiagramDisplayProperties(
      Value.DiagramDisplayProperties).ConceptDisplayProperties;
end;  // TfraDiagramConceptPropertiesForDiagram.SetDiagram 

{$R *.dfm}

end.

