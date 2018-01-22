{===============================================================================
  Unit:        DiagramConceptPropertiesManager

  Defines:     TdlgDiagramConceptPropertiesManager

  Description: A dialog for setting concept display properties in the diagram

  Model:       Thesaurus.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 13/02/04 12:43 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptPropertiesManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDiagramPropertiesManager, ComCtrls, StdCtrls,
  ImageListButton, DiagramConceptPropertiesInherited, ExceptionForm,
  DiagramConceptPropertiesForCG, BaseDiagramObjectProperties,
  DiagramConceptProperties, DiagramConceptPropertiesForDiagram,
  DiagramConceptPropertiesForConcept, ThesaurusDiagramObjects;

type
  EDiagramConceptPropertiesManagerException = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Dialog allowing the user to specify the display properties for concepts on 
    a diagram.
    The user is able to specify settings that apply to the entire diagram, to 
    an enter concept group (i.e. all concepts in a given group) or to a single 
    concept
    Note that this dialog must be owned by the diagram.
  }
  TdlgDiagramConceptPropertiesManager = class (TdlgBaseDiagramPropertiesManager)
    fraDiagramConceptPropertiesForCG: TfraDiagramConceptPropertiesForCG;
    fraDiagramConceptPropertiesForConcept: 
        TfraDiagramConceptPropertiesForConcept;
    fraDiagramConceptPropertiesForDiagram: 
        TfraDiagramConceptPropertiesForDiagram;
  public
    constructor Create(AOwner : TComponent); override;
  end;
  
implementation

uses
  ThesaurusDiagram, ResourceStrings;

{$R *.dfm}

{-==============================================================================
    TdlgDiagramConceptPropertiesManager
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgDiagramConceptPropertiesManager.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  fraDiagramConceptPropertiesForDiagram.Diagram := TThesaurusDiagram(AOwner);
  fraDiagramConceptPropertiesForCG.Diagram := TThesaurusDiagram(AOwner);
  fraDiagramConceptPropertiesForConcept.Diagram := TThesaurusDiagram(AOwner);
  pcTabs.ActivePage := tsItem;
end;  // TdlgDiagramConceptPropertiesManager.Create 



end.



