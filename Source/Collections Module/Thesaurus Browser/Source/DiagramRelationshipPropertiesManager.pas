{===============================================================================
  Unit:        DiagramRelationshipPropertiesManager

  Defines:     TdlgThesaurusDiagramPropertiesManager

  Description: A dialog for setting display properties in the diagram

  Model:       Thesaurus.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 7/01/04 16:46 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipPropertiesManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDiagramPropertiesManager, ComCtrls, StdCtrls, ImageListButton,
  BaseDiagramObjectProperties,  DiagramRelationshipPropertiesForRel,
  DiagramRelationshipPropertiesInherited,
  DiagramRelationshipPropertiesForType, DiagramRelationshipProperties, 
  DiagramRelationshipPropertiesForDiagram;

type
  {-----------------------------------------------------------------------------
    Dialog allowing the user to specify the display properties for 
    relationships on a diagram.
    The user is able to specify settings that apply to the entire diagram, to 
    an enter relationship type (i.e. all relationships of a given type) or to a 
    single relationship.
  }
  TdlgDiagramRelationshipPropertiesManager = class (
      TdlgBaseDiagramPropertiesManager)
    fraDiagramRelationshipPropertiesForDiagram: 
        TfraDiagramRelationshipPropertiesForDiagram;
    fraDiagramRelationshipPropertiesForRel: 
        TfraDiagramRelationshipPropertiesForRel;
    fraDiagramRelationshipPropertiesForType: 
        TfraDiagramRelationshipPropertiesForType;
  public
    constructor Create(AOwner : TComponent); override;
  end;
  

implementation

uses 
  ThesaurusDiagram;

{$R *.dfm}

{-==============================================================================
    TdlgDiagramRelationshipPropertiesManager
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TdlgDiagramRelationshipPropertiesManager.Create(AOwner : 
    TComponent);
begin
  inherited Create(AOwner);
  
  fraDiagramRelationshipPropertiesForDiagram.Diagram := TThesaurusDiagram(
      AOwner);
  fraDiagramRelationshipPropertiesForRel.Diagram     := TThesaurusDiagram(
      AOwner);
  fraDiagramRelationshipPropertiesForType.Diagram    := TThesaurusDiagram(
      AOwner);
end;  // TdlgDiagramRelationshipPropertiesManager.Create 



end.



