{===============================================================================
  Unit:        DiagramXMLConstants

  Defines:     list of constants

  Description: XML element and attribute constants for the Thesaurus Diagrams

  Created:     Dec 2003

  Model:       None

  Last revision information:
    $Revision: 1 $
    $Date: 3/09/04 10:28 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramXMLConstants;

interface

uses
  XMLIntf;

function HasChildNode(AXMLNode: IXMLNode; const AName: string): boolean;

const
  // XML Elements
  EL_BRUSH =               'brush';
  EL_CONCEPT =             'concept';
  EL_CONCEPTS =            'concepts';
  EL_CONCEPTDISPLAY =      'concept_display';
  EL_CONCEPTGROUP=         'concept_group';
  EL_CONCEPTGROUPS=        'concept_groups';
  EL_DIAGRAMDISPLAY =      'diagram_display';
  EL_FONT =                'font';
  EL_LABELS =              'labels';
  EL_PEN =                 'pen';
  EL_RELATIONSHIP =        'relationship';
  EL_RELATIONSHIPS =       'relationships';
  EL_RELATIONSHIPDISPLAY = 'relationship_display';
  EL_RELATIONTYPE=       'thesaurus_relation_type';
  EL_RELATIONTYPES=      'thesaurus_relation_types';
  EL_THESAURUSDIAGRAM=   'thesaurus_diagram';
  EL_TITLE=              'title_detail';

  // XML attributes
  AT_BOLD =                'bold';
  AT_CHARSET =             'charset';
  AT_COLOUR =              'colour';
  AT_CONCEPTKEY =          'concept_key';
  AT_CONCEPTGROUPKEY=      'concept_group_key';
  AT_FORWARDTERM =         'forward_term';
  AT_FROMOBJECTID =        'from_object_id';
  AT_GRIDSIZE =            'grid_size';
  AT_ITALIC =              'italic';
  AT_LEFT =                'left';
  AT_MAINTERM =            'main_term';
  AT_NAME =                'name';
  AT_OBJECTID =            'object_id';
  AT_RELATIONTYPEKEY=      'thesaurus_relation_type_key';
  AT_REVERSETERM =         'reverse_term';
  AT_SHOWGRID =            'show_grid';
  AT_SHOWTITLE =           'show_title';
  AT_SNAPTOGRID =          'snap_to_grid';
  AT_SIZE =                'size';
  AT_STRIKEOUT =           'strikeout';
  AT_TITLE =               'title';
  AT_TITLEPOSX =           'title_x';
  AT_TITLEPOSY =           'title_y';
  AT_TOOBJECTID =          'to_object_id';
  AT_TOP =                 'top';
  AT_UNDERLINE =           'underline';
  AT_WIDTH =               'width';

implementation


function HasChildNode(AXMLNode: IXMLNode; const AName: string): boolean;
var
  lIdx: integer;
begin
  Result := False;
  if Assigned(AXMLNode) then
    for lIdx := 0 to AXMLNode.ChildNodes.Count-1 do
      if AXMLNode.ChildNodes[lIdx].NodeName=AName then begin
        Result := True;
        Break;
      end;
end;

end.
