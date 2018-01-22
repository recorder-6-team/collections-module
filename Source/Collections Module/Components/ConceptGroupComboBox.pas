{===============================================================================
  Unit:        ConceptGroupComboBox

  Defines:     TConceptGroupComboBox

  Description: A combo box linked to a concept group. The first item for a 
               DropDownList combobox is considered to be the <no selection> item,
               meaning that selecting this item will set the text in the edit box
               to blank, and set the key to an empty string.

  Model:       Components

  Created:     August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 10/09/03 17:52 $
    $Author: Ericsalmon $

===============================================================================}

unit ConceptGroupComboBox;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, LuxIDComboBox;

type
  TConceptGroupComboBox = class (TLuxIDComboBox)
  end;
  
//==============================================================================
implementation

end.
