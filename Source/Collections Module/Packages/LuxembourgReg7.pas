{===============================================================================
  Unit:        LuxembourgReg7

  Defines:     <nothing>

  Description: Registration unit for Luxembourg7 package for the following
               components:
               TLinkedEdit           in LinkedControls
               TLuxIDComboBox        in LuxIDComboBox
               TConceptGroupComboBox in ConceptGroupComboBox
               TBarCode              in BarCode
               TTermLabel            in TermLabel

  Created:     August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 15/06/11 9:56 $
    $Author: Jamesbichard $

===============================================================================}

unit LuxembourgReg7;

interface

uses
  Classes, LuxIDComboBox, ConceptGroupComboBox, BarCode,
  TermLabel, FilteredStringGrid;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('Luxembourg', [TLuxIDComboBox,
                                    TConceptGroupComboBox,
                                    TBarCode,
                                    TTermLabel,
				    TPopupButton,
				    TFilteredStringGrid]);
end;

//==============================================================================
end.
 