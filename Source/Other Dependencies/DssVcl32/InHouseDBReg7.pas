{===============================================================================

  Copyright © Dorset Software Services Ltd, 1999

  Unit:
    InHouseReg.pas - Eric Salmon 14/04/1999

  Registration of the following components:
    TDBGlyphLookupListBox  in DBGlyphCtrls
    TDBGlyphLookupComboBox in DBGlyphCtrls
    TDBHintGrid            in DBHintGrid
    TDBRTFGrid             in RTFGrid

  Updates:

  Packages:
    InHouseDB7, Delphi 7 package for in house components.

  Description:
    Registers all TESTED and STABLE components going on the In House palette
    tab, removing the need for each component to have its own Register function.
    Glyphs for the components are stored in the associated component resouce
    file InHouseReg.dcr.

    See individual units for more information on each components.

===============================================================================}

unit InHouseDBReg7;

interface

uses
  Classes, DBGlyphCtrls, DBHintGrid, RTFGrid;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('In House DB', [TDBGlyphLookupListBox,
                                     TDBGlyphLookupComboBox,
                                     TDBHintGrid,
                                     TDBRTFGrid]);
end;  { Register }

//==============================================================================
end.

