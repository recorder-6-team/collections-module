{===============================================================================
  Unit:        MacroGenerator.pas

  Defines:     TMacroGenerator

  Description: Returns the result of the Macro Output. (Currently Not Funded)

  Model:       CollectionBrowserFramework.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/09/03 12:52 $
    $Author: Markaddis $

===============================================================================}
unit MacroGenerator;

interface

type
  TMacroType = (mtAccession,
                mtLoan,
                mtMovement,
                mtSpecimenRegistration);

  TMacroGenerator = class
  public
    class function GetNumber(AMacroType: TMacroType): string;
  end;

implementation

// function that returns the output of the macro
// returns an empty string because currently not funded
class function TMacroGenerator.GetNumber(AMacroType: TMacroType) : string;
begin
  case ord(AMacroType) of
    0:  GetNumber := '';
    1:  GetNumber := '';
    2:  GetNumber := '';
    3:  GetNumber := '';
  else
    GetNumber := '';
  end;
end;

end.
 