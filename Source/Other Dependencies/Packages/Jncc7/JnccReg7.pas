unit JnccReg7;

interface

uses
  Classes, Finder, JnccDatasets, JnccGrid, Measurements,
  Sources, SpatialRef, VagueDateEdit, ImportWizardDBGrid, DBFilterGrid;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('JNCC', [TSpatialRef,
                              TMeasurements,
                              TSources,
                              TVagueDateEdit,
                              TJnccTable,
                              TJnccQuery,
                              TJNCCNamesQuery,
                              TDBJnccGrid,
                              TFinder,
                              TImportWizardDBGrid,
                              TDBFilterGrid]);
end;  { Register }

//==============================================================================
end.

