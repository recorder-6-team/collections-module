{===============================================================================
  Unit:        QEImportColWidthsSelect.pas

  Defines:     TfraQEImportColWidthsSelect

  Description: Wizard page that allows the user to divide the import file into
               a number of columns at fixed points.  

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 4 $
    $Date: 13/12/07 17:01 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImportColWidthsSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FixedWidthColumnSelector, StdCtrls, QEImportFrameUnit;

type
  TfraQEImportColWidthsSelect = class(TQEImportFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FixedWidthColumnSelector: TFixedWidthColumnSelector;
  protected
    function GetNext: TQEImportFrameClass; override;
    procedure LoadText; virtual;
  public
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;

implementation

{$R *.dfm}

uses
  QEImportColumnMatching;

{-==============================================================================
    TfraQEImportColWidthsSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraQEImportColWidthsSelect.LoadContent;
begin
  LoadText;
  SetIsComplete(True);
end;  // TfraQEImportColWidthsSelect.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColWidthsSelect.LoadText;
var
  lText: TStringList;
begin
  lText := TStringList.Create;
  try
    try
      lText.LoadFromFile(Definition.DataSource);
    except
      on E: EFOpenError do
      begin
        MessageDlg(
            Format(ResStr_FileOpenError, [E.Message]),
            mtWarning,
            [mbOk],
            0);
        Definition.CloseWindow;
      end;
    end;
    FixedWidthColumnSelector.LoadText(lText);
  finally
    lText.Free;
  end;
end;  // TfraQEImportColWidthsSelect.LoadText 

{-------------------------------------------------------------------------------
}
function TfraQEImportColWidthsSelect.GetNext: TQEImportFrameClass;
begin
  Result := TfraQEImportColumnMatching;
end;  // TfraQEImportColWidthsSelect.GetNext 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColWidthsSelect.SaveContent;
var
  I: Integer;
  lStart: Integer;
begin
  Definition.ClearFixedWidthColumns;

  lStart := 0;
  for I := 0 to FixedWidthColumnSelector.BreakCount - 1 do
  begin
    Definition.AddFixedWidthColumn(
        FixedWidthColumnSelector.Breaks[I] - lStart);
    lStart := FixedWidthColumnSelector.Breaks[I];    
  end;
  
  Definition.AddFixedWidthColumn(
      FixedWidthColumnSelector.LineLength - lStart);
end;  // TfraQEImportColWidthsSelect.SaveContent 

end.
