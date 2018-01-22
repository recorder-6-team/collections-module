{===============================================================================
  Unit:        JobSeed

  Defines:     TfraJobSeed

  Description:

  Created:     September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/03/04 15:35 $
    $Author: Ericsalmon $

===============================================================================}

unit JobSeedFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls, ExceptionForm;

type
  TfraJobSeed = class (TfraBaseStep)
    eJobSeed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TStepFrameClass; override;
    function GetNextCaption: String; override;
    function GetPrevious: TStepFrameClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  Settings, LanguagesFrame, DataSetupFrame, GeneralFunctions;

resourcestring
  ResStr_InvalidSeed = 'The seed must be a valid positive integer value.';

{-==============================================================================
    TfraJobSeed
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraJobSeed.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraJobSeed.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraJobSeed.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraJobSeed.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraJobSeed.GetNext: TStepFrameClass;
begin
  Result := TfraDataSetup;
end;  // TfraJobSeed.GetNext

{-------------------------------------------------------------------------------
}
function TfraJobSeed.GetNextCaption: String;
begin
  Result := '&Install';
end;  // TfraJobSeed.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraJobSeed.GetPrevious: TStepFrameClass;
begin
  Result := TfraLanguages;
end;  // TfraJobSeed.GetPrevious 

{-------------------------------------------------------------------------------
}
procedure TfraJobSeed.LoadContent;
begin
  inherited;
  eJobSeed.Text := Settings.JobSeed;
end;  // TfraJobSeed.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraJobSeed.SaveContent;
begin
  inherited;
  if eJobSeed.Text <> '' then
    ValidateValue(IsInt(eJobSeed.Text) and (StrToInt(eJobSeed.Text) > 0),
                  ResStr_InvalidSeed, eJobSeed);
  Settings.JobSeed := eJobSeed.Text;
end;  // TfraJobSeed.SaveContent

end.


