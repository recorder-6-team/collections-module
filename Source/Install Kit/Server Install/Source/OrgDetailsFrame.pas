{===============================================================================
  Unit:        OrgDetailsFrame

  Defines:     TfraOrgDetails

  Description: Details of organisation which is linked to the installation.

  Created:     September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 10/03/04 9:13 $
    $Author: Johnvanbreda $

===============================================================================}

unit OrgDetailsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, StdCtrls;

type
  TfraOrgDetails = class (TfraBaseStep)
    eAcronym: TEdit;
    eOrgName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure eOrgNameChange(Sender: TObject);
  protected
    function GetHasNext: Boolean; override;
    function GetNext: TStepFrameClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  Settings, BarcodeFrame;

{-==============================================================================
    TfraOrgDetails
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraOrgDetails.eOrgNameChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraOrgDetails.eOrgNameChange 

{-------------------------------------------------------------------------------
}
function TfraOrgDetails.GetHasNext: Boolean;
begin
  Result := eOrgName.Text <> '';
end;  // TfraOrgDetails.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraOrgDetails.GetNext: TStepFrameClass;
begin
  Result := TfraBarcode;
end;  // TfraOrgDetails.GetNext

{-------------------------------------------------------------------------------
}
procedure TfraOrgDetails.LoadContent;
begin
  inherited;
  eOrgName.Text := Settings.OrgFullName;
  eAcronym.Text := Settings.OrgAcronym;
end;  // TfraOrgDetails.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraOrgDetails.SaveContent;
begin
  inherited;
  Settings.OrgFullName := eOrgName.Text;
  Settings.OrgAcronym := eAcronym.Text;
end;  // TfraOrgDetails.SaveContent 

end.
