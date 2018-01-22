{===============================================================================
  Unit:        FrameFunding.pas

  Defines:     TfraFunding

  Description: Container frame for funding related data.

  Created:     May 2003

  Last revision information:
    $Revision: 5 $
    $Date: 13/04/04 11:47 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameFunding;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, DataTypes, BaseDetailFrameUnit;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a funding record for a job.  This frame 
    consists of an empty tab control onto which the following tabs are embedded in this 
    order: TfraFundingGeneral, TfraSources.
  }
  TfraFunding = class (TBasePageControlFrame)
  protected
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameFundingGeneral, FrameSources, BaseTabSheetFrameUnit, ResourceStrings,
  LuxembourgConstants;

{-==============================================================================
    TfraFunding
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFunding.AddTabs;
begin
  AddTab('General', TfraFundingGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraFunding.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraFunding.SetFrameProperties(AFrame: TBaseDetailFrame);
var
  lNodeContext: TNodeContext;
begin
  inherited;
  
  if AFrame is TfraFundingGeneral then
  begin
    lNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);

    case lNodeContext of
      ncLoan,
      ncMovement,
      ncAccession:    TfraFundingGeneral(AFrame).IsMovement := True;
    else
      TfraFundingGeneral(AFrame).IsMovement := False;
    end;
  end;
end;  // TfraFunding.SetFrameProperties 

end.

