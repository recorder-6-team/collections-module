{===============================================================================
  Unit:        FrameJob.pas

  Defines:     fraJob

  Description: Container frame for Job details and funding.

  Created:     May 2003

  Last revision information:
    $Revision: 4 $
    $Date: 9/02/04 17:32 $
    $Author: Bencollier $

===============================================================================}

unit FrameJob;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit,
  ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a job.  This frame consists of an empty 
    tab control onto which the following tabs are embedded in this order: TfraJobGeneral, 
    TfraJobMaterials, TfraSources.
  }
  TfraJob = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameJobGeneral, FrameMaterials, FrameSources;

{-==============================================================================
    TfraJob
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraJob.AddTabs;
begin
  AddTab('General', TfraJobGeneral);
  AddTab('Materials', TfraMaterials);
  AddTab('Sources', TfraSources);
end;  // TfraJob.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraJob.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMaterials then
    TfraMaterials(AFrame).MasterFrameType := mftJob;
end;  // TfraJob.SetFrameProperties 

end.

