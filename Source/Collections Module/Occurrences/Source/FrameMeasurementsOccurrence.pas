{===============================================================================
  Unit:        FrameMeasurementsOccurrence.pas

  Defines:     TfraMeasurements

  Description: Measurement frame for Occurrences module.

  Model:       Occurrences.mpb

  Created:     November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 1/09/04 17:04 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMeasurementsOccurrence;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit;

type
  TfraMeasurementsOcc = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: String; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameMeasurementsGeneral, FrameSources, BaseTabSheetFrameUnit, ResourceStrings;

{-==============================================================================
    TfraMeasurements
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsOcc.AddTabs;
begin
  AddTab(ResStr_General, TfraMeasurementsGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraMeasurements.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraMeasurementsOcc.GetCaption: String;
begin
  if Tabs[0].ContainedFrame is TfraMeasurementsGeneral then
    Result := TfraMeasurementsGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraMeasurements.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsOcc.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  inherited;
  // FrameMeasurementsGeneral is used in various places with different tables
  // But in this instance, it's Collection_Unit_Data.
  if AFrame is TfraMeasurementsGeneral then
    TfraMeasurementsGeneral(AFrame).MasterFrameType := mftOccurrence;
end;  // TfraMeasurements.SetFrameProperties 

end.


