{===============================================================================
  Unit:        FrameMeasurementsLocation.pas

  Defines:     TfraMeasurementsLoc

  Description: Measurement frame for Location Features.

  Model:       Occurrences.mpb

  Created:     November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 1/09/04 17:03 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameMeasurementsLocation;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit;

type
  TfraMeasurementsLoc = class(TBasePageControlFrame)
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
    TfraMeasurementsLoc
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsLoc.AddTabs;
begin
  AddTab(ResStr_General, TfraMeasurementsGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraMeasurementsLoc.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraMeasurementsLoc.GetCaption: String;
begin
  if Tabs[0].ContainedFrame is TfraMeasurementsGeneral then
    Result := TfraMeasurementsGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraMeasurementsLoc.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraMeasurementsLoc.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  inherited;
  // FrameMeasurementsGeneral is used in various places with different tables
  // But in this instance, it's Collection_Unit_Data.
  if AFrame is TfraMeasurementsGeneral then
    TfraMeasurementsGeneral(AFrame).MasterFrameType := mftLocationFeature;
end;  // TfraMeasurementsLoc.SetFrameProperties 

end.


