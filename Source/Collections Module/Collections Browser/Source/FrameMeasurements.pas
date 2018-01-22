{===============================================================================
  Unit:        FrameMeasurements.pas

  Defines:     FrameMeasurements

  Description: Measurement frame for CollectionUnit module.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 9/02/04 17:18 $
    $Author: Bencollier $

===============================================================================}

unit FrameMeasurements;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit;

type
  {-----------------------------------------------------------------------------
    Empty page control that has the following tabs embedded to display details of a 
    measurement:
    TfraMeasurementsGeneral
    TfraSources
  }
  TfraMeasurements = class (TBasePageControlFrame)
  protected
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    procedure AddTabs; override;
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
  Load data related to selected measurement. 
}
procedure TfraMeasurements.AddTabs;
begin
  AddTab('Measurements', TfraMeasurementsGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraMeasurements.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraMeasurements.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  inherited;
  // FrameMeasurementsGeneral is used in various places with different tables
  // But in this instance, it's Collection_Unit_Data.
  if AFrame is TfraMeasurementsGeneral then
    TfraMeasurementsGeneral(AFrame).MasterFrameType := mftCollectionUnit;
end;  // TfraMeasurements.SetFrameProperties 

end.


