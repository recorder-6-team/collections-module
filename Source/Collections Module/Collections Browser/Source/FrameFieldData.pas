{===============================================================================
  Unit:        FrameFieldData.pas

  Defines:     TfraFieldData

  Description: field data details.

  Created:     September 2007

  Last revision information:
    $Revision: 1 $
    $Date: 12/09/07 13:09 $
    $Author: Davidkelly $

===============================================================================}

unit FrameFieldData;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type 
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a field data record.  This frame consists
    of an empty tab control onto which the following tabs are embedded in this order:
    TfraFieldDataGeneral, TfraFieldDataGeoAreas.
  }
  TfraFieldData = class(TBasePageControlFrame)
  protected
    procedure AddTabs; override;
  end;
      
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameFieldDataGeneral, FrameFieldDataGeoAreas;

{-==============================================================================
    TfraFieldData
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFieldData.AddTabs;
begin
  AddTab('General', TfraFieldDataGeneral);
  AddTab('Geo. Areas', TfraFieldDataGeoAreas);
end;  // TfraFieldData.AddTabs

end.
