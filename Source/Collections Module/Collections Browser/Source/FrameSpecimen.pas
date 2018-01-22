{===============================================================================
  Unit:        FrameSpecimen

  Defines:     TfraSpecimen

  Description:

  Created:     May 2003

  Last revision information:
    $Revision: 6 $
    $Date: 9/02/04 17:43 $
    $Author: Bencollier $

===============================================================================}

unit FrameSpecimen;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit;

type
  {-----------------------------------------------------------------------------
    Details screen for viewing and editing information regarding a specimen.  Empty page 
    control onto which the following tab pages are embedded as required:
    TfraSpecimenGeneral
    TfraSources
  }
  TfraSpecimen = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameSpecimenGeneral, FrameMetaData, FrameDescriptors, FrameMaterialsDocumented,
  FrameSources, ResourceStrings;

{-==============================================================================
    TfraSpecimen
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSpecimen.AddTabs;
begin
  AddTab('General', TfraSpecimenGeneral);
  AddTab('MetaData', TfraMetadata);
  AddTab('Descriptors', TfraDescriptors);
  AddTab('Materials', TfraMaterialsDocumented);
  //Next line for prototype only
  AddTab('Sources', TfraSources);
end;  // TfraSpecimen.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimen.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := 'Specimen'
  else
  if AFrame is TfraDescriptors then
    TfraDescriptors(AFrame).MasterFrameType := mftCollectionUnit
  else
  if AFrame is TfraMaterialsDocumented then
    TfraMaterialsDocumented(AFrame).MasterFrameType := mftCollectionUnit;
end;  // TfraSpecimen.SetFrameProperties 

end.


