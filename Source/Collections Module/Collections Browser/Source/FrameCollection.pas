{===============================================================================
  Unit:        FrameCollection.pas

  Defines:     TfraCollection

  Description: Detail screen for colleciton items

  Model:       CollectionsCollectionsAndStorage.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 9 $
    $Date: 9/02/04 17:27 $
    $Author: Bencollier $

===============================================================================}

unit FrameCollection;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit;

type
  {-----------------------------------------------------------------------------
    Screen to display and allow editing of the basic details of a collection.  Empty page 
    control onto which the following tabs are embedded as required:
    TfraCollectionGeneral, TfraMetadata, TfraCollectionTimePeriod, TfraDescriptors, 
    TfraMaterials, TfraSources.
  }
  TfraCollection = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameCollectionGeneral, FrameMetaData, FrameCollectionTimePeriod,
  FrameDescriptors, FrameMaterials, FrameSources, LuxembourgConstants;

{-==============================================================================
    TfraCollection
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraCollection.AddTabs;
begin
  AddTab('General', TfraCollectionGeneral);
  AddTab('MetaData', TfraMetadata);
  AddTab('Time Period', TfraCollectionTimePeriod);
  AddTab('Descriptors', TfraDescriptors);
  AddTab('Materials', TfraMaterials);
  AddTab('Sources', TfraSources);
end;  // TfraCollection.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraCollection.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := TN_COLLECTION
  else
  if AFrame is TfraDescriptors then
    TfraDescriptors(AFrame).MasterFrameType := mftCollectionUnit
  else
  if AFrame is TfraMaterials then
    TfraMaterials(AFrame).MasterFrameType := mftCollectionUnit;
end;  // TfraCollection.SetFrameProperties 

end.
