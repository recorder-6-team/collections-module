{===============================================================================
  Unit:        FrameStorage.pas

  Defines:     TfraStorage

  Description:

  Model:       CollectionsCollectionsAndStorage.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 11 $
    $Date: 3/09/04 16:22 $
    $Author: Johnvanbreda $

===============================================================================}

unit FrameStorage;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit,
  StdCtrls, Buttons;

type
  {-----------------------------------------------------------------------------
    Screen to display and allow editing of the basic details of a store.  Empty page control 
    onto which the following tabs are embedded as required:
    TfraStorageGeneral, TfraMetadata, TfraDescriptors, TfraMaterials, TfraSources
  }
  TfraStorage = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameStorageGeneral, FrameMetaData, FrameDescriptors, FrameMaterials,
  FrameSources, ResourceStrings, FrameStorageLayout;

{-==============================================================================
    TfraStorage
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraStorage.AddTabs;
begin
  AddTab('General', TfraStorageGeneral);
  AddTab('MetaData', TfraMetadata);
  AddTab('Descriptors', TfraDescriptors);
  AddTab('Materials', TfraMaterials);
  AddTab('Store Layout', TfraStorageLayout);
  AddTab('Sources', TfraSources);
end;  // TfraStorage.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraStorage.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraMetadata then
    TfraMetadata(AFrame).TableName := 'Store'
  else
  if AFrame is TfraDescriptors then
    TfraDescriptors(AFrame).MasterFrameType := mftCollectionUnit
  else
  if AFrame is TfraMaterials then
    TfraMaterials(AFrame).MasterFrameType := mftCollectionUnit;
end;  // TfraStorage.SetFrameProperties 

end.


