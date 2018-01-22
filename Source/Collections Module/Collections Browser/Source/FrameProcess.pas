{===============================================================================
  Unit:        FrameProcess.pas

  Defines:     TfraProcess

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/04/04 11:55 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameProcess;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings,
  DataTypes, BaseDetailFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Empty page control that has the following tabs embedded to display details of a process 
    associated with a specimen or store:
  }
  TfraProcess = class (TBasePageControlFrame)
  protected
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameProcessGeneral, FrameSources;

{-==============================================================================
    TfraProcess
===============================================================================}
{-------------------------------------------------------------------------------
  Load data related to selected multimedia item. 
}
procedure TfraProcess.AddTabs;
begin
  AddTab('General', TfraProcessGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraProcess.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraProcess.SetFrameProperties(AFrame: TBaseDetailFrame);
var
  lTopNodeContext: TNodeContext;
begin
  inherited;

  lTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);
  
  if lTopNodeContext = ncCollection then
    TfraProcessGeneral(AFrame).MasterFrameType := mftCollection
  else if lTopNodeContext = ncSpecimen then
    TfraProcessGeneral(AFrame).MasterFrameType := mftSpecimen
  else if lTopNodeContext = ncStore then
    TfraProcessGeneral(AFrame).MasterFrameType := mftStore;
end;  // TfraProcess.SetFrameProperties 


end.
