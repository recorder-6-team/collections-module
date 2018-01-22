{===============================================================================
  Unit:        FrameOccurrence

  Defines:     TfraOccurrence

  Description: Page control frame for occurrence detail screen.

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 19/12/06 14:36 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameOccurrence;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes, Dialogs,
  Variants, BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit,
  DSSDataTypes;

type
  TfraOccurrence = class (TBasePageControlFrame)
    procedure pcDetailsChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FHasPreferredDetermination: Boolean;
  protected
    procedure AddTabs; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  public
    property HasPreferredDetermination: Boolean read FHasPreferredDetermination;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, FrameOccurrenceGeneral, FrameDescriptors, FrameRelatedOccurrences,
  FrameSpecimens, FrameSources, ResourceStrings;

{-==============================================================================
    TfraOccurrence
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraOccurrence.AddTabs;
begin
  //Next line for prototype only
  AddTab(ResStr_General, TfraOccurrenceGeneral);
  AddTab(ResStr_Descriptors, TfraDescriptors);
  AddTab(ResStr_RelatedOccurrences, TfraRelatedOccurrences);
  AddTab(ResStr_Specimens, TfraSpecimens);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraOccurrence.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraOccurrence.EnableControls(AEnabled: Boolean);
begin
  // If changing to edit mode when occurence doesn't yet have a preferred determination,
  // focus on first tab, and allow changes on that one only.
  if AEnabled and not HasPreferredDetermination then
    pcDetails.ActivePageIndex := 0;
  inherited;
end;  // TfraOccurrence.EnableControls 

{-------------------------------------------------------------------------------
}
function TfraOccurrence.GetCaption: String;
begin
  if Tabs[0].ContainedFrame is TfraOccurrenceGeneral then
    Result := TfraOccurrenceGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraOccurrence.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraOccurrence.LoadData;
begin
  // Find out if this occurrence already has a preferred determination.
  FHasPreferredDetermination := VarToStr(dmGeneral.GetStoredProcOutputParam(
                                'usp_PreferredDeterminationName_Get_ForOccurrence',
                                ['@OccurrenceKey', Key], '@ItemName')) <> '';
  // Carry on loading the rest.
  inherited;
end;  // TfraOccurrence.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraOccurrence.pcDetailsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  inherited;
  // Allow all tabs to be displayed, but only if browsing or editing after a
  // preferred determination has been set.
  AllowChange := (EditMode = emBrowse) or
                 ((EditMode = emEdit) and HasPreferredDetermination);
  if not AllowChange then
    MessageDlg(ResStr_MissingDetermination, mtInformation, [mbOk], 0);
end;  // TfraOccurrence.pcDetailsChanging 

{-------------------------------------------------------------------------------
}
procedure TfraOccurrence.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  inherited;
  if AFrame is TfraDescriptors then
    TfraDescriptors(AFrame).MasterFrameType := mftOccurrence;
end;  // TfraOccurrence.SetFrameProperties 

end.




 
