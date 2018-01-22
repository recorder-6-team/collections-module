{===============================================================================
  Unit:        FrameSpecimenGeneral.pas

  Defines:     TfraSpecimenGeneral

  Description:

  Created:     May 2003
===============================================================================}

unit FrameSpecimenGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  ComboListID, LuxIDComboBox, BaseCompositeComponent, LinkedControls,
  InterfaceDataModule, DataClasses, ConceptGroupComboBox, SearchManager,
  DataTypes, TermLabel, UserMessages, RapTree;

type
  {-----------------------------------------------------------------------------
    General tab page control for the details of a specimen.  Most information on this
    control summarises information entered elsewhere in the system and is read only on this
    control.
  }
  TfraSpecimenGeneral = class(TBaseTabSheetFrame)
    chkConfidential: TCheckBox;
    chkDangerous: TCheckBox;
    cmbType: TLuxIDComboBox;
    eCollection: TLinkedEdit;
    eCurrentLocation: TLinkedEdit;
    eCurrentLocCode: TEdit;
    eUsualLocation: TLinkedEdit;
    eUsualLocCode: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblAccNumber: TLabel;
    lblDept: TLabel;
    lblDomains: TLabel;
    lblGatheringDate: TLabel;
    lblGatheringSite: TLabel;
    lblName: TTermLabel;
    lblPrefNumber: TLabel;
    lblStatus: TLabel;
    mmPeople: TMemo;
    chkChecked: TCheckBox;
    lblConditionCheck: TLabel;
    lblLastJob: TLabel;
    lblMovementSpacer: TLabel;
    lblJobSpacer: TLabel;
    chkInternalUse: TCheckBox;
    chkPublishToWeb: TCheckBox;
    pnlThumbnail: TPanel;
    imgThumbnail: TImage;
    lblThumbnail: TLabel;
    procedure cmbTypePopulate(Sender: TObject);
    procedure DropCurrentLocation(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropUsualLocation(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure eCurrentLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eCurrentLocCodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eUsualLocationChange(Sender: TObject);
    procedure eUsualLocationEnter(Sender: TObject);
    procedure eUsualLocCodeEnter(Sender: TObject);
    procedure eUsualLocCodeChange(Sender: TObject);
    procedure lblStatusClick(Sender: TObject);
    procedure lblConditionCheckClick(Sender: TObject);
    procedure lblLastJobClick(Sender: TObject);
    procedure lblDomainsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblGatheringDateMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lblGatheringSiteMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FCUTimestamp: TSQLSvrTimestamp;
    FDomainMask: Integer;
    FLifeSciences: Boolean;
    FSpecimenTypeLoaded: String;
    FSUTimestamp: TSQLSvrTimestamp;
    FUsualCurrentLocationMatch: Boolean;
    FUsualCurrentLocationCodeMatch: Boolean;
    FMaterialCollectionUnitKey: String;
    FMovementType: Integer;
    FConditionCheckKey: String;
    FLastJobKey: String;
    function AddOrDeleteStore: Boolean;
    function GetThumbnailParams: TVariantArray;
    function ThumbnailFileName(const ImageFileName: String): String;
    procedure UpdateCurrentLocation;
    procedure UpdateCurrentLocationCode;
    procedure HideThumbnail;
    procedure LoadThumbnail(const FileName: String);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    function GetParams: TVariantArray;
    function GetStatusParams: TVariantArray;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  public
    procedure CancelChanges; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  DropTarget, Validation, GeneralData, LuxembourgConstants,
  GeneralFunctions, ResourceStrings, BaseADODataModule,
  BaseDetailFrameUnit, ADOInt;

{-==============================================================================
    TfraSpecimenGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraSpecimenGeneral.AddOrDeleteStore: Boolean;
var
  sParams: Array of Variant;
  lStoreName: String;
begin
  Result := true;
  if lblName.Caption = '' then
    lStoreName := ResStr_Unspecified
  else
    lStoreName := RemoveSubStrings(lblName.Caption, ['<i>', '</i>']);
  
  sParams := VarArrayOf(['@Key', Key, '@ItemName', lStoreName]);
  
  // If details are edited and Store is selected in cmbType create store
  // record for the CollectionUnitKey
  if (cmbType.StrID[cmbType.ItemIndex] = 'SYSTEM000000008U') and //Store
     (FSpecimenTypeLoaded <> 'SYSTEM000000008U') then
    dmGeneral.RunStoredProc('usp_Store_Insert_ForSpecimen',sParams)
  // If details are edited and Store is de-selected in cmbType
  else
  if (FSpecimenTypeLoaded = 'SYSTEM000000008U') and
     (cmbType.CurrentStrID <> 'SYSTEM000000008U') then
    // Display delete warning message with Yes/No
    if ConfirmDeletionYesNo(ResStr_DeleteStore) = mrYes then
      // If Yes then delete store record
      dmGeneral.RunStoredProc('usp_Store_Delete_ForSpecimen',['@Key',Key])
    else
      Result := false;
end;  // TfraSpecimenGeneral.AddOrDeleteStore 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.CancelChanges;
begin
  { We might have a Key because a Store has been inserted that is a Specimen,
    and the user was prompted to come here. However, we need to check whether
    the Specimen_Unit exists.}
  if (Key <> '') and
     (dmGeneral.GetStoredProcOutputParam('usp_SpecimenExists_Get',
                                         ['@Key', Key], '@Exists') = False) then
    MessageDlg(ResStr_StoreNotRecordedAsSpecimen, mtInformation, [mbOk], 0);
end;  // TfraSpecimenGeneral.CancelChanges

{-------------------------------------------------------------------------------
  Populates the combo box with the required data. 
}
procedure TfraSpecimenGeneral.cmbTypePopulate(Sender: TObject);
begin
  with dmGeneral.GetRecordset('usp_SpecimenTypes_Select', ['@Mask', FDomainMask]) do
    while not EOF do begin
      cmbType.Add(VarToStr(Fields['PlainText'].Value),
                  VarToStr(Fields['Concept_Key'].Value));
      MoveNext;
    end; // while
end;  // TfraSpecimenGeneral.cmbTypePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.DeleteData;
begin
  inherited;
  dmGeneral.RunDeleteStoredProc('usp_Specimen_Delete',
                                ['@SpecimenKey', Key, '@Timestamp', FSUTimestamp]);
end;  // TfraSpecimenGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Allows the user to drop an item into the CurrentLocation TLinkedEdit control. 
}
procedure TfraSpecimenGeneral.DropCurrentLocation(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eCurrentLocation, TN_STORE,
                     'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraSpecimenGeneral.DropCurrentLocation 

{-------------------------------------------------------------------------------
  Allows the user to drop an item into the UsualLocation TLinkedEdit control. 
}
procedure TfraSpecimenGeneral.DropUsualLocation(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eUsualLocation, TN_STORE,
                     'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraSpecimenGeneral.DropUsualLocation 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [cmbType, eUsualLocation,
                                          eCollection]);
  mmPeople.Readonly := True;
end;  // TfraSpecimenGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Gets the caption of the frame. 
}
function TfraSpecimenGeneral.GetCaption: String;
begin
  if (lblName.Caption = '') and (lblPrefNumber.Caption = '') then
    Result := ResStr_NoDetermination
  else if lblPrefNumber.Caption = '' then
    Result := lblName.Caption
  else
    Result := (lblName.Caption + ' - ' + lblPrefNumber.Caption);
end;  // TfraSpecimenGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Top level Specimens can either be a life science or earth science. Hence, when they are
      clicked on, this information needs to be passed into the usp_Specimen_Select procedure
      for each node. Hence, FLifeScience needs to be set for each node clicked on.
}
function TfraSpecimenGeneral.GetParams: TVariantArray;
begin
  FLifeSciences := AdditionalProperties.GetProperty(PROP_SPECIMEN_IS_LIFESCIENCES);
  Result := VarArrayOf(['@Key', Key, '@IsLifeScience', FLifeSciences]);
end;  // TfraSpecimenGeneral.GetParams

{-------------------------------------------------------------------------------
  Gets parameters for the Status Label stored procedure.
}
function TfraSpecimenGeneral.GetStatusParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key]);
end;  // TfraSpecimenGeneral.GetStatusParams

{-------------------------------------------------------------------------------
  Gets parameters for the thumbnail stored procedure.
}
function TfraSpecimenGeneral.GetThumbnailParams: TVariantArray;
begin
  Result := VarArrayOf([
      '@TableName', TN_SPECIMEN_UNIT,
      '@RecordKey', Key]);
end;

{-------------------------------------------------------------------------------
  Loads any extra data required by the frame. 
}
procedure TfraSpecimenGeneral.LoadData;
var
  lTopNodeContext: TNodeContext;
  conditionCheckCaption: String;
  jobCaption: String;
  movementCaption: String;
  statusHorizontalSpacing: Integer;
  statusVerticalSpacing: Integer;
begin
  inherited LoadData;
  
  lblName.Caption := '';
  mmPeople.Clear;
  
  if not RegisteredRecordsets[0].Eof then
    with RegisteredRecordsets[0] do begin
      FDomainMask         := Fields['Domain_Mask'].Value;
      FSpecimenTypeLoaded := Fields['Specimen_Type_Concept_Key'].Value;
      FSUTimestamp        := Fields['SUTimestamp'].Value;
      FCUTimestamp        := Fields['CUTimestamp'].Value;

      //FMaterialCollectionUnitKey := Fields['MaterialCollectionUnitKey'].Value;

      // The name caption is no longer an async control. The name can be easily
      // returned from using the main recordset so there is no point in having
      // to get the key and then use different stored procs to get the name again
      // depending on whether it is an earth or life science specimen.
      lblName.Caption := VarToStr(Fields['Item_Name'].Value);
    end
  else begin
    // If the recordset is empty, we are dealing with a new Specimen (or a
    // Specimen without a determination).
    lTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);
    // If the Node Context is a Collection, then we are looking at a Specimen
    // leaf node. Also, we know it is a new Specimen, so fill the eCollection
    // field with the name of the top level collection.
    if lTopNodeContext = ncCollection then
      with dmGeneral.GetRecordset('usp_Collection_Select', ['@Key', ParentKey]) do
        if not Eof then begin
          eCollection.Key := VarToStr(Fields['Collection_Unit_Key'].Value);
          eCollection.Text := VarToStr(Fields['Item_Name'].Value);
        end;
  end;
  // Load the memo box with Field Collectors.
  with RegisteredRecordsets[1] do
    if not Eof then begin
      MoveFirst;
      while not Eof do begin
        if VarToStr(Fields['GathererName'].Value) <> '' then begin
          // Add a comma if necessary.
          if mmPeople.Text <> '' then mmPeople.Text := mmPeople.Text + ', ';
          // Add the next name.
          mmPeople.Text := mmPeople.Text + VarToStr(Fields['GathererName'].Value);
        end;
        MoveNext;
      end;
    end;

  // Populate the status label data.
  with RegisteredRecordsets[2] do begin
    if not Eof then begin
      MoveFirst;

      // Set up spacing
      statusHorizontalSpacing := 14;
      statusVerticalSpacing := 14;

      conditionCheckCaption := VarToStr(Fields['ConditionStatus'].Value);
      if ((conditionCheckCaption = Null) or (conditionCheckCaption = ''))
      // Are strings like this localized?
      then
      begin
        conditionCheckCaption := 'unknown';
      end;

      // First set the Condition Caption and then work out sizes / positions from that.
      lblConditionCheck.Caption := conditionCheckCaption;
      FConditionCheckKey := VarToStr(Fields['ConditionKey'].Value);

      FMaterialCollectionUnitKey := VarToStr(Fields['MovementKey'].Value);
      if ((FMaterialCollectionUnitKey = Null)
        or (FMaterialCollectionUnitKey = '')
        or (Fields['MovementType'].Value = Null))
      then
      begin
        movementCaption := 'unknown';
        FMovementType := -1;
      end
      else
      begin
        movementCaption := VarToStr(Fields['MovementStatus'].Value);
        FMovementType :=  Fields['MovementType'].Value;
      end;

      lblStatus.Caption := movementCaption;

      lblMovementSpacer.Left := lblConditionCheck.Left + lblConditionCheck.Width + 4;
      // Figure out where the Movement label should go.
      if lblConditionCheck.Left + lblConditionCheck.Width +
        statusHorizontalSpacing + lblStatus.Width < 245
      then
      begin
        lblStatus.Top := lblConditionCheck.Top;
        lblStatus.Left := lblConditionCheck.Left + lblConditionCheck.Width + statusHorizontalSpacing;
      end
      else
      begin
        lblStatus.Left := lblConditionCheck.Left;
        lblStatus.Top := lblConditionCheck.Top + statusVerticalSpacing;
      end;

      jobCaption := VarToStr(Fields['ConservationJobTitle'].Value);
      // Finally add the last job if we can
      if ((Fields['DisplayLastJob'].Value = 1)
        and (jobCaption <> Null)
        and (jobCaption <> '')) then
      begin
        lblLastJob.Visible := True;
        lblJobSpacer.Visible := True;
        lblLastJob.Caption := jobCaption;
        FLastJobKey := VarToStr(Fields['ConservationJobKey'].Value);
        lblJobSpacer.Left := lblStatus.Left + lblStatus.Width + 4;
        lblJobSpacer.Top := lblStatus.Top;
        if lblConditionCheck.Left + lblConditionCheck.Width + lblStatus.Width +
          (2 * statusHorizontalSpacing) + lblLastJob.Width < 245
        then
        begin
          lblLastJob.Top := lblStatus.Top;
          lblLastJob.Left := lblStatus.Left + lblStatus.Width + statusHorizontalSpacing;
        end
        else
        begin
          lblLastJob.Left := lblConditionCheck.Left;
          lblLastJob.Top := lblStatus.Top + statusVerticalSpacing;
        end;
      end
      else
      begin
        lblLastJob.Visible := False;
        lblJobSpacer.Visible := False;
        FLastJobKey := '';
      end;
    end;
  end;

  // multimedia image
  if RegisteredRecordsets[3].Eof then
    HideThumbnail
  else
    LoadThumbnail(RegisteredRecordsets[3].Fields['FILE_NAME'].Value);

  eCurrentLocation.ShowHint := True;
  eCurrentLocation.Hint := 'Press F10 to default to Usual Location data';
  eCurrentLocCode.ShowHint := True;
  eCurrentLocCode.Hint := 'Press F10 to default to Usual Location data';
end;  // TfraSpecimenGeneral.LoadData

{-------------------------------------------------------------------------------
  Hides the thumbnail image.
}
procedure TfraSpecimenGeneral.HideThumbnail;
begin
  imgThumbnail.Picture.Graphic := nil;
  lblThumbnail.Visible := False;
  pnlThumbnail.Visible := False;
end;

{-------------------------------------------------------------------------------
  Loads the thumbnail image.
}
procedure TfraSpecimenGeneral.LoadThumbnail(const FileName: String);
begin
  if not FileExists(FileName) then
    HideThumbnail
  else begin
    try
      imgThumbnail.Picture.LoadFromFile(ThumbnailFileName(FileName));
      lblThumbnail.Visible := True;
      pnlThumbnail.Visible := True;
    except
      HideThumbnail;
    end;
  end
end;

{-------------------------------------------------------------------------------
  The name of the thumbnail file corresponding to the specified image file, if
  it has a thumbnail; otherwise, returns the specified file name.
}
function TfraSpecimenGeneral.ThumbnailFileName(
  const ImageFileName: String): String;
var
  lThumbnailFileName: String;
begin
  lThumbnailFileName := ImageFileName + THUMBNAIL_SUFFIX;
  if FileExists(lThumbnailFileName) then
    Result := lThumbnailFileName
  else
    Result := ImageFileName;
end;

{-------------------------------------------------------------------------------
  Registers the controls in the frame.
}
procedure TfraSpecimenGeneral.RegisterControls;
begin
  inherited RegisterControls;

  RegisterRecordset('usp_Specimen_Select', GetParams);
  RegisterRecordset('usp_Individual_Select_ForSpecimenGeneral');

  // Procedure for getting all the keys / values for status labels
  RegisterRecordset('usp_CollectionUnitStatus_Select', GetStatusParams);
  RegisterRecordset('dbo.usp_Multimedia_Select_Preferred', GetThumbnailParams);

  RegisterControl(eCurrentLocCode,'Current_Location_Code');
  RegisterControl(eUsualLocCode,'Usual_Location_Code');
  RegisterControl(eCollection, 'ParentCollectionCollectionUnitName', 'Parent_Unit_Key', True,
                  ResStr_Collections, CheckLinkedCollection, 'Collection',
                  ConvertCollectionKeyToCaption);
  RegisterAsyncControl(lblDept, 'usp_CollectionUnitDepartment_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Department');
  RegisterControl(chkDangerous,'Dangerous');
  RegisterControl(chkPublishToWeb, 'Publish_To_Web');
  RegisterControl(chkConfidential,'Confidential');
  RegisterControl(chkChecked, 'Checked');
  RegisterControl(chkInternalUse, 'Internal_Use');
  RegisterControl(eCurrentLocation,'Current_Location_Name',
                  'Current_Container_Collection_Unit_Key', CheckLinkedStore, 'Store',
                  ConvertStoreKeyToCaption);
  RegisterControl(eUsualLocation,'Usual_Location_Name',
                  'Usual_Container_Collection_Unit_Key', True, ResStr_UsualLocation,
                  CheckLinkedStore, 'Store', ConvertStoreKeyToCaption);
  RegisterAsyncControl(lblDomains, 'usp_DomainsForMask_Get',
                       ['@Mask', 'Domain_Mask'], '@Domains');
  RegisterAsyncControl(lblGatheringSite, 'usp_SpecimenBestLocationName_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Name');
  RegisterAsyncControl(lblGatheringDate, 'usp_SpecimenDate_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Date');

  //RegisterAsyncControl(lblStatus, 'usp_CollectionUnitStatus_Get',
  //                     ['@Key', 'Collection_Unit_Key'], '@Status');
  RegisterAsyncControl(lblAccNumber, 'usp_CollectionUnitAccessionNumber_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Number');
  RegisterAsyncControl(lblPrefNumber, 'usp_CollectionUnitNumber_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Number');
  RegisterControl(cmbType, 'Type', 'Specimen_Type_Concept_Key', True, ResStr_Type);
end;  // TfraSpecimenGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Allows the user to drag items into the registered drag drop components. 
}
procedure TfraSpecimenGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eCurrentLocation, DropCurrentLocation, [TN_STORE], [CF_JNCCDATA]);
  RegisterDropComponent(eUsualLocation, DropUsualLocation, [TN_STORE], [CF_JNCCDATA]);
end;  // TfraSpecimenGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Saves\Updates the tables using the frame data. 
}
procedure TfraSpecimenGeneral.SaveData;
var
  lParams: Array of Variant;
  lSpecimenExists: Boolean;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@ExistingCollectionUnitKey', Key,   // For Insert only
                         '@ParentCollectionCollectionUnitKey', eCollection.Key,
                         '@SpecimenTypeConceptKey', cmbType.CurrentStrID,
                         '@Confidential', chkConfidential.Checked,
                         '@Dangerous', chkDangerous.Checked,
                         '@PublishToWeb', chkPublishToWeb.Checked,
                         '@LifeSciences', FLifeSciences,
                         '@Checked', chkChecked.Checked,
                         '@CurrentContainerCollectionUnitKey', eCurrentLocation.Key,
                         '@CurrentLocationCode', eCurrentLocCode.Text,
                         '@UsualContainerCollectionUnitKey', eUsualLocation.Key,
                         '@UsualLocationCode', eUsualLocCode.Text,
                         '@SUTimestamp', FSUTimestamp,
                         '@CUTimestamp', FCUTimestamp,
                         '@InternalUse', chkInternalUse.Checked]);
  
  // We might have a Key because a Store has been inserted that is a Specimen,
  // and the user was prompted to come here. However, we need to check whether
  // the Specimen_Unit exists, and whether it needs to be inserted.
  lSpecimenExists := dmGeneral.GetStoredProcOutputParam('usp_SpecimenExists_Get',
                                                        ['@Key', Key], '@Exists') = True;
  
  // Empty key means new record.
  if (Key = '') or (not lSpecimenExists) then begin
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_SPECIMEN_UNIT, 'usp_Specimen_Insert',
                                                  lParams, '@Key'));
    AddOrDeleteStore;
    // Fire an event that will force the user to make a determination
    if Assigned(OnFrameNotification) then
      if FLifeSciences then
        OnFrameNotification(Self, etAddDeterminationLifeScience,
                            VarArrayOf(['Key', Key]))
      else
        OnFrameNotification(Self, etAddDeterminationEarthScience,
                            VarArrayOf(['Key', Key]));
  end else begin
    // Update a record.
    if not AddOrDeleteStore then begin
      // Make the SpecimenType go back to Store.
      cmbType.ItemIndex := cmbType.IDIndexOf('SYSTEM000000008U');
      Abort;
    end;
    // Run the update proc to update table information.
    dmGeneral.RunUpdateStoredProc('usp_Specimen_Update', lParams);
  end;
end;  // TfraSpecimenGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.UpdateCurrentLocation;
begin
  eCurrentLocation.Text := eUsualLocation.Text;
  eCurrentLocation.Key := eUsualLocation.Key;
  // The locations will always match at this point.
  FUsualCurrentLocationMatch := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.UpdateCurrentLocationCode;
begin
  eCurrentLocCode.Text := eUsualLocCode.Text;
  // The location codes will always match at this point.
  FUsualCurrentLocationCodeMatch := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eCurrentLocationKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F10) and (not eCurrentLocation.EditBox.ReadOnly) then
  begin
    UpdateCurrentLocation;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eCurrentLocCodeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F10) and (not eCurrentLocCode.ReadOnly) then
  begin
    UpdateCurrentLocationCode;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eUsualLocationChange(Sender: TObject);
begin
  inherited;
  { Only do this if it is empty AND new }
  if ((Key = '') and ((Length(eCurrentLocation.Text) = 0))
    or ((Length(eCurrentLocation.Text) = 0) and (Length(eUsualLocation.Text) = 0))
    or FUsualCurrentLocationMatch) then
  begin
    UpdateCurrentLocation;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eUsualLocationEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationMatch := eUsualLocation.Text = eCurrentLocation.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eUsualLocCodeEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationCodeMatch := eUsualLocCode.Text = eCurrentLocCode.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.eUsualLocCodeChange(Sender: TObject);
begin
  inherited;
  if ((Length(eCurrentLocCode.Text) = 0)
    or FUsualCurrentLocationCodeMatch) then
  begin
    UpdateCurrentLocationCode;
  end;
end;

procedure TfraSpecimenGeneral.lblStatusClick(Sender: TObject);
var
  lNavigationType: TEventType;
begin
  inherited;
  if (FMovementType > -1) then
    begin
      lNavigationType := etNavigateMovement;
      if (FMovementType = 0) or (FMovementType = 1) then
      begin
        lNavigationType := etNavigateAccession;
      end
      else if (FMovementType = 2) or (FMovementType = 3) then
      begin
        lNavigationType := etNavigateLoan;
      end;
      //MessageDlg(VarToStr(FMovementType), mtInformation, [mbOK], 0);
      OnFrameNotification(Owner, lNavigationType,
        VarArrayOf(['Key', FMaterialCollectionUnitKey]));
    end;
end;

procedure TfraSpecimenGeneral.lblConditionCheckClick(Sender: TObject);
begin
  inherited;
  if (not (FConditionCheckKey = '')) and
    (not (FConditionCheckKey = Null)) then
    OnFrameNotification(Owner, etNavigateConditionCheck,
      VarArrayOf(['Key', FConditionCheckKey]));
end;

procedure TfraSpecimenGeneral.lblLastJobClick(Sender: TObject);
begin
  inherited;
  if (not (FLastJobKey = '')) and
    (not (FLastJobKey = Null)) then
    OnFrameNotification(Owner, etNavigateJob,
      VarArrayOf(['Key', FLastJobKey]));
end;

{-------------------------------------------------------------------------------
  Must execute this on Mouse Up otherwise the mouse up event occurs after the
  data tab has changed and causes errors.
}
procedure TfraSpecimenGeneral.lblDomainsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  OnFrameNotification(Owner, etSelectFolder,
      VarArrayOf(['Key', 'Determinations']));
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.lblGatheringDateMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  OnFrameNotification(Owner, etSelectFolder,
      VarArrayOf(['Key', 'Field Data']));
end;

{-------------------------------------------------------------------------------
}
procedure TfraSpecimenGeneral.lblGatheringSiteMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  OnFrameNotification(Owner, etSelectFolder,
      VarArrayOf(['Key', 'Field Data']));
end;

end.
