{===============================================================================
  Unit:        FrameStorageGeneral.pas

  Defines:     TfraStorageGeneral

  Description:

  Model:       CollectionsCollectionsAndStorage.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 32 $
    $Date: 18/12/12 16:04 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameStorageGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  DataTypes, DropTarget, InterfaceDataModule, BaseCompositeComponent,
  LinkedControls, ComboListID, LuxIDComboBox, ConceptGroupComboBox, Dataclasses,
  GeneralData, ApplicationSettings;

type
  {-----------------------------------------------------------------------------
    General tab page control allowing the user to view and edit details of a selected store.
  }
  TfraStorageGeneral = class(TBaseTabSheetFrame)
    chkSpecimen: TCheckBox;
    cmbType: TConceptGroupComboBox;
    eCurrentLocation: TLinkedEdit;
    eCurrentLocCode: TEdit;
    eName: TEdit;
    eUsualLocCode: TEdit;
    Label1: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lblNumber: TLabel;
    lblStatus: TLabel;
    mmComments: TMemo;
    mmDetails: TMemo;
    Label8: TLabel;
    eUsualLocation: TLinkedEdit;
    lblConditionCheck: TLabel;
    lblMovementSpacer: TLabel;
    lblJobSpacer: TLabel;
    lblLastJob: TLabel;
    procedure chkSpecimenClick(Sender: TObject);
    procedure eUsualLocCodeChange(Sender: TObject);
    procedure eUsualLocCodeEnter(Sender: TObject);
    procedure eCurrentLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eUsualLocationChange(Sender: TObject);
    procedure eUsualLocationEnter(Sender: TObject);
    procedure eCurrentLocCodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblStatusClick(Sender: TObject);
    procedure lblConditionCheckClick(Sender: TObject);
    procedure lblLastJobClick(Sender: TObject);
  private
    FSpecimenUnchecked: Boolean;
    FStoreTimestamp: TSQLSvrTimestamp;
    FUnitTimestamp: TSQLSvrTimestamp;  
    FUsualCurrentLocationMatch: Boolean;
    FUsualCurrentLocationCodeMatch: Boolean;
    FMaterialCollectionUnitKey: String;
    FMovementType: Integer;
    FConditionCheckKey: String;
    FLastJobKey: String;
    procedure DropeCurrentLocation(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropeUsualLocation(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure SaveDeleteSpecimen;
    procedure UpdateCurrentLocation;
    procedure UpdateCurrentLocationCode;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
    function GetStatusParams: TVariantArray;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  LuxembourgConstants, Validation, BaseDetailFrameUnit, ResourceStrings,
  FrameContainerUnit, ExceptionForm, BaseADODataModule;

{-==============================================================================
    TfraStorageGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  If chkSpecimen was checked and it has been clicked (i.e. it is now unchecked) and we have a
      Key for the Store, then set the FSpecimenUnchecked boolean value to true. Otherwise set
      it to false.
}
procedure TfraStorageGeneral.chkSpecimenClick(Sender: TObject);
begin
  inherited;
  FSpecimenUnchecked := (Key <> '') and (not chkSpecimen.Checked);
end;  // TfraStorageGeneral.chkSpecimenClick 

{-------------------------------------------------------------------------------
  Delete the record from the database that was used to load the frame. 
}
procedure TfraStorageGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Store_Delete',
                               ['@StoreKey', Key, '@Timestamp', FStoreTimestamp]);
end;  // TfraStorageGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle dropping of a store into the current location edit control. 
}
procedure TfraStorageGeneral.DropeCurrentLocation(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eCurrentLocation, 'Store',
                 'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraStorageGeneral.DropeCurrentLocation 

{-------------------------------------------------------------------------------
  Handle dropping of a store into the usual location edit control. 
}
procedure TfraStorageGeneral.DropeUsualLocation(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eUsualLocation, 'Store',
                 'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraStorageGeneral.DropeUsualLocation 

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  mmDetails.ReadOnly := true;
end;  // TfraStorageGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Retrieve the treenode caption when this item is updated.  Set to <Store.Item_Name> -
      <Collection_Unit_Number.Number for the preferred number where
      Collection_Unit_Number.Type_Concept_Key indicates a Registration Number>
}
function TfraStorageGeneral.GetCaption: String;
begin
  Result := eName.Text;
  if eCurrentLocCode.Text <> '' then
    Result := Result + ' - ' + eCurrentLocCode.Text
  else if eUsualLocCode.Text <> '' then
    Result := Result + ' - ' + eUsualLocCode.Text;
end;  // TfraStorageGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Record the timestamps for use when updating 
}
procedure TfraStorageGeneral.LoadData;
var
  conditionCheckCaption: String;
  jobCaption: String;
  movementCaption: String;
  statusHorizontalSpacing: Integer;
begin
  inherited;
  FSpecimenUnchecked := False;
  if not RegisteredRecordsets[0].Eof then begin
    FStoreTimeStamp := RegisteredRecordsets[0].Fields['StoreTimestamp'].Value;
    FUnitTimeStamp := RegisteredRecordsets[0].Fields['UnitTimestamp'].Value;
  end;
  
  if (Key = '') then
    with AdditionalProperties do
      if (GetProperty(PROP_TOP_NODE_CONTEXT) = ncStoreHierarchy) then begin
        eCurrentLocation.Key := GetProperty(PROP_PARENT_KEY);
        eCurrentLocation.Text := dmGeneral.GetStoredProcOutputParam
                                          ('usp_StoreName_Get',
                                          ['@StoreKey', eCurrentLocation.Key],
                                          '@StoreName');
        eUsualLocation.Key := eCurrentLocation.Key;
        eUsualLocation.Text := eCurrentLocation.Text;
      end;

    // Populate the status label data.
    with RegisteredRecordsets[1] do begin
      if not Eof then begin
        MoveFirst;

        // Set up spacing
        statusHorizontalSpacing := 14;
        //statusVerticalSpacing := 14;

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

        movementCaption := VarToStr(Fields['MovementStatus'].Value);
        FMaterialCollectionUnitKey := VarToStr(Fields['MovementKey'].Value);
        if (Fields['MovementType'].Value = Null) then
        begin
          FMovementType := -1;
        end
        else
        begin
          FMovementType := Fields['MovementType'].Value;
        end;

        if ((FMaterialCollectionUnitKey = Null) or (FMaterialCollectionUnitKey = ''))
        then
        begin
          movementCaption := 'unknown';
          FMovementType := -1;
        end;

        lblStatus.Caption := movementCaption;

        lblMovementSpacer.Left := lblConditionCheck.Left + lblConditionCheck.Width + 4;
        // Figure out where the Movement label should go.
        lblStatus.Left := lblConditionCheck.Left + lblConditionCheck.Width + statusHorizontalSpacing;

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
          lblLastJob.Left := lblStatus.Left + lblStatus.Width + statusHorizontalSpacing;
          
        end
        else
        begin
          lblLastJob.Visible := False;
          lblJobSpacer.Visible := False;
          FLastJobKey := '';
        end;
      end;
    end;

    eCurrentLocation.ShowHint := True;
    eCurrentLocation.Hint := 'Press F10 to default to Usual Location data';
    eCurrentLocCode.ShowHint := True;
    eCurrentLocCode.Hint := 'Press F10 to default to Usual Location data';
end;  // TfraStorageGeneral.LoadData

{-------------------------------------------------------------------------------
  Gets parameters for the Status Label stored procedure.
}
function TfraStorageGeneral.GetStatusParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key]);
end;  // TfraSpecimenGeneral.GetStatusParams

{-------------------------------------------------------------------------------
  Register recordsets and controls used to load the data. 
}
procedure TfraStorageGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Store_Select');

  // Procedure for getting all the keys / values for status labels
  RegisterRecordset('usp_CollectionUnitStatus_Select', GetStatusParams);

  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'Item_Name', True, ResStr_Name);
  RegisterControl(cmbType, 'TypeTerm', 'Store_Type_Concept_Key', True, ResStr_Type);
  RegisterConceptGroupComboBox(cmbType, CG_STORE_TYPE);
  RegisterControl(chkSpecimen, 'IsSpecimen');
  RegisterControl(eCurrentLocation, 'Current_Container',
      'Current_Container_Collection_Unit_Key', CheckLinkedStore, 'Store',
      ConvertStoreKeyToCaption);
  RegisterControl(eCurrentLocCode, 'Current_Location_Code');
  RegisterControl(eUsualLocation, 'Usual_Container', 'Usual_Container_Collection_Unit_Key',
      True, ResStr_UsualLocation, CheckLinkedStore, 'Store', ConvertStoreKeyToCaption);
  RegisterControl(eUsualLocCode, 'Usual_Location_Code');
  RegisterControl(mmComments, 'Comment');
  
  // Register asynchronously loaded controls
  RegisterAsyncControl(lblNumber, 'usp_CollectionUnitNumber_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Number');
  //RegisterAsyncControl(lblStatus, 'usp_CollectionUnitStatus_Get',
  //                     ['@Key', 'Collection_Unit_Key'], '@Status');
  RegisterAsyncControl(mmDetails, 'usp_StoreDetailsMemo_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Output');
end;  // TfraStorageGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register controls for drag and drop operations. 
}
procedure TfraStorageGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eCurrentLocation, DropeCurrentLocation, ['STORE'], [CF_JNCCDATA]);
  
  RegisterDropComponent(eUsualLocation, DropeUsualLocation, ['STORE'], [CF_JNCCDATA]);
end;  // TfraStorageGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Insert or edit an item in the database. 
}
procedure TfraStorageGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eName.Text,
                          '@StoreTypeConceptKey', cmbType.CurrentStrID,
                          '@Comment', mmComments.Text,
                          '@CurrentContainerKey', eCurrentLocation.Key,
                          '@UsualContainerKey', eUsualLocation.Key,
                          '@CurrentLocationCode', eCurrentLocCode.Text,
                          '@UsualLocationCode', eUsualLocCode.Text,
                          '@Timestamp', FStoreTimestamp
                          ]);
  
  // Empty key means new record.
  if Key = '' then
     Key := VarToStr(dmGeneral.RunInsertStoredProc('Store',
                                                    'usp_Store_Insert',
                                                    lParams,
                                                    '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Store_Update', lParams);
  
  // Depending on the state of chkSpecimen a Specimen record may need to
  // be added and deleted, so call this method.
  SaveDeleteSpecimen;
end;  // TfraStorageGeneral.SaveData 

{-------------------------------------------------------------------------------
  Depending on the state of chkSpecimen a Specimen record may need to be added and deleted, so
      call this method.
}
procedure TfraStorageGeneral.SaveDeleteSpecimen;
var
  lStoreIsSpecimen: Boolean;
begin
  // See if the current Store is also a Specimen
  lStoreIsSpecimen := not (dmGeneral.GetStoredProcOutputParam
                                      ('usp_Specimen_Exists_ForCollectionUnit',
                                      ['@Key', Key],
                                      '@SpecimenExists') = 0);
  
  // If chkSpecimen has been unchecked in the current edit, see if the
  // Store is a Specimen and if so, ask the user if they are sure they
  // want to delete the Specimen's details.
  if FSpecimenUnchecked then begin
    if lStoreIsSpecimen then
      if MessageDlg(ResStr_ConfirmDeleteSpecimenForStore, mtInformation,
                                               [mbOK, mbCancel], 0) = mrOK then
        dmGeneral.RunDeleteStoredProc('usp_Specimen_Delete', ['@SpecimenKey', Key])
      else
        Abort
  // If chkSpecimen is checked and the Store is not currently a Specimen,
  // then prompt the user to enter details for the Specimen.
  end else if chkSpecimen.Checked then
    if not lStoreIsSpecimen then
      if Assigned(OnFrameNotification) then
          OnFrameNotification(Self, etAddSpecimen, VarArrayOf(['Key', Key]));
end;  // TfraStorageGeneral.SaveDeleteSpecimen 

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.ValidateData;
var
  lRecursionPassed: Boolean;
begin
  inherited;
  //A new store cannot be validated for recursion problems
  if Key = EmptyStr then Exit;
  
  //Ensure that we are not linking to ourself
  ValidateValue(eCurrentLocation.Key <> Key,
      Format(ResStr_RecursionFailureCannotBeLinkedToItself, [ResStr_Store]),
          eCurrentLocation);
  
  //Test for recursion on the stores
  if eCurrentLocation.Key <> '' then begin
    lRecursionPassed := (dmGeneral.GetStoredProcOutputParam(
          'usp_Store_Contains_Get', ['@ContainerStoreKey', Key,
                '@ContainedStoreKey', eCurrentLocation.Key,
                      '@IsCurrentLocation', true], '@Contains') = 0);
  
    ValidateValue(lRecursionPassed,
        Format(ResStr_RecursionFailureStoreContains, [eCurrentLocation.Text]),
            eCurrentLocation);
  end;
end;  // TfraStorageGeneral.ValidateData 


{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.eUsualLocCodeChange(Sender: TObject);
begin
  inherited;
  if (((Length(eCurrentLocCode.Text) = 0) and (Length(eUsualLocCode.Text) = 0))
    or FUsualCurrentLocationCodeMatch) then
  begin
    UpdateCurrentLocationCode;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.eUsualLocCodeEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationCodeMatch := eUsualLocCode.Text = eCurrentLocCode.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.eCurrentLocationKeyDown(Sender: TObject;
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
procedure TfraStorageGeneral.eUsualLocationChange(Sender: TObject);
begin
  inherited;
  if (((Length(eCurrentLocation.Text) = 0) and (Length(eUsualLocation.Text) = 0))
    or FUsualCurrentLocationMatch) then
  begin
    UpdateCurrentLocation;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.eUsualLocationEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationMatch := eUsualLocation.Text = eCurrentLocation.Text
end;

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.eCurrentLocCodeKeyDown(Sender: TObject;
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
procedure TfraStorageGeneral.UpdateCurrentLocation;
begin
  eCurrentLocation.Text := eUsualLocation.Text;
  eCurrentLocation.Key := eUsualLocation.Key;
  FUsualCurrentLocationMatch := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfraStorageGeneral.UpdateCurrentLocationCode;
begin
  eCurrentLocCode.Text := eUsualLocCode.Text;
  FUsualCurrentLocationCodeMatch := True;
end;

procedure TfraStorageGeneral.lblStatusClick(Sender: TObject);
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
      OnFrameNotification(Owner, lNavigationType,
        VarArrayOf(['Key', FMaterialCollectionUnitKey]));
    end;
end;

procedure TfraStorageGeneral.lblConditionCheckClick(Sender: TObject);
begin
  inherited;
  if (not (FConditionCheckKey = '')) and
    (not (FConditionCheckKey = Null)) then
    OnFrameNotification(Owner, etNavigateConditionCheck,
      VarArrayOf(['Key', FConditionCheckKey]));
end;

procedure TfraStorageGeneral.lblLastJobClick(Sender: TObject);
begin
  inherited;
  if (not (FLastJobKey = '')) and
    (not (FLastJobKey = Null)) then
    OnFrameNotification(Owner, etNavigateJob,
      VarArrayOf(['Key', FLastJobKey]));
end;

end.



