{===============================================================================
  Unit:        FrameCollectionDetails

  Defines:     TfraCollectionDetails

  Description: General details about collection items.

  Model:       CollectionsCollectionsAndStorage.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 31 $
    $Date: 25/10/12 11:28 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameCollectionGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  ComboListID, ADODB, ADOInt, BaseCompositeComponent, InterfaceDataModule,
  LinkedControls, DataTypes, ConceptGroupComboBox, DataClasses, DropStruct,
  DropTarget, ExceptionForm, LuxIDComboBox, UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page component for viewing and editing the basic details of a collection.
  }
  TfraCollectionGeneral = class (TBaseTabSheetFrame)
    cmbRisk: TConceptGroupComboBox;
    eAssembledBy: TUserEdit;
    eCurrentLocation: TLinkedEdit;
    eCurrentLocCode: TEdit;
    eName: TEdit;
    eTopic: TEdit;
    eUsualLocation: TLinkedEdit;
    eUsualLocCode: TEdit;
    Label1: TLabel;
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
    lblName: TLabel;
    lblNumber: TLabel;
    lblOwner: TLabel;
    lblStatus: TLabel;
    mmDomains: TMemo;
    lblConditionCheck: TLabel;
    lblMovementSpacer: TLabel;
    lblJobSpacer: TLabel;
    lblLastJob: TLabel;
    procedure eCurrentLocCodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eUsualLocationChange(Sender: TObject);
    procedure eUsualLocationEnter(Sender: TObject);
    procedure eUsualLocCodeChange(Sender: TObject);
    procedure eUsualLocCodeEnter(Sender: TObject);
    procedure eCurrentLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblStatusClick(Sender: TObject);
    procedure lblConditionCheckClick(Sender: TObject);
    procedure lblLastJobClick(Sender: TObject);
  private
    FCollectionTimestamp: TSQLSvrTimestamp;
    FCollectionUnitTimestamp: TSQLSvrTimestamp;
    FParentCollectionKey: String;    
    FUsualCurrentLocationMatch: Boolean;
    FUsualCurrentLocationCodeMatch: Boolean;
    FMaterialCollectionUnitKey: String;
    FMovementType: Integer;
    FConditionCheckKey: String;
    FLastJobKey: String;
    procedure DropAssembledBy(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropCurrentLocation(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropUsualLocation(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function GetAccNumberFromDatabase: String;
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
    function GetStatusParams: TVariantArray;
  public
    property ParentCollectionKey: String read FParentCollectionKey write FParentCollectionKey;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, GeneralData, ResourceStrings, LuxembourgConstants, Validation,
  BaseADODataModule;

{-==============================================================================
    TfraCollectionGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Delete the record from the database that was used to load the frame. 
}
procedure TfraCollectionGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Collection_Delete',
                                ['@Key', Key, '@Timestamp', FCollectionUnitTimestamp]);
end;  // TfraCollectionGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.DropAssembledBy(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eAssembledBy,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
end;  // TfraCollectionGeneral.DropAssembledBy

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.DropCurrentLocation(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eCurrentLocation, TN_STORE,
                     'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraCollectionGeneral.DropCurrentLocation

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.DropUsualLocation(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eUsualLocation, TN_STORE,
                     'usp_StoreName_Get', '@StoreKey', '@StoreName');
end;  // TfraCollectionGeneral.DropUsualLocation 

{-------------------------------------------------------------------------------
  mmDomains needs to stay ReadOnly when going into edit mode. Because it is a registered 
      control, by default it would become edittable. 
}
procedure TfraCollectionGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  mmDomains.ReadOnly := True;
end;  // TfraCollectionGeneral.EnableControls 

{-------------------------------------------------------------------------------
  This method will get the accession number for a new Collection from the database. This is required when adding a Collection, because to get the Accession number, UpdateNodeRelationship needs to have been run and created its join records. We can't wait until the frame is reloaded to let the Accession number load asynchronously because the node will need its caption long before that. 
}
function TfraCollectionGeneral.GetAccNumberFromDatabase: String;
begin
  if (lblAccNumber.Caption = '') or
              (CompareText(lblAccNumber.Caption, 'Unknown') = 0) then
    Result := dmGeneral.GetStoredProcOutputParam('usp_CollectionUnitAccessionNumber_Get',
                                                 ['@Key', Key], '@Number')
  else
    Result := lblAccNumber.Caption;
end;  // TfraCollectionGeneral.GetAccNumberFromDatabase 

{-------------------------------------------------------------------------------
  Retrieve the caption. 
}
function TfraCollectionGeneral.GetCaption: String;
begin
  lblAccNumber.Caption := GetAccNumberFromDatabase;
  if (lblAccNumber.Caption = '') or (CompareText(lblAccNumber.Caption, 'Unknown') = 0) then
    Result := eName.Text
  else
    Result := eName.Text + ' - ' + lblAccNumber.Caption;
end;  // TfraCollectionGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Loads the data. 
}
procedure TfraCollectionGeneral.LoadData;
var
  conditionCheckCaption: String;
  jobCaption: String;
  movementCaption: String;
  statusHorizontalSpacing: Integer;
begin
  inherited LoadData;
  
  with RegisteredRecordsets[0] do
    if not Eof then begin
      FParentCollectionKey := VarToStr(Fields['Parent_Collection_Collection_Unit_Key'].Value);
      FCollectionTimestamp := Fields['Collection_Timestamp'].Value;
      FCollectionUnitTimestamp := Fields['Collection_Unit_Timestamp'].Value;
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
end;  // TfraCollectionGeneral.LoadData 

{-------------------------------------------------------------------------------
  Gets parameters for the Status Label stored procedure.
}
function TfraCollectionGeneral.GetStatusParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key]);
end;  // TfraSpecimenGeneral.GetStatusParams

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraCollectionGeneral.RegisterControls;
begin
  inherited;
  
  // Register recordsets used
  RegisterRecordset('usp_Collection_Select');

  // Procedure for getting all the keys / values for status labels
  RegisterRecordset('usp_CollectionUnitStatus_Select', GetStatusParams);

  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'Item_Name', True, ResStr_Name);
  RegisterControl(eAssembledBy,
                  'Assembler_Name', 'Assembler_Name_Key', True, ResStr_AssembledBy,
                  CheckLinkedName, 'Name', ConvertNameKeyToCaption);
  RegisterControl(eTopic, 'Topic', True, ResStr_Topic);
  RegisterControl(eCurrentLocation,
                  'Current_Location_Name', 'Current_Container_Collection_Unit_Key',
                  CheckLinkedStore, 'Store', ConvertStoreKeyToCaption);
  RegisterControl(eCurrentLocCode, 'Current_Location_Code');
  RegisterControl(eUsualLocation,
                  'Usual_Location_Name', 'Usual_Container_Collection_Unit_Key',
                  False, ResStr_UsualLocation, CheckLinkedStore, 'Store',
                  ConvertStoreKeyToCaption);
  RegisterControl(eUsualLocCode, 'Usual_Location_Code');
  // Register cmbRisk to get its first item, and then as a ConceptGroupCombo for the rest.
  RegisterControl(cmbRisk, 'Risk_Name', 'Risk_Concept_Key');
  RegisterConceptGroupComboBox(cmbRisk, CG_COLLECTION_RISKS);
  
  // Register asynchronously loaded controls
  RegisterAsyncControl(lblOwner, 'usp_FormattedNameForNameKey_Get',
                       ['@NameKey', 'Owner_Name_Key'], '@FormattedName');
  RegisterAsyncControl(lblAccNumber, 'usp_CollectionUnitAccessionNumber_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Number');
  RegisterAsyncControl(lblDept, 'usp_CollectionUnitDepartment_Get',
                       ['@Key', 'Collection_Unit_Key'], '@Department');
  RegisterAsyncControl(mmDomains, 'usp_DomainsForMask_Get',
                       ['@Mask', 'Domain_Mask'], '@Domains');
  //RegisterAsyncControl(lblStatus, 'usp_CollectionUnitStatus_Get',
  //                     ['@Key', 'Collection_Unit_Key'], '@Status');
end;  // TfraCollectionGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TfraCollectionGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eAssembledBy, DropAssembledBy,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA]);
  RegisterDropComponent(eCurrentLocation, DropCurrentLocation, [TN_STORE], [CF_JNCCDATA]);
  RegisterDropComponent(eUsualLocation, DropUsualLocation, [TN_STORE], [CF_JNCCDATA]);
end;  // TfraCollectionGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraCollectionGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@ParentCollectionKey', FParentCollectionKey,
                         '@ItemName', eName.Text,
                         '@AssemblerNameKey', eAssembledBy.Key,
                         '@Topic', eTopic.Text,
                         '@RiskConceptKey', cmbRisk.CurrentStrID,
                         '@CollectionTimestamp', FCollectionTimestamp,
                         '@CurrentContainerKey', eCurrentLocation.Key,
                         '@CurrentLocationCode', eCurrentLocCode.Text,
                         '@UsualContainerKey', eUsualLocation.Key,
                         '@UsualLocationCode', eUsualLocCode.Text,
                         '@CollectionUnitTimestamp', FCollectionUnitTimestamp]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT,
                                                  'usp_Collection_Insert',
                                                  lParams,
                                                  '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Collection_Update', lParams);
end;  // TfraCollectionGeneral.SaveData



{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.UpdateCurrentLocation;
begin
  eCurrentLocation.Text := eUsualLocation.Text;
  eCurrentLocation.Key := eUsualLocation.Key;
  FUsualCurrentLocationMatch := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.UpdateCurrentLocationCode;
begin
  eCurrentLocCode.Text := eUsualLocCode.Text;
  FUsualCurrentLocationCodeMatch := True
end;

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.eCurrentLocCodeKeyDown(Sender: TObject;
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
procedure TfraCollectionGeneral.eUsualLocationChange(Sender: TObject);
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
procedure TfraCollectionGeneral.eUsualLocationEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationMatch := eUsualLocation.Text = eCurrentLocation.Text;
end;

procedure TfraCollectionGeneral.eUsualLocCodeChange(Sender: TObject);
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
procedure TfraCollectionGeneral.eUsualLocCodeEnter(Sender: TObject);
begin
  inherited;
  FUsualCurrentLocationCodeMatch := eUsualLocCode.Text = eCurrentLocCode.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfraCollectionGeneral.eCurrentLocationKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F10) and (not eCurrentLocation.EditBox.ReadOnly) then
  begin
    UpdateCurrentLocation;
  end;
end;

procedure TfraCollectionGeneral.lblStatusClick(Sender: TObject);
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

procedure TfraCollectionGeneral.lblConditionCheckClick(Sender: TObject);
begin
  inherited;
  if (not (FConditionCheckKey = '')) and
    (not (FConditionCheckKey = Null)) then
    OnFrameNotification(Owner, etNavigateConditionCheck,
      VarArrayOf(['Key', FConditionCheckKey]));
end;

procedure TfraCollectionGeneral.lblLastJobClick(Sender: TObject);
begin
  inherited;
  if (not (FLastJobKey = '')) and
    (not (FLastJobKey = Null)) then
    OnFrameNotification(Owner, etNavigateJob,
      VarArrayOf(['Key', FLastJobKey]));
end;

end.

