{===============================================================================
  Unit:        FrameDeterminationGeneral.pas

  Defines:     TfraDeterminationGeneral

  Description:

  Model:       CollectionsSpecimens.mpb

  Created:     May 2003

===============================================================================}
unit FrameDeterminationGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  BaseCompositeComponent, LinkedControls, InterfaceDataModule,
  ConceptGroupComboBox, ComboListID, LuxIDComboBox, VagueDateEdit, DataTypes,
  GeneralData, Validation, DataClasses, Recorder2000_TLB, ExceptionForm, DSSDataTypes,
  UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page control for a determination's details.  This screen operates in two
    modes, either using the Determination table or the Taxon_Determination table.
    Determinations created for a specimen where Specimen_Unit.Life_Sciences=0 use the
    Determination table, Determinations created for a specimen where Specimen_Unit.
    Life_Sciences=1 use the Taxon_Determination table.
  }
  TfraDeterminationGeneral = class(TBaseTabSheetFrame)
    btnDeterminerInferred: TImageListButton;
    chkPreferred: TCheckBox;
    cmbConfidence: TLuxIDComboBox;
    cmbDetMadeAgainst: TLuxIDComboBox;
    cmbRole: TLuxIDComboBox;
    cmbStatus: TConceptGroupComboBox;
    cmbType: TLuxIDComboBox;
    eDate: TVagueDateEdit;
    eDetermination: TLinkedEdit;
    eDeterminer: TUserEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblDeterminer: TLabel;
    lblDomain: TLabel;
    mmMethod: TMemo;
    mmNotes: TMemo;
    chkIncludeInLabel: TCheckBox;
    procedure cmbConfidencePopulate(Sender: TObject);
    procedure cmbDetMadeAgainstPopulate(Sender: TObject);
    procedure cmbRolePopulate(Sender: TObject);
    procedure cmbTypePopulate(Sender: TObject);
    procedure eDeterminationFindData(Sender: TObject);
    procedure eDeterminationGetData(Sender: TObject);
    procedure eDeterminerGetData(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure InferenceClick(Sender: TObject);
  private
    FCollectionUnitKey: TKeyString;
    FLifeScience: Boolean;
    FOccurrenceKey: TKeyString;
    FPreviousTerm: String;
    FTimestamp: TSQLSvrTimestamp;
    procedure ClearInferredButtons;
    function ConceptHasMappedTaxon(const AConceptKey: String): Boolean;
    function ConceptHasOccurrences(const AKey: TKeyString): Boolean;
    procedure DragOverDeterminationCheck(APoint: TPoint; const ATable, AFieldKey: String;
        var Accept: Boolean);
    procedure DropDetermination(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; const AIsPasteOperation:
        Boolean; var AHandled: Boolean);
    procedure DropDeterminer(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function InferredCaptionToValue(ACaption: String): ShortInt;
    procedure InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
    procedure UpdateDetermination(const AKeyList: IKeyList);
    procedure UpdateDeterminer(const AKeyList: IKeyList);
    procedure UpdateStoreItemName;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    function GetParams: TVariantArray;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  LuxembourgConstants, ResourceStrings, ADOInt, DropTarget, VagueDate,
  ThesaurusBrowser_TLB, BaseADODataModule, GeneralFunctions,
  LuxembourgFunctions;

{-==============================================================================
    TfraDeterminationGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.ClearInferredButtons;
begin
  InferredValueToButtonImage(btnDeterminerInferred, 0);
end;  // TfraDeterminationGeneral.ClearInferredButtons 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.cmbConfidencePopulate(Sender: TObject);
begin
  with cmbConfidence do begin
    Add(ResStr_UnspecifiedConfidence, 0);
    Add(ResStr_NoConfidence, 1);
    Add(ResStr_LowConfidence, 2);
    Add(ResStr_MediumConfidence, 3);
    Add(ResStr_HighConfidence, 4);
  end;
end;  // TfraDeterminationGeneral.cmbConfidencePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.cmbDetMadeAgainstPopulate(Sender: TObject);
begin
  cmbDetMadeAgainst.Add(ResStr_FieldObservation, 0);
  cmbDetMadeAgainst.Add(ResStr_Specimen, 1);
end;  // TfraDeterminationGeneral.cmbDetMadeAgainstPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.cmbRolePopulate(Sender: TObject);
begin
  with dmGeneral.GetRecordset('usp_DeterminerRoles_Select', []) do begin
    while not Eof do begin
      cmbRole.Add(VarToStr(Fields['Item_Name'].Value),
                  VarToStr(Fields['Item_Key'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraDeterminationGeneral.cmbRolePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.cmbTypePopulate(Sender: TObject);
begin
  with dmGeneral.GetRecordset('usp_DeterminationTypes_Select', []) do begin
    while not Eof do begin
      cmbType.Add(VarToStr(Fields['Item_Name'].Value),
                  VarToStr(Fields['Item_Key'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraDeterminationGeneral.cmbTypePopulate

{-------------------------------------------------------------------------------
}
function TfraDeterminationGeneral.ConceptHasMappedTaxon(const AConceptKey: String): Boolean;
begin
  Result := not VarIsNull(dmGeneral.GetStoredProcOutputParam('usp_Concept_GetTaxonListItem',
                                                             ['@ConceptKey', AConceptKey],
                                                             '@TaxonListItemKey'));
  if Result then
    MessageDlg(ResStr_TaxonConceptMappingExists, mtInformation, [mbOk], 0);
end;  // TfraDeterminationGeneral.ConceptHasMappedTaxon 

{-------------------------------------------------------------------------------
}
function TfraDeterminationGeneral.ConceptHasOccurrences(const AKey: TKeyString): Boolean;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_ConceptHasOccurrences_Get',
                ['@ConceptKey', AKey], '@HasOccurrences') <> 0;
end;  // TfraDeterminationGeneral.ConceptHasOccurrences 

{-------------------------------------------------------------------------------
  Handles the deletion of Determination. Before deleting, it checks to see whether the current determination is the preferred determination. If it is, then they won't be allowed to delete it.
}
procedure TfraDeterminationGeneral.DeleteData;
var
  lIsPreferred: Boolean;
begin
  // The user should not be able to delete the preferred determination for
  // a collection unit.
  if MasterFrameType = mftCollectionUnit then begin
    lIsPreferred := dmGeneral.GetStoredProcOutputParam
                    ('usp_Determination_IsPreferred_Get',
                    ['@DeterminationKey', Key,
                    '@SpecimenUnitKey', ParentKey,
                    '@IsLifeSciences', FLifeScience,
                    '@IsSpecimenUnit', Ord(MasterFrameType = mftCollectionUnit)],
                    '@IsPreferred') = True;
    ValidateValue(not lIsPreferred, ResStr_CannotDeletePreferredDetermination);
  end;
  
  if FLifeScience then
    dmGeneral.RunDeleteStoredProc('usp_TaxonDetermination_Delete',
                                  ['@DeterminationKey', Key,
                                   '@IsForSpecimen', Ord(MasterFrameType = mftCollectionUnit),
                                   '@Timestamp', FTimestamp])
  else
    dmGeneral.RunDeleteStoredProc('usp_Determination_Delete',
                                  ['@DeterminationKey', Key,
                                   '@IsForSpecimen', Ord(MasterFrameType = mftCollectionUnit),
                                   '@Timestamp', FTimestamp]);
end;  // TfraDeterminationGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.DragOverDeterminationCheck(APoint: TPoint; const ATable,
    AFieldKey: String; var Accept: Boolean);
begin
  if FLifeScience then Accept := CompareText(ATable, TN_TAXON_LIST_ITEM) = 0
                  else Accept := CompareText(ATable, TN_CONCEPT) = 0;
end;  // TfraDeterminationGeneral.DragOverDeterminationCheck 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.DropDetermination(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; const
    AIsPasteOperation: Boolean; var AHandled: Boolean);
var
  lKey: TKeyString;
begin
  if EditMode = emBrowse then
    AHandled := True
  else
  if AFormat = CF_JNCCDATA then begin
    AHandled := True;
    if ASourceData.Header.ItemCount > 0 then begin
      lKey := ASourceData.Items[0].KeyField1;
      if FLifeScience then begin
        if (CompareText(ASourceData.Header.TableName, TN_TAXON_LIST_ITEM) = 0) then
        begin //all Life Science Terms can be added
          eDetermination.Text :=
              ConvertDeterminationLifeScienceKeyToCaption(lKey, TN_TAXON_LIST_ITEM);
          eDetermination.Key := lKey;
        end;
      end else // only Earth Science Terms with Occurrences can be added
      if (CompareText(ASourceData.Header.TableName, TN_CONCEPT) = 0) then
        if ConceptHasOccurrences(lKey) then begin
          if not ConceptHasMappedTaxon(lKey) then begin
            eDetermination.Text :=
                ConvertDeterminationEarthScienceKeyToCaption(lKey, TN_CONCEPT);
            eDetermination.Key := lKey;
          end
        end else
          MessageDlg(ResStr_DictionaryTermsWithOccurrences, mtInformation, [mbOK], 0);
    end;
  end;
end;  // TfraDeterminationGeneral.DropDetermination 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.DropDeterminer(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled:
    Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eDeterminer, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraDeterminationGeneral.DropDeterminer 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.eDeterminationFindData(Sender: TObject);
begin
  inherited;
  // Call the Convertxxx functions to get a nicely formatted name.
  if FLifeScience then begin
    if CheckLinkedDeterminationLifeScience(eDetermination) then
      eDetermination.Text := ConvertDeterminationLifeScienceKeyToCaption(eDetermination.Key,
                                                                         TN_TAXON_LIST_ITEM);
  end else
    if CheckLinkedDeterminationEarthScience(eDetermination) then
      if not ConceptHasMappedTaxon(eDetermination.Key) then
      begin
        eDetermination.Text :=
            ConvertDeterminationEarthScienceKeyToCaption(eDetermination.Key, TN_CONCEPT);
        if ConceptHasDomain(eDetermination.Key, DOM_STRATIGRAPHY) then begin
          chkIncludeInLabel.Checked := True;
        end;
      end;
end;  // TfraDeterminationGeneral.eDeterminationFindData

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.eDeterminationGetData(Sender: TObject);
begin
  inherited;
  if FLifeScience then
    InitReturnData(UpdateDetermination, TN_TAXON)
  else
    InitReturnData(UpdateDetermination, GUIDToString(CLASS_frmThesaurusBrowser));
end;  // TfraDeterminationGeneral.eDeterminationGetData 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.eDeterminerGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateDeterminer, TN_NAME);
end;  // TfraDeterminationGeneral.eDeterminerGetData 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  // If it is a preferred determination, can't untick it - need to select a
  // different preferred determination.
  if AEnabled then
    chkPreferred.Enabled := not chkPreferred.Checked
  else
    chkPreferred.Enabled := False;
end;  // TfraDeterminationGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.FrameResize(Sender: TObject);
begin
  inherited;
  // Keep buttons same height.
  btnDeterminerInferred.Height := eDeterminer.Height;
end;  // TfraDeterminationGeneral.FrameResize 

{-------------------------------------------------------------------------------
  Gets the caption of the frame. 
}
function TfraDeterminationGeneral.GetCaption: String;
begin
  Result := eDetermination.Text;
end;  // TfraDeterminationGeneral.GetCaption 

{-------------------------------------------------------------------------------
}
function TfraDeterminationGeneral.GetParams: TVariantArray;
begin
  FLifeScience := AdditionalProperties.GetProperty(PROP_SPECIMEN_IS_LIFESCIENCES);
  Result := VarArrayOf(['@Key', Key, '@IsLifeScience', FLifeScience,
                        '@IsSpecimenUnit', Ord(MasterFrameType = mftCollectionUnit)]);
end;  // TfraDeterminationGeneral.GetParams 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.InferenceClick(Sender: TObject);
begin
  with TImageListButton(Sender) do begin
    if Caption='' then
      Caption:='!'
    else if Caption='!' then
      Caption:='?'
    else if Caption='?' then
      Caption:='!?'
    else if Caption='!?' then
      Caption:='';
    if Caption='' then
      ImageIndex := 6    // enable the tick
    else
      ImageIndex := -1;  // disable the tick to show the caption
  end;
end;  // TfraDeterminationGeneral.InferenceClick 

{-------------------------------------------------------------------------------
}
function TfraDeterminationGeneral.InferredCaptionToValue(ACaption: String): ShortInt;
begin
  if      ACaption = ''   then Result := 0
  else if ACaption = '!'  then Result := 1
  else if ACaption = '?'  then Result := 2
  else if ACaption = '!?' then Result := 3
  else
    raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
end;  // TfraDeterminationGeneral.InferredCaptionToValue 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.InferredValueToButtonImage(AButton: TImageListButton;
    AValue: Integer);
begin
  with AButton do begin
    case AValue of
      0 : begin
            Caption := '';
            ImageIndex := 6;
          end;
      1 : begin
            Caption := '!';
            ImageIndex := -1;
          end;
      2 : begin
            Caption := '?';
            ImageIndex := -1;
          end;
      3 : begin
            Caption := '!?';
            ImageIndex  := -1;
          end;
    else
      raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
    end;
  end;
end;  // TfraDeterminationGeneral.InferredValueToButtonImage 

{-------------------------------------------------------------------------------
  Loads any extra required data for the frame. 
}
procedure TfraDeterminationGeneral.LoadData;
begin
  inherited LoadData;

  // Initialise parent keys, according to the context.
  FCollectionUnitKey := '';
  FOccurrenceKey := '';
  if Key = '' then begin
    case MasterFrameType of
      mftCollectionUnit: FCollectionUnitKey := ParentKey;
      mftOccurrence:     FOccurrenceKey := ParentKey;
    end;
    FPreviousTerm := '';
  end;

    // If new Determination, recordset will be empty (key='' won't be found).
  if not RegisteredRecordsets[0].Eof then begin
    with RegisteredRecordsets[0] do begin
      FTimestamp         := Fields['Timestamp'].Value;
      FOccurrenceKey     := VarToStr(Fields['Occurrence_Key'].Value);
      FCollectionUnitKey := VarToStr(Fields['Specimen_Collection_Unit_Key'].Value);
      InferredValueToButtonImage(btnDeterminerInferred, Fields['Inferred_Determiner'].Value);

      FPreviousTerm      := VarToStr(Fields['Term'].Value);
    end;
  end else
    ClearInferredButtons;
end;  // TfraDeterminationGeneral.LoadData

{-------------------------------------------------------------------------------
  Registers the controls used in the frame.
}
procedure TfraDeterminationGeneral.RegisterControls;
begin
  inherited;
  RegisterRecordSet('usp_Determination_Select', GetParams);
  
  RegisterControl(eDetermination, 'Term', 'DetKey', True, ResStr_Determination,
                  CheckLinkedDeterminationEarthScience);
  //Since ARequestDataType is dependent upon the the data when it is loaded
  //ie FLifeSciences, reset the OnGetData event to compensate
  eDetermination.OnGetData := eDeterminationGetData;
  eDetermination.OnFindData := eDeterminationFindData;
  
  RegisterAsyncControl(lblDomain, 'usp_DomainsForMask_Get',
                       ['@Mask', 'Domain_Mask'], '@Domains');
  RegisterControl(cmbType, 'Type', 'Determination_Type_Key',
                  True, ResStr_DeterminationType);
  RegisterControl(cmbStatus, 'Status_Term', 'Status_Concept_Key');
  RegisterConceptGroupComboBox(cmbStatus, CG_NOMENCLATURAL_STATUSES);
  RegisterControl(cmbConfidence, '', 'Confidence', True, ResStr_Confidence);
  RegisterControl(eDeterminer, 'Determiner', 'Determiner_Name_Key',
                  True, ResStr_DeterminerName,
                  CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  eDeterminer.OnGetData := eDeterminerGetData;
  
  RegisterControl(cmbRole, 'Determiner_Role', 'Determiner_Role_Key',
                  True, ResStr_DeterminerRole);
  RegisterControl(eDate, '', True, ResStr_DeterminationDate);
  RegisterControl(cmbDetMadeAgainst, '', 'Used_Specimen',
                  True, ResStr_DeterminationMadeAgainst);
  RegisterControl(chkPreferred, 'Preferred');
  RegisterControl(mmMethod, 'Method');
  RegisterControl(mmNotes, 'Notes');
  RegisterControl(chkIncludeInLabel, 'Include_In_Label');
end;  // TfraDeterminationGeneral.RegisterControls

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.RegisterDragDropComponents;
begin
  inherited;
  RegisterDropComponent(eDetermination, DropDetermination,
                        [TN_TAXON_LIST_ITEM, TN_CONCEPT], [CF_JNCCDATA],
                        DragOverDeterminationCheck);
  
  RegisterDropComponent(eDeterminer, DropDeterminer,
                        [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraDeterminationGeneral.RegisterDragDropComponents

{-------------------------------------------------------------------------------
  Saves\Updates data in tables with frame data. 
}
procedure TfraDeterminationGeneral.SaveData;
var
  lParams: Array of Variant;
  lProcName: String;
  lDet_Key: String;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@DeterminedItemKey', eDetermination.Key,
                         '@OccurrenceKey', FOccurrenceKey,
                         '@SpecimenCollectionUnitKey', FCollectionUnitKey,
                         '@DeterminationTypeKey', cmbType.CurrentStrID,
                         '@NomenclaturalStatusConceptKey', cmbStatus.CurrentStrID,
                         '@Confidence', cmbConfidence.CurrentStrID,
                         '@DeterminerNameKey', eDeterminer.Key,
                         '@InferredDeterminer', InferredCaptionToValue(
                             btnDeterminerInferred.Caption),
                         '@DeterminerRoleKey', cmbRole.CurrentStrID,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@UsedSpecimen', cmbDetMadeAgainst.CurrentStrID,
                         '@Preferred', Ord(chkPreferred.Checked),
                         '@Method', mmMethod.Text,
                         '@Notes', mmNotes.Text,
                         '@Timestamp', FTimestamp,
                         '@IncludeInLabel', Ord(chkIncludeInLabel.Checked),
                         '@IsForSpecimen', Ord(MasterFrameType = mftCollectionUnit),
                         '@RecordsAffected', ''
                        ]);

  if FLifeScience then lProcName := 'usp_TaxonDetermination_'
                  else lProcName := 'usp_Determination_';
  
  // Empty key means new record.
  if Key = '' then begin
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_DETERMINATION,
                                                  lProcName + 'Insert', lParams,
                                                  '@Key'));
    if MasterFrameType = mftCollectionUnit then
      if FLifeScience then
        dmGeneral.RunStoredProc(lProcName + 'TaxonOccurrence_Update',
                                ['@TaxonDeterminationKey', Key,
                                 '@SpecimenUnitKey', FCollectionUnitKey])
      else
        dmGeneral.RunStoredProc(lProcName + 'Occurrence_Update',
                                ['@DeterminationKey', Key,
                                 '@SpecimenUnitKey', FCollectionUnitKey]);
  end else
    dmGeneral.RunUpdateStoredProc(lProcName + 'Update', lParams);
  
  //Set chkPreferred depending on whether Determination is preferred
  chkPreferred.Checked := dmGeneral.GetStoredProcOutputParam(
                              'usp_Determination_IsPreferred_Get',
                              ['@DeterminationKey', Key,
                               '@SpecimenUnitKey', FCollectionUnitKey,
                               '@IsLifeSciences', FLifeScience,
                               '@IsSpecimenUnit', Ord(MasterFrameType = mftCollectionUnit)],
                               '@IsPreferred') <> 0;

  // Note: Always update caption because the noclamentural status could
  // cause an update.
  //Update Specimen node caption if this is the preferred Determination
  //if chkPreferred.Checked then begin
    if Assigned(OnFrameNotification) then begin
      //NB Det_Key is Concept_Key or Index_Taxon_Name_Key
      lDet_Key := dmGeneral.GetStoredProcOutputParam('usp_Det_Key_From_Determination_Get',
                      ['@A_Determination_Key', Key, '@IsLifeSciences', FLifeScience],
                      '@Det_Key');
      if Assigned(OnFrameNotification) then
        OnFrameNotification(Self, etRefreshSpecimenCaption, VarArrayOf(['Key', lDet_Key]));
    end;
  if chkPreferred.Checked then begin
    UpdateStoreItemName;
  end;
end;  // TfraDeterminationGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.UpdateDetermination(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  // If nothing is returned then set lKey to ''. An appropriate message will
  // be displayed below
  if AKeyList.ItemCount = 0 then lKey := ''
                            else lKey := AKeyList.GetKeyItem(0).KeyField1;
  if FLifeScience then begin
    eDetermination.Text := ConvertDeterminationLifeScienceKeyToCaption(lKey,
                                                                       TN_TAXON_LIST_ITEM);
    eDetermination.Key := lKey;
  end else
  if ConceptHasOccurrences(lKey) then begin
    if not ConceptHasMappedTaxon(lKey) then begin
      eDetermination.Text := ConvertDeterminationEarthScienceKeyToCaption(lKey, TN_CONCEPT);
      eDetermination.Key := lKey;
      if ConceptHasDomain(lKey, DOM_STRATIGRAPHY) then begin
        chkIncludeInLabel.Checked := True;
      end;
    end;
  end else
  if lKey = '' then
    MessageDlg(ResStr_NoDeterminationSelected, mtInformation, [mbOK], 0)
  else
    MessageDlg(ResStr_DictionaryTermsWithOccurrences, mtInformation, [mbOK], 0);
end;  // TfraDeterminationGeneral.UpdateDetermination 

{-------------------------------------------------------------------------------
}
procedure TfraDeterminationGeneral.UpdateDeterminer(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eDeterminer, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblDeterminer.Caption)]));
end;  // TfraDeterminationGeneral.UpdateDeterminer 

{-------------------------------------------------------------------------------
  If this determination is for a specimen that is also a store, and this determination is being added because the user has been prompted to add it after adding the specimen, the store's item_name will currently be 'unspecified'. Now the specimen has a determination, the store can be given a proper name. The following method updates the store record so it now has the correct name.
}
procedure TfraDeterminationGeneral.UpdateStoreItemName;
var
  lRecordset: _Recordset;
  lStoreName: String;
begin
  // Get store name only when about to save, in case store name already changed while
  // editing here...
  lStoreName := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_StoreName_Get',
                                 ['@StoreKey', FCollectionUnitKey], '@StoreName'));
  
  lRecordset := dmGeneral.GetRecordset('usp_Store_Select', ['@Key', FCollectionUnitKey]);
  
  with lRecordset do
    if not Eof or not Bof then
      if (VarToStr(Fields['Item_Name'].Value) = ResStr_Unspecified) or
         (lStoreName = FPreviousTerm) then
        dmGeneral.RunUpdateStoredProc('usp_Store_Update',
              ['@Key', FCollectionUnitKey,
               '@ItemName', eDetermination.Text,
               '@CurrentContainerKey', Fields['Current_Container_Collection_Unit_Key'].Value,
               '@UsualContainerKey', Fields['Usual_Container_Collection_Unit_Key'].Value,
               '@StoreTypeConceptKey', Fields['Store_Type_Concept_Key'].Value,
               '@Comment', Fields['Comment'].Value,
               '@CurrentLocationCode', Fields['Current_Location_Code'].Value,
               '@UsualLocationCode', Fields['Usual_Location_Code'].Value,
               '@Timestamp', Fields['StoreTimestamp'].Value
              ]);
end;  // TfraDeterminationGeneral.UpdateStoreItemName 

{-------------------------------------------------------------------------------
  Checks that the user hasn't entered a future date in the Date field. 
}
procedure TfraDeterminationGeneral.ValidateData;
var
  lCurrentDate: TVagueDate;
begin
  inherited ValidateData;
  
  lCurrentDate := StringToVagueDate(DateToStr(Date));
  
  ValidateValue(CompareVagueDateToVagueDate(lCurrentDate,
      eDate.VagueDate) <> -1, Format(ResStr_DateCannotBeInFuture,
      [ResStr_Date]), eDate);
end;  // TfraDeterminationGeneral.ValidateData


end.


