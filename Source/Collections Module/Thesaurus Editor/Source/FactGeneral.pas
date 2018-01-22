unit FactGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, ComboListID,
  LuxIDComboBox, DataClasses, DataTypes, VagueDateEdit, LuxembourgConstants,
  ImageListButton, ExtDlgs, Registry, ResourceStrings, ExceptionForm;

type
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a fact to be viewed and edited.
    Facts can be associated with all terms that share a meaning, a single
    concept, a single term version, or all related term versions (identified by
    using the relationships in Term_Version_Relation to find term versions that
    at least have some overlap with the selected term).
  }
  TfraFactGeneral = class(TBaseTabSheetFrame)
    btnDataFileDialog: TImageListButton;
    chkInherited: TCheckBox;
    cmbAppliesTo: TComboBox;
    cmbLanguage: TLuxIDComboBox;
    cmbType: TLuxIDComboBox;
    dlgGetFilename: TOpenPictureDialog;
    eDataFile: TEdit;
    eDate: TVagueDateEdit;
    eTitle: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblDataCaption: TLabel;
    mmData: TMemo;
    procedure btnDataFileDialogClick(Sender: TObject);
    procedure cmbLanguagePopulate(Sender: TObject);
    procedure cmbTypeChange(Sender: TObject);
    procedure cmbTypePopulate(Sender: TObject);
  private
    FChkBoxEnableable: Boolean;
    FConceptKey: TKeyString;
    FDictImagesPath: string;
    FLocalImagesPath: string;
    FMeaningKey: TKeyString;
    FTermVersionKey: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    FTypeIsImage: Boolean;
    procedure ComboIDToKey(AComboBoxID: Integer; var ConceptKey, MeaningKey,
            TermVersionKey: TKeyString; var RelatedTermVersions: boolean);
    function ComboKeysToID(Concept_Key, Meaning_Key, Term_Version_Key:
            TKeyString; RelatedTermVersions: Boolean): Integer;
    function GenerateFileName(AFilename: String): string;
    procedure GetImagePaths;
    function RetrieveFileName(AFilename: String): string;
    procedure SeeIfTypeIsImage(AMeaningKey: TKeyString);
    procedure SetChkBoxEnableableCallback(ATarget: TObject; AValue: Variant);
    procedure SetTypeIsImage(Value: Boolean);
    property TypeIsImage: Boolean read FTypeIsImage write SetTypeIsImage;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner : TComponent); override;
  end;
  
//==============================================================================
implementation

uses GeneralData, BaseDetailFrameUnit, ThesaurusApplicationSettings;

{$R *.dfm}

{-==============================================================================
    TfraFactGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor initialises objects and lays out the controls correctly.  This
          allows anchoring to work.
}
constructor TfraFactGeneral.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  GetImagePaths;
end;  // TfraFactGeneral.Create 

{-------------------------------------------------------------------------------
  Put the filename selected from a 'file open' dialog into eDataFile and give
          it focus.
}
procedure TfraFactGeneral.btnDataFileDialogClick(Sender: TObject);
begin
  inherited;
  
  if dlgGetFilename.Execute then begin
    eDataFile.Text := '<IMG src="' + dlgGetFilename.FileName + '"/>';
  end;
  
  eDataFile.SetFocus;
end;  // TfraFactGeneral.btnDataFileDialogClick 

{-------------------------------------------------------------------------------
  Populate the Language combo box. 
}
procedure TfraFactGeneral.cmbLanguagePopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_Languages_Select', []) do
    while not EOF do begin
      cmbLanguage.Add(
          VarToStr(Fields['Language_Key'].Value) + ' - ' + VarToStr(
              Fields['Item_Name'].Value),
          VarToStr(Fields['Language_Key'].Value));
      MoveNext;
    end;
end;  // TfraFactGeneral.cmbLanguagePopulate 

{-------------------------------------------------------------------------------
  If the Type combo box selection changes, see if the new type is an image. 
}
procedure TfraFactGeneral.cmbTypeChange(Sender: TObject);
begin
  inherited;
  
  SeeIfTypeIsImage(cmbType.CurrentStrID);
end;  // TfraFactGeneral.cmbTypeChange 

{-------------------------------------------------------------------------------
  Populate the Type combo box. 
}
procedure TfraFactGeneral.cmbTypePopulate(Sender: TObject);
begin
  inherited;
  
  with dmGeneral.GetRecordset('usp_ThesaurusFactTypes_Select', []) do
    while not EOF do begin
      cmbType.Add(VarToStr(Fields['Item_Name'].Value),
                  VarToStr(Fields['Meaning_Key'].Value));
      MoveNext;
    end;
end;  // TfraFactGeneral.cmbTypePopulate 

{-------------------------------------------------------------------------------
  Depending on the value of the combo box, populate the either the ConceptKey,
          MeaningKey, or TermVersionKey variables.
}
procedure TfraFactGeneral.ComboIDToKey(AComboBoxID: Integer; var ConceptKey,
        MeaningKey, TermVersionKey: TKeyString; var RelatedTermVersions:
        boolean);
begin
  case AComboBoxID of
   0 : begin
          ConceptKey := '';
          MeaningKey := FMeaningKey;
          TermVersionKey := '';
          RelatedTermVersions := False;
       end;
   1 : begin
          ConceptKey := FConceptKey;
          MeaningKey := '';
          TermVersionKey := '';
          RelatedTermVersions := False;
       end;
   2 : begin
          ConceptKey := '';
          MeaningKey := '';
          TermVersionKey := FTermVersionKey;
          RelatedTermVersions := False;
       end;
   3 : begin
          ConceptKey := '';
          MeaningKey := '';
          TermVersionKey := FTermVersionKey;
          RelatedTermVersions := True;
       end;
  end;
end;  // TfraFactGeneral.ComboIDToKey 

{-------------------------------------------------------------------------------
  Depending on what keys have values, choose the correct item in the combo box. 
}
function TfraFactGeneral.ComboKeysToID(Concept_Key, Meaning_Key,
        Term_Version_Key: TKeyString; RelatedTermVersions: Boolean): Integer;
begin
  if Meaning_Key <> '' then
    Result := 0
  else if Concept_Key <> '' then
    Result := 1
  else if Term_Version_Key <> '' then begin
    if RelatedTermVersions then
      Result := 3
    else
      Result := 2;
  end
  else
    Result := -1;
end;  // TfraFactGeneral.ComboKeysToID 

{-------------------------------------------------------------------------------
  Run the delete stored proc. 
}
procedure TfraFactGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ThesaurusFact_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_ThesaurusFact_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
end;  // TfraFactGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TfraFactGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  
  cmbAppliesTo.Enabled := AEnabled;
  btnDataFileDialog.Enabled := AEnabled;
  
  if FChkBoxEnableable then
    chkInherited.Enabled := AEnabled
  else
    chkInherited.Enabled := False;
  
  SetRequiredFieldsColourState(AEnabled, [eTitle, cmbType, cmbLanguage,
                                          cmbAppliesTo, eDataFile, mmData]);
end;  // TfraFactGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Generate the filename to be stored in the database. 
}
function TfraFactGeneral.GenerateFileName(AFilename: String): string;
var
  lText: string;
begin
  lText := AFilename;
  
  while (Pos('\', lText) <> 0) do begin
    lText := Copy(lText, Pos('\', lText) + 1, Length(lText));
  end;
  
  // Compare the filenames with the paths.
  if StrLIComp(PChar(FLocalImagesPath),
               PChar(AFilename),
               length(FLocalImagesPath) - 1) = 0 then
    Result := '<#LOCAL_IMAGES>\' + lText
  else if StrLIComp(PChar(FDictImagesPath),
               PChar(AFilename),
               length(FDictImagesPath) -1) = 0 then
    Result := '<#IMAGES>\' + lText
  else
    Result := AFilename;
end;  // TfraFactGeneral.GenerateFileName 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraFactGeneral.GetCaption: string;
begin
  Result := eTitle.Text;
end;  // TfraFactGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Get the image paths from the registry. 
}
procedure TfraFactGeneral.GetImagePaths;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lReg.OpenKeyReadOnly(USER_SETTINGS_REG_PATH);
  
    if lReg.ValueExists('Local Images File Path') then
      FLocalImagesPath := lReg.ReadString('Local Images File Path');
  
    if lReg.ValueExists('Dict Images Path') then
      FDictImagesPath := lReg.ReadString('Dict Images Path');
  finally
    lReg.Free;
  end;
end;  // TfraFactGeneral.GetImagePaths 

{-------------------------------------------------------------------------------
  Load the data into the frame. 
}
procedure TfraFactGeneral.LoadData;
begin
  inherited;
  
  with AdditionalProperties do begin
    FMeaningKey       := GetProperty(PROP_MEANING_KEY);
    FTermVersionKey   := GetProperty(PROP_TERM_VERSION_KEY);
    FConceptKey       := GetProperty(PROP_CONCEPT_KEY); // Will this always be the same as ParentKey?
  end;
  
  // If FTermVersionKey is '', then it could be that the user just added a term
  // version, but the Concept node has not yet got the Term Version key. So,
  // try the database to see if the database has a Term Version key for the Concept
  if (FTermVersionKey = '') or (FMeaningKey='') then
    with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', ParentKey]) do begin
      if FTermVersionKey='' then
        FTermVersionKey   := VarToStr(Fields['Term_Version_Key'].Value);
      if FMeaningKey='' then
        FMeaningKey := VarToStr(Fields['Meaning_Key'].Value);
      Close;
    end;
  
  SeeIfTypeIsImage(cmbType.CurrentStrID);
  
  with RegisteredRecordsets[0] do begin
    if not EOF then begin
      FTimestamp := Fields['Timestamp'].Value;
      cmbAppliesTo.ItemIndex := ComboKeysToID(VarToStr(Fields['Concept_Key'].Value),
                                     VarToStr(Fields['Meaning_Key'].Value),
                                     VarToStr(Fields['Term_Version_Key'].Value),
                                     Fields['Related_Term_Versions'].Value);
      if FTypeIsImage then
        eDataFile.Text := RetrieveFileName(VarToStr(Fields['Data'].Value))
      else
        mmData.Text := VarToStr(Fields['Data'].Value);
    end; // if not EOF
  end; // with RegisteredRecordsets[0]
  
  dmGeneral.GetAsyncData('usp_ConceptGroupHierarchyKey_Get',
                            ['@ConceptKey', FConceptKey],
                            '@HierarchyRelationTypeKey',
                            nil,
                            SetChkBoxEnableableCallback);
  
  SeeIfTypeIsImage(cmbType.CurrentStrID);
end;  // TfraFactGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraFactGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ThesaurusFact_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eTitle, 'Item_Name', True, ResStr_Title);
  RegisterControl(cmbType, 'Fact_Type_Concept_Name', 'Fact_Type_Meaning_Key',
                                    True, ResStr_Type);
  RegisterControl(mmData, 'Data');
  RegisterControl(eDataFile, 'Data');
  RegisterControl(cmbLanguage, 'Language_Name', 'Language_Key', True,
                                    ResStr_Language);
  RegisterControl(chkInherited, 'Inherited');
  RegisterControl(eDate, 'Fact');
  
end;  // TfraFactGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Retrieve the filename without any of the path information 
}
function TfraFactGeneral.RetrieveFileName(AFilename: String): string;
var
  lText: string;
begin
  lText := AFilename;
  
  while (Pos('\', lText) <> 0) do begin
    lText := Copy(lText, Pos('\', lText) + 1, Length(lText));
  end;
  
  if StrLIComp(PChar('<#LOCAL_IMAGES>\'),
               PChar(AFilename),
               length('<#LOCAL_IMAGES>\') - 1) = 0 then
    Result := FLocalImagesPath + lText
  else if StrLIComp(PChar('<#IMAGES>\'),
               PChar(AFilename),
               length('<#IMAGES>\') -1) = 0 then
    Result := FDictImagesPath + lText
  else
    Result := AFilename;
end;  // TfraFactGeneral.RetrieveFileName 

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraFactGeneral.SaveData;
var
  lParams: Array of Variant;
  lConceptKey, lMeaningKey, lTermVersionKey: TKeyString;
  lRelatedTermVersions: Boolean;
  lData: string;
begin
  ComboIDToKey(cmbAppliesTo.ItemIndex, lConceptKey, lMeaningKey, lTermVersionKey,
              lRelatedTermVersions);
  
  if FTypeIsImage then
    lData := GenerateFileName(eDataFile.Text)
  else
    lData := mmData.Text;
  
  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eTitle.Text,
                          '@Data', lData,
                          '@MeaningKey', lMeaningKey,
                          '@ConceptKey', lConceptKey,
                          '@TermVersionKey', lTermVersionKey,
                          '@RelatedTermVersions', lRelatedTermVersions,
                          '@Inherited', chkInherited.Checked,
                          '@LanguageKey', cmbLanguage.CurrentStrID,
                          '@FactTypeMeaningKey', cmbType.CurrentStrID,
                          '@FactTypeMeaningName', cmbType.CurrentItem,
                          '@FactVagueDateStart', eDate.VagueDate.StartDate,
                          '@FactVagueDateEnd', eDate.VagueDate.EndDate,
                          '@FactVagueDateType', eDate.VagueDate.DateTypeString,
                          '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_THESAURUS_FACT,
                                    'usp_ThesaurusFact_Insert',
                                    lParams,
                                    '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_ThesaurusFact_Update', lParams);
end;  // TfraFactGeneral.SaveData 

{-------------------------------------------------------------------------------
  The meaning keys of the JPEG, GIF and Bitmap images have been hardcoded into
          the unit. If the current Meaning Key matches one of these, then
          return true. If it doesn't match, it isn't a JPEG, GIF or Bitmap
          image, so return false.
}
procedure TfraFactGeneral.SeeIfTypeIsImage(AMeaningKey: TKeyString);
begin
  if (AMeaningKey = MEAN_JPEG) or
     (AMeaningKey = MEAN_GIF) or
     (AMeaningKey = MEAN_Bitmap) then
    TypeIsImage := True
  else
    TypeIsImage := False;
end;  // TfraFactGeneral.SeeIfTypeIsImage 

{-------------------------------------------------------------------------------
  Callback method for GetAsyncData in LoadData. Determines whether the check
          box should be enabled or not.
}
procedure TfraFactGeneral.SetChkBoxEnableableCallback(ATarget: TObject; AValue:
        Variant);
begin
  if VarToStr(AValue) <> '' then
    FChkBoxEnableable := True
  else
    FChkBoxEnableable := False;
end;  // TfraFactGeneral.SetChkBoxEnableableCallback 

{-------------------------------------------------------------------------------
  Depending on whether the type is an image or not, various fields should be
          enabled / disabled.
}
procedure TfraFactGeneral.SetTypeIsImage(Value: Boolean);
begin
  FTypeIsImage := Value;
  
  mmData.Visible := not Value;
  eDataFile.Visible := Value;
  btnDataFileDialog.Visible := Value;
end;  // TfraFactGeneral.SetTypeIsImage 

{-------------------------------------------------------------------------------
  Validate the content of the frame. 
}
procedure TfraFactGeneral.ValidateData;
begin
  inherited;
  if TypeIsImage then
    ValidateValue(eDataFile.Text <> '', Format(ResStr_MissingData, [ResStr_Data]))
  else
    ValidateValue(mmData.Text <> '', Format(ResStr_MissingData, [ResStr_Data]));
  ValidateValue(cmbAppliesTo.ItemIndex <> -1, Format(ResStr_MissingData,
                                                          [ResStr_AppliesTo]));
  if FTermVersionKey = '' then
    ValidateValue((cmbAppliesTo.ItemIndex = 0) or (cmbAppliesTo.ItemIndex = 1),
                                            ResStr_ConceptHasNoTermVersion);
end;  // TfraFactGeneral.ValidateData 

end.
