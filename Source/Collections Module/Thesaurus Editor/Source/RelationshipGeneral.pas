{===============================================================================
  Unit:        RelationshipGeneral

  Defines:     TfraRelationshipGeneral

  Description:

  Model:       ThesaurusEditor.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 19 $
    $Date: 15/12/06 10:59 $
    $Author: Ericsalmon $

===============================================================================}

unit RelationshipGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, DataTypes, DSSDataTypes,
  ComboListID, LuxIDComboBox, ComCtrls, LuxembourgConstants,
  BaseCompositeComponent, LinkedControls, InterfaceDataModule, ResourceStrings,
  Validation, DataClasses, ExceptionForm, SearchManager;

type

  ERelationshipGeneralException = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a relationship between 2 concepts,
    2 meanings or 2 term versions to be viewed and edited.
    Depending on the selection in cmbApplyTo, the details are loaded from and
    saved to either the Concept_Relation, Meaning_Relation or
    Term_Version_Relation table.  If the user changes the cmbApplyTo setting
    when editing an existing relationship, the relationship record is removed
    and a new one inserted into the appropriate table (this occurs when the
    relationship is saved).
  }
  TfraRelationshipGeneral = class(TBaseTabSheetFrame)
    chkInherited: TCheckBox;
    cmbApplyTo: TComboBox;
    cmbRelationshipType: TLuxIDComboBox;
    eMultiplicity: TEdit;
    eText: TLinkedEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mmComment: TMemo;
    procedure cmbRelationshipTypePopulate(Sender: TObject);
    procedure eTextExit(Sender: TObject);
    procedure cmbApplyToChange(Sender: TObject);
  private
    FConceptGroupKey: TKeyString;
    FConceptGroupKeyChild: TKeyString;
    FDirectionList: TStringList;
    FHierarchyRelationTypeKey: TKeyString;
    FOriginalRelationshipAppliesTo: TAppliesTo;
    FRelationshipAppliesTo: TAppliesTo;
    FTableName: string;
    FTableNameCompact: string;
    FTermVersionKeyChild: TKeyString;
    FTermVersionKeyParent: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    FLastApplyToIndex: integer;
    procedure CheckNotSynonyms;
    procedure CheckValidConcept(ADoCheckOK: Boolean);
    procedure cmbApplyToPopulate;
    function ConvertComboIDToTableName(AAppliesTo: TAppliesTo): string;
    function ConvertComboIDToTableNameCompact(AAppliesTo: TAppliesTo): string;
    procedure CreateTermsForRelationship(AFromKey, AToKey: TKeyString);
    procedure DropRelationship(const Sender: TObject; const AFormat: Integer;
        const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure GetChildInformation(AConceptKey: TKeyString);
    function GetParams: TVariantArray;
    function GetStoredProcName: string;
    procedure RepopulateRelationshipTypes;
    procedure SetRelationshipAppliesTo(Value: TAppliesTo);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property RelationshipAppliesTo: TAppliesTo read FRelationshipAppliesTo
            write SetRelationshipAppliesTo;
  end;
  
//==============================================================================
implementation

uses GeneralData, BaseDetailFrameUnit, GeneralFunctions, ComObj, DropTarget;

{$R *.dfm}

{-==============================================================================
    TfraRelationshipGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Creates the string list, anchors the eText linked edit and
          populates the cmbApplyTo combo box.
}
constructor TfraRelationshipGeneral.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  FDirectionList := TStringList.Create;
  eText.Anchors := [akLeft, akTop, akRight];
  
  cmbApplyToPopulate;
end;  // TfraRelationshipGeneral.Create 

{-------------------------------------------------------------------------------
  Destructor. Frees the string list. 
}
destructor TfraRelationshipGeneral.Destroy;
begin
  FDirectionList.Free;
  inherited;
end;  // TfraRelationshipGeneral.Destroy 

{-------------------------------------------------------------------------------
  Synonyms cannot have meaning relationships applied to them. 
}
procedure TfraRelationshipGeneral.CheckNotSynonyms;
var
  lAreSynonyms: Boolean;
begin
  lAreSynonyms := dmGeneral.GetStoredProcOutputParam('usp_ConceptsAreSynonyms_Get',
                                                    ['@ConceptKey1', ParentKey,
                                                    '@ConceptKey2', eText.Key],
                                                    '@AreSynonyms');
  ValidateValue(lAreSynonyms = False, ResStr_SynonymsCannotHaveMeaningRelations);
end;  // TfraRelationshipGeneral.CheckNotSynonyms 

{-------------------------------------------------------------------------------
  Check the concept is in the correct concept group if the RelationshipType is the hierarchy relation type. If it isn't, it will loop and keep on asking the user to enter a correct concept unless they click cancel, or actually do enter a concept in the correct concept group.
}
procedure TfraRelationshipGeneral.CheckValidConcept(ADoCheckOK: Boolean);
var
  lKey, lText: string;
begin
  while ADoCheckOK do begin // While the user hasn't clicked Cancel on the save dialog.
    if cmbRelationshipType.CurrentStrID = FHierarchyRelationTypeKey then
      if FConceptGroupKey <> FConceptGroupKeyChild then begin
        lKey := '';
        lText := '';
        ShowMessage(ResStr_IncorrectConceptGroup);
        ADoCheckOK := DoCheck(lKey, lText, stConcept);
        if ADoCheckOK then begin
          eText.Key := lKey;
          eText.Text := lText;
        end else
          Abort; // Forget trying to save if the user clicks cancel on the find dialog
      end else
        Break // If the parent and child concept groups are now the same, we have succeeded.
    else
      Break;  // If the user has changed the relationship type, it doesn't matter about
              // the concept groups being the same anymore.
  end;
end;  // TfraRelationshipGeneral.CheckValidConcept 

{-------------------------------------------------------------------------------
  Depending on the term version keys, there should be different items in the
          Apply To combo box. This method adjusts the contents by deleting
          ResStr_TermVersions or adding it back where necessary.
}
procedure TfraRelationshipGeneral.cmbApplyToPopulate;
begin
  with cmbApplyTo.Items do begin
    if (cmbApplyTo.Items.Count = 0) then begin
      Add(ResStr_Concepts);
      Add(ResStr_AllConceptsThatShareSameMeaning);
      Add(ResStr_TermVersions);
    end
    else if (cmbApplyTo.Items.Count = 3) then begin
      if (FTermVersionKeyChild = '') or (FTermVersionKeyParent = '') then
        Delete(2);
    end
    else if (FTermVersionKeyChild <> '') and (FTermVersionKeyParent <> '') then
      Add(ResStr_TermVersions)
  end;
end;  // TfraRelationshipGeneral.cmbApplyToPopulate 

{-------------------------------------------------------------------------------
  Populate the Relationship Type combo box. 
}
procedure TfraRelationshipGeneral.cmbRelationshipTypePopulate(Sender: TObject);
var
  lRelationTypeUsage: Integer;
begin
  inherited;
  
  // Map the applies to combo box onto the type usage number
  case cmbApplyTo.ItemIndex of
    0: lRelationTypeUsage := 2;
    1: lRelationTypeUsage := 1;
    2: lRelationTypeUsage := 3;
  else
    lRelationTypeUsage := 0;
  end;
  with dmGeneral.GetRecordset('usp_ThesaurusRelationForwardReverse_Select',[
      '@RelationUsage', lRelationTypeUsage]) do begin
    while not Eof do begin
      if VarToStr(Fields['Item_Name'].Value) <> '' then begin
        cmbRelationshipType.Add(VarToStr(Fields['Item_Name'].Value),
                    VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value));
  
        if Fields['IsForward'].Value = 1 then
          FDirectionList.Add('F' + VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value) +
                             '=' + VarToStr(Fields['Item_Name'].Value))
        else
          FDirectionList.Add('R' + VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value) +
                             '=' + VarToStr(Fields['Item_Name'].Value));
      end;
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraRelationshipGeneral.cmbRelationshipTypePopulate 

{-------------------------------------------------------------------------------
  The tablename used to determine which update and insert stored proc to use
          depends on the ApplyTo combobox.
}
function TfraRelationshipGeneral.ConvertComboIDToTableName(AAppliesTo:
        TAppliesTo): string;
begin
  case AAppliesTo of
    atConcept:      Result := TN_CONCEPT_RELATION;
    atMeaning:      Result := TN_MEANING_RELATION;
    atTermVersion:  Result := TN_TERM_VERSION_RELATION;
  end;
end;  // TfraRelationshipGeneral.ConvertComboIDToTableName 

{-------------------------------------------------------------------------------
  The tablenamecompact used to determine which update and insert stored proc to
          use depends on the ApplyTo combobox.
}
function TfraRelationshipGeneral.ConvertComboIDToTableNameCompact(AAppliesTo:
        TAppliesTo): string;
begin
  case AAppliesTo of
    atConcept :
      Result := StringReplace(TN_CONCEPT_RELATION, '_', '', [rfReplaceAll]);
    atMeaning :
      Result := StringReplace(TN_MEANING_RELATION, '_', '', [rfReplaceAll]);
    atTermVersion :
      Result := StringReplace(TN_TERM_VERSION_RELATION, '_', '', [rfReplaceAll]);
  end;
end;  // TfraRelationshipGeneral.ConvertComboIDToTableNameCompact 

{-------------------------------------------------------------------------------
  Ask user if they want to ensure terms are available at one/both ends of a
          relationship. See 4.2.17.23 of TSD.
}
procedure TfraRelationshipGeneral.CreateTermsForRelationship(AFromKey, AToKey:
        TKeyString);
var
  lMessage: string;
begin
  if dmGeneral.GetStoredProcOutputParam('usp_MeaningsShareTermsCount_Get',
                  ['@FromConceptKey', AFromKey,
                  '@ToConceptKey', AToKey,
                  '@ThesaurusRelationTypeKey', cmbRelationshipType.CurrentStrID],
                  '@Matches') > 0 then begin
    // See if the user has selected a unidirectional relationship or not.
    if FDirectionList.Values['R' + cmbRelationshipType.CurrentStrID] = ''
                              then lMessage := ResStr_MeaningsShareTermUnidirectional
                              else lMessage := ResStr_MeaningsShareTerm;
    if MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      dmGeneral.RunUpdateStoredProc('usp_MeaningsShareTermsClone_Update',
            ['@FromConceptKey', AFromKey,
            '@ToConceptKey', AToKey,
            '@Unidirectional', Ord(lMessage = ResStr_MeaningsShareTermUnidirectional)]);
  end;
end;  // TfraRelationshipGeneral.CreateTermsForRelationship 

{-------------------------------------------------------------------------------
  Delete method. 
}
procedure TfraRelationshipGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_' + FTableNameCompact + '_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraRelationshipGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Set the colours of fields. 
}
procedure TfraRelationshipGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  
  if RelationshipAppliesTo = atTermVersion then
    chkInherited.Enabled := False
  else
    chkInherited.Enabled := True;
  
  SetRequiredFieldsColourState(AEnabled, [eText, cmbRelationshipType,
                                          cmbApplyTo]);
end;  // TfraRelationshipGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraRelationshipGeneral.eTextExit(Sender: TObject);
begin
  inherited;
  if eText.Key <> '' then
    GetChildInformation(eText.Key);
  cmbApplyToPopulate;
end;  // TfraRelationshipGeneral.eTextExit 

{-------------------------------------------------------------------------------
  Return the caption. 
}
function TfraRelationshipGeneral.GetCaption: string;
begin
  Result := cmbRelationshipType.CurrentItem + ' ' + eText.Text;
end;  // TfraRelationshipGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Uses the concept key to get the concept group and term version key of the
          child node in the relationship.
}
procedure TfraRelationshipGeneral.GetChildInformation(AConceptKey: TKeyString);
begin
  if AConceptKey <> '' then begin
    with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', AConceptKey])
    do begin
      FTermVersionKeyChild  := VarToStr(Fields['Term_Version_Key'].Value);
      FConceptGroupKeyChild := VarToStr(Fields['Concept_Group_Key'].Value);
      Close;
    end;
  end else begin
    FTermVersionKeyChild := '';
    FConceptGroupKeyChild := '';
  end;
end;  // TfraRelationshipGeneral.GetChildInformation

{-------------------------------------------------------------------------------
  Get the parameters required to populate the recordset. 
}
function TfraRelationshipGeneral.GetParams: TVariantArray;
begin
  RelationshipAppliesTo := AdditionalProperties.GetProperty(PROP_RELATIONSHIP_APPLIES_TO);
  
  // ParentKey is the ConceptKey
  Result := VarArrayOf(['@Key', Key, '@ConceptKey', ParentKey, '@AppliesTo',
                                            Integer(FRelationshipAppliesTo)]);
  
end;  // TfraRelationshipGeneral.GetParams 

{-------------------------------------------------------------------------------
  Get the stored proc name required to populate the recordset. 
}
function TfraRelationshipGeneral.GetStoredProcName: string;
begin
  case RelationshipAppliesTo of
    atConcept:      Result := 'usp_ConceptRelation_Select';
    atMeaning:      Result := 'usp_MeaningRelation_Select';
    atTermVersion:  Result := 'usp_TermVersionRelation_Select';
  end;
end;  // TfraRelationshipGeneral.GetStoredProcName 

{-------------------------------------------------------------------------------
  Load data into the frame. 
}
procedure TfraRelationshipGeneral.LoadData;
begin
  // Without this line, there is a glitch that means cmbRelationship will
  // only show the term it loaded first for that key (i.e. if the forward term
  // or a relationship type is selected, changing it to the reverse term, and
  // then reloading the frame, will still cause the forward term to be shown).
  cmbRelationshipType.Clear;
  
  inherited;
  
  // Set the value in cmbApplyTo combo box.
  case FRelationshipAppliesTo of
    atConcept     : cmbApplyTo.ItemIndex := 0;
    atMeaning     : cmbApplyTo.ItemIndex := 1;
    atTermVersion : cmbApplyTo.ItemIndex := 2;
  end;
  
  
  with RegisteredRecordsets[0] do
    if not RegisteredRecordsets[0].Eof then begin
      FTimestamp := Fields['Timestamp'].Value;
    end else
      cmbApplyTo.ItemIndex := -1;
  
  // Get the Concept_Group_Key and the Hierarchy_Relation_Type_Key using the
  // ParentKey, rather than from the relationship recordsets, because if we
  // are adding a new record we need to know this info.
  
  with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', ParentKey]) do begin
    FConceptGroupKey          := VarToStr(Fields['Concept_Group_Key'].Value);
    FHierarchyRelationTypeKey := VarToStr(Fields['Hierarchy_Relation_Type_Key'].Value);
    FTermVersionKeyParent     := VarToStr(Fields['Term_Version_Key'].Value);
    Close;
  end;
  
  GetChildInformation(eText.Key);
  
  cmbApplyToPopulate;
  FOriginalRelationshipAppliesTo := FRelationshipAppliesTo;
end;  // TfraRelationshipGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraRelationshipGeneral.RegisterControls;
begin
  inherited;
  // Gets the name of the stored proc each time it needs it.
  RegisterRecordset(GetStoredProcName, GetParams);
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eText, 'Item_Name', 'Item_Key', True, ResStr_RelatedTo,
                              CheckLinkedConcept, ResStr_RelatedTo,
                              ConvertConceptKeyToCaption);
  RegisterControl(cmbRelationshipType, 'Thesaurus_Relation_Type_Name',
                  'Thesaurus_Relation_Type_Key', True, ResStr_RelationshipType);
  RegisterControl(chkInherited, 'Inherited');
  RegisterControl(mmComment, 'Comment');
  RegisterControl(eMultiplicity, 'Multiplicity');
  
end;  // TfraRelationshipGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  When the Applies to field is changed, the list of available relationship
          types may also change.  Repopulate the list and reselect the previous
          relationship if it has changed.
}
procedure TfraRelationshipGeneral.RepopulateRelationshipTypes;
var
  lRelationshipType: string;
begin
  // Remember the previous selection
  lRelationshipType := cmbRelationshipType.CurrentStrID;
  cmbRelationshipType.Clear;
  cmbRelationshipType.PopulateContent;
  cmbRelationshipType.ItemIndex := cmbRelationshipType.IDIndexOf(lRelationshipType);
end;  // TfraRelationshipGeneral.RepopulateRelationshipTypes 

{-------------------------------------------------------------------------------
  Save data to the database. 
}
procedure TfraRelationshipGeneral.SaveData;
var
  lParams: Array of Variant;
  lFromKey: TKeyString;
  lToKey: TKeyString;
begin
  // If the ApplyTo selection has changed, the record will need to be deleted
  // and a new one added in another table.
  if (FOriginalRelationshipAppliesTo <> FRelationshipAppliesTo)
      and (Key <> '')then begin
    dmGeneral.RunDeleteStoredProc('usp_' +
                  ConvertComboIDToTableNameCompact(FOriginalRelationshipAppliesTo) +
                  '_Delete', ['@Key', Key, '@Timestamp', FTimestamp]);
    Key := '';  // Clear the key so a new record will now be added.
  end;
  
  // Depending on whether the forward or reverse version of the relation has
  // been selected, determines the order the current key and the parent key
  // are stored in the Collection_Unit_Relation table.
  if FDirectionList.Values['F' + cmbRelationshipType.CurrentStrID] = cmbRelationshipType.Text then begin
    lFromKey := ParentKey;
    lToKey   := eText.Key;
  end else begin
    lFromKey := eText.Key;
    lToKey   := ParentKey;
  end;
  
  lParams := VarArrayOf(['@Key', Key,
                          '@FromConceptKey', lFromKey,
                          '@ToConceptKey', lToKey,
                          '@ThesaurusRelationTypeKey', cmbRelationshipType.CurrentStrID,
                          '@Multiplicity', eMultiplicity.Text,
                          '@Inherited', chkInherited.Checked and chkInherited.Enabled,
                          '@Comment', mmComment.Text,
                          '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then try
    Key := VarToStr(dmGeneral.RunInsertStoredProc(FTableName,
                                    'usp_' + FTableNameCompact + '_Insert',
                                    lParams,
                                    '@Key'))
    except
      on E:EOleException do begin
        if CompareText(E.Message, 'Cyclical relationship')=0 then
          raise ERelationshipGeneralException.CreateNonCritical(Format(
              ResStr_CyclicRelationship, [ResStr_Parent]), E)
        else
          raise E;
      end;
    end // try
  else
    dmGeneral.RunUpdateStoredProc('usp_' + FTableNameCompact + '_Update', lParams);
  
  CreateTermsForRelationship(lFromKey, lToKey);
end;  // TfraRelationshipGeneral.SaveData 

{-------------------------------------------------------------------------------
  Whenever the RelationshipAppliesTo property is changed, FTableName and
          FTableNameCompact need to be updated to reflect what the combo box is
          showing.
}
procedure TfraRelationshipGeneral.SetRelationshipAppliesTo(Value: TAppliesTo);
begin
  FRelationshipAppliesTo := Value;
  
  FTableName := ConvertComboIDToTableName(Value);
  FTableNameCompact := ConvertComboIDToTableNameCompact(Value);
  chkInherited.Enabled := not (RelationshipAppliesTo = atTermVersion);
  RepopulateRelationshipTypes;
end;  // TfraRelationshipGeneral.SetRelationshipAppliesTo 

{-------------------------------------------------------------------------------
  Validate the data. 
}
procedure TfraRelationshipGeneral.ValidateData;
begin
  inherited;
  if (eText.Key <> '') and (FConceptGroupKeyChild='') then
    GetChildInformation(eText.Key);
  ValidateValue(cmbApplyTo.ItemIndex >= 0, Format(ResStr_MissingData,
      [ResStr_ApplyTo]), cmbApplyTo);
  ValidateValue((IsFloat(eMultiplicity.Text) or (eMultiplicity.Text=''))
      and (eMultiplicity.Text<>'.'),
      ResStr_SupplyValidMultiplicity, eMultiplicity);
  CheckValidConcept(True);
  if FRelationshipAppliesTo = atMeaning then CheckNotSynonyms;
end;  // TfraRelationshipGeneral.ValidateData 

procedure TfraRelationshipGeneral.cmbApplyToChange(Sender: TObject);
begin
  inherited;
  if EditMode = emEdit then begin
    case cmbApplyTo.ItemIndex of
      0 : RelationshipAppliesTo := atConcept;
      1 : RelationshipAppliesTo := atMeaning;
      2 : RelationshipAppliesTo := atTermVersion;
    end;
    FLastApplyToIndex := cmbApplyTo.ItemIndex;
  end
  else
    // Reset the old selection as it is read only
    cmbApplyTo.ItemIndex := FLastApplyToIndex;
end;

{-------------------------------------------------------------------------------

}
procedure TfraRelationshipGeneral.RegisterDragDropComponents;
begin
  inherited;
  RegisterDropcomponent(eText, DropRelationship, [TN_CONCEPT], [CF_JNCCDATA]);
end;

{-------------------------------------------------------------------------------
  Drop a relationship onto the linked edit box
}
procedure TfraRelationshipGeneral.DropRelationship(const Sender: TObject;
  const AFormat: Integer; const ASourceData: TKeyList;
  const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eText, TN_CONCEPT,
      'usp_Concept_Get', '@Key', '@Caption');
  eTextExit(nil);
end;

end.

