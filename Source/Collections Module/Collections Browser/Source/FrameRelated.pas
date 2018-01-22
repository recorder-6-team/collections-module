{===============================================================================
  Unit:        FrameRelated.pas

  Defines:     TfraRelated

  Description:

  Created:     May 2003

  Model:       CollectionsSpecimens

  Last revision information:
    $Revision: 26 $
    $Date: 30/10/12 16:52 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameRelated;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  BaseDetailFrameUnit, VagueDateEdit, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  BaseCompositeComponent, LinkedControls, InterfaceDataModule, Validation,
  LuxembourgConstants, GeneralData, SearchManager, ResourceStrings, RegisteredControls,
  DataTypes, DataClasses, DropTarget, BaseTabSheetFrameUnit, RapTree,
  BrowserNodeCollectionUnits, Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    Details screen for a relationship between collection units specified in the
    Collection_Unit_Relation table.
  }
  TfraRelated = class(TBaseFullScreenFrame)
    btnTypeInferred: TImageListButton;
    cmbType: TLuxIDComboBox;
    eAuthor: TUserEdit;
    eDate: TVagueDateEdit;
    eName: TLinkedEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    lblAuthor: TLabel;
    lblRelatedUnit: TLabel;
    mmComments: TMemo;
    procedure cmbTypePopulate(Sender: TObject);
    procedure eAuthorGetData(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure InferenceClick(Sender: TObject);
  private
    FDirectionList: TStringList;
    FRelatedToCaption: String;
    FTimestamp: TSQLSvrTimestamp;
    FTopNodeContext: TNodeContext;
    procedure ClearInferredButtons;
    procedure DropAuthor(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function GetParams: TVariantArray;
    function InferredCaptionToValue(ACaption: String): ShortInt;
    procedure InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
    procedure UpdateAuthor(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  CollectionsBrowser_TLB, ExceptionForm, GeneralFunctions;

{-------------------------------------------------------------------------------
}
function CheckLinkedUnit(AControl: TLinkedEdit; const AAdditionalCaption:
    String = ''): Boolean;
var
  lNodeContext: TNodeContext;
begin
  if not Assigned(AControl.Parent) then
    raise EBrowserFrameError.Create(ResStr_ParentUnassigned)
  else
  if not (AControl.Parent is TfraRelated) then
    raise EBrowserFrameError.Create(ResStr_ParentUnassigned)
  else
    lNodeContext := TfraRelated(AControl.Parent).AdditionalProperties.GetProperty(
                                                         PROP_TOP_NODE_CONTEXT);
  case lNodeContext of
    ncSpecimen:   Result := DoCheck(AControl, stSpecimen, '', AAdditionalCaption);
    ncCollection: Result := DoCheck(AControl, stCollection, '', AAdditionalCaption);
    ncStore:      Result := DoCheck(AControl, stStoreName, '', AAdditionalCaption);
  else
    raise EBrowserFrameError.Create(ResStr_InvalidNodeContext);
  end; // case
end;  // TfraRelated.CheckLinkedUnit

{-==============================================================================
    TfraRelated
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraRelated.Create(aOwner: TComponent);
begin
  inherited;
  FDirectionList := TStringList.Create;
end;  // TfraRelated.Create 

{-------------------------------------------------------------------------------
}
destructor TfraRelated.Destroy;
begin
  FDirectionList.Free;
  inherited;
end;  // TfraRelated.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraRelated.ClearInferredButtons;
begin
  InferredValueToButtonImage(btnTypeInferred, 0);
end;  // TfraRelated.ClearInferredButtons 

{-------------------------------------------------------------------------------
  Populates the combo box with the required data. 
}
procedure TfraRelated.cmbTypePopulate(Sender: TObject);
begin
  // Each record in the Thesaurus_Relation_Table has both a forward and a reverse
  // term. Both of these terms share the same key. The combo box should list
  // both for each relationship. Hence, we need a way of establishing both the key,
  // and whether the user has clicked the forward or reverse term for that key.
  // We use a string list to store this information.
  
  with dmGeneral.GetRecordset('usp_ThesaurusRelationForwardReverse_Select',[
      '@RelationUsage', 5]) do begin
    while not Eof do begin
      if VarToStr(Fields['Item_Name'].Value) <> '' then
        cmbType.Add(VarToStr(Fields['Item_Name'].Value),
                    VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value));
  
      if Fields['IsForward'].Value = 1 then
        FDirectionList.Add('F' + VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value) +
                           '=' + VarToStr(Fields['Item_Name'].Value))
      else if VarToStr(Fields['Item_Name'].Value) <> '' then
        FDirectionList.Add('R' + VarToStr(Fields['Thesaurus_Relation_Type_Key'].Value) +
                           '=' + VarToStr(Fields['Item_Name'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraRelated.cmbTypePopulate 

{-------------------------------------------------------------------------------
}
procedure TfraRelated.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_CollectionUnitRelation_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraRelated.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraRelated.DropAuthor(const Sender: TObject; const AFormat: Integer; const
  ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eAuthor, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraRelated.DropAuthor

{-------------------------------------------------------------------------------
}
procedure TfraRelated.DropName(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  case FTopNodeContext of
    ncSpecimen:
      DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_SPECIMEN_UNIT,
                         'usp_SpecimenName_Get', '@Key', '@SpecimenName');
    ncCollection:
      DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_COLLECTION,
                         'usp_Collection_Get', '@Key', '@Caption');
    ncStore:
      DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_STORE,
                         'usp_StoreName_Get', '@StoreKey', '@StoreName');
  else
    raise EBrowserFrameError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TfraRelated.DropName

{-------------------------------------------------------------------------------
}
procedure TfraRelated.eAuthorGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateAuthor, TN_NAME);
end;  // TfraRelated.eAuthorGetData

{-------------------------------------------------------------------------------
  To remove italic tags that may appear from the find dialog, or a node that has been dragged
      and dropped. Will also replace any italic tags the user tries to enter.
}
procedure TfraRelated.eNameChange(Sender: TObject);
begin
  inherited;
  eName.Text := RemoveSubStrings(eName.Text, ['<i>', '</i>']);
end;  // TfraRelated.eNameChange

{-------------------------------------------------------------------------------
}
function TfraRelated.GetCaption: String;
begin
  Result := eName.Text;
end;  // TfraRelated.GetCaption

{-------------------------------------------------------------------------------
}
function TfraRelated.GetParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', Key, '@ParentKey',ParentKey]);
end;  // TfraRelated.GetParams

{-------------------------------------------------------------------------------
}
procedure TfraRelated.InferenceClick(Sender: TObject);
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
end;  // TfraRelated.InferenceClick

{-------------------------------------------------------------------------------
}
function TfraRelated.InferredCaptionToValue(ACaption: String): ShortInt;
begin
  if      ACaption = ''   then Result := 0
  else if ACaption = '!'  then Result := 1
  else if ACaption = '?'  then Result := 2
  else if ACaption = '!?' then Result := 3
  else
    raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
end;  // TfraRelated.InferredCaptionToValue

{-------------------------------------------------------------------------------
}
procedure TfraRelated.InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
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
end;  // TfraRelated.InferredValueToButtonImage

{-------------------------------------------------------------------------------
}
procedure TfraRelated.LoadData;
begin
  inherited;
  with RegisteredRecordsets[0] do
    if not Eof then begin
      FTimeStamp := Fields['Timestamp'].Value;
      InferredValueToButtonImage(btnTypeInferred, Fields['Inferred_Type'].Value);
      if cmbType.Populated then
        cmbType.ItemIndex := cmbType.IndexOf(VarToStr(Fields['RelationType'].Value));
    end else
      ClearInferredButtons;
end;  // TfraRelated.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraRelated.RegisterControls;
begin
  inherited;
  // Get the correct caption for lblRelatedUnit
  FTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);

  case FTopNodeContext of
    ncCollection: FRelatedToCaption := ResStr_RelatedCollection;
    ncSpecimen  : FRelatedToCaption := ResStr_RelatedSpecimen;
    ncStore     : FRelatedToCaption := ResStr_RelatedStore;
  else
    raise EBrowserFrameError.Create(ResStr_InvalidNodeContext);
  end;
  lblRelatedUnit.Caption := FRelatedToCaption + ':';

  // This stored procedure needs to be passed specific parameters of its own.
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnitRelation_Select', GetParams);
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'RelatedTo', 'Related_Unit_Key', True,
                  StringReplace(lblRelatedUnit.Caption, ':', '', [rfReplaceAll]),
                  CheckLinkedUnit);

  RegisterControl(cmbType, 'RelationType', 'Thesaurus_Relation_Type_Key',
                  True, ResStr_Type);
  RegisterControl(eDate, '', ResStr_Date);
  RegisterControl(eAuthor, 'AuthorName', 'Author_Name_Key', CheckLinkedIndividual,
                  'Name', ConvertIndividualKeyToCaption);
  eAuthor.OnGetData := eAuthorGetData;

  RegisterControl(mmComments, 'Comment');
end;  // TfraRelated.RegisterControls

{-------------------------------------------------------------------------------
}
procedure TfraRelated.RegisterDragDropComponents;
begin
  RegisterDropComponent(eAuthor, DropAuthor, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
  case FTopNodeContext of
    ncSpecimen:   RegisterDropComponent(eName, DropName, [TN_SPECIMEN_UNIT], [CF_JNCCDATA]);
    ncCollection: RegisterDropComponent(eName, DropName, [TN_COLLECTION], [CF_JNCCDATA]);
    ncStore:      RegisterDropComponent(eName, DropName, [TN_STORE], [CF_JNCCDATA]);
  else
    raise EBrowserFrameError.Create(ResStr_InvalidNodeContext);
  end;
end;  // TfraRelated.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraRelated.SaveData;
var
  lParams: Array of Variant;
  lFromKey: TKeyString;
  lToKey: TKeyString;
begin
  inherited;  // For validation to be called on a TBaseFullScreenFrame frame
  // Depending on whether the forward or reverse version of the relation has
  // been selected, determines the order the current key and the parent key
  // are stored in the Collection_Unit_Relation table.
  if FDirectionList.Values['F' + cmbType.CurrentStrID] = cmbType.Text then begin
    lFromKey := ParentKey;
    lToKey   := eName.Key;
  end else begin
    lFromKey := eName.Key;
    lToKey   := ParentKey;
  end;

  lParams := VarArrayOf(['@Key', Key,
                         '@FromCollectionUnitKey', lFromKey,
                         '@ToCollectionUnitKey', lToKey,
                         '@ThesaurusRelationTypeKey', cmbType.CurrentStrID,
                         '@InferredType', InferredCaptionToValue(btnTypeInferred.Caption),
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@AuthorNameKey', eAuthor.Key,
                         '@Comment', mmComments.Text,
                         '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT_RELATION,
                                                  'usp_CollectionUnitRelation_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_CollectionUnitRelation_Update', lParams);
end;  // TfraRelated.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraRelated.UpdateAuthor(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eAuthor, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblAuthor.Caption)]));
end;  // TfraRelated.UpdateAuthor

{-------------------------------------------------------------------------------
}
procedure TfraRelated.ValidateData;
var
  lRecursionPassed: Boolean;
begin
  inherited;
  lRecursionPassed := (eName.Key <> AdditionalProperties.GetProperty(PROP_PARENT_KEY));

  ValidateValue(lRecursionPassed,
                Format(ResStr_RecursionFailureCannotBeLinkedToItself, [ResStr_CollectionUnit]),
                eName);
end;  // TfraRelated.ValidateData

end.

