{===============================================================================
  Unit:        FrameProcessGeneral

  Defines:     TfraProcessGeneral

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:    May 2003

  Last revision information:
    $Revision: 16 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameProcessGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  DataTypes, ComboListID, LuxIDComboBox, ConceptGroupComboBox, BaseCompositeComponent,
  LinkedControls, VagueDateEdit, DataClasses, InterfaceDataModule, ResourceStrings,
  Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page control for the details of a fabrication process for a specimen or a 
    store.
  }
  TfraProcessGeneral = class (TBaseTabSheetFrame)
    btnDateInferred: TImageListButton;
    btnProcessDescriptionInferred: TImageListButton;
    btnProcessInferred: TImageListButton;
    btnProcessPersonInferred: TImageListButton;
    cmbProcess: TConceptGroupComboBox;
    eDate: TVagueDateEdit;
    ePerson: TUserEdit;
    Label1: TLabel;
    Label10: TLabel;
    lblPerson: TLabel;
    Label6: TLabel;
    mmDescription: TMemo;
    procedure cmbProcessKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbProcessPopulate(Sender: TObject);
    procedure ePersonGetData(Sender: TObject);
    procedure InferenceClick(Sender: TObject);
  private
    FConceptGroupName: String;
    FLast10Concepts: TStringList;
    FPersistentListName: String;
    FTimestamp: TSqlSvrTimestamp;
    procedure ClearInferredButtons;
    procedure DropPerson(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function InferredCaptionToValue(ACaption: String): ShortInt;
    procedure InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
    procedure UpdateComboBox;
    procedure UpdatePerson(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure SetMasterFrameType(Value: TMasterFrameType); override;
    procedure ValidateData; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Last10Concepts: TStringList read FLast10Concepts write FLast10Concepts;
  end;
  
//==============================================================================
implementation

uses
  Validation, DropTarget, GeneralData, LuxembourgConstants, ExceptionForm,
  GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TfraProcessGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Ensure correct initialisation of class. 
}
constructor TfraProcessGeneral.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  // override default behaviour of LuxIDComboBox so it can be typed in to.
  cmbProcess.Style := csDropDown;
  
  FLast10Concepts := TStringList.Create;
end;  // TfraProcessGeneral.Create 

{-------------------------------------------------------------------------------
  Free the string list. 
}
destructor TfraProcessGeneral.Destroy;
begin
  FLast10Concepts.Free;
  
  inherited Destroy;
end;  // TfraProcessGeneral.Destroy 

{-------------------------------------------------------------------------------
  Reset the inferred buttons. 
}
procedure TfraProcessGeneral.ClearInferredButtons;
begin
  InferredValueToButtonImage(btnProcessInferred, 0);
  InferredValueToButtonImage(btnProcessDescriptionInferred, 0);
  InferredValueToButtonImage(btnProcessPersonInferred, 0);
  InferredValueToButtonImage(btnDateInferred, 0);
end;  // TfraProcessGeneral.ClearInferredButtons 

{-------------------------------------------------------------------------------
  See if return has been pressed. 
}
procedure TfraProcessGeneral.cmbProcessKeyDown(Sender: TObject; var Key: Word; Shift: 
    TShiftState);
begin
  inherited;
  
  if (Key = VK_RETURN) then begin
    if not CheckConceptGroupProcess(cmbProcess, cmbProcess.text,
                FConceptGroupName) then
      Key := 0
    else
      UpdateComboBox;
  end;
end;  // TfraProcessGeneral.cmbProcessKeyDown 

{-------------------------------------------------------------------------------
  Populate the combo box with the 10 last used concepts. 
}
procedure TfraProcessGeneral.cmbProcessPopulate(Sender: TObject);
begin
  PopulateLast10ConceptsCombo(cmbProcess, FConceptGroupName, FPersistentListName);
end;  // TfraProcessGeneral.cmbProcessPopulate 

{-------------------------------------------------------------------------------
  Delete the record used in the current frame. 
}
procedure TfraProcessGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_CollectionUnitProcess_Delete',
                                ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraProcessGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle Individual data dropped onto ePerson. 
}
procedure TfraProcessGeneral.DropPerson(const Sender: TObject; const AFormat: Integer; const 
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, ePerson, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraProcessGeneral.DropPerson

{-------------------------------------------------------------------------------
}
procedure TfraProcessGeneral.ePersonGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdatePerson, TN_NAME);
end;  // TfraProcessGeneral.ePersonGetData

{-------------------------------------------------------------------------------
  Return <Process term> as the caption. 
}
function TfraProcessGeneral.GetCaption: String;
begin
  Result := cmbProcess.Text;
end;  // TfraProcessGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Handle a click on one of the inferred buttons. 
}
procedure TfraProcessGeneral.InferenceClick(Sender: TObject);
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
end;  // TfraProcessGeneral.InferenceClick 

{-------------------------------------------------------------------------------
  Change the caption of the inferred button to a value. 
}
function TfraProcessGeneral.InferredCaptionToValue(ACaption: String): ShortInt;
begin
  if      ACaption = ''   then Result := 0
  else if ACaption = '!'  then Result := 1
  else if ACaption = '?'  then Result := 2
  else if ACaption = '!?' then Result := 3
  else
    raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
end;  // TfraProcessGeneral.InferredCaptionToValue 

{-------------------------------------------------------------------------------
  Convert the value of the inferred button to an image. 
}
procedure TfraProcessGeneral.InferredValueToButtonImage(AButton: TImageListButton; AValue: 
    Integer);
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
end;  // TfraProcessGeneral.InferredValueToButtonImage 

{-------------------------------------------------------------------------------
  Store the timestamp and set up the inferred buttons. 
}
procedure TfraProcessGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    with RegisteredRecordsets[0] do begin
      FTimestamp := Fields['Timestamp'].Value;
      // Initialise the Inferred fields.
      InferredValueToButtonImage(btnProcessInferred, Fields['Inferred_Process'].Value);
      InferredValueToButtonImage(btnProcessPersonInferred, Fields['Inferred_Person'].Value);
      InferredValueToButtonImage(btnProcessDescriptionInferred, 
          Fields['Inferred_Description'].Value);
      InferredValueToButtonImage(btnDateInferred, Fields['Inferred_Date'].Value);
    end;
  end
  else
    ClearInferredButtons;
end;  // TfraProcessGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and controls for populating the tab 
}
procedure TfraProcessGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnitProcess_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(cmbProcess, 'ProcessTerm', 'Process_Concept_Key', true, ResStr_Process);
  RegisterConceptGroupComboBox(cmbProcess, FConceptGroupName);
  RegisterControl(mmDescription, 'Description');
  RegisterControl(ePerson, 'Person', 'Name_Key', CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  ePerson.OnGetData := ePersonGetData;
  
  RegisterControl(eDate, '');
end;  // TfraProcessGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register ePerson to accept dropped Individual data. 
}
procedure TfraProcessGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(ePerson, DropPerson, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraProcessGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Saves the data to the database. 
}
procedure TfraProcessGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  SaveLast10ConceptsCombo(cmbProcess, FConceptGroupName, FPersistentListName);
  lParams := VarArrayOf(['@Key', Key,
                         '@CollectionUnitKey', ParentKey,
                         '@ProcessConceptKey', cmbProcess.CurrentStrID,
                         '@Description', mmDescription.Text,
                         '@NameKey', ePerson.Key,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@InferredProcess', InferredCaptionToValue(
                              btnProcessInferred.Caption),
                         '@InferredDescription', InferredCaptionToValue(
                              btnProcessDescriptionInferred.Caption),
                         '@InferredPerson', InferredCaptionToValue(
                              btnProcessPersonInferred.Caption),
                         '@InferredDate', InferredCaptionToValue(btnDateInferred.Caption),
                         '@Timestamp', FTimestamp
                         ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT_PROCESS,
                                                  'usp_CollectionUnitProcess_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_CollectionUnitProcess_Update', lParams);
end;  // TfraProcessGeneral.SaveData 

{-------------------------------------------------------------------------------
  Sets the FPersistentListName and FConceptGroupName variables prior to registering the 
      recordset because the frame can be used with different collection_units and different 
      concept groups and persistent lists need to be used depending on this. 
}
procedure TfraProcessGeneral.SetMasterFrameType(Value: TMasterFrameType);
begin
  inherited;
  case MasterFrameType of
    mftCollection:
        begin
          FConceptGroupName := CG_COLLECTION_PROCESSES;
          FPersistentListName := PL_COLLECTION_PROCESSES;
        end;
  
    mftSpecimen:
        begin
          FConceptGroupName := CG_SPECIMEN_PROCESSES;
          FPersistentListName := PL_SPECIMEN_PROCESSES;
        end;
  
    mftStore:
        begin
          FConceptGroupName := CG_STORE_PROCESSES;
          FPersistentListName := PL_STORE_PROCESSES;
        end;
  end;
end;  // TfraProcessGeneral.SetMasterFrameType 

{-------------------------------------------------------------------------------
  Updates the combo box. 
}
procedure TfraProcessGeneral.UpdateComboBox;
var
  i: Integer;
begin
  for i := 0 to Last10Concepts.Count - 1 do
    if cmbProcess.IndexOf(Last10Concepts[i]) = -1 then
      cmbProcess.Add(Last10Concepts[i],TKeyData(Last10Concepts.Objects[i]).ItemKey);
end;  // TfraProcessGeneral.UpdateComboBox

{-------------------------------------------------------------------------------
}
procedure TfraProcessGeneral.UpdatePerson(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(ePerson, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblPerson.Caption)]));
end;  // TfraProcessGeneral.UpdatePerson

{-------------------------------------------------------------------------------
  Method that validates the name in the person field is an individual. 
}
procedure TfraProcessGeneral.ValidateData;
begin
  inherited;
  if ePerson.Key <> '' then
    ValidateValue(CheckIsIndividual(ePerson.Key),
                  Format(ResStr_MustBeIndividual, [ResStr_Person]));
end;  // TfraProcessGeneral.ValidateData 

end.

