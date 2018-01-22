{===============================================================================
  Unit:        FrameEnquiryGeneral.pas

  Defines:     TfraEnquiryGeneral

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 19 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameEnquiryGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  InterfaceDataModule, BaseCompositeComponent, LinkedControls, ComboListID,
  LuxIDComboBox, ConceptGroupComboBox, DataTypes, VagueDateEdit, DataClasses,
  Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page control for the main details of an enquiry.
  }
  TfraEnquiryGeneral = class (TBaseTabSheetFrame)
    chkAnswered: TCheckBox;
    chkMaterialLeft: TCheckBox;
    chkObservationRequired: TCheckBox;
    cmbMethod: TConceptGroupComboBox;
    cmbType: TConceptGroupComboBox;
    eAnsweredBy: TUserEdit;
    eDate: TVagueDateEdit;
    eDateAnswered: TVagueDateEdit;
    eName: TUserEdit;
    eOtherName: TEdit;
    gbAnswered: TGroupBox;
    gbEnquiry: TGroupBox;
    lblAnsweredBy: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblAnsweredDate: TLabel;
    lblDate: TLabel;
    lblDept: TLabel;
    lblLinkedName: TLabel;
    lblOtherName: TLabel;
    mmDescription: TMemo;
    procedure cmbTypeChange(Sender: TObject);
    procedure eAnsweredByGetData(Sender: TObject);
    procedure eNameGetData(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure DropAnsweredBy(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateAnsweredBy(const AKeyList: IKeyList);
    procedure UpdateLinkedName(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses GeneralFunctions, LuxembourgConstants, Validation, GeneralData,
     ApplicationSettings, DropTarget, ExceptionForm, ResourceStrings,
     VagueDate, BaseADODataModule;

{-==============================================================================
    TfraEnquiryGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  If the selected item in cmbType is Identification, hide two combo boxes. 
}
procedure TfraEnquiryGeneral.cmbTypeChange(Sender: TObject);
begin
  inherited;
  if cmbType.CurrentStrID = 'SYSTEM0000001YEI' then begin
    chkMaterialLeft.Visible         := False;
    chkObservationRequired.Visible  := False;
  end else begin
    chkMaterialLeft.Visible         := True;
    chkObservationRequired.Visible  := True;
  end
end;  // TfraEnquiryGeneral.cmbTypeChange 

{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame.
}
procedure TfraEnquiryGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Enquiry_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraEnquiryGeneral.DeleteData

{-------------------------------------------------------------------------------
  Handle the dropping of data onto eAnsweredBy.
}
procedure TfraEnquiryGeneral.DropAnsweredBy(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eAnsweredBy, TN_INDIVIDUAL,
      'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraEnquiryGeneral.DropAnsweredBy

{-------------------------------------------------------------------------------
  Handle the dropping of data onto eName.
}
procedure TfraEnquiryGeneral.DropName(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_INDIVIDUAL,
      'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraEnquiryGeneral.DropName

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryGeneral.eAnsweredByGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateAnsweredBy, TN_NAME);
end;  // TfraEnquiryGeneral.eAnsweredByGetData

{-------------------------------------------------------------------------------
  Enable/disable controls according to EditMode state.
}
procedure TfraEnquiryGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  // Handle sets of controls which require only one to be populated
  if AEnabled then
    eName.Color := MergeColours(AppSettings.MandatoryColour, clWindow, 25)
  else
    eName.Color := clWindow;
  eOtherName.Color := eName.Color;
end;  // TfraEnquiryGeneral.EnableControls

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryGeneral.eNameGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateLinkedName, TN_NAME);
end;  // TfraEnquiryGeneral.eNameGetData

{-------------------------------------------------------------------------------
  Return <date> - <type> as the node caption.
}
function TfraEnquiryGeneral.GetCaption: String;
begin
  Result := eDate.Text + ' - ' + cmbType.Text;
end;  // TfraEnquiryGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Store timestamps. 
}
procedure TfraEnquiryGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
  // Ensure the check boxes are (in)visible following a load.
  cmbTypeChange(Self);
end;  // TfraEnquiryGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraEnquiryGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Enquiry_Select');
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'Linked_Name', 'Enquirer_Name_Key',
                  CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  eName.OnGetData := eNameGetData;
  
  RegisterControl(eOtherName, 'Vague_Enquirer');
  RegisterControl(cmbType, 'TypeTerm', 'Enquiry_Type_Concept_Key', True, ResStr_EnquiryType);
  RegisterConceptGroupComboBox(cmbType, CG_ENQUIRY_TYPES);
  RegisterControl(cmbMethod, 'MethodTerm', 'Enquiry_Method_Concept_Key', True,
      ResStr_EnquiryMethod);
  RegisterConceptGroupComboBox(cmbMethod, CG_ENQUIRY_METHODS);
  RegisterControl(eDate, '', True, 'Date');
  RegisterControl(chkMaterialLeft, 'Material_Left');
  RegisterControl(chkObservationRequired, 'Observation_Planned');
  RegisterControl(mmDescription, 'Description');
  RegisterControl(eAnsweredBy, 'Answered_By', 'Answered_By_Name_Key',
                 CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  eAnsweredBy.OnGetData := eAnsweredByGetData;
  
  RegisterControl(chkAnswered, 'Answered');
  RegisterControl(eDateAnswered, 'Answered');
  
  // Register asynchronously loaded controls
  RegisterAsyncControl(lblDept, 'usp_Department_Get_ForIndividual',
                         ['@Key', 'Answered_By_Name_Key'], '@Output');
end;  // TfraEnquiryGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register eName and eAnsweredBy for drag and drop. 
}
procedure TfraEnquiryGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eName, DropName, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
  RegisterDropComponent(eAnsweredBy, DropAnsweredBy, [TN_NAME, TN_INDIVIDUAL],
      [CF_JNCCDATA]);
end;  // TfraEnquiryGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the data to the database. 
}
procedure TfraEnquiryGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@EnquirerNameKey', eName.Key,
                         '@VagueEnquirer', eOtherName.Text,
                         '@EnquiryTypeConceptKey', cmbType.CurrentStrID,
                         '@EnquiryMethodConceptKey', cmbMethod.CurrentStrID,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@MaterialLeft', chkMaterialLeft.Checked,
                         '@ObservationPlanned', chkObservationRequired.Checked,
                         '@Description', mmDescription.Text,
                         '@AnsweredByNameKey', eAnsweredBy.Key,
                         '@Answered', chkAnswered.Checked,
                         '@AnsweredVagueDateStart', eDateAnswered.VagueDate.StartDate,
                         '@AnsweredVagueDateEnd', eDateAnswered.VagueDate.EndDate,
                         '@AnsweredVagueDateType', eDateAnswered.VagueDate.DateTypeString,
                         '@Response', '',
                         '@Timestamp', FTimestamp
                         ]);

  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_ENQUIRY, 'usp_Enquiry_Insert',
                                                  lParams, '@Key'))
  else
      dmGeneral.RunUpdateStoredProc('usp_Enquiry_Update', lParams);
end;  // TfraEnquiryGeneral.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryGeneral.UpdateAnsweredBy(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eAnsweredBy, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblAnsweredBy.Caption)]));
end;  // TfraEnquiryGeneral.UpdateAnsweredBy

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryGeneral.UpdateLinkedName(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eName, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblLinkedName.Caption)]));
end;  // TfraEnquiryGeneral.UpdateLinkedName

{-------------------------------------------------------------------------------
  Extra validation required to check dates are consistent.
}
procedure TfraEnquiryGeneral.ValidateData;
var
  lCurrentDate: TVagueDate;
begin
  inherited;
  lCurrentDate := StringToVagueDate(DateToStr(Date));

  // Use on screen labels to ensure error message is correct, minus the ':'
  ValidateValue((eName.Text <> '') or (eOtherName.Text <> ''),
      Format(ResStr_MissingDataForControls,
             [LopColon(lblLinkedName.Caption) + ', ' + LopColon(lblOtherName.Caption)]),
      eName);

  if eName.Key <> '' then
   ValidateValue(CheckIsIndividual(eName.Key),
                 Format(ResStr_MustBeIndividual, [LopColon(lblLinkedName.Caption)]));

  if eAnsweredBy.Key <> '' then
    ValidateValue(CheckIsIndividual(eAnsweredBy.Key),
                  Format(ResStr_MustBeIndividual, [LopColon(lblAnsweredBy.Caption)]));
  // If the user enters something like '17c -' in eDate, we don't mind if
  // eDateAnswered is empty.
  try
    if eDate.VagueDate.EndDate <> 0 then
      ValidateValue(CompareVagueDateToVagueDate(eDate.VagueDate,
          eDateAnswered.VagueDate)<>1,
          Format(ResStr_DateMustBeAfterDate, [LopColon(lblAnsweredDate.Caption),
          LopColon(lblDate.Caption)]), eDateAnswered);
    ValidateValue(CompareVagueDateToVagueDate(lCurrentDate,
        eDate.VagueDate) <> -1, Format(ResStr_DateCannotBeInFuture,
        [ResStr_EnquiredDate]), eDate);
    ValidateValue(CompareVagueDateToVagueDate(lCurrentDate,
        eDateAnswered.VagueDate) <> -1, Format(ResStr_DateCannotBeInFuture,
        [ResStr_DateAnswered]), eDateAnswered);
  except
    // Catch a EVagueDateError if it occurs and display it in a friendlier message.
    on E:EVagueDateError do
      raise EBrowserFrameError.CreateNonCritical(Format(ResStr_DateError, [E.Message]));
  end;
end;  // TfraEnquiryGeneral.ValidateData

end.
