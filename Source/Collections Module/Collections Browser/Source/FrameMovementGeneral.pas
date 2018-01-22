{===============================================================================
  Unit:        FrameMovementGeneral.pas

  Defines:     TfraMovementGeneral

  Description: General details about a movement

  Created:     June 2003

  Last revision information:
    $Revision: 46 $
    $Date: 11/04/14 9:11 $
    $Author: Brynhorsfieldschonhut $

===============================================================================}

unit FrameMovementGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  InterfaceDataModule, VagueDateEdit, BaseCompositeComponent, LinkedControls,
  ResourceStrings, DataClasses, Validation, DataTypes, GeneralData, DropTarget,
  Recorder2000_TLB, SearchManager, UserEdit, DSSDataTypes;

type
  {-----------------------------------------------------------------------------
    General tab page to display the top level details of any movement.  This includes loans,
    accessions and other movements.
    When entering the details of a new movement and the Save button is clicked, it is
    necessary to force the user to enter details for the subcomponents of the movement
    before the data is saved to the database.  For example, if the user creates an exchange,
    then the user must enter accession, acquisition, transfer of ownership and disposal
    details before the exchange is saved.  In this instance, after the Save button is
    clicked each sub-node that contains mandatory fields is displayed in turn.  The
    relevant details page is displayed in edit mode allowing the user to enter and save the
    required detail.  As each sub-node details page is displayed, a message is displayed
    'Please enter details for the ' + the name of the sub-node.  When the user presses the
    Save button then the next required sub-node is displayed, until all required data is
    gathered.  Pressing Save on the last sub-node page svaes the data to the database and
    the browser returns to the main movement top level node.  Pressing Cancel at any time
    during this process displays a message 'This will cancel all the data entered so far
    for the ' + movement type + '.  Are you sure you wish to proceed?' with Yes and No
    option buttons.  If the user selects Yes then all data entered is lost.  If the user
    selects No, then the Cancel operation is aborted.
    The same principle applies when saving TfraMovementGeneral for a movement newly created
    within a folder inside another top-level node, except that the sub-nodes are never
    visible in the hierarchy.
  }
  TfraMovementGeneral = class(TBaseTabSheetFrame)
    eCompletionDate: TVagueDateEdit;
    eContact: TUserEdit;
    eName: TUserEdit;
    eNumber: TEdit;
    eOtherParty: TUserEdit;
    gbSummary: TGroupBox;
    lblCompletionDate: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCollections: TLabel;
    lblContact: TLabel;
    lblNumber: TLabel;
    lblOtherParty: TLabel;
    lblSpecimens: TLabel;
    lblStaff: TLabel;
    lblStatus: TLabel;
    lblStorage: TLabel;
    mmNotes: TMemo;
    pnlOtherParty: TPanel;
    lblLoanDate: TLabel;
    lblActualReturnDate: TLabel;
    chkLoanComplete: TCheckBox;
    chkReturnComplete: TCheckBox;
    eActualReturnDate: TVagueDateEdit;
    eLoanDate: TVagueDateEdit;
    procedure eContactFindData(Sender: TObject);
    procedure eContactGetData(Sender: TObject);
    procedure eNameGetData(Sender: TObject);
    procedure eOtherPartyChange(Sender: TObject);
    procedure eOtherPartyFindData(Sender: TObject);
    procedure eOtherPartyGetData(Sender: TObject);
    procedure eNameFindData(Sender: TObject);
    procedure chkReturnCompleteClick(Sender: TObject);
    procedure eActualReturnDateExit(Sender: TObject);
    //procedure GetCurrentUserF11(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FMovementType: ShortInt;
    FNumber: String;
    FOtherPartyIsIndividual: Boolean;
    FTimestamp: TSQLSvrTimestamp;
    FWithAcquisition: Boolean;
    FOldNameKey: string;
    procedure CheckMacroNumber;
    procedure DropContact(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropName(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropOtherParty(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function GetHoldingOrgName: String;
    function GetOtherPartyCaption(AMovementType: Integer): String;
    function IsMemberOfHoldingOrg(const AKey :TKeyString): Boolean;
    //function GetCurrentUserParams: TVariantArray;
    procedure UpdateContact(const AKeyList: IKeyList);
    procedure UpdateName(const AKeyList: IKeyList);
    procedure UpdateOtherParty(const AKeyList: IKeyList);
    function ValidateContact(const AKey: TKeyString): Boolean;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure ValidateData; override;
  public
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  LuxembourgConstants, ExceptionForm, BaseADODataModule, StrUtils,
  GeneralFunctions, ApplicationSettings, BaseDetailFrameUnit;

ResourceString
  FstAccNumber = 'Acc. Number:';
  FstRefNumber = 'Ref. Number:';

{-==============================================================================
    TfraMovementGeneral
===============================================================================}

{-------------------------------------------------------------------------------
  If the user updates the staff responsible, check if the macro needs to be
     updated (i.e. if we now know the dept but did not before
}
procedure TfraMovementGeneral.CheckMacroNumber;
var
  lDepartmentName: string;
begin
  if (Pos('#' + ResStr_Unknown + '#', FNumber)>0) and (eName.Key <> FOldNameKey) then begin
    lDepartmentName := VarToStr(dmGeneral.GetStoredProcOutputParam
                                      ('usp_Department_Get_ForIndividual',
                                      ['@Key', eName.Key,
                                       '@GetAcronym', 1],
                                      '@Output'));
    if lDepartmentName <> '' then
      eNumber.Text := StringReplace(FNumber, '#' + ResStr_Unknown + '#',
          lDepartmentName, [])
    else
      eNumber.Text := FNumber; // reset dept to unknown
    FOldNameKey := eName.Key;
  end; // if
end;

{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame.
}
procedure TfraMovementGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Movement_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraMovementGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle a drop onto the Contact linked edit. 
}
procedure TfraMovementGeneral.DropContact(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  if not (ASourceData.Header.ItemCount > 0) then Exit;
  
  DropLinkedEditData(AFormat, ASourceData, AHandled, eContact, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
  
  if not ValidateContact(eContact.Key) then begin
    MessageDlg(Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
               [eContact.Text, eOtherParty.Text]), mtInformation, [mbOK], 0);
    if eContact.CanFocus then eContact.SetFocus;
  end;
end;  // TfraMovementGeneral.DropContact 

{-------------------------------------------------------------------------------
  Handle a drop onto the Name linked edit. 
}
procedure TfraMovementGeneral.DropName(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eName, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
  CheckMacroNumber;                     
end;  // TfraMovementGeneral.DropName 

{-------------------------------------------------------------------------------
  Handle a drop onto the OtherParty linked edit. 
}
procedure TfraMovementGeneral.DropOtherParty(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eOtherParty,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
  
  if not ValidateContact(eContact.Key) then begin
    MessageDlg(Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
               [eContact.Text, eOtherParty.Text]), mtInformation, [mbOK], 0);
    if eContact.CanFocus then eContact.SetFocus;
  end;
end;  // TfraMovementGeneral.DropOtherParty 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eContactFindData(Sender: TObject);
begin
  inherited;
  CheckLinkedIndividual(eContact);
  ValidateValue(ValidateContact(eContact.Key),
                Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
                [eContact.Text, eOtherParty.Text]), eContact);
end;  // TfraMovementGeneral.eContactFindData 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eContactGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateContact, TN_NAME);
end;  // TfraMovementGeneral.eContactGetData

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [eOtherParty]);
end;  // TfraMovementGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eNameGetData(Sender: TObject);

begin
  inherited;
  //MessageDlg('Get', mtInformation, [mbOK], 0);
  InitReturnData(UpdateName, TN_NAME);
end;  // TfraMovementGeneral.eNameGetData 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eOtherPartyChange(Sender: TObject);
var
  lKey, lText: String;
begin
  inherited;
  lKey := eOtherParty.Key;
  lText := eOtherParty.Text;
  DoCheckUnique(lKey, lText, stPeopleOrganisation);
  if lKey <> '' then begin
    eOtherParty.Text := lText;
    FOtherPartyIsIndividual := CheckIsIndividual(lKey);
    eContact.Visible := not FOtherPartyIsIndividual;
    if FOtherPartyIsIndividual then begin
      eContact.Text := '';
      eContact.Key := '';
    end;
  end else
    eContact.Visible := True;
  lblContact.Visible := eContact.Visible;
end;  // TfraMovementGeneral.eOtherPartyChange 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eOtherPartyFindData(Sender: TObject);
begin
  inherited;
  CheckLinkedName(eOtherParty);
  
  if not ValidateContact(eContact.Key) then begin
    MessageDlg(Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
               [eContact.Text, eOtherParty.Text]), mtInformation, [mbOK], 0);
    if eContact.CanFocus then eContact.SetFocus;
  end;
end;  // TfraMovementGeneral.eOtherPartyFindData 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eOtherPartyGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateOtherParty, TN_NAME);
end;  // TfraMovementGeneral.eOtherPartyGetData 

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraMovementGeneral.GetCaption: String;
begin
  case FMovementType of
    0 : Result := ResStr_Accession;
    1 : Result := ResStr_Exchange;
    2 : Result := ResStr_LoanIn;
    3 : Result := ResStr_LoanOut;
    4 : Result := ResStr_Destroyed;
    5 : Result := ResStr_Disposed;
    6 : Result := ResStr_InternalTransfer;
    7 : Result := ResStr_Lost;
    8 : Result := ResStr_Sold;
    9 : Result := ResStr_HostedMaterial;
  end;
  
  case FMovementType of
    0, 1: Result := Result + ' (' + eNumber.Text + ') - ' + eCompletionDate.Text;
    2, 3: Result := Result + ' - ' + eOtherParty.Text + ' - ' + eCompletionDate.Text;
  else
    Result := Result + ' - ' + eCompletionDate.Text;
  end;
end;  // TfraMovementGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Get the name of the holding organisation from the database for the validation message. 
}
function TfraMovementGeneral.GetHoldingOrgName: String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_HoldingOrgName_Get', [],
                                                        '@HoldingOrg'));
end;  // TfraMovementGeneral.GetHoldingOrgName 

{-------------------------------------------------------------------------------
  Get the caption for lblOtherParty. 
}
function TfraMovementGeneral.GetOtherPartyCaption(AMovementType: Integer): String;
begin
  case AMovementType of
    0 : Result := ResStr_AccessionedFrom;
    1 : Result := ResStr_ExchangedWith;
    2 : Result := ResStr_Lender;
    3 : Result := ResStr_Borrower;
    5 : Result := ResStr_DisposedTo;
    8 : Result := ResStr_SoldTo;
    9 : Result := ResStr_Owner;
  else
    Result := ResStr_OtherParty;
  end;
  Result := Result + ':';
end;  // TfraMovementGeneral.GetOtherPartyCaption 

{-------------------------------------------------------------------------------
}
function TfraMovementGeneral.IsMemberOfHoldingOrg(const AKey :TKeyString): Boolean;
begin
  Result := (dmGeneral.GetStoredProcOutputParam('usp_MemberOfHoldingOrg_Get',
                                                ['@Key', AKey], '@IsMember') <> 0);
end;  // TfraMovementGeneral.IsMemberOfHoldingOrg

{-------------------------------------------------------------------------------
  Load the timestamp and the movement type.
}
procedure TfraMovementGeneral.LoadData;
var
  lNumberTypeString, lDepartmentName: String;
begin
  inherited LoadData;
  FOldNameKey := '';
  FMovementType := AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE);

  with RegisteredRecordsets[0] do
    if not Eof then begin
     FTimestamp               := Fields['Timestamp'].Value;
     FOtherPartyIsIndividual  := (Fields['OtherPartyIsOrganisation'].Value = False);
     FNumber                  := Fields['Number'].Value;
    end else begin
      // Generate a number using the macro.
      if FMovementType in [0, 1] then lNumberTypeString := 'Accession'
      else if FMovementType in [2, 3] then lNumberTypeString := 'Loan'
      else lNumberTypeString := 'Movement';
      // Department name is currently unknown, since we don't know the staff responsible
      if lDepartmentName = '' then lDepartmentName := '#' + ResStr_Unknown + '#';
      // Use the macro to generate a number.
      FNumber := dmGeneral.GetStoredProcOutputParam('usp_Macro_Get',
                                      ['@NumberType', lNumberTypeString,
                                       '@Dept', lDepartmentName],
                                      '@Macro');
      eNumber.Text := FNumber;
    end;
  
  // eOtherParty and eContact shouldn't be visible for 'Internal Transfer',
  // 'Lost' or 'Destroyed' movement types.
  eOtherParty.Visible   := not (FMovementType in [4, 6, 7]);
  lblOtherParty.Caption := GetOtherPartyCaption(FMovementType);
  lblOtherParty.Visible := not (FMovementType in [4, 6, 7]);
  eContact.Visible      := not ((FMovementType in [4, 6, 7]) or FOtherPartyIsIndividual);
  lblContact.Visible    := not ((FMovementType in [4, 6, 7]) or FOtherPartyIsIndividual);
  lblLoanDate.Visible   := FMovementType in [2, 3];
  eLoanDate.Visible     := FMovementType in [2, 3];
  chkLoanComplete.Visible := FMovementType in [2, 3];
  chkReturnComplete.Visible := FMovementType in [2, 3];
  lblActualReturnDate.Visible := (FMovementType in [2, 3]) and chkReturnComplete.Checked;
  eActualReturnDate.Visible := (FMovementType in [2, 3]) and chkReturnComplete.Checked;

  if not eActualReturnDate.Visible then
    eActualReturnDate.Text := eCompletionDate.Text;
  
  // The caption of the Number label needs to be set depending on the
  // movement type.
  if FMovementType in [0, 1] then
    lblNumber.Caption := 'Acc. Number'
  else
    lblNumber.Caption := 'Ref. Number';

  if FMovementType in [2, 3] then
    lblCompletionDate.Caption := 'Expected Return Date:'
  else
    lblCompletionDate.Caption := 'Expected Completion:';

  // Hack to make the tooltip appear above the status label.
  Label2.Width := 180;
  Label2.Transparent := True;
  Label2.BringToFront;
end;  // TfraMovementGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraMovementGeneral.RegisterControls;
begin
  inherited;

  FMovementType := AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE);
  
  // Register recordsets used
  RegisterRecordset('usp_Movement_Select');

  RegisterControl(eName, 'Staff_Responsible_Name', 'Staff_Responsible_Name_Key', True,
                  ResStr_StaffResponsible, CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  eName.OnGetData := eNameGetData;
  eName.OnFindData := eNameFindData;
  //eName.OnKeyDown := GetCurrentUserF11;

  RegisterControl(eOtherParty, 'Other_Party_Name', 'Other_Party_Name_Key',
                  CheckLinkedName, 'Name', ConvertNameKeyToCaption);
  eOtherParty.OnGetData := eOtherPartyGetData;
  eOtherParty.OnFindData := eOtherPartyFindData;

  RegisterControl(eContact, 'Contact_Name', 'Contact_Name_Key', False,
                  ResStr_Contact, CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  eContact.OnGetData := eContactGetData;
  eContact.OnFindData := eContactFindData;

  RegisterControl(eNumber, 'Number', True, ResStr_Number);

  if (FMovementType = 2) Or (FMovementType = 3) then
  begin
    RegisterControl(eCompletionDate, 'Exp', True, 'Expected Return Date');
    RegisterControl(chkLoanComplete, 'LoanComplete');
    RegisterControl(chkReturnComplete, 'ReturnComplete');
    RegisterControl(eLoanDate, 'LoanDate', True, 'Loan Date');
    RegisterControl(eActualReturnDate, 'ActualReturnDate', chkReturnComplete.Checked, 'Actual Return Date');
    eActualReturnDate.OnExit := eActualReturnDateExit;
    
  end
  else
  begin
      RegisterControl(eCompletionDate, 'Exp', True, ResStr_ExpectedCompletionDate);
  end;


  RegisterControl(mmNotes, 'Notes');
  
  // Register asynchronously loaded controls
  RegisterAsyncControl(lblCollections, 'usp_Movement_MaterialSummary_Get',
                       ['@Key', 'Movement_Key','@Category','CollectionIndex'], '@Count');
  RegisterAsyncControl(lblSpecimens, 'usp_Movement_MaterialSummary_Get',
                       ['@Key', 'Movement_Key','@Category','SpecimenIndex'], '@Count');
  RegisterAsyncControl(lblStorage, 'usp_Movement_MaterialSummary_Get',
                       ['@Key', 'Movement_Key','@Category','StoreIndex'], '@Count');
  RegisterAsyncControl(lblStatus, 'usp_Movement_Status_Get',
                       ['@Key', 'Movement_Key'], '@Status');
end;  // TfraMovementGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TfraMovementGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eName, DropName, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
  RegisterDropComponent(eOtherParty, DropOtherParty,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA]);
  RegisterDropComponent(eContact, DropContact, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraMovementGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Save the data to the database.
}
procedure TfraMovementGeneral.SaveData;
var
  lParams: Array Of Variant;
begin
  // for new records, retrieve the movement type from the AdditionalProperties
  if Key = '' then begin
    FMovementType := AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE);
    FWithAcquisition := AdditionalProperties.GetProperty(PROP_WITH_ACQUISITION);
  end;

  if chkReturnComplete.Checked then
    lParams := VarArrayOf(['@Key', Key,
                           '@MovementType', FMovementType,
                           '@WithAcquisition', FWithAcquisition,
                           '@StaffResponsibleNameKey', eName.Key,
                           '@OtherPartyNameKey', eOtherParty.Key,
                           '@ContactNameKey', eContact.Key,
                           '@VagueDateStart', eCompletionDate.VagueDate.StartDate,
                           '@VagueDateEnd', eCompletionDate.VagueDate.EndDate,
                           '@VagueDateType', eCompletionDate.VagueDate.DateTypeString,
                           '@LoanVagueDateStart', eLoanDate.VagueDate.StartDate,
                           '@LoanVagueDateEnd', eLoanDate.VagueDate.EndDate,
                           '@LoanVagueDateType', eLoanDate.VagueDate.DateTypeString,
                           '@LoanComplete', chkLoanComplete.Checked,
                           '@ReturnVagueDateStart', eActualReturnDate.VagueDate.StartDate,
                           '@ReturnVagueDateEnd', eActualReturnDate.VagueDate.EndDate,
                           '@ReturnVagueDateType', eActualReturnDate.VagueDate.DateTypeString,
                           '@ReturnComplete', chkReturnComplete.Checked,
                           '@Number', eNumber.Text,
                           '@Notes', mmNotes.Text,
                           '@Timestamp', FTimestamp
                           ])
  else
    lParams := VarArrayOf(['@Key', Key,
                           '@MovementType', FMovementType,
                           '@WithAcquisition', FWithAcquisition,
                           '@StaffResponsibleNameKey', eName.Key,
                           '@OtherPartyNameKey', eOtherParty.Key,
                           '@ContactNameKey', eContact.Key,
                           '@VagueDateStart', eCompletionDate.VagueDate.StartDate,
                           '@VagueDateEnd', eCompletionDate.VagueDate.EndDate,
                           '@VagueDateType', eCompletionDate.VagueDate.DateTypeString,
                           '@LoanVagueDateStart', eLoanDate.VagueDate.StartDate,
                           '@LoanVagueDateEnd', eLoanDate.VagueDate.EndDate,
                           '@LoanVagueDateType', eLoanDate.VagueDate.DateTypeString,
                           '@LoanComplete', chkLoanComplete.Checked,
                           '@ReturnComplete', chkReturnComplete.Checked,
                           '@Number', eNumber.Text,
                           '@Notes', mmNotes.Text,
                           '@Timestamp', FTimestamp
                           ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT, 'usp_Movement_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Movement_Update', lParams);
end;  // TfraMovementGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.UpdateContact(const AKeyList: IKeyList);
begin
  if UpdateIndividualNameControl(eContact, AKeyList.GetKeyItem(0).KeyField1,
         Format(ResStr_MustBeIndividual, [LopColon(lblContact.Caption)])) then
    if not ValidateContact(eContact.Key) then begin
      MessageDlg(Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
                 [eContact.Text, eOtherParty.Text]), mtInformation, [mbOK], 0);
      if eContact.CanFocus then eContact.SetFocus;
    end;
end;  // TfraMovementGeneral.UpdateContact

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.UpdateName(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eName, AKeyList.GetKeyItem(0).KeyField1,
                              Format(ResStr_MustBeIndividual, [LopColon(lblStaff.Caption)]));
  CheckMacroNumber;
end;  // TfraMovementGeneral.UpdateName

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.UpdateOtherParty(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  lKey := AKeyList.GetKeyItem(0).KeyField1;
  eOtherParty.Key := lKey;
  eOtherParty.Text := ConvertNameKeyToCaption(lKey, TN_INDIVIDUAL);
  if not ValidateContact(eContact.Key) then begin
    MessageDlg(Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
               [eContact.Text, eOtherParty.Text]), mtInformation, [mbOK], 0);
    if eContact.CanFocus then eContact.SetFocus;
  end;
end;  // TfraMovementGeneral.UpdateOtherParty 

{-------------------------------------------------------------------------------
}
function TfraMovementGeneral.ValidateContact(const AKey: TKeyString): Boolean;
begin
  Result := false;
  if (AKey = '') or (eOtherParty.Key = '') then
    Result := true
  else
  if eOtherParty.Key <> '' then
    if not CheckIsIndividual(eOtherParty.Key) then
      if (dmGeneral.GetStoredProcOutputParam('usp_NameRelation_Exists_Get',
              ['@NameKey1', eOtherParty.Key, '@NameKey2', AKey], '@RelationExists') = true)
                  then
        Result := true;
  
  if Result then begin
    eContact.Key := AKey;
    eContact.Text := ConvertIndividualKeyToCaption(AKey, TN_INDIVIDUAL);
  end;
end;  // TfraMovementGeneral.ValidateContact 

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.ValidateData;
var
  lValidateContact, lOtherPartySameOrganisation, lMovementNumberExists: Boolean;
  lOtherParty, lCaption: String;
  lBelongsToHoldingOrg: Boolean;
begin
  inherited;
  ValidateValue(IsMemberOfHoldingOrg(eName.Key) = True,
                Format(ResStr_MustBeMemberOfOrg, [ResStr_StaffResponsible, GetHoldingOrgName]),
                eName);

  lOtherParty := LopColon(lblOtherParty.Caption);
  
  // If this movement is being added as a leaf node of a collection unit, check
  // that the item belongs to the holding organisation at the stated point in
  // time. Need to check ParentKey <> Key because records without parents
  // seem to have their parent key set to their key sometimes.
  if (ParentKey <> '') and (ParentKey <> Key) then begin
    lBelongsToHoldingOrg := dmGeneral.GetStoredProcOutputParam
              ('usp_CollectionUnit_OwnedByHoldingOrg_Get',
               ['@CollectionUnitKey', ParentKey,
                '@MovementKey', Key,
                '@NewMovementVagueDateEnd', eCompletionDate.VagueDate.EndDate],
               '@OwnedByHoldingOrg') = True;
    lCaption := RemoveSubStrings(AdditionalProperties.GetProperty(PROP_TOP_NODE_CAPTION),
                                 ['<i>', '</i>']);
    ValidateValue(lBelongsToHoldingOrg = True,
                  Format(ResStr_MovementCannotBeAdded, [lCaption]));
  end;
  
  if not (FMovementType in [4, 6, 7]) then begin
    if (FMovementType in [1, 5, 8, 9]) then
    begin
      ValidateValue(IsMemberOfHoldingOrg(eOtherParty.Key) = False,
                    Format(ResStr_MustNotBeMemberOfOrg, [lOtherParty, GetHoldingOrgName]),
                    eOtherParty);
    end;
    ValidateValue(eOtherParty.Key <> '',
                  Format(ResStr_MissingData, [lOtherParty]),
                  eOtherParty);
    ValidateValue(eName.Key <> eOtherParty.Key,
                  Format(ResStr_CannotBeSamePerson, [ResStr_StaffResponsible, lOtherParty]),
                  eOtherParty);
    ValidateValue(eName.Key <> eContact.Key,
                  Format(ResStr_CannotBeSamePerson, [ResStr_StaffResponsible, ResStr_Contact]),
                  eContact);
  
    // Checks that the Staff Responsible and the Other Party aren't from the
    // same Organisation. However, it is permitted if they belong
    // to more than one organisation.
    // This should not be checked if the movement is an Internal Transfer.
    // Internal Transfer's have a Movement Type number of 6, so we don't
    // need to worry because it won't get here anyway.
    if (FMovementType in [1, 5, 8, 9]) then
    begin
    lOtherPartySameOrganisation := (dmGeneral.GetStoredProcOutputParam(
                                'usp_OtherPartySameOrganisation_Get',
                                ['@StaffResponsibleKey', eName.Key,
                                 '@OtherPartyKey', eOtherParty.Key],
                                '@SameOrganisation') = True);
    ValidateValue(not lOtherPartySameOrganisation,
                  Format(ResStr_OtherPartyNotFromSameOrganisation, [lOtherParty]),
                  eOtherParty);
    end;

    if eContact.Key = '' then lValidateContact := True
                         else lValidateContact := ValidateContact(eContact.Key);

    ValidateValue(lValidateContact, Format(ResStr_ContactMustBeAnIndividualOfOtherParty,
                  [eContact.Text, eOtherParty.Text]), eContact);
  end;

  // If the number entered already exists for a movement, inform the user of
  // this, but allow them to continue.
  lMovementNumberExists := (dmGeneral.GetStoredProcOutputParam(
                                                'usp_MovementNumberExists_Get',
                                                ['@Number', eNumber.Text],
                                                '@Exists') = True);
  if lMovementNumberExists and (FNumber <> eNumber.Text) then
    MessageDlg(Format(ResStr_MovementNumberExists, [lblNumber.Caption]),
               mtInformation, [mbOK], 0);

end;  // TfraMovementGeneral.ValidateData

{-------------------------------------------------------------------------------
}
procedure TfraMovementGeneral.eNameFindData(Sender: TObject);
begin
  inherited;
  LinkedEditFindData(Sender);
  CheckMacroNumber;
end;

procedure TfraMovementGeneral.chkReturnCompleteClick(Sender: TObject);
begin
  inherited;
  lblActualReturnDate.Visible := chkReturnComplete.Checked;
  eActualReturnDate.Visible := chkReturnComplete.Checked;
  eActualReturnDate.Text := DateTimeToStr(Date);

  RegisterControl(eActualReturnDate, 'ActualReturnDate', chkReturnComplete.Checked, 'Actual Return Date');
  eActualReturnDate.OnExit := eActualReturnDateExit;

  { Colour of mandatory fields is only set to yellow when the user clicks the edit
  button, so if eActualReturnDate wasn't visible when the frame loaded then it
  won't be the right colour. Set the colour to yellow here to make sure it is the
  right colour. }
  SetRequiredFieldColours(EditMode = emEdit);
  
end; //  TfraMovementGeneral.chkReturnCompleteClick

procedure TfraMovementGeneral.eActualReturnDateExit(Sender: TObject);
begin
  inherited;
  VagueDateEditExit(Sender);
  if chkReturnComplete.Checked and (eCompletionDate.Text = '')
      and (eActualReturnDate.Text <> '') then
    eCompletionDate.Text := eActualReturnDate.Text;
end; // TfraMovementGeneral.eActualReturnDateExit

end.



