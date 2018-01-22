{===============================================================================
  Unit:        FrameJobDetails.pas

  Defines:     TfraJobGeneral

  Description:

  Model:       CollectionsJobsAndTasks

  Created:     May 2003

  Last revision information:
    $Revision: 28 $
    $Date: 24/10/12 14:28 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameJobGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  ComCtrls, Grids, RestrictedEdits, ComboListID, LuxIDComboBox, GeneralFunctions,
  ConceptGroupComboBox, VagueDateEdit, DataTypes, LuxembourgDataClasses,
  DataClasses, ADOInt, ExceptionForm, DssStringGrid, Validation, SearchManager,
  BaseCompositeComponent, LinkedControls, DropTarget, DSSDataTypes, Recorder2000_TLB,
  UserEdit;

type
  TStaffItem = class(TLuxGridDataItem)
  private
    FName: String;
    FNameKey: TKeyString;
  protected
    procedure GetData(const Column: Integer; var AText: String; var ANameKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
    procedure ValidateData; override;
  public
    property Name: String read FName;
    property NameKey: TKeyString read FNameKey;
  end;

  TStaffList = class(TLuxGridDataList)
  protected
    procedure AddToList(AItem: TLuxCachedDataItem; AIndex: Integer); override;
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    procedure CheckGridColumnWidth;
    procedure DeleteItem(const AIndex: integer); override;
  end;
  
  {-----------------------------------------------------------------------------
    General details tab page for the main aspects of jobs.  This is embedded onto TfraJob
    when required.
  }
  TfraJobGeneral = class(TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnRemove: TImageListButton;
    cmbCurrency: TConceptGroupComboBox;
    cmbDuration: TConceptGroupComboBox;
    cmbStatus: TLuxIDComboBox;
    eAmount: TNumberEdit;
    eDateEnd: TVagueDateEdit;
    eDateStart: TVagueDateEdit;
    eDuration: TEdit;
    eName: TEdit;
    eStaff: TUserEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblStaff: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblActualDuration: TLabel;
    lblDomains: TLabel;
    lblJobNo: TLabel;
    mmDetails: TMemo;
    sgConservators: TDSSStringGrid;
    Shape1: TShape;
    udDuration: TUpDown;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cmbStatusPopulate(Sender: TObject);
    procedure DropStaff(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure eStaffExit(Sender: TObject);
    procedure eStaffFindData(Sender: TObject);
    procedure eStaffGetData(Sender: TObject);
    procedure sgConservatorsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgConservatorsSelectCell(Sender: TObject; ACol, ARow: Integer; var
        CanSelect: Boolean);
    procedure udDurationClick(Sender: TObject; Button: TUDBtnType);
    procedure eStaffKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eStaffChange(Sender: TObject);
  private
    FStaffList: TStaffList;
    FTimestamp: TSQLSvrTimestamp;
    procedure btnRemoveToggleEnabled;
    function CanAddRow: Boolean;
    function CheckNoDuplication(var AName: String): Boolean;
    procedure UpdateStaff(const AKeyList: IKeyList);
    procedure ValidateStringGrid;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled : Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings, LuxembourgConstants, GeneralData, InterfaceDataModule;

{-==============================================================================
    TfraJobGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor sets up the string grid. 
}
constructor TfraJobGeneral.Create(AOwner: TComponent);
begin
  inherited;
  FStaffList := TStaffList.Create(TStaffItem, sgConservators);
  // Setting the control sets the type to custom.
  sgConservators.ColumnsInfo[0].WinControl := eStaff;
end;  // TfraJobGeneral.CheckColumnWidth

{-------------------------------------------------------------------------------
  Destroy the list for the string grid.
}
destructor TfraJobGeneral.Destroy;
begin
  FStaffList.Free;
  inherited Destroy;
end;  // TfraJobGeneral.Destroy

{-------------------------------------------------------------------------------
  Add an item to the string grid.
}
procedure TfraJobGeneral.btnAddClick(Sender: TObject);
begin
  inherited;
  if CanAddRow then begin
    FStaffList.AddNew(TStaffItem.CreateNew(FStaffList));
    sgConservators.Row := FStaffList.ItemCount - 1;
    eStaff.SetFocus;
  end else
    sgConservators.Row := sgConservators.RowCount - 1;
end;  // TfraJobGeneral.btnAddClick

{-------------------------------------------------------------------------------
  Remove an item from the string grid.
}
procedure TfraJobGeneral.btnRemoveClick(Sender: TObject);
begin
  inherited;
  if (eStaff.Text = '') or
      (MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                  mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    FStaffList.DeleteItem(sgConservators.Row);
  end;
end;  // TfraJobGeneral.btnRemoveClick

{-------------------------------------------------------------------------------
  Method that decides whether btnRemove should be enabled or not.
}
procedure TfraJobGeneral.btnRemoveToggleEnabled;
begin
  btnRemove.Enabled := (EditMode = emEdit) and ((eStaff.Text <> '')
      or ((sgConservators.Cells[0, 0] <> '') or (sgConservators.RowCount > 1))
      or ((sgConservators.RowCount = 1) and (sgConservators.Cells[0, 0] <> '')));
end;  // TfraJobGeneral.btnRemoveToggleEnabled

{-------------------------------------------------------------------------------
}
function TfraJobGeneral.CanAddRow: Boolean;
var
  lLastRowIndex: Integer;
begin
  lLastRowIndex := sgConservators.RowCount - 1;

  // If the linked edit is on the last row of the grid, check the contents
  // of the linked edit rather than the grid, because the grid won't have
  // got the linked edit's data yet.
  if lLastRowIndex = sgConservators.Row then
    Result := eStaff.Text <> ''
  // If the linked edit isn't on the last row of the grid, check to see if
  // the last row of the grid contains any data. If it doesn't, then a
  // new row can be added.
  else
    Result := sgConservators.Cells[0, lLastRowIndex] <> '';
end;  // TfraJobGeneral.CanAddRow

{-------------------------------------------------------------------------------
  Method that checks there is no duplication of names in the string grid.
}
function TfraJobGeneral.CheckNoDuplication(var AName: String): Boolean;
var
  x, y: Integer;
begin
  Result := True;
  with sgConservators do
    for x := 0 to RowCount - 2 do
      for y := x + 1 to RowCount - 1 do
        if (Cells[0, x] = Cells[0, y]) and (Cells[0, x] <> '') then
        begin
          Result := False;
          AName := Cells[0, x];
          Break;
        end;
end;  // TfraJobGeneral.CheckNoDuplication

{-------------------------------------------------------------------------------
  Populate cmbStatus.
}
procedure TfraJobGeneral.cmbStatusPopulate(Sender: TObject);
begin
  cmbStatus.Add(ResStr_Pending, 0);
  cmbStatus.Add(ResStr_Open, 1);
  cmbStatus.Add(ResStr_Closed, 2);
  cmbStatus.Add(ResStr_Postponed, 3);
  cmbStatus.Add(ResStr_Abandoned, 4);
end;  // TfraJobGeneral.cmbStatusPopulate 

{-------------------------------------------------------------------------------
  Run the delete proc. for the current frame. 
}
procedure TfraJobGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConservationJob_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraJobGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.DropStaff(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eStaff, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraJobGeneral.DropStaff 

{-------------------------------------------------------------------------------
  Toggle whether the string grid is enabled or not. 
}
procedure TfraJobGeneral.EnableControls(AEnabled : Boolean);
begin
  inherited;
  sgConservators.ReadOnly := not AEnabled;
  btnRemoveToggleEnabled;
end;  // TfraJobGeneral.EnableControls

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.eStaffChange(Sender: TObject);
begin
  inherited;
  btnRemoveToggleEnabled;
end;  // TfraJobGeneral.eStaffChange

{-------------------------------------------------------------------------------
  When leaving the eStaff linked edit it will now bring up the find dialog if invalid
      data is entered. This means that if the user clicks save and invalid data is
      present, the find dialog automatically comes up.
}
procedure TfraJobGeneral.eStaffExit(Sender: TObject);
begin
  inherited;
  //  eStaffFindData(Self);
  btnRemoveToggleEnabled;
end;  // TfraJobGeneral.eStaffExit

{-------------------------------------------------------------------------------
  Shows the find dialog for the linked edit on the string grid. 
}
procedure TfraJobGeneral.eStaffFindData(Sender: TObject);
begin
  inherited;
  DoCheck(eStaff, stIndividual);
end;  // TfraJobGeneral.eStaffFindData

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.eStaffGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateStaff, TN_NAME)
end;  // TfraJobGeneral.eStaffGetData

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.eStaffKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DELETE) and (eStaff.Text = '') then begin
    btnRemove.Click;
    Key := 0;
  end
  else
    // This is unfortunately necessary since the eStaff control overrides
    // the OnKeyPress of the TUserEdit.
    eStaff.GetCurrentUserF11(Sender, Key, Shift);
end;  // TfraJobGeneral.eStaffKeyDown

{-------------------------------------------------------------------------------
  Gets the caption of the frame.
}
function TfraJobGeneral.GetCaption: String;
begin
  if eDateStart.Text = '' then Result := 'Unknown'
                          else Result := eDateStart.Text;
  Result := Result + ' - ' + eName.Text;
end;  // TfraJobGeneral.GetCaption

{-------------------------------------------------------------------------------
  Loads any extra required data into the frame controls.
}
procedure TfraJobGeneral.LoadData;
begin
  inherited LoadData;

  with RegisteredRecordSets[0] do
    if not Eof then begin
      FTimestamp := Fields['Timestamp'].Value;
      lblJobNo.Caption := VarToStr(Fields['Job_Number'].Value);
    end;

  FStaffList.MasterKey := Key;
  FStaffList.Refresh;
  FStaffList.CheckGridColumnWidth;
end;  // TfraJobGeneral.LoadData

{-------------------------------------------------------------------------------
  Registers the controls used in the frame.
}
procedure TfraJobGeneral.RegisterControls;
begin
  inherited RegisterControls;

  RegisterRecordSet('usp_ConservationJob_Select');
  RegisterRecordSet('usp_JobGeneralStaff_Select');

  RegisterControl(eName,'Item_Name',True,ResStr_Name);
  RegisterControl(eDateStart,'From');
  RegisterControl(eDateEnd,'To');
  RegisterControl(eDuration,'Duration');
  RegisterControl(cmbDuration,'TimeUnit','Duration_Unit_Concept_Key');
  RegisterConceptGroupComboBox(cmbDuration,CG_TIME_UNITS);
  RegisterAsyncControl(lblActualDuration,'usp_JobGeneralActualDuration_Get',
                       ['@Key','Conservation_Job_Key'],'@Duration');
  RegisterControl(cmbStatus,'','Status',True,ResStr_Status);
  RegisterAsyncControl(lblDomains,'usp_DomainsForMask_Get',
                       ['@Mask','Domain_Mask'],'@Domains');
  RegisterControl(eAmount,'Cost');
  RegisterControl(cmbCurrency,'Currency','Currency_Concept_Key');
  RegisterConceptGroupComboBox(cmbCurrency,CG_CURRENCIES);
  RegisterControl(mmDetails,'Details');
end;  // TfraJobGeneral.RegisterControls

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eStaff, DropStaff, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraJobGeneral.RegisterDragDropComponents

{-------------------------------------------------------------------------------
  Saves\Updates the data in the tables using the frame data.
}
procedure TfraJobGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                         '@ItemName', eName.Text,
                         '@FromVagueDateStart', eDateStart.StartDate,
                         '@FromVagueDateEnd', eDateStart.EndDate,
                         '@FromVagueDateType', eDateStart.DateTypeString,
                         '@ToVagueDateStart', eDateEnd.StartDate,
                         '@ToVagueDateEnd', eDateEnd.EndDate,
                         '@ToVagueDateType', eDateEnd.DateTypeString,
                         '@Duration', eDuration.Text,
                         '@DurationType', cmbDuration.CurrentStrID,
                         '@Status', cmbStatus.CurrentStrID,
                         '@CostAmount', eAmount.Text,
                         '@Currency', cmbCurrency.CurrentStrID,
                         '@Details', mmDetails.Text,
                         '@Timestamp', FTimestamp]);

  if Key = '' then begin
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONSERVATION_JOB,
                                                  'usp_ConservationJob_Insert',
                                                  lParams,
                                                  '@Key'));
    FStaffList.MasterKey := Key;
  end else
    dmGeneral.RunUpdateStoredProc('usp_ConservationJob_Update', lParams);

  // Now we definitely have a key, the contents of the stringgrid can be saved.
  FStaffList.Update;
end;  // TfraJobGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.sgConservatorsKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  inherited;
  if (Key = VK_DOWN) and not CanAddRow then begin
    // If not allowed to add a row, try and move on to the next row, whilst
    // making sure that it won't try to add a new row.
    with sgConservators do
      if Row < RowCount - 1 then Row := Row + 1;
    Key := 0;
  end;
end;  // TfraJobGeneral.sgConservatorsKeyDown

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.sgConservatorsSelectCell(Sender: TObject; ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  inherited;
  btnRemoveToggleEnabled;
end;  // TfraJobGeneral.sgConservatorsSelectCell

{-------------------------------------------------------------------------------
  Can't use the 'Associate' property of udDuration because then it can't cope with
      floating point numbers. Hence, the OnClick event handler does the
      addition/subtraction manually here.
}
procedure TfraJobGeneral.udDurationClick(Sender: TObject; Button: TUDBtnType);
begin
  inherited;
  if EditMode = emEdit then
    with eDuration do
      if Text = '' then Text := '0'
      else
      if IsFloat(Text) then
        if StrToFloat(Text) < 0 then Text := '0'
        else
        if Button = btNext then
          Text := FloatToStr(StrToFloat(Text) + 1)
        else
        if StrToFloat(Text) - 1 >= 0 then
          Text := FloatToStr(StrToFloat(Text) - 1);
end;  // TfraJobGeneral.udDurationClick

{-------------------------------------------------------------------------------
}
procedure TfraJobGeneral.UpdateStaff(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eStaff, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblStaff.Caption)]));
end;  // TfraJobGeneral.UpdateStaff

{-------------------------------------------------------------------------------
  Validate the contents of the string grid.
}
procedure TfraJobGeneral.ValidateData;
var
  lText: String;
  lNoDuplication: Boolean;
begin
  inherited;
  if eDuration.Text <> '' then begin
    ValidateValue(IsFloat(eDuration.Text), ResStr_DurationMustBeNumber, eDuration);
    ValidateValue(StrToFloat(eDuration.Text) >= 0,
                  Format(ResStr_MustBeNonNegative, [ResStr_Duration]), eDuration);
    ValidateValue(cmbDuration.ItemIndex <> -1,
                  Format(ResStr_ValueRequiresUnit, [ResStr_EstimatedDuration]), eDuration);
  end else
    cmbDuration.ItemIndex := -1;

  ValidateStringGrid;
  lNoDuplication := CheckNoDuplication(lText);
  ValidateValue(lNoDuplication,
                Format(ResStr_DuplicationInGrid, [lText, ResStr_Staff]), sgConservators);
end;  // TfraJobGeneral.ValidateData

{-------------------------------------------------------------------------------
  Before saving the data, giving the user the option to change any incorrect names in the
      string grid by using the Find Dialog. If they press cancel, validation furthur down
      the line will reveal the invalid name to the user.
}
procedure TfraJobGeneral.ValidateStringGrid;
var
  lIdx: Integer;
begin
  for lIdx := 0 to sgConservators.RowCount - 1 do begin
    sgConservators.Row := lIdx;
    eStaffFindData(Self);
  end;
  FStaffList.ValidateContent;
end;  // TfraJobGeneral.ValidateStringGrid


{-==============================================================================
    TStaffItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStaffItem.GetData(const Column: Integer; var AText: String; var ANameKey:
    TKeyString);
begin
  AText := Name;
  ANameKey := NameKey;
end;  // TStaffItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TStaffItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Key'].Value));
  FNameKey := AFields['Name_Key'].Value;
  FName := AFields['Name'].Value;
end;  // TStaffItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TStaffItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  FName := AText;
  FNameKey := AKey;
  SetModified;
end;  // TStaffItem.SetData

{-------------------------------------------------------------------------------
}
procedure TStaffItem.ValidateData;
begin
  if FNameKey = '' then Abort;
  ValidateValue(CheckIsIndividual(FNameKey),
                Format(ResStr_MustBeIndividual, [ResStr_Staff]));
end;  // TStaffItem.ValidateData

{-==============================================================================
    TStaffList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStaffList.AddToList(AItem: TLuxCachedDataItem; AIndex: Integer);
begin
  inherited;
  CheckGridColumnWidth;
end;  // TStaffList.AddToList

{-------------------------------------------------------------------------------
}
procedure TStaffList.CheckGridColumnWidth;
var
  lMin, lMax: Integer;
begin
  with Grid do begin
    ColWidths[0] := Width - 4;  // 4 for borders

    // Check the horizontal scrollbar is visible, or not.
    // That would be because column width is too big.
    GetScrollRange(Handle, SB_HORZ, lMin, lMax);
    // Adjust width of Title column accordingly so that it disappears.
    if lMin <> lMax then
      ColWidths[0] := ColWidths[0] - GetSystemMetrics(SM_CYHSCROLL);
  end;
end;  // TStaffList.CheckGridColumnWidth

{-------------------------------------------------------------------------------
}
procedure TStaffList.DeleteItem(const AIndex: integer);
begin
  inherited;
  CheckGridColumnWidth;
end;  // TStaffList.DeleteItem

{-------------------------------------------------------------------------------
}
procedure TStaffList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TStaffItem(AItem) do begin
    dmGeneral.RunInsertStoredProc(TN_CONSERVATION_JOB_STAFF,
                    'usp_ConservationJobStaff_Insert',
                   ['@ConservationJobKey', MasterKey,
                    '@NameKey', NameKey
                   ], '@Key');
  end;
end;  // TStaffList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TStaffList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TStaffItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_ConservationJobStaff_Delete',
                                    ['@Key', ItemKey]);
end;  // TStaffList.DoDeletion 

{-------------------------------------------------------------------------------
}
procedure TStaffList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TStaffItem(AItem) do
    dmGeneral.RunUpdateStoredProc('usp_ConservationJobStaff_Update',
                                    ['@ConservationJobKey', MasterKey,
                                      '@NameKey', NameKey,
                                      '@Key', ItemKey
                                    ]);
end;  // TStaffList.DoModification 

{-------------------------------------------------------------------------------
}
function TStaffList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_ConservationJobStaff_Select',
                                     ['@Key', MasterKey]);
end;  // TStaffList.GetRecordset 

end.
