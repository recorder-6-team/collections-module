{===============================================================================
  Unit:        NumberMacroOptionsFrame

  Defines:     TfraNumberMacroOptions

  Description:

  Model:       CollectionsModuleManager.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 11/08/04 11:20 $
    $Author: Anthonysimpson $

===============================================================================}

unit NumberMacroOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, RestrictedEdits, ADODb;

type
  TMacro = class(TObject)
  private
    FIDMacro: String;
    FIDSeed: Integer;
    FMacro: String;
    FMacroIDGeneration: Boolean;
    FModified: Boolean;
    FNumberType: String;
    procedure SetIDMacro(const Value: String);
    procedure SetIDSeed(const Value: Integer);
    procedure SetMacro(const Value: String);
    procedure SetMacroIDGeneration(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetNumberType(const Value: String);
  public
    procedure Load(ARecordset: _Recordset);
    procedure Save;
    property IDMacro: String read FIDMacro write SetIDMacro;
    property IDSeed: Integer read FIDSeed write SetIDSeed;
    property Macro: String read FMacro write SetMacro;
    property MacroIDGeneration: Boolean read FMacroIDGeneration write SetMacroIDGeneration;
    property Modified: Boolean read FModified write SetModified;
    property NumberType: String read FNumberType write SetNumberType;
  end;
  
  TfraNumberMacroOptions = class(TFrame)
    btnDay: TButton;
    btnDept: TButton;
    btnID: TButton;
    btnIDDay: TButton;
    btnIDDept: TButton;
    btnIDMonth: TButton;
    btnIDYear: TButton;
    btnMonth: TButton;
    btnYear: TButton;
    cmbNumberType: TComboBox;
    eNumberSequenceStart: TEdit;
    gbIdGeneration: TGroupBox;
    Label1: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    lblExample: TLabel;
    mmIDMacro: TMemo;
    mmMacro: TMemo;
    rbGlobalID: TRadioButton;
    rbMacroID: TRadioButton;
    procedure btnDayClick(Sender: TObject);
    procedure btnDeptClick(Sender: TObject);
    procedure btnIDClick(Sender: TObject);
    procedure btnIDDayClick(Sender: TObject);
    procedure btnIDDeptClick(Sender: TObject);
    procedure btnIDMonthClick(Sender: TObject);
    procedure btnIDYearClick(Sender: TObject);
    procedure btnMonthClick(Sender: TObject);
    procedure btnYearClick(Sender: TObject);
    procedure cmbNumberTypeChange(Sender: TObject);
    procedure eNumberSequenceStartChange(Sender: TObject);
    procedure mmIDMacroChange(Sender: TObject);
    procedure mmIDMacroExit(Sender: TObject);
    procedure mmMacroChange(Sender: TObject);
    procedure rbGlobalIDClick(Sender: TObject);
    procedure rbMacroIDClick(Sender: TObject);
  private
    procedure EnableControls;
    procedure EnableMacroIDGenerationControls(AEnabled: Boolean);
    function GetCurrentMacro: TMacro;
    procedure InsertTag(AMemo: TMemo; const ATag: String);
  public
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property CurrentMacro: TMacro read GetCurrentMacro;
  end;
  
//==============================================================================
implementation

uses
  GeneralData, LuxembourgConstants, GeneralFunctions, ResourceStrings,
  ApplicationSettings;

{$R *.dfm}

{-==============================================================================
    TfraNumberMacroOptions
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TfraNumberMacroOptions.Destroy;
var
  i: Integer;
begin
  for i := 0 to cmbNumberType.Items.Count - 1 do cmbNumberType.Items.Objects[i].Free;
  
  inherited;
end;  // TfraNumberMacroOptions.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnDayClick(Sender: TObject);
begin
  // Insert <#Day> in mmMacro
  InsertTag(mmMacro, MACRO_DAY);
end;  // TfraNumberMacroOptions.btnDayClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnDeptClick(Sender: TObject);
begin
  // Insert <#Dept> in mmMacro
  InsertTag(mmMacro, MACRO_DEPT);
end;  // TfraNumberMacroOptions.btnDeptClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnIDClick(Sender: TObject);
begin
  // Insert <#ID> in mmMacro
  InsertTag(mmMacro, MACRO_ID);
end;  // TfraNumberMacroOptions.btnIDClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnIDDayClick(Sender: TObject);
begin
  // Insert <#Day> in mmIDMacro
  InsertTag(mmIDMacro, MACRO_DAY);
end;  // TfraNumberMacroOptions.btnIDDayClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnIDDeptClick(Sender: TObject);
begin
  // Insert <#Dept> in mmIDMacro
  InsertTag(mmIDMacro, MACRO_DEPT);
end;  // TfraNumberMacroOptions.btnIDDeptClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnIDMonthClick(Sender: TObject);
begin
  // Insert <#Month> in mmIDMacro
  InsertTag(mmIDMacro, MACRO_MONTH);
end;  // TfraNumberMacroOptions.btnIDMonthClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnIDYearClick(Sender: TObject);
begin
  // Insert <#Year> in mmIDMacro
  InsertTag(mmIDMacro, MACRO_YEAR);
end;  // TfraNumberMacroOptions.btnIDYearClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnMonthClick(Sender: TObject);
begin
  // Insert <#Month> in mmMacro
  InsertTag(mmMacro, MACRO_MONTH);
end;  // TfraNumberMacroOptions.btnMonthClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.btnYearClick(Sender: TObject);
begin
  // Insert <#Year> in mmMacro
  InsertTag(mmMacro, MACRO_YEAR);
end;  // TfraNumberMacroOptions.btnYearClick 

{-------------------------------------------------------------------------------
  When an item is selected in the combo box, populate the details of the selected number macro
      type.
}
procedure TfraNumberMacroOptions.cmbNumberTypeChange(Sender: TObject);
var
  lModified: Boolean;
begin
  with TMacro(cmbNumberType.Items.Objects[cmbNumberType.ItemIndex]) do begin
    // Preserve the status of Modified, as changing the controls will update it
    lModified := Modified;
    mmMacro.Text := Macro;
    if MacroIDGeneration then
      rbMacroID.Checked := True
    else
      rbGlobalID.Checked := True;
    mmIDMacro.Text := IDMacro;
    eNumberSequenceStart.Text := IntToStr(IDSeed);
    Modified := lModified;
  end;
end;  // TfraNumberMacroOptions.cmbNumberTypeChange 

{-------------------------------------------------------------------------------
  If the user isn't a System Manager, all of the controls should be disabled except for
      cmbNumberType.
}
procedure TfraNumberMacroOptions.EnableControls;
begin
  if AppSettings.UserAccessLevel < 4 then begin
    btnID.Enabled := False;
    btnYear.Enabled := False;
    btnMonth.Enabled := False;
    btnDay.Enabled := False;
    btnDept.Enabled := False;
    mmMacro.Enabled := False;
    rbGlobalID.Enabled := False;
    rbMacroID.Enabled := False;
    mmIDMacro.Enabled := False;
    btnIDYear.Enabled := False;
    btnIDMonth.Enabled := False;
    btnIDDay.Enabled := False;
    btnIDDept.Enabled := False;
    eNumberSequenceStart.Enabled := False;
  end;
end;  // TfraNumberMacroOptions.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.EnableMacroIDGenerationControls(AEnabled: Boolean);
begin
  btnIDYear.Enabled  := AEnabled;
  btnIDMonth.Enabled := AEnabled;
  btnIDDay.Enabled   := AEnabled;
  btnIDDept.Enabled  := AEnabled;
  mmIDMacro.Enabled  := AEnabled;
  
  CurrentMacro.MacroIDGeneration := AEnabled;
end;  // TfraNumberMacroOptions.EnableMacroIDGenerationControls 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.eNumberSequenceStartChange(Sender: TObject);
begin
  if IsInt(eNumberSequenceStart.Text) then
    CurrentMacro.IDSeed := StrToInt(eNumberSequenceStart.Text)
  else begin
    ShowInformation(ResStr_SupplyValidSeed);
    eNumberSequenceStart.Text := IntToStr(CurrentMacro.IDSeed);
  end;
end;  // TfraNumberMacroOptions.eNumberSequenceStartChange 

{-------------------------------------------------------------------------------
}
function TfraNumberMacroOptions.GetCurrentMacro: TMacro;
begin
  Result := TMacro(cmbNumberType.Items.Objects[cmbNumberType.ItemIndex]);
end;  // TfraNumberMacroOptions.GetCurrentMacro 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.InsertTag(AMemo: TMemo; const ATag: String);
var
  lCaretPos: Integer;
begin
  lCaretPos := AMemo.SelStart;
  AMemo.Text := Copy(AMemo.Text, 1, AMemo.SelStart) +
                ATag +
                Copy(AMemo.Text, AMemo.SelStart + 1, Length(AMemo.Text));
  AMemo.SelStart := lCaretPos + Length(ATag);
  AMemo.SetFocus;
end;  // TfraNumberMacroOptions.InsertTag 

{-------------------------------------------------------------------------------
  Load the list of number types and the details of the first type. 
}
procedure TfraNumberMacroOptions.Load;
var
  lMacro: TMacro;
  lRecordset: _Recordset;
begin
  lRecordset := dmGeneral.GetRecordset('usp_Macros_Select', []);
  with lRecordset do begin
    if not EOF then MoveFirst;
    while not EOF do begin
      lMacro := TMacro.Create;
      lMacro.Load(lRecordset);
      cmbNumberType.Items.AddObject(Fields['Number_Type'].Value, lMacro);
      MoveNext;
    end;
  end;
  // Select first item
  cmbNumberType.ItemIndex := Min(0, cmbNumberType.Items.Count-1);
  cmbNumberTypeChange(nil);
  EnableControls;
end;  // TfraNumberMacroOptions.Load 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.mmIDMacroChange(Sender: TObject);
begin
  CurrentMacro.IDMacro := mmIDMacro.Text;
end;  // TfraNumberMacroOptions.mmIDMacroChange 

{-------------------------------------------------------------------------------
  When the user leaves the mmIDMacro memo box, if the contents is empty, select the radio
      button to automatically generate the ID. Otherwise usp_Macro_Get will fail when called
      from a frame that generates the macro.
}
procedure TfraNumberMacroOptions.mmIDMacroExit(Sender: TObject);
begin
  if Trim(mmIDMacro.Text) = '' then rbGlobalID.Checked := True;
end;  // TfraNumberMacroOptions.mmIDMacroExit 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.mmMacroChange(Sender: TObject);
begin
  CurrentMacro.Macro := mmMacro.Text;
end;  // TfraNumberMacroOptions.mmMacroChange 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.rbGlobalIDClick(Sender: TObject);
begin
  EnableMacroIDGenerationControls(False);
end;  // TfraNumberMacroOptions.rbGlobalIDClick 

{-------------------------------------------------------------------------------
}
procedure TfraNumberMacroOptions.rbMacroIDClick(Sender: TObject);
begin
  EnableMacroIDGenerationControls(True);
end;  // TfraNumberMacroOptions.rbMacroIDClick 

{-------------------------------------------------------------------------------
  Save each changed macro detail to the database. 
}
procedure TfraNumberMacroOptions.Save;
var
  lIdx: Integer;
begin
  for lIdx := 0 to cmbNumberType.Items.Count-1 do
    TMacro(cmbNumberType.Items.Objects[lIdx]).Save;
end;  // TfraNumberMacroOptions.Save 

{-==============================================================================
    TMacro
===============================================================================}
{-------------------------------------------------------------------------------
  Load macro detail from the record supplied. 
}
procedure TMacro.Load(ARecordset: _Recordset);
begin
  with ARecordset do begin
    FNumberType := Fields['Number_Type'].Value;
    FMacro := VarToStr(Fields['Macro'].Value);
    FIDMacro := VarToStr(Fields['ID_Macro'].Value);
    FIDSeed := Fields['ID_Seed'].Value;
    FMacroIDGeneration := Fields['Macro_ID_Generation'].Value;
  end; // with
  FModified := False;
end;  // TMacro.Load 

{-------------------------------------------------------------------------------
  Save any changes to the number type macro 
}
procedure TMacro.Save;
begin
  if Modified then
    dmGeneral.RunStoredProc('usp_Macro_Update', [
        '@NumberType', FNumberType,
        '@Macro', FMacro,
        '@IDMacro', FIDMacro,
        '@MacroIDGeneration', FMacroIDGeneration,
        '@IDSeed', FIDSeed]);
  Modified := False;
end;  // TMacro.Save 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetIDMacro(const Value: String);
begin
  if FIDMacro <> Value then begin
    FIDMacro := Value;
    FModified := True;
  end;
end;  // TMacro.SetIDMacro 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetIDSeed(const Value: Integer);
begin
  if FIDSeed <> Value then begin
    FIDSeed := Value;
    FModified := True;
  end;
end;  // TMacro.SetIDSeed 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetMacro(const Value: String);
begin
  if FMacro <> Value then begin
    FMacro := Value;
    FModified := True;
  end;
end;  // TMacro.SetMacro 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetMacroIDGeneration(const Value: Boolean);
begin
  if FMacroIDGeneration <> Value then begin
    FMacroIDGeneration := Value;
    FModified := True;
  end;
end;  // TMacro.SetMacroIDGeneration 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;  // TMacro.SetModified 

{-------------------------------------------------------------------------------
}
procedure TMacro.SetNumberType(const Value: String);
begin
  FNumberType := Value;
end;  // TMacro.SetNumberType 

end.


