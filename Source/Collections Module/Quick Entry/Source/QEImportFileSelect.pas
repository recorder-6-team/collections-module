{===============================================================================
  Unit:        QEImportFileSelect.pas

  Defines:     TfraQEImportFileSelect

  Description: Wizard page that allows the user to select and configure a file
               for import into a quick entry session.  

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 6 $
    $Date: 4/10/04 16:21 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImportFileSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, RestrictedEdits, ExtCtrls, Buttons, QEImportFrameUnit,
  QEImportDefinition;

resourcestring
  ResStr_BadConnectionString = 
      'An error occured trying to open a connection to the database.';
  ResStr_NoDelimiterSymbol = 'You must specify a field delimiter.';
  ResStr_NotAPositiveInteger = 'You must enter a positive integer.';
  ResStr_InvalidRange = 'You must enter a valid range.';

type
  TfraQEImportFileSelect = class(TQEImportFrame)
    pnlDelimiters: TPanel;
    lblQualifier: TLabel;
    lblRecSeparator: TLabel;
    Label1: TLabel;
    Shape1: TShape;
    btnLess: TBitBtn;
    btnMore: TBitBtn;
    rgSeparator: TRadioGroup;
    eSymbol: TEdit;
    cmbTextQualifier: TComboBox;
    cmbRecSeparator: TComboBox;
    rbDelimited: TRadioButton;
    rbFixedWidth: TRadioButton;
    pnlFormatting: TPanel;
    lblAdvanced: TLabel;
    shpAdvanced: TShape;
    gbImportedRows: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    rbImportAllRows: TRadioButton;
    rbImportSelectedRows: TRadioButton;
    eRowsFrom: TNumberEdit;
    eRowsTo: TNumberEdit;
    pnlTableSelect: TPanel;
    Label4: TLabel;
    Shape3: TShape;
    Label5: TLabel;
    lbTables: TListBox;
    Panel1: TPanel;
    Shape2: TShape;
    Label2: TLabel;
    eFilePath: TEdit;
    btnGetFilePath: TButton;
    FileOpenDialog: TOpenDialog;
    gbDateTime: TGroupBox;
    lblDateOrder: TLabel;
    lblDateDelimiter: TLabel;
    eDateDelimiter: TEdit;
    cmbDateFormat: TComboBox;
    procedure btnLessClick(Sender: TObject);
    procedure btnGetFilePathClick(Sender: TObject);
    procedure btnMoreClick(Sender: TObject);
    procedure FileOpenDialogTypeChange(Sender: TObject);
    procedure lbTablesClick(Sender: TObject);
    procedure rbDelimitedClick(Sender: TObject);
    procedure rbFixedWidthClick(Sender: TObject);
    procedure rbImportAllRowsClick(Sender: TObject);
    procedure rbImportSelectedRowsClick(Sender: TObject);
    procedure rgSeparatorClick(Sender: TObject);
  protected
    FImportType: TQEImportType;
    function FileImportType: TQEImportType; virtual;
    function GetNext: TQEImportFrameClass; override;
    procedure LoadTables; virtual;
    procedure SetFormattingOptionsVisibility(Value: Boolean); virtual;
    procedure SetImportType(Value: TQEImportType); virtual;
    function ValidConnectionString(const AConnectionString: String): Boolean;
        virtual;
  public
    procedure SaveContent; override;
    procedure Validate; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, ADOConnStringEditor, ADODB, ComObj, ADOInt,
  QEImportColWidthsSelect, QEImportColumnMatching, SMIBase, Validation;

const
  ADO_FILTER_INDEX = 8;

  DELIMITER_TAB       = 0;
  DELIMITER_SEMICOLON = 1;
  DELIMITER_COMMA     = 2;
  DELIMITER_SPACE     = 3;
  DELIMITER_OTHER     = 4;

  TEXT_IMPORT_TYPES = [itText, itCsv];

{-==============================================================================
    TfraQEImportFileSelect
===============================================================================}
{+------------------------------------------------------------------------------
  Display the "open file" dialogue.  
}
procedure TfraQEImportFileSelect.btnGetFilePathClick(Sender: TObject);
begin
  FileOpenDialog.FilterIndex := AppSettings.QuickEntryImportTypeIndex;
  if FileOpenDialog.Execute then
  begin
    AppSettings.QuickEntryImportTypeIndex := FileOpenDialog.FilterIndex;    
    eFilePath.Text := FileOpenDialog.FileName;
    SetImportType(FileImportType);
    SetIsComplete(True);
  end;
end;  // TfraQEImportFileSelect.btnGetFilePathClick 

{+------------------------------------------------------------------------------
  Show formatting options.
}
procedure TfraQEImportFileSelect.btnMoreClick(Sender: TObject);
begin
  SetFormattingOptionsVisibility(True);
  VertScrollBar.Visible := True;
end;  // TfraQEImportFileSelect.btnMoreClick 

{+------------------------------------------------------------------------------
  Hide formatting options.
}
procedure TfraQEImportFileSelect.btnLessClick(Sender: TObject);
begin
  SetFormattingOptionsVisibility(False);
  VertScrollBar.Visible := False;
end;  // TfraQEImportFileSelect.btnLessClick 

{+------------------------------------------------------------------------------
  Display ADO Connection builder if 'ADO Connection' is selected.
}
procedure TfraQEImportFileSelect.FileOpenDialogTypeChange(Sender: TObject);
begin
  if FileOpenDialog.FilterIndex = ADO_FILTER_INDEX then
  begin
    EndDialog(GetParent(FileOpenDialog.Handle), 0);
    
    with TdlgConnStringEditor.Create(nil) do
      try
        if ShowModal = mrOk then 
        begin
          if not ValidConnectionString(ConnectionString) then
            MessageDlg(ResStr_BadConnectionString, mtWarning, [mbOk], 0)
          else
          begin
            eFilePath.Text := ConnectionString;
            SetImportType(itAdo);
          end;
        end;
      finally
        Free;
      end;
  end;
end;  // TfraQEImportFileSelect.FileOpenDialogTypeChange 

{+------------------------------------------------------------------------------
  Import type corresponding to the selected file name extension.
}
function TfraQEImportFileSelect.FileImportType: TQEImportType;
var
  lExtension: String;
begin
  Result := itUnknown;
  if FileExists(eFilePath.Text) then
  begin
    lExtension := LowerCase(ExtractFileExt(eFilePath.Text));    
    if lExtension = '.txt' then Result := itText
    else if lExtension = '.csv' then Result := itCsv
    else if lExtension = '.xls' then Result := itExcel
    else if lExtension = '.dbf' then Result := itDBase
    else if lExtension = '.db'  then Result := itParadox
    else if lExtension = '.wk1' then Result := itLotus
    else if lExtension = '.wq1' then Result := itQuattro;
  end;
end;  // TfraQEImportFileSelect.FileImportType 

{-------------------------------------------------------------------------------
}
function TfraQEImportFileSelect.GetNext: TQEImportFrameClass;
begin
  if (FImportType in TEXT_IMPORT_TYPES) and rbFixedWidth.Checked then
    Result := TfraQEImportColWidthsSelect
  else
    Result := TfraQEImportColumnMatching;
end;  // TfraQEImportFileSelect.GetNext

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.lbTablesClick(Sender: TObject);
begin
  if lbTables.ItemIndex <> -1 then SetIsComplete(True);
end;  // TfraQEImportFileSelect.lbTablesClick 

{+------------------------------------------------------------------------------
  Populate the list of tables from the specified ADO connection.
}
procedure TfraQEImportFileSelect.LoadTables;
var
  lConnection: TADOConnection;
  lSchema: TADODataSet;
  lTypeField: Field;
  lNameField: Field;
begin
  lbTables.Clear;
  
  lConnection := TADOConnection.Create(nil);
  try
    lConnection.LoginPrompt := False;
    lConnection.ConnectionString := eFilePath.Text;
    lSchema := TADODataSet.Create(nil);
    try
      lConnection.OpenSchema(siTables, EmptyParam, EmptyParam, lSchema);
      with lSchema.Recordset do
      begin
        lTypeField := Fields['TABLE_TYPE'];
        lNameField := Fields['TABLE_NAME'];
        while not Eof do
        begin
          if lTypeField.Value = 'TABLE' then
            lbTables.Items.Add(lNameField.Value);
          MoveNext;
        end;
      end;
    finally
      lSchema.Free;
    end;    
  finally
    lConnection.Free;
  end;
end;  // TfraQEImportFileSelect.LoadTables

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.rbDelimitedClick(Sender: TObject);
begin
  rgSeparator.Enabled := True;
  eSymbol.Enabled := rgSeparator.ItemIndex = DELIMITER_OTHER;
  cmbTextQualifier.Enabled := True;
end;  // TfraQEImportFileSelect.rbDelimitedClick

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.rbFixedWidthClick(Sender: TObject);
begin
  rgSeparator.Enabled := False;
  eSymbol.Enabled := False;
  cmbTextQualifier.Enabled := False;
end;  // TfraQEImportFileSelect.rbFixedWidthClick

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.rbImportAllRowsClick(Sender: TObject);
begin
  eRowsFrom.Enabled := False;
  eRowsTo.Enabled := False;
end;  // TfraQEImportFileSelect.rbImportAllRowsClick 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.rbImportSelectedRowsClick(Sender: TObject);
begin
  eRowsFrom.Enabled := True;
  eRowsTo.Enabled := True;
end;  // TfraQEImportFileSelect.rbImportSelectedRowsClick 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.rgSeparatorClick(Sender: TObject);
begin
  eSymbol.Enabled := rgSeparator.ItemIndex = DELIMITER_OTHER;
end;  // TfraQEImportFileSelect.rgSeparatorClick 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.SaveContent;
begin
  Definition.ImportType := FImportType;
  Definition.DataSource := eFilePath.Text;

  if FImportType = itAdo then
    Definition.TableName := lbTables.Items[lbTables.ItemIndex]
  else
  if FImportType in TEXT_IMPORT_TYPES then
  begin
    Definition.FixedColumns := rbFixedWidth.Checked;
    case cmbRecSeparator.ItemIndex of
      0: Definition.RecordSeparator := rsCrLf;
      1: Definition.RecordSeparator := rsCr;
      2: Definition.RecordSeparator := rsLf;
    end;
    if not rbFixedWidth.Checked then
    begin
      case rgSeparator.ItemIndex of
        DELIMITER_TAB:       Definition.Delimiter := #9;
        DELIMITER_SEMICOLON: Definition.Delimiter := ';';
        DELIMITER_COMMA:     Definition.Delimiter := ',';
        DELIMITER_SPACE:     Definition.Delimiter := ' ';
        DELIMITER_OTHER:     Definition.Delimiter := eSymbol.Text[1];
      end;
      Definition.TextQualifier := cmbTextQualifier.Text[1];
    end;
  end;
  if FImportType in itSpreadsheet then
  begin
    if rbImportAllRows.Checked then
    begin
      Definition.FirstRow := 0;
      Definition.LastRow := 0;
    end
    else
    begin
      Definition.FirstRow := StrToInt(eRowsFrom.Text);
      Definition.LastRow := StrToInt(eRowsTo.Text);
    end;
  end;

  Definition.DateFormat := cmbDateFormat.Text;
  if Length(eDateDelimiter.Text) > 0 then
    Definition.DateDelimiter := eDateDelimiter.Text[1]
  else
    Definition.DateDelimiter := #0;
end;  // TfraQEImportFileSelect.SaveContent

{+------------------------------------------------------------------------------
  Set visibility of formatting options.
}
procedure TfraQEImportFileSelect.SetFormattingOptionsVisibility(Value: Boolean);
begin
  pnlFormatting.Visible := Value;
  btnMore.Visible := not Value;
  btnLess.Visible := Value;
end;  // TfraQEImportFileSelect.SetFormattingOptionsVisibility 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.SetImportType(Value: TQEImportType);
begin
  FImportType := Value;
  VertScrollBar.Visible := False;
  pnlDelimiters.Visible := Value in TEXT_IMPORT_TYPES;
  SetFormattingOptionsVisibility(
      (Value in itSpreadsheet) 
      and not (Value in TEXT_IMPORT_TYPES));
  pnlTableSelect.Visible := Value = itAdo;
  case Value of
    itCsv: 
      begin
        rbDelimited.Checked := True;
        rgSeparator.ItemIndex := DELIMITER_COMMA;
      end;

    itAdo:
      LoadTables;
  end;
end;  // TfraQEImportFileSelect.SetImportType 

{+------------------------------------------------------------------------------
  Is the given ADO connection string valid?
}
function TfraQEImportFileSelect.ValidConnectionString(const AConnectionString:
    String): Boolean;
begin
  with TADOConnection.Create(nil) do
    try
      ConnectionString := AConnectionString;
      LoginPrompt := False;
      try
        Open;
        Result := True;
      except 
        on EOleException do
          Result := False;
      end;
    finally
      Free;
    end;
end;  // TfraQEImportFileSelect.ValidConnectionString

{-------------------------------------------------------------------------------
}
procedure TfraQEImportFileSelect.Validate;
var
  lIsValid: Boolean;

  procedure Fail(const Message: String; Control: TWinControl);
  begin
    lIsValid := False;
    if (Control.Parent = pnlFormatting) and not pnlFormatting.Visible then
      SetFormattingOptionsVisibility(True);  
    MessageDlg(Message, mtInformation, [mbOk], 0);
    Control.SetFocus;
  end;

  procedure CheckPositiveInteger(const Value: String; Control: TWinControl);
  var
    I: Integer;
  begin
    try
      I := StrToInt(Value);
    except
      on EConvertError do I := 0;
    end;
    if I < 1 then Fail(ResStr_NotAPositiveInteger, Control);
  end;
  
begin
  lIsValid := True;
  
  if pnlDelimiters.Visible
      and rbDelimited.Checked 
      and (rgSeparator.ItemIndex = DELIMITER_OTHER)
      and (eSymbol.Text = '')
  then
    Fail(ResStr_NoDelimiterSymbol, eSymbol);

  if lIsValid
      and (pnlDelimiters.Visible or pnlFormatting.Visible)
      and rbImportSelectedRows.Checked then
  begin
    CheckPositiveInteger(eRowsFrom.Text, eRowsFrom);
    if lIsValid then CheckPositiveInteger(eRowsTo.Text, eRowsTo);
    if lIsValid then
      if StrToInt(eRowsFrom.Text) > StrToInt(eRowsTo.Text) then
        Fail(ResStr_InvalidRange, eRowsTo);
  end;

  SetIsValid(lIsValid);
end;  // TfraQEImportFileSelect.Validate 

end.
