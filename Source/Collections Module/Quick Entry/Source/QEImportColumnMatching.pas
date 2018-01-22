{===============================================================================
  Unit:        QEImportColumnMatching.pas

  Defines:     TfraQEImportColumnMatching

  Description: Wizard page that allows the user to match columns in the import
               file to the fields selected in the Quick Entry session.  

  Model:       -

  Created:     September 2004

  Last revision information:
    $Revision: 4 $
    $Date: 13/12/07 17:01 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImportColumnMatching;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, DssStringGrid, StdCtrls, ExtCtrls, QEImportFrameUnit,
  QEImportDefinition;

resourcestring
  ResStr_ColumnsInFile = 'Columns in file';
  ResStr_FieldsInSession = 'Fields in session';
  ResStr_DefaultColumnName = 'Column %d';
  ResStr_NoImportData = 'There are no data rows in the specified import file.'
      + ' Please try again with a non-empty file.';

type
  TfraQEImportColumnMatching = class(TQEImportFrame)
    chkColumnTitles: TCheckBox;
    cmbFields: TComboBox;
    Label2: TLabel;
    sgMap: TDSSStringGrid;
    procedure chkColumnTitlesClick(Sender: TObject);
    procedure cmbFieldsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbFieldsSelect(Sender: TObject);
    procedure sgMapCellSelectedCustom(Sender: TObject; ACol, ARow: Integer;
        WinControl: TWinControl);
  protected
    procedure CheckMappings; virtual;
    function GetNext: TQEImportFrameClass; override;
    procedure LoadColumnNames; virtual;
    procedure LoadImportData; virtual;
    procedure LoadSessionFieldNames; virtual;
    procedure Resize; override;
  public
    constructor Create(Definition: TQEImportDefinition); override;
    destructor Destroy; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;



  
implementation

{$R *.dfm}

uses
  QEImportProgress, GeneralData, Types;

{-==============================================================================
    TfraQEImportColumnMatching
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraQEImportColumnMatching. Create(Definition: TQEImportDefinition);
begin
  inherited;
  chkColumnTitles.Visible := not (Definition.ImportType in itDatabase);
  sgMap.Rows[0].CommaText := Format(
      '"%0:s","%1:s"', 
      [ResStr_ColumnsInFile, ResStr_FieldsInSession]);
  sgMap.ColumnsInfo[1].WinControl := cmbFields;
end;  // TfraQEImportColumnMatching.Create 

{-------------------------------------------------------------------------------
}
destructor TfraQEImportColumnMatching.Destroy;
begin
  inherited;
end;  // TfraQEImportColumnMatching.Destroy

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.CheckMappings;
var
  I: Integer;
  lNone: Boolean;
begin
  lNone := True;
  for I := 1 to sgMap.RowCount - 1 do
    if sgMap.Cells[1, I] <> '' then
    begin
      lNone := False;
      Break;
    end;
    
  if lNone then SetIsComplete(False);      
end;  // TfraQEImportColumnMatching.CheckMappings 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.chkColumnTitlesClick(Sender: TObject);
begin
  LoadColumnNames;
end;  // TfraQEImportColumnMatching.chkColumnTitlesClick 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.cmbFieldsKeyDown(Sender: TObject; var Key:
    Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_DELETE, VK_BACK: begin
      Key := 0;
      cmbFields.ItemIndex := -1;
      cmbFieldsSelect(cmbFields);
    end;
  end;
end;  // TfraQEImportColumnMatching.cmbFieldsKeyDown

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.cmbFieldsSelect(Sender: TObject);
var
  I: Integer;
begin
  inherited;        
  sgMap.Cells[sgMap.Col, sgMap.Row] := cmbFields.Text;
  if cmbFields.ItemIndex = -1 then
    CheckMappings
  else
  begin
    for I := 1 to sgMap.RowCount - 1 do
      if (I <> sgMap.Row) and (sgMap.Cells[sgMap.Col, I] = cmbFields.Text) then
        sgMap.Cells[sgMap.Col, I] := '';
    SetIsComplete(True);
  end;
end;  // TfraQEImportColumnMatching.cmbFieldsSelect 

{-------------------------------------------------------------------------------
}
function TfraQEImportColumnMatching.GetNext: TQEImportFrameClass;
begin
  Result := TfraQEImportProgress;
end;  // TfraQEImportColumnMatching.GetNext 

{+------------------------------------------------------------------------------
  Load any state when the frame is displayed.
}
procedure TfraQEImportColumnMatching.LoadContent;
begin
  LoadSessionFieldNames;
  LoadImportData;
  if Definition.HasImportData then LoadColumnNames;
end;  // TfraQEImportColumnMatching.LoadContent

{+------------------------------------------------------------------------------
  Load data from import source as specified in definition.
}
procedure TfraQEImportColumnMatching.LoadImportData;
begin
  try
    Definition.LoadImportData;

    if not Definition.HasImportData then
    begin
      MessageDlg(ResStr_NoImportData, mtWarning, [mbOk], 0);
      Definition.CloseWindow;
    end;
  except
    on E: EFOpenError do
    begin
      MessageDlg(
          Format(ResStr_FileOpenError, [E.Message]),
          mtWarning,
          [mbOk],
          0);
      Definition.CloseWindow;
    end;
  end;
end;  // TfraQEImportColumnMatching.LoadImportData

{+------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.LoadColumnNames;
var
  I: Integer;
begin
  sgMap.RowCount := Definition.ImportFieldCount + 1;
  for I := 1 to sgMap.RowCount - 1 do
    if chkColumnTitles.Checked then
      sgMap.Cells[0, I] := Definition.ImportFieldNames[I - 1]
    else
      sgMap.Cells[0, I] := Format(ResStr_DefaultColumnName, [I]);
end;  // TfraQEImportColumnMatching.LoadColumnNames 

{+------------------------------------------------------------------------------
  Populate combo box with field names from Quick Entry session.
}
procedure TfraQEImportColumnMatching.LoadSessionFieldNames;
var
  I: Integer;
begin
  cmbFields.Clear;
  for I := 0 to Definition.SessionFieldCount - 1 do
    cmbFields.Items.Add(Definition.SessionFieldLabels[I]);        
end;  // TfraQEImportColumnMatching.LoadSessionFieldNames

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.Resize;
var
  lGridWidth: Integer;
begin
  with sgMap do
  begin
    lGridWidth := ClientWidth 
        - 2 * GridLineWidth 
        - GetSystemMetrics(SM_CXVSCROLL);
    ColWidths[0] := lGridWidth div 2;
    ColWidths[1] := lGridWidth - ColWidths[0];
  end;
  inherited;  
end;  // TfraQEImportColumnMatching.Resize 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.SaveContent;
var
  I: Integer;
begin
  Definition.ImportFieldNamesFromData := chkColumnTitles.Checked;
  Definition.ClearColumnMappings;
  
  for I := 1 to sgMap.RowCount - 1 do
    if sgMap.Cells[1, I] <> '' then
      Definition.AddColumnMapping(
          cmbFields.Items.IndexOf(sgMap.Cells[1, I]),
          I - 1);
end;  // TfraQEImportColumnMatching.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraQEImportColumnMatching.sgMapCellSelectedCustom(
    Sender: TObject; ACol, ARow: Integer; WinControl: TWinControl);
var
  lRect: TRect;
begin
  inherited;
  cmbFields.ItemIndex := cmbFields.Items.IndexOf(sgMap.Cells[ACol, ARow]);
  // fix up combo box position since TDSSStringGrid gets it wrong
  lRect := sgMap.CellRect(ACol, ARow);
  cmbFields.SetBounds(
      lRect.Left,
      lRect.Top,
      lRect.Right - lRect.Left,
      lRect.Bottom - lRect.Top);
end;  // TfraQEImportColumnMatching.sgMapCellSelectedCustom 

end.
