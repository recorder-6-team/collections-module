{===============================================================================
  Unit:        QuickEntryData.pas

  Defines:     TdmQuickEntry

  Description: Data module for use in the QuickEntry screens.

  Model:       QuickEntry.mpb

  Created:     August 2003

  Last revision information:
    $Revision: 20 $
    $Date: 5/07/16 9:25 $
    $Author: Christopherknight $

===============================================================================}
unit QuickEntryData;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, DataTypes, LuxIDComboBox, ExceptionForm;

resourcestring
  ResStr_QEDataItemInsertFailed = 'Failure to insert data item into QE_Data_Item table.';

type
  EQEData = class(TExceptionPath);

  TdmQuickEntry = class (TDataModule)
  private
    procedure FillIDCombo(ACombo: TLuxIDComboBox; const AStoredProc, AItemField,
        AKeyField: String);
  public
    procedure DeleteDataItem(const ADataItemKey: integer; ATimestamp: 
        OleVariant);
    procedure DeleteSession(SessionKey: integer; Timestamp: TSQLSvrTimestamp);
    procedure FillCollectionCombo(Sender: TObject);
    procedure FillDeterminerRoleCombo(Sender: TObject);
    procedure FillDepartmentCombo(Sender: TObject);
    procedure FillRecordTypeCombo(Sender: TObject);
    procedure FillSampleTypeCombo(Sender: TObject);
    procedure FillStoreCombo(Sender: TObject);
    procedure FillSubjectAreaCombo(Sender:TObject);
    procedure FillSurveyCombo(Sender: Tobject);
    procedure FillYesNoCombo(Sender: TObject);
    procedure FillInferredCombo(Sender: TObject);
    procedure InsertDataItem(DataRowKey: integer; DataValue, DataDisplay,
        TemplateFieldKey: String;  out DataItemKey: Integer; out Timestamp:
        TSQLSvrTimestamp);
    procedure RefreshDataItem(DataItemKey: Integer; Var DataValue, DataDisplay: 
        String; Var Timestamp: TSQLSvrTimestamp);
    procedure RefreshDataRow(DataRowKey: Integer; var Validated, Processed:
        boolean; var Timestamp:TSQLSvrTimestamp);
    function SessionHasUnprocessedRecords(const ASessionKey: integer): Boolean;
    procedure UpdateDataItem(DataItemKey: Integer; DataValue, DataDisplay: 
        String; Timestamp: TSQLSvrTimestamp);
    procedure UpdateDataRow(DataRowKey: Integer; Validated, Processed: boolean;
        Timestamp: TSQLSvrTimestamp);
    procedure UpdateSessionName(SessionKey: integer; SessionName: String; 
        Timestamp: TSQLSvrTimestamp);
  end;
  
var
  dmQuickEntry: TdmQuickEntry;

//==============================================================================
implementation

{$R *.dfm}

{TODO: Some of the stored procedures called from this unit don't adhere to
naming standards. The names must be changed.}

uses
  ResourceStrings, ADODB, GeneralData, Variants, ApplicationSettings;

{-==============================================================================
    TdmQuickEntry
===============================================================================}
{-------------------------------------------------------------------------------
  Deletes a quick entry data item record. 
}
procedure TdmQuickEntry.DeleteDataItem(const ADataItemKey: integer; ATimestamp: 
    OleVariant);
begin
  dmGeneral.RunDeleteStoredProc('usp_QEDataItem_Delete',
                                ['@Key', ADataItemKey, '@Timestamp', ATimestamp]);
end;  // TdmQuickEntry.DeleteDataItem

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.DeleteSession(SessionKey: integer; Timestamp:
    TSQLSvrTimestamp);
begin
  dmGeneral.RunDeleteStoredProc('usp_QESession_Delete',
                                ['@QESessionKey', SessionKey,
                                '@Timestamp', Timestamp,
                                '@SessionID', dmGeneral.Recorder.CurrentSettings.SessionID]);
end;  // TdmQuickEntry.DeleteSession 

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillCollectionCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_CollectionList',
              'Item_Name', 'Collection_Unit_Key');
end;  // TdmQuickEntry.FillCollectionCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillDeterminerRoleCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_DeterminerRoles_Select',
              'Item_Name', 'Item_Key');
end;  // TdmQuickEntry.FillDeterminerRoleCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillDepartmentCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_InternalDepartments_Select',
              'Item_Name', 'Organisation_Department_Key');
end;  // TdmQuickEntry.FillDepartmentCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillRecordTypeCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_RecordTypes_Select', 'Item_Name', 'Item_Key');
end;  // TdmQuickEntry.FillRecordTypeCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillSampleTypeCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_SampleTypeList', 'Short_Name', 'Sample_Type_Key');
end;  // TdmQuickEntry.FillSampleTypeCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillStoreCombo(Sender: TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_StoreList', 'Item_Name', 'Collection_Unit_Key');
end;
{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillSubjectAreaCombo(Sender:TObject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_SubjectAreas_Select',
              'Item_Name', 'Subject_Area_Key');
end;  // TdmQuickEntry.FillSubjectAreaCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillSurveyCombo(Sender: Tobject);
begin
  FillIDCombo(TLuxIDComboBox(Sender), 'usp_SurveyList', 'Item_Name', 'Survey_Key');
end;  // TdmQuickEntry.FillSurveyCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillYesNoCombo(Sender: TObject);
begin
  TLuxIDComboBox(Sender).Add(ResStr_Yes, Sysutils.BoolToStr(true));
  TLuxIDComboBox(Sender).Add(ResStr_No, Sysutils.BoolToStr(false));
end;  // TdmQuickEntry.FillYesNoCombo

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillInferredCombo(Sender: TObject);
begin
  TLuxIDComboBox(Sender).Add('No', 0);
  TLuxIDComboBox(Sender).Add('!', 1);
  TLuxIDComboBox(Sender).Add('?', 2);
  TLuxIDComboBox(Sender).Add('!?', 3);
end;

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.FillIDCombo(ACombo: TLuxIDComboBox; const AStoredProc, AItemField,
    AKeyField: String);
begin
  with dmgeneral.GetRecordset(AStoredProc, []) do
    try
      while not Eof do begin
        ACombo.Add(VarToStr(Fields[AItemField].Value),
                   VarToStr(Fields[AKeyField].Value));
        MoveNext;
      end;
    finally
      Close;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.InsertDataItem(DataRowKey: integer; DataValue,
    DataDisplay, TemplateFieldKey: String;  out DataItemKey: Integer; out
    Timestamp: TSQLSvrTimestamp);
var
  lrs: _Recordset;
begin
  lrs := dmGeneral.GetRecordset('usp_QEDataItem_Insert',
                                ['@QEDataRowKey', DataRowKey, '@DataValue', DataValue,
                                 '@DataDisplay', DataDisplay,
                                 '@QETemplateFieldKey', TemplateFieldKey]);
  with lrs do
    try
      if not (bof and eof) then
      begin
        DataItemKey := Fields['QE_Data_Item_Key'].Value;
        Timestamp   := Fields['Timestamp'].value;
      end
      else
        raise EQEData.Create(ResStr_QEDataItemInsertFailed);
    finally
      Close;
    end;
end;  // TdmQuickEntry.InsertDataItem

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.RefreshDataItem(DataItemKey: Integer; Var DataValue, 
    DataDisplay: String; Var Timestamp: TSQLSvrTimestamp);
var
  lrs: _Recordset;
begin
  lrs := dmGeneral.GetRecordset('usp_QEDataItem_Select_ForKey',
                                ['@QEDataItemKey', DataItemKey]);
  with lrs do
    try
      if not (eof and bof) then
      begin
        DataValue   := VarToStr(Fields['Data_Value'].Value);
        DataDisplay := VarToStr(Fields['Data_Display'].Value);
        Timestamp   := Fields['Timestamp'].Value;
      end
    finally
      Close;
    end;
end;  // TdmQuickEntry.RefreshDataItem 

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.RefreshDataRow(DataRowKey: Integer; var Validated, 
    Processed: boolean; var Timestamp:TSQLSvrTimestamp);
var
  lrs: _Recordset;
begin
  lrs := dmGeneral.GetRecordset('usp_QEDataRow_Select_ForKey',
        ['@QEDataRowKey', DataRowKey]);
  with lrs do
    try
      if not (eof and bof) then
      begin
        Validated := Fields['Validated'].Value;
        Processed := Fields['Processed'].Value;
        Timestamp := Fields['Timestamp'].Value;
      end
    finally
      Close;
    end;
end;  // TdmQuickEntry.RefreshDataRow 

{-------------------------------------------------------------------------------
  Returns tru if the identified session has any unprocessed records. 
}
function TdmQuickEntry.SessionHasUnprocessedRecords(const ASessionKey: 
    integer): Boolean;
begin
  Result := dmGeneral.GetStoredProcOutputParam('usp_QESessionUnprocessedRows_Count',
                                               ['@Key', ASessionKey], '@Count') > 0;
end;  // TdmQuickEntry.SessionHasUnprocessedRecords

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.UpdateDataItem(DataItemKey: Integer; DataValue,
    DataDisplay: String; Timestamp: TSQLSvrTimestamp);
begin
  dmGeneral.RunUpdateStoredProc('usp_QEDataItem_Update',
                                ['@QEDataItemKey', DataItemKey, '@DataValue', DataValue,
                                 '@DataDisplay', DataDisplay, '@Timestamp', Timestamp]);
end;  // TdmQuickEntry.UpdateDataItem

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.UpdateDataRow(DataRowKey: Integer; Validated,
    Processed: boolean; Timestamp: TSQLSvrTimestamp);
begin
  dmGeneral.RunUpdateStoredProc('usp_QEDataRow_Update',
                                ['@QEDataRowKey', DataRowKey, '@Validated', Validated,
                                 '@Processed', Processed, '@Timestamp', Timestamp]);
end;  // TdmQuickEntry.UpdateDataRow

{-------------------------------------------------------------------------------
}
procedure TdmQuickEntry.UpdateSessionName(SessionKey: integer; SessionName:
    String; Timestamp: TSQLSvrTimestamp);
begin
  dmGeneral.RunUpdateStoredProc('usp_QESession_Update',
                                ['@QESessionKey', SessionKey, '@QESessionName', SessionName,
                                 '@Timestamp', Timestamp]);
end;  // TdmQuickEntry.UpdateSessionName


end.









