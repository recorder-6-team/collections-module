{===============================================================================
  Unit:        FrameOccurrenceGeneral

  Defines:     TfraOccurrenceGeneral

  Description:

  Model:       Occurrences.mdp

  Created:     October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/03/04 16:23 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameOccurrenceGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, DBListCombo, DataTypes,
  ComboListID, LuxIDComboBox, ConceptGroupComboBox;

type
  TfraOccurrenceGeneral = class (TBaseTabSheetFrame)
    chkChecked: TCheckBox;
    chkConfidential: TCheckBox;
    cmbRecordType: TConceptGroupComboBox;
    eSurveyorRef: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label28: TLabel;
    Label8: TLabel;
    lblVerificationStatus: TLabel;
    mmComments: TMemo;
    mmCount: TMemo;
    procedure cmbRecordTypePopulate(Sender: TObject);
  private
    FChecked: Boolean;
    FCheckedBy: Variant;
    FCheckedDate: Variant;
    FDomainKey: Variant;
    FTimeStamp: TSQLSvrTimestamp;
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

uses
  ApplicationSettings, GeneralData, BaseDetailFrameUnit, ResourceStrings,
  LuxembourgConstants;

{$R *.dfm}

{-==============================================================================
    TfraOccurrenceGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraOccurrenceGeneral.cmbRecordTypePopulate(Sender: TObject);
begin
  with dmGeneral.GetRecordset('usp_RecordTypeConceptGroup_Select_ForDomain',
                              ['@Key', FDomainKey]) do
  begin
    while not Eof do begin
      cmbRecordType.Add(VarToStr(Fields['PlainText'].Value),
                        VarToStr(Fields['Concept_Key'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraOccurrenceGeneral.cmbRecordTypePopulate

{-------------------------------------------------------------------------------
}
procedure TfraOccurrenceGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Occurrence_Delete',
                                ['@Key', Key, '@Timestamp', FTimeStamp]);
end;  // TfraOccurrenceGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
function TfraOccurrenceGeneral.GetCaption: String;
begin
  // Need to locate preferred determination for correct caption!
  Result := ResStr_NoDetermination;
  with dmGeneral.GetRecordset('usp_Occurrences_Select_ForSample',
                              ['@SampleKey', ParentKey]) do
  begin
    while not Eof do begin
      if Fields['Item_Key'].Value = Key then begin
        if VarToStr(Fields['Item_Name'].Value) <> '' then
          Result := VarToStr(Fields['Item_Name'].Value);
        Break; // No need to continue.
      end;
      MoveNext;
    end;
    Close;
  end;
end;  // TfraOccurrenceGeneral.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraOccurrenceGeneral.LoadData;
begin
  inherited;
  with RegisteredRecordsets[0] do
    if not Eof then begin
      FChecked := Fields['Checked'].Value;
      FCheckedBy := Fields['Checked_By'].Value;
      FCheckedDate := Fields['Checked_Date'].Value;
      FDomainKey := Fields['Domain_Key'].Value;
      FTimeStamp := Fields['TimeStamp'].Value;
      case Fields['Verified'].Value of
        1: lblVerificationStatus.Caption := ResStr_ValidationFailed;
        2: lblVerificationStatus.Caption := ResStr_ValidationPassed;
      else
        lblVerificationStatus.Caption := ResStr_ValidationUnknown;
      end;
    end;
end;  // TfraOccurrenceGeneral.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraOccurrenceGeneral.RegisterControls;
begin
  inherited;
  RegisterRecordset('usp_Occurrence_Select');
  RegisterControl(eSurveyorRef, 'Surveyors_Ref');
  RegisterControl(cmbRecordType, 'Record_Type_Name', 'Record_Type_Key');
  RegisterControl(mmComments, 'Comment');
  RegisterControl(chkConfidential, 'Confidential');
  RegisterControl(chkChecked, 'Checked');
end;  // TfraOccurrenceGeneral.RegisterControls

{-------------------------------------------------------------------------------
}
procedure TfraOccurrenceGeneral.SaveData;
var
  lParams: Array of Variant;
  lCheckedBy, lCheckedDate: Variant;
begin
  // Initialise with original values, and see if they need updating.
  lCheckedBy   := FCheckedBy;
  lCheckedDate := FCheckedDate;
  // Modify those values only if "Checked" value has actually changed.
  if chkChecked.Checked <> FChecked then
    if chkChecked.Checked then begin
      // If box checked, update with correct values.
      lCheckedBy   := AppSettings.UserID;
      lCheckedDate := Date;
    end else begin
      // Otherwise, clear them.
      lCheckedBy   := Null;
      lCheckedDate := Null;
    end;
  
  // Setup array of parameters.
  lParams := VarArrayOf(['@Key', Key,
                         '@SurveyorsRef', eSurveyorRef.Text,
                         '@SampleKey', ParentKey,  // Ignored for update
                         '@RecordTypeKey', cmbRecordType.CurrentStrID,
                         '@Comment', mmComments.Text,
                         '@Confidential', Ord(chkConfidential.Checked),
                         '@Checked', Ord(chkChecked.Checked),
                         '@CheckedBy', lCheckedBy,
                         '@CheckedDate', lCheckedDate,
                         '@Timestamp', FTimestamp]); // Ignored for insert
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_OCCURRENCE,
                                                  'usp_Occurrence_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Occurrence_Update', lParams);
end;  // TfraOccurrenceGeneral.SaveData 

end.



