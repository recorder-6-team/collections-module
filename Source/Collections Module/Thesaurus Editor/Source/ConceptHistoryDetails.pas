{===============================================================================
  Unit:        ConceptHistoryDetails

  Defines:     TfraConceptHistoryDetails

  Description: Details frame for history of a concept

  Created:     Dec 2003

  Last revision information:
    $Revision: 7 $
    $Date: 18/10/04 11:53 $
    $Author: Johnvanbreda $

===============================================================================}
unit ConceptHistoryDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, VagueDateEdit,
  ComboListID, LuxIDComboBox, DataTypes, DataClasses, LuxembourgConstants,
  ExceptionForm, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Screen allowing a single range of concept group versions or dates that a
    concept is applicable for to be defined.
  }
  TfraConceptHistoryDetails = class(TBaseTabSheetFrame)
    cmbFromVersion: TLuxIDComboBox;
    cmbToVersion: TLuxIDComboBox;
    eFromDate: TVagueDateEdit;
    eToDate: TVagueDateEdit;
    gbFrom: TGroupBox;
    gbTo: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure cmbFromVersionChange(Sender: TObject);
    procedure cmbToVersionChange(Sender: TObject);
    procedure cmbVersionPopulate(Sender: TObject);
  private
    FCaptionWithoutDates: TStringList;
    FFromVagueDateCGV: string;
    FSequenceList: TStringList;
    FTimestamp: TSQLSvrTimestamp;
    FToVagueDateCGV: string;
  protected
    procedure DeleteData; override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
implementation

{$R *.dfm}

uses GeneralData;

{-==============================================================================
    TfraConceptHistoryDetails
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Creates the string list 
}
constructor TfraConceptHistoryDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FSequenceList := TStringList.Create;
  FCaptionWithoutDates := TStringList.Create;
end;  // TfraConceptHistoryDetails.Create 

{-------------------------------------------------------------------------------
  Destructor. Gets rid of the string list. 
}
destructor TfraConceptHistoryDetails.Destroy;
begin
  FSequenceList.Free;
  FCaptionWithoutDates.Free;
  inherited;
end;  // TfraConceptHistoryDetails.Destroy 

{-------------------------------------------------------------------------------
  On selection of an item, cmbToVersion and eToDate are cleared if they refer
          to a version prior to the selected version (i.e. if the Sequence
          field is lower in the concept group version referred to by
          cmbToVersion).  eFromDate is set to the concept group versions From
          vague date.
}
procedure TfraConceptHistoryDetails.cmbFromVersionChange(Sender: TObject);
begin
  inherited;
  
  if FSequenceList.Values[cmbFromVersion.CurrentStrID] >
     FSequenceList.Values[cmbToVersion.CurrentStrID] then
  begin
    cmbToVersion.ItemIndex := 0;
    eToDate.Text := FToVagueDateCGV;
  end;
end;  // TfraConceptHistoryDetails.cmbFromVersionChange 

{-------------------------------------------------------------------------------
  On selection of an item, cmbFromVersion and eFromDate are cleared if they
          refer to a version after to the selected version (i.e. if the
          Sequence field is higher in the concept group version referred to by
          cmbFromVersion).  eToDate is set to the concept group versions To
          vague date.
}
procedure TfraConceptHistoryDetails.cmbToVersionChange(Sender: TObject);
begin
  inherited;
  
  if FSequenceList.Values[cmbFromVersion.CurrentStrID] <
     FSequenceList.Values[cmbToVersion.CurrentStrID] then
  begin
    cmbFromVersion.ItemIndex := 0;
    eFromDate.Text := FFromVagueDateCGV;
  end;
end;  // TfraConceptHistoryDetails.cmbToVersionChange 

{-------------------------------------------------------------------------------
  Populate the Version combo box. The Sequence number of items in the combo box
          is also required. This extra information cannot be stored directly in
          the combo box, so it is stored is a string list.
}
procedure TfraConceptHistoryDetails.cmbVersionPopulate(Sender: TObject);
var
  lFillSequenceList: Boolean;
begin
  inherited;
  
  if FSequenceList.Count = 0 then
    lFillSequenceList := True
  else
    lFillSequenceList := False;
  
  with dmGeneral.GetRecordset('usp_ConceptGroupVersions_Select_ForConcept',
                              ['@ConceptKey', ParentKey]) do begin
    while not EOF do begin
      if Sender = cmbToVersion then
        cmbToVersion.Add(VarToStr(Fields['Item_Name'].Value) + ' '
                            + VarToStr(Fields['Dates'].Value),
                            VarToStr(Fields['Key'].Value))
      else
        cmbFromVersion.Add(VarToStr(Fields['Item_Name'].Value) + ' '
                            + VarToStr(Fields['Dates'].Value),
                            VarToStr(Fields['Key'].Value));
  
      if lFillSequenceList then begin
        FSequenceList.Add(VarToStr(Fields['Key'].Value) + '=' +
                                        VarToStr(Fields['Sequence'].Value));
        FCaptionWithoutDates.Add(VarToStr(Fields['Key'].Value) + '=' +
                                        VarToStr(Fields['Item_Name'].Value));
      end;
  
      MoveNext;
    end;
  end;
end;  // TfraConceptHistoryDetails.cmbVersionPopulate 

{-------------------------------------------------------------------------------
  Deletes the record that was used to populate the frame with data. 
}
procedure TfraConceptHistoryDetails.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptHistory_Delete',
                               ['@Key', Key,
                                '@Timestamp', FTimestamp]);
end;  // TfraConceptHistoryDetails.DeleteData

{-------------------------------------------------------------------------------
  Returns the caption.
}
function TfraConceptHistoryDetails.GetCaption: string;
begin
  // From
  Result := FCaptionWithoutDates.Values[cmbFromVersion.CurrentStrID];
  if eFromDate.Text <> '' then
    Result := Result + ' (' + eFromDate.Text + ')';
  // arrow separator
  if cmbToVersion.CurrentStrID + eToDate.Text<>'' then
    Result := Result + ' ->';
  // To
  if cmbToVersion.CurrentStrID <> '' then
    Result := Result + ' ' + FCaptionWithoutDates.Values[cmbToVersion.CurrentStrID];
  if eToDate.Text <> '' then
    Result := Result + ' (' + eToDate.Text + ')';
end;  // TfraConceptHistoryDetails.GetCaption 

{-------------------------------------------------------------------------------
  Stores the timestamp and the from and to vague dates of the concept group
          version.
}
procedure TfraConceptHistoryDetails.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
  
  FFromVagueDateCGV := eFromDate.Text;
  FToVagueDateCGV := eToDate.Text;
  cmbVersionPopulate(Self);
end;  // TfraConceptHistoryDetails.LoadData 

{-------------------------------------------------------------------------------
  Registers the name of the recordset and the controls. 
}
procedure TfraConceptHistoryDetails.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ConceptHistory_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eFromDate, 'From');
  RegisterControl(cmbFromVersion, 'Concept_Group_Version_From_Name',
       'Concept_Group_Version_From_Key');
  RegisterControl(eToDate, 'To');
  RegisterControl(cmbToVersion, 'Concept_Group_Version_To_Name',
      'Concept_Group_Version_To_Key');
end;  // TfraConceptHistoryDetails.RegisterControls 

{-------------------------------------------------------------------------------
  Saves an inserted or updated record. 
}
procedure TfraConceptHistoryDetails.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@ConceptKey', ParentKey,
                          '@ConceptGroupVersionFromKey', cmbFromVersion.CurrentStrID,
                          '@ConceptGroupVersionToKey', cmbToVersion.CurrentStrID,
                          '@FromVagueDateStart', eFromDate.VagueDate.StartDate,
                          '@FromVagueDateEnd', eFromDate.VagueDate.EndDate,
                          '@FromVagueDateType', eFromDate.VagueDate.DateTypeString,
                          '@ToVagueDateStart', eToDate.VagueDate.StartDate,
                          '@ToVagueDateEnd', eToDate.VagueDate.EndDate,
                          '@ToVagueDateType', eToDate.VagueDate.DateTypeString,
                          '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONCEPT_HISTORY,
                                                  'usp_ConceptHistory_Insert',
                                                  lParams,
                                                  '@Key'
                                                  ))
  else
    dmGeneral.RunUpdateStoredProc('usp_ConceptHistory_Update', lParams);
end;  // TfraConceptHistoryDetails.SaveData 

end.







