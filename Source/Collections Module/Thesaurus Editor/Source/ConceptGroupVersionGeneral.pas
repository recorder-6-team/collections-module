{===============================================================================
  Unit:         ConceptGroupVersionGenerals

  Defines:      TfraConceptGroupVersionGeneral

  Description:  Frame for editing general details of a concept group version

  Created:      18/7/2003

  Last revision information:
    $Revision: 10 $
    $Date: 10/09/08 14:35 $
    $Author: Ericsalmon $

===============================================================================}

unit ConceptGroupVersionGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RestrictedEdits, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls,
  VagueDateEdit, DataTypes, GeneralData, EasyShell, LuxembourgConstants,
  ExceptionForm, VagueDate;

type
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a concept group version to be
    viewed and edited.
  }
  TfraConceptGroupVersionGeneral = class(TBaseTabSheetFrame)
    btnUrlGo: TButton;
    eDateAcquired: TVagueDateEdit;
    eDateFrom: TVagueDateEdit;
    eDateTo: TVagueDateEdit;
    eUrl: TEdit;
    eVersionNumber: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblDate: TLabel;
    procedure btnUrlGoClick(Sender: TObject);
    procedure eUrlChange(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure EnableGoButton;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); overload; override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings, ThesaurusApplicationSettings;

//==============================================================================
{-==============================================================================
    TfraConceptGroupVersionGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  When clicked, shells to the user's default internet browser and displays the
          Url specified in eUrl.
}
procedure TfraConceptGroupVersionGeneral.btnUrlGoClick(Sender: TObject);
begin
  inherited;
  ShellFile(eUrl.Text);
end;  // TfraConceptGroupVersionGeneral.btnUrlGoClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersionGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_ConceptGroupVersion_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_ConceptGroupVersion_Delete',
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
end;  // TfraConceptGroupVersionGeneral.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersionGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  EnableGoButton;
end;  // TfraConceptGroupVersionGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersionGeneral.EnableGoButton;
begin
  if eUrl.Text = '' then btnUrlGo.Enabled := False
                    else btnUrlGo.Enabled := True;
end;  // TfraConceptGroupVersionGeneral.EnableGoButton 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersionGeneral.eUrlChange(Sender: TObject);
begin
  inherited;
  EnableGoButton;
end;  // TfraConceptGroupVersionGeneral.eUrlChange 

{-------------------------------------------------------------------------------
  Returns the caption for the tree view. 
}
function TfraConceptGroupVersionGeneral.GetCaption: string;
begin
  Result := eVersionNumber.Text + ' (' + eDateFrom.Text;
  if eDateTo.Text <> '' then
    Result := Result + ' - ' + eDateTo.Text;
  Result := Result + ')';
end;  // TfraConceptGroupVersionGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Loads the timestamp. 
}
procedure TfraConceptGroupVersionGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TfraConceptGroupVersionGeneral.LoadData 

{-------------------------------------------------------------------------------
  Registers the recordset and the controls. 
}
procedure TfraConceptGroupVersionGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_ConceptGroupVersion_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eVersionNumber, 'Version', True, ResStr_Version);
  RegisterControl(eDateFrom, 'From', True, ResStr_DateVersionApplicableFrom);
  RegisterControl(eDateTo, 'To');
  RegisterControl(eDateAcquired, 'Acq', True, ResStr_DateAcquired);
  RegisterControl(eUrl, 'Url');
end;  // TfraConceptGroupVersionGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Saves the data. 
}
procedure TfraConceptGroupVersionGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@ConceptGroupKey', ParentKey,
                          '@Version', eVersionNumber.Text,
                          '@FromVagueDateStart', eDateFrom.VagueDate.StartDate,
                          '@FromVagueDateEnd', eDateFrom.VagueDate.EndDate,
                          '@FromVagueDateType', eDateFrom.VagueDate.DateTypeString,
                          '@ToVagueDateStart', eDateTo.VagueDate.StartDate,
                          '@ToVagueDateEnd', eDateTo.VagueDate.EndDate,
                          '@ToVagueDateType', eDateTo.VagueDate.DateTypeString,
                          '@AcqVagueDateStart', eDateAcquired.VagueDate.StartDate,
                          '@AcqVagueDateEnd', eDateAcquired.VagueDate.EndDate,
                          '@AcqVagueDateType', eDateAcquired.VagueDate.DateTypeString,
                          '@Url', eUrl.Text,
                          '@Timestamp', FTimestamp
                        ]);
  
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_CONCEPT_GROUP_VERSION,
                                          'usp_ConceptGroupVersion_Insert',
                                          lParams,
                                          '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_ConceptGroupVersion_Update', lParams);
end;  // TfraConceptGroupVersionGeneral.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGroupVersionGeneral.ValidateData;
begin
  inherited;
  ValidateValue(CompareVagueDateToVagueDate(StringToVagueDate(DateToStr(Date)),
      eDateAcquired.VagueDate) <> -1, Format(ResStr_DateCannotBeInFuture,
      [ResStr_DateAcquired]), eDateAcquired);
end;  // TfraConceptGroupVersionGeneral.ValidateData 

end.



