{===============================================================================
  Unit:         SubjectAreaDetails

  Defines:      TfraSubjectAreaDetails

  Description:  Subject Area details General Tab frame.

  Created:      June 2003

  Last revision information:
    $Revision: 8 $
    $Date: 18/10/04 11:53 $
    $Author: Johnvanbreda $

===============================================================================}

unit SubjectAreaGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ExtCtrls, RestrictedEdits,
  DataTypes, GeneralData;

type
  {-----------------------------------------------------------------------------
    General details tab for a subject area.  This is embedded onto
    TfraSubjectArea when required.
  }
  TfraSubjectAreaGeneral = class(TBaseTabSheetFrame)
    eItemName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mmComment: TMemo;
  private
    FTimestamp: TSQLSvrTimestamp;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings, ThesaurusApplicationSettings;

//==============================================================================
{-==============================================================================
    TfraSubjectAreaGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSubjectAreaGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_SubjectArea_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
  with ThesApplicationSettings do
    if LogDeletions then LogDeletion('usp_SubjectArea_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraSubjectAreaGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Toggle the colour of the required controls. 
}
procedure TfraSubjectAreaGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  SetRequiredFieldsColourState(AEnabled, [eItemName]);
end;  // TfraSubjectAreaGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Return the caption for the treeview. 
}
function TfraSubjectAreaGeneral.GetCaption: string;
begin
  Result := eItemName.Text;
end;  // TfraSubjectAreaGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Load the timestamp. 
}
procedure TfraSubjectAreaGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].EOF then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TfraSubjectAreaGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraSubjectAreaGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_SubjectArea_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eItemName, 'Item_Name', True, ResStr_SubjectArea);
  RegisterControl(mmComment, 'Comment');
end;  // TfraSubjectAreaGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Save the data. 
}
procedure TfraSubjectAreaGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@ItemName', eItemName.Text,
                          '@Comment', mmComment.Text,
                          '@Timestamp', FTimestamp,
                          '@SystemSuppliedData', 0
                          ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc('Subject_Area',
                                                    'usp_SubjectArea_Insert',
                                                    lParams,
                                                    '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_SubjectArea_Update', lParams);
end;  // TfraSubjectAreaGeneral.SaveData 


end.






