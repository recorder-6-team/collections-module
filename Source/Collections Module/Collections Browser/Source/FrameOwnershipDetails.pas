{===============================================================================
  Unit:        BaseOwnershipFrameUnit.pas

  Defines:     TBaseOwnershipFrame

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 14 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameOwnershipDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, StdCtrls, ImageListButton, ExtCtrls,
  BaseDetailFrameUnit, VagueDateEdit, BaseCompositeComponent,
  LinkedControls, InterfaceDataModule, GeneralData, ResourceStrings,
  Validation, DataTypes, DataClasses, DropTarget, LuxembourgConstants,
  BaseTabSheetFrameUnit, Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    Details screen displayed when the user selects to view the details of an ownership 
    movement (Accession or Transfer or Ownership).
  }
  TfraOwnershipDetails = class (TBaseFullScreenFrame)
    chkCompleted: TCheckBox;
    eContact: TUserEdit;
    eDate: TVagueDateEdit;
    lblContact: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    mmNotes: TMemo;
    procedure eContactGetData(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure DropContact(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateContact(const AKeyList: IKeyList);
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    function MovementTypeToOutbound(AMovementType: Integer): Boolean;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;

//==============================================================================
implementation

uses
  ExceptionForm, GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TfraOwnershipDetails
===============================================================================}
{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraOwnershipDetails.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_MovementOfOwnership_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraOwnershipDetails.DeleteData 

{-------------------------------------------------------------------------------
  Handle a drop on to the eContact linked edit. 
}
procedure TfraOwnershipDetails.DropContact(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eContact, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraOwnershipDetails.DropContact 

{-------------------------------------------------------------------------------
}
procedure TfraOwnershipDetails.eContactGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateContact, TN_NAME);
end;  // TfraOwnershipDetails.eContactGetData

{-------------------------------------------------------------------------------
  Get the caption. 
}
function TfraOwnershipDetails.GetCaption: String;
begin
  Result := ResStr_AccessionDetails + ' - ' + eDate.text;
end;  // TfraOwnershipDetails.GetCaption 

{-------------------------------------------------------------------------------
  Load the timestamp. 
}
procedure TfraOwnershipDetails.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
     FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TfraOwnershipDetails.LoadData 

{-------------------------------------------------------------------------------
}
function TfraOwnershipDetails.MovementTypeToOutbound(AMovementType: Integer): Boolean;
begin
  case AMovementType of
    0,1,3,6,9 : Result := False;
    2,4,5,7,8 : Result := True;
  else
    raise EBrowserFrameError.Create(ResStr_IncorrectMovementType);
  end;
end;  // TfraOwnershipDetails.MovementTypeToOutbound 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraOwnershipDetails.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_MovementOfOwnership_Select');

  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eContact, 'Contact_Name', 'Contact_Name_Key', False,
                  ResStr_Contact, CheckLinkedIndividual, 'Name',
                  ConvertIndividualKeyToCaption);
  eContact.OnGetData := eContactGetData;

  RegisterControl(eDate, '', True, ResStr_Date);
  RegisterControl(chkCompleted, 'Completed', ResStr_Completed);
  RegisterControl(mmNotes, 'Notes', ResStr_Notes);
end;  // TfraOwnershipDetails.RegisterControls

{-------------------------------------------------------------------------------
  Register the drag and drop components.
}
procedure TfraOwnershipDetails.RegisterDragDropComponents;
begin
  RegisterDropComponent(eContact, DropContact, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TfraOwnershipDetails.RegisterDragDropComponents

{-------------------------------------------------------------------------------
  Save the frame's data to the database.
}
procedure TfraOwnershipDetails.SaveData;
var
  lParams: Array of Variant;
  lOutbound: Boolean;
begin
  inherited;    // For validation to be called.
  lOutbound := MovementTypeToOutbound(AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE));
  lParams := VarArrayOf(['@Key', Key,
                         '@ParentKey', ParentKey,
                         '@MovementType',0,
                         '@OtherPartyNameKey', NULL,
                         '@StaffResponsibleNameKey', NULL,
                         '@ContactNameKey', eContact.Key,
                         '@VagueDateStart', eDate.VagueDate.StartDate,
                         '@VagueDateEnd', eDate.VagueDate.EndDate,
                         '@VagueDateType', eDate.VagueDate.DateTypeString,
                         '@Notes', mmNotes.Text,
                         '@Outbound', lOutbound,
                         '@Completed',chkCompleted.checked,
                         '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_MOVEMENT_OF_OWNERSHIP,
                                                  'usp_MovementOfOwnership_Insert',
                                                  lParams, '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_MovementOfOwnership_Update', lParams);
end;  // TfraOwnershipDetails.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraOwnershipDetails.UpdateContact(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eContact, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblContact.Caption)]));
end;  // TfraOwnershipDetails.UpdateLinkedName

{-------------------------------------------------------------------------------
  Validation method. 
}
procedure TfraOwnershipDetails.ValidateData;
begin
  inherited;
  if eContact.Key <> '' then
    ValidateValue(CheckIsIndividual(eContact.Key),
                  Format(ResStr_MustBeIndividual, [ResStr_Contact]));
end;  // TfraOwnershipDetails.ValidateData

end.


