{===============================================================================
  Unit:        BaseMaterialMovementDetailsFrameUnit.pas

  Defines:     TBaseMovementDetails

  Description:

  Model:       CollectionsMovements

  Created:     June 2003

  Last revision information:
    $Revision: 15 $
    $Date: 14/11/12 13:23 $
    $Author: Alexanderpadley $

===============================================================================}

unit BaseMaterialMovementDetailsFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, BaseFullScreenFrameUnit,
  BaseTabSheetFrameUnit, InterfaceDataModule, VagueDateEdit, DataTypes, DataClasses,
  BaseCompositeComponent, LinkedControls, Validation, BasePageControlFrameUnit,
  DropTarget, Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    Base class for to tab controls used to display the details of a material movement (
    acquisition or disposal).  The tab is embedded onto the page control on 
    TfraMovementDetails when required.
    Note that this base class never appears in the application in this form.  Tab controls are 
    inherited from this base class to provide functionality specific to the type of movement.  
    Tab controls that inherit from this base class inherit the controls on it and add 
    additional ones specific to them.
  }
  TBaseMaterialMovementFrame = class (TBaseTabSheetFrame)
    chkCompleted: TCheckBox;
    eDate: TVagueDateEdit;
    lblContact: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    mmNotes: TMemo;
    eContact: TUserEdit;
    // Filthy Hack to make TLinkedEdit available to Delphi at runtime as well.
    eHiddenEdit: TLinkedEdit;
    procedure eContactGetData(Sender: TObject);
  private
    procedure DropContact(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateContact(const AKeyList: IKeyList);
  protected
    FOutbound: Boolean;
    FTimestamp: TSQLSvrTimestamp;
    function MovementTypeToOutbound(AMovementType: Integer): Boolean;
    procedure RegisterControls; override;
  public
    procedure DoRegisterControls; virtual; abstract;
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

uses
  LuxembourgConstants, ResourceStrings, BaseDetailFrameUnit, GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TBaseMaterialMovementFrame
===============================================================================}
{-------------------------------------------------------------------------------
  Handle the dropping of an individual on to eContact. 
}
procedure TBaseMaterialMovementFrame.DropContact(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eContact, TN_INDIVIDUAL,
                     'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TBaseMaterialMovementFrame.DropContact 

{-------------------------------------------------------------------------------
}
procedure TBaseMaterialMovementFrame.eContactGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateContact, TN_NAME);
end;  // TBaseMaterialMovementFrame.eContactGetData

{-------------------------------------------------------------------------------
  Load the timestamp.
}
procedure TBaseMaterialMovementFrame.LoadData;
begin
  inherited LoadData;
  
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TBaseMaterialMovementFrame.LoadData 

{-------------------------------------------------------------------------------
}
function TBaseMaterialMovementFrame.MovementTypeToOutbound(AMovementType: Integer): Boolean;
begin
  case AMovementType of
    0,3,6,9 : Result := False;
    2,4,5,7,8 : Result := True;
    // If the movement type is an exchange, whether we are dealing with inbound/
    // outbound needs to be retrieved from the leaf nodes because an
    // Exchange had two directions.
    1 : Result := StrToBool(AdditionalProperties.GetProperty(PROP_MOVEMENT_OUTBOUND));
  else
    raise EBrowserFrameError.Create(ResStr_IncorrectMovementType);
  end;
end;  // TBaseMaterialMovementFrame.MovementTypeToOutbound

{-------------------------------------------------------------------------------
  Register the controls. 
}
procedure TBaseMaterialMovementFrame.RegisterControls;
begin
  inherited;

  // To get the recordset registered before the controls themselves.
  DoRegisterControls();

  RegisterControl(eContact, 'Contact_Name', 'Contact_Name_Key', False, ResStr_Contact,
                  CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  eContact.OnGetData := eContactGetData;
  
  RegisterControl(eDate, '', True, ResStr_Date);
  RegisterControl(chkCompleted, 'Completed', ResStr_Completed);
  RegisterControl(mmNotes, 'Notes', ResStr_Notes);
end;  // TBaseMaterialMovementFrame.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag and drop components. 
}
procedure TBaseMaterialMovementFrame.RegisterDragDropComponents;
begin
  RegisterDropComponent(eContact, DropContact, [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA]);
end;  // TBaseMaterialMovementFrame.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TBaseMaterialMovementFrame.SaveData;
begin
  ValidateContent;    // Validation doesn't happen automatically for frames
                      // that use this base class. So call it here.
  FOutbound := MovementTypeToOutbound(AdditionalProperties.GetProperty(PROP_MOVEMENT_TYPE));
end;  // TBaseMaterialMovementFrame.SaveData

{-------------------------------------------------------------------------------
}
procedure TBaseMaterialMovementFrame.UpdateContact(const AKeyList: IKeyList);
begin
  UpdateIndividualNameControl(eContact, AKeyList.GetKeyItem(0).KeyField1,
      Format(ResStr_MustBeIndividual, [LopColon(lblContact.Caption)]));
end;  // TBaseMaterialMovementFrame.UpdateLinkedName

end.

