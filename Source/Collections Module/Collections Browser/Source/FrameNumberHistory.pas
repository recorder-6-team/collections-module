{===============================================================================
  Unit:        FrameNumberHistory

  Defines:     TfraNumberHistory

  Description:

  Created:

  Last revision information:
    $Revision: 17 $
    $Date: 5/11/09 14:39 $
    $Author: Simonlewis $

===============================================================================}

unit FrameNumberHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseNumberHistoryFrameUnit, StdCtrls, ExtCtrls, ConceptGroupComboBox,
  DataTypes, Barcode, ResourceStrings, ComboListID, LuxIDComboBox, BrowserNodeFramework,
  RestrictedEdits;

type
  {-----------------------------------------------------------------------------
    Tab page for viewing and editing the details of a registration number for a collection 
    unit, inherited from TBaseNumberHistoryFrame.
    The mmNotes field (inherited from TBaseNumberHistoryFrame) is linked to the 
    Collection_Unit_Number.Notes field.
  }
  TfraNumberHistory = class (TBaseNumberHistoryFrame)
    Barcode: TBarcode;
    cmbStatus: TLuxIDComboBox;
    cmbType: TConceptGroupComboBox;
    eNumber: TEdit;
    imgBarcode: TImage;
    Label1: TLabel;
    Label2: TLabel;
    ScrollBox1: TScrollBox;
    procedure cmbStatusPopulate(Sender: TObject);
    procedure cmbTypeChange(Sender: TObject);
    procedure eNumberChange(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure GenerateNumber;
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses LuxembourgConstants, GeneralData, MacroGenerator, ExceptionForm,
      GeneralFunctions, StrUtils, BaseTabSheetFrameUnit;

{-==============================================================================
    TfraNumberHistory
===============================================================================}
{-------------------------------------------------------------------------------
  Populates the Status Combo Box with values from ResourceStrings
}
procedure TfraNumberHistory.cmbStatusPopulate(Sender: TObject);
begin
  cmbStatus.Add(ResStr_Previous, 0);
  cmbStatus.Add(ResStr_Preferred, 1);
  cmbStatus.Add(ResStr_New, 2);
end;  // TfraNumberHistory.cmbStatusPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraNumberHistory.cmbTypeChange(Sender: TObject);
begin
  inherited;
  if eNumber.Text = '' then
    GenerateNumber;
end;  // TfraNumberHistory.cmbTypeChange 

{-------------------------------------------------------------------------------
  Method to delete the current Collection_Unit_Number record. 
}
procedure TfraNumberHistory.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_CollectionUnitNumber_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etRefreshSpecimenCaption, nil);
end;  // TfraNumberHistory.DeleteData

{-------------------------------------------------------------------------------
  Draws Barcode using new eNumber. 
}
procedure TfraNumberHistory.eNumberChange(Sender: TObject);
begin
  inherited;
  try
    imgBarcode.Picture := nil;
    imgBarcode.Height := 35;
    with Barcode do begin
      Height := 35;
      Top := 5;
      Left := 5;
    end;
    Barcode.Text := eNumber.Text;
    imgBarcode.Width := Barcode.Width + 10;
    Barcode.DrawBarcode(imgBarcode.Canvas);
  except on E:Exception do
    // Don't want to raise the exception about 'the barcode must be numeric'
    // Just hide it.
  end;
end;  // TfraNumberHistory.eNumberChange 

{-------------------------------------------------------------------------------
  If cmbType is set to Registration Number (or any term that has the same meaning), and the 
      collection unit is a specimen, then the macro is used to populate the field, if 
      specified. 
}
procedure TfraNumberHistory.GenerateNumber;
var
  lDepartment: String;
begin
  if TBrowserNode(AssociatedContainerNode).TopNodeContext <> ncSpecimen then Exit;

  if ((cmbType.CurrentStrID = 'SYSTEM0000000001') or
      (dmGeneral.GetStoredProcOutputParam('usp_IsSynonymOf_Get',
                                         // Concept key of Registration number
                                         ['@Key1', 'SYSTEM0000000001',
                                         '@Key2', cmbType.CurrentStrID],
                                         '@IsSynonym'))) then begin
    lDepartment := dmGeneral.GetStoredProcOutputParam
                                      ('usp_CollectionUnitDepartment_Get',
                                      ['@Key', ParentKey],
                                      '@Department');
    // Generate a number using the macro.
    eNumber.Text := dmGeneral.GetStoredProcOutputParam('usp_Macro_Get',
                                            ['@NumberType', 'Registration',
                                            '@Dept', lDepartment],
                                            '@Macro');
  end;
end;  // TfraNumberHistory.GenerateNumber 

{-------------------------------------------------------------------------------
  Gets the caption for the frame. 
}
function TfraNumberHistory.GetCaption: String;
begin
  Result := (cmbType.Text + ' - ' + eNumber.Text);
end;  // TfraNumberHistory.GetCaption 

{-------------------------------------------------------------------------------
  Loads data and stores the Timestamp. 
}
procedure TfraNumberHistory.LoadData;
begin
  // The Barcode type is loaded after the data for the frame. However, without
  // unlinking eNumber's OnChange event, it would try and construct the
  // barcode using the wrong type. So - unlink it before the data is loaded,
  // link it back up, then when the barcode's type is loaded, generate the
  // barcode.
  eNumber.OnChange := nil;
  
  inherited LoadData;
  
  if not RegisteredRecordsets[0].Eof then begin
     // Store Timestamp
     FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
  
  if not RegisteredRecordsets[1].Eof then begin
     // Set Barcode Type
     Barcode.Typ := RegisteredRecordsets[1].Fields['Data'].Value;
     eNumberChange(nil);
  end;
  
  eNumber.OnChange := eNumberChange;
end;  // TfraNumberHistory.LoadData 

{-------------------------------------------------------------------------------
  Registers the controls in the frame. 
}
procedure TfraNumberHistory.RegisterControls;
begin
  inherited RegisterControls;
  
  RegisterRecordSet('usp_CollectionUnitNumber_Select');
  RegisterControl(cmbType, 'TypeName', 'Type_Concept_Key', True, ResStr_NumberType);
  RegisterConceptGroupComboBox(cmbType, CG_COLLECTION_UNIT_NUMBER_TYPES);
  RegisterControl(cmbStatus, '', 'Preferred',True,ResStr_Status);
  RegisterControl(eNumber,'Number', True, ResStr_Number);
  RegisterRecordSet('usp_Barcode_Get');
end;  // TfraNumberHistory.RegisterControls 

{-------------------------------------------------------------------------------
  Updates or Saves new data. 
}
procedure TfraNumberHistory.SaveData;
var
  lParams: Array of Variant;
begin
  inherited;  // For validation to be called.
  lParams := VarArrayOf(['@Key', Key,
                           '@CollectionUnitKey', ParentKey,
                           '@Number', eNumber.Text,
                           '@TypeConceptKey', cmbType.CurrentStrID,
                           '@Preferred', cmbStatus.CurrentStrID,
                           '@Notes', mmNotes.Text,
                           '@Timestamp', FTimestamp]);
  
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_COLLECTION_UNIT_NUMBER,
                                            'usp_CollectionUnitNumber_Insert',
                                            lParams,
                                            '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_CollectionUnitNumber_Update', lParams);

  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etRefreshSpecimenCaption, nil);
end;  // TfraNumberHistory.SaveData

end.
