unit QEAddNumber;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComboListID, LuxIDComboBox,
  ConceptGroupComboBox, Buttons, InterfaceDataModule, ImageListButton;

type
  TfrmQuickEntryNumber = class(TForm)
    chkPreferred: TCheckBox;
    cmbNumberType: TConceptGroupComboBox;
    Bevel1: TBevel;
    Label1: TLabel;
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure ValidateData;
    function GetIsPreferred: Boolean;
    function GetNumberType: String;
    function GetNumberTypeKey: String;
  public
    constructor Create(AOwner: TComponent); override; 
    procedure RemoveFromCombo(AKey: String);
    property IsPreferred: Boolean read GetIsPreferred;
    property NumberType: String read GetNumberType;
    property NumberTypeKey: String read GetNumberTypeKey;
  end;

implementation

{$R *.dfm}

uses
  ApplicationSettings, DataTypes, LuxembourgConstants, ExceptionForm,
  ResourceStrings, GeneralData, GeneralFunctions;

{-==============================================================================
    TfrmQuickEntryNumber
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmQuickEntryNumber.Create(AOwner: TComponent);
begin
  inherited;
  cmbNumberType.Color := AppSettings.MandatoryColour;
  cmbNumberType.PopulateContent;
  with dmGeneral.GetRecordset('usp_Concept_Select_ForConceptGroup', ['@ConceptGroupKey', CG_COLLECTION_UNIT_NUMBER_TYPES]) do begin
    while not Eof do begin
      cmbNumberType.Add(VarToStr(Fields['Item_Name'].Value), VarToStr(Fields['Concept_Key'].Value));
      MoveNext;
    end;
    Close;
  end;
end;  // TfrmQuickEntryNumber.Create

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryNumber.btnOKClick(Sender: TObject);
begin
  ValidateData;
  ModalResult := mrOK;
end;  // TfrmQuickEntryNumber.btnOKClick

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryNumber.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;  // TfrmQuickEntryNumber.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryNumber.RemoveFromCombo(AKey: String);
begin
  if (cmbNumberType.IDIndexOf(AKey) > -1) then
    cmbNumberType.Delete(cmbNumberType.IDIndexOf(AKey));
end;

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryNumber.GetIsPreferred: Boolean;
begin
  Result := chkPreferred.Checked;
end;

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryNumber.GetNumberType: String;
begin
  Result := cmbNumberType.Text;
end;

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryNumber.GetNumberTypeKey: String;
begin
  Result := cmbNumberType.CurrentStrID;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryNumber.ValidateData;
begin
  ValidateValue(cmbNumberType.ItemIndex <> -1, Format(ResStr_MissingData, [ResStr_NumberType]));
end;

end.
