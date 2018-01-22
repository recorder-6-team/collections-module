{===============================================================================
  Unit:        QEAddMetadata.pas

  Defines:     TfrmQuickEntryMetadata

  Description: Implements the Quick Entry Add Metadata form

  Model:       QuickEntryManager.mpb

  Created:     September 2007

  Last revision information:
    $Revision: 5 $
    $Date: 17/12/07 8:42 $
    $Author: Davidkelly $

===============================================================================}
unit QEAddMetadata;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComboListID, LuxIDComboBox,
  ConceptGroupComboBox, InterfaceDataModule, ImageListButton;

type
  TfrmQuickEntryMetadata = class(TForm)
    cmbTableName: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cmbAttribute: TLuxIDComboBox;
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    Bevel1: TBevel;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cmbTableNameChange(Sender: TObject);
  private
    FKeyList: TStringList;
    procedure ValidateData;
    function GetTableName: string;
    function GetItemName: string;
    function GetMetadataTypeKey: string;
    procedure PopulateAttributes;
  public
    constructor Create(AOwner: TComponent); override; 
    procedure RemoveFromCombo(AKey: String);
    property TableName: string read GetTableName;
    property ItemName: string read GetItemName;
    property MetadataTypeKey: string read GetMetadataTypeKey;
  end;

implementation

{$R *.dfm}

uses
  GeneralData, ApplicationSettings, DataTypes, LuxembourgConstants, ExceptionForm,
  ResourceStrings, GeneralFunctions;

{-==============================================================================
    TfrmQuickEntryMetadata
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmQuickEntryMetadata.Create(AOwner: TComponent);
begin
  inherited;
  cmbTableName.Color := AppSettings.MandatoryColour;

  // List of tables that can appear in cmbTableNames
  cmbTableName.Items.Add('Specimen');

  cmbAttribute.Color := AppSettings.MandatoryColour;
  cmbAttribute.PopulateContent;
  FKeyList := TStringList.Create;

  // Populate comboboxes with default values  
  cmbTableName.ItemIndex := 0;
  PopulateAttributes;
end;  // TfrmQuickEntryMetadata.Create

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.btnOKClick(Sender: TObject);
begin
  ValidateData;
  ModalResult := mrOK;
end;  // TfrmQuickentryMetadata.btnOKClick

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;  // TfrmQuickEntryMetadata.btnCancelClick
 
{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.cmbTableNameChange(Sender: TObject);
begin
  PopulateAttributes;
end;  // TfrmQuickEntryMetadata.cmbTableNameChange

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.PopulateAttributes; 
var
  i: Integer;
begin 
  with dmGeneral.GetRecordset('usp_MetadataType_Select', ['@TableName', cmbTableName.Text]) do begin
    while not Eof do begin
      cmbAttribute.Add(VarToStr(Fields['Item_Name'].Value), VarToStr(Fields['Metadata_Type_Key'].Value));
      MoveNext;
    end;
    Close;
  end;
  for i := 0 to FKeyList.Count - 1 do
    cmbAttribute.Delete(cmbAttribute.IDIndexOf(FKeyList[i]));
end;

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.ValidateData;
begin
  ValidateValue(cmbTableName.ItemIndex <> -1, Format(ResStr_MissingData, ['Table Name']));
  ValidateValue(cmbAttribute.ItemIndex <> -1, Format(ResStr_MissingData, ['Item Name']));
end;  // TfrmQuickEntryMetadata
                               
{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMetadata.GetTableName: string;
begin
  Result := cmbTableName.Text;
end;  // TfrmQuickEntryMetadata.GetTableName

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMetadata.GetItemName: string;
begin
  Result := cmbAttribute.Text;
end;  // TfrmQuickEntryMetadata.GetItemName

{-------------------------------------------------------------------------------
}
function TfrmQuickEntryMetadata.GetMetadataTypeKey: String;
begin
  Result := cmbAttribute.CurrentStrID;
end;  // TfrmQuickEntryMetadata.GetMetadataTypeKey

{-------------------------------------------------------------------------------
}
procedure TfrmQuickEntryMetadata.RemoveFromCombo(AKey: String);
begin
  FKeyList.Add(AKey);
end;  // TfrmQuickEntryMetadata.RemoveFromCombo

end.
