{===============================================================================
  Unit:        ConceptGroupQualityCheckerOptions

  Defines:     TdlgConceptGroupQualityCheckerOptions

  Description: Checklist import dialog

  Created:     Dec 2003

  Last revision information:
    $Revision: 10 $
    $Date: 9/06/11 14:29 $
    $Author: Jamesbichard $

===============================================================================}
unit ConceptGroupQualityCheckerOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataTypes, ChecklistTransferForm, StdCtrls, ImageListButton,
  ExtCtrls, DomainConceptGroupSelector, InterfaceDataModule,
  RestrictedEdits;

resourcestring
  ResStr_AllAreas   = 'All Areas';
  ResStr_AllDomains = '<All>';

type
  TdlgConceptGroupQualityCheckerOptions = class(TForm)
    gbDomainConceptGroup: TGroupBox;
    fraDCGSelector: TfraDomainConceptGroupsSelector;
    rgScanOptions: TRadioGroup;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    gbResults: TGroupBox;
    Label1: TLabel;
    eMaxRecords: TNumberEdit;
    gbDCGWithin: TGroupBox;
    fraDCGWithin: TfraDomainConceptGroupsSelector;
    procedure btnOkClick(Sender: TObject);
    procedure fraDCGSelectorcmbDomainsSelect(Sender: TObject);
    procedure fraDCGSelectorcmbDomainsKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure rgScanOptionsClick(Sender: TObject);
    procedure fraDCGWithincmbDomainsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure fraDCGWithincmbDomainsSelect(Sender: TObject);
  private
    function GetConceptGroupKey: String;
    function GetMaxRecords: Integer;
    function GetConceptGroupSearchKey: String;
    procedure UpdateSelection(DCGFrame: TfraDomainConceptGroupsSelector);
    procedure PopulateDomainCombobox(DCGFrame: TfraDomainConceptGroupsSelector);
    function CheckConceptGroup(DCGFrame: TfraDomainConceptGroupsSelector): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetComboboxes(ADomainKey: String; AConceptGroupKey: String);
    property ConceptGroupKey: String read GetConceptGroupKey;
    property MaxRecords: Integer read GetMaxRecords;
    property ConceptGroupSearchKey: String read GetConceptGroupSearchKey;
  end;

//==============================================================================
implementation

{$R *.dfm}
{-==============================================================================
    TdlgConceptGroupQualityCheckerOptions
===============================================================================}

{-------------------------------------------------------------------------------
  Sets up the domain and concept group selection frames
}
constructor TdlgConceptGroupQualityCheckerOptions.Create(AOwner: TComponent);
begin
  inherited;
  PopulateDomainCombobox(fraDCGSelector);
  PopulateDomainCombobox(fraDCGWithin);
  eMaxRecords.Text := '1000';
  fraDCGWithin.cmbDomains.ItemIndex := 1;
  UpdateSelection(fraDCGWithin);
end;

{-------------------------------------------------------------------------------
  Ensures that domain combobox is populated, and also contains an 'All Domains'
  option
}
procedure TdlgConceptGroupQualityCheckerOptions.PopulateDomainCombobox(
  DCGFrame: TfraDomainConceptGroupsSelector);
var
  lDomain: TDomainComboItem;
begin
  DCGFrame.PopulateDomainCombo;
  lDomain := TDomainComboItem.Create;
  lDomain.Initialise(itSubjectArea, '');
  DCGFrame.cmbDomains.Items.InsertObject(0, ResStr_AllAreas, lDomain);
  lDomain := TDomainComboItem.Create;
  lDomain.Initialise(itSubjectArea, '1');
  DCGFrame.cmbDomains.Items.InsertObject(1, ResStr_AllDomains, lDomain);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.btnOkClick(Sender: TObject);
begin
  inherited;
  if CheckConceptGroup(fraDCGSelector) and (rgScanOptions.ItemIndex = 0) then
    CheckConceptGroup(fraDCGWithin);
end;

{-------------------------------------------------------------------------------
  Ensures that a domain must be selected, and if 'All Domains' is not selected,
  a concept group must also be selected
}
function TdlgConceptGroupQualityCheckerOptions.CheckConceptGroup(
  DCGFrame: TfraDomainConceptGroupsSelector): Boolean;
begin
  Result := true;
  if (DCGFrame.cmbDomains.ItemIndex <> 1) and
     (DCGFrame.cmbConceptGroups.ItemIndex = -1) then
  begin
    MessageDlg('Please select a Concept Group.', mtInformation, [mbOk], 0);
    ModalResult := mrNone;
    Result := false;
    DCGFrame.cmbConceptGroups.SetFocus;
  end;
end;

{-------------------------------------------------------------------------------
}
function TdlgConceptGroupQualityCheckerOptions.GetConceptGroupKey: String;
begin
  result := fraDCGSelector.ConceptGroupKey;
end;

{-------------------------------------------------------------------------------
}
function TdlgConceptGroupQualityCheckerOptions.GetMaxRecords: Integer;
begin
  result := StrToInt(eMaxRecords.Text);
end;

{-------------------------------------------------------------------------------
}
function TdlgConceptGroupQualityCheckerOptions.GetConceptGroupSearchKey: String;
begin
  result := fraDCGWithin.ConceptGroupKey;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.fraDCGSelectorcmbDomainsSelect(Sender: TObject);
begin
  UpdateSelection(fraDCGSelector);
  fraDCGSelector.cmbDomainsSelect(Sender);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.fraDCGSelectorcmbDomainsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fraDCGSelector.cmbDomainsKeyDown(Sender, Key, Shift);
  UpdateSelection(fraDCGSelector);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.UpdateSelection(
  DCGFrame: TfraDomainConceptGroupsSelector);
begin
  if DCGFrame.cmbDomains.ItemIndex < 2 then begin
    DCGFrame.cmbConceptGroups.Clear;
    DCGFrame.cmbConceptGroups.Enabled := False;
    DCGFrame.lblConceptGroup.Enabled := False;
  end else begin
    DCGFrame.cmbConceptGroups.Enabled := True;
    DCGFrame.lblConceptGroup.Enabled := True;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.rgScanOptionsClick(
  Sender: TObject);
begin
  fraDCGWithin.Enabled := (rgScanOptions.ItemIndex = 0);
  fraDCGWithin.cmbConceptGroups.ItemIndex := -1;
  if (rgScanOptions.ItemIndex = 0) then
    fraDCGWithin.cmbDomains.ItemIndex := 1
  else
    fraDCGWithin.cmbDomains.ItemIndex := -1;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.fraDCGWithincmbDomainsKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  fraDCGWithin.cmbDomainsKeyDown(Sender, Key, Shift);
  UpdateSelection(fraDCGWithin);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckerOptions.fraDCGWithincmbDomainsSelect(
  Sender: TObject);
begin
  UpdateSelection(fraDCGWithin);
  fraDCGWithin.cmbDomainsSelect(Sender);
end;

{-------------------------------------------------------------------------------
  Sets the values in fraDCGSelector using the specified keys
}
procedure TdlgConceptGroupQualityCheckerOptions.SetComboboxes(
  ADomainKey: String;
  AConceptGroupKey: String);
begin
  if Length(ADomainKey) > 0 then
  begin
    fraDCGSelector.SelectDomain(ADomainKey);
    if Length(AConceptGroupKey) > 0 then
      fraDCGSelector.SelectConceptGroup(AConceptGroupKey);
  end;
end;

end.
