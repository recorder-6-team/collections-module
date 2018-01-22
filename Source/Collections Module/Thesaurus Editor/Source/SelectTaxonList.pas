{===============================================================================
  Unit:        SelectTaxonList

  Defines:     TdlgSelectTaxonList

  Description: Allows the user to select from a list of taxon lists.

  Created:     May 2011

  Last revision information:
    $Revision: 3 $
    $Date: 23/06/11 16:52 $
    $Author: Jamesbichard $

===============================================================================}

unit SelectTaxonList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComboListID, GeneralData, DataClasses, ADODB;

type
  TdlgSelectTaxonList = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cmbTaxonList: TIDComboBox;
    procedure cmbTaxonListPopulate(Sender: TObject);
    procedure cmbTaxonListChange(Sender: TObject);
  private
    FTaxonLists: TStringList;
    FExcludedLists: TStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetTaxonLists;
    procedure AddPreferredList(const AKey: String);
    function GetTaxonListKey: String;
    function GetTaxonListName: String;
  end;
  
var
  dlgSelectTaxonList: TdlgSelectTaxonList;

implementation

uses ADOInt;

{$R *.dfm}

{-------------------------------------------------------------------------------
}
constructor TdlgSelectTaxonList.Create(AOwner: TComponent);
begin
  inherited;
  FExcludedLists := TStringList.Create;
end;

{-------------------------------------------------------------------------------
}
function TdlgSelectTaxonList.GetTaxonListKey: String;
begin
  Result := cmbTaxonList.CurrentStrID;
end;

{-------------------------------------------------------------------------------
}
function TdlgSelectTaxonList.GetTaxonListName: String;
begin
  Result := cmbTaxonList.CurrentItem;
end;

{-------------------------------------------------------------------------------
  Populates the FTaxonList object.
}
procedure TdlgSelectTaxonList.GetTaxonLists;
var
  lRecordset: _Recordset;
  lID: TID;
begin
  if not Assigned(FTaxonLists) then
  begin
    FTaxonLists := TStringList.Create;
  end;

  lRecordset := dmGeneral.GetRecordset('usp_TaxonList_Select', []);
  with lRecordset do
  begin
    while not EOF do
    begin
      if FExcludedLists.IndexOf(Fields['Taxon_List_Key'].Value) = -1 then
      begin
        lID := TID.Create;
        lID.StrValue := Fields['Taxon_List_Key'].Value;
        FTaxonLists.AddObject(Fields['Item_Name'].Value, lID);
      end;
      MoveNext;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
destructor TdlgSelectTaxonList.Destroy;
begin
  inherited;
  FTaxonLists.Free;
  FExcludedLists.Free;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgSelectTaxonList.cmbTaxonListPopulate(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FTaxonLists) then
  begin
    for i := 0 to FTaxonLists.Count - 1 do
    begin
      cmbTaxonList.Add(
        FTaxonLists.Strings[i],
        TID(FTaxonLists.Objects[i]).StrValue);
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgSelectTaxonList.cmbTaxonListChange(Sender: TObject);
begin
    btnOk.Enabled := (cmbTaxonList.ItemIndex <> -1);
    cmbTaxonList.ShowHint := (cmbTaxonList.ItemIndex <> -1);
    if (cmbTaxonList.ItemIndex <> -1) then
      cmbTaxonList.Hint := cmbTaxonList.CurrentItem;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgSelectTaxonList.AddPreferredList(const AKey: String);
begin
  FExcludedLists.Add(AKey)
end;

end.
