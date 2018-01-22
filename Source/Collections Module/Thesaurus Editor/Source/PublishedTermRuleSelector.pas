{===============================================================================
  Unit:        PublishedTermRuleSelector

  Defines:     TfraPublishedTermRuleSelector

  Description: Displays all available term generators in a list

  Created:     August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 17/08/11 11:54 $
    $Author: Jamesbichard $

===============================================================================}

unit PublishedTermRuleSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComboListID, LuxIDComboBox, GeneralData;

type
  TfraPublishedTermRuleSelector = class(TFrame)
    lblPublishedTermRule: TLabel;
    cmbPublishedTermRule: TLuxIDComboBox;

    procedure cmbPublishedTermRulePopulate(Sender: TObject);
  private
    FKey: string;
    procedure SetKey(const AKey: string);
    function GetKey: string;
    { Private declarations }
  public
    procedure PopulateRules;
    procedure SetDefaultItem(AText: string = '<inherited>');
    property Key: string read GetKey write SetKey;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
}
procedure TfraPublishedTermRuleSelector.PopulateRules;
begin
  with dmGeneral.GetRecordset('usp_TermGenerator_GetRules', []) do
  begin
    while not EOF do
    begin
     cmbPublishedTermRule.Add(
        Fields['Item_Name'].Value,
        VarToStr(Fields['Term_Generator_Key'].Value));
     MoveNext;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraPublishedTermRuleSelector.SetKey(const AKey: string);
begin
  cmbPublishedTermRule.ItemIndex := cmbPublishedTermRule.IDIndexOf(AKey);
  FKey := AKey;
end;

{-------------------------------------------------------------------------------
}
function TfraPublishedTermRuleSelector.GetKey: string;
begin
  Result := cmbPublishedTermRule.CurrentStrID;
end;

{-------------------------------------------------------------------------------
}
procedure TfraPublishedTermRuleSelector.cmbPublishedTermRulePopulate(
  Sender: TObject);
begin
  PopulateRules;
end;

{-------------------------------------------------------------------------------
}
procedure TfraPublishedTermRuleSelector.SetDefaultItem(AText: string);
begin
  cmbPublishedTermRule.HasNoSelectionItem := true;
  cmbPublishedTermRule.NoSelectionItemText := AText;
end;

end.
