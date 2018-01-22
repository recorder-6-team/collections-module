{===============================================================================
  Unit:        SearchTerms

  Defines:     TfraSearchTerms

  Description: Tab page for viewing and adding search terms

  Created:     Aug 2011

  Last revision information:
    $Revision: 2 $
    $Date: 8/08/11 14:02 $
    $Author: Jamesbichard $

===============================================================================}

unit SearchTerms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  Grids, DssStringGrid, GeneralData, LuxembourgDataClasses, DataClasses, ADOint,
  ResourceStrings, LuxembourgConstants, Validation, ExceptionForm, DSSDataTypes;

type
  TAdditionalTermItem = class(TLuxGridDataItem)
  private
    FSearchTerm: string;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
    procedure ValidateData; override;
  end;

  TAdditionalTermList = class (TLuxGridDataList)
  private
    FConceptKey: String;
  protected
    function AllowedAddOnKeyDown: Boolean; override;
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property ConceptKey: String read FConceptKey write FConceptKey;
  end;

  TfraSearchTerms = class(TBaseTabSheetFrame)
    lblAutomaticTerms: TLabel;
    mmAutomaticTerms: TMemo;
    lblAdditionalTerms: TLabel;
    sgAdditionalTerms: TDSSStringGrid;
    btnDel: TImageListButton;
    btnAdd: TImageListButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
  private
    FTermsList: TAdditionalTermList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadData; override;
    procedure ValidateData; override;
    procedure SaveData; override;
    procedure EnableControls(AEnabled: Boolean); override;
  end;

var
  fraSearchTerms: TfraSearchTerms;

implementation

uses BaseDetailFrameUnit;

{$R *.dfm}

{==============================================================================
    TAdditionalTermItem implementation
 ==============================================================================}

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermItem.GetData(const Column: Integer; var AText: String; var AKey:
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := FSearchTerm;
  end;
end;  // TAdditionalTermItem.GetData

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Search_Term_Key'].Value));
  FSearchTerm  := AFields['Plaintext'].Value;
end;

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  case Column of
    0: if AText <> FSearchTerm then
    begin
      FSearchTerm := AText;
      SetModified;
    end;
  end;
end;  // TAdditionalTermItem.SetData

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermItem.ValidateData;
begin
  ValidateValue(FSearchTerm <> '', Format(ResStr_MissingData, [ResStr_Document]));
end;  // TAdditionalTermItem.ValidateData

{==============================================================================
    TAdditionalTermList implementation
 ==============================================================================}

{-------------------------------------------------------------------------------
}
function TAdditionalTermList.AllowedAddOnKeyDown: Boolean;
begin
  Result := False;
end;  // TAdditionalTermList.AllowedAddOnKeyDown

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermList.DoAddition(AItem: TLuxCachedDataItem);
var
  lSearchTermKey: string;
begin
  with TAdditionalTermItem(AItem) do begin
    lSearchTermKey := VarToStr(dmGeneral.RunInsertStoredProc(TN_SEARCH_TERM,
                        'usp_SearchTerm_Insert',
                        ['@ConceptKey', FConceptKey,
                         '@SystemGenerated', false,
                         '@Plaintext', FSearchTerm],
                        '@Key'));
    SetItemKey(lSearchTermKey);
  end;
end;  // TAdditionalTermList.DoAddition

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TAdditionalTermItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_SearchTerm_Delete',
                                  ['@Key', ItemKey]);
end;  // TAdditionalTermList.DoDeletion

{-------------------------------------------------------------------------------
}
procedure TAdditionalTermList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TAdditionalTermItem(AItem) do begin
    dmGeneral.RunUpdateStoredProc('usp_SearchTerm_Update',
                                  ['@Key', ItemKey,
                                   '@Plaintext', FSearchTerm]);
  end;
end;  // TAdditionalTermList.DoModification

{-------------------------------------------------------------------------------
}
function TAdditionalTermList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_SearchTerm_Select',
                                  ['@ConceptKey', FConceptKey,
                                  '@SystemGenerated', false]);
end;  // TAdditionalTermList.GetRecordset

{==============================================================================
    TfraSearchTerms implementation
 ==============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraSearchTerms.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTermsList := TAdditionalTermList.Create(TAdditionalTermItem, sgAdditionalTerms);
  EnableControls(EditMode = emEdit);
  sgAdditionalTerms.Rows[0].Text := ResStr_SearchTerms;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSearchTerms.LoadData;
begin
  inherited;
  FTermsList.ConceptKey := Key;
  FTermsList.Refresh;
end;

{-------------------------------------------------------------------------------
}
procedure TfraSearchTerms.ValidateData;
begin
  inherited;
  FTermsList.ValidateContent;
end;

{-------------------------------------------------------------------------------

}
procedure TfraSearchTerms.SaveData;
begin
  inherited;
  if (FTermsList.ConceptKey = '') and (Key <> '') then
    FTermsList.ConceptKey := Key;
  FTermsList.Update;
end;

{-------------------------------------------------------------------------------

}
procedure TfraSearchTerms.btnAddClick(Sender: TObject);
var
  lItem: TAdditionalTermItem;
begin
  inherited;
  lItem := TAdditionalTermItem.CreateNew(FTermsList);
  FTermsList.AddNew(lItem);
  EnableControls(true);
end;

{-------------------------------------------------------------------------------
  Removes the selected row from the grid.
}
procedure TfraSearchTerms.btnDelClick(Sender: TObject);
var
  lItemIndex: Integer;
begin
  inherited;
  lItemIndex := sgAdditionalTerms.Row;
  FTermsList.DeleteItem(lItemIndex);
    if lItemIndex > 1 then Dec(lItemIndex);
    sgAdditionalTerms.Row := lItemIndex;
  sgAdditionalTerms.SetFocus();
  EnableControls(true);
end;

{-------------------------------------------------------------------------------
  Ensures the grid is disabled unless in edit mode.
}
procedure TfraSearchTerms.EnableControls(AEnabled: Boolean);
begin
  sgAdditionalTerms.ReadOnly := not AEnabled;
  inherited;
end;

end.
