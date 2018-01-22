{===============================================================================
  Unit:         ConceptGroupQualityCheckHistory

  Defines:      THistoryItem
                THistoryList
                TdlgConceptGroupQualityCheckHistory

  Description:  Dialog showing the quality check history for a concept group

  Created:      May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 24/05/11 16:35 $
    $Author: Jamesbichard $

===============================================================================}

unit ConceptGroupQualityCheckHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DssStringGrid, ExtCtrls, GeneralData, DataClasses,
  LuxembourgDataClasses, ADOInt, ResourceStrings;

type

  TdlgConceptGroupQualityCheckHistory = class;

  THistoryItem = class(TLuxGridDataItem)
  private
    FCheckedDateTime: string;
    FCheckedBy: string;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString);
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString);
        override;
  public
    property CheckedDateTime: string read FCheckedDateTime;
    property CheckedBy: string read FCheckedBy;
  end;

  THistoryList = class(TLuxGridDataList)
  private
    FFrame: TdlgConceptGroupQualityCheckHistory;
  protected
    function GetRecordset: _Recordset; override;
  public
    property Frame: TdlgConceptGroupQualityCheckHistory read FFrame write FFrame;
  end;

  TdlgConceptGroupQualityCheckHistory = class(TForm)
    pnGrid: TPanel;
    pnButtons: TPanel;
    btnClose: TButton;
    sgHistory: TDSSStringGrid;
    procedure btnCloseClick(Sender: TObject);
  private
    FConceptGroupKey: TKeyString;
    FHistoryList: THistoryList;
    { Private declarations }
  public
    { Public declarations }
    procedure LoadHistory(AKey: TKeyString);
    property ConceptGroupKey: TKeyString
        read FConceptGroupKey
        write FConceptGroupKey;
  end;

var
  dlgConceptGroupQualityCheckHistory: TdlgConceptGroupQualityCheckHistory;

implementation

{$R *.dfm}

const
  COL_LASTCHECKED = 0;
  COL_LASTCHECKEDBY = 1;

{-==============================================================================
    THistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure THistoryItem.GetData(const Column: Integer; var AText: String; var AKey:
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := CheckedDateTime;
    1: AText := CheckedBy;
  end;
end;  // THistory.GetData

{-------------------------------------------------------------------------------
}
procedure THistoryItem.InitFromRecord(AFields: Fields);
begin
  // Set DomainConceptGroupKey for list.
  // Set item's properties.
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FCheckedDateTime  := AFields['Checked_Date_Time'].Value;
  FCheckedBy := AFields['Checked_By'].Value;
end;  // THistoryItem.InitFromRecord

{-------------------------------------------------------------------------------
}
procedure THistoryItem.SetData(const Column: Integer; const AText: String; const AKey:
    TKeyString);
begin
  case Column of
    0: if FCheckedDateTime <> AText then begin
         FCheckedDateTime := AText;
         SetModified;
       end;
    1: if (FCheckedBy <> AText) then
       begin
         FCheckedBy := AText;
         SetModified;
       end;
  end;
end;  // THistoryItem.SetData

{-==============================================================================
    THistoryList
===============================================================================}
{-------------------------------------------------------------------------------
}
function THistoryList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_ConceptGroup_Select_History',
                                   ['@Key', MasterKey]);
end;  // THistoryList.GetRecordset

{-==============================================================================
    TdlgConceptGroupQualityCheckHistory
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckHistory.LoadHistory(AKey: TKeyString);
begin
  sgHistory.Cells[COL_LASTCHECKED, 0] := ResStr_LastChecked;
  sgHistory.Cells[COL_LASTCHECKEDBY, 0] := ResStr_LastCheckedBy;

  FHistoryList := THistoryList.Create(THistoryItem, sgHistory);
  FHistoryList.MasterKey := AKey;
  FHistoryList.Refresh;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgConceptGroupQualityCheckHistory.btnCloseClick(
  Sender: TObject);
begin
  Close;
end;

end.
