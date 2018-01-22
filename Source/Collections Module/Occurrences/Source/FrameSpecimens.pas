{===============================================================================
  Unit:        FrameSpecimens

  Defines:     TfraSpecimens

  Description: Specimens and registration numbers associated with the current
               occurrence, if any.

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 10 $
    $Date: 2/05/08 12:17 $
    $Author: Johnvanbreda $

===============================================================================}

unit FrameSpecimens;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ImageListButton, Grids, ExtCtrls,
  DssStringGrid, LuxembourgDataClasses, DataClasses, ADOInt, DropTarget,
  DSSDataTypes, ComClasses, Recorder2000_TLB;

type
  TSpecimenList = class;

  TfraSpecimens = class (TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnDel: TImageListButton;
    sgSpecimens: TDSSStringGrid;
    shpGrid: TShape;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure sgSpecimensDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; State: TGridDrawState);
    procedure sgSpecimensDblClick(Sender: TObject);
  private
    FIsTaxonOccurrence: Boolean;
    procedure AddToList(const AKey: String);
    procedure DropSpecimen(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
    procedure UpdateButtonState;
  protected
    FSpecimenList: TSpecimenList;
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  public
    constructor Create(AOwner: TComponent); override;
    property IsTaxonOccurrence: Boolean read FIsTaxonOccurrence write FIsTaxonOccurrence;
  end;
  
  TSpecimenItem = class (TLuxGridDataItem)
  private
    FDisplayText: String;
    FSpecimenKey: TKeyString;
    procedure SetDisplayText(const Value: String);
    procedure SetSpecimenKey(const Value: TKeyString);
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); 
        override;
    procedure InitFromRecord(AFields: Fields); override;
  public
    property DisplayText: String read FDisplayText write SetDisplayText;
    property SpecimenKey: TKeyString read FSpecimenKey write SetSpecimenKey;
  end;
  
  TSpecimenList = class (TLuxGridDataList)
  private
    FIsTaxonOccurrence: Boolean;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  public
    property IsTaxonOccurrence: Boolean read FIsTaxonOccurrence write FIsTaxonOccurrence;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, Validation, SearchManager, ResourceStrings, DataTypes, LuxembourgConstants,
  InterfaceDataModule, CollectionsBrowser_TLB, BaseDetailFrameUnit;

{-==============================================================================
    TfraSpecimens
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraSpecimens.Create(AOwner: TComponent);
begin
  inherited;
  IsTaxonOccurrence := False;
  FSpecimenList := TSpecimenList.Create(TSpecimenItem, sgSpecimens);
  
  sgSpecimens.Cells[0, 0] := ResStr_DeterminationAndRegistration;
  sgSpecimens.ColumnsInfo[0].ReadOnly := True;
end;  // TfraSpecimens.Create 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.AddToList(const AKey: String);
var
  lItemName: String;
  lItem: TSpecimenItem;
begin
  if AKey = Key then
    MessageDlg(ResStr_OccurrenceCircularRef, mtInformation, [mbOk], 0)
  else begin
    lItemName := ConvertSpecimenKeyToCaption(AKey, TN_SPECIMEN_UNIT);
    if lItemName <> '' then
      if sgSpecimens.Cols[0].IndexOf(lItemName) = -1 then begin
        lItem := TSpecimenItem.CreateNew(FSpecimenList);
        lItem.DisplayText := lItemName;
        lItem.SpecimenKey := AKey;
        FSpecimenList.AddNew(lItem);
      end else
        MessageDlg(ResStr_DuplicateSpecimen, mtInformation, [mbOk], 0);
  end;
  UpdateButtonState;
end;  // TfraSpecimens.AddToList 

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.btnAddClick(Sender: TObject);
var
  lKey, lText: String;
begin
  inherited;
  if DoCheck(lKey, lText, stSpecimen) then
    AddToList(lKey);
end;  // TfraSpecimens.btnAddClick

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.btnDelClick(Sender: TObject);
begin
  inherited;
  if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Specimen]),
                mtWarning, [mbYes, mbNo], 0) = mrYes then
    FSpecimenList.DeleteItem(sgSpecimens.Row);
  UpdateButtonState;
end;  // TfraSpecimens.btnDelClick

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_SpecimenFieldData_Delete_ForOccurrence',
                                ['@OccurrenceKey', Key,
                                 '@IsTaxonOccurrence', IsTaxonOccurrence]);
end;  // TfraSpecimens.DeleteData

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.DropSpecimen(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TstringList; var AHandled: Boolean);
begin
  if EditMode = emBrowse then
    AHandled := True
  else
  if (AFormat = CF_JNCCDATA) and (ASourceData.Header.ItemCount > 0) and
     (CompareText(ASourceData.Header.TableName, TN_SPECIMEN_UNIT) = 0) then
  begin
    AHandled := True;
    AddToList(ASourceData.Items[0].KeyField1);
  end;
  UpdateButtonState;
end;  // TfraSpecimens.DropSpecimen

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.EnableControls(AEnabled: Boolean);
begin
  inherited;
  UpdateButtonState;
end;  // TfraSpecimens.EnableControls

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.LoadData;
begin
  inherited;
  FSpecimenList.MasterKey := Key;
  FSpecimenList.IsTaxonOccurrence := IsTaxonOccurrence;
  FSpecimenList.Refresh;
  UpdateButtonState;
end;  // TfraSpecimens.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.RegisterDragDropComponents;
begin
  inherited;
  RegisterDropComponent(sgSpecimens, DropSpecimen, [TN_SPECIMEN_UNIT], [CF_JNCCDATA]);
end;  // TfraSpecimens.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.SaveData;
begin
  inherited;
  FSpecimenList.Update;
end;  // TfraSpecimens.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.UpdateButtonState;
begin
  btnDel.Enabled := (EditMode <> emBrowse) and (FSpecimenList.ItemCount > 0); 
end;  // TfraSpecimens.UpdateButtonState

{-==============================================================================
    TSpecimenItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TSpecimenItem.GetData(const Column: Integer; var AText: String; var AKey: 
    TKeyString);
begin
  inherited;
  AText := FDisplayText;
end;  // TSpecimenItem.GetData 

{-------------------------------------------------------------------------------
}
procedure TSpecimenItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FSpecimenKey := AFields['Specimen_Key'].Value;
  FDisplayText := AFields['Display_Text'].Value;
end;  // TSpecimenItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TSpecimenItem.SetDisplayText(const Value: String);
begin
  FDisplayText := Value;
  SetModified;
end;  // TSpecimenItem.SetDisplayText 

{-------------------------------------------------------------------------------
}
procedure TSpecimenItem.SetSpecimenKey(const Value: TKeyString);
begin
  FSpecimenKey := Value;
  SetModified;
end;  // TSpecimenItem.SetSpecimenKey 

{-==============================================================================
    TSpecimenList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TSpecimenList.DoAddition(AItem: TLuxCachedDataItem);
var
  lOccKey, lTxOccKey: Variant;
begin
  lOccKey := Null;
  lTxOccKey := Null;
  if IsTaxonOccurrence then lTxOccKey := MasterKey
                       else lOccKey := MasterKey;
  
  with TSpecimenItem(AItem) do
    dmGeneral.RunInsertStoredProc(TN_SPECIMEN_FIELD_DATA, 'usp_SpecimenFieldData_Insert',
                                  ['@CollectionUnitKey', SpecimenKey,
                                   '@OccurrenceKey', lOccKey,
                                   '@TaxonOccurrenceKey', lTxOccKey,
                                   '@InferredSurvey', 0,
                                   '@InferredLocation', 0,
                                   '@InferredSpatialRef', 0,
                                   '@InferredSampleType', 0,
                                   '@InferredDate', 0,
                                   '@InferredCollectors', 0,
                                   '@InferredDeterminers', 0,
                                   '@GatheringEvent', 1
                                  ], '@Key');
end;  // TSpecimenList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TSpecimenList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  dmGeneral.RunDeleteStoredProc('usp_SpecimenFieldData_Delete',
                                ['@Key', TSpecimenItem(AItem).ItemKey,
                                 '@Timestamp', TSpecimenItem(AItem).Timestamp]);
end;  // TSpecimenList.DoDeletion 

{-------------------------------------------------------------------------------
}
function TSpecimenList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_SpecimenFieldData_Select_ForOccurrence',
                                   ['@OccurrenceKey', MasterKey,
                                    '@IsTaxonOccurrence', IsTaxonOccurrence]);
end;  // TSpecimenList.GetRecordset

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.sgSpecimensDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; State: TGridDrawState);
begin
  if ARow > 0 then
    dmInterface.DrawTerm(TStringGrid(Sender).Canvas,
                         Rect(ARect.Left, ARect.Top + 2, ARect.Right - 2, ARect.Bottom),
                         sgSpecimens.Cells[ACol, ARow], gdSelected in State);
end;  // TfraSpecimens.sgSpecimensDrawCell

{-------------------------------------------------------------------------------
}
procedure TfraSpecimens.sgSpecimensDblClick(Sender: TObject);
var
  specimen: TSpecimenItem;
  keyList: TEditableKeyList;
  comKeyList: TComKeyList;
begin
  inherited;

  specimen := TSpecimenItem(sgSpecimens.Objects[0, sgSpecimens.Row]);
  if Assigned(specimen) then begin
    keyList := TEditableKeyList.Create();
    keyList.SetTable(TN_SPECIMEN_UNIT);
    keyList.AddItem(specimen.SpecimenKey, '');
    comKeyList := TComKeyList.Create(keyList);

    // Start an instance of Collections Browser, or use current one.
    dmGeneral.Recorder.ShowActiveForm(CLASS_frmCBMain);

    dmGeneral.Recorder.DisplayData(TN_SPECIMEN_UNIT, comKeyList as IKeyList);
  end;
end;  // TfraSpecimens.sgSpecimensDblClick

end.
