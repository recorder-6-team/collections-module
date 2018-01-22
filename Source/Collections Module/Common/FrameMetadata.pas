{===============================================================================
  Unit:        FrameMetadata.pas

  Defines:     TfraMetadata

  Description: Frame that generically handles metadata fields for various
               entities within the Thesaurus

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     17/6/2003

  Last revision information:
    $Revision: 13 $
    $Date: 14/12/07 14:20 $
    $Author: Ericsalmon $

===============================================================================}
unit FrameMetadata;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, DataClasses,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, DSSDataTypes, LuxembourgDataClasses,
  ADODB, ADOInt;

type
  TMetadataItem = class (TLuxStringDataItem)
  private
    FDescription: String;
    FItemName: String;
    FMetadataKey: TKeyString;
    FMetadataText: String;
    procedure SetMetadataText(const Value: String);
  protected
    procedure InitFromRecord(AFields: Fields); override;
  public
    property Description: String read FDescription write FDescription;
    property ItemName: String read FItemName write FItemName;
    property MetadataKey: TKeyString read FMetadataKey write FMetadataKey;
    property MetadataText: String read FMetadataText write SetMetadataText;
  end;

  TMetadataList = class (TLuxStringDataList)
  private
    FTableName: String;
  protected
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
    function GetText(AItem: TLuxCachedDataItem): String; override;
  public
    property TableName: String read FTableName write FTableName;
  end;

  {-----------------------------------------------------------------------------
    Embeddable frame that allows the user to specify metadata (descriptive data) for various
    entities within the thesaurus.  The metadata field types available depend on the table
    name of the editing screen that the frame is embedded onto.
    The frame is read only unless the user has selected to edit or add the current record.
    Data is not saved to the database untill the user clicks the Save button on the containing
    edit screen.
  }
  TfraMetadata = class (TBaseTabSheetFrame)
    Label2: TLabel;
    lbMetadataItem: TListBox;
    mmData: TMemo;
    mmLabel: TMemo;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pnlData: TPanel;
    pnlList: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure lbMetadataItemClick(Sender: TObject);
    procedure lbMetadataItemDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
        TOwnerDrawState);
    procedure mmDataExit(Sender: TObject);
  private
    FCurrentItem: TMetadataItem;
    FMetadataList: TMetadataList;
    FPreviousSelected: Integer;
    FTableName: String;
    FEnabled: boolean;
    procedure SetTableName(const AValue: String);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetHasData: Boolean; virtual;
    procedure LoadData; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TableName: String read FTableName write SetTableName;
    property HasData: Boolean read GetHasData;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData;

{-==============================================================================
    TMetadataItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMetadataItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Metadata_Type_Key'].Value));
  FItemName := AFields['Item_Name'].Value;
  FDescription := AFields['Description'].Value;
  // These fields can be null
  FMetadataKey := VarToStr(AFields['Metadata_Key'].Value);
  FMetadataText := VarToStr(AFields['Text'].Value);
end;  // TMetadataItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
procedure TMetadataItem.SetMetadataText(const Value: String);
begin
  if Value <> FMetadataText then begin
    FMetadataText := Value;
    SetModified;
  end;
end;  // TMetadataItem.SetMetadataText 

{-==============================================================================
    TMetadataList
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMetadataList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TMetadataItem(AItem) do
    // MetadataKey present, item exists in DB
    if MetadataKey <> '' then begin
      if MetadataText = '' then
        // So, should it be deleted
        dmGeneral.RunDeleteStoredProc('usp_Metadata_Delete',
                                      ['@MetadataKey', MetadataKey])
      else
        // Or just updated
        dmGeneral.RunUpdateStoredProc('usp_Metadata_Update',
                                      ['@MetadataKey', MetadataKey,
                                       '@Text', MetadataText,
                                       '@Timestamp', Timestamp]);
    end else
    // Not in DB, but do we need to really add it
    if MetadataText <> '' then
      // The output param is not really necessary here, as everything will be
      // reloaded when the whole frame is refreshed.
      MetadataKey := dmGeneral.RunInsertStoredProc('Metadata', 'usp_Metadata_Insert',
                                    ['@MetadataKey', MetadataKey,
                                     '@RecordKey', MasterKey,
                                     '@Text', MetadataText,
                                     '@MetadataTypeKey', ItemKey], '@MetadataKey');
end;  // TMetadataList.DoModification 

{-------------------------------------------------------------------------------
}
function TMetadataList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_MetadataAndType_Select',
                                   ['@TableName', TableName, '@RecordKey', MasterKey]);
end;  // TMetadataList.GetRecordset 

{-------------------------------------------------------------------------------
}
function TMetadataList.GetText(AItem: TLuxCachedDataItem): String;
begin
  Result := TMetadataItem(AItem).ItemName;
end;  // TMetadataList.GetText 

{-==============================================================================
    TfraMetadata
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraMetadata.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  FMetadataList := TMetadataList.Create(TMetadataItem, lbMetadataItem.Items);
  FPreviousSelected := -1;
  FEnabled := False;
end;  // TfraMetadata.Create 

{-------------------------------------------------------------------------------
}
destructor TfraMetadata.Destroy;
begin
  FMetadataList.Free;
  
  inherited Destroy;
end;  // TfraMetadata.Destroy 

{-------------------------------------------------------------------------------
  Handles the deleting of multiple metadata records. 
}
procedure TfraMetadata.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_Metadatas_Delete',
                               ['@Key', Key, '@TableName', FTableName]);
end;  // TfraMetadata.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraMetadata.EnableControls(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
  mmData.ReadOnly := (lbMetadataItem.ItemIndex = -1) or not FEnabled;
end;  // TfraMetadata.EnableControls 

{-------------------------------------------------------------------------------
}
function TfraMetadata.GetHasData: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FMetadataList.Count - 1 do
    if TMetadataItem(FMetadataList.Items[I]).MetadataText <> '' then
    begin
      Result := True;
      Break;
    end;
end;  // TfraMetadata.GetHasData

{-------------------------------------------------------------------------------
}
procedure TfraMetadata.lbMetadataItemClick(Sender: TObject);
begin
  inherited;

  if (EditMode = emEdit) and Assigned(FCurrentItem) then
    FCurrentItem.MetadataText := mmData.Text;

  with lbMetadataItem do
    FCurrentItem := Items.Objects[ItemIndex] as TMetadataItem;

  mmLabel.Text := FCurrentItem.Description;
  mmData.Text := FCurrentItem.MetadataText;
  mmData.ReadOnly := (lbMetadataItem.ItemIndex = -1) or not FEnabled;
  FPreviousSelected := lbMetadataItem.ItemIndex;
end;  // TfraMetadata.lbMetadataItemClick 

{-------------------------------------------------------------------------------
  Draw the list item in bold if there is some data for the selected metadata type. 
}
procedure TfraMetadata.lbMetadataItemDrawItem(Control: TWinControl; Index: Integer; Rect: 
    TRect; State: TOwnerDrawState);
begin
  inherited;
  with lbMetadataItem do begin
    Canvas.FillRect(Rect);
    if TMetadataItem(Items.Objects[Index]).MetadataText = '' then
      Canvas.Font.Style := []
    else
      Canvas.Font.Style := [fsBold];
  
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index])
  end;
end;  // TfraMetadata.lbMetadataItemDrawItem 

{-------------------------------------------------------------------------------
}
procedure TfraMetadata.LoadData;
begin
  inherited LoadData;

  FMetadataList.MasterKey := Key;
  FMetadataList.Refresh;
  FCurrentItem := nil;  
  if (FPreviousSelected <> -1)
    and (lbMetadataItem.Items.Count > FPreviousSelected) then
  begin
    lbMetadataItem.ItemIndex := FPreviousSelected;
    lbMetadataItemClick(nil);
  end;
end;  // TfraMetadata.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraMetadata.mmDataExit(Sender: TObject);
begin
  inherited;
  if (EditMode = emEdit) and Assigned(FCurrentItem) then
    FCurrentItem.MetadataText := mmData.Text;
end;  // TfraMetadata.mmDataExit 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraMetadata.SaveData;
begin
  FPreviousSelected := lbMetadataItem.ItemIndex;
  FMetadataList.Update;
end;  // TfraMetadata.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraMetadata.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  
  FMetadataList.MasterKey := Key;
end;  // TfraMetadata.SetKey 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TfraMetadata.SetTableName(const AValue: String);
begin
  FTableName := AValue;
  // Make sure the list gets the name too.
  FMetadataList.TableName := AValue;
end;  // TfraMetadata.SetTableName 

end.

