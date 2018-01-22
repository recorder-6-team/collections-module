{===============================================================================
  Unit:        DomainHyperlinks

  Defines:     TfraDomainHyperlinks

  Description:

  Created:

  Last revision information:
    $Revision: 12 $
    $Date: 14/12/07 14:21 $
    $Author: Ericsalmon $

===============================================================================}
unit DomainHyperlinks;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, InterfaceDataModule, StdCtrls,
  ImageListButton, LuxembourgDataClasses, ADOInt, DataTypes, DataClasses,
  ResourceStrings, ExtDlgs, Registry, LuxembourgConstants, ExceptionForm;

type
  TDomainHyperlinkItem = class (TLuxStringDataItem)
  private
    FImageFile: string;
    FItemName: string;
    FTimestamp: TSQLSvrTimestamp;
    FURL: string;
    FUseConceptKey: Boolean;
    FWordSeparator: string;
    function RetrieveFileName(AFilename: String): string;
  protected
    procedure InitFromRecord(AFields: Fields); override;
    procedure ValidateData; override;
  public
    property ImageFile: string read FImageFile write FImageFile;
    property ItemName: string read FItemName write FItemName;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property URL: string read FURL write FURL;
    property UseConceptKey: Boolean read FUseConceptKey write FUseConceptKey;
    property WordSeparator: string read FWordSeparator write FWordSeparator;
  end;
  
  TDomainHyperlinkList = class (TLuxStringDataList)
  private
    function GenerateFileName(AFilename: String): string;
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
    function GetText(AItem: TLuxCachedDataItem): string; override;
  end;
  
  {-----------------------------------------------------------------------------
    Tab sheet allowing the user to add, edit or delete the hyperlinks that are 
    available within the thesaurus browser for a given local domain.  Changes 
    to this screen are not applied to the database until the save button is 
    pressed on the container screen, i.e. the individual items are cached until 
    the save button is clicked.
  }
  TfraDomainHyperlinks = class (TBaseTabSheetFrame)
    btnAdd: TImageListButton;
    btnCancel: TImageListButton;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    btnGetImageFilePath: TButton;
    btnSave: TImageListButton;
    chkUseConceptKey: TCheckBox;
    dlgOpenPic: TOpenPictureDialog;
    eHyperlink: TEdit;
    eImageFilePath: TEdit;
    eItemName: TEdit;
    eWordSeparator: TEdit;
    gbDetails: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbHyperlinks: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnGetImageFilePathClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkUseConceptKeyClick(Sender: TObject);
    procedure lbHyperlinksClick(Sender: TObject);
    procedure lbHyperlinksKeyDown(Sender: TObject; var Key: Word; Shift:
            TShiftState);
  private
    FAccepted: Boolean;
    FDomainHyperlinkList: TDomainHyperlinkList;
    FEditPressed: Boolean;
    FFullImagePath: string;
    FItemIndex: Integer;
    procedure ClearDetails;
    procedure EnableContainedControls(AEnabled : Boolean);
    procedure GetImagePaths;
    procedure InternalEnableControls(AEnabled : Boolean);
    procedure RefreshDetails;
    function StripPath(AFullPath: String): string;
    procedure ListboxEmptyButtons(AButton: TImageListButton; AEnabled: Boolean);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
var
  fraDomainHyperlinks: TfraDomainHyperlinks;

implementation

uses GeneralData;

var
  mdmLocalImagePath: String;

{-==============================================================================
    TDomainHyperlinkList
===============================================================================}
{-------------------------------------------------------------------------------
  This method is called for each modified row in the String Grid 
}
procedure TDomainHyperlinkList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TDomainHyperlinkItem(AItem) do begin
    dmGeneral.RunInsertStoredProc(TN_DOMAIN_HYPERLINK,
                                  'usp_DomainHyperlink_Insert',
                                 ['@ItemName', ItemName,
                                  '@ImageFile', GenerateFileName(ImageFile),
                                  '@URL', URL,
                                  '@UseConceptKey', UseConceptKey,
                                  '@WordSeparator', WordSeparator,
                                  '@LocalDomainKey', MasterKey,
                                  '@Timestamp', Timestamp],
                                  '@Key');
  end;
end;  // TDomainHyperlinkList.DoAddition 

{-------------------------------------------------------------------------------
}
procedure TDomainHyperlinkList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TDomainHyperlinkItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_DomainHyperlink_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TDomainHyperlinkList.DoDeletion 

{-------------------------------------------------------------------------------
  This method is called for each modified row in the String Grid 
}
procedure TDomainHyperlinkList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TDomainHyperlinkItem(AItem) do begin
    dmGeneral.RunUpdateStoredProc('usp_DomainHyperlink_Update',
                                 ['@Key', ItemKey,
                                  '@ItemName', ItemName,
                                  '@ImageFile', GenerateFileName(ImageFile),
                                  '@URL', URL,
                                  '@UseConceptKey', UseConceptKey,
                                  '@WordSeparator', WordSeparator,
                                  '@LocalDomainKey', MasterKey,
                                  '@Timestamp', Timestamp
                                 ]);
  end;
end;  // TDomainHyperlinkList.DoModification 

{-------------------------------------------------------------------------------
}
function TDomainHyperlinkList.GenerateFileName(AFilename: String): string;
var
  lText: string;
begin
  lText := AFilename;
  
  while (Pos('\', lText) <> 0) do begin
    lText := Copy(lText, Pos('\', lText) + 1, Length(lText));
  end;
  
  // Compare the filenames with the paths.
  if StrLIComp(PChar(mdmLocalImagePath),
               PChar(AFilename),
               length(mdmLocalImagePath) - 1) = 0 then
    Result := '<#LOCAL_IMAGES>\' + lText
  else
    Result := AFilename;
end;  // TDomainHyperlinkList.GenerateFileName 

{-------------------------------------------------------------------------------
}
function TDomainHyperlinkList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_DomainHyperlinks_Select',
                                  ['@Key', MasterKey]);
end;  // TDomainHyperlinkList.GetRecordset 

{-------------------------------------------------------------------------------
}
function TDomainHyperlinkList.GetText(AItem: TLuxCachedDataItem): string;
begin
  Result := TDomainHyperlinkItem(AItem).ItemName;
end;  // TDomainHyperlinkList.GetText 

{-==============================================================================
    TfraDomainHyperlinks
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraDomainHyperlinks.Create(AOwner: TComponent);
begin
  inherited;
  FDomainHyperlinkList := TDomainHyperlinkList.Create(TDomainHyperlinkItem,
      lbHyperlinks.Items);
  FItemIndex := -1;
  btnAdd.Hint     := Format(ResStr_AddNewItem, [ResStr_Hyperlink]);
  btnEdit.Hint    := Format(ResStr_EditItem, [ResStr_Hyperlink]);
  btnDelete.Hint  := Format(ResStr_DeleteItem, [ResStr_Hyperlink]);
  btnSave.Hint    := Format(ResStr_AcceptItemDetails, [ResStr_Hyperlink]);
  btnCancel.Hint  := Format(ResStr_DiscardItemDetails, [ResStr_Hyperlink]);
  btnGetImageFilePath.Hint := ResStr_GetFilePath;
 //lbHyperlinks.Align..top
end;  // TfraDomainHyperlinks.Create

{-------------------------------------------------------------------------------
}
destructor TfraDomainHyperlinks.Destroy;
begin
  FDomainHyperlinkList.Free;
  inherited;
end;  // TfraDomainHyperlinks.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnAddClick(Sender: TObject);
var
  lDomainHyperlinkItem: TDomainHyperlinkItem;
begin
  inherited;
  FAccepted := False;
  InternalEnableControls(False);
  EnableContainedControls(True);
  ClearDetails;
  FEditPressed := False;
  lDomainHyperlinkItem := TDomainHyperlinkItem.CreateNew(FDomainHyperlinkList);
  lDomainHyperlinkItem.ItemName := ResStr_NewHyperlink;
  FDomainHyperlinkList.AddNew(lDomainHyperlinkItem);
  eItemName.Text := ResStr_NewHyperlink;
  lbHyperlinks.ItemIndex := lbHyperlinks.Items.Count - 1;
  FItemIndex := lbHyperlinks.ItemIndex;
end;  // TfraDomainHyperlinks.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnCancelClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
  InternalEnableControls(True);
  EnableContainedControls(False);
  
  // If the user clicked the 'add' button, then clicks cancel, the item should
  // be removed. However, if the user added an item, clicked accept, then clicked
  // edit, then cancel, the item should not be deleted - merely refreshed.
  if not FEditPressed then begin
    ItemIndex := lbHyperlinks.ItemIndex;
    FDomainHyperlinkList.DeleteItem(ItemIndex);
    if ItemIndex > 0 then lbHyperLinks.ItemIndex := ItemIndex - 1;
  end;
  RefreshDetails;
  FAccepted := True;
end;  // TfraDomainHyperlinks.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnDeleteClick(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
  if lbHyperlinks.ItemIndex>-1 then begin
    if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                                      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      FEditPressed := False;

      ItemIndex := lbHyperlinks.ItemIndex;
      FDomainHyperlinkList.DeleteItem(ItemIndex);
      if ItemIndex > 0 then Dec(ItemIndex);
      lbHyperlinks.ItemIndex := ItemIndex;

      ListboxEmptyButtons(btnEdit, True);
      ListboxEmptyButtons(btnDelete, True);
      FAccepted := True;
      RefreshDetails;
    end;
  end;
end;  // TfraDomainHyperlinks.btnDeleteClick

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnEditClick(Sender: TObject);
begin
  inherited;
  FAccepted := False;
  FEditPressed := True;
  InternalEnableControls(False);
  EnableContainedControls(True);
end;  // TfraDomainHyperlinks.btnEditClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnGetImageFilePathClick(Sender: TObject);
begin
  inherited;
  dlgOpenPic.InitialDir := mdmLocalImagePath;
  if dlgOpenPic.Execute then
    eImageFilePath.Text := dlgOpenPic.FileName;
end;  // TfraDomainHyperlinks.btnGetImageFilePathClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.btnSaveClick(Sender: TObject);
var
  lDomainHyperlinkItem: TDomainHyperlinkItem;
begin
  inherited;
  lDomainHyperlinkItem := 
      TDomainHyperlinkItem(lbHyperlinks.Items.Objects[lbHyperlinks.ItemIndex]);
  FFullImagePath := eImageFilePath.Text;
  if chkUseConceptKey.Checked then eWordSeparator.Text := '';
  with lDomainHyperlinkItem do
  begin
    ItemName := eItemName.Text;
    ImageFile := FFullImagePath;
    URL := eHyperlink.Text;
    UseConceptKey := chkUseConceptKey.Checked;
    WordSeparator := eWordSeparator.Text;
    SetModified;
  end;
  FDomainHyperlinkList.ValidateContent;
  InternalEnableControls(True);
  EnableContainedControls(False);
  eImageFilePath.Text := StripPath(FFullImagePath);
  FAccepted := True;
end;  // TfraDomainHyperlinks.btnSaveClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.chkUseConceptKeyClick(Sender: TObject);
begin
  inherited;
  if chkUseConceptKey.Checked then eWordSeparator.Enabled := False
                              else eWordSeparator.Enabled := True;
end;  // TfraDomainHyperlinks.chkUseConceptKeyClick 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.ClearDetails;
begin
  eItemName.Text := '';
  eImageFilePath.Text := '';
  FFullImagePath := '';
  eHyperlink.Text := '';
  eWordSeparator.Text := '';
  chkUseConceptKey.Checked := False;
end;  // TfraDomainHyperlinks.ClearDetails 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_DomainHyperlinks_Delete', ['@Key', Key]);
end;  // TfraDomainHyperlinks.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.EnableContainedControls(AEnabled : Boolean);
begin
  SetRequiredFieldsColourState(AEnabled, [eItemName, eHyperlink]);
  eItemName.Enabled := AEnabled;
  eImageFilePath.Enabled := AEnabled;
  eHyperlink.Enabled := AEnabled;
  lbHyperlinks.Enabled := not AEnabled;
  if AEnabled then chkUseConceptKeyClick(Self)
              else eWordSeparator.Enabled := AEnabled;
  chkUseConceptKey.Enabled := AEnabled;
  btnGetImageFilePath.Enabled := AEnabled;
  btnSave.Enabled := AEnabled;
  btnCancel.Enabled := AEnabled;
end;  // TfraDomainHyperlinks.EnableContainedControls 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.EnableControls(AEnabled: Boolean);
begin
  inherited;
  InternalEnableControls(AEnabled);
  if AEnabled then
    EnableContainedControls(not AEnabled);
end;  // TfraDomainHyperlinks.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.GetImagePaths;
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  
  try
    lReg.RootKey := HKEY_CURRENT_USER;
    lReg.OpenKeyReadOnly(USER_SETTINGS_REG_PATH);
  
    if lReg.ValueExists('Local Images File Path') then
      mdmLocalImagePath := lReg.ReadString('Local Images File Path');
  finally
    lReg.Free;
  end;
end;  // TfraDomainHyperlinks.GetImagePaths 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.InternalEnableControls(AEnabled : Boolean);
begin
  EnableContainedControls(AEnabled);
  btnAdd.Enabled := AEnabled;
  ListboxEmptyButtons(btnEdit, AEnabled);
  ListboxEmptyButtons(btnDelete, AEnabled);
end;  // TfraDomainHyperlinks.InternalEnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.lbHyperlinksClick(Sender: TObject);
begin
  inherited;
  RefreshDetails;
end;  // TfraDomainHyperlinks.lbHyperlinksClick

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.lbHyperlinksKeyDown(Sender: TObject; var Key: 
        Word; Shift: TShiftState);
begin
  inherited;
  if      Key = VK_INSERT then btnAddClick(Self)
  else if Key = VK_DELETE then btnDeleteClick(Self);
end;  // TfraDomainHyperlinks.lbHyperlinksKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.LoadData;
begin
  inherited LoadData;
  FAccepted := True;
  GetImagePaths;
  FDomainHyperlinkList.MasterKey := Key;
  FDomainHyperlinkList.Refresh;
  
  if lbHyperlinks.Items.Count = 0 then begin
    btnEdit.Enabled := False;
    btnDelete.Enabled := False;
  end
  else begin 
    if FItemIndex = -1 then
      lbHyperlinks.ItemIndex := 0
    else if FItemIndex < lbHyperlinks.Items.Count then      
      lbHyperlinks.ItemIndex := FItemIndex
    else 
      lbHyperlinks.ItemIndex := lbHyperlinks.Items.Count - 1;
  end;
  
  RefreshDetails;
end;  // TfraDomainHyperlinks.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.RefreshDetails;
begin
  FItemIndex := lbHyperlinks.ItemIndex;
  if lbHyperlinks.ItemIndex >= 0 then begin
    if Assigned(lbHyperlinks.Items.Objects[lbHyperlinks.ItemIndex]) then
      with TDomainHyperlinkItem(lbHyperlinks.Items.Objects[lbHyperlinks.ItemIndex])
          do begin
        eItemName.Text := ItemName;
        eImageFilePath.Text := StripPath(ImageFile);
        FFullImagePath := ImageFile;
        eHyperlink.Text := URL;
        eWordSeparator.Text := WordSeparator;
        chkUseConceptKey.Checked := UseConceptKey;
      end; // with
  end else
    ClearDetails;
end;  // TfraDomainHyperlinks.RefreshDetails 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.SaveData;
begin
  // If records are being added to the hyperlinks tab page at the same time
  // a record is being added to the main tab page (say, LocalDomainGeneral) the
  // MasterKey of the list won't have been updated. So do it before the save.
  FDomainHyperlinkList.MasterKey := Key;
  FDomainHyperlinkList.Update;
  FAccepted := False;
end;  // TfraDomainHyperlinks.SaveData 

{-------------------------------------------------------------------------------
}
function TfraDomainHyperlinks.StripPath(AFullPath: String): string;
begin
  Result := AFullPath;
  while (Pos('\', Result) <> 0) do begin
    Result := Copy(Result, Pos('\', Result) + 1, Length(Result));
  end;
end;  // TfraDomainHyperlinks.StripPath 

{-------------------------------------------------------------------------------
}
procedure TfraDomainHyperlinks.ValidateData;
begin
  ValidateValue(FAccepted <> False, ResStr_AcceptButtonNotClicked);
end;  // TfraDomainHyperlinks.ValidateData 

procedure TfraDomainHyperlinks.ListboxEmptyButtons(AButton: TImageListButton;
    AEnabled: Boolean);
begin
  if AEnabled then begin
    if lbHyperlinks.Count = 0 then AButton.Enabled := False
                              else AButton.Enabled := True;
  end else
    AButton.Enabled := AEnabled;
end;

{-==============================================================================
    TDomainHyperlinkItem
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDomainHyperlinkItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FImageFile := RetrieveFileName(VarToStr(AFields['Image_File'].Value));
  FItemName := VarToStr(AFields['Item_Name'].Value);
  FTimestamp := AFields['Timestamp'].Value;
  FURL := VarToStr(AFields['URL'].Value);
  FUseConceptKey := AFields['Use_Concept_Key'].Value;
  FWordSeparator := VarToStr(AFields['Word_Separator'].Value);
end;  // TDomainHyperlinkItem.InitFromRecord 

{-------------------------------------------------------------------------------
}
function TDomainHyperlinkItem.RetrieveFileName(AFilename: String): string;
var
  lText: string;
begin
  lText := AFilename;
  
  while (Pos('\', lText) <> 0) do begin
    lText := Copy(lText, Pos('\', lText) + 1, Length(lText));
  end;
  
  if StrLIComp(PChar('<#LOCAL_IMAGES>\'),
               PChar(AFilename),
               length('<#LOCAL_IMAGES>\') - 1) = 0 then
    Result := mdmLocalImagePath + '\' + lText
  else
    Result := AFilename;
end;  // TDomainHyperlinkItem.RetrieveFileName 

{-------------------------------------------------------------------------------
}
procedure TDomainHyperlinkItem.ValidateData;
begin
  ValidateValue(FItemName <> '', Format(ResStr_MissingData,
      [ResStr_Name]));
  ValidateValue(FUrl <> '', Format(ResStr_MissingData, [ResStr_URL]));
end;  // TDomainHyperlinkItem.ValidateData 

{$R *.dfm}

end.

