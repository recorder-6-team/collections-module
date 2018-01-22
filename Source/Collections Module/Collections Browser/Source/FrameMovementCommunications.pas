{===============================================================================
  Unit:        FrameMovementCommunications

  Defines:     TfraMovementCommunications

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 19 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameMovementCommunications;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  Grids, VagueDateEdit, ComCtrls, GeneralData, InterfaceDataModule,
  BaseCompositeComponent, LinkedControls, ResourceStrings, Validation,
  LuxembourgConstants, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  LuxembourgDataClasses, ADOInt, BaseDetailFrameUnit, DssStringGrid,
  DataClasses, DataTypes, VagueDate, ExceptionForm, DropTarget, DSSDataTypes,
  UserEdit;

type
  TCommunicationItem = class (TLuxGridDataItem)
  private
    FCommunicationTypeKey: TKeyString;
    FCommunicationTypeName: String;
    FContent: String;
    FFileRef: String;
    FReceiverKey: TKeyString;
    FReceiverName: String;
    FSenderKey: TKeyString;
    FSenderName: String;
    FTimestamp: TSQLSvrTimestamp;
    FVagueDate: TVagueDate;
  protected
    procedure GetData(const Column: Integer; var AText: String; var AKey: TKeyString); 
        override;
    procedure InitFromRecord(AFields: Fields); override;
    procedure SetData(const Column: Integer; const AText: String; const AKey: TKeyString); 
        override;
  public
    property CommunicationTypeKey: TKeyString read FCommunicationTypeKey write 
        FCommunicationTypeKey;
    property CommunicationTypeName: String read FCommunicationTypeName write 
        FCommunicationTypeName;
    property Content: String read FContent write FContent;
    property FileRef: String read FFileRef write FFileRef;
    property ReceiverKey: TKeyString read FReceiverKey write FReceiverKey;
    property ReceiverName: String read FReceiverName write FReceiverName;
    property SenderKey: TKeyString read FSenderKey write FSenderKey;
    property SenderName: String read FSenderName write FSenderName;
    property Timestamp: TSQLSvrTimestamp read FTimestamp write FTimestamp;
    property VagueDate: TVagueDate read FVagueDate write FVagueDate;
  end;
  
  TCommunicationList = class (TLuxGridDataList)
  protected
    procedure DoAddition(AItem: TLuxCachedDataItem); override;
    procedure DoDeletion(AItem: TLuxCachedDataItem); override;
    procedure DoModification(AItem: TLuxCachedDataItem); override;
    function GetRecordset: _Recordset; override;
  end;
  
  {-----------------------------------------------------------------------------
    Class to handle communication data about movements.
  }
  TfraMovementCommunications = class (TBaseTabSheetFrame)
    btnAccept: TImageListButton;
    btnAdd: TImageListButton;
    btnDiscard: TImageListButton;
    btnEdit: TImageListButton;
    btnRemove: TImageListButton;
    cmbType: TConceptGroupComboBox;
    eDate: TVagueDateEdit;
    eNameFrom: TUserEdit;
    eNameTo: TUserEdit;
    eReference: TEdit;
    gbDetails: TGroupBox;
    Label1: TLabel;
    Label27: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    lblCommDate: TLabel;
    mmContent: TMemo;
    sgCommunications: TDSSStringGrid;
    Shape3: TShape;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure DropNameFrom(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropNameTo(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure eDateExit(Sender: TObject);
    procedure sgCommunicationsClick(Sender: TObject);
    procedure sgCommunicationsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FAccepted: Boolean;
    FCommunicationList: TCommunicationList;
    FNewItem: Boolean;
    procedure ClearDetails;
    procedure EnableDetails(AEnabled: Boolean);
    procedure RefreshEditRemoveButtons;
    procedure RefreshDetails;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure SetKey(Value: TKeyString); override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TCommunicationItem
===============================================================================}
{-------------------------------------------------------------------------------
  Get data from an item. 
}
procedure TCommunicationItem.GetData(const Column: Integer; var AText: String; var AKey: 
    TKeyString);
begin
  AKey := '';
  AText := '';
  case Column of
    0: AText := VagueDateToString(FVagueDate);
    1: AText := FSenderName;
    2: AText := FCommunicationTypeName;
  end;
end;  // TCommunicationItem.GetData 

{-------------------------------------------------------------------------------
  Initialise an item with data from the database. 
}
procedure TCommunicationItem.InitFromRecord(AFields: Fields);
begin
  SetItemKey(VarToStr(AFields['Item_Key'].Value));
  FCommunicationTypeKey := AFields['Communication_Type_Concept_Key'].Value;
  FCommunicationTypeName := VarToStr(AFields['Communication_Type_Item_Name'].Value);
  FReceiverKey := AFields['Receiver_Name_Key'].Value;
  FReceiverName := VarToStr(AFields['Receiver_Name'].Value);
  FSenderKey := AFields['Sender_Name_Key'].Value;
  FSenderName := VarToStr(AFields['Sender_Name'].Value);
  FContent := VarToStr(AFields['Content'].Value);
  FFileRef := VarToStr(AFields['File_Ref'].Value);
  FTimestamp := AFields['Timestamp'].Value;
  FVagueDate.StartDate := AFields['Vague_Date_Start'].Value;
  FVagueDate.EndDate := AFields['Vague_Date_End'].Value;
  FVagueDate.DateTypeString := AFields['Vague_Date_Type'].Value;
end;  // TCommunicationItem.InitFromRecord 

{-------------------------------------------------------------------------------
  Set the data of an item. 
}
procedure TCommunicationItem.SetData(const Column: Integer; const AText: String; const AKey: 
    TKeyString);
begin
  case Column of
     0: begin
          //FVagueDate := AText;
          SetModified;
        end;
    1: if FSenderName <> AText then
       begin
         FSenderName := AText;
         SetModified;
       end;
    2: if FCommunicationTypeName <> AText then
       begin
         FCommunicationTypeName := AText;
         SetModified;
       end;
  end;
end;  // TCommunicationItem.SetData 

{-==============================================================================
    TCommunicationList
===============================================================================}
{-------------------------------------------------------------------------------
  This method is called for each added row in the String Grid. 
}
procedure TCommunicationList.DoAddition(AItem: TLuxCachedDataItem);
begin
  with TCommunicationItem(AItem) do begin
    dmGeneral.RunInsertStoredProc('Movement_Communication',
                                  'usp_MovementCommunication_Insert',
                                 ['@MovementKey', MasterKey,
                                  '@SenderNameKey', SenderKey,
                                  '@ReceiverNameKey', ReceiverKey,
                                  '@CommunicationTypeConceptKey', CommunicationTypeKey,
                                  '@VagueDateStart', VagueDate.StartDate,
                                  '@VagueDateEnd', VagueDate.EndDate,
                                  '@VagueDateType', VagueDate.DateTypeString,
                                  '@Content', Content,
                                  '@FileRef', FileRef
                                 ], '@Key');
  end;
end;  // TCommunicationList.DoAddition 

{-------------------------------------------------------------------------------
  This method is called for each deleted row in the String Grid. 
}
procedure TCommunicationList.DoDeletion(AItem: TLuxCachedDataItem);
begin
  with TCommunicationItem(AItem) do
    dmGeneral.RunDeleteStoredProc('usp_MovementCommunication_Delete',
                                  ['@Key', ItemKey, '@Timestamp', Timestamp]);
end;  // TCommunicationList.DoDeletion 

{-------------------------------------------------------------------------------
  This method is called for each modified row in the String Grid. 
}
procedure TCommunicationList.DoModification(AItem: TLuxCachedDataItem);
begin
  with TCommunicationItem(AItem) do begin
    dmGeneral.RunUpdateStoredProc('usp_MovementCommunication_Update',
                                 ['@Key', ItemKey,
                                  '@MovementKey', MasterKey,
                                  '@SenderNameKey', SenderKey,
                                  '@ReceiverNameKey', ReceiverKey,
                                  '@CommunicationTypeConceptKey', CommunicationTypeKey,
                                  '@VagueDateStart', VagueDate.StartDate,
                                  '@VagueDateEnd', VagueDate.EndDate,
                                  '@VagueDateType', VagueDate.DateTypeString,
                                  '@Content', Content,
                                  '@FileRef', FileRef,
                                  '@Timestamp', Timestamp
                                 ]);
  end;
end;  // TCommunicationList.DoModification 

{-------------------------------------------------------------------------------
  Register the correct recordset. 
}
function TCommunicationList.GetRecordset: _Recordset;
begin
  Result := dmGeneral.GetRecordset('usp_MovementCommunications_Select',
                                  ['@Key', MasterKey]);
end;  // TCommunicationList.GetRecordset 

{-==============================================================================
    TfraMovementCommunications
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor sets up the list for the string grid and gives the grid it's column headings. 
}
constructor TfraMovementCommunications.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FCommunicationList := TCommunicationList.Create(TCommunicationItem, sgCommunications);
  sgCommunications.Rows[0].CommaText := ResStr_Date + ',' + ResStr_With + ',' + ResStr_Type;

  FAccepted := True;  // Initialised to False, but we want True.
end;  // TfraMovementCommunications.Create 

{-------------------------------------------------------------------------------
  Takes additions and changes made in the details section of the tab page, and makes sure they 
      are done to the string grid. 
}
procedure TfraMovementCommunications.btnAcceptClick(Sender: TObject);
var
  lCommunicationItem: TCommunicationItem;
begin
  inherited;
  FAccepted := True;
  try
    ValidateContent;
  except
    on E:Exception do begin
      FAccepted := False;
      Raise;
    end;
  end;
  
  EnableDetails(False);
  // If new item, create object
  if FNewItem then
    lCommunicationItem := TCommunicationItem.CreateNew(FCommunicationList)
  else
    lCommunicationItem := TCommunicationItem(FCommunicationList.Items[sgCommunications.Row-1]);
  
  with lCommunicationItem do
  begin
    VagueDate             := eDate.VagueDate;
    ReceiverKey           := eNameTo.Key;
    ReceiverName          := eNameTo.Text;
    SenderKey             := eNameFrom.Key;
    SenderName            := eNameFrom.Text;
    CommunicationTypeKey  := cmbType.CurrentStrID;
    CommunicationTypeName := cmbType.Text;
    Content               := mmContent.Text;
    FileRef               := eReference.Text;
    SetModified;
  end;
  
  // Add new item to list
  if FNewItem then
    FCommunicationList.AddNew(lCommunicationItem);
    
  sgCommunicationsClick(Self);
end;  // TfraMovementCommunications.btnAcceptClick

{-------------------------------------------------------------------------------
  Lets the user enter information into the details section of the tab page. When the 'accept' 
      button is clicked this information is then used to add a row to the string grid. 
}
procedure TfraMovementCommunications.btnAddClick(Sender: TObject);
begin
  inherited;
  EnableDetails(True);
  FNewItem := True;
  FAccepted := False;
  ClearDetails;
end;  // TfraMovementCommunications.btnAddClick

{-------------------------------------------------------------------------------
  Gets rid of changes entered in the details section of the tab page. 
}
procedure TfraMovementCommunications.btnDiscardClick(Sender: TObject);
begin
  inherited;
  EnableDetails(False);
  RefreshDetails;
  FAccepted := True;
end;  // TfraMovementCommunications.btnDiscardClick 

{-------------------------------------------------------------------------------
  Puts the 'details' section of the string grid into edit mode. 
}
procedure TfraMovementCommunications.btnEditClick(Sender: TObject);
begin
  inherited;
  EnableDetails(True);
  FNewItem := False;
  FAccepted := False;
end;  // TfraMovementCommunications.btnEditClick

{-------------------------------------------------------------------------------
  Removes a row from the grid. 
}
procedure TfraMovementCommunications.btnRemoveClick(Sender: TObject);
begin
  inherited;
  sgCommunications.ReadOnly := False;
  if MessageDlg(Format(ResStr_ConfirmRowDelete, [ResStr_Row]),
                       mtWarning, [mbYes, mbNo], 0) = mrYes then
    FCommunicationList.DeleteItem(sgCommunications.Row);
  sgCommunications.ReadOnly := True;
  FAccepted := True;
  sgCommunicationsClick(Self);
  RefreshEditRemoveButtons;
end;  // TfraMovementCommunications.btnRemoveClick 

{-------------------------------------------------------------------------------
  Clear the details in the lower half of the screen. 
}
procedure TfraMovementCommunications.ClearDetails;
begin
  eDate.Text        := '';
  eNameTo.Text      := '';
  eNameTo.Key       := '';
  eNameFrom.Text    := '';
  eNameFrom.Key     := '';
  cmbType.ItemIndex := -1;
  mmContent.Text    := '';
  eReference.Text   := '';
end;  // TfraMovementCommunications.ClearDetails 

{-------------------------------------------------------------------------------
  Run the delete stored proc. for this frame. 
}
procedure TfraMovementCommunications.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_MovementCommunications_Delete',
                               ['@Key', Key]);
end;  // TfraMovementCommunications.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TfraMovementCommunications.DropNameFrom(const Sender: TObject; const AFormat: 
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: 
    Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eNameFrom,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
end;  // TfraMovementCommunications.DropNameFrom

{-------------------------------------------------------------------------------
}
procedure TfraMovementCommunications.DropNameTo(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, eNameTo,
      [TN_INDIVIDUAL, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName',
       TN_ORGANISATION, 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName']);
end;  // TfraMovementCommunications.DropNameTo

{-------------------------------------------------------------------------------
  This method ensures that however the user actually enters a vague date, when the focus 
      leaves the Vague Date Edit component, it is displayed properly. 
}
procedure TfraMovementCommunications.eDateExit(Sender: TObject);
begin
  inherited;
  eDate.Text := VagueDateToString(eDate.VagueDate);
end;  // TfraMovementCommunications.eDateExit 

{-------------------------------------------------------------------------------
  Enable/disable controls according to EditMode state. 
}
procedure TfraMovementCommunications.EnableControls(AEnabled: Boolean);
begin
  inherited;
  EnableContainedControls(gbDetails, AEnabled and not sgCommunications.Enabled);
  SetRequiredFieldsColourState(AEnabled and not sgCommunications.Enabled,
                               [eNameFrom, eNameTo, cmbType, eDate]);
  btnAdd.Enabled := AEnabled and sgCommunications.Enabled;
  RefreshEditRemoveButtons;
  btnEdit.Enabled := btnEdit.Enabled and AEnabled;
  btnRemove.Enabled := btnRemove.Enabled and AEnabled;
end;  // TfraMovementCommunications.EnableControls

{-------------------------------------------------------------------------------
  Get the caption.
}
function TfraMovementCommunications.GetCaption: String;
begin
  Result := RegisteredRecordsets[0].Fields['Display_Caption'].Value;
end;  // TfraMovementCommunications.GetCaption

{-------------------------------------------------------------------------------
}
procedure TfraMovementCommunications.EnableDetails(AEnabled: Boolean);
begin
  EnableContainedControls(gbDetails, AEnabled);
  SetRequiredFieldsColourState(AEnabled, [eNameFrom, eNameTo, cmbType, eDate]);

  sgCommunications.Enabled := not AEnabled;
  btnAdd.Enabled := not AEnabled;
  RefreshEditRemoveButtons;
end;  // TfraMovementCommunications.EnableDetails

{-------------------------------------------------------------------------------
  Load data. 
}
procedure TfraMovementCommunications.LoadData;
begin
  inherited LoadData;
  FCommunicationList.MasterKey := Key;
  FCommunicationList.Refresh;
end;  // TfraMovementCommunications.LoadData 

{-------------------------------------------------------------------------------
  Refresh the lower half of the frame.
}
procedure TfraMovementCommunications.RefreshDetails;
begin
  with sgCommunications do
    // If an item is selected, update the fields in details.
    if Assigned(Objects[0, Row]) then
      with TCommunicationItem(Objects[0, Row]) do begin
        eDate.Text      := VagueDateToString(VagueDate);
        eNameTo.Text    := ReceiverName;
        eNameFrom.Text  := SenderName;
        mmContent.Text  := Content;
        eReference.Text := FileRef;

        if not cmbType.Populated then begin
          // If not populated, always clear previous single item.
          cmbType.Clear;
          // Then add selected item's values.
          cmbType.Add(CommunicationTypeName, CommunicationTypeKey);
        end else
          cmbType.ItemIndex := cmbType.IDIndexOf(CommunicationTypeKey);
      end
    else
      ClearDetails;
end;  // TfraMovementCommunications.RefreshDetails

{-------------------------------------------------------------------------------
}
procedure TfraMovementCommunications.RefreshEditRemoveButtons;
begin
  with sgCommunications do
    btnRemove.Enabled := Enabled and
                         ((Cells[0, 1] <> '') or (Cells[1, 1] <> '') or (Cells[2, 1] <> ''));
  btnEdit.Enabled := btnRemove.Enabled;
end;  // TfraMovementCommunications.RefreshEditRemoveButtons

{-------------------------------------------------------------------------------
  This method is used so that when the tab page loads for the first time, the information it
      shows in the 'details' section is relevant to the top item in the string grid. 
}
procedure TfraMovementCommunications.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_MovementCommunication_Select');
  
  RegisterControl(eNameFrom, 'Sender_Name', 'Sender_Name_Key',
      True, ResStr_From, CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  RegisterControl(eNameTo, 'Receiver_Name', 'Receiver_Name_Key',
      True, ResStr_To, CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  RegisterControl(cmbType, 'Communication_Type_Item_Name',
      'Communication_Type_Concept_Key', True, ResStr_Type);
  RegisterConceptGroupComboBox(cmbType, CG_COMMUNICATION_TYPES);
  RegisterControl(eDate, '', True, ResStr_Date);
  RegisterControl(mmContent, 'Content');
  RegisterControl(eReference, 'File_Ref');
end;  // TfraMovementCommunications.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag/drop components. 
}
procedure TfraMovementCommunications.RegisterDragDropComponents;
begin
  RegisterDropComponent(eNameFrom, DropNameFrom, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA]);
  RegisterDropComponent(eNameTo, DropNameTo, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA]);
end;  // TfraMovementCommunications.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Tells the list to save any updates. 
}
procedure TfraMovementCommunications.SaveData;
begin
  FCommunicationList.Update;
end;  // TfraMovementCommunications.SaveData 

{-------------------------------------------------------------------------------
  Sets the master key for the list. 
}
procedure TfraMovementCommunications.SetKey(Value: TKeyString);
begin
  inherited SetKey(Value);
  
  FCommunicationList.MasterKey := Key;
end;  // TfraMovementCommunications.SetKey 

{-------------------------------------------------------------------------------
  When the user clicks on a row in the string grid, it needs to populate the details section 
      of the tabpage. 
}
procedure TfraMovementCommunications.sgCommunicationsClick(Sender: TObject);
begin
  with sgCommunications do
    if Assigned(Objects[0, Row]) then
      with TCommunicationItem(Objects[0, Row]) do begin
        eNameFrom.Text    := SenderName;
        eNameFrom.Key     := SenderKey;
        eNameTo.Text      := ReceiverName;
        eNameTo.Key       := ReceiverKey;
        cmbType.ItemIndex := cmbType.IDIndexOf(CommunicationTypeKey);
        mmContent.text    := Content;
        eReference.Text   := FileRef;
        eDate.Text        := VagueDateToString(VagueDate);
      end
    else
      ClearDetails;
  RefreshEditRemoveButtons;
end;  // TfraMovementCommunications.sgCommunicationsClick

{-------------------------------------------------------------------------------
  If the String Grid is selected, it needs to be able to handle 'insert' and 'delete' 
      keypresses. 
}
procedure TfraMovementCommunications.sgCommunicationsKeyDown(Sender: TObject; var Key: Word; 
    Shift: TShiftState);
begin
  inherited;
  
  if (EditMode = emEdit) and (sgCommunications.Enabled) then
  begin
    if (Key = VK_INSERT) then
      btnAddClick(nil)
    else if (Key = VK_DELETE) then
      btnRemoveClick(nil);
  end;
end;  // TfraMovementCommunications.sgCommunicationsKeyDown 

{-------------------------------------------------------------------------------
  Makes sure that the user has clicked the accept button before they click save. 
}
procedure TfraMovementCommunications.ValidateData;
begin
  inherited;
  if not FAccepted then begin
    raise TExceptionPath.CreateNonCritical(ResStr_AcceptButtonNotClicked);
    Abort;
  end;
end;  // TfraMovementCommunications.ValidateData 

end.



