{===============================================================================
  Unit:        BaseDetailFrameUnit.pas

  Defines:     TBaseDetailFrame

  Description: Base frame to display and save data on details side.

  Model:       CollectionBrowserFramework.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 94 $
    $Date: 11/04/14 9:08 $
    $Author: Brynhorsfieldschonhut $

===============================================================================}

unit BaseDetailFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataTypes, StdCtrls, ComCtrls, ExtCtrls, Recorder2000_TLB, Contnrs,
  ExceptionForm, ADODB, ADOInt, GeneralData, Validation, LuxIDComboBox, DataClasses,
  ConceptGroupComboBox, BaseDragFrameUnit, LinkedControls, DropTarget, VagueDateEdit,
  RegisteredControls, BaseADODataModule, RapTree, DSSDataTypes;

type
  ECaptionControlException = class(TExceptionPath)
  end;
  
  EBaseDetailFrameException = class(TExceptionPath)
  end;
  
  ERegisteredAsyncControlError = class(EBaseDetailFrameException)
  end;
  
  TRequestDataRecipientMethod = procedure (const AKeyList: IKeyList) of object;
  TBaseDetailFrameClass = class of TBaseDetailFrame;

  {-----------------------------------------------------------------------------
    Base frame for all detail screens in the Collections Browser and all details tab pages
    embedded into those screens.  Inherited directly from the Delphi TFrame component.
    This allows the frame to be developed within Delphi as if it is a screen, but embedded
    into application forms as if it is a component.
    The Collections Module is constructed so that each details screen and each tab within a
    screen is a separate frame component.  This reduces the complexity of each module of
    code and therefore improves maintainability.
    All functionality common to the data entry and viewing panels loaded into the
    Collections Browser is maintained in this class.
  }
  TBaseDetailFrame = class(TBaseDragFrame, iRequestor)
    procedure IRequestor.Update = UpdateRequestedData;
  private
    FAssociatedContainerNode: TFlyNode;
    FCGCombos: TObjectList;
    FControlsRegistered: Boolean;
    FDisplayTextTarget: TObject;
    FDisplayTextValue: Variant;
    FEditMode: TEditMode;
    FKey: TKeyString;
    FNodeContext: TNodeContext;
    FOnFrameNotification: TFrameNotificationEvent;
    FParentKey: TKeyString;
    FRegisteredAsyncControls: TObjectList;
    FRegisteredControls: TObjectList;
    FRegisteredRecordsets: TObjectList;
    FRequestDataRecipientLinkedEdit: TLinkedEdit;
    FRequestDataRecipientMethod: TRequestDataRecipientMethod;
    procedure CloseRecordsets;
    procedure DisplayTextValue(ATarget: TObject; AValue: Variant);
    procedure EnableSingleControl(AComponent: TComponent; AEnabled: Boolean);
    function GetDefaultParams: TVariantArray;
    function GetRegisteredRecordsets(Index: Integer): _Recordset;
    function InitReturnData(const ADataType: String): IUnknown; overload;
    procedure OpenRecordsets;
    procedure PopulateConceptGroupCombo(Sender: TObject);
    procedure SearchForAdditionToLast10ConceptsCombo(AComboBox: TConceptGroupComboBox; const
        AConceptGroup, APersistentList: string; const AConceptGroup2: string='');
    procedure SetEditMode(const Value: TEditMode);
    procedure SetKeys(const ACallerMethod: String);
    procedure SetOnFrameNotification(Value: TFrameNotificationEvent);
    procedure SynchedDisplayTextValue;
    procedure ValidateMandatoryControls;
    procedure ValidateVagueDates;
  protected
    FAdditionalProperties: TObject;
    FCaption: String;
    procedure VagueDateEditExit(Sender: TObject);
    procedure DeleteData; virtual;
    procedure DropLinkedEditData(AFormat: Integer; ASourceData: TKeyList; var AHandled:
        Boolean; AControl: TLinkedEdit; const ATableName, AStoredProcName, AParamName,
        AOutParamName: String); virtual;
    procedure DropLinkedEditDataMultiple(AFormat: Integer; ASourceData: TKeyList; var
        AHandled: Boolean; AControl: TLinkedEdit; AValues: Array of String); virtual;
    procedure EnableContainedControls(AContainer: TWinControl; AEnabled: Boolean);
    procedure EnableControls(AEnabled: Boolean); virtual;
    function GetAdditionalProperties: IAdditionalProperties;
    function GetCaption: String; virtual;
    function InitReturnData(ARecipientLinkedEdit: TLinkedEdit; const ADataType: String):
        IUnknown; overload;
    function InitReturnData(ARecipientMethod: TRequestDataRecipientMethod; const ADataType:
        String): IUnknown; overload;
    procedure LinkedEditFindData(Sender: TObject);
    procedure LinkedEditGetData(Sender: TObject);
    procedure LoadAsyncData; virtual;
    procedure LoadControlData(AControl: TRegisteredLinkedControl; ARecordset: _Recordset);
        overload; virtual;
    procedure LoadControlData(AControl: TRegisteredVagueDateControl; ARecordset: _Recordset);
        overload; virtual;
    procedure LoadData; virtual;
    procedure PopulateLast10ConceptsCombo(AComboBox: TConceptGroupComboBox; const
        AConceptGroup, APersistentList: String; const AConceptGroup2: String = '');
    procedure RegisterAsyncControl(AControl: TControl; AStoredProcName: String; AParams: Array
        of Variant; AOutputParam: String; ARecordsetIndex: Integer = 0; ACallbackProc:
        TAsyncExecuteCompleteProc = nil);
    procedure RegisterConceptGroupComboBox(AComboBox: TConceptGroupComboBox; AConceptGroupKey:
        String);
    procedure RegisterControl(AControl: TWinControl; const ADataFieldName: String; AMandatory:
        Boolean; const ADisplayFieldName: String; ARecordsetIndex: Integer = 0); overload;
    procedure RegisterControl(AControl: TWinControl; const ADataFieldName: String;
        ARecordsetIndex: Integer = 0); overload;
    procedure RegisterControl(AControl: TWinControl; const ADataFieldName, AKeyFieldName:
        String; AMandatory: Boolean; const ADisplayFieldName: String; ACheckDataFunction:
        TCheckLinkedDataFunction = nil; const ARequestDataType: string = '';
        AKeyToCaptionFunction: TConvertKeyToCaptionFunction = nil; ARecordsetIndex: Integer =
        0); overload;
    procedure RegisterControl(AControl: TWinControl; const ADataFieldName, AKeyFieldName:
        String; ACheckDataFunction: TCheckLinkedDataFunction = nil; const ARequestDataType:
        string = ''; AKeyToCaptionFunction: TConvertKeyToCaptionFunction = nil;
        ARecordsetIndex: Integer = 0); overload;
    procedure RegisterControls; virtual;
    function RegisterRecordset(const AStoredProcName: String; AGetParamsFunction:
        TGetParamsFunction = nil; AGetStoredProcName: TGetStoredProcNameFunction = nil):
        Integer; overload;
    function RegisterRecordset(const AStoredProcName: String; const AGetStoredProcName:
        TGetStoredProcNameFunction; AGetParamsFunction: TGetParamsFunction = nil): Integer;
        overload;
    function RegisterRecordset(const AGetStoredProcName: TGetStoredProcNameFunction;
        AGetParamsFunction: TGetParamsFunction = nil): Integer; overload;
    procedure SaveData; virtual; abstract;
    procedure SaveLast10ConceptsCombo(AComboBox: TConceptGroupComboBox; const AConceptGroup,
        APersistentList: string; const AConceptGroup2: string='');
    procedure SetKey(Value: TKeyString); virtual;
    procedure SetNodeContext(Value: TNodeContext); virtual;
    procedure SetParentKey(const Value: TKeyString); virtual;
    procedure SetRequiredFieldColours(AEnabled: boolean);
    procedure SetRequiredFieldsColourState(AEnabled: Boolean; WinControls: Array of
        TWinControl);
    procedure UpdateRequestedData(const KeyList: IKeyList); safecall;
    procedure ValidateData; virtual;
    procedure ValidateLinkedEdit(ALinkedControl: TRegisteredLinkedControl); virtual;
    property RegisteredRecordsets[Index: Integer]: _Recordset read GetRegisteredRecordsets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelChanges; virtual;
    procedure DeleteContent;
    procedure DropInterfaceReferences; virtual;
    procedure LoadContent;
    procedure ReloadContent;
    procedure ResizeFrame(NewWidth, NewHeight: Integer); virtual;
    procedure SaveContent;
    procedure SetAdditionalProperties(AAdditionalProperties: TObject);
    procedure UpdateMapWindowSelector; virtual;
    procedure ValidateContent;
    procedure UpdateAsyncControls(); virtual;
    property AdditionalProperties: IAdditionalProperties read GetAdditionalProperties;
    property AssociatedContainerNode: TFlyNode read FAssociatedContainerNode write
        FAssociatedContainerNode;
    property Caption: String read GetCaption;
    property EditMode: TEditMode read FEditMode write SetEditMode;
    property Key: TKeyString read FKey write SetKey;
    property NodeContext: TNodeContext read FNodeContext write SetNodeContext;
    property OnFrameNotification: TFrameNotificationEvent read FOnFrameNotification write
        SetOnFrameNotification;
    property ParentKey: TKeyString read FParentKey write SetParentKey;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, ResourceStrings, BaseCompositeComponent,
  LuxembourgConstants, VagueDate, DateUtils, SearchManager, ComObj, DB,
  GeneralFunctions;

type
  {-----------------------------------------------------------------------------
    Helper class for comboboxes dedicated to handle a Concept Group.
  }
  TCGComboAssistor = class(TObject)
  private
    FComboBox: TConceptGroupComboBox;
    FConceptGroupKey: String;
  public
    property ComboBox: TConceptGroupComboBox read FComboBox write FComboBox;
    property ConceptGroupKey: String read FConceptGroupKey write FConceptGroupKey;
  end;
  
  {-----------------------------------------------------------------------------
    Class to hold info on recordsets used by a frame.  The StoredProcName property holds
    the name of the stored procedure to run when the data is requested.  The Recordset
    property holds the recordset returned by the stored proc.
  }
  TRegisteredRecordset = class(TObject)
  private
    FGetParameters: TGetParamsFunction;
    FGetStoredProcName: TGetStoredProcNameFunction;
    FRecordset: _Recordset;
    FStoredProcName: String;
  public
    function RetrieveStoredProcName: String;
    property GetParameters: TGetParamsFunction read FGetParameters write FGetParameters;
    property GetStoredProcName: TGetStoredProcNameFunction read FGetStoredProcName write
        FGetStoredProcName;
    property Recordset: _Recordset read FRecordset write FRecordset;
    property StoredProcName: String read FStoredProcName write FStoredProcName;
  end;
  
{-==============================================================================
    TBaseDetailFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseDetailFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCGCombos                := TObjectList.Create;
  FRegisteredControls      := TObjectList.Create;
  FRegisteredAsyncControls := TObjectList.Create;
  FRegisteredRecordsets    := TObjectList.Create;
  FAdditionalProperties    := nil;
end;  // TBaseDetailFrame.Create 

{-------------------------------------------------------------------------------
}
destructor TBaseDetailFrame.Destroy;
begin
  // Do some cleanup
  if TdmGeneral.Allocated then
    if Assigned(FRegisteredAsyncControls) then
      if FRegisteredAsyncControls.Count > 0 then
        try
        dmGeneral.CancelAsyncCommands(TRegisteredAsyncControl(
           FRegisteredAsyncControls[0]).CallbackProc);
        except
        end;
  
  FCGCombos.Free;
  FRegisteredControls.Free;
  FRegisteredAsyncControls.Free;
  FRegisteredRecordsets.Free;
  
  inherited Destroy;
end;  // TBaseDetailFrame.Destroy 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.CancelChanges;
begin
  // Override as and when needed.
end;  // TBaseDetailFrame.CancelChanges 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.CloseRecordsets;
var
  i: Integer;
begin
  // Finished using the recordsets, close them
  with FRegisteredRecordsets do
    for i := 0 to Count - 1 do
      TRegisteredRecordset(Items[i]).Recordset.Close;
end;  // TBaseDetailFrame.CloseRecordsets 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.DeleteContent;
begin
  // Making sure the keys have the correct values assigned.
  SetKeys('TBaseDetailFrame.DeleteContent');
  
  DeleteData;
end;  // TBaseDetailFrame.DeleteContent 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.DeleteData;
begin
  // Do nothing in base class, but override in descendants as and when required.
  // In some cases, content of a record from one table is spread across more than
  // one tab. Therefore, there is no need for some screen to override this method.
end;  // TBaseDetailFrame.DeleteData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.DisplayTextValue(ATarget: TObject; AValue: Variant);
begin
  FDisplayTextTarget := ATarget;
  FDisplayTextValue := AValue;
  TThread.Synchronize(nil, SynchedDisplayTextValue);
end;  // TBaseDetailFrame.DisplayTextValue 

{-------------------------------------------------------------------------------
  Requests that the base detail frame nils any interface pointers to IAdditionalProperties.
}
procedure TBaseDetailFrame.DropInterfaceReferences;
begin
  FAdditionalProperties := nil;
end;  // TBaseDetailFrame.DropInterfaceReferences 

{-------------------------------------------------------------------------------
  When data is dropped onto an edit control handles the population of the edit control's text
      and key.
}
procedure TBaseDetailFrame.DropLinkedEditData(AFormat: Integer; ASourceData: TKeyList; var
    AHandled: Boolean; AControl: TLinkedEdit; const ATableName, AStoredProcName, AParamName,
    AOutParamName: String);
begin
  DropLinkedEditDataMultiple(AFormat, ASourceData, AHandled, AControl,
                             [ATableName, AStoredProcName, AParamName, AOutParamName]);
end;  // TBaseDetailFrame.DropLinkedEditData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.DropLinkedEditDataMultiple(AFormat: Integer; ASourceData: TKeyList;
    var AHandled: Boolean; AControl: TLinkedEdit; AValues: Array of String);
var
  i: Integer;
begin
  if EditMode = emBrowse then
    AHandled := True
  else
  if AFormat = CF_TEXT then
    AControl.Key := ''
  else
  if AFormat = CF_JNCCDATA then
  begin
    AHandled := True;
    with ASourceData do
      if Header.ItemCount > 0 then
        for i := 0 to (Length(AValues) div 4) - 1 do
          if (CompareText(Header.TableName, AValues[i * 4]) = 0) or
             (CompareText(Items[0].KeyField2, AValues[i * 4]) = 0) then
          begin
            AControl.Text := VarToStr(dmGeneral.GetStoredProcOutputParam(AValues[i * 4 + 1],
                                          [AValues[i * 4 + 2], Items[0].KeyField1],
                                          AValues[i * 4 + 3]));
            // No text, no item, so no key.
            if AControl.Text = '' then
              AControl.Key := ''
            else
              AControl.Key := Items[0].KeyField1;
            Exit;
          end;
  end;
end;  // TBaseDetailFrame.DropLinkedEditDataMultiple 

{-------------------------------------------------------------------------------
  Loop through the controls contained in the given Windows control and applies the default
      enabling behaviour as described in EnableSingleControl.
}
procedure TBaseDetailFrame.EnableContainedControls(AContainer: TWinControl; AEnabled: Boolean);
var
  i: Integer;
begin
  for i := 0 to AContainer.ControlCount - 1 do
    EnableSingleControl(AContainer.Controls[i], AEnabled);
end;  // TBaseDetailFrame.EnableContainedControls 

{-------------------------------------------------------------------------------
  Applies the default behaviour when setting edit mode or browse mode for the frame.
      Descendants are able to override this method to implement special cases, for example
      where buttons are not enabled automatically when in edit mode.
  Each control on the frame is looped through and the following behaviour is applied to the
      control depending on its type.  The list details what happend when the control is made
      read-only (in browse mode), the changes are reversed to place the control in edit mode:
  Edit controls are made read only.
  Memo controls are made read only.
  Button controls are made disabled.
  Check boxes and radio buttons are made disabled
  Combo boxes are set to ignore any attempt to change the selected item.
  If disabling the controls, the EM_EMPTYUNDOBUFFER message is sent to all edit boxes, including the edit box part of a combobox, to stop the Undo menu item on Window's own context popup menu to be enabled, which would allow users to change the content of read-only edit boxes.
}
procedure TBaseDetailFrame.EnableControls(AEnabled: Boolean);
var
  i: Integer;
begin
  // Implement default behaviour by looping through controls on this frame
  for i := 0 to Self.ComponentCount - 1 do
    EnableSingleControl(Self.Components[i], AEnabled);
  SetRequiredFieldColours(AEnabled);
end;  // TBaseDetailFrame.EnableControls 

{-------------------------------------------------------------------------------
  Applies the default enabling behaviour to the given control, as detailed in the following
      list:
  Edit controls are made read only.
  Memo controls are made read only.
  Button controls are made disabled.
  Check boxes and radio buttons are made disabled
  Combo boxes are set to ignore any attempt to change the selected item.
  If disabling the controls, the EM_EMPTYUNDOBUFFER message is sent to all edit boxes, including the edit box part of a combobox, to stop the Undo menu item on Window's own context popup menu to be enabled, which would allow users to change the content of read-only edit boxes.
}
procedure TBaseDetailFrame.EnableSingleControl(AComponent: TComponent; AEnabled: Boolean);
begin
  if AComponent is TBaseCompositeComponent then begin
    if AEnabled then TBaseCompositeComponent(AComponent).EditMode := emEdit
                else TBaseCompositecomponent(AComponent).EditMode := emBrowse;
  end else
  // TCustomEdit is ancestor for TEdit, TMemo and TRichEdit
  if AComponent is TCustomEdit then begin
    TCustomEditAccessor(AComponent).ReadOnly := not AEnabled;
    if not AEnabled then
      SendMessage(TCustomEdit(AComponent).Handle, EM_EMPTYUNDOBUFFER, 0, 0);
  end else
  if AComponent is TLuxIDComboBox then begin
    TLuxIDComboBox(AComponent).ReadOnly := not AEnabled;
    // Clear undo buffer on edit box part
    if not AEnabled and (TLuxIDComboBox(AComponent).Style <> csDropDownList) then
      SendMessage(GetWindow(TLuxIDComboBox(AComponent).Handle, GW_CHILD),
                  EM_EMPTYUNDOBUFFER, 0, 0);
  end else
  // TButtonControl is ancestor to TButton, TCheckBox and TRadioButton
  if AComponent is TButtonControl then
    (AComponent as TButtonControl).Enabled := AEnabled;
end;  // TBaseDetailFrame.EnableSingleControl

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.GetAdditionalProperties: IAdditionalProperties;
begin
  if Assigned(FAdditionalProperties) then
    Supports(FAdditionalProperties, IID_IAdditionalProperties, Result)
  else
    Result := nil;
end;  // TBaseDetailFrame.GetAdditionalProperties 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.GetCaption: String;
begin
  Result := FCaption;
end;  // TBaseDetailFrame.GetCaption 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.GetDefaultParams: TVariantArray;
begin
  Result := VarArrayOf(['@Key', FKey]);
end;  // TBaseDetailFrame.GetDefaultParams 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.GetRegisteredRecordsets(Index: Integer): _Recordset;
begin
  if (Index > -1) and (Index < FRegisteredRecordsets.Count) then
    Result := TRegisteredRecordset(FRegisteredRecordsets[Index]).Recordset;
end;  // TBaseDetailFrame.GetRegisteredRecordsets 

{-------------------------------------------------------------------------------
  Internal method for initialising a return data link.
  Returns the addin instance if a COM object used to get data from.
}
function TBaseDetailFrame.InitReturnData(const ADataType: String): IUnknown;
var
  lGUID: TGUID;
begin
  try
    // If conversion fails, it's a normal string.
    lGUID := StringToGUID(ADataType);
    with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
      Result := RequestCOMData(Self as IRequestor, lGUID);
  except
    on EOleSysError do begin
      with CreateCOMObject(CLASS_AutoApplicationSettings) as IRecorder2000 do
        RequestData(Self as IRequestor, ADataType);
      Result := nil;
    end;
  end;
end;  // TBaseDetailFrame.InitReturnData 

{-------------------------------------------------------------------------------
  Allows an F9 - Request data link to be setup.  The method supplied is responsible for
      handling the item returned.  The data type supplied specifies which screen the data is
      requested from.
  Returns the addin instance if a COM object used to get data from.
}
function TBaseDetailFrame.InitReturnData(ARecipientLinkedEdit: TLinkedEdit; const ADataType:
    String): IUnknown;
begin
  FRequestDataRecipientLinkedEdit := ARecipientLinkedEdit;
  FRequestDataRecipientMethod := nil;
  Result := InitReturnData(ADataType);
end;  // TBaseDetailFrame.InitReturnData 

{-------------------------------------------------------------------------------
  Allows an F9 - Request data link to be setup.  The method supplied is responsible for
      handling the item returned.  The data type supplied specifies which screen the data is
      requested from.
  Returns the addin instance if a COM object used to get data from.
}
function TBaseDetailFrame.InitReturnData(ARecipientMethod: TRequestDataRecipientMethod; const
    ADataType: String): IUnknown;
begin
  FRequestDataRecipientMethod := ARecipientMethod;
  FRequestDataRecipientLinkedEdit := nil;
  Result := InitReturnData(ADataType);
end;  // TBaseDetailFrame.InitReturnData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.LinkedEditFindData(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FRegisteredControls.Count - 1 do
    if FRegisteredControls[i] is TRegisteredLinkedControl then
      with TRegisteredLinkedControl(FRegisteredControls[i]) do
        if (Control is TLinkedEdit) and (Control = Sender) then
          CheckLinkedData(TLinkedEdit(Control));
end;  // TBaseDetailFrame.LinkedEditFindData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.LinkedEditGetData(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FRegisteredControls.Count - 1 do
    if FRegisteredControls[i] is TRegisteredLinkedControl then
      with TRegisteredLinkedControl(FRegisteredControls[i]) do
        if (Control is TLinkedEdit) and (Control = Sender) then
          InitReturnData(TLinkedEdit(Control), DataType);
end;  // TBaseDetailFrame.LinkedEditGetData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.LoadAsyncData;
var
  i, lIdx: Integer;
  lRegRec: _Recordset;
  lParams: Array of Variant;
begin
  for i := 0 to FRegisteredAsyncControls.Count - 1 do
    with TRegisteredAsyncControl(FRegisteredAsyncControls[i]) do begin
      // Use local var for clearer code.
      lRegRec := RegisteredRecordsets[RecordsetIndex];
      // Only do it if there is something to do!
      if not lRegRec.Eof then begin
        // Populate params with proper values.
        SetLength(lParams, Length(Params));
        if Length(Params) > 0 then begin
          lParams := VarArrayOf(Params);
          for lIdx := 0 to High(Params) div 2 do
            lParams[(lIdx * 2) + 1] := lRegRec.Fields[lParams[(lIdx * 2) + 1]].Value;
        end;
        // And get the data.
        dmGeneral.GetAsyncData(StoredProcName, lParams, OutputParam, Control, CallbackProc);
      end;
    end;
end;  // TBaseDetailFrame.LoadAsyncData

{-------------------------------------------------------------------------------
  Updates the asynchronious controls registered to the frame
}
procedure TBaseDetailFrame.UpdateAsyncControls;
var
  i, lIdx: Integer;
  lRegRec: _Recordset;
  lParams: Array of Variant;
begin
  OpenRecordsets;
  for i := 0 to FRegisteredAsyncControls.Count - 1 do
    with TRegisteredAsyncControl(FRegisteredAsyncControls[i]) do begin
      // Use local var for clearer code.
      lRegRec := RegisteredRecordsets[RecordsetIndex];
      // Only do it if there is something to do!

      if not lRegRec.Eof then begin
        // Populate params with proper values.
        SetLength(lParams, Length(Params));
        if Length(Params) > 0 then begin
          lParams := VarArrayOf(Params);
          for lIdx := 0 to High(Params) div 2 do
            lParams[(lIdx * 2) + 1] := lRegRec.Fields[lParams[(lIdx * 2) + 1]].Value;
        end;
        // And get the data.
        dmGeneral.GetAsyncData(StoredProcName, lParams, OutputParam, Control, CallbackProc);
      end;
    end;

  CloseRecordsets;
end;  // TBaseDetailFrame.LoadAsyncData


{-------------------------------------------------------------------------------
  The primary key of the record being loaded is passed in the Key parameter and stored in the
      Key property.
}
procedure TBaseDetailFrame.LoadContent;
begin
  SetKeys('TBaseDetailFrame.LoadContent');
  
  // Call RegisterControls only once!
  if not FControlsRegistered then RegisterControls;
  
  // Open all recordsets, ready for use.
  OpenRecordsets;
  
  // Populate the controls with the data.
  LoadData;
  LoadAsyncData;
  
  // Finished using the recordsets, close them.
  CloseRecordsets;
end;  // TBaseDetailFrame.LoadContent 

{-------------------------------------------------------------------------------
  Populates a linked control (TConceptGroupCombo or a linked control with return data
      functionality) from the dataset.
}
procedure TBaseDetailFrame.LoadControlData(AControl: TRegisteredLinkedControl; ARecordset:
    _Recordset);
begin
  with AControl do begin
    if KeyFieldName <> '' then begin
      // IDComboBox.
      if Control is TLuxIDComboBox then
        with TLuxIDComboBox(Control) do
          // If nothing, regardless of combo populated, set index to -1.
          if ARecordset.Eof or VarIsNull(ARecordset.Fields[KeyFieldName].Value) then
            ItemIndex := -1
          else begin
            if not Populated then
              if DataFieldName <> '' then
                // If not populated, add single item for concept group combos.
                ItemIndex := Add(VarToStr(ARecordset.Fields[DataFieldName].Value),
                                 VarToStr(ARecordset.Fields[KeyFieldName].Value))
              else
                PopulateContent; // For other combos, actually populate them.
  
            if Populated then
              if VarIsStr(ARecordset.Fields[KeyFieldName].Value) then
                ItemIndex := IDIndexOf(VarToStr(ARecordset.Fields[KeyFieldName].Value))
              else
              if VarIsType(ARecordset.Fields[KeyFieldName].Value, varBoolean) then begin
                // For boolean values, the second item in the list evaluates to True.
                if ARecordset.Fields[KeyFieldName].Value then
                  ItemIndex := IDIndexOf(1)
                else
                  ItemIndex := IDIndexOf(0);
              end else
                ItemIndex := IDIndexOf(Integer(ARecordset.Fields[KeyFieldName].Value));
          end
      else
      // LinkedControl.
      if Control is TLinkedEdit then
        if ARecordset.Eof then begin
          TLinkedEdit(Control).Text := '';
          TLinkedEdit(Control).Key  := '';
        end else begin
          TLinkedEdit(Control).Text := VarToStr(ARecordset.Fields[DataFieldName].Value);
          TLinkedEdit(Control).Key  := VarToStr(ARecordset.Fields[KeyFieldName].Value);
        end;
    end;
  end
end;  // TBaseDetailFrame.LoadControlData 

{-------------------------------------------------------------------------------
  Populates a vague date control from the dataset.
}
procedure TBaseDetailFrame.LoadControlData(AControl: TRegisteredVagueDateControl; ARecordset:
    _Recordset);
begin
  if ARecordset.Eof then
    AControl.Control.Text := ''
  else
    AControl.Control.Text := dmGeneral.GetVagueDateStringFromRecordset(
                                       ARecordset, AControl.DataFieldName);
end;  // TBaseDetailFrame.LoadControlData 

{-------------------------------------------------------------------------------
  Virtual method that allows descendents to load data and populate their controls. Each frame
      knows only about its own controls, and how to load and save the associated data.  The
      implementation in the base frame loops through and populate registered controls,
      reducing the amount of code written in each inherited frames.
  This method is implemented in all details frames in the Collections Module.
}
procedure TBaseDetailFrame.LoadData;
var
  i: Integer;
  lRegRec: _Recordset;
  lDefaultCursor: TCursor;
begin
  lDefaultCursor := HourglassCursor;
  
  for i := 0 to FRegisteredControls.Count - 1 do begin
    // Use local var for clearer code.
    lRegRec := RegisteredRecordsets[TRegisteredControl(
        FRegisteredControls[i]).RecordsetIndex];
    // VagueDate control
    if FRegisteredControls[i] is TRegisteredVagueDateControl then
      LoadControlData(TRegisteredVagueDateControl(FRegisteredControls[i]), lRegRec)
    else
    // IDComboBox or LinkedControl
    if FRegisteredControls[i] is TRegisteredLinkedControl then
      LoadControlData(TRegisteredLinkedControl(FRegisteredControls[i]), lRegRec)
    else
      // Control is straightforward data field
      with TRegisteredControl(FRegisteredControls[i]) do begin
        if Control is TCustomEdit then begin
          if lRegRec.Eof then
            TCustomEdit(Control).Text := ''
          else
            TCustomEdit(Control).Text := VarToStr(lRegRec.Fields[DataFieldName].Value);
          // Set max length on control if we can, but NOT for numeric fields!!!!!
          if (Control is TEdit) and
             (lRegRec.Fields[DataFieldName].Type_ in [adChar, adVarChar, adWChar, adVarWChar]) then
            TEdit(Control).MaxLength := lRegRec.Fields[DataFieldName].DefinedSize;
        end;
        if Control is TCheckBox then begin
          if LRegRec.Eof then
            TCheckBox(Control).Checked := False
          else
            TCheckBox(Control).Checked := lRegRec.Fields[DataFieldName].Value;
        end;
      end;
  end;
  
  // This is necessary to clear the Async controls when a node is added.
  for i := 0 to FRegisteredAsyncControls.Count - 1 do begin
    lRegRec := RegisteredRecordsets[TRegisteredAsyncControl(
        FRegisteredAsyncControls[i]).RecordsetIndex];
  
      with TRegisteredAsyncControl(FRegisteredAsyncControls[i]) do begin
        if Control is TLabel then
          if lRegRec.Eof then TLabel(Control).Caption := '';
        if Control is TMemo then
          if lRegRec.Eof then TMemo(Control).Lines.Text := '';
      end;
  end;
  
  DefaultCursor(lDefaultCursor);
end;  // TBaseDetailFrame.LoadData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.OpenRecordsets;
var
  i: Integer;
begin
  // Open all registered recordsets, ready to use for populating the registered controls.
  for i := 0 to FRegisteredRecordsets.Count - 1 do
    with TRegisteredRecordset(FRegisteredRecordsets[i]) do
      Recordset := dmGeneral.GetRecordset(RetrieveStoredProcName, GetParameters);
end;  // TBaseDetailFrame.OpenRecordsets 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.PopulateConceptGroupCombo(Sender: TObject);
var
  i: Integer;
  lRS: _Recordset;
begin
  for i := 0 to FCGCombos.Count - 1 do
    // If there are more than one combo on the frame, find the right one to populate.
    if Sender = TCGComboAssistor(FCGCombos[i]).ComboBox then begin
      // Get data from database.
      lRS := dmGeneral.GetRecordset('usp_Concept_Select_ForConceptGroup',
                 ['@ConceptGroupKey', TCGComboAssistor(FCGCombos[i]).ConceptGroupKey]);
      with TConceptGroupComboBox(Sender) do begin
        // Then the rest from DB.
        while not lRS.Eof do begin
          Add(VarToStr(lRS.Fields['Item_Name'].Value),
              VarToStr(lRS.Fields['Concept_Key'].Value));
          lRS.MoveNext;
        end;
        lRS.Close;
      end;
      // Combo populated, exit now.
      Exit;
    end;
end;  // TBaseDetailFrame.PopulateConceptGroupCombo 

{-------------------------------------------------------------------------------
  Gets the last 10 used concepts for a concept group combo box.  The concept group key, plus
      the name used to store the list in the registry are supplied.
}
procedure TBaseDetailFrame.PopulateLast10ConceptsCombo(AComboBox: TConceptGroupComboBox; const
    AConceptGroup, APersistentList: String; const AConceptGroup2: String = '');
var
  i: Integer;
  lKeys: TStringList;
  lParams: Array of Variant;
begin
  lKeys := TStringList.Create;
  try
    lKeys.Delimiter := ',';
    lKeys.QuoteChar := '''';
    lKeys.CommaText := AppSettings.GetItemsFromPersistentList(APersistentList);
  
    if lKeys.Count > 0 then
    begin
      SetLength(lParams, lKeys.Count * 2 + 4);
  
      // Prepare the parameters
      for i := lKeys.Count - 1 downto 0 do
      begin
        lParams[(i + 1) * 2 + 2] := '@Key' + IntToStr(i + 1);
        lParams[(i + 1) * 2 + 3] := lKeys[i];
      end;
  
      lParams[0] := '@Concept_Group_Key1';  // Set first concept group key
      lParams[1] := AConceptGroup;
      lParams[2] := '@Concept_Group_Key2';  // Set second, optional, concept group key
      lParams[3] := AConceptGroup2;
  
      with dmGeneral.GetRecordset('usp_Concept_Select_ForKeyListAndGroup', lParams) do begin
        while not Eof do begin
          AComboBox.Add(VarToStr(Fields['PlainText'].Value),
                        VarToStr(Fields['Concept_Key'].Value));
          MoveNext;
        end;
        Close;
      end;
    end;
  finally
    lKeys.Free;
  end;
end;  // TBaseDetailFrame.PopulateLast10ConceptsCombo 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterAsyncControl(AControl: TControl; AStoredProcName: String;
    AParams: Array of Variant; AOutputParam: String; ARecordsetIndex: Integer = 0;
    ACallbackProc: TAsyncExecuteCompleteProc = nil);
var
  lItem: TRegisteredAsyncControl;
begin
  lItem := TRegisteredAsyncControl.Create;
  lItem.Control        := AControl;
  lItem.StoredProcName := AStoredProcName;
  lItem.OutputParam    := AOutputParam;
  lItem.RecordsetIndex := ARecordsetIndex;
  
  // Set the parameters array, used when setting up the call to stored proc.
  lItem.SetParamCount(Length(AParams));
  if Length(AParams) > 0 then
    lItem.Params := VarArrayOf(AParams);
  
  // Set callback function, or default if none specified.
  if Assigned(ACallbackProc) then
    lItem.CallbackProc := ACallbackProc
  else
    lItem.CallbackProc := DisplayTextValue;
  
  FRegisteredAsyncControls.Add(lItem);
end;  // TBaseDetailFrame.RegisterAsyncControl 

{-------------------------------------------------------------------------------
  Registers a combo box as linked to a particular concept group.  The OnPopulate event of the
      control is linked to a private handler.
}
procedure TBaseDetailFrame.RegisterConceptGroupComboBox(AComboBox: TConceptGroupComboBox;
    AConceptGroupKey: String);
var
  i: Integer;
  lCombo: TCGComboAssistor;
begin
  // If the combo has already been register, just clear the content and
  // refresh the ConceptGroupKey.
  for i := 0 to FCGCombos.Count - 1 do begin
    lCombo := TCGComboAssistor(FCGCombos[i]);
    if lCombo.ComboBox = AComboBox then begin
      lCombo.ComboBox.Clear;  // Also reset the Populated flag.
      lCombo.ConceptGroupKey := AConceptGroupKey;
      Exit;
    end;
  end;
  
  // Create new item to store in the list.
  lCombo := TCGComboAssistor.Create;
  lCombo.ComboBox := AComboBox;
  lCombo.ConceptGroupKey := AConceptGroupKey;
  AComboBox.OnPopulate := PopulateConceptGroupCombo;
  
  // Add item to list.
  FCGCombos.Add(lCombo);
end;  // TBaseDetailFrame.RegisterConceptGroupComboBox 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterControl(AControl: TWinControl; const ADataFieldName: String;
    AMandatory: Boolean; const ADisplayFieldName: String; ARecordsetIndex: Integer = 0);
begin
  RegisterControl(AControl, ADataFieldName, '', AMandatory, ADisplayFieldName,
                  nil, '', nil, ARecordsetIndex);
end;  // TBaseDetailFrame.RegisterControl 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterControl(AControl: TWinControl; const ADataFieldName: String;
    ARecordsetIndex: Integer = 0);
begin
  RegisterControl(AControl, ADataFieldName, '', False, '', nil, '', nil, ARecordsetIndex);
end;  // TBaseDetailFrame.RegisterControl 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterControl(AControl: TWinControl; const ADataFieldName,
    AKeyFieldName: String; AMandatory: Boolean; const ADisplayFieldName: String;
    ACheckDataFunction: TCheckLinkedDataFunction = nil; const ARequestDataType: string = '';
    AKeyToCaptionFunction: TConvertKeyToCaptionFunction = nil; ARecordsetIndex: Integer = 0);
var
  lItem: TRegisteredControl;
begin
  // Create a different type of object depending on control's class
  if (AControl is TLinkedEdit) or (AControl is TLuxIDComboBox) then begin
    lItem := TRegisteredLinkedControl.Create;
    with TRegisteredLinkedControl(lItem) do begin
      KeyFieldName := AKeyFieldName;
      if AControl is TLinkedEdit then begin
        TLinkedEdit(AControl).OnFindData := LinkedEditFindData;
        TLinkedEdit(AControl).OnGetData := LinkedEditGetData;
        CheckLinkedData := ACheckDataFunction;  // Which function to use
        ConvertKeyToCaption := AKeyToCaptionFunction;
        DataType := ARequestDataType;
      end;
    end;
    // Default "no selection" item if field NOT required.
    if (AControl is TLuxIDComboBox) and not AMandatory then begin
      TLuxIDComboBox(AControl).HasNoSelectionItem := True;
      TLuxIDComboBox(AControl).NoSelectionItemText := ResStr_NoSelection;
    end;
  end else
  if (AControl is TVagueDateEdit) then begin
    lItem := TRegisteredVagueDateControl.Create;
    TVagueDateEdit(AControl).OnExit := VagueDateEditExit;
  end else
    lItem := TRegisteredControl.Create;
  
  // Common properties
  lItem.Control := AControl;
  lItem.DataFieldName := ADataFieldName;
  lItem.RecordsetIndex := ARecordsetIndex;
  lItem.Mandatory := AMandatory;
  lItem.DisplayFieldName := ADisplayFieldName;
  
  // Finally add to list
  FRegisteredControls.Add(lItem);
end;  // TBaseDetailFrame.RegisterControl 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterControl(AControl: TWinControl; const ADataFieldName,
    AKeyFieldName: String; ACheckDataFunction: TCheckLinkedDataFunction = nil; const
    ARequestDataType: string = ''; AKeyToCaptionFunction: TConvertKeyToCaptionFunction = nil;
    ARecordsetIndex: Integer = 0);
begin
  RegisterControl(AControl, ADataFieldName, AKeyFieldName, False, '', ACheckDataFunction,
                  ARequestDataType, AKeyToCaptionFunction, ARecordsetIndex);
end;  // TBaseDetailFrame.RegisterControl 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.RegisterControls;
begin
  // Do nothing by default. Override as needed.
  // Just set flag to say we've been there, no need to come back.
  FControlsRegistered := True;
end;  // TBaseDetailFrame.RegisterControls 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.RegisterRecordset(const AStoredProcName: String; AGetParamsFunction:
    TGetParamsFunction = nil; AGetStoredProcName: TGetStoredProcNameFunction = nil): Integer;
  
  //var
   // lblah: TGetStoredProcNameFunction;
  
begin
  Result := RegisterRecordset(AStoredProcName, AGetStoredProcName, AGetParamsFunction);
end;  // TBaseDetailFrame.RegisterRecordset 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.RegisterRecordset(const AStoredProcName: String; const
    AGetStoredProcName: TGetStoredProcNameFunction; AGetParamsFunction: TGetParamsFunction =
    nil): Integer;
var
  lItem: TRegisteredRecordset;
begin
  lItem := TRegisteredRecordset.Create;
  
  if Assigned(AGetStoredProcName) then
    lItem.GetStoredProcName := AGetStoredProcName
  else
    lItem.StoredProcName := AStoredProcName;
  
  if Assigned(AGetParamsFunction) then
    lItem.GetParameters := AGetParamsFunction
  else
    lItem.GetParameters := GetDefaultParams;
  
  Result := FRegisteredRecordsets.Add(lItem);
end;  // TBaseDetailFrame.RegisterRecordset 

{-------------------------------------------------------------------------------
}
function TBaseDetailFrame.RegisterRecordset(const AGetStoredProcName:
    TGetStoredProcNameFunction; AGetParamsFunction: TGetParamsFunction = nil): Integer;
begin
  Result := RegisterRecordset('', AGetStoredProcName, AGetParamsFunction);
end;  // TBaseDetailFrame.RegisterRecordset 

{-------------------------------------------------------------------------------
  Refreshes the display with orignal data. Discards any changes.
}
procedure TBaseDetailFrame.ReloadContent;
begin
  if FKey <> '' then LoadContent();
end;  // TBaseDetailFrame.ReloadContent 

{-------------------------------------------------------------------------------
  A frame that can be used as both a tabsheet and standalone frame must implement this method.
      This allows the frame container to resize a tabsheet-sized frame to occupy the space of
      a standalone-sized frame.
}
procedure TBaseDetailFrame.ResizeFrame(NewWidth, NewHeight: Integer);
begin
end;  // TBaseDetailFrame.ResizeFrame 

{-------------------------------------------------------------------------------
  Abstract method that is implemented by all details frames.  The details frame saves the data
      changes made to it when this method is called.
}
procedure TBaseDetailFrame.SaveContent;
begin
  // Save changes.
  SaveData;
end;  // TBaseDetailFrame.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SaveLast10ConceptsCombo(AComboBox: TConceptGroupComboBox; const
    AConceptGroup, APersistentList: string; const AConceptGroup2: string='');
var
  lRS: _Recordset;
  lVagueDate: TVagueDate;
  lIdx: Integer;
begin
  if Trim(AComboBox.Text) <> '' then
  begin
    lIdx := AComboBox.IndexOf(AComboBox.Text);
  
    if lIdx >= 0 then
      AComboBox.ItemIndex := lIdx
    else begin
      //find out whether measurement already exists
      lRS := dmGeneral.GetRecordset('usp_concept_Select_ForGroupAndItemName',
             ['@Concept_Group_Key', AConceptGroup, '@PlainText', AComboBox.Text]);
      with lRS do
      begin
        if not (Eof and Bof) then
        begin
          AComboBox.Add(AComboBox.Text, VarToStr(Fields['Concept_Key'].Value));
          AComboBox.ItemIndex := AComboBox.Count - 1;
          AppSettings.AddItemToPersistentList(APersistentList,
                                              Fields['Concept_Key'].Value, MAX_LAST_USED);
        end
        else
          if MessageDlg(Format(ResStr_AddMeasurementParameter, [AComboBox.Text]),
                        mtInformation, [mbYes, mbNo], 0) = mrYes then
            begin
              //get today's date
              lVagueDate := StringToVagueDate(FormatDateTime('dd/mm/yyyy', Today));
  
              {TODO: Insert doesn't work for some reason}
              AComboBox.Add(AComboBox.Text,
                            VarToStr(dmGeneral.RunInsertStoredProc('Concept',
                                     'usp_Concept_Insert',
                                     ['@ConceptGroupKey', AConceptGroup,
                                      '@TermName', AComboBox.Text,
                                      '@LanguageKey', AppSettings.ISOLanguage,
                                      '@VagueDateFrom', lVagueDate.StartDate,
                                      '@VagueDateTo', lVagueDate.EndDate,
                                      '@VagueDateType', lVagueDate.DateTypeString,
                                      '@Key', Null], '@Key')));
              AComboBox.ItemIndex := AComboBox.Count - 1;
              AppSettings.AddItemToPersistentList(APersistentList,
                                                  AComboBox.CurrentStrID,
                                                  MAX_LAST_USED);
            end
          else
            // Show find dialog allowing user to select item
            SearchForAdditionToLast10ConceptsCombo(AComboBox, AConceptGroup,
                                                   APersistentList, AConceptGroup2);
        Close;
      end;
    end;
  end;
end;  // TBaseDetailFrame.SaveLast10ConceptsCombo 

{-------------------------------------------------------------------------------
  Displays a find dialog, allowing the user to select a concept from the supplied concept
      group.  The concept is then added to the combo box list and selected.
}
procedure TBaseDetailFrame.SearchForAdditionToLast10ConceptsCombo(AComboBox:
    TConceptGroupComboBox; const AConceptGroup, APersistentList: string; const AConceptGroup2:
    string='');
var
  lSearchResult: String;
begin
  with TSearchManager.Create do
    try
      SearchType := stTermInConceptGroup;
      SearchKey := AConceptGroup;
      lSearchResult := RunSearch(AComboBox.Text);
      // if no search result, but have second concept group to search, then search it
      if (lSearchResult='') and (AConceptGroup2<>'') then begin
        SearchKey := AConceptGroup2;
        lSearchResult := RunSearch(AComboBox.Text);
      end;
      if lSearchResult <> '' then begin
        AComboBox.Add(ResultText, lSearchResult);
        AComboBox.ItemIndex := AComboBox.Count -1;
        AppSettings.AddItemToPersistentList(APersistentList, AComboBox.CurrentStrID, 10);
      end;
    finally
      Free;
    end; // try
end;  // TBaseDetailFrame.SearchForAdditionToLast10ConceptsCombo 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SetAdditionalProperties(AAdditionalProperties: TObject);
begin
  Assert(Supports(AAdditionalProperties, IID_IAdditionalProperties), 'bang!');
  FAdditionalProperties := AAdditionalProperties;
end;  // TBaseDetailFrame.SetAdditionalProperties 

{-------------------------------------------------------------------------------
  Sets the EditMode property. Calls the EnableControls method to allow descendents to
      enable/disable their controls.
}
procedure TBaseDetailFrame.SetEditMode(const Value: TEditMode);
begin
  FEditMode := Value;
  EnableControls(EditMode = emEdit);
end;  // TBaseDetailFrame.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SetKey(Value: TKeyString);
begin
  FKey := Value;
end;  // TBaseDetailFrame.SetKey 

{-------------------------------------------------------------------------------
  Get the Key and ParentKey values through the IAdditionalProperties interface.
}
procedure TBaseDetailFrame.SetKeys(const ACallerMethod: String);
var
  lNodeContext: Variant;
  lProperties: IAdditionalProperties;
begin
  // hold a pointer in case of an asynchronous call to DropInterfaceReferences
  lProperties := AdditionalProperties;
  
  if not Assigned(lProperties) then
    raise EBaseDetailFrameException.Create(Format(ResStr_InvalidMethodCall, [ACallerMethod]));
  
  // Setup the keys. Do not use FKey or FParentKey directly, the Setxxx methods are virtual!
  Key       := VarToStr(lProperties.GetProperty(PROP_KEY));
  ParentKey := VarToStr(lProperties.GetProperty(PROP_PARENT_KEY));
  
  // Not eveyone provide a PROP_NODE_CONTEXT, so before casting, check there is someting to cast.
  lNodeContext := lProperties.GetProperty(PROP_NODE_CONTEXT);
  if VarIsNull(lNodeContext) then NodeContext := ncNone
                             else NodeContext := TNodeContext(lNodeContext);
end;  // TBaseDetailFrame.SetKeys 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SetNodeContext(Value: TNodeContext);
begin
  FNodeContext := Value;
end;  // TBaseDetailFrame.SetNodeContext 

{-------------------------------------------------------------------------------
  Accessor method.  Frame notification event property.
}
procedure TBaseDetailFrame.SetOnFrameNotification(Value: TFrameNotificationEvent);
begin
  FOnFrameNotification := Value;
end;  // TBaseDetailFrame.SetOnFrameNotification 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SetParentKey(const Value: TKeyString);
begin
  FParentKey := Value;
end;  // TBaseDetailFrame.SetParentKey 

{-------------------------------------------------------------------------------
  Automates the coloration of mandatory controls.
}
procedure TBaseDetailFrame.SetRequiredFieldColours(AEnabled: boolean);
var
  i: Integer;
  lRegCtrl: TRegisteredControl;
begin
  // Automate colour change of required fields
  for i := 0 to FRegisteredControls.Count - 1 do begin
    lRegCtrl := TRegisteredControl(FRegisteredControls[i]);
    if lRegCtrl.Mandatory then
      if lRegCtrl.Control is TWinControl then
        SetRequiredFieldsColourState(AEnabled, [TWinControl(lRegCtrl.Control)])
      else
        raise EBaseDetailFrameException.Create(ResStr_AutoMandatoryControlNotWinControl);
  end;
end;  // TBaseDetailFrame.SetRequiredFieldColours 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SetRequiredFieldsColourState(AEnabled: Boolean; WinControls: Array
    of TWinControl);
var
  i: Integer;
  lColour: TColor;
begin
  if AEnabled then
    lColour := AppSettings.MandatoryColour
  else
    lColour := clWindow;
  
  for i := 0 to High(WinControls) do
    // Have to differentiate, or composite components don't get updated.
    if WinControls[i] is TBaseCompositeComponent then
      TBaseCompositeComponent(WinControls[i]).Color := lColour
    else
      TWinControlAccessor(WinControls[i]).Color := lColour;
end;  // TBaseDetailFrame.SetRequiredFieldsColourState 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.SynchedDisplayTextValue;
var
  lValue: String;
begin
  lValue := VarToStr(FDisplayTextValue);
  if FDisplayTextTarget is TLabel then
    with TLabel(FDisplayTextTarget) do begin
      Caption := lValue;
      Canvas.Font.Assign(Font);
      // If too wide, then display a hint and ... at the end
      if WordWrap then
        Caption := lValue
      else begin
        if AutoSize then
          if Constraints.MaxWidth <> 0 then
            Caption := GetTextWithinLimit(Canvas, lValue, Constraints.MaxWidth)
          else
            Caption := GetTextWithinLimit(Canvas, lValue, Parent.ClientWidth - Left)
        else
          Caption := GetTextWithinLimit(Canvas, lValue, Width);
  
        if Caption <> lValue then begin
          Hint := lValue;
          ShowHint := True;
        end else begin
          Hint := '';
          ShowHint := False;
        end;
      end;
    end
  else
  if FDisplayTextTarget is TMemo then
    TMemo(FDisplayTextTarget).Text := lValue;
end;  // TBaseDetailFrame.SynchedDisplayTextValue 

{-------------------------------------------------------------------------------
  Method called whenever the maps have been updated in Recorder.
}
procedure TBaseDetailFrame.UpdateMapWindowSelector;
begin
  // Override as and when needed.
end;  // TBaseDetailFrame.UpdateMapWindowSelector 

{-------------------------------------------------------------------------------
  Implement IRequestor.Update.  Marshalls the returned data to the method originally supplied
      to handle the data.
}
procedure TBaseDetailFrame.UpdateRequestedData(const KeyList: IKeyList);
var
  i: Integer;
  lLinkedEditText: String;
  lItem: IKeyItem;
begin
  if Assigned(FRequestDataRecipientMethod) then
    FRequestDataRecipientMethod(KeyList)
  else
  if Assigned(FRequestDataRecipientLinkedEdit) then begin
    for i := 0 to FRegisteredControls.Count - 1 do
      if FRegisteredControls[i] is TRegisteredLinkedControl then
        with TRegisteredLinkedControl(FRegisteredControls[i]) do
          if (Control is TLinkedEdit) and (Control = FRequestDataRecipientLinkedEdit) then
          begin
            lItem := KeyList.GetKeyItem(0);
            // These lines have been swapped around so that the Key is correct
            // for the Spatial Reference when coming back from the Location module.
            if CompareText(KeyList.TableName, MIXED_DATA) = 0 then
              // Keylist is mixed, proper table name in KeyField2.
              lLinkedEditText := ConvertKeyToCaption(lItem.KeyField1,
                                                     lItem.KeyField2)
            else
              lLinkedEditText := ConvertKeyToCaption(lItem.KeyField1,
                                                     KeyList.TableName);
            if lLinkedEditText <> '' then
              FRequestDataRecipientLinkedEdit.Key := lItem.KeyField1;
            FRequestDataRecipientLinkedEdit.Text := lLinkedEditText;
          end;
  end;
end;  // TBaseDetailFrame.UpdateRequestedData 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.VagueDateEditExit(Sender: TObject);
begin
  // Fired only for VagueDateEdit controls, or something has gone very wrong.
  with TVagueDateEdit(Sender) do begin
    try
      Text := VagueDateToString(StringToVagueDate(Text));
    except
      // Ignore the error on exit.
      on Exception do;
    end;
  end;
end;  // TBaseDetailFrame.VagueDateEditExit 

{-------------------------------------------------------------------------------
}
procedure TBaseDetailFrame.ValidateContent;
begin
  // Validate any registered mandatory controls are populated
  ValidateMandatoryControls;
  // Check vague dates contain valid dates.
  ValidateVagueDates;
  // Validate - this should raise an exception if there is a failure
  ValidateData;
end;  // TBaseDetailFrame.ValidateContent 

{-------------------------------------------------------------------------------
  Virtual method that is implemented by all details frames.  The details frame checks the data
      changes made to it when this method is called.  Raises exceptions if any validation
      tests fail.  This method is not abstract as some descendants may not need to implement
      it.
}
procedure TBaseDetailFrame.ValidateData;
var
  i: Integer;
  lRegCtrl: TRegisteredLinkedControl;
begin
  // Ensure all linked edits have obtained a key if the user has entered text
  for i := 0 to FRegisteredControls.Count - 1 do
    if FRegisteredControls[i] is TRegisteredLinkedControl then begin
      lRegCtrl := TRegisteredLinkedControl(FRegisteredControls[i]);
      if lRegCtrl.Control is TLinkedEdit then
        ValidateLinkedEdit(lRegCtrl);
    end;
end;  // TBaseDetailFrame.ValidateData 

{-------------------------------------------------------------------------------
  The code in this method used to be included in the ValidateData method. However, it has been
      separated so that it can be overridden if necessary. For instance, the ConceptGeneral
      frame in the Thesaurus Editor has a linked edit that might not have a key at validation,
      but would acquire a key during the save. Therefore it would fail this validation
      unnecessarily.
}
procedure TBaseDetailFrame.ValidateLinkedEdit(ALinkedControl: TRegisteredLinkedControl);
begin
  with TLinkedEdit(ALinkedControl.Control) do
    if (Text <> '') and (Key = '') and Assigned(OnFindData) then begin
      OnFindData(ALinkedControl.Control);
      if Key = '' then Abort;
    end;
end;  // TBaseDetailFrame.ValidateLinkedEdit 

{-------------------------------------------------------------------------------
  Automatically validates any registered mandatory controls are populated.
}
procedure TBaseDetailFrame.ValidateMandatoryControls;
var
  i: Integer;
  lRegCtrl: TRegisteredControl;
  lText: String;
  lDoCheck: Boolean;
begin
  // Automate validation required fields
  for i := 0 to FRegisteredControls.Count-1 do begin
    lRegCtrl := TRegisteredControl(FRegisteredControls[i]);

    if lRegCtrl.Mandatory then begin
      if not (lRegCtrl.Control is TWinControl) then
        raise EBaseDetailFrameException.Create(ResStr_AutoMandatoryControlNotWinControl);

      // If the control is disabled, or readonly, it doesn't need checking, otherwise
      // user would still be allowed to change the value, which would need checking.
      lDoCheck := True;

      if lRegCtrl.Control is TBaseCompositeComponent then
        lDoCheck := TBaseCompositeComponent(lRegCtrl.Control).EditMode = emEdit
      else
      // TCustomEdit is ancestor for TEdit, TMemo and TRichEdit
      if lRegCtrl.Control is TCustomEdit then
        lDoCheck := not TCustomEditAccessor(lRegCtrl.Control).ReadOnly
      else
      if lRegCtrl.Control is TLuxIDComboBox then
        lDoCheck := not TLuxIDComboBox(lRegCtrl.Control).ReadOnly
      else
      // TButtonControl is ancestor to TButton, TCheckBox and TRadioButton
      if lRegCtrl.Control is TButtonControl then
        lDoCheck := (lRegCtrl.Control as TButtonControl).Enabled;

      if lDoCheck then begin
        if lRegCtrl.Control is TLinkedEdit then
          lText := TLinkedEdit(lRegCtrl.Control).Text
        else
          lText := TControlAccessor(lRegCtrl.Control).Text;

        ValidateValue(Trim(lText) <> '',
                      Format(ResStr_MissingData, [lRegCtrl.DisplayFieldName]),
                      TWinControl(lRegCtrl.Control));
      end;
    end;
  end;
end;  // TBaseDetailFrame.ValidateMandatoryControls 

{-------------------------------------------------------------------------------
  Automatically validates vague dates.
}
procedure TBaseDetailFrame.ValidateVagueDates;
var
  i: Integer;
  lVagueDate: TVagueDate;
  lRegCtrl: TRegisteredControl;
  lVagueDateString: String;
begin
  for i := 0 to FRegisteredControls.Count-1 do begin
    lRegCtrl := TRegisteredControl(FRegisteredControls[i]);
    if lRegCtrl.Control is TVagueDateEdit then begin
      try
        lVagueDateString := TVagueDateEdit(lRegCtrl.Control).Text;
        lVagueDate := StringToVagueDate(lVagueDateString);
      except
        on Exception do
          ValidateValue(False, lVagueDateString + ResStr_IsAnInvalidVagueDate);
      end;
    end;
  end;
end;  // TBaseDetailFrame.ValidateVagueDates 

{-==============================================================================
    TRegisteredRecordset
===============================================================================}
{-------------------------------------------------------------------------------
}
function TRegisteredRecordset.RetrieveStoredProcName: String;
begin
  if Assigned(FGetStoredProcName) then
    Result := FGetStoredProcName
  else
    Result := FStoredProcName;
end;  // TRegisteredRecordset.RetrieveStoredProcName 

end.




