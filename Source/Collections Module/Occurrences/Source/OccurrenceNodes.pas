{===============================================================================
  Unit:        OccurrenceNodes

  Defines:     TOccurrenceNodes

  Description: Implements IOccurrenceNodeManager interface from Recorder.

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 9 $
    $Date: 11/05/09 16:51 $
    $Author: Ericsalmon $

===============================================================================}

unit OccurrenceNodes;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Dialogs, Variants, ComObj, ActiveX, StdVcl, Contnrs, Occurrences_TLB,
  Recorder2000_TLB, ADODB_TLB, ContainerFormOcc;

type
  TOccurrenceNodes = class (TAutoObject, IRecorderAddin, IOccurrenceNodes, 
      IOccurrenceNodeManager)
  private
    FDomains: TObjectList;
    FLastDomainMask: Integer;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    function Get_ImageList: LongWord; safecall;
    function Get_Name: WideString; safecall;
    function Get_StateImageList: LongWord; safecall;
    function Get_TypeCount: Integer; safecall;
    function Get_TypeID(Index: Integer): Integer; safecall;
    function Get_TypeName(TypeID: Integer): WideString; safecall;
    procedure PopulateDomains;
  public
    destructor Destroy; override;
    function AddNode(TypeID: Integer; const ParentItemKey: WideString; var NodeInfo:
        TNodeInfo): WordBool; safecall;
    function GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): 
        WordBool; safecall;
    function GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; 
        safecall;
    function GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: WideString; var 
        SQL: WideString): WordBool; safecall;
    function GetNodeInfo(TypeID: Integer; const DataFields: Fields; var NodeInfo: TNodeInfo): 
        WordBool; safecall;
    function GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; var NodeInfo: TNodeInfo): 
        WordBool; safecall;
    procedure Initialize; override;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    function NodeChanged(const ParentItemKey: WideString; var NodeInfo: TNodeInfo; var 
        DetailScreenGUID: TGUID): WordBool; safecall;
  end;
  
//==============================================================================
implementation

uses
  ComServ, DataClasses, ApplicationSettings, GeneralData, InterfaceDataModule,
  ResourceStrings, LuxembourgConstants;

{-==============================================================================
    TOccurrenceNodes
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TOccurrenceNodes.Destroy;
begin
  FDomains.Free;
  if TdmGeneral.Allocated then TdmGeneral.Discard;
  inherited Destroy;
end;  // TOccurrenceNodes.Destroy 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.AddNode(TypeID: Integer; const ParentItemKey: WideString; var 
    NodeInfo: TNodeInfo): WordBool;
begin
  Result := True;
  NodeInfo.ItemKey := '';
  NodeInfo.ChildrenPopulated := True;
  NodeInfo.TypeID := TypeID;
  if TypeID = FDomains.Count - 2 then begin
    NodeInfo.ImageIndex := 37;  // Determination in ilBrowserNodes.
    NodeInfo.TableName := TN_DETERMINATION;
    NodeInfo.Caption := Format(ResStr_NewNodeSpecified, [ResStr_Determination]);
  end else
  if TypeID = FDomains.Count - 1 then begin
    // Check there is at least 1 determination before allowing measurements to be added.
    with dmGeneral.GetRecordset('usp_Determinations_Select_ForOccurrence',
                                ['@OccurrenceKey', ParentItemKey]) do
    begin
      if Eof then begin
        Result := False;
        MessageDlg(ResStr_MissingDetermination, mtInformation, [mbOk], 0);
      end else begin
        NodeInfo.ImageIndex := 31;  // Measurement in ilBrowserNodes.
        NodeInfo.TableName := TN_OCCURRENCE_DATA;
        NodeInfo.Caption := Format(ResStr_NewNodeSpecified, [ResStr_Measurement]);
      end;
      Close;
    end;
  end else begin
    NodeInfo.ChildrenPopulated := False;
    NodeInfo.ImageIndex := 3;  // Specimen in ilBrowserNodes.
    NodeInfo.TableName := TN_OCCURRENCE;
    NodeInfo.Caption := Format(ResStr_NewNodeSpecified, [Get_TypeName(TypeID)]);
  end;
end;  // TOccurrenceNodes.AddNode 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: 
    Integer): WordBool;
begin
  PopulateDomains;
  Result := True;
  // All Occurrence types are allowed to have Determination and Measurement sub-nodes.
  // But not Determinations and Measurements, hence the count-4
  if (ParentTypeID > -1) and (ParentTypeID < FDomains.Count - 4) then
    case Index of
      0: TypeID := FDomains.Count - 2;  // Determination TypeID index in FDomains
      1: TypeID := FDomains.Count - 1;  // Measurement TypeID index in FDomains
    else
      Result := False;
    end
  else
    Result := False;
end;  // TOccurrenceNodes.GetAddSubType 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: 
    Integer): WordBool;
begin
  PopulateDomains;
  Result := True;
  // -1 is level above occurrences and 0 to Count - 4 refer to domains from DB.
  if ParentTypeID < FDomains.Count - 4 then begin
    if Index < FDomains.Count - 4 then TypeID := Index else Result := False;
  end else
  // All other cases, only 1 option.
  if Index = 0 then begin
    // Folder for Determinations or Measurements, allowed one option only.
    if (ParentTypeID = FDomains.Count - 4) or (ParentTypeID = FDomains.Count - 3) then
      TypeID := ParentTypeID + 2
    else
    // Determination or Measurement item, allowed one option only.
    if (ParentTypeID = FDomains.Count - 2) or (ParentTypeID = FDomains.Count - 1) then
      TypeID := ParentTypeID
    else
      Result := False;
  end else
    Result := False;
end;  // TOccurrenceNodes.GetAddType 

{-------------------------------------------------------------------------------
  Called when node is expanded. 
}
function TOccurrenceNodes.GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: 
    WideString; var SQL: WideString): WordBool;
begin
  PopulateDomains;
  Result := True;
  // Active node is level above Occurrences.
  if ParentTypeID = -1 then
    SQL := 'EXECUTE usp_Occurrences_Select_ForSample ''' + ParentKey + ''''
  else
  // Determinations.
  if ParentTypeID = FDomains.Count - 4 then begin
    if TypeID = FDomains.Count - 2 then
      SQL := 'EXECUTE usp_Determinations_Select_ForOccurrence ''' + ParentKey + ''''
    else
      Result := False;
  end else
  // Measurements.
  if ParentTypeID = FDomains.Count - 3 then begin
    if TypeID = FDomains.Count - 1 then
      SQL := 'EXECUTE usp_Measurements_Select_ForOccurrence ''' + ParentKey + ''''
    else
      Result := False;
  end else
    Result := False;
end;  // TOccurrenceNodes.GetData 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.GetNodeInfo(TypeID: Integer; const DataFields: Fields; var NodeInfo: 
    TNodeInfo): WordBool;
begin
  PopulateDomains;
  Result := True;
  
  if Assigned(DataFields) then begin
    NodeInfo.Caption := VarToStr(DataFields['Item_Name'].Value);
    NodeInfo.ItemKey := VarToStr(DataFields['Item_Key'].Value);
  end;
  
  NodeInfo.ChildrenPopulated := True;
  NodeInfo.TypeID := TypeID;
  if NodeInfo.TypeID = FDomains.Count - 2 then begin
    NodeInfo.ImageIndex := 37;  // Determination in ilBrowserNodes.
    NodeInfo.TableName := TN_DETERMINATION;
  end else
  if NodeInfo.TypeID = FDomains.Count - 1 then begin
    NodeInfo.ImageIndex := 31;  // Measurement in ilBrowserNodes.
    NodeInfo.TableName := TN_OCCURRENCE_DATA;
  end else begin
    NodeInfo.ChildrenPopulated := False;
    NodeInfo.ImageIndex := 3;  // Specimen in ilBrowserNodes.
    NodeInfo.TableName := TN_OCCURRENCE;
    NodeInfo.Editable := True;
    if NodeInfo.Caption = '' then NodeInfo.Caption := ResStr_NoDetermination;
  end;
end;  // TOccurrenceNodes.GetNodeInfo

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; var NodeInfo: 
    TNodeInfo): WordBool;
begin
  PopulateDomains;
  Result := True;
  if (ParentTypeID > -1) and (ParentTypeID < FDomains.Count - 4) then
    case Index of
      0: NodeInfo.TypeID := FDomains.Count - 4;  // Allow Determinations.
      1: NodeInfo.TypeID := FDomains.Count - 3;  // Allow Measurements.
    else
      Result := False;  // No more sub nodes.
    end
  else
    Result := False;  // All other types don't have sub nodes.
  
  // Setup remaining properties.
  if Result then
    with NodeInfo do begin
      ChildrenPopulated := False;
      Deletable := False;
      Editable := False;
      ImageIndex := 1;  // Folder in ilBrowserNodes
      Caption := Get_TypeName(TypeID);
    end;
end;  // TOccurrenceNodes.GetSubNodeInfo 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_Description: WideString;
begin
  Result := 'Luxembourg Occurrences addin module for Recorder 6.';
end;  // TOccurrenceNodes.Get_Description 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_ImageFileName: WideString;
begin
  Result := '';
end;  // TOccurrenceNodes.Get_ImageFileName 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_ImageList: LongWord;
begin
  Result := dmInterface.ilBrowserNodes.Handle;
end;  // TOccurrenceNodes.Get_ImageList 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_Name: WideString;
begin
  Result := 'Occurrences';
end;  // TOccurrenceNodes.Get_Name 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_StateImageList: LongWord;
begin
  Result := 0;  // No state provided by addin.
end;  // TOccurrenceNodes.Get_StateImageList 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_TypeCount: Integer;
begin
  PopulateDomains;
  Result := FDomains.Count;  // Includes Folder Node types too.
end;  // TOccurrenceNodes.Get_TypeCount 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_TypeID(Index: Integer): Integer;
begin
  Result := Index;  // Keeping it simple.
end;  // TOccurrenceNodes.Get_TypeID 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.Get_TypeName(TypeID: Integer): WideString;
begin
  PopulateDomains;
  // Because of the way the Types are created, TypeID can be used as Index with FDomains.
  Result := TKeyData(FDomains[TypeID]).ItemAdditional;
end;  // TOccurrenceNodes.Get_TypeName 

{-------------------------------------------------------------------------------
}
procedure TOccurrenceNodes.Initialize;
begin
  inherited Initialize;
  FDomains := TObjectList.Create;
end;  // TOccurrenceNodes.Initialize 

{-------------------------------------------------------------------------------
}
procedure TOccurrenceNodes.Install(const iInstalledFilePath: WideString);
begin
  
end;  // TOccurrenceNodes.Install 

{-------------------------------------------------------------------------------
}
function TOccurrenceNodes.NodeChanged(const ParentItemKey: WideString; var NodeInfo: TNodeInfo;
    var DetailScreenGUID: TGUID): WordBool;
begin
  Result := True;
  NodeInfo.Editable := True;
  NodeInfo.Deletable := True;
  
  // Not allowed to delete last determination of an occurrence.
  if NodeInfo.TypeID = FDomains.Count - 2 then
    with dmGeneral.GetRecordset('usp_Determinations_Select_ForOccurrence',
                                ['@OccurrenceKey', ParentItemKey]) do
    begin
      NodeInfo.Deletable := RecordCount > 1;
      Close;
    end;
  
  if (NodeInfo.TypeID = FDomains.Count - 2) or
     (NodeInfo.TypeID = FDomains.Count - 1) or
     (NodeInfo.TypeID < FDomains.Count - 4) then
    DetailScreenGUID := CLASS_frmContainerOcc
  else begin
    NodeInfo.Editable := False;
    NodeInfo.Deletable := False;
  end;
end;  // TOccurrenceNodes.NodeChanged 

{-------------------------------------------------------------------------------
}
procedure TOccurrenceNodes.PopulateDomains;
  
  function CreateItem(const AName, AKey: String): TKeyData;
  begin
    Result := TKeyData.Create;
    Result.ItemKey := AKey;
    Result.ItemAdditional := AName;
  end;
  
begin
  // No items means not populated yet.
  if (FDomains.Count = 0) or (AppSettings.DomainMask <> FLastDomainMask) then begin
    FLastDomainMask := AppSettings.DomainMask;
    FDomains.Clear;
    // Get the list of domains.
    with dmGeneral.GetRecordset('usp_Domains_Select_ForOccurrences',
                          ['@NameKey', dmGeneral.Recorder.CurrentSettings.UserIDKey]) do
      while not Eof do begin
        FDomains.Add(CreateItem(VarToStr(Fields['Item_Name'].Value),
                                VarToStr(Fields['Domain_Key'].Value)));
        MoveNext;
      end;
  
    // Add the Determinations/Measurements Folder Node items.
    FDomains.Add(CreateItem(ResStr_Determinations, ''));
    FDomains.Add(CreateItem(ResStr_Measurements, ''));
  
    // Add the Determination/Measurement type items.
    FDomains.Add(CreateItem(ResStr_Determination, ''));
    FDomains.Add(CreateItem(ResStr_Measurement, ''));
  end;
end;  // TOccurrenceNodes.PopulateDomains 

initialization
  TAutoObjectFactory.Create(ComServer, TOccurrenceNodes, Class_OccurrenceNodes,
    ciMultiInstance, tmApartment);
end.
