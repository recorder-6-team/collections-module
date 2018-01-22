{===============================================================================
  Unit:        LocationNodes

  Defines:     TLocationNodes

  Description: Implements ILocationNodeManager interface from Recorder.

  Model:       Occurrences.mpb

  Created:     October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 1/09/04 17:03 $
    $Author: Ericsalmon $

===============================================================================}

unit LocationNodes;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Dialogs, Variants, ComObj, ActiveX, StdVcl, Contnrs, Occurrences_TLB,
  Recorder2000_TLB, ADODB_TLB, ContainerFormLoc;

type
  TLocationNodes = class (TAutoObject, IRecorderAddin, ILocationNodes, 
      ILocationNodeManager)
  private
    FDomains: TObjectList;
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
    TLocationNodes
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TLocationNodes.Destroy;
begin
  FDomains.Free;
  
  inherited Destroy;
end;  // TLocationNodes.Destroy 

{-------------------------------------------------------------------------------
}
function TLocationNodes.AddNode(TypeID: Integer; const ParentItemKey: WideString; var 
    NodeInfo: TNodeInfo): WordBool;
begin
  Result := True;
  NodeInfo.ItemKey := '';
  NodeInfo.ChildrenPopulated := True;
  NodeInfo.TypeID := TypeID;

  if TypeID = FDomains.Count - 1 then begin
    NodeInfo.ImageIndex := 31;  // Measurement in ilBrowserNodes.
    NodeInfo.TableName := TN_LOCATION_FEATURE_DATA;
    NodeInfo.Caption := Format(ResStr_NewNodeSpecified, [ResStr_Measurement]);
  end else
    Result := False;
end;  // TLocationNodes.AddNode 

{-------------------------------------------------------------------------------
}
function TLocationNodes.GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: 
    Integer): WordBool;
begin
  PopulateDomains;
  Result := True;
  // All Location Features are allowed to have Measurement sub-nodes.
  // But not Measurements.
  if (ParentTypeID < 0) and (Index = 0) then
    TypeID := FDomains.Count - 1  // Measurement TypeID index in FDomains
  else
    Result := False;
end;  // TLocationNodes.GetAddSubType

{-------------------------------------------------------------------------------
}
function TLocationNodes.GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: 
    Integer): WordBool;
begin
  PopulateDomains;
  Result := True;

  if (ParentTypeID > -1) and (Index = 0) then begin
    // Folder for Measurements, allowed one option only.
    if ParentTypeID = FDomains.Count - 2 then
      TypeID := ParentTypeID + 1
    else
    // Measurement item, allowed one option only.
    if ParentTypeID = FDomains.Count - 1 then
      TypeID := ParentTypeID
    else
      Result := False;
  end else
    Result := False;
end;  // TLocationNodes.GetAddType 

{-------------------------------------------------------------------------------
  Called when node is expanded. 
}
function TLocationNodes.GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: 
    WideString; var SQL: WideString): WordBool;
begin
  PopulateDomains;
  Result := True;
  // Active node is level feature.
  if ParentTypeID = FDomains.Count - 2 then
    if TypeID = FDomains.Count - 1 then
      SQL := 'EXECUTE usp_Measurements_Select_ForLocationFeature ''' + ParentKey + ''''
    else
      Result := False
  else
    Result := False;
end;  // TLocationNodes.GetData 

{-------------------------------------------------------------------------------
}
function TLocationNodes.GetNodeInfo(TypeID: Integer; const DataFields: Fields; var NodeInfo: 
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
  if NodeInfo.TypeID = FDomains.Count - 1 then begin
    NodeInfo.ImageIndex := 31;  // Measurement in ilBrowserNodes.
    NodeInfo.TableName := TN_LOCATION_FEATURE_DATA;
  end;
end;  // TLocationNodes.GetNodeInfo

{-------------------------------------------------------------------------------
}
function TLocationNodes.GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; var NodeInfo: 
    TNodeInfo): WordBool;
begin
  PopulateDomains;
  Result := True;
  // Parent is Feature node, and allowed only measurements, and always show folder.
  if (ParentTypeID = -1) and (Index = 0) then
    NodeInfo.TypeID := FDomains.Count - 2  // Allow Measurements.
  else
    Result := False;  // No more sub nodes.

  // Setup remaining properties.
  if Result then
    with NodeInfo do begin
      ChildrenPopulated := False;
      Deletable := False;
      Editable := False;
      ImageIndex := 1;  // Folder in ilBrowserNodes
      Caption := Get_TypeName(TypeID);
    end;
end;  // TLocationNodes.GetSubNodeInfo 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_Description: WideString;
begin
  Result := 'Luxembourg Locations addin module for Recorder 6.';
end;  // TLocationNodes.Get_Description 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_ImageFileName: WideString;
begin
  Result := '';
end;  // TLocationNodes.Get_ImageFileName 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_ImageList: LongWord;
begin
  Result := dmInterface.ilBrowserNodes.Handle;
end;  // TLocationNodes.Get_ImageList 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_Name: WideString;
begin
  Result := 'Locations';
end;  // TLocationNodes.Get_Name 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_StateImageList: LongWord;
begin
  Result := 0;  // No state provided by addin.
end;  // TLocationNodes.Get_StateImageList 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_TypeCount: Integer;
begin
  PopulateDomains;
  Result := FDomains.Count;  // Includes Folder Node types too.
end;  // TLocationNodes.Get_TypeCount 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_TypeID(Index: Integer): Integer;
begin
  Result := Index;  // Keeping it simple.
end;  // TLocationNodes.Get_TypeID 

{-------------------------------------------------------------------------------
}
function TLocationNodes.Get_TypeName(TypeID: Integer): WideString;
begin
  PopulateDomains;
  // Because of the way the Types are created, TypeID can be used as Index with FDomains.
  Result := TKeyData(FDomains[TypeID]).ItemAdditional;
end;  // TLocationNodes.Get_TypeName 

{-------------------------------------------------------------------------------
}
procedure TLocationNodes.Initialize;
begin
  inherited Initialize;
  FDomains := TObjectList.Create;
end;  // TLocationNodes.Initialize 

{-------------------------------------------------------------------------------
}
procedure TLocationNodes.Install(const iInstalledFilePath: WideString);
begin
  
end;  // TLocationNodes.Install 

{-------------------------------------------------------------------------------
}
function TLocationNodes.NodeChanged(const ParentItemKey: WideString; var NodeInfo: TNodeInfo;
    var DetailScreenGUID: TGUID): WordBool;
begin
  Result := True;
  NodeInfo.Editable := True;
  NodeInfo.Deletable := True;
  
  if NodeInfo.TypeID = FDomains.Count - 1 then   // Measurement node.
    DetailScreenGUID := CLASS_frmContainerLoc
  else begin
    NodeInfo.Editable := False;
    NodeInfo.Deletable := False;
  end;
end;  // TLocationNodes.NodeChanged 

{-------------------------------------------------------------------------------
}
procedure TLocationNodes.PopulateDomains;

  function CreateItem(const AName, AKey: String): TKeyData;
  begin
    Result := TKeyData.Create;
    Result.ItemKey := AKey;
    Result.ItemAdditional := AName;
  end;

begin
  // No items means not populated yet.
  if FDomains.Count = 0 then begin
    // Add the Measurements Folder Node items.
    FDomains.Add(CreateItem(ResStr_Measurements, ''));

    // Add the Measurement type items.
    FDomains.Add(CreateItem(ResStr_Measurement, ''));
  end;
end;  // TLocationNodes.PopulateDomains 

initialization
  TAutoObjectFactory.Create(ComServer, TLocationNodes, Class_LocationNodes,
    ciMultiInstance, tmApartment);
end.