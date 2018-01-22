{===============================================================================
  Unit:        CollectionsMenus

  Defines:     TCollectionsMenus

  Description: Provides an interface to Recorder by declaring an addin that
               implements the menus

  Created:     Oct 2003

  Model:       CollectionsModuleManager.mpb

  Last revision information:
    $Revision: 17 $
    $Date: 31/01/11 16:02 $
    $Author: Jamesbichard $

===============================================================================}
unit CollectionsMenus;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, ComObj, ActiveX, SysUtils, CollectionsModuleManager_TLB, StdVcl,
  Recorder2000_TLB, QuickEntryMenuItem, ThesaurusBrowserMenuItem, Variants,
  CollectionsBrowserMenuItem, ExtendedUserConfigMenuItem, SpecimenFinderMenuItem,
  ReportSeparator, MenuImages, Contnrs, DSSDataTypes;

type
  TSingletonCollectionsMenusFactory = class (TAutoObjectFactory)
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
  end;

  {-----------------------------------------------------------------------------
    Implementation of IReportKeytype interface
  }
  TReportKeyType = class(TAutoObject, IReportKeytype)
  private
    FName: String;
    FMultivalue: Boolean;
    function Get_Name: WideString; safecall;
    function Get_Multivalue: WordBool; safecall;
  public
    constructor Create(AName: String; AMultivalue: Boolean);
    property Name: WideString read Get_Name;
    property Multivalue: WordBool read Get_Multivalue;
  end;
  
  {-----------------------------------------------------------------------------
    Class managing all the permanent menu items for the Collections Module.
  }
  TCollectionsMenus = class (TAutoObject, ICollectionsMenus, IRecorderAddin,
      IDynamicMenuList, IOptionsPages, IMapDropFormat, IReportKeytypeList,
      INodeMenuManager)
  private
    FCollectionsBrowserMenuItem: TCollectionsBrowserMenuItem;
    FdmMenuImages: TdmMenuImages;
    FExtendedUserConfigMenuItem: TExtendedUserConfigMenuItem;
    FInitialisedDatasetTableName: String;
    FMapPoints: TObjectList;
    FQuickEntryMenuItem: TQuickEntryMenuItem;
    FRptSeparatorMenuItem: TReportSeparator;
    FSpecimenFinderMenuItem: TSpecimenFinderMenuItem;
    FThesaurusBrowserMenuItem: TThesaurusBrowserMenuItem; 
    FKeyTypes: TInterfaceList;
    function GetCollectionsBrowserMenuItem: TCollectionsBrowserMenuItem;
    function GetExtendedUserConfigMenuItem: IDynamicMenu;
    function GetQuickEntryMenuItem: IDynamicMenu;
    function GetRptSeparatorMenuItem: TReportSeparator;
    function GetSpecimenFinderMenuItem: TSpecimenFinderMenuItem;
    function GetThesaurusBrowserMenuItem: IDynamicMenu;
    function GetMenuItemsForNode(ANodeInfo: TNodeInfo): IDynamicMenuList; safecall;
    function Get_Count: Integer; safecall;
    function Get_DatasetTableName: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    function Get_ImageListHandle: Integer; safecall; 
    function Get_KeytypeCount: Integer; safecall;
    function Get_Keytype(Index: Integer): IReportKeytype; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
    function Get_MapPoint(Index: Integer): IMapPoint; safecall;
    function Get_MapPointCount: Integer; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_OptionsPageCount: Integer; safecall;
    function Get_OptionsPageItems(Index: Integer): IOptionsPage; safecall;
    function Get_SupportedTable(Index: Integer): WideString; safecall;
    function Get_SupportedTableCount: Integer; safecall;
    procedure InitialiseDataset(const KeyList: IKeyList); safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    function Items(AIndex: Integer): IDynamicMenu; safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
  end;
  
//==============================================================================
implementation

uses
  ComServ, ResourceStrings, ImgList, ApplicationSettings, SpatialRefFuncs,
  GeneralData, ADOInt, VersionInfo;

const
  ITEM_COUNT = 6;

  // menu item indexes
  MNU_COLLECTIONS_BROWSER = 0;
  MNU_THESAURUS_BROWSER = 1;
  MNU_EXTENDED_USER_CONFIG = 2;
  MNU_REPORTS_SEPARATOR = 3;
  MNU_SPECIMEN_FINDER = 4;
  // Quick Entry last so it can easily be disabled using security
  MNU_QUICK_ENTRY = 5;

  Class_COMMapPoint: TGUID = '{9B565CBB-ACB2-459E-94E3-61DE0E090394}';

var
  mComObject: TCOMObject;

type
  TMapPoint = class (TObject)
  private
    FAccuracy: Integer;
    FDate: TDateTime;
    FLat: Double;
    FLong: Double;
    FRecordKey: String;
    FValue: Integer;
  public
    procedure Assign(ASource: TMapPoint);
    property Accuracy: Integer read FAccuracy write FAccuracy;
    property Date: TDateTime read FDate write FDate;
    property Lat: Double read FLat write FLat;
    property Long: Double read FLong write FLong;
    property RecordKey: String read FRecordKey write FRecordKey;
    property Value: Integer read FValue write FValue;
  end;
  
  TCOMMapPoint = class (TComObject, IMapPoint)
  private
    FMapPoint: TMapPoint;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer; 
        DispIDs: pointer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetTypeInfoCount(out Count: integer ): HResult; stdcall;
    function Get_Accuracy: Integer; safecall;
    function Get_Date: TDateTime; safecall;
    function Get_Lat: Double; safecall;
    function Get_Long: Double; safecall;
    function Get_RecordKey: WideString; safecall;
    function Get_Value: Integer; safecall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var
        Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
  public
    constructor Create(AMapPoint: TMapPoint);
    destructor Destroy; override;
  end;
  
{-==============================================================================
    TCollectionsMenus
===============================================================================}
{-------------------------------------------------------------------------------
  Cleanup the data module. 
}
destructor TCollectionsMenus.Destroy;
begin       
  FKeyTypes.Free;
  FKeyTypes := nil;
  FMapPoints.Free;
  FdmMenuImages.Free;
  DiscardAppSettings;
  inherited;
end;  // TCollectionsMenus.Destroy 

{-------------------------------------------------------------------------------
  Returns the collections browser IDynamicMenu instance, creating the instance if required. 
}
function TCollectionsMenus.GetCollectionsBrowserMenuItem: TCollectionsBrowserMenuItem;
begin
  if not Assigned(FCollectionsBrowserMenuItem) then
    // Create a COM class for the menu item
    FCollectionsBrowserMenuItem := TCollectionsBrowserMenuItem.Create;
  Result := FCollectionsBrowserMenuItem;
end;  // TCollectionsMenus.GetCollectionsBrowserMenuItem 

{-------------------------------------------------------------------------------
  Returns the extended user config IDynamicMenu instance, creating the instance if required. 
}
function TCollectionsMenus.GetExtendedUserConfigMenuItem: IDynamicMenu;
begin
  Result := nil;
  if TUserAccessLevel(AppSettings.UserAccessLevel) < ualAdmin then
    Exit;
  if not Assigned(FExtendedUserConfigMenuItem) then
    // Create a COM class for the menu item
    FExtendedUserConfigMenuItem := TExtendedUserConfigMenuItem.Create;
  Result := FExtendedUserConfigMenuItem;
end;  // TCollectionsMenus.GetExtendedUserConfigMenuItem 

{-------------------------------------------------------------------------------
  Returns the Quick Entry dynamic menu item 
}
function TCollectionsMenus.GetQuickEntryMenuItem: IDynamicMenu;
begin
  Result := nil;
  if TUserAccessLevel(AppSettings.UserAccessLevel) < ualRecordOnly then
    Exit;

  if not Assigned(FQuickEntryMenuItem) then begin
    // Create a COM wrapper class for the menu item
    FQuickEntryMenuItem := TQuickEntryMenuItem.Create;
    // Assign the appropriate class to manage the menu item
    FQuickEntryMenuItem.InternalMenuItem := TQuickEntryTopLevelMenuItem.Create;
  end;
  Result := FQuickEntryMenuItem;
end;  // TCollectionsMenus.GetQuickEntryMenuItem 

{-------------------------------------------------------------------------------
  Returns the report separator menu item, instantiating it if necessary. 
}
function TCollectionsMenus.GetRptSeparatorMenuItem: TReportSeparator;
begin
  if not Assigned(FRptSeparatorMenuItem) then
    // Create a COM class for the menu item
    FRptSeparatorMenuItem := TReportSeparator.Create;
  Result := FRptSeparatorMenuItem;
end;  // TCollectionsMenus.GetRptSeparatorMenuItem 

{-------------------------------------------------------------------------------
  Returns the specimen finder IDynamicMenu instance, creating the instance if required. 
}
function TCollectionsMenus.GetSpecimenFinderMenuItem: TSpecimenFinderMenuItem;
begin
  if not Assigned(FSpecimenFinderMenuItem) then
    // Create a COM class for the menu item
    FSpecimenFinderMenuItem := TSpecimenFinderMenuItem.Create;
  Result := FSpecimenFinderMenuItem;
end;  // TCollectionsMenus.GetSpecimenFinderMenuItem 

{-------------------------------------------------------------------------------
  Returns the Thesaurus browser IDynamicMenu instance, creating the instance if required.
}
function TCollectionsMenus.GetThesaurusBrowserMenuItem: IDynamicMenu;
begin
  if not Assigned(FThesaurusBrowserMenuItem) then
    // Create a COM class for the menu item
    FThesaurusBrowserMenuItem := TThesaurusBrowserMenuItem.Create;
  Result := FThesaurusBrowserMenuItem;
end;  // TCollectionsMenus.GetThesaurusBrowserMenuItem


{-------------------------------------------------------------------------------
  If the node is an occurrence (either taxon or thesaurus), then returns
  a "load into quick entry" menu item.
}
function TCollectionsMenus.GetMenuItemsForNode(ANodeInfo: TNodeInfo): IDynamicMenuList;
begin
  If ((CompareText(ANodeInfo.TableName, 'Taxon_Occurrence') = 0)
      or (CompareText(ANodeInfo.TableName, 'Occurrence') = 0))
      and AppSettings.AllowQuickEntry then
    Result := TLoadIntoQuickEntryMenu.Create(ANodeInfo)
  else
    Result := nil;
end;  // TCollectionsMenus.GetMenuItemsForNode


{-------------------------------------------------------------------------------
  Count of top level quick entry menu items. 
}
function TCollectionsMenus.Get_Count: Integer;
begin
  Result := ITEM_COUNT;
  // Disable quick entry if user not allowed access
  if not (AppSettings.AllowQuickEntry) then
    Dec(Result);
end;  // TCollectionsMenus.Get_Count 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_DatasetTableName: WideString;
begin
  Result := FInitialisedDatasetTableName;
end;  // TCollectionsMenus.Get_DatasetTableName 

{-------------------------------------------------------------------------------
  Description of addin  
}
function TCollectionsMenus.Get_Description: WideString;
var
  lKey: String;
  lPath: String;
begin
  lKey := Format(
      'CLSID\%s\InprocServer32',
      [GUIDToString(CLASS_CollectionsMenus)]);
      
  lPath := GetRegStringValue(lKey, '');

  Result := Format(
      ResStr_CollectionsModuleDescription,
      [GetFileVersion(lPath)]);
end;  // TCollectionsMenus.Get_Description

{-------------------------------------------------------------------------------
  Addin image file  
}
function TCollectionsMenus.Get_ImageFileName: WideString;
begin
  Result := 'Collections.bmp';
end;  // TCollectionsMenus.Get_ImageFileName

{-------------------------------------------------------------------------------
  Handle to image list that exposes images for the menu items.
}
function TCollectionsMenus.Get_ImageListHandle: Integer;
begin
  Result := FdmMenuImages.ilImages.Handle;
end;  // TCollectionsMenus.Get_ImageListHandle  

{-------------------------------------------------------------------------------
  Handle to image list that exposes images for the menu items.
}
function TCollectionsMenus.Get_KeytypeCount: Integer;
begin
  Result := FKeyTypes.Count;
end;  // TCollectionsMenus.Get_KeytypeCount

{-------------------------------------------------------------------------------
  Handle to image list that exposes images for the menu items.
}
function TCollectionsMenus.Get_Keytype(Index: Integer): IReportKeytype;
begin
  Result := FKeyTypes[Index] as IReportKeytype;
end;  // TCollectionsMenus.Get_Keytype

{-------------------------------------------------------------------------------
  Menu item to insert the item after, for the item identified by the index 
}
function TCollectionsMenus.Get_InsertAfterMenu(AIndex: Integer): WideString;
begin
  case AIndex of
    MNU_COLLECTIONS_BROWSER: Result := ResStr_MnuDataEntry + '\' + ResStr_MnuObservations;
    MNU_THESAURUS_BROWSER: Result := ResStr_MnuDataEntry + '\' + ResStr_MnuAdminArea;
    MNU_EXTENDED_USER_CONFIG: Result := ResStr_MnuTools + '\' + ResStr_MnuUserConfiguration;
    MNU_REPORTS_SEPARATOR: Result := ResStr_MnuReports + '\' + ResStr_MnuQuickReport;
    MNU_SPECIMEN_FINDER: Result := ResStr_MnuReports + '\-'; // after separator
    MNU_QUICK_ENTRY: Result := ResStr_MnuDataEntry + '\' + ResStr_MnuEnterSpeciesForAPlace;
  end;
end;  // TCollectionsMenus.Get_InsertAfterMenu

{-------------------------------------------------------------------------------
  Not used 
}
function TCollectionsMenus.Get_InsertBeforeMenu(AIndex: Integer): WideString;
begin
  Result := '';
end;  // TCollectionsMenus.Get_InsertBeforeMenu 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_MapPoint(Index: Integer): IMapPoint;
begin
  Result := TCOMMapPoint.Create(TMapPoint(FMapPoints[Index])) as IMapPoint;
end;  // TCollectionsMenus.Get_MapPoint 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_MapPointCount: Integer;
begin
  Result := FMapPoints.Count;
end;  // TCollectionsMenus.Get_MapPointCount 

{-------------------------------------------------------------------------------
  Path to the menu that the item identified by the index is created on. 
}
function TCollectionsMenus.Get_MenuPath(AIndex: Integer): WideString;
begin
  case AIndex of
    MNU_COLLECTIONS_BROWSER: Result := ResStr_MnuDataEntry + '\' +
        ResStr_MnuCollectionsBrowser;
    MNU_THESAURUS_BROWSER: Result := ResStr_MnuDictionaries + '\' + ResStr_ThesaurusBrowser;
    MNU_EXTENDED_USER_CONFIG: Result := ResStr_MnuTools + '\' +
        ResStr_MnuExtendedUserConfiguration;
    MNU_REPORTS_SEPARATOR: Result := ResStr_MnuReports + '\-';
    MNU_SPECIMEN_FINDER: Result := ResStr_MnuReports + '\' + ResStr_MnuSpecimenFinder;
    MNU_QUICK_ENTRY: Result := ResStr_MnuDataEntry + '\' + ResStr_MnuQuickEntry;
  end;
end;  // TCollectionsMenus.Get_MenuPath 

{-------------------------------------------------------------------------------
  Addin name 
}
function TCollectionsMenus.Get_Name: WideString;
begin
  Result := ResStr_CollectionsModuleName;
end;  // TCollectionsMenus.Get_Name 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_OptionsPageCount: Integer;
begin
  Result := 1;
end;  // TCollectionsMenus.Get_OptionsPageCount 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_OptionsPageItems(Index: Integer): IOptionsPage;
begin
  Result := CreateComObject(CLASS_CollectionsOptionsPage) as IOptionsPage;
end;  // TCollectionsMenus.Get_OptionsPageItems 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_SupportedTable(Index: Integer): WideString;
begin
  case Index of
    0: Result := 'Collection';
    1: Result := 'Specimen_Unit';
    2: Result := 'Concept';
  end;
end;  // TCollectionsMenus.Get_SupportedTable 

{-------------------------------------------------------------------------------
}
function TCollectionsMenus.Get_SupportedTableCount: Integer;
begin
  Result := 3;
end;  // TCollectionsMenus.Get_SupportedTableCount 

{-------------------------------------------------------------------------------
}
procedure TCollectionsMenus.InitialiseDataset(const KeyList: IKeyList);
var
  i: Integer;
  lStoredProcName: String;
  lMapPoint: TMapPoint;
  lRS: ADOInt._Recordset;
  
  function GetAccuracy(const ASpatialRef, ASystem: String): Integer;
  var
    lSR: String;
  begin
    // Default to 0 for all.
    Result := 0;
  
    // For UK and Irish spatial references then points have a cut-in and a cut out
    if (CompareText(ASystem, OS_GB) = 0) or (CompareText(ASystem, OS_NI) = 0) then begin
      lSR := UpperCase(StringReplace(ASpatialRef, ' ', '', [rfReplaceAll]));
  
      // Check for a TETRAD
      if ((ASystem = OS_GB) and (Length(lSR) = 5) and (lSR[5] in ['A'..'Z'])) or
         ((ASystem = OS_NI) and (Length(lSR) = 4) and (lSR[4] in ['A'..'Z'])) then
      begin
        Result := 2000;
        Exit;
      end;
  
      // Obviously not a tetrad then.
      if ASystem = OS_NI then
        // Add a supurious 'Z' so that it now has a 2 figure prefix, so its
        // length is equivalent to a UK Spatial Ref of the same accuracy
        lSR := 'Z' + lSR;
  
      // Now work out the accuracy.
      case Length(lSR) of
         2: Result := 100000;
         4: Result :=  10000;
         6: Result :=   1000;
         8: Result :=    100;
        10: Result :=     10;
        12: Result :=      1;
      end;
    end;
  end;
  
begin
  FMapPoints.Clear;
  FInitialisedDatasetTableName := '';
  
  with KeyList do
    if ItemCount > 0 then begin
      if CompareText(TableName, 'Collection') = 0 then
        lStoredProcName := 'usp_Sample_Select_ForCollection'
      else
      if CompareText(TableName, 'Specimen_Unit') = 0 then
        lStoredProcName := 'usp_Sample_Select_ForSpecimen'
      else
      if CompareText(TableName, 'Concept') = 0 then
        lStoredProcName := 'usp_Sample_Select_ForConcept'
      else
        Exit;

      for i := 0 to ItemCount - 1 do begin
        lRS := dmGeneral.GetRecordset(lStoredProcName, ['@Key', GetKeyItem(i).KeyField1]);
        while not lRS.Eof do begin
          if not VarIsNull(lRS.Fields['Lat'].Value) then begin
            lMapPoint := TMapPoint.Create;
            lMapPoint.Lat       := lRS.Fields['Lat'].Value;
            lMapPoint.Long      := lRS.Fields['Long'].Value;
            lMapPoint.RecordKey := lRS.Fields['Sample_Key'].Value;
            if not VarIsNull(lRS.Fields['Vague_Date_Start'].Value) then
              lMapPoint.Date    := lRS.Fields['Vague_Date_Start'].Value
            else
              lMapPoint.Date    := 0;
            lMapPoint.Accuracy  := GetAccuracy(VarToStr(lRS.Fields['Spatial_Ref'].Value),
                                               VarToStr(lRS.Fields['Spatial_Ref_System'].Value));
            lMapPoint.Value     := 0;
            FMapPoints.Add(lMapPoint);
          end;
          lRS.MoveNext;
        end;
        lRS.Close;
      end;
  
      FInitialisedDatasetTableName := 'Sample';
    end;
end;  // TCollectionsMenus.InitialiseDataset 

{-------------------------------------------------------------------------------
  Create the data module that holds the image list. 
}
procedure TCollectionsMenus.Initialize;
var
  lReportKeyType: TReportKeyType;
begin
  inherited;
  FdmMenuImages := TdmMenuImages.Create(nil);
  FMapPoints := TObjectList.Create;

  FKeyTypes := TInterfaceList.Create;
  lReportKeyType := TReportKeyType.Create('Specimen', True);
  FKeyTypes.Add(lReportKeyType as IReportKeyType);
end;  // TCollectionsMenus.Initialize 

{-------------------------------------------------------------------------------
  No action on install  
}
procedure TCollectionsMenus.Install(const iInstalledFilePath: WideString);
begin
  // No action
end;  // TCollectionsMenus.Install 

{-------------------------------------------------------------------------------
  Return the top level menu item for each part of the entire Collections Module system. 
}
function TCollectionsMenus.Items(AIndex: Integer): IDynamicMenu;
begin
  case AIndex of
    MNU_QUICK_ENTRY: Result := GetQuickEntryMenuItem;
    MNU_COLLECTIONS_BROWSER: Result := GetCollectionsBrowserMenuItem;
    MNU_THESAURUS_BROWSER: Result := GetThesaurusBrowserMenuItem;
    MNU_EXTENDED_USER_CONFIG: Result := GetExtendedUserConfigMenuItem;
    MNU_REPORTS_SEPARATOR: Result := GetRptSeparatorMenuItem;
    MNU_SPECIMEN_FINDER: Result := GetSpecimenFinderMenuItem;
  end; // case
end;  // TCollectionsMenus.Items

{-==============================================================================
    TCOMMapPoint
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TCOMMapPoint.Create(AMapPoint: TMapPoint);
begin
  inherited Create;
  FMapPoint := TMapPoint.Create;
  FMapPoint.Assign(AMapPoint);
end;  // TCOMMapPoint.Create 

{-------------------------------------------------------------------------------
}
destructor TCOMMapPoint.Destroy;
begin
  FMapPoint.Free;
  inherited;
end;  // TCOMMapPoint.Destroy 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: 
    integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;  // TCOMMapPoint.GetIDsOfNames 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := 0;
end;  // TCOMMapPoint.GetTypeInfo 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.GetTypeInfoCount(out Count: integer ): HResult;
begin
  Result := 0;
end;  // TCOMMapPoint.GetTypeInfoCount 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_Accuracy: Integer;
begin
  Result := FMapPoint.Accuracy;
end;  // TCOMMapPoint.Get_Accuracy 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_Date: TDateTime;
begin
  Result := FMapPoint.Date;
end;  // TCOMMapPoint.Get_Date 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_Lat: Double;
begin
  Result := FMapPoint.Lat;
end;  // TCOMMapPoint.Get_Lat 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_Long: Double;
begin
  Result := FMapPoint.Long;
end;  // TCOMMapPoint.Get_Long 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_RecordKey: WideString;
begin
  Result := FMapPoint.RecordKey;
end;  // TCOMMapPoint.Get_RecordKey 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Get_Value: Integer;
begin
  Result := FMapPoint.Value;
end;  // TCOMMapPoint.Get_Value 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult;
begin
  Result := 0;
end;  // TCOMMapPoint.Invoke 

{-------------------------------------------------------------------------------
}
function TCOMMapPoint.QueryInterface(const IID:TGuid; out Obj): HResult;
begin
  Result := 0;
end;  // TCOMMapPoint.QueryInterface 

{-==============================================================================
    TMapPoint
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMapPoint.Assign(ASource: TMapPoint);
begin
  FAccuracy  := ASource.Accuracy;
  FDate      := ASource.Date;
  FLat       := ASource.Lat;
  FLong      := ASource.Long;
  FRecordKey := ASource.RecordKey;
  FValue     := ASource.Value;
end;  // TMapPoint.Assign 

{-==============================================================================
    TSingletonCollectionsMenusFactory
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSingletonCollectionsMenusFactory.CreateComObject(const Controller: IUnknown): 
    TComObject;
begin
  if not Assigned(mComObject) then
  begin
    mComObject := inherited CreateComObject(Controller);
    mComObject.ObjAddRef();
  end;
  Result := mComObject;
end;  // TSingletonCollectionsMenusFactory.CreateComObject 
  
{-==============================================================================
    TReportKeyType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TReportKeyType.Create(AName: String; AMultivalue: Boolean);
begin
  FName := AName;
  FMultivalue := AMultivalue;
end;  // TReportKeyType.Create

{-------------------------------------------------------------------------------
}
function TReportKeyType.Get_Name: WideString;
begin
  Result := FName;
end;  // TReportKeyType.Get_Name

{-------------------------------------------------------------------------------
}
function TReportKeyType.Get_Multivalue: WordBool;
begin
  Result := FMultivalue;
end;  // TReportKeyType.Get_Multivalue

initialization
  TSingletonCollectionsMenusFactory.Create(ComServer, TCollectionsMenus,
    Class_CollectionsMenus, ciMultiInstance, tmApartment);

  TComObjectFactory.Create(ComServer, TCOMMapPoint, Class_COMMapPoint,
    'COMMapPoint', '', ciMultiInstance, tmApartment);
end.
