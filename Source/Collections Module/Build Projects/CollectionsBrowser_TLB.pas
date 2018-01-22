unit CollectionsBrowser_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 20/09/2017 11:11:34 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\Collections Module\Build Projects\CollectionsBrowser.tlb (1)
// LIBID: {C86040BA-4519-4B25-B768-27F1DE395958}
// LCID: 0
// Helpfile: 
// HelpString: CollectionsBrowser Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CollectionsBrowserMajorVersion = 1;
  CollectionsBrowserMinorVersion = 0;

  LIBID_CollectionsBrowser: TGUID = '{C86040BA-4519-4B25-B768-27F1DE395958}';

  IID_IfrmCBMain: TGUID = '{3E7358C0-3ECC-4D4F-8307-EEB4D59F0BB6}';
  DIID_IfrmCBMainEvents: TGUID = '{A68AB5A0-597B-410A-A384-C0C7D9706B04}';
  CLASS_frmCBMain: TGUID = '{2D8100A3-F8F0-49F9-A363-9855DAE8A0FE}';
  IID_ICollectionsReportsMenu: TGUID = '{8332039C-CC5A-4A74-9EFE-4F5359575ADF}';
  CLASS_CollectionsReportsMenu: TGUID = '{8AAA1B75-4D51-49E7-9B21-F52457D21A1E}';
  IID_IReportItemsProvider: TGUID = '{6A0689E7-38B6-4B9B-9E2A-11C984B483D6}';
  IID_ICollectionsReportsMenuItem: TGUID = '{3DA1EFC8-8407-4D47-AD12-BE9414691F51}';
  CLASS_CollectionsReportsMenuItem: TGUID = '{468593EC-E022-42F3-B588-DFC0CA2FA86D}';
  IID_ISortableScreen: TGUID = '{82ACB927-7840-4024-9248-77E41BCBD85F}';
  IID_ISortOrderMenu: TGUID = '{B8F52844-E3D1-4393-9CFF-83528C895E13}';
  CLASS_SortOrderMenu: TGUID = '{C62D2385-FC6E-44B1-AA12-406CD8F65CF3}';
  IID_ICopyPasteMenu: TGUID = '{4363B3D8-6108-4D14-8CF4-A713DD80B97A}';
  CLASS_CopyPasteMenu: TGUID = '{DE8DF11E-5830-42D2-A2FB-5BDD69F9F794}';
  IID_ICopyPasteActions: TGUID = '{C40C89CE-00B6-44DD-B3F0-488F5F161A69}';
  IID_IfrmStorageLayout: TGUID = '{D2528890-757D-4992-99D8-F0310E317A37}';
  DIID_IfrmStorageLayoutEvents: TGUID = '{C98F5CB9-DC6D-4240-9F26-334197CE8935}';
  CLASS_frmStorageLayout: TGUID = '{604FC14A-F892-4864-97B7-1CB56D88CBE5}';
  IID_IRefreshTab: TGUID = '{1A6F9910-A6E1-40A3-95F9-51C3CF20EBB4}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxActiveFormBorderStyle
type
  TxActiveFormBorderStyle = TOleEnum;
const
  afbNone = $00000000;
  afbSingle = $00000001;
  afbSunken = $00000002;
  afbRaised = $00000003;

// Constants for enum TxPrintScale
type
  TxPrintScale = TOleEnum;
const
  poNone = $00000000;
  poProportional = $00000001;
  poPrintToFit = $00000002;

// Constants for enum TxMouseButton
type
  TxMouseButton = TOleEnum;
const
  mbLeft = $00000000;
  mbRight = $00000001;
  mbMiddle = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IfrmCBMain = interface;
  IfrmCBMainDisp = dispinterface;
  IfrmCBMainEvents = dispinterface;
  ICollectionsReportsMenu = interface;
  ICollectionsReportsMenuDisp = dispinterface;
  IReportItemsProvider = interface;
  IReportItemsProviderDisp = dispinterface;
  ICollectionsReportsMenuItem = interface;
  ICollectionsReportsMenuItemDisp = dispinterface;
  ISortableScreen = interface;
  ISortableScreenDisp = dispinterface;
  ISortOrderMenu = interface;
  ISortOrderMenuDisp = dispinterface;
  ICopyPasteMenu = interface;
  ICopyPasteMenuDisp = dispinterface;
  ICopyPasteActions = interface;
  ICopyPasteActionsDisp = dispinterface;
  IfrmStorageLayout = interface;
  IfrmStorageLayoutDisp = dispinterface;
  IfrmStorageLayoutEvents = dispinterface;
  IRefreshTab = interface;
  IRefreshTabDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  frmCBMain = IfrmCBMain;
  CollectionsReportsMenu = ICollectionsReportsMenu;
  CollectionsReportsMenuItem = ICollectionsReportsMenuItem;
  SortOrderMenu = ISortOrderMenu;
  CopyPasteMenu = ICopyPasteMenu;
  frmStorageLayout = IfrmStorageLayout;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IfrmCBMain
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3E7358C0-3ECC-4D4F-8307-EEB4D59F0BB6}
// *********************************************************************//
  IfrmCBMain = interface(IDispatch)
    ['{3E7358C0-3ECC-4D4F-8307-EEB4D59F0BB6}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AutoScroll: WordBool; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: OLE_COLOR; safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_KeyPreview: WordBool; safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    function Get_PixelsPerInch: Integer; safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    function Get_Scaled: WordBool; safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    function Get_Active: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    function Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    function Get_ScreenSnap: WordBool; safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    function Get_SnapBuffer: Integer; safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure AboutBox; safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoScroll: WordBool read Get_AutoScroll write Set_AutoScroll;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property AxBorderStyle: TxActiveFormBorderStyle read Get_AxBorderStyle write Set_AxBorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: OLE_COLOR read Get_Color write Set_Color;
    property Font: IFontDisp read Get_Font write Set_Font;
    property KeyPreview: WordBool read Get_KeyPreview write Set_KeyPreview;
    property PixelsPerInch: Integer read Get_PixelsPerInch write Set_PixelsPerInch;
    property PrintScale: TxPrintScale read Get_PrintScale write Set_PrintScale;
    property Scaled: WordBool read Get_Scaled write Set_Scaled;
    property Active: WordBool read Get_Active;
    property DropTarget: WordBool read Get_DropTarget write Set_DropTarget;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property ScreenSnap: WordBool read Get_ScreenSnap write Set_ScreenSnap;
    property SnapBuffer: Integer read Get_SnapBuffer write Set_SnapBuffer;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  IfrmCBMainDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3E7358C0-3ECC-4D4F-8307-EEB4D59F0BB6}
// *********************************************************************//
  IfrmCBMainDisp = dispinterface
    ['{3E7358C0-3ECC-4D4F-8307-EEB4D59F0BB6}']
    property Visible: WordBool dispid 201;
    property AutoScroll: WordBool dispid 202;
    property AutoSize: WordBool dispid 203;
    property AxBorderStyle: TxActiveFormBorderStyle dispid 204;
    property Caption: WideString dispid -518;
    property Color: OLE_COLOR dispid -501;
    property Font: IFontDisp dispid -512;
    property KeyPreview: WordBool dispid 205;
    property PixelsPerInch: Integer dispid 206;
    property PrintScale: TxPrintScale dispid 207;
    property Scaled: WordBool dispid 208;
    property Active: WordBool readonly dispid 209;
    property DropTarget: WordBool dispid 210;
    property HelpFile: WideString dispid 211;
    property ScreenSnap: WordBool dispid 212;
    property SnapBuffer: Integer dispid 213;
    property DoubleBuffered: WordBool dispid 214;
    property AlignDisabled: WordBool readonly dispid 215;
    property VisibleDockClientCount: Integer readonly dispid 216;
    property Enabled: WordBool dispid -514;
    procedure AboutBox; dispid -552;
  end;

// *********************************************************************//
// DispIntf:  IfrmCBMainEvents
// Flags:     (4096) Dispatchable
// GUID:      {A68AB5A0-597B-410A-A384-C0C7D9706B04}
// *********************************************************************//
  IfrmCBMainEvents = dispinterface
    ['{A68AB5A0-597B-410A-A384-C0C7D9706B04}']
    procedure OnActivate; dispid 201;
    procedure OnClick; dispid 202;
    procedure OnCreate; dispid 203;
    procedure OnDblClick; dispid 204;
    procedure OnDestroy; dispid 205;
    procedure OnDeactivate; dispid 206;
    procedure OnKeyPress(var Key: Smallint); dispid 207;
    procedure OnPaint; dispid 208;
  end;

// *********************************************************************//
// Interface: ICollectionsReportsMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8332039C-CC5A-4A74-9EFE-4F5359575ADF}
// *********************************************************************//
  ICollectionsReportsMenu = interface(IDispatch)
    ['{8332039C-CC5A-4A74-9EFE-4F5359575ADF}']
    function Get_ItemsProvider: IReportItemsProvider; safecall;
    procedure Set_ItemsProvider(const Value: IReportItemsProvider); safecall;
    property ItemsProvider: IReportItemsProvider read Get_ItemsProvider write Set_ItemsProvider;
  end;

// *********************************************************************//
// DispIntf:  ICollectionsReportsMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8332039C-CC5A-4A74-9EFE-4F5359575ADF}
// *********************************************************************//
  ICollectionsReportsMenuDisp = dispinterface
    ['{8332039C-CC5A-4A74-9EFE-4F5359575ADF}']
    property ItemsProvider: IReportItemsProvider dispid 201;
  end;

// *********************************************************************//
// Interface: IReportItemsProvider
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6A0689E7-38B6-4B9B-9E2A-11C984B483D6}
// *********************************************************************//
  IReportItemsProvider = interface(IDispatch)
    ['{6A0689E7-38B6-4B9B-9E2A-11C984B483D6}']
    function Get_SelectedItemTable: WideString; safecall;
    function Get_SelectedItemKey: WideString; safecall;
    function Get_TopLevelListCount: Integer; safecall;
    function Get_TopLevelListKey(AIndex: Integer): WideString; safecall;
    function Get_FolderListCount: Integer; safecall;
    function Get_FolderListKey(AIndex: Integer): WideString; safecall;
    function Get_TopLevelListTable: WideString; safecall;
    function Get_FolderListTable: WideString; safecall;
    property SelectedItemTable: WideString read Get_SelectedItemTable;
    property SelectedItemKey: WideString read Get_SelectedItemKey;
    property TopLevelListCount: Integer read Get_TopLevelListCount;
    property TopLevelListKey[AIndex: Integer]: WideString read Get_TopLevelListKey;
    property FolderListCount: Integer read Get_FolderListCount;
    property FolderListKey[AIndex: Integer]: WideString read Get_FolderListKey;
    property TopLevelListTable: WideString read Get_TopLevelListTable;
    property FolderListTable: WideString read Get_FolderListTable;
  end;

// *********************************************************************//
// DispIntf:  IReportItemsProviderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6A0689E7-38B6-4B9B-9E2A-11C984B483D6}
// *********************************************************************//
  IReportItemsProviderDisp = dispinterface
    ['{6A0689E7-38B6-4B9B-9E2A-11C984B483D6}']
    property SelectedItemTable: WideString readonly dispid 201;
    property SelectedItemKey: WideString readonly dispid 202;
    property TopLevelListCount: Integer readonly dispid 203;
    property TopLevelListKey[AIndex: Integer]: WideString readonly dispid 204;
    property FolderListCount: Integer readonly dispid 205;
    property FolderListKey[AIndex: Integer]: WideString readonly dispid 206;
    property TopLevelListTable: WideString readonly dispid 207;
    property FolderListTable: WideString readonly dispid 208;
  end;

// *********************************************************************//
// Interface: ICollectionsReportsMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA1EFC8-8407-4D47-AD12-BE9414691F51}
// *********************************************************************//
  ICollectionsReportsMenuItem = interface(IDispatch)
    ['{3DA1EFC8-8407-4D47-AD12-BE9414691F51}']
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ItemKey: WideString; safecall;
    procedure Set_ItemKey(const Value: WideString); safecall;
    function Get_ItemsProvider: IReportItemsProvider; safecall;
    procedure Set_ItemsProvider(const Value: IReportItemsProvider); safecall;
    function Get_ReportSource: Integer; safecall;
    procedure Set_ReportSource(Value: Integer); safecall;
    function Get_EnquiryStats: WordBool; safecall;
    procedure Set_EnquiryStats(Value: WordBool); safecall;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ItemKey: WideString read Get_ItemKey write Set_ItemKey;
    property ItemsProvider: IReportItemsProvider read Get_ItemsProvider write Set_ItemsProvider;
    property ReportSource: Integer read Get_ReportSource write Set_ReportSource;
    property EnquiryStats: WordBool read Get_EnquiryStats write Set_EnquiryStats;
  end;

// *********************************************************************//
// DispIntf:  ICollectionsReportsMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA1EFC8-8407-4D47-AD12-BE9414691F51}
// *********************************************************************//
  ICollectionsReportsMenuItemDisp = dispinterface
    ['{3DA1EFC8-8407-4D47-AD12-BE9414691F51}']
    property Caption: WideString dispid 201;
    property ItemKey: WideString dispid 202;
    property ItemsProvider: IReportItemsProvider dispid 203;
    property ReportSource: Integer dispid 204;
    property EnquiryStats: WordBool dispid 205;
  end;

// *********************************************************************//
// Interface: ISortableScreen
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82ACB927-7840-4024-9248-77E41BCBD85F}
// *********************************************************************//
  ISortableScreen = interface(IDispatch)
    ['{82ACB927-7840-4024-9248-77E41BCBD85F}']
    function Get_SortProvider: IDispatch; safecall;
    property SortProvider: IDispatch read Get_SortProvider;
  end;

// *********************************************************************//
// DispIntf:  ISortableScreenDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {82ACB927-7840-4024-9248-77E41BCBD85F}
// *********************************************************************//
  ISortableScreenDisp = dispinterface
    ['{82ACB927-7840-4024-9248-77E41BCBD85F}']
    property SortProvider: IDispatch readonly dispid 202;
  end;

// *********************************************************************//
// Interface: ISortOrderMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B8F52844-E3D1-4393-9CFF-83528C895E13}
// *********************************************************************//
  ISortOrderMenu = interface(IDispatch)
    ['{B8F52844-E3D1-4393-9CFF-83528C895E13}']
    function Get_SortableScreen: ISortableScreen; safecall;
    procedure Set_SortableScreen(const Value: ISortableScreen); safecall;
    function Get_Index: Integer; safecall;
    procedure Set_Index(Value: Integer); safecall;
    property SortableScreen: ISortableScreen read Get_SortableScreen write Set_SortableScreen;
    property Index: Integer read Get_Index write Set_Index;
  end;

// *********************************************************************//
// DispIntf:  ISortOrderMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B8F52844-E3D1-4393-9CFF-83528C895E13}
// *********************************************************************//
  ISortOrderMenuDisp = dispinterface
    ['{B8F52844-E3D1-4393-9CFF-83528C895E13}']
    property SortableScreen: ISortableScreen dispid 203;
    property Index: Integer dispid 201;
  end;

// *********************************************************************//
// Interface: ICopyPasteMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4363B3D8-6108-4D14-8CF4-A713DD80B97A}
// *********************************************************************//
  ICopyPasteMenu = interface(IDispatch)
    ['{4363B3D8-6108-4D14-8CF4-A713DD80B97A}']
    function Get_MenuTypeIndex: Integer; safecall;
    procedure Set_MenuTypeIndex(Value: Integer); safecall;
    function Get_CopyPasteActions: ICopyPasteActions; safecall;
    procedure Set_CopyPasteActions(const Value: ICopyPasteActions); safecall;
    property MenuTypeIndex: Integer read Get_MenuTypeIndex write Set_MenuTypeIndex;
    property CopyPasteActions: ICopyPasteActions read Get_CopyPasteActions write Set_CopyPasteActions;
  end;

// *********************************************************************//
// DispIntf:  ICopyPasteMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4363B3D8-6108-4D14-8CF4-A713DD80B97A}
// *********************************************************************//
  ICopyPasteMenuDisp = dispinterface
    ['{4363B3D8-6108-4D14-8CF4-A713DD80B97A}']
    property MenuTypeIndex: Integer dispid 201;
    property CopyPasteActions: ICopyPasteActions dispid 202;
  end;

// *********************************************************************//
// Interface: ICopyPasteActions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C40C89CE-00B6-44DD-B3F0-488F5F161A69}
// *********************************************************************//
  ICopyPasteActions = interface(IDispatch)
    ['{C40C89CE-00B6-44DD-B3F0-488F5F161A69}']
    procedure Copy; safecall;
    procedure Paste; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICopyPasteActionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C40C89CE-00B6-44DD-B3F0-488F5F161A69}
// *********************************************************************//
  ICopyPasteActionsDisp = dispinterface
    ['{C40C89CE-00B6-44DD-B3F0-488F5F161A69}']
    procedure Copy; dispid 201;
    procedure Paste; dispid 202;
  end;

// *********************************************************************//
// Interface: IfrmStorageLayout
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2528890-757D-4992-99D8-F0310E317A37}
// *********************************************************************//
  IfrmStorageLayout = interface(IDispatch)
    ['{D2528890-757D-4992-99D8-F0310E317A37}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AutoScroll: WordBool; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: OLE_COLOR; safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_KeyPreview: WordBool; safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    function Get_PixelsPerInch: Integer; safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    function Get_Scaled: WordBool; safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    function Get_Active: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    function Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    function Get_ScreenSnap: WordBool; safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    function Get_SnapBuffer: Integer; safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_StoreKey: WideString; safecall;
    procedure Set_StoreKey(const Value: WideString); safecall;
    function Get_Caller: IRefreshTab; safecall;
    procedure Set_Caller(const Value: IRefreshTab); safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoScroll: WordBool read Get_AutoScroll write Set_AutoScroll;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property AxBorderStyle: TxActiveFormBorderStyle read Get_AxBorderStyle write Set_AxBorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: OLE_COLOR read Get_Color write Set_Color;
    property Font: IFontDisp read Get_Font write Set_Font;
    property KeyPreview: WordBool read Get_KeyPreview write Set_KeyPreview;
    property PixelsPerInch: Integer read Get_PixelsPerInch write Set_PixelsPerInch;
    property PrintScale: TxPrintScale read Get_PrintScale write Set_PrintScale;
    property Scaled: WordBool read Get_Scaled write Set_Scaled;
    property Active: WordBool read Get_Active;
    property DropTarget: WordBool read Get_DropTarget write Set_DropTarget;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property ScreenSnap: WordBool read Get_ScreenSnap write Set_ScreenSnap;
    property SnapBuffer: Integer read Get_SnapBuffer write Set_SnapBuffer;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property StoreKey: WideString read Get_StoreKey write Set_StoreKey;
    property Caller: IRefreshTab read Get_Caller write Set_Caller;
  end;

// *********************************************************************//
// DispIntf:  IfrmStorageLayoutDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2528890-757D-4992-99D8-F0310E317A37}
// *********************************************************************//
  IfrmStorageLayoutDisp = dispinterface
    ['{D2528890-757D-4992-99D8-F0310E317A37}']
    property Visible: WordBool dispid 201;
    property AutoScroll: WordBool dispid 202;
    property AutoSize: WordBool dispid 203;
    property AxBorderStyle: TxActiveFormBorderStyle dispid 204;
    property Caption: WideString dispid -518;
    property Color: OLE_COLOR dispid -501;
    property Font: IFontDisp dispid -512;
    property KeyPreview: WordBool dispid 205;
    property PixelsPerInch: Integer dispid 206;
    property PrintScale: TxPrintScale dispid 207;
    property Scaled: WordBool dispid 208;
    property Active: WordBool readonly dispid 209;
    property DropTarget: WordBool dispid 210;
    property HelpFile: WideString dispid 211;
    property ScreenSnap: WordBool dispid 212;
    property SnapBuffer: Integer dispid 213;
    property DoubleBuffered: WordBool dispid 214;
    property AlignDisabled: WordBool readonly dispid 215;
    property VisibleDockClientCount: Integer readonly dispid 216;
    property Enabled: WordBool dispid -514;
    property StoreKey: WideString dispid 217;
    property Caller: IRefreshTab dispid 218;
  end;

// *********************************************************************//
// DispIntf:  IfrmStorageLayoutEvents
// Flags:     (4096) Dispatchable
// GUID:      {C98F5CB9-DC6D-4240-9F26-334197CE8935}
// *********************************************************************//
  IfrmStorageLayoutEvents = dispinterface
    ['{C98F5CB9-DC6D-4240-9F26-334197CE8935}']
    procedure OnActivate; dispid 201;
    procedure OnClick; dispid 202;
    procedure OnCreate; dispid 203;
    procedure OnDblClick; dispid 204;
    procedure OnDestroy; dispid 205;
    procedure OnDeactivate; dispid 206;
    procedure OnKeyPress(var Key: Smallint); dispid 207;
    procedure OnPaint; dispid 208;
  end;

// *********************************************************************//
// Interface: IRefreshTab
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A6F9910-A6E1-40A3-95F9-51C3CF20EBB4}
// *********************************************************************//
  IRefreshTab = interface(IDispatch)
    ['{1A6F9910-A6E1-40A3-95F9-51C3CF20EBB4}']
    procedure RefreshTab; safecall;
  end;

// *********************************************************************//
// DispIntf:  IRefreshTabDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A6F9910-A6E1-40A3-95F9-51C3CF20EBB4}
// *********************************************************************//
  IRefreshTabDisp = dispinterface
    ['{1A6F9910-A6E1-40A3-95F9-51C3CF20EBB4}']
    procedure RefreshTab; dispid 201;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TfrmCBMain
// Help String      : frmCBMain Control
// Default Interface: IfrmCBMain
// Def. Intf. DISP? : No
// Event   Interface: IfrmCBMainEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TfrmCBMainOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TfrmCBMain = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TfrmCBMainOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: IfrmCBMain;
    function  GetControlInterface: IfrmCBMain;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure AboutBox;
    property  ControlInterface: IfrmCBMain read GetControlInterface;
    property  DefaultInterface: IfrmCBMain read GetControlInterface;
    property Visible: WordBool index 201 read GetWordBoolProp write SetWordBoolProp;
    property Active: WordBool index 209 read GetWordBoolProp;
    property DropTarget: WordBool index 210 read GetWordBoolProp write SetWordBoolProp;
    property HelpFile: WideString index 211 read GetWideStringProp write SetWideStringProp;
    property ScreenSnap: WordBool index 212 read GetWordBoolProp write SetWordBoolProp;
    property SnapBuffer: Integer index 213 read GetIntegerProp write SetIntegerProp;
    property DoubleBuffered: WordBool index 214 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 215 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 216 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
  published
    property Anchors;
    property  ParentColor;
    property  ParentFont;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property AutoScroll: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 203 read GetWordBoolProp write SetWordBoolProp stored False;
    property AxBorderStyle: TOleEnum index 204 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Color: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property KeyPreview: WordBool index 205 read GetWordBoolProp write SetWordBoolProp stored False;
    property PixelsPerInch: Integer index 206 read GetIntegerProp write SetIntegerProp stored False;
    property PrintScale: TOleEnum index 207 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Scaled: WordBool index 208 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TfrmCBMainOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

// *********************************************************************//
// The Class CoCollectionsReportsMenu provides a Create and CreateRemote method to          
// create instances of the default interface ICollectionsReportsMenu exposed by              
// the CoClass CollectionsReportsMenu. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCollectionsReportsMenu = class
    class function Create: ICollectionsReportsMenu;
    class function CreateRemote(const MachineName: string): ICollectionsReportsMenu;
  end;

// *********************************************************************//
// The Class CoCollectionsReportsMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface ICollectionsReportsMenuItem exposed by              
// the CoClass CollectionsReportsMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCollectionsReportsMenuItem = class
    class function Create: ICollectionsReportsMenuItem;
    class function CreateRemote(const MachineName: string): ICollectionsReportsMenuItem;
  end;

// *********************************************************************//
// The Class CoSortOrderMenu provides a Create and CreateRemote method to          
// create instances of the default interface ISortOrderMenu exposed by              
// the CoClass SortOrderMenu. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSortOrderMenu = class
    class function Create: ISortOrderMenu;
    class function CreateRemote(const MachineName: string): ISortOrderMenu;
  end;

// *********************************************************************//
// The Class CoCopyPasteMenu provides a Create and CreateRemote method to          
// create instances of the default interface ICopyPasteMenu exposed by              
// the CoClass CopyPasteMenu. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCopyPasteMenu = class
    class function Create: ICopyPasteMenu;
    class function CreateRemote(const MachineName: string): ICopyPasteMenu;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TfrmStorageLayout
// Help String      : frmStorageLayout Control
// Default Interface: IfrmStorageLayout
// Def. Intf. DISP? : No
// Event   Interface: IfrmStorageLayoutEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TfrmStorageLayoutOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TfrmStorageLayout = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TfrmStorageLayoutOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: IfrmStorageLayout;
    function  GetControlInterface: IfrmStorageLayout;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Caller: IRefreshTab;
    procedure Set_Caller(const Value: IRefreshTab);
  public
    property  ControlInterface: IfrmStorageLayout read GetControlInterface;
    property  DefaultInterface: IfrmStorageLayout read GetControlInterface;
    property Visible: WordBool index 201 read GetWordBoolProp write SetWordBoolProp;
    property Active: WordBool index 209 read GetWordBoolProp;
    property DropTarget: WordBool index 210 read GetWordBoolProp write SetWordBoolProp;
    property HelpFile: WideString index 211 read GetWideStringProp write SetWideStringProp;
    property ScreenSnap: WordBool index 212 read GetWordBoolProp write SetWordBoolProp;
    property SnapBuffer: Integer index 213 read GetIntegerProp write SetIntegerProp;
    property DoubleBuffered: WordBool index 214 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 215 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 216 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
  published
    property Anchors;
    property  ParentColor;
    property  ParentFont;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property AutoScroll: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 203 read GetWordBoolProp write SetWordBoolProp stored False;
    property AxBorderStyle: TOleEnum index 204 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Color: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property KeyPreview: WordBool index 205 read GetWordBoolProp write SetWordBoolProp stored False;
    property PixelsPerInch: Integer index 206 read GetIntegerProp write SetIntegerProp stored False;
    property PrintScale: TOleEnum index 207 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Scaled: WordBool index 208 read GetWordBoolProp write SetWordBoolProp stored False;
    property StoreKey: WideString index 217 read GetWideStringProp write SetWideStringProp stored False;
    property Caller: IRefreshTab read Get_Caller write Set_Caller stored False;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TfrmStorageLayoutOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TfrmCBMain.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{2D8100A3-F8F0-49F9-A363-9855DAE8A0FE}';
    EventIID: '{A68AB5A0-597B-410A-A384-C0C7D9706B04}';
    EventCount: 8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $0000001D;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivate) - Cardinal(Self);
end;

procedure TfrmCBMain.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IfrmCBMain;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TfrmCBMain.GetControlInterface: IfrmCBMain;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TfrmCBMain.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

class function CoCollectionsReportsMenu.Create: ICollectionsReportsMenu;
begin
  Result := CreateComObject(CLASS_CollectionsReportsMenu) as ICollectionsReportsMenu;
end;

class function CoCollectionsReportsMenu.CreateRemote(const MachineName: string): ICollectionsReportsMenu;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CollectionsReportsMenu) as ICollectionsReportsMenu;
end;

class function CoCollectionsReportsMenuItem.Create: ICollectionsReportsMenuItem;
begin
  Result := CreateComObject(CLASS_CollectionsReportsMenuItem) as ICollectionsReportsMenuItem;
end;

class function CoCollectionsReportsMenuItem.CreateRemote(const MachineName: string): ICollectionsReportsMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CollectionsReportsMenuItem) as ICollectionsReportsMenuItem;
end;

class function CoSortOrderMenu.Create: ISortOrderMenu;
begin
  Result := CreateComObject(CLASS_SortOrderMenu) as ISortOrderMenu;
end;

class function CoSortOrderMenu.CreateRemote(const MachineName: string): ISortOrderMenu;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SortOrderMenu) as ISortOrderMenu;
end;

class function CoCopyPasteMenu.Create: ICopyPasteMenu;
begin
  Result := CreateComObject(CLASS_CopyPasteMenu) as ICopyPasteMenu;
end;

class function CoCopyPasteMenu.CreateRemote(const MachineName: string): ICopyPasteMenu;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CopyPasteMenu) as ICopyPasteMenu;
end;

procedure TfrmStorageLayout.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{604FC14A-F892-4864-97B7-1CB56D88CBE5}';
    EventIID: '{C98F5CB9-DC6D-4240-9F26-334197CE8935}';
    EventCount: 8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $0000001D;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivate) - Cardinal(Self);
end;

procedure TfrmStorageLayout.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IfrmStorageLayout;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TfrmStorageLayout.GetControlInterface: IfrmStorageLayout;
begin
  CreateControl;
  Result := FIntf;
end;

function TfrmStorageLayout.Get_Caller: IRefreshTab;
begin
    Result := DefaultInterface.Caller;
end;

procedure TfrmStorageLayout.Set_Caller(const Value: IRefreshTab);
begin
  DefaultInterface.Set_Caller(Value);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TfrmCBMain, TfrmStorageLayout]);
end;

end.
