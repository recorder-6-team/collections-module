unit CollectionsModuleManager_TLB;

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
// Type Lib: F:\Collections Module\Build Projects\CollectionsModuleManager.tlb (1)
// LIBID: {FE7BB20B-B2B9-4266-A750-976226A1CFAC}
// LCID: 0
// Helpfile: 
// HelpString: CollectionsModuleManager Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
//   (2) v2.1 ADODB, (C:\Program Files\Common Files\System\ado\msado21.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, ADODB_TLB, Classes, Graphics, OleCtrls, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CollectionsModuleManagerMajorVersion = 1;
  CollectionsModuleManagerMinorVersion = 0;

  LIBID_CollectionsModuleManager: TGUID = '{FE7BB20B-B2B9-4266-A750-976226A1CFAC}';

  IID_ICollectionsMenus: TGUID = '{8E0DE05E-6F14-457D-9499-AA666D5A0262}';
  CLASS_CollectionsMenus: TGUID = '{3739181F-25A2-4605-8835-80C423BABA71}';
  IID_IQuickEntryMenuItem: TGUID = '{ED868649-7DBE-4D9B-9100-57E6EC94F93D}';
  CLASS_QuickEntryMenuItem: TGUID = '{9F12E9A8-79C1-4A9F-91BB-754CBE5DF271}';
  IID_ISpecimenFinderMenuItem: TGUID = '{DDF75DAA-AD27-48F1-A4C0-686BB898B01B}';
  CLASS_SpecimenFinderMenuItem: TGUID = '{6204C039-0A49-4419-9727-0C8F354EB92D}';
  IID_IThesaurusBrowserMenuItem: TGUID = '{FBFD4BE8-074C-4B2F-8A87-E5AEAEC3F9C9}';
  CLASS_ThesaurusBrowserMenuItem: TGUID = '{9712CD48-57A2-4BDE-BC49-60106E61F6EC}';
  IID_ICollectionsBrowserMenuItem: TGUID = '{30CF5CDB-A3FC-4D7A-82BF-6C7BA07C90CB}';
  CLASS_CollectionsBrowserMenuItem: TGUID = '{8D9B561D-23A7-4942-90BA-8BCDDDB49597}';
  IID_IExtendedUserConfigMenuItem: TGUID = '{41C7B28B-5FE2-4AF1-844B-97DE77E6089D}';
  CLASS_ExtendedUserConfigMenuItem: TGUID = '{F88AEBCE-01F6-4790-BB7A-8DE3829EFA12}';
  IID_IReportSeparator: TGUID = '{488B130B-3AC6-4DC7-8065-C78B56922AA2}';
  CLASS_ReportSeparator: TGUID = '{0E549FA5-C704-43C6-B002-650F5134F251}';
  IID_IOptionsPageManager: TGUID = '{3548FC45-A5DE-4F5A-9469-1ED29C345CDF}';
  CLASS_OptionsPageManager: TGUID = '{A23BBEDD-155C-4814-9DAF-4B880EF2298F}';
  IID_ICollectionsOptionsPage: TGUID = '{4C6D20C2-548D-429A-BAC7-482E70F1AC43}';
  DIID_ICollectionsOptionsPageEvents: TGUID = '{E9C57BC1-DBA7-4DD7-960A-B10256C49A3A}';
  CLASS_CollectionsOptionsPage: TGUID = '{BF10D52A-EFFA-4EAC-B567-D7A670032AFF}';
  IID_ICollectionsModuleSettings: TGUID = '{1ACA0945-92DB-4CEF-9209-E7D03E2D5163}';
  CLASS_CollectionsModuleSettings: TGUID = '{8ADF66A6-C00E-44C1-AF2D-058E7534B360}';

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
  ICollectionsMenus = interface;
  ICollectionsMenusDisp = dispinterface;
  IQuickEntryMenuItem = interface;
  IQuickEntryMenuItemDisp = dispinterface;
  ISpecimenFinderMenuItem = interface;
  ISpecimenFinderMenuItemDisp = dispinterface;
  IThesaurusBrowserMenuItem = interface;
  IThesaurusBrowserMenuItemDisp = dispinterface;
  ICollectionsBrowserMenuItem = interface;
  ICollectionsBrowserMenuItemDisp = dispinterface;
  IExtendedUserConfigMenuItem = interface;
  IExtendedUserConfigMenuItemDisp = dispinterface;
  IReportSeparator = interface;
  IReportSeparatorDisp = dispinterface;
  IOptionsPageManager = interface;
  IOptionsPageManagerDisp = dispinterface;
  ICollectionsOptionsPage = interface;
  ICollectionsOptionsPageDisp = dispinterface;
  ICollectionsOptionsPageEvents = dispinterface;
  ICollectionsModuleSettings = interface;
  ICollectionsModuleSettingsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CollectionsMenus = ICollectionsMenus;
  QuickEntryMenuItem = IQuickEntryMenuItem;
  SpecimenFinderMenuItem = ISpecimenFinderMenuItem;
  ThesaurusBrowserMenuItem = IThesaurusBrowserMenuItem;
  CollectionsBrowserMenuItem = ICollectionsBrowserMenuItem;
  ExtendedUserConfigMenuItem = IExtendedUserConfigMenuItem;
  ReportSeparator = IReportSeparator;
  OptionsPageManager = IOptionsPageManager;
  CollectionsOptionsPage = ICollectionsOptionsPage;
  CollectionsModuleSettings = ICollectionsModuleSettings;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: ICollectionsMenus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E0DE05E-6F14-457D-9499-AA666D5A0262}
// *********************************************************************//
  ICollectionsMenus = interface(IDispatch)
    ['{8E0DE05E-6F14-457D-9499-AA666D5A0262}']
  end;

// *********************************************************************//
// DispIntf:  ICollectionsMenusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E0DE05E-6F14-457D-9499-AA666D5A0262}
// *********************************************************************//
  ICollectionsMenusDisp = dispinterface
    ['{8E0DE05E-6F14-457D-9499-AA666D5A0262}']
  end;

// *********************************************************************//
// Interface: IQuickEntryMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED868649-7DBE-4D9B-9100-57E6EC94F93D}
// *********************************************************************//
  IQuickEntryMenuItem = interface(IDispatch)
    ['{ED868649-7DBE-4D9B-9100-57E6EC94F93D}']
  end;

// *********************************************************************//
// DispIntf:  IQuickEntryMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED868649-7DBE-4D9B-9100-57E6EC94F93D}
// *********************************************************************//
  IQuickEntryMenuItemDisp = dispinterface
    ['{ED868649-7DBE-4D9B-9100-57E6EC94F93D}']
  end;

// *********************************************************************//
// Interface: ISpecimenFinderMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDF75DAA-AD27-48F1-A4C0-686BB898B01B}
// *********************************************************************//
  ISpecimenFinderMenuItem = interface(IDispatch)
    ['{DDF75DAA-AD27-48F1-A4C0-686BB898B01B}']
  end;

// *********************************************************************//
// DispIntf:  ISpecimenFinderMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDF75DAA-AD27-48F1-A4C0-686BB898B01B}
// *********************************************************************//
  ISpecimenFinderMenuItemDisp = dispinterface
    ['{DDF75DAA-AD27-48F1-A4C0-686BB898B01B}']
  end;

// *********************************************************************//
// Interface: IThesaurusBrowserMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FBFD4BE8-074C-4B2F-8A87-E5AEAEC3F9C9}
// *********************************************************************//
  IThesaurusBrowserMenuItem = interface(IDispatch)
    ['{FBFD4BE8-074C-4B2F-8A87-E5AEAEC3F9C9}']
  end;

// *********************************************************************//
// DispIntf:  IThesaurusBrowserMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FBFD4BE8-074C-4B2F-8A87-E5AEAEC3F9C9}
// *********************************************************************//
  IThesaurusBrowserMenuItemDisp = dispinterface
    ['{FBFD4BE8-074C-4B2F-8A87-E5AEAEC3F9C9}']
  end;

// *********************************************************************//
// Interface: ICollectionsBrowserMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {30CF5CDB-A3FC-4D7A-82BF-6C7BA07C90CB}
// *********************************************************************//
  ICollectionsBrowserMenuItem = interface(IDispatch)
    ['{30CF5CDB-A3FC-4D7A-82BF-6C7BA07C90CB}']
  end;

// *********************************************************************//
// DispIntf:  ICollectionsBrowserMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {30CF5CDB-A3FC-4D7A-82BF-6C7BA07C90CB}
// *********************************************************************//
  ICollectionsBrowserMenuItemDisp = dispinterface
    ['{30CF5CDB-A3FC-4D7A-82BF-6C7BA07C90CB}']
  end;

// *********************************************************************//
// Interface: IExtendedUserConfigMenuItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41C7B28B-5FE2-4AF1-844B-97DE77E6089D}
// *********************************************************************//
  IExtendedUserConfigMenuItem = interface(IDispatch)
    ['{41C7B28B-5FE2-4AF1-844B-97DE77E6089D}']
  end;

// *********************************************************************//
// DispIntf:  IExtendedUserConfigMenuItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41C7B28B-5FE2-4AF1-844B-97DE77E6089D}
// *********************************************************************//
  IExtendedUserConfigMenuItemDisp = dispinterface
    ['{41C7B28B-5FE2-4AF1-844B-97DE77E6089D}']
  end;

// *********************************************************************//
// Interface: IReportSeparator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {488B130B-3AC6-4DC7-8065-C78B56922AA2}
// *********************************************************************//
  IReportSeparator = interface(IDispatch)
    ['{488B130B-3AC6-4DC7-8065-C78B56922AA2}']
  end;

// *********************************************************************//
// DispIntf:  IReportSeparatorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {488B130B-3AC6-4DC7-8065-C78B56922AA2}
// *********************************************************************//
  IReportSeparatorDisp = dispinterface
    ['{488B130B-3AC6-4DC7-8065-C78B56922AA2}']
  end;

// *********************************************************************//
// Interface: IOptionsPageManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3548FC45-A5DE-4F5A-9469-1ED29C345CDF}
// *********************************************************************//
  IOptionsPageManager = interface(IDispatch)
    ['{3548FC45-A5DE-4F5A-9469-1ED29C345CDF}']
  end;

// *********************************************************************//
// DispIntf:  IOptionsPageManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3548FC45-A5DE-4F5A-9469-1ED29C345CDF}
// *********************************************************************//
  IOptionsPageManagerDisp = dispinterface
    ['{3548FC45-A5DE-4F5A-9469-1ED29C345CDF}']
  end;

// *********************************************************************//
// Interface: ICollectionsOptionsPage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C6D20C2-548D-429A-BAC7-482E70F1AC43}
// *********************************************************************//
  ICollectionsOptionsPage = interface(IDispatch)
    ['{4C6D20C2-548D-429A-BAC7-482E70F1AC43}']
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
// DispIntf:  ICollectionsOptionsPageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4C6D20C2-548D-429A-BAC7-482E70F1AC43}
// *********************************************************************//
  ICollectionsOptionsPageDisp = dispinterface
    ['{4C6D20C2-548D-429A-BAC7-482E70F1AC43}']
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
  end;

// *********************************************************************//
// DispIntf:  ICollectionsOptionsPageEvents
// Flags:     (4096) Dispatchable
// GUID:      {E9C57BC1-DBA7-4DD7-960A-B10256C49A3A}
// *********************************************************************//
  ICollectionsOptionsPageEvents = dispinterface
    ['{E9C57BC1-DBA7-4DD7-960A-B10256C49A3A}']
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
// Interface: ICollectionsModuleSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1ACA0945-92DB-4CEF-9209-E7D03E2D5163}
// *********************************************************************//
  ICollectionsModuleSettings = interface(IDispatch)
    ['{1ACA0945-92DB-4CEF-9209-E7D03E2D5163}']
    function Get_AllowFinance: WordBool; safecall;
    function Get_AllowQuickEntry: WordBool; safecall;
    function Get_AllowQuickEntryProcessing: WordBool; safecall;
    function Get_DictImagesPath: WideString; safecall;
    function Get_DisableDragDropFrames: WordBool; safecall;
    function Get_DisplayCommonNames: WordBool; safecall;
    function Get_DragDestinationColour: Integer; safecall;
    function Get_DragSourceColour: Integer; safecall;
    function Get_IncludeHierarchySynonyms: WordBool; safecall;
    procedure Set_IncludeHierarchySynonyms(Value: WordBool); safecall;
    function Get_InstallationPath: WideString; safecall;
    function Get_ISOLanguage: WideString; safecall;
    function Get_ISOCurrency: WideString; safecall;
    function Get_LocalImagesPath: WideString; safecall;
    function Get_MandatoryColour: Integer; safecall;
    function Get_SessionID: WideString; safecall;
    procedure AddItemToPersistentList(const AListName: WideString; const AItem: WideString; 
                                      AMaxLength: Integer); safecall;
    function GetItemsFromPersistentList(const AListName: WideString): WideString; safecall;
    function Get_UserID: WideString; safecall;
    procedure Set_UserID(const Value: WideString); safecall;
    function Get_Connection: Connection; safecall;
    function Get_QuickEntryShowAsForm: WordBool; safecall;
    procedure Set_QuickEntryShowAsForm(Value: WordBool); safecall;
    function Get_LastThesaurusConceptGroup: WideString; safecall;
    procedure Set_LastThesaurusConceptGroup(const Value: WideString); safecall;
    function Get_AllowMovementEdit: WordBool; safecall;
    function Get_AllowAdd(ADomainMask: Integer): WordBool; safecall;
    function Get_AllowEdit(ADomainMask: Integer): WordBool; safecall;
    function Get_UserAccessLevel: Smallint; safecall;
    function Get_DomainMask: Integer; safecall;
    procedure Set_DomainMask(Value: Integer); safecall;
    function Get_AddDomainMask: Integer; safecall;
    function Get_EditDomainMask: Integer; safecall;
    procedure Refresh; safecall;
    function Get_DefaultViewType: WideString; safecall;
    procedure Set_DefaultViewType(const Value: WideString); safecall;
    function Get_QuickEntryImportTypeIndex: Integer; safecall;
    procedure Set_QuickEntryImportTypeIndex(Value: Integer); safecall;
    function Get_SpecimenFinderSortOrderIndex: Integer; safecall;
    procedure Set_SpecimenFinderSortOrderIndex(Value: Integer); safecall;
    function Get_SpecimenImagePath: WideString; safecall;
    procedure Set_SpecimenImagePath(const Value: WideString); safecall;
    function Get_StandardReportTemplatePath: WideString; safecall;
    procedure Set_StandardReportTemplatePath(const Value: WideString); safecall;
    function Get_AddInPath: WideString; safecall;
    function Get_PreferredSynonymsOnly: WordBool; safecall;
    procedure Set_PreferredSynonymsOnly(Value: WordBool); safecall;
    function Get_GroupDeterminationsByDomain: WordBool; safecall;
    procedure Set_GroupDeterminationsByDomain(Value: WordBool); safecall;
    function Get_UseOriginalSpecimenNames: WordBool; safecall;
    procedure Set_UseOriginalSpecimenNames(Value: WordBool); safecall;
    function Get_ShowGroupInQuickEntry: WordBool; safecall;
    procedure Set_ShowGroupInQuickEntry(Value: WordBool); safecall;
    function Get_ShowRecorderSpecimensTab: WordBool; safecall;
    procedure Set_ShowRecorderSpecimensTab(Value: WordBool); safecall;
    property AllowFinance: WordBool read Get_AllowFinance;
    property AllowQuickEntry: WordBool read Get_AllowQuickEntry;
    property AllowQuickEntryProcessing: WordBool read Get_AllowQuickEntryProcessing;
    property DictImagesPath: WideString read Get_DictImagesPath;
    property DisableDragDropFrames: WordBool read Get_DisableDragDropFrames;
    property DisplayCommonNames: WordBool read Get_DisplayCommonNames;
    property DragDestinationColour: Integer read Get_DragDestinationColour;
    property DragSourceColour: Integer read Get_DragSourceColour;
    property IncludeHierarchySynonyms: WordBool read Get_IncludeHierarchySynonyms write Set_IncludeHierarchySynonyms;
    property InstallationPath: WideString read Get_InstallationPath;
    property ISOLanguage: WideString read Get_ISOLanguage;
    property ISOCurrency: WideString read Get_ISOCurrency;
    property LocalImagesPath: WideString read Get_LocalImagesPath;
    property MandatoryColour: Integer read Get_MandatoryColour;
    property SessionID: WideString read Get_SessionID;
    property UserID: WideString read Get_UserID write Set_UserID;
    property Connection: Connection read Get_Connection;
    property QuickEntryShowAsForm: WordBool read Get_QuickEntryShowAsForm write Set_QuickEntryShowAsForm;
    property LastThesaurusConceptGroup: WideString read Get_LastThesaurusConceptGroup write Set_LastThesaurusConceptGroup;
    property AllowMovementEdit: WordBool read Get_AllowMovementEdit;
    property AllowAdd[ADomainMask: Integer]: WordBool read Get_AllowAdd;
    property AllowEdit[ADomainMask: Integer]: WordBool read Get_AllowEdit;
    property UserAccessLevel: Smallint read Get_UserAccessLevel;
    property DomainMask: Integer read Get_DomainMask write Set_DomainMask;
    property AddDomainMask: Integer read Get_AddDomainMask;
    property EditDomainMask: Integer read Get_EditDomainMask;
    property DefaultViewType: WideString read Get_DefaultViewType write Set_DefaultViewType;
    property QuickEntryImportTypeIndex: Integer read Get_QuickEntryImportTypeIndex write Set_QuickEntryImportTypeIndex;
    property SpecimenFinderSortOrderIndex: Integer read Get_SpecimenFinderSortOrderIndex write Set_SpecimenFinderSortOrderIndex;
    property SpecimenImagePath: WideString read Get_SpecimenImagePath write Set_SpecimenImagePath;
    property StandardReportTemplatePath: WideString read Get_StandardReportTemplatePath write Set_StandardReportTemplatePath;
    property AddInPath: WideString read Get_AddInPath;
    property PreferredSynonymsOnly: WordBool read Get_PreferredSynonymsOnly write Set_PreferredSynonymsOnly;
    property GroupDeterminationsByDomain: WordBool read Get_GroupDeterminationsByDomain write Set_GroupDeterminationsByDomain;
    property UseOriginalSpecimenNames: WordBool read Get_UseOriginalSpecimenNames write Set_UseOriginalSpecimenNames;
    property ShowGroupInQuickEntry: WordBool read Get_ShowGroupInQuickEntry write Set_ShowGroupInQuickEntry;
    property ShowRecorderSpecimensTab: WordBool read Get_ShowRecorderSpecimensTab write Set_ShowRecorderSpecimensTab;
  end;

// *********************************************************************//
// DispIntf:  ICollectionsModuleSettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1ACA0945-92DB-4CEF-9209-E7D03E2D5163}
// *********************************************************************//
  ICollectionsModuleSettingsDisp = dispinterface
    ['{1ACA0945-92DB-4CEF-9209-E7D03E2D5163}']
    property AllowFinance: WordBool readonly dispid 201;
    property AllowQuickEntry: WordBool readonly dispid 202;
    property AllowQuickEntryProcessing: WordBool readonly dispid 203;
    property DictImagesPath: WideString readonly dispid 204;
    property DisableDragDropFrames: WordBool readonly dispid 205;
    property DisplayCommonNames: WordBool readonly dispid 206;
    property DragDestinationColour: Integer readonly dispid 208;
    property DragSourceColour: Integer readonly dispid 209;
    property IncludeHierarchySynonyms: WordBool dispid 210;
    property InstallationPath: WideString readonly dispid 211;
    property ISOLanguage: WideString readonly dispid 212;
    property ISOCurrency: WideString readonly dispid 213;
    property LocalImagesPath: WideString readonly dispid 214;
    property MandatoryColour: Integer readonly dispid 215;
    property SessionID: WideString readonly dispid 217;
    procedure AddItemToPersistentList(const AListName: WideString; const AItem: WideString; 
                                      AMaxLength: Integer); dispid 219;
    function GetItemsFromPersistentList(const AListName: WideString): WideString; dispid 220;
    property UserID: WideString dispid 218;
    property Connection: Connection readonly dispid 222;
    property QuickEntryShowAsForm: WordBool dispid 216;
    property LastThesaurusConceptGroup: WideString dispid 223;
    property AllowMovementEdit: WordBool readonly dispid 224;
    property AllowAdd[ADomainMask: Integer]: WordBool readonly dispid 225;
    property AllowEdit[ADomainMask: Integer]: WordBool readonly dispid 226;
    property UserAccessLevel: Smallint readonly dispid 228;
    property DomainMask: Integer dispid 207;
    property AddDomainMask: Integer readonly dispid 227;
    property EditDomainMask: Integer readonly dispid 229;
    procedure Refresh; dispid 230;
    property DefaultViewType: WideString dispid 231;
    property QuickEntryImportTypeIndex: Integer dispid 232;
    property SpecimenFinderSortOrderIndex: Integer dispid 233;
    property SpecimenImagePath: WideString dispid 234;
    property StandardReportTemplatePath: WideString dispid 221;
    property AddInPath: WideString readonly dispid 235;
    property PreferredSynonymsOnly: WordBool dispid 236;
    property GroupDeterminationsByDomain: WordBool dispid 237;
    property UseOriginalSpecimenNames: WordBool dispid 238;
    property ShowGroupInQuickEntry: WordBool dispid 239;
    property ShowRecorderSpecimensTab: WordBool dispid 240;
  end;

// *********************************************************************//
// The Class CoCollectionsMenus provides a Create and CreateRemote method to          
// create instances of the default interface ICollectionsMenus exposed by              
// the CoClass CollectionsMenus. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCollectionsMenus = class
    class function Create: ICollectionsMenus;
    class function CreateRemote(const MachineName: string): ICollectionsMenus;
  end;

// *********************************************************************//
// The Class CoQuickEntryMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface IQuickEntryMenuItem exposed by              
// the CoClass QuickEntryMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoQuickEntryMenuItem = class
    class function Create: IQuickEntryMenuItem;
    class function CreateRemote(const MachineName: string): IQuickEntryMenuItem;
  end;

// *********************************************************************//
// The Class CoSpecimenFinderMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface ISpecimenFinderMenuItem exposed by              
// the CoClass SpecimenFinderMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpecimenFinderMenuItem = class
    class function Create: ISpecimenFinderMenuItem;
    class function CreateRemote(const MachineName: string): ISpecimenFinderMenuItem;
  end;

// *********************************************************************//
// The Class CoThesaurusBrowserMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface IThesaurusBrowserMenuItem exposed by              
// the CoClass ThesaurusBrowserMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoThesaurusBrowserMenuItem = class
    class function Create: IThesaurusBrowserMenuItem;
    class function CreateRemote(const MachineName: string): IThesaurusBrowserMenuItem;
  end;

// *********************************************************************//
// The Class CoCollectionsBrowserMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface ICollectionsBrowserMenuItem exposed by              
// the CoClass CollectionsBrowserMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCollectionsBrowserMenuItem = class
    class function Create: ICollectionsBrowserMenuItem;
    class function CreateRemote(const MachineName: string): ICollectionsBrowserMenuItem;
  end;

// *********************************************************************//
// The Class CoExtendedUserConfigMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface IExtendedUserConfigMenuItem exposed by              
// the CoClass ExtendedUserConfigMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtendedUserConfigMenuItem = class
    class function Create: IExtendedUserConfigMenuItem;
    class function CreateRemote(const MachineName: string): IExtendedUserConfigMenuItem;
  end;

// *********************************************************************//
// The Class CoReportSeparator provides a Create and CreateRemote method to          
// create instances of the default interface IReportSeparator exposed by              
// the CoClass ReportSeparator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReportSeparator = class
    class function Create: IReportSeparator;
    class function CreateRemote(const MachineName: string): IReportSeparator;
  end;

// *********************************************************************//
// The Class CoOptionsPageManager provides a Create and CreateRemote method to          
// create instances of the default interface IOptionsPageManager exposed by              
// the CoClass OptionsPageManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoOptionsPageManager = class
    class function Create: IOptionsPageManager;
    class function CreateRemote(const MachineName: string): IOptionsPageManager;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TCollectionsOptionsPage
// Help String      : CollectionsOptionsPage Control
// Default Interface: ICollectionsOptionsPage
// Def. Intf. DISP? : No
// Event   Interface: ICollectionsOptionsPageEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TCollectionsOptionsPageOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TCollectionsOptionsPage = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TCollectionsOptionsPageOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: ICollectionsOptionsPage;
    function  GetControlInterface: ICollectionsOptionsPage;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    property  ControlInterface: ICollectionsOptionsPage read GetControlInterface;
    property  DefaultInterface: ICollectionsOptionsPage read GetControlInterface;
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
    property OnKeyPress: TCollectionsOptionsPageOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

// *********************************************************************//
// The Class CoCollectionsModuleSettings provides a Create and CreateRemote method to          
// create instances of the default interface ICollectionsModuleSettings exposed by              
// the CoClass CollectionsModuleSettings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCollectionsModuleSettings = class
    class function Create: ICollectionsModuleSettings;
    class function CreateRemote(const MachineName: string): ICollectionsModuleSettings;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoCollectionsMenus.Create: ICollectionsMenus;
begin
  Result := CreateComObject(CLASS_CollectionsMenus) as ICollectionsMenus;
end;

class function CoCollectionsMenus.CreateRemote(const MachineName: string): ICollectionsMenus;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CollectionsMenus) as ICollectionsMenus;
end;

class function CoQuickEntryMenuItem.Create: IQuickEntryMenuItem;
begin
  Result := CreateComObject(CLASS_QuickEntryMenuItem) as IQuickEntryMenuItem;
end;

class function CoQuickEntryMenuItem.CreateRemote(const MachineName: string): IQuickEntryMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_QuickEntryMenuItem) as IQuickEntryMenuItem;
end;

class function CoSpecimenFinderMenuItem.Create: ISpecimenFinderMenuItem;
begin
  Result := CreateComObject(CLASS_SpecimenFinderMenuItem) as ISpecimenFinderMenuItem;
end;

class function CoSpecimenFinderMenuItem.CreateRemote(const MachineName: string): ISpecimenFinderMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpecimenFinderMenuItem) as ISpecimenFinderMenuItem;
end;

class function CoThesaurusBrowserMenuItem.Create: IThesaurusBrowserMenuItem;
begin
  Result := CreateComObject(CLASS_ThesaurusBrowserMenuItem) as IThesaurusBrowserMenuItem;
end;

class function CoThesaurusBrowserMenuItem.CreateRemote(const MachineName: string): IThesaurusBrowserMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ThesaurusBrowserMenuItem) as IThesaurusBrowserMenuItem;
end;

class function CoCollectionsBrowserMenuItem.Create: ICollectionsBrowserMenuItem;
begin
  Result := CreateComObject(CLASS_CollectionsBrowserMenuItem) as ICollectionsBrowserMenuItem;
end;

class function CoCollectionsBrowserMenuItem.CreateRemote(const MachineName: string): ICollectionsBrowserMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CollectionsBrowserMenuItem) as ICollectionsBrowserMenuItem;
end;

class function CoExtendedUserConfigMenuItem.Create: IExtendedUserConfigMenuItem;
begin
  Result := CreateComObject(CLASS_ExtendedUserConfigMenuItem) as IExtendedUserConfigMenuItem;
end;

class function CoExtendedUserConfigMenuItem.CreateRemote(const MachineName: string): IExtendedUserConfigMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedUserConfigMenuItem) as IExtendedUserConfigMenuItem;
end;

class function CoReportSeparator.Create: IReportSeparator;
begin
  Result := CreateComObject(CLASS_ReportSeparator) as IReportSeparator;
end;

class function CoReportSeparator.CreateRemote(const MachineName: string): IReportSeparator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ReportSeparator) as IReportSeparator;
end;

class function CoOptionsPageManager.Create: IOptionsPageManager;
begin
  Result := CreateComObject(CLASS_OptionsPageManager) as IOptionsPageManager;
end;

class function CoOptionsPageManager.CreateRemote(const MachineName: string): IOptionsPageManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OptionsPageManager) as IOptionsPageManager;
end;

procedure TCollectionsOptionsPage.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{BF10D52A-EFFA-4EAC-B567-D7A670032AFF}';
    EventIID: '{E9C57BC1-DBA7-4DD7-960A-B10256C49A3A}';
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

procedure TCollectionsOptionsPage.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ICollectionsOptionsPage;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TCollectionsOptionsPage.GetControlInterface: ICollectionsOptionsPage;
begin
  CreateControl;
  Result := FIntf;
end;

class function CoCollectionsModuleSettings.Create: ICollectionsModuleSettings;
begin
  Result := CreateComObject(CLASS_CollectionsModuleSettings) as ICollectionsModuleSettings;
end;

class function CoCollectionsModuleSettings.CreateRemote(const MachineName: string): ICollectionsModuleSettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CollectionsModuleSettings) as ICollectionsModuleSettings;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TCollectionsOptionsPage]);
end;

end.
