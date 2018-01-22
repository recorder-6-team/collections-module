unit ThesaurusBrowser_TLB;

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
// File generated on 20/09/2017 11:11:36 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\Collections Module\Build Projects\ThesaurusBrowser.tlb (1)
// LIBID: {412F3A99-7FB6-4F48-8D07-7D5556537FBD}
// LCID: 0
// Helpfile: 
// HelpString: ThesaurusBrowser Library
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
  ThesaurusBrowserMajorVersion = 1;
  ThesaurusBrowserMinorVersion = 0;

  LIBID_ThesaurusBrowser: TGUID = '{412F3A99-7FB6-4F48-8D07-7D5556537FBD}';

  IID_IDiagramsMenu: TGUID = '{2B7F9437-407E-489D-A6C2-44627F25A1B4}';
  CLASS_DiagramsMenu: TGUID = '{878C74F6-ECDF-4D1C-8ADD-10ED7209B8A3}';
  IID_IfrmThesaurusBrowser: TGUID = '{35438EF0-7738-424A-B49F-F442370C051A}';
  DIID_IfrmThesaurusBrowserEvents: TGUID = '{76E7EA39-9D0E-448F-BE21-10CC74BBB59F}';
  CLASS_frmThesaurusBrowser: TGUID = '{DC5B5BC9-C1CA-4C53-8BFD-C16D2276568B}';
  IID_ISynchronizable: TGUID = '{6ABAB80C-6736-4D6A-9681-ED3C0B1B75E9}';

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
  IDiagramsMenu = interface;
  IDiagramsMenuDisp = dispinterface;
  IfrmThesaurusBrowser = interface;
  IfrmThesaurusBrowserDisp = dispinterface;
  IfrmThesaurusBrowserEvents = dispinterface;
  ISynchronizable = interface;
  ISynchronizableDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  DiagramsMenu = IDiagramsMenu;
  frmThesaurusBrowser = IfrmThesaurusBrowser;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IDiagramsMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B7F9437-407E-489D-A6C2-44627F25A1B4}
// *********************************************************************//
  IDiagramsMenu = interface(IDispatch)
    ['{2B7F9437-407E-489D-A6C2-44627F25A1B4}']
    function Get_ItemType: Integer; safecall;
    procedure Set_ItemType(Value: Integer); safecall;
    function Get_ThesaurusBrowser: Integer; safecall;
    procedure Set_ThesaurusBrowser(Value: Integer); safecall;
    property ItemType: Integer read Get_ItemType write Set_ItemType;
    property ThesaurusBrowser: Integer read Get_ThesaurusBrowser write Set_ThesaurusBrowser;
  end;

// *********************************************************************//
// DispIntf:  IDiagramsMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B7F9437-407E-489D-A6C2-44627F25A1B4}
// *********************************************************************//
  IDiagramsMenuDisp = dispinterface
    ['{2B7F9437-407E-489D-A6C2-44627F25A1B4}']
    property ItemType: Integer dispid 201;
    property ThesaurusBrowser: Integer dispid 202;
  end;

// *********************************************************************//
// Interface: IfrmThesaurusBrowser
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {35438EF0-7738-424A-B49F-F442370C051A}
// *********************************************************************//
  IfrmThesaurusBrowser = interface(IDispatch)
    ['{35438EF0-7738-424A-B49F-F442370C051A}']
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
    procedure DisplayConceptGroup(const AKey: WideString); safecall;
    procedure DisplayConcept(const AKey: WideString); safecall;
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
// DispIntf:  IfrmThesaurusBrowserDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {35438EF0-7738-424A-B49F-F442370C051A}
// *********************************************************************//
  IfrmThesaurusBrowserDisp = dispinterface
    ['{35438EF0-7738-424A-B49F-F442370C051A}']
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
    procedure DisplayConceptGroup(const AKey: WideString); dispid 217;
    procedure DisplayConcept(const AKey: WideString); dispid 218;
  end;

// *********************************************************************//
// DispIntf:  IfrmThesaurusBrowserEvents
// Flags:     (4096) Dispatchable
// GUID:      {76E7EA39-9D0E-448F-BE21-10CC74BBB59F}
// *********************************************************************//
  IfrmThesaurusBrowserEvents = dispinterface
    ['{76E7EA39-9D0E-448F-BE21-10CC74BBB59F}']
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
// Interface: ISynchronizable
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ABAB80C-6736-4D6A-9681-ED3C0B1B75E9}
// *********************************************************************//
  ISynchronizable = interface(IDispatch)
    ['{6ABAB80C-6736-4D6A-9681-ED3C0B1B75E9}']
    procedure SynchronizedCallback(ID: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISynchronizableDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ABAB80C-6736-4D6A-9681-ED3C0B1B75E9}
// *********************************************************************//
  ISynchronizableDisp = dispinterface
    ['{6ABAB80C-6736-4D6A-9681-ED3C0B1B75E9}']
    procedure SynchronizedCallback(ID: Integer); dispid 201;
  end;

// *********************************************************************//
// The Class CoDiagramsMenu provides a Create and CreateRemote method to          
// create instances of the default interface IDiagramsMenu exposed by              
// the CoClass DiagramsMenu. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDiagramsMenu = class
    class function Create: IDiagramsMenu;
    class function CreateRemote(const MachineName: string): IDiagramsMenu;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TfrmThesaurusBrowser
// Help String      : frmThesaurusBrowser Control
// Default Interface: IfrmThesaurusBrowser
// Def. Intf. DISP? : No
// Event   Interface: IfrmThesaurusBrowserEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TfrmThesaurusBrowserOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TfrmThesaurusBrowser = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TfrmThesaurusBrowserOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: IfrmThesaurusBrowser;
    function  GetControlInterface: IfrmThesaurusBrowser;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure DisplayConceptGroup(const AKey: WideString);
    procedure DisplayConcept(const AKey: WideString);
    property  ControlInterface: IfrmThesaurusBrowser read GetControlInterface;
    property  DefaultInterface: IfrmThesaurusBrowser read GetControlInterface;
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
    property OnKeyPress: TfrmThesaurusBrowserOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoDiagramsMenu.Create: IDiagramsMenu;
begin
  Result := CreateComObject(CLASS_DiagramsMenu) as IDiagramsMenu;
end;

class function CoDiagramsMenu.CreateRemote(const MachineName: string): IDiagramsMenu;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DiagramsMenu) as IDiagramsMenu;
end;

procedure TfrmThesaurusBrowser.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{DC5B5BC9-C1CA-4C53-8BFD-C16D2276568B}';
    EventIID: '{76E7EA39-9D0E-448F-BE21-10CC74BBB59F}';
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

procedure TfrmThesaurusBrowser.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IfrmThesaurusBrowser;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TfrmThesaurusBrowser.GetControlInterface: IfrmThesaurusBrowser;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TfrmThesaurusBrowser.DisplayConceptGroup(const AKey: WideString);
begin
  DefaultInterface.DisplayConceptGroup(AKey);
end;

procedure TfrmThesaurusBrowser.DisplayConcept(const AKey: WideString);
begin
  DefaultInterface.DisplayConcept(AKey);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TfrmThesaurusBrowser]);
end;

end.
