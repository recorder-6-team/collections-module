unit SpecimenFinderModule_TLB;

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
// File generated on 20/09/2017 11:11:35 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\Collections Module\Build Projects\SpecimenFinderModule.tlb (1)
// LIBID: {3B277FA6-97AD-4544-9031-0E20E101B5CA}
// LCID: 0
// Helpfile: 
// HelpString: SpecimenFinderModule Library
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
  SpecimenFinderModuleMajorVersion = 1;
  SpecimenFinderModuleMinorVersion = 0;

  LIBID_SpecimenFinderModule: TGUID = '{3B277FA6-97AD-4544-9031-0E20E101B5CA}';

  IID_ISpecimenFinder: TGUID = '{84AA030A-8F9C-4906-AA5C-64A18EA06EC5}';
  DIID_ISpecimenFinderEvents: TGUID = '{96CFD171-DFB9-4607-AD6F-5735D0035917}';
  CLASS_SpecimenFinder: TGUID = '{83520A36-5682-4521-9255-CD0C8233ECE8}';
  IID_IfrmSpecimenFinder: TGUID = '{5F9649B5-0643-48B5-B45D-C3E0F5D6861D}';
  DIID_IfrmSpecimenFinderEvents: TGUID = '{78CF9321-6BB8-4E0D-A645-9341AD69C55D}';
  CLASS_frmSpecimenFinder: TGUID = '{3960DCF4-ECA4-4DC6-A6CF-F4945B841B53}';
  IID_ISpecimenFinderQueryMenu: TGUID = '{1012F1B7-F710-46F1-9EEC-B431993C89D7}';
  CLASS_SpecimenFinderQueryMenu: TGUID = '{64496912-834B-4AF0-AEF7-73B9D0F4F822}';

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
  ISpecimenFinder = interface;
  ISpecimenFinderDisp = dispinterface;
  ISpecimenFinderEvents = dispinterface;
  IfrmSpecimenFinder = interface;
  IfrmSpecimenFinderDisp = dispinterface;
  IfrmSpecimenFinderEvents = dispinterface;
  ISpecimenFinderQueryMenu = interface;
  ISpecimenFinderQueryMenuDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SpecimenFinder = ISpecimenFinder;
  frmSpecimenFinder = IfrmSpecimenFinder;
  SpecimenFinderQueryMenu = ISpecimenFinderQueryMenu;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: ISpecimenFinder
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84AA030A-8F9C-4906-AA5C-64A18EA06EC5}
// *********************************************************************//
  ISpecimenFinder = interface(IDispatch)
    ['{84AA030A-8F9C-4906-AA5C-64A18EA06EC5}']
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
// DispIntf:  ISpecimenFinderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84AA030A-8F9C-4906-AA5C-64A18EA06EC5}
// *********************************************************************//
  ISpecimenFinderDisp = dispinterface
    ['{84AA030A-8F9C-4906-AA5C-64A18EA06EC5}']
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
// DispIntf:  ISpecimenFinderEvents
// Flags:     (4096) Dispatchable
// GUID:      {96CFD171-DFB9-4607-AD6F-5735D0035917}
// *********************************************************************//
  ISpecimenFinderEvents = dispinterface
    ['{96CFD171-DFB9-4607-AD6F-5735D0035917}']
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
// Interface: IfrmSpecimenFinder
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F9649B5-0643-48B5-B45D-C3E0F5D6861D}
// *********************************************************************//
  IfrmSpecimenFinder = interface(IDispatch)
    ['{5F9649B5-0643-48B5-B45D-C3E0F5D6861D}']
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
// DispIntf:  IfrmSpecimenFinderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F9649B5-0643-48B5-B45D-C3E0F5D6861D}
// *********************************************************************//
  IfrmSpecimenFinderDisp = dispinterface
    ['{5F9649B5-0643-48B5-B45D-C3E0F5D6861D}']
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
// DispIntf:  IfrmSpecimenFinderEvents
// Flags:     (4096) Dispatchable
// GUID:      {78CF9321-6BB8-4E0D-A645-9341AD69C55D}
// *********************************************************************//
  IfrmSpecimenFinderEvents = dispinterface
    ['{78CF9321-6BB8-4E0D-A645-9341AD69C55D}']
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
// Interface: ISpecimenFinderQueryMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1012F1B7-F710-46F1-9EEC-B431993C89D7}
// *********************************************************************//
  ISpecimenFinderQueryMenu = interface(IDispatch)
    ['{1012F1B7-F710-46F1-9EEC-B431993C89D7}']
    function Get_ItemType: Integer; safecall;
    procedure Set_ItemType(Value: Integer); safecall;
    function Get_SpecimenFinder: Integer; safecall;
    procedure Set_SpecimenFinder(Value: Integer); safecall;
    property ItemType: Integer read Get_ItemType write Set_ItemType;
    property SpecimenFinder: Integer read Get_SpecimenFinder write Set_SpecimenFinder;
  end;

// *********************************************************************//
// DispIntf:  ISpecimenFinderQueryMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1012F1B7-F710-46F1-9EEC-B431993C89D7}
// *********************************************************************//
  ISpecimenFinderQueryMenuDisp = dispinterface
    ['{1012F1B7-F710-46F1-9EEC-B431993C89D7}']
    property ItemType: Integer dispid 201;
    property SpecimenFinder: Integer dispid 202;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TSpecimenFinder
// Help String      : SpecimenFinder Control
// Default Interface: ISpecimenFinder
// Def. Intf. DISP? : No
// Event   Interface: ISpecimenFinderEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TSpecimenFinderOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TSpecimenFinder = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TSpecimenFinderOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: ISpecimenFinder;
    function  GetControlInterface: ISpecimenFinder;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    property  ControlInterface: ISpecimenFinder read GetControlInterface;
    property  DefaultInterface: ISpecimenFinder read GetControlInterface;
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
    property OnKeyPress: TSpecimenFinderOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TfrmSpecimenFinder
// Help String      : frmSpecimenFinder Control
// Default Interface: IfrmSpecimenFinder
// Def. Intf. DISP? : No
// Event   Interface: IfrmSpecimenFinderEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TfrmSpecimenFinderOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TfrmSpecimenFinder = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TfrmSpecimenFinderOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: IfrmSpecimenFinder;
    function  GetControlInterface: IfrmSpecimenFinder;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    property  ControlInterface: IfrmSpecimenFinder read GetControlInterface;
    property  DefaultInterface: IfrmSpecimenFinder read GetControlInterface;
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
    property OnKeyPress: TfrmSpecimenFinderOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

// *********************************************************************//
// The Class CoSpecimenFinderQueryMenu provides a Create and CreateRemote method to          
// create instances of the default interface ISpecimenFinderQueryMenu exposed by              
// the CoClass SpecimenFinderQueryMenu. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpecimenFinderQueryMenu = class
    class function Create: ISpecimenFinderQueryMenu;
    class function CreateRemote(const MachineName: string): ISpecimenFinderQueryMenu;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TSpecimenFinder.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{83520A36-5682-4521-9255-CD0C8233ECE8}';
    EventIID: '{96CFD171-DFB9-4607-AD6F-5735D0035917}';
    EventCount: 8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040154*);
    Flags: $0000001D;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivate) - Cardinal(Self);
end;

procedure TSpecimenFinder.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ISpecimenFinder;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TSpecimenFinder.GetControlInterface: ISpecimenFinder;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TfrmSpecimenFinder.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{3960DCF4-ECA4-4DC6-A6CF-F4945B841B53}';
    EventIID: '{78CF9321-6BB8-4E0D-A645-9341AD69C55D}';
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

procedure TfrmSpecimenFinder.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IfrmSpecimenFinder;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TfrmSpecimenFinder.GetControlInterface: IfrmSpecimenFinder;
begin
  CreateControl;
  Result := FIntf;
end;

class function CoSpecimenFinderQueryMenu.Create: ISpecimenFinderQueryMenu;
begin
  Result := CreateComObject(CLASS_SpecimenFinderQueryMenu) as ISpecimenFinderQueryMenu;
end;

class function CoSpecimenFinderQueryMenu.CreateRemote(const MachineName: string): ISpecimenFinderQueryMenu;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpecimenFinderQueryMenu) as ISpecimenFinderQueryMenu;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TSpecimenFinder, TfrmSpecimenFinder]);
end;

end.
