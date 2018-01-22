unit ImageMagickObject_TLB;

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
// File generated on 17/06/2008 14:42:25 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\ImageMagick-6.4.1-Q16\ImageMagickObject.dll (1)
// LIBID: {9AA0FC6A-63C7-3632-BD6B-7CAF646E51A0}
// LCID: 0
// Helpfile: 
// HelpString: ImageMagickObject 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\STDOLE2.TLB)
// Errors:
//   Error creating palette bitmap of (TMagickImage) : Server C:\Program Files\ImageMagick-6.4.1-Q16\ImageMagickObject.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ImageMagickObjectMajorVersion = 1;
  ImageMagickObjectMinorVersion = 0;

  LIBID_ImageMagickObject: TGUID = '{9AA0FC6A-63C7-3632-BD6B-7CAF646E51A0}';

  IID_IMagickImage: TGUID = '{7F670536-00AE-4EDF-B06F-13BD22B25624}';
  CLASS_MagickImage: TGUID = '{5630BE5A-3F5F-4BCA-A511-AD6A6386CAC1}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IMagickImage = interface;
  IMagickImageDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  MagickImage = IMagickImage;


// *********************************************************************//
// Interface: IMagickImage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F670536-00AE-4EDF-B06F-13BD22B25624}
// *********************************************************************//
  IMagickImage = interface(IDispatch)
    ['{7F670536-00AE-4EDF-B06F-13BD22B25624}']
    procedure OnStartPage(const piUnk: IUnknown); safecall;
    procedure OnEndPage; safecall;
    function Get_Count: Integer; safecall;
    function Add(var pArrayVar: PSafeArray): OleVariant; safecall;
    procedure Remove(varIndex: OleVariant); safecall;
    function Compare(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Composite(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Convert(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Identify(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Mogrify(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Montage(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Stream(var pArrayVar: PSafeArray): OleVariant; safecall;
    function TestHarness(var pArrayVar: PSafeArray): OleVariant; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(varIndex: OleVariant): OleVariant; safecall;
    function Get_Messages: OleVariant; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[varIndex: OleVariant]: OleVariant read Get_Item; default;
    property Messages: OleVariant read Get_Messages;
  end;

// *********************************************************************//
// DispIntf:  IMagickImageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F670536-00AE-4EDF-B06F-13BD22B25624}
// *********************************************************************//
  IMagickImageDisp = dispinterface
    ['{7F670536-00AE-4EDF-B06F-13BD22B25624}']
    procedure OnStartPage(const piUnk: IUnknown); dispid 1610743808;
    procedure OnEndPage; dispid 1610743809;
    property Count: Integer readonly dispid 1;
    function Add(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 2;
    procedure Remove(varIndex: OleVariant); dispid 3;
    function Compare(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 4;
    function Composite(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 5;
    function Convert(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 6;
    function Identify(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 7;
    function Mogrify(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 8;
    function Montage(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 9;
    function Stream(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 10;
    function TestHarness(var pArrayVar: {??PSafeArray}OleVariant): OleVariant; dispid 11;
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[varIndex: OleVariant]: OleVariant readonly dispid 0; default;
    property Messages: OleVariant readonly dispid 14;
  end;

// *********************************************************************//
// The Class CoMagickImage provides a Create and CreateRemote method to          
// create instances of the default interface IMagickImage exposed by              
// the CoClass MagickImage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMagickImage = class
    class function Create: IMagickImage;
    class function CreateRemote(const MachineName: string): IMagickImage;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TMagickImage
// Help String      : MagickImage Class
// Default Interface: IMagickImage
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TMagickImageProperties= class;
{$ENDIF}
  TMagickImage = class(TOleServer)
  private
    FIntf:        IMagickImage;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TMagickImageProperties;
    function      GetServerProperties: TMagickImageProperties;
{$ENDIF}
    function      GetDefaultInterface: IMagickImage;
  protected
    procedure InitServerData; override;
    function Get_Count: Integer;
    function Get__NewEnum: IUnknown;
    function Get_Item(varIndex: OleVariant): OleVariant;
    function Get_Messages: OleVariant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IMagickImage);
    procedure Disconnect; override;
    procedure OnStartPage(const piUnk: IUnknown);
    procedure OnEndPage;
    function Add(var pArrayVar: PSafeArray): OleVariant;
    procedure Remove(varIndex: OleVariant);
    function Compare(var pArrayVar: PSafeArray): OleVariant;
    function Composite(var pArrayVar: PSafeArray): OleVariant;
    function Convert(var pArrayVar: PSafeArray): OleVariant;
    function Identify(var pArrayVar: PSafeArray): OleVariant;
    function Mogrify(var pArrayVar: PSafeArray): OleVariant;
    function Montage(var pArrayVar: PSafeArray): OleVariant;
    function Stream(var pArrayVar: PSafeArray): OleVariant;
    function TestHarness(var pArrayVar: PSafeArray): OleVariant;
    property DefaultInterface: IMagickImage read GetDefaultInterface;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[varIndex: OleVariant]: OleVariant read Get_Item; default;
    property Messages: OleVariant read Get_Messages;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TMagickImageProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TMagickImage
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TMagickImageProperties = class(TPersistent)
  private
    FServer:    TMagickImage;
    function    GetDefaultInterface: IMagickImage;
    constructor Create(AServer: TMagickImage);
  protected
    function Get_Count: Integer;
    function Get__NewEnum: IUnknown;
    function Get_Item(varIndex: OleVariant): OleVariant;
    function Get_Messages: OleVariant;
  public
    property DefaultInterface: IMagickImage read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoMagickImage.Create: IMagickImage;
begin
  Result := CreateComObject(CLASS_MagickImage) as IMagickImage;
end;

class function CoMagickImage.CreateRemote(const MachineName: string): IMagickImage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MagickImage) as IMagickImage;
end;

procedure TMagickImage.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5630BE5A-3F5F-4BCA-A511-AD6A6386CAC1}';
    IntfIID:   '{7F670536-00AE-4EDF-B06F-13BD22B25624}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TMagickImage.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IMagickImage;
  end;
end;

procedure TMagickImage.ConnectTo(svrIntf: IMagickImage);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TMagickImage.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TMagickImage.GetDefaultInterface: IMagickImage;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TMagickImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TMagickImageProperties.Create(Self);
{$ENDIF}
end;

destructor TMagickImage.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TMagickImage.GetServerProperties: TMagickImageProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TMagickImage.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TMagickImage.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TMagickImage.Get_Item(varIndex: OleVariant): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Item[varIndex];
end;

function TMagickImage.Get_Messages: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Messages;
end;

procedure TMagickImage.OnStartPage(const piUnk: IUnknown);
begin
  DefaultInterface.OnStartPage(piUnk);
end;

procedure TMagickImage.OnEndPage;
begin
  DefaultInterface.OnEndPage;
end;

function TMagickImage.Add(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Add(pArrayVar);
end;

procedure TMagickImage.Remove(varIndex: OleVariant);
begin
  DefaultInterface.Remove(varIndex);
end;

function TMagickImage.Compare(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Compare(pArrayVar);
end;

function TMagickImage.Composite(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Composite(pArrayVar);
end;

function TMagickImage.Convert(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Convert(pArrayVar);
end;

function TMagickImage.Identify(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Identify(pArrayVar);
end;

function TMagickImage.Mogrify(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Mogrify(pArrayVar);
end;

function TMagickImage.Montage(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Montage(pArrayVar);
end;

function TMagickImage.Stream(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.Stream(pArrayVar);
end;

function TMagickImage.TestHarness(var pArrayVar: PSafeArray): OleVariant;
begin
  Result := DefaultInterface.TestHarness(pArrayVar);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TMagickImageProperties.Create(AServer: TMagickImage);
begin
  inherited Create;
  FServer := AServer;
end;

function TMagickImageProperties.GetDefaultInterface: IMagickImage;
begin
  Result := FServer.DefaultInterface;
end;

function TMagickImageProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TMagickImageProperties.Get__NewEnum: IUnknown;
begin
    Result := DefaultInterface._NewEnum;
end;

function TMagickImageProperties.Get_Item(varIndex: OleVariant): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Item[varIndex];
end;

function TMagickImageProperties.Get_Messages: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Messages;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TMagickImage]);
end;

end.
