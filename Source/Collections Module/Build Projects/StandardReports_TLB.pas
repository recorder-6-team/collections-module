unit StandardReports_TLB;

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
// Type Lib: F:\Collections Module\Build Projects\StandardReports.tlb (1)
// LIBID: {99CFA60D-F566-426F-90D9-3D55D32F2239}
// LCID: 0
// Helpfile: 
// HelpString: StandardReports Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  StandardReportsMajorVersion = 1;
  StandardReportsMinorVersion = 0;

  LIBID_StandardReports: TGUID = '{99CFA60D-F566-426F-90D9-3D55D32F2239}';

  IID_IReportGenerator: TGUID = '{0871F266-4CB8-40BF-8A3D-3CD4DF7606B6}';
  IID_IListReportGenerator: TGUID = '{DA4B6791-D842-4CF5-8542-B9E3D7C4BF76}';
  CLASS_ListReportGenerator: TGUID = '{B4CD9059-4682-42BB-A6D7-F7AEE46DF1C6}';
  IID_IDetailsReportGenerator: TGUID = '{F445B166-A01E-4C62-AEA7-76A5E4161164}';
  CLASS_DetailsReportGenerator: TGUID = '{DDB5D2B0-F765-4D14-BE78-295B63DC3E93}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IReportGenerator = interface;
  IReportGeneratorDisp = dispinterface;
  IListReportGenerator = interface;
  IListReportGeneratorDisp = dispinterface;
  IDetailsReportGenerator = interface;
  IDetailsReportGeneratorDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ListReportGenerator = IListReportGenerator;
  DetailsReportGenerator = IDetailsReportGenerator;


// *********************************************************************//
// Interface: IReportGenerator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0871F266-4CB8-40BF-8A3D-3CD4DF7606B6}
// *********************************************************************//
  IReportGenerator = interface(IDispatch)
    ['{0871F266-4CB8-40BF-8A3D-3CD4DF7606B6}']
    procedure GenerateReport; safecall;
    function Get_ReportKey: WideString; safecall;
    procedure Set_ReportKey(const Value: WideString); safecall;
    property ReportKey: WideString read Get_ReportKey write Set_ReportKey;
  end;

// *********************************************************************//
// DispIntf:  IReportGeneratorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0871F266-4CB8-40BF-8A3D-3CD4DF7606B6}
// *********************************************************************//
  IReportGeneratorDisp = dispinterface
    ['{0871F266-4CB8-40BF-8A3D-3CD4DF7606B6}']
    procedure GenerateReport; dispid 202;
    property ReportKey: WideString dispid 203;
  end;

// *********************************************************************//
// Interface: IListReportGenerator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA4B6791-D842-4CF5-8542-B9E3D7C4BF76}
// *********************************************************************//
  IListReportGenerator = interface(IReportGenerator)
    ['{DA4B6791-D842-4CF5-8542-B9E3D7C4BF76}']
    procedure AddItemKey(const AKey: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IListReportGeneratorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA4B6791-D842-4CF5-8542-B9E3D7C4BF76}
// *********************************************************************//
  IListReportGeneratorDisp = dispinterface
    ['{DA4B6791-D842-4CF5-8542-B9E3D7C4BF76}']
    procedure AddItemKey(const AKey: WideString); dispid 204;
    procedure GenerateReport; dispid 202;
    property ReportKey: WideString dispid 203;
  end;

// *********************************************************************//
// Interface: IDetailsReportGenerator
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F445B166-A01E-4C62-AEA7-76A5E4161164}
// *********************************************************************//
  IDetailsReportGenerator = interface(IReportGenerator)
    ['{F445B166-A01E-4C62-AEA7-76A5E4161164}']
    function Get_ItemKey: WideString; safecall;
    procedure Set_ItemKey(const Value: WideString); safecall;
    property ItemKey: WideString read Get_ItemKey write Set_ItemKey;
  end;

// *********************************************************************//
// DispIntf:  IDetailsReportGeneratorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F445B166-A01E-4C62-AEA7-76A5E4161164}
// *********************************************************************//
  IDetailsReportGeneratorDisp = dispinterface
    ['{F445B166-A01E-4C62-AEA7-76A5E4161164}']
    property ItemKey: WideString dispid 301;
    procedure GenerateReport; dispid 202;
    property ReportKey: WideString dispid 203;
  end;

// *********************************************************************//
// The Class CoListReportGenerator provides a Create and CreateRemote method to          
// create instances of the default interface IListReportGenerator exposed by              
// the CoClass ListReportGenerator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoListReportGenerator = class
    class function Create: IListReportGenerator;
    class function CreateRemote(const MachineName: string): IListReportGenerator;
  end;

// *********************************************************************//
// The Class CoDetailsReportGenerator provides a Create and CreateRemote method to          
// create instances of the default interface IDetailsReportGenerator exposed by              
// the CoClass DetailsReportGenerator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDetailsReportGenerator = class
    class function Create: IDetailsReportGenerator;
    class function CreateRemote(const MachineName: string): IDetailsReportGenerator;
  end;

implementation

uses ComObj;

class function CoListReportGenerator.Create: IListReportGenerator;
begin
  Result := CreateComObject(CLASS_ListReportGenerator) as IListReportGenerator;
end;

class function CoListReportGenerator.CreateRemote(const MachineName: string): IListReportGenerator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ListReportGenerator) as IListReportGenerator;
end;

class function CoDetailsReportGenerator.Create: IDetailsReportGenerator;
begin
  Result := CreateComObject(CLASS_DetailsReportGenerator) as IDetailsReportGenerator;
end;

class function CoDetailsReportGenerator.CreateRemote(const MachineName: string): IDetailsReportGenerator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DetailsReportGenerator) as IDetailsReportGenerator;
end;

end.
