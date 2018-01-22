{===============================================================================
  Unit:           DebuggerTracker

  Defines:        TDebugger
                  TMethodEvent

  Description:    A generic method debugger class.  Use method instrumentation
    in ModelMaker to enable calls into the class.

  Created:        September 2003

  Model: m:\dssvl132\Debugger.mpb

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}
unit DebuggerTracker;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, MMSystem, GeneralFunctions, Contnrs;

type
  TEvent = class (TObject)
  private
    FAvailablePhysMem: Cardinal;
    FAvailableVirtMem: Cardinal;
    FTime: DWord;
    FTitle: String;
  public
    procedure Initialise(const ATitle: string); virtual;
    property AvailablePhysMem: Cardinal read FAvailablePhysMem;
    property AvailableVirtMem: Cardinal read FAvailableVirtMem;
    property Time: DWord read FTime;
    property Title: String read FTitle;
  end;
  
  TMethodEvent = class (TEvent)
  private
    FExitProc: Boolean;
  public
    procedure Initialise(const ATitle: string; AExitProc: boolean); reintroduce; overload;
    property ExitProc: Boolean read FExitProc;
  end;
  
  TExceptionEvent = class (TEvent)
  end;
  
  TMessageEvent = class (TEvent)
  end;
  
  TDebugger = class (TObject)
  private
    FMethodTrack: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    class function Allocated: Boolean;
    class procedure Discard;
    procedure DisplayAnalysis;
    procedure EnterMethod(const AMethod: string);
    procedure Exception(const AClassAndMessage: string);
    procedure ExitMethod(const AMethod: string);
    procedure Log(const AMessage: string);
  end;
  

function Debugger: TDebugger;

implementation

uses
  DebuggerAnalysis;

var
  mDebugger: TDebugger;

{===============================================================================
 Instance Accessor Method
===============================================================================}
function Debugger: TDebugger;
begin
  if not Assigned(mDebugger) then
    mDebugger := TDebugger.Create;
  Result := mDebugger;
end;


{-==============================================================================
    TEvent
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TEvent.Initialise(const ATitle: string);
var
  lMS: TMemoryStatus;
begin
  FTitle := ATitle;
  // Use accurate multimedia API to get time
  FTime := TimeGetTime;
  // Retrieve physical and virtual memory available
  lMS.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(lMS);
  FAvailablePhysMem := lMS.dwAvailPhys;
  FAvailableVirtMem := lMS.dwAvailVirtual;
end;  // TEvent.Initialise 

{-==============================================================================
    TMethodEvent
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMethodEvent.Initialise(const ATitle: string; AExitProc: boolean);
begin
  FExitProc := AExitProc;
  Inherited Initialise(ATitle);
end;  // TMethodEvent.Initialise 

{-==============================================================================
    TDebugger
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDebugger.Create;
begin
  inherited Create;
  
  FMethodTrack := TObjectList.Create;
end;  // TDebugger.Create 

{-------------------------------------------------------------------------------
}
destructor TDebugger.Destroy;
begin
  FMethodTrack.Free;
  
  inherited Destroy;
end;  // TDebugger.Destroy 

{-------------------------------------------------------------------------------
  Returns true if a debugger instance currently exist. 
}
class function TDebugger.Allocated: Boolean;
begin
  Result := Assigned(mDebugger);
end;  // TDebugger.Allocated 

{-------------------------------------------------------------------------------
  Discard the instance pointer. 
}
class procedure TDebugger.Discard;
begin
  FreeAndNil(mDebugger);
end;  // TDebugger.Discard 

{-------------------------------------------------------------------------------
  Display the analysis form. 
}
procedure TDebugger.DisplayAnalysis;
begin
  with TdlgDebuggerAnalysis.Create(nil) do try
    SetMethodTrack(FMethodTrack);
    ShowModal;
  finally
    Free;
  end;
end;  // TDebugger.DisplayAnalysis 

{-------------------------------------------------------------------------------
  Record a method entry point. 
}
procedure TDebugger.EnterMethod(const AMethod: string);
var
  lNewItem: TMethodEvent;
begin
  lNewItem := TMethodEvent.Create;
  lNewItem.Initialise(AMethod, False);
  FMethodTrack.Add(lNewItem);
end;  // TDebugger.EnterMethod 

{-------------------------------------------------------------------------------
}
procedure TDebugger.Exception(const AClassAndMessage: string);
var
  lNewItem: TExceptionEvent;
begin
  lNewItem := TExceptionEvent.Create;
  lNewItem.Initialise(AClassAndMessage);
  FMethodTrack.Add(lNewItem);
  DisplayAnalysis;
end;  // TDebugger.Exception 

{-------------------------------------------------------------------------------
  Record a method exit point 
}
procedure TDebugger.ExitMethod(const AMethod: string);
var
  lNewItem: TMethodEvent;
begin
  lNewItem := TMethodEvent.Create;
  lNewItem.Initialise(AMethod, True);
  FMethodTrack.Add(lNewItem);
end;  // TDebugger.ExitMethod 

{-------------------------------------------------------------------------------
  Log a text message 
}
procedure TDebugger.Log(const AMessage: string);
var
  lNewItem: TMessageEvent;
begin
  lNewItem := TMessageEvent.Create;
  lNewItem.Initialise(AMessage);
  FMethodTrack.Add(lNewItem);
end;  // TDebugger.Log 


end.
