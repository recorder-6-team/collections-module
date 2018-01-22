{ Exception handler, with TExceptionPath class.
    Set the application exception handler to the one in this unit
    (TfrmException.GlobalExceptionHandler).
    Inherit all your exception classes from TExeptionPath.
    Saves critical exception info to a file, LastError.Txt in the application
    directory.  If you catch an exception, pass the information into the new
    exception constructor as the second parameter - this way entire paths
    of exceptions can be traced.
    Use CreateNonCritical to demote the exception message to non-critical.
    This tones down the exception handler form.
    NEW//
    Add items to the NonCriticalExceptions string list to force that execption
       class to be non critical

  Copyright 1999 Dorset Software Services Ltd
  By John van Breda 05/05/99 }


unit ExceptionForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, VersionInfo;


type
  { Class to hold each item on the path of exceptions - we need to keep it this
       way because exceptions are automatically destructed when handled }
  TExceptionInfo = class
    ClassName : string;
    Msg : string;
    Critical : boolean;
    constructor Create( const iClassName, iMsg : string; const iCritical : boolean );
  end;  // TExceptionInfo

  //----------------------------------------------------------------------------
  { Exception class }
  TExceptionPath = class(Exception)
    FCritical : boolean;
  public
    constructor Create(const Msg : string); overload;
    constructor Create(const Msg : string; iException : Exception); overload;
    constructor CreateNonCritical(const Msg : string); overload;
    constructor CreateNonCritical(const Msg : string; iException : Exception); overload;
    constructor CreateValidation(const Msg : string;
                                        iFocusControl : TWinControl); overload;
    constructor CreateValidation(const Msg : string; iException : Exception;
                                        iFocusControl : TWinControl); overload;
    property Critical : boolean read FCritical;
  end;  // TExceptionPath

  //----------------------------------------------------------------------------
  { Exception handler form }
  TfrmException = class(TForm)
    bbMore: TButton;
    bbOk: TBitBtn;
    lblError: TLabel;
    lblMoreInfo: TLabel;
    imgIcon: TImage;
    mmMoreInfo: TMemo;
    procedure bbMoreClick(Sender: TObject);
  private
    { Private declarations }
    FException : Exception;
    FExceptionStrings : TStringList;
    FErrorFileName: string;
    function GetExceptionInfo(iIndex: integer): TExceptionInfo;
    procedure SaveLogFile;
  protected
    class function GetFirstNonCritical: TExceptionInfo;
    class function IsCritical(Exception: TObject): Boolean;
    procedure ReadLogStrings;
    class procedure ShowNonCriticalMessage(E: Exception);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; iException : Exception); reintroduce; overload;
    constructor Create(Aowner : TComponent); reintroduce; overload;
    destructor Destroy; override;
    class procedure GlobalExceptionHandler(Sender: TObject; E: Exception);
    {$IFDEF madExcept}
    class procedure MadExceptionHandler(frozen          : boolean;
                                        exceptObject    : TObject;
                                        exceptAddr      : pointer;
                                        crashedThreadId : dword;
                                        var bugReport   : string;
                                        var screenShot  : string;
                                        var canContinue : boolean;
                                        var handled     : boolean);
    {$ENDIF}
    class procedure NewExceptionList;
    class procedure FreeExceptionList;
    class procedure LogEvent(const iEvent : string); // keep event log for error reports
    property ExceptionItems[ iIndex : integer ] : TExceptionInfo read GetExceptionInfo;
  end;  // TfrmException

resourcestring
  ResStr_CriticalError = 'A critical error has occurred in the %s application.  ' +
                         'Click the More Details button if you require further information.';

  ResStr_ExceptionOccuredInApp = 'Exception occurred in application %s at %s.';

var
  frmException: TfrmException;
  ExceptionList : TList;
  NonCriticalExceptions : TStringList;
  // configurable variable for the error message
  MstErrorMessage : string = ResStr_CriticalError;
  // Defined file output so it can be modified by application using it.
  DefaultOutputPath: String;

// global validation procedures
procedure ValidateValue(const AValue: boolean; const AMsg: string); overload;
procedure ValidateValue(const AValue: boolean; const AMsg: string;
  const AControl: TWinControl); overload;

//==============================================================================
implementation

uses GeneralFunctions, ComCtrls;

{$R *.DFM}
                                                          
const
  { Set size of recent event log according to debug mode }
  {$IFDEF DEBUG}
  EVENT_LOG_SIZE = 30;
  {$ELSE}
  EVENT_LOG_SIZE = 8; // change this to increase event log size for error analysis
  {$ENDIF}


resourcestring
  { This error will never happen ! }
  ResStr_BadCall = 'Trying to construct the error handler form using the wrong' +
                 ' overloaded constructor.';
  ResStr_InfoSavedToFile =  'Information has been saved to the file %s';
  ResStr_Version = 'Version : %s';
  ResStr_ExceptionPath =  'Exception path:';
  ResStr_LastEventAction =  'Last event\actions: ';
  ResStr_OSInfo = 'Operating System : %s';
  ResStr_PhysicalMemInfo =  'Physical Memory available : %s';
  ResStr_DLLsLoaded = 'DLLs loaded:';
  ResStr_FileSaveError =  '<File not saved due to error>';


// module level class data
var
  mEventLog : array[0..EVENT_LOG_SIZE] of string;
  mEventLogPosition : integer = 0;

//==============================================================================
//==============================================================================
procedure ValidateValue(const AValue: boolean; const AMsg: string);
begin
  if not AValue then
    raise TExceptionPath.CreateNonCritical(AMsg);
end;  // ValidateValue


//==============================================================================
procedure ValidateValue(const AValue: boolean; const AMsg: string;
  const AControl: TWinControl);

    procedure MakeControlVisible;
    var
      lParent: TWinControl;
      lTabSheet: TTabSheet;
    begin
      if not AControl.CanFocus then begin
        // Control cannot receive focus.  If this is because it is on a hidden
        // tab sheet, bring it to the front.
        lParent := AControl.Parent;
        lTabSheet := nil;
        // Look up the containers, setting ALL page controls to the right tab
        while Assigned(lParent) do begin
          if (lParent is TPageControl) then begin
            if Assigned(lTabSheet) then
              (lParent as TPageControl).ActivePage := lTabSheet;
          end else
          begin
            if lParent is TTabSheet then
              lTabSheet := lParent as TTabSheet;
          end;
          lParent := lParent.Parent;
        end; // while
      end;
    end;

begin
  if not AValue then begin
    MakeControlVisible;
    if AControl.Visible then // safety in case hidden by COM addin
      AControl.SetFocus;
    raise TExceptionPath.CreateValidation(AMsg, AControl);
  end;
end;  // ValidateValue

//==============================================================================
//==============================================================================
{ Create a new instance of the exception list, and cleanup the old one if there
     is one. }
class procedure TfrmException.NewExceptionList;
var
  i : integer;
begin
  for i := 0 to ExceptionList.Count-1 do
    TExceptionInfo(ExceptionList[i]).Free;
  ExceptionList.Clear;
end;  // TfrmException.NewExceptionList

//------------------------------------------------------------------------------
{ Cleanup - occurs during finalization }
class procedure TfrmException.FreeExceptionList;
var
  i : integer;
begin
  if ExceptionList <> nil then
  begin
    for i := 0 to ExceptionList.Count-1 do
      TExceptionInfo(ExceptionList[i]).Free;
    ExceptionList.Free;
  end;
end;  // TfrmException.FreeExceptionList

//==============================================================================
{ TExceptionInfo }
//==============================================================================

{ Constructor - just store any info we require to remember about the exception }
constructor TExceptionInfo.Create(const iClassName, iMsg: string;
  const iCritical: boolean);
begin
  inherited Create;
  ClassName := iClassName;
  Msg := iMsg;
  { Check for specified non critical classes }
  Critical := iCritical and (NonCriticalExceptions.IndexOf(iClassName)=-1);
end;  // TExceptionInfo.Create

//==============================================================================
{ TExceptionPath }
//==============================================================================

{ Create a critical exception at the root level }
constructor TExceptionPath.Create(const Msg: string);
begin
  FCritical := True;
  TfrmException.NewExceptionList;
  inherited Create(Msg);
end;  // TExceptionPath.Create (Message only)

//------------------------------------------------------------------------------
{ Create a critical secondary exception, passing in a pointer to the previous
     exception.  Use the previous exception path only if it is relevant }
constructor TExceptionPath.Create(const Msg: string; iException: Exception);
var
  lExceptionInfo : TExceptionInfo;
begin
  if iException is TExceptionPath then
  begin
    { Add previous exception to list }
    lExceptionInfo := TExceptionInfo.Create( iException.ClassName, iException.Message,
                                             TExceptionPath(iException).Critical );
    { Read the previous exception's critical status }
    FCritical := TExceptionPath(iException).Critical;
  end else
  begin
    FCritical := True;
    TfrmException.NewExceptionList;
    { Add previous exception to list }
    lExceptionInfo := TExceptionInfo.Create( iException.ClassName, iException.Message,
                                             True );
  end;
  ExceptionList.Add( lExceptionInfo );
  inherited Create(Msg);
end;  // TExceptionPath.Create (Message and exception)

//------------------------------------------------------------------------------
{ Create a root exception which is non-critical (ie just a nice message if it
     gets to the top level }
constructor TExceptionPath.CreateNonCritical(const Msg: string);
begin
  FCritical := False;
  TfrmException.NewExceptionList;
  inherited Create(Msg);
end;  // TExceptionPath.CreateNonCritical (Message only)

//------------------------------------------------------------------------------
{ Create a non-critical secondary exception, passing in a pointer to the previous
     exception.  Use the previous exception path only if it is relevant }
constructor TExceptionPath.CreateNonCritical(const Msg: string; iException: Exception);
var
  lExceptionInfo : TExceptionInfo;
begin
  FCritical := False;
  { Read the previous exception's critical status }
  if iException is TExceptionPath then
    { Add previous exception to list }
    lExceptionInfo := TExceptionInfo.Create( iException.ClassName, iException.Message,
                                             TExceptionPath(iException).Critical )
  else begin
    TfrmException.NewExceptionList;
    { Add previous exception to list }
    lExceptionInfo := TExceptionInfo.Create( iException.ClassName, iException.Message,
                                             True );
  end;
  ExceptionList.Add( lExceptionInfo );
  inherited Create(Msg);
end;  // TExceptionPath.CreateNonCritical (Message and exception)

//------------------------------------------------------------------------------
{ Versions of CreateNonCritical which automatically set a control
    to focus - to prompt the user to enter correct data }
constructor TExceptionPath.CreateValidation(const Msg: string; iFocusControl: TWinControl);
begin
  CreateNonCritical(Msg);
  if iFocusControl <> nil then
    if iFocusControl.Visible and iFocusControl.Enabled then
      iFocusControl.SetFocus;
end;  // TExceptionPath.CreateValidation (Message and contol)

//------------------------------------------------------------------------------
{ And again }
constructor TExceptionPath.CreateValidation(const Msg: string;
  iException: Exception; iFocusControl: TWinControl);
begin
  CreateNonCritical(Msg, iException);
  if iFocusControl <> nil then
    if iFocusControl.Visible and iFocusControl.Enabled then
      iFocusControl.SetFocus;
end;  // TExceptionPath.CreateValidation (Message, exception and control)

//==============================================================================
{ TfrmException }
//==============================================================================
{-------------------------------------------------------------------------------
  Should this exception be treated as critical?
}
class function TfrmException.IsCritical(Exception: TObject): Boolean;
begin
  if not (Exception is TExceptionPath) then
    Result := NonCriticalExceptions.IndexOf(Exception.ClassName) = -1
  else
  begin
    { must look at first item in the list to check if specified non critical }
    if ExceptionList.Count > 0 then
      Result := TExceptionPath(Exception).Critical
          and (NonCriticalExceptions.IndexOf(
                  TExceptionInfo(ExceptionList[0]).ClassName) = -1)
    else
      Result := TExceptionPath(exceptObject).Critical
          and (NonCriticalExceptions.IndexOf(Exception.ClassName) = -1);
  end;
end;

{ Global exception handler - a class procedure so that is can be called without
     an instance of the form - effectively it is a sort of constructor for the
     form - exception that it throws the form away afterwards }
class procedure TfrmException.GlobalExceptionHandler(Sender: TObject; E: Exception);
begin
  if not IsCritical(E) then
    ShowNonCriticalMessage(E)
  else
  begin
    frmException := Create(Application, E);
    try
      frmException.ShowModal;
    finally
      frmException.Free;
    end; // finally
  end;
end;  // TfrmException.GlobalExceptionHandler

{$IFDEF madExcept}
{-------------------------------------------------------------------------------
  Global exception handled for when Mad exceptions are installed
}
class procedure TfrmException.MadExceptionHandler(
                              frozen          : boolean;
                              exceptObject    : TObject;
                              exceptAddr      : pointer;
                              crashedThreadId : dword;
                              var bugReport   : string;
                              var screenShot  : string;
                              var canContinue : boolean;
                              var handled     : boolean);
var
  lIdx: integer;
begin
  if not (exceptObject is TExceptionPath) then NewExceptionList;
  if not IsCritical(exceptObject) then begin
    TfrmException.GlobalExceptionHandler(nil, exceptObject as Exception);
    Handled := True;
  end
  else begin
    Handled := False;
    // Add our exception stack to the Mad exception bug report.
    if ExceptionList.Count>0 then begin
      bugReport := bugReport + #$D#$A#$D#$A + 'Exception Stack';
      bugReport := bugReport + #$D#$A#$D#$A + '----------------';
      for lIdx := 0 to ExceptionList.Count-1 do
        bugReport:= bugReport + #$D#$A + TExceptionInfo(ExceptionList[lIdx]).ClassName +
                                ' : ' + TExceptionInfo(ExceptionList[lIdx]).Msg;
    end;
  end;
end; // TfrmException.MadExceptionHandler
{$ENDIF}                        

//------------------------------------------------------------------------------
{ Set up the form appearance according to the exception occurring }
constructor TfrmException.Create(AOwner: TComponent;
  iException: Exception);
begin
  inherited Create(AOwner);
  { Set the dialog title }
  Caption := Application.Title;
  FExceptionStrings:=TStringList.Create;
  FException := iException;

  if not (iException is TExceptionPath) then NewExceptionList;

  { Format a critical error message to contain the application title }
  lblError.Caption := Format(MstErrorMessage, [Application.Title]);

  // Use Windows' own icon
  imgIcon.Picture.Icon.Handle := LoadIcon(0, IDI_WARNING);
  
  { Read the log file }
  ReadLogStrings;
  ClientHeight := bbOk.Top + bbOk.Height + 8;
  mmMoreInfo.Visible := False;
  lblmoreInfo.Visible := False;
end;  // TfrmException.Create

//------------------------------------------------------------------------------
{ A normal constructor - just to get rid of the annoying compiler warning really.
     This should never occur.  Use ShowMessage to prevent possibility of
     infinite loops (ie if the exception handling is bust, don't use it. }
constructor TfrmException.Create(Aowner: TComponent);
begin
  ShowMessage(ResStr_BadCall);
end;  // TfrmException.Create

//------------------------------------------------------------------------------
{ Find the first non-critical exception raised in the path.  This is the message
     the user should see.  A return result of nil indicates that the main
     exception (ie not one on the path list) must be used. }
class function TfrmException.GetFirstNonCritical: TExceptionInfo;
var i : integer;
begin
  Result := nil; // default value - indicates use the main exception
  if ExceptionList.Count > 0 then
  begin
    if not TExceptionInfo(ExceptionList[0]).Critical then
      for i := 0 to ExceptionList.Count-1 do
      begin
        { Set the result so we are ok if we never find a critical item }
        Result := ExceptionList[i];
        if TExceptionInfo(ExceptionList[i]).Critical then
        begin
          Result := ExceptionList[i-1];
          break; // from for loop - we have found our result
        end; // if Critical
      end; // for
  end;
end;  // TfrmException.GetFirstNonCritical

//------------------------------------------------------------------------------
{ Expand form to show exception path details }
procedure TfrmException.bbMoreClick(Sender: TObject);
begin
  ClientHeight := bbOk.Top + bbOk.Height + 120;
  lblMoreinfo.Visible := true;
  with mmMoreInfo do begin
    Visible := true;
    if FExceptionStrings <> nil then // just a safeguard
      Lines.Assign(FExceptionStrings);
    { Add some helpful info ! }
    Lines.Add('');
    Lines.Add(Format(ResStr_InfoSavedToFile, [FErrorFileName]));
    SelStart := 0;
    SelLength:= 0;
  end;
  bbMore.Enabled := False;
  BorderStyle := bsSizeable;
end;  // TfrmException.bbMoreClick

//------------------------------------------------------------------------------
{ Read the Exception list into a stringlist.  We can then save it to the
     incident report file.  Now also adds dlls loaded list. }
procedure TfrmException.ReadLogStrings;
var lIdx  : integer;
begin
  { Add metainfo }
  FExceptionStrings.Add(Format(ResStr_ExceptionOccuredInApp, [Application.Title, DateTimeToStr(now)]));
  FExceptionStrings.Add(Format(ResStr_Version,[GetFileVersion]));
  FExceptionStrings.Add('');

  FExceptionStrings.Add(ResStr_ExceptionPath);

  { Walk down the stack inserting each exception }
  if ExceptionList <> nil then
    for lIdx := 0 to ExceptionList.Count-1 do
    begin
      FExceptionStrings.Add( ExceptionItems[lIdx].ClassName + ' : ' +
                             ExceptionItems[lIdx].Msg );
    end;
  { Insert the current exception (most recent) at the end }
  FExceptionStrings.Add( FException.ClassName + ' : ' +
                                FException.Message );
  FExceptionStrings.Add('');
  { Add recent events }
  if mEventLog[0]<>'' then begin // got some events to log
    FExceptionStrings.Add(ResStr_LastEventAction);
    // find last events first
    for lIdx := mEventLogPosition to High(mEventLog) do
      if mEventLog[lIdx] <> '' then
        FExceptionStrings.Add('  ' +mEventLog[lIdx]);
    // now more recent events
    for lIdx := 0 to mEventLogPosition-1 do
      if mEventLog[lIdx] <> '' then
        FExceptionStrings.Add('  ' + mEventLog[lIdx]);
    FExceptionStrings.Add('');
  end;

  FExceptionStrings.Add(Format(ResStr_OSInfo,[GetOSInfo]));
  FExceptionStrings.Add(Format(ResStr_PhysicalMemInfo, [GetMemoryInfo]));
  FExceptionStrings.Add('');
  FExceptionStrings.Add(ResStr_DLLsLoaded);
  GetDllVersions(FExceptionStrings);

  SaveLogFile;
end;  // TfrmException.ReadLogStrings

//------------------------------------------------------------------------------
{ Destructor - just cleanup list }
destructor TfrmException.Destroy;
begin
  FExceptionStrings.Free; // it doesn't matter if it is nil
  inherited Destroy;
end;  // TfrmException.Destroy

//------------------------------------------------------------------------------
{ Accessor method to exception list items, saves us typecasting }
function TfrmException.GetExceptionInfo(iIndex: integer): TExceptionInfo;
begin
  Result := TExceptionInfo(ExceptionList[iIndex]);
end;  // TfrmException.GetExceptionInfo

//------------------------------------------------------------------------------
{ Locate the first (chronologically speaking) non-critical error, display it in
     a message box  }
class procedure TfrmException.ShowNonCriticalMessage(E: Exception);
var
  lMainExceptInfo: TExceptionInfo;
  lMessage: String;
begin
  lMainExceptInfo := GetFirstNonCritical;
  if not Assigned(lMainExceptInfo) then
    lMessage := E.Message
  else
    lMessage := lMainExceptInfo.Msg;
    
  MessageDlg(lMessage, mtInformation, [mbOk], 0);
end;  // TfrmException.SetExceptionMessage


//==============================================================================
{ Class procedure adds event information to the event log.  This allows our
    error analysis report to list previous occurrences.  We use a rolling stack
    for performance, so no data is shifted around }
class procedure TfrmException.LogEvent(const iEvent: string);
begin
  mEventLog[mEventLogPosition] := iEvent;
  Inc(mEventLogPosition);
  if mEventLogPosition > High(mEventLog) then
    // back to start of circle
    mEventLogPosition := 0;
end;


{-------------------------------------------------------------------------------
  Saves the error log to the specified location, application directory by default.
  If this is not available, try the temp folder.  Otherwise, give up and record a message
  in the file name.
}
procedure TfrmException.SaveLogFile;
begin
  try
    FErrorFileName := DefaultOutputPath + 'LastError.txt';
    FExceptionStrings.SaveToFile(FErrorFileName);
  except
    on Exception do
      try
        FErrorFileName := GetWindowsTempDir + 'LastError.txt';
        FExceptionStrings.SaveToFile(FErrorFileName);
      except
        on Exception do
          FErrorFileName := ResStr_FileSaveError;
      end; // try
  end;
end;


initialization
  ExceptionList := TList.Create;
  NonCriticalExceptions := TStringList.Create;
  // Default to application's location.
  DefaultOutputPath := ExtractFilePath(Application.Exename);

finalization
  TfrmException.FreeExceptionList;
  NonCriticalExceptions.Free;

end.
