unit LicenceCheck;
{===============================================================================

   Copyright(c) Dorset Software Services 2001

   Created:       TLicence - Rob Kinsey 20/06/2001

   Updates:

   Packages:      Inhouse5 - Inhouse components for Delphi 5

   Description:   TLicence - Licensing component
                  (See individual notes for details)

===============================================================================}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  ELicenceError = class(Exception);
  EUsedAllLicencesError = class(Exception);

{-------------------------------------------------------------------------------
  ***   Licensing component  ***
  Name: TLicence
  Date Created: 20/06/2001
  Ownership: Dorset Software Services Ltd.
  Copyright Dorset Software Services (2001)

  Description:
    Component checks the number of licenced applications running and
  returns an error if more than the allowed number of licences has been exceeded.
  If not, then a file is created in the common folder to indicate another instance
  has been created, another licence in use.

  Usage:
    Drop component onto the main form of the application.
    Set the Location to be used for the licence locking files (LicenceDir)
    Set the number of copies of the application allowed (NumberOfLicences)
    Set whether to allow automatic Locking (AutoLock=True) or handle this yourself

  Author : Rob Kinsey (20/06/2001)
-------------------------------------------------------------------------------}
 TLicence = class(TComponent)
  private
    { Private declarations }
    FLicenceDir: string;
    FLicencesCount: integer;
    FAutoLock: boolean;
    FLocked: boolean;
    FLockFileName: string;
    FLockFileStream: TFileStream;
    function  GetLicenceDir: string;
    procedure SetLicenceDir(APath: string);
    function  GetLicencesCount: integer;
    procedure SetLicencesCount(ALicences: integer);
    function  GetAutoLock: boolean;
    procedure SetAutolock(ALock: boolean);
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    function Lock: boolean; virtual;
    procedure Unlock; virtual;
    destructor Destroy; override;
  published
    { Published declarations }
    property LicenceDir: string read GetLicenceDir write SetLicenceDir;
    property NumberOfLicences: integer read GetLicencesCount write SetLicencesCount;
    property AutoLock: boolean read GetAutolock write SetAutoLock stored True;
  end;

implementation

{ TLicence }
//==============================================================================
{ Ensures the licence is unlocked }
destructor TLicence.Destroy;
begin
  Unlock;
  inherited;
end; // Destroy
//==============================================================================
function TLicence.GetAutoLock: boolean;
begin
  Result := FAutoLock;
end; // GetAutoLock

//==============================================================================
function TLicence.GetLicenceDir: string;
begin
  Result := FLicenceDir;
end; // GetLicenceDir

//==============================================================================
function TLicence.GetLicencesCount: integer;
begin
  Result := FLicencesCount;
end; // GetLicencesCount

//==============================================================================
procedure TLicence.Loaded;
begin
  inherited;
  if AutoLock then Lock;
end; // Loaded

//==============================================================================
{ Locks a Licence for the current application instance.
    Conditions: * Only one Licence is allowed for each LicenceCheck component }
function TLicence.Lock: boolean;
var
  lCopy: integer;
  FProtoFileName: string;
begin
  Result := False;
  if FLocked then
    ELicenceError.Create('Licence already held by this instantiation of the application')
  else begin
    if FLicenceDir[Length(FLicenceDir)] = '\' then
      FProtoFileName := FLicenceDir + 'Licence'
    else
      FProtoFileName := FLicenceDir + '\Licence';
    for lCopy := 1 to FLicencesCount do begin
      if not FileExists(FProtoFileName + IntToStr(lCopy)) then begin
        try
          FLockFileName   := FProtoFileName + IntToStr(lCopy);
          FLockFileStream := TFileStream.Create(FLockFileName, fmCreate);
          FLocked := True;
          Result  := True;
          Break;  // from For loop: one lock file is enough
        except
          on E:Exception do
            ELicenceError.Create('Error creating Licence Locking File');
        end;  // try..except
      end;  // if not FileExists
    end;  // for
    if not Result then begin// Indicate all licences have been used up
      EUsedAllLicencesError.Create('All licences for this application are currently in use');
    end;
  end;  // if not FLocked
end;  // Lock

//==============================================================================
procedure TLicence.SetAutolock(ALock: boolean);
begin
  FAutoLock := ALock;
end; // SetAutolock

//==============================================================================
procedure TLicence.SetLicenceDir(APath: string);
begin
  FLicenceDir := APath;
end; // SetLicenceDir

//==============================================================================
procedure TLicence.SetLicencesCount(ALicences: integer);
begin
  FLicencesCount := ALicences;
end; // SetLicencesCount

//==============================================================================
{ Depending Unlocks the licence held by this instance of the application.
    Conditions: * Cannot be called in Autolock mode (except on destroy of LicenceCheck component)
                * Only deletes Lock file if a licence is currently locked. }
procedure TLicence.Unlock;
begin
  if (AutoLock and not (csDestroying in ComponentState )) then
    ELicenceError.Create('AutoLock mode in use, hence call to unlock licence not allowed')
  else if not FLocked then
    ELicenceError.Create('Application licence already unlocked for this instantiation')
  else begin
    try
      FLockFileStream.Free;
      if FileExists(FLockFileName) then
        if not DeleteFile(FLockFileName) then
          ELicenceError.Create('Could not delete Licence Locking File');
      FLocked := False;
    except on E:Exception do
      raise;
    end; // try..except
  end;
end; // Unlock

//==============================================================================

end.
