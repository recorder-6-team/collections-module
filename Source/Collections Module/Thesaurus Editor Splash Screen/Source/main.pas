unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

const
  WM_RUNAPP = WM_APP + 10;

type
  TfrmMain = class(TForm)
    lblVersion: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    FHDC: HDC;
    FBitmap: TBitmap;
    FClosing : boolean;
    FAppName: string;
    procedure Paint; override;
    function BuildRgnFromFile(theFile: String; var Rgn: HRGN): Boolean;
    function CreateRgnFromBitmap(ABmp: TBitMap; ABackColor: TColor): HRGN;
    function SaveRgnToFile(theFile: String; Rgn: HRGN): Boolean;
    procedure LoadIniFile;
    procedure RunApp(var Message: TMessage); message WM_RUNAPP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses GeneralFunctions, VersionInfo, ErrorMessages;

{$R *.dfm}

const
  AE_OK = 0;
  AE_CORRUPT= 1;
  AE_BAD_REGION = 2;
  AE_EMPTY = 3;

var
  errorcode: integer;

function TfrmMain.SaveRgnToFile(theFile: String; Rgn : HRGN):Boolean;
var
  fs: TFileStream;
  theSize: integer;
  blah: integer;
  theData: PRGNDATA;
begin
  Result := True;
  theSize := GetRegionData(Rgn,1,nil);
  GetMem(theData,theSize);
  Blah := GetRegionData(Rgn,theSize,theData);
  fs := TFileStream.Create(theFile, fmCreate);
  try
    fs.Write(theData^,theSize);
    FreeMem(theData,theSize);
    fs.Free;
  except
    fs.Free;
    FreeMem(theData,theSize);
    Result:=False;
  end;
end;


function TfrmMain.BuildRgnFromFile(theFile: String; var Rgn: HRGN ):Boolean;
var
  fs: TFileStream;
  theSize: integer;
  blah: integer;
  theData: PRGNDATA;
begin
  try
    fs := TFileStream.Create(theFile, fmOpenRead);
    if fs.Size > 0 then
    begin
      theSize:=fs.Size;
      GetMem(theData,theSize);
      fs.Position:=0;
      blah := fs.Read(theData^,theSize);
      Rgn := (ExtCreateRegion (NIL,theSize,theData^));
      if Rgn > 0 then
      begin
        errorCode := AE_OK;
        Result := True;
      end else begin
        errorCode := AE_BAD_REGION;
        Result := False;
      end;
      FreeMem(theData);
    end else
    begin
      Result := False;
      errorCode := AE_EMPTY;
    end;
    fs.Free;
  except
    Result := False;
    errorCode := AE_CORRUPT;
    fs.Free;
    if theSize > 0 then
      FreeMem(theData);
  end;
end;


function TfrmMain.CreateRgnFromBitmap(ABmp: TBitMap; ABackColor: TColor): HRGN;
var
  lR1: HRGN;
  x, y, lRes: Integer;
  lbrgn: Boolean;
begin
 Result := 0;
 lbrgn := False;
 for y := 0 to ABmp.Height - 1 do
   for x := 0 to ABmp.Width - 1 do begin
     if ABmp.Canvas.Pixels[x, y] <> ABackColor then begin
       if lbrgn then begin
         lR1  := CreateRECTRgn(x ,y, x + 1, y + 1);
         lRes := CombineRgn(Result, Result, lR1, RGN_OR);
         DeleteObject(lR1);
         if lRes = ERROR then
           raise Exception.CreateFmt('Error [%d,%d]', [x, y]);
       end else begin
         Result := CreateRECTRgn(x ,y, x + 1, y + 1);
         lbrgn  := True;
       end;
     end;
   end;
  SaveRgnToFile(ExtractFilePath(Application.Exename) + 'splash.rgn', Result);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ReleaseDC(Handle, FHDC);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FClosing := True;
  while AlphaBlendValue>0 do begin
    AlphaBlendValue := Max(AlphaBlendValue - 40, 0);
    Sleep(10);
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
var
  lR: HRgn;
begin
  inherited;

  FClosing := False;
  FBitmap := TBitmap.Create;
  FHDC    := GetWindowDC(Handle);
  FBitmap.LoadFromFile(ExtractFilePath(Application.exename) + 'Splash.bmp');
  Width  := FBitmap.Width;
  Height := FBitmap.Height;
  if not BuildRgnFromFile(ExtractFilePath(Application.Exename) + 'splash.rgn', LR) then
    ShowMessage(IntToStr(errorcode));
  SetWindowRgn(Handle, lR, True);
  DeleteObject(lR);
  LoadIniFile;
end;


procedure TfrmMain.LoadIniFile;
var
  lIniFile: TStringList;
begin
  lIniFile := TStringList.Create;
  try
    with lIniFile do begin
      LoadFromFile(ExtractFilePath(Application.Exename) + 'splash.ini');
      if IndexOf('[Version]')<>-1 then begin
        lblVersion.Font.Size := StrToInt(Values['FontSize']);
        lblVersion.Font.Name := Values['FontName'];
        lblVersion.Left := StrToInt(Values['Left']);
        lblVersion.Top := StrToInt(Values['Top']);
        if IndexOfName('Color')>-1 then
          lblVersion.Font.Color := StrToInt(Values['Color']);
        if IndexOfName('VersionPrefix')>-1 then
          lblVersion.Caption := Values['VersionPrefix']
        else
          lblVersion.Caption := '';
        FAppName := ExtractFilePath(Application.Exename) + Values['Name'];
        lblVersion.Caption := lblVersion.Caption + GetFileVersion(FAppName);
      end else
        lblVersion.Visible := False;
    end;
  finally
    lIniFile.Free;
  end; // try
end;

destructor TfrmMain.Destroy;
begin

  inherited;
end;

procedure TfrmMain.Paint;
begin
  Win32Check(BitBlt(FHDC, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY));
  if not FClosing then
    AlphaBlendValue := 255;
end;

procedure TfrmMain.RunApp(var Message: TMessage);
var
  lStartStatus: integer;
begin
  // Ensure we are painted properly before starting to load the app
  Application.ProcessMessages;
  { Execute the main application }
  lStartStatus := WinExec(PChar(FAppName + ' /' + IntToStr(Handle) ),
                   SW_SHOW );
  { Check it ran OK }
  case lStartStatus of // trap errors
    0 :
        MessageDlg(EST_OUT_OF_RESOURCES, mtError, [mbOk], 0);
    ERROR_FILE_NOT_FOUND :
        MessageDlg(EST_EXEFILE_MISSING, mtError, [mbOk], 0);
    ERROR_BAD_FORMAT, ERROR_PATH_NOT_FOUND :
        MessageDlg(EST_MAINFILE_PROBLEM, mtError, [mbOk], 0);
  end; // case
  if lStartStatus in [0, ERROR_FILE_NOT_FOUND, ERROR_BAD_FORMAT, ERROR_PATH_NOT_FOUND] then
    Application.Terminate;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_RUNAPP, 0, 0);
end;

end.
