unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmAbout = class(TForm)
    redModuleList: TRichEdit;
    lblAppVer: TLabel;
    lblAppDescription: TLabel;
    Button1: TButton;
    imgLogo: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TAboutInfo=class
  private
    FstAppName: AnsiString;
    FstAppDesc: AnsiString;
    FstAppVer : AnsiString;
    FstlModules : TStringList;
  public
    {global procedure for getting info}
  procedure ModuleRegister(stModule : AnsiString);
  constructor Create;
  destructor Destroy; override;

  property AppName : AnsiString read FstAppName write FstAppName;
  property AppVer : AnsiString read FstAppVer write FstAppVer;
  property AppDesc : AnsiString read FstAppDesc write FstAppDesc;
  property ModuleList : TStringList read FstlModules;

  end;


var
  frmAbout: TfrmAbout;
  AboutInfo : TAboutInfo;


implementation

{$R *.DFM}

const

UNIT_NAME = 'About';
VERSION_STRING='0.1';

{------------------------------------------------------------------------------}
{TABoutInfo}
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TAboutInfo.ModuleRegister(stModule : AnsiString);
begin
     FstlModules.Add(stModule);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TAboutInfo.Create;
begin
     inherited Create;
     FstlModules:=TStringList.Create;
end;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TAboutInfo.Destroy;
begin
     FstlModules.Free;
     inherited Destroy;
end;



{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TfrmAbout.Button1Click(Sender: TObject);
begin
Close;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TfrmAbout.FormActivate(Sender: TObject);
begin
     Caption:=AboutInfo.AppName;
     lblAppVer.Caption:='Version : '+AboutInfo.AppVer;
     lblAppDescription.Caption:=AboutInfo.AppDesc;
     redModuleList.Lines.Assign(AboutInfo.ModuleList);
     redModuleList.SelStart:=0;
end;

initialization
AboutInfo:=TAboutInfo.Create;
AboutInfo.ModuleRegister(UNIT_NAME+' :'+VERSION_STRING);

finalization
AboutInfo.Free;

end.
