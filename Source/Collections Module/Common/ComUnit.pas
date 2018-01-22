unit ComUnit;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, Registry, Recorder2000_TLB, ExceptionForm, ImgList,
  ComObj, ActnList, OLETools, CommCtrl, Contnrs, SpatialRefFuncs, Constants;

resourcestring
  ResStr_BadAddins = 'The following addins could not be loaded for the specified reasons:'#13;
  ResStr_GoodAddins = 'The following addins loaded successfully:'#13;
  ResStr_ReportSavedIn = 'This report has been saved in %s';
  ResStr_ComRegistryProblem = 'A problem occurred opening registry keys for COM addins';
  ResStr_Version = 'Version';
  ResStr_Date = 'Date';
  ResStr_InvalidAddin = 'A Registered Add-in is not valid and cannot be used.';

type
  EComError = class(TExceptionPath);

  TComAddins = class(TObject)
  private
    FSpatialSystems: TStringList;
    FSpatialSystemInterfaces: TInterfaceList;
    procedure ReadComObjects;
    function AddNewComObject(iClsID: TGUID): string;
    procedure CheckSpatialReference(iComObject: IInterface);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  GUID_FilterResultToExcel : TGUID = '{1EEF0271-98BD-42DE-90DA-7888E95F5591}';

constructor TComAddins.Create;
begin
  inherited Create;
  FSpatialSystems := TStringList.Create;
  FSpatialSystemInterfaces := TInterfaceList.Create;
  ReadComObjects;
  SetCommAddinLink(FSpatialSystems, FSpatialSystemInterfaces);
end;

destructor TComAddins.Destroy;
begin
  FSpatialSystems.Free;
  FspatialSystems := nil;
  FSpatialSystemInterfaces.Free;
  FSpatialSystemInterfaces := nil;
end;

procedure TComAddins.ReadComObjects;
var
  lKeys : TStringList;
  i : Integer;
  lInstalled : boolean;
  lClsId : TGUID;
  lBadCOMs: TStringList;
  lGoodCOMs: TStringList;
  lResult: String;
begin
  { Read COM objects from the registry }
  lKeys := TStringList.Create;
  lBadCOMs := TStringList.Create;
  lGoodCOMs := TStringList.Create;
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly(REG_KEY_ADDIN);
      GetKeyNames(lKeys);  { Create }
      { Loop through the objects in regsitry }
      for i := 0 to lKeys.Count - 1 do
      begin
        CloseKey;
        if not OpenKeyReadOnly(REG_KEY_ADDIN + '\' + lKeys[i]) then
          raise EComError.Create(ResStr_ComRegistryProblem);
        lInstalled := (ReadString(REG_INSTALLED) = '1');

        { Check its installed.  If not, ignore it }
        if lInstalled then
        begin
          lClsID := StringToGuid(ReadString(REG_CLASS_ID));
          // filterresulttoexcel addin deprecated
          if not IsEqualGUID(lClsID, GUID_FilterResultToExcel) then
            try
              lGoodCOMs.Add(#9 + AddNewComObject(lClsID));
            except
              on E:Exception do
                lBadCOMs.Add(#9 + lKeys[i] + ': ' + E.Message);
            end;
        end;
      end;

      if lBadCOMs.Count > 0 then begin
        lResult := ResStr_BadAddins + lBadComs.Text;
        lResult := lResult + #13 + ResStr_GoodAddins;
        lResult := lResult + lGoodCOMs.Text;
      end;
    finally
      Free;
      lKeys.Free;
      lBadCOMs.Free;
      lGoodCOMs.Free;
    end;// try..finally
end;  // ReadComObjects

function TComAddins.AddNewComObject(iClsID: TGUID): string;
var
  lComObject : IUnknown;
  lRecorderAddin : IRecorderAddin;
begin
  Result := '';
  lComObject := CreateComObject(iClsID);
  if not Supports(lComObject, IRecorderAddin, lRecorderAddin) then
    Raise EComError.Create(ResStr_InvalidAddin);
  Result := lRecorderAddin.Name;
  CheckSpatialReference(lComObject);
end;  // AddNewComObject

procedure TComAddins.CheckSpatialReference(iComObject: IInterface);
var                                          
  i: Integer;
  lSpatialRefIntf: ISpatialReference;
  lSpatialRefListIntf: ISpatialReferenceList;
begin
  if Supports(iComObject, IID_ISpatialReference, lSpatialRefIntf) then begin
    // Store the 4 character ID
    FSpatialSystems.Add(lSpatialRefIntf.SpatialRefSystem);
    // and an interface pointer
    FSpatialSystemInterfaces.Add(lSpatialRefIntf);
  end
  else if Supports(iComObject, IID_ISpatialReferenceList, lSpatialRefListIntf) then begin
    for i:=0 to lSpatialRefListIntf.SystemCount-1 do begin
      // Store the 4 character ID
      FSpatialSystems.Add(lSpatialRefListIntf.SystemInterface[i].SpatialRefSystem);
      // and an interface pointer
      FSpatialSystemInterfaces.Add(lSpatialRefListIntf.SystemInterface[i]);
    end;
  end;
end;

end.
