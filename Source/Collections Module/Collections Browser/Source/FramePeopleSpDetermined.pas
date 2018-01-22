{===============================================================================
  Unit:        FramePeopleSpDetermined

  Defines:     TfraPeopleSpDetermined

  Description: Displays specimens determined by person.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 9 $
    $Date: 15/12/06 11:12 $
    $Author: Ericsalmon $

===============================================================================}

unit FramePeopleSpDetermined;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, DataTypes, DSSDataTypes,
  RegisteredControls, LuxembourgConstants, ComboListID;

type
  {-----------------------------------------------------------------------------
    Tab page control displaying a list of specimens determined by the selected person.  
  }
  TfraPeopleSpDetermined = class(TBaseTabSheetFrame)
    lbSpecimens: TIDListBox;
    procedure lbSpecimensDblClick(Sender: TObject);
  private
    FTopNodeContext: TNodeContext;
    function GetParams: TVariantArray;
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  UserMessages, BaseDetailFrameUnit;

{-==============================================================================
    TfraPeopleSpDetermined
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpDetermined.EnableControls(AEnabled: Boolean);
begin
  inherited;
  
end;  // TfraPeopleSpDetermined.EnableControls 

{-------------------------------------------------------------------------------
}
function TfraPeopleSpDetermined.GetParams: TVariantArray;
var
  lKeyIsSpecimen: Boolean;
begin
  FTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);
  
  if FTopNodeContext = ncSpecimen then
    lKeyIsSpecimen := True
  else
    lKeyIsSpecimen := False;
  
  Result := VarArrayOf(['@Key', Key,
                        '@CollectionUnitKey',ParentKey,
                        '@KeyIsSpecimen', lKeyIsSpecimen]);
end;  // TfraPeopleSpDetermined.GetParams 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpDetermined.lbSpecimensDblClick(Sender: TObject);
begin
  inherited;
  if EditMode = emBrowse then
    if Assigned(OnFrameNotification) then
          OnFrameNotification(Self, etNavigateSpecimen,
                        VarArrayOf(['Key', lbSpecimens.CurrentStrID]));
end;  // TfraPeopleSpDetermined.lbSpecimensDblClick 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpDetermined.LoadData;
begin
  inherited;
  
  // The list box needs to be cleared each time it is populated.
  lbSpecimens.Clear;
  
  if RegisteredRecordsets[0].RecordCount<>0 then begin
    RegisteredRecordsets[0].MoveFirst;
    while not RegisteredRecordsets[0].EOF do begin
      lbSpecimens.Add(VarToStr(RegisteredRecordsets[0].Fields['Item_Name'].Value),
                      VarToStr(RegisteredRecordsets[0].Fields['Item_Key'].Value)
                      );
      RegisteredRecordsets[0].MoveNext;
    end;
  end;
end;  // TfraPeopleSpDetermined.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpDetermined.RegisterControls;
var
  lParams: TGetParamsFunction;
begin
  inherited;
  // This stored procedure needs to be passed specific parameters of its own.
  lParams := GetParams;
  // Register recordsets used
  RegisterRecordset('usp_SpecimensDetermined_Select', lParams);
end;  // TfraPeopleSpDetermined.RegisterControls 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpDetermined.SaveData;
begin
  inherited;
  // Do nothing. This method is to stop Abstract Errors occurring.
end;  // TfraPeopleSpDetermined.SaveData 

end.
