{===============================================================================
  Unit:        FramePeopleSpCollected

  Defines:     TfraPeopleSpCollected

  Description: Displays specimens collected by person.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 9 $
    $Date: 15/12/06 11:12 $
    $Author: Ericsalmon $

===============================================================================}
unit FramePeopleSpCollected;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ExtCtrls, RegisteredControls,
  DataTypes, LuxembourgConstants, ComboListID, DSSDataTypes;

type
  {-----------------------------------------------------------------------------
    Tab page control displaying a list of specimens collected by the selected person.  
  }
  TfraPeopleSpCollected = class(TBaseTabSheetFrame)
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

{-==============================================================================
    TfraPeopleSpCollected
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpCollected.EnableControls(AEnabled: Boolean);
begin
  inherited;
  
end;  // TfraPeopleSpCollected.EnableControls 

{-------------------------------------------------------------------------------
}
function TfraPeopleSpCollected.GetParams: TVariantArray;
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
end;  // TfraPeopleSpCollected.GetParams 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpCollected.lbSpecimensDblClick(Sender: TObject);
begin
  inherited;
  if EditMode = emBrowse then
    if Assigned(OnFrameNotification) then
          OnFrameNotification(Self, etNavigateSpecimen,
                       VarArrayOf(['Key', lbSpecimens.CurrentStrID]));
end;  // TfraPeopleSpCollected.lbSpecimensDblClick 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpCollected.LoadData;
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
end;  // TfraPeopleSpCollected.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpCollected.RegisterControls;
var
  lParams: TGetParamsFunction;
begin
  inherited;
  // This stored procedure needs to be passed specific parameters of its own.
  lParams := GetParams;
  // Register recordsets used
  RegisterRecordset('usp_SpecimensCollected_Select', lParams);
end;  // TfraPeopleSpCollected.RegisterControls 

{-------------------------------------------------------------------------------
}
procedure TfraPeopleSpCollected.SaveData;
begin
  inherited;
  // Do nothing. This method is to stop Abstract Errors occurring.
end;  // TfraPeopleSpCollected.SaveData 

end.

