{===============================================================================
  Unit:        BarcodeFrame

  Defines:     TfraBarcode

  Description: Barcode selection

  Created:     September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 10/03/04 9:14 $
    $Author: Johnvanbreda $

===============================================================================}

unit BarcodeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseStepFrame, Barcode, ExtCtrls, StdCtrls, Contnrs;

type
  TfraBarcode = class (TfraBaseStep)
    Barcode: TBarcode;
    cmbBarcodeType: TComboBox;
    imgBarcode: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblNumbers: TLabel;
    lblPreview: TLabel;
    procedure cmbBarcodeTypeChange(Sender: TObject);
  private
    FBarcodeInfo: TObjectList;
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TStepFrameClass; override;
    function GetPrevious: TStepFrameClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  Settings, OrgDetailsFrame, LanguagesFrame;

type
  TBarcodeInfo = class (TObject)
    FIsNum: Boolean;
    FSample: String;
    FType: TBarcodeType;
  end;
  
{-==============================================================================
    TfraBarcode
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraBarcode.cmbBarcodeTypeChange(Sender: TObject);
begin
  inherited;
  Barcode.Left := 1;
  Barcode.Top := 5;
  with TBarcodeInfo(cmbBarcodeType.Items.Objects[cmbBarcodeType.ItemIndex]) do begin
    Barcode.Typ := FType;
    Barcode.Text := FSample;
    lblNumbers.Visible := FIsNum;
    if FIsNum then lblPreview.Caption := 'Preview (01234567):'
              else lblPreview.Caption := 'Preview (ABCD0123):';
  end;
  // Get rid of previous picture.
  imgBarcode.Picture := nil;
  // Draw new one
  Barcode.DrawBarcode(imgBarcode.Canvas);
end;  // TfraBarcode.cmbBarcodeTypeChange 

{-------------------------------------------------------------------------------
}
function TfraBarcode.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraBarcode.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraBarcode.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraBarcode.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraBarcode.GetNext: TStepFrameClass;
begin
  Result := TfraLanguages;
end;  // TfraBarcode.GetNext 

{-------------------------------------------------------------------------------
}
function TfraBarcode.GetPrevious: TStepFrameClass;
begin
  Result := TfraOrgDetails;
end;  // TfraBarcode.GetPrevious 

{-------------------------------------------------------------------------------
}
procedure TfraBarcode.LoadContent;
var
  i: TBarcodeType;
  lInfo: TBarcodeInfo;
begin
  inherited;
  FBarcodeInfo := TObjectList.Create;
  
  with cmbBarcodeType.Items do begin
    Clear;
    for i := Low(TBarcodeType) to High(TBarcodeType) do begin
      lInfo := TBarcodeInfo.Create;
      lInfo.FType := i;
      lInfo.FIsNum := BCData[i].Num;
      if BCData[i].Num then lInfo.FSample := '01234567'
                       else lInfo.FSample := 'ABCD0123';
      FBarcodeInfo.Add(lInfo);
      AddObject(BCData[i].Name, lInfo);
    end;
  end;
  cmbBarcodeType.ItemIndex := Settings.BarCodeFormat;
  cmbBarcodeTypeChange(nil);
end;  // TfraBarcode.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraBarcode.SaveContent;
begin
  inherited;
  FBarcodeInfo.Free;
  Settings.BarCodeFormat := cmbBarcodeType.ItemIndex;
end;  // TfraBarcode.SaveContent 

end.

