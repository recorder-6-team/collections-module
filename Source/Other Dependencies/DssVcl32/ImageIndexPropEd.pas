{===============================================================================
  Unit:        ImageIndexPropEd

  Defines:     TImageIndexPropertyEditor

  Description: Property editor for TImageIndex on custom components.
               Works if there is a "ImageList" property of type TCustomImageList.

  Packages:    InHouse7, Delphi 7 package for generic components.

  Created:     February 2004

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

  Copyright © Dorset Software Services Ltd, 2003

===============================================================================}

unit ImageIndexPropEd;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Dialogs, ActnList, ImgList,
  DesignIntf, DesignEditors, VCLEditors, ImageListButton;

type
  TImageIndexPropertyEditor = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: String; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: String; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: String; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

//==============================================================================
implementation

uses
  TypInfo;

{-------------------------------------------------------------------------------
}
function TImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;  // TImageIndexPropertyEditor.GetAttributes

{-------------------------------------------------------------------------------
}
function TImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  lComp: TPersistent;
  lImgList: TObject;
begin
  lComp := GetComponent(Index);

  lImgList := GetObjectProp(lComp, 'ImageList');
  if Assigned(lImgList) and (lImgList is TCustomImageList) then
    Result := TCustomImageList(lImgList)
  else
    Result := nil;
end;  // TImageIndexPropertyEditor.GetImageListAt

{-------------------------------------------------------------------------------
}
procedure TImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  lImgList: TCustomImageList;
  i: Integer;
begin
  lImgList := GetImageListAt(0);
  if Assigned(lImgList) then
    for i := 0 to lImgList.Count - 1 do
      Proc(IntToStr(i));
end;  // TImageIndexPropertyEditor.GetValues

{-------------------------------------------------------------------------------
}
procedure TImageIndexPropertyEditor.ListDrawValue(const Value: String;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  lImgList: TCustomImageList;
  lX: Integer;
begin
  lImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  lX := ARect.Left + 2;
  if Assigned(lImgList) then begin
    lImgList.Draw(ACanvas, lX, ARect.Top + 2, StrToInt(Value));
    Inc(lX, lImgList.Width);
  end;
  ACanvas.TextOut(lX + 3, ARect.Top + 1, Value);
end;  // TImageIndexPropertyEditor.ListDrawValue

{-------------------------------------------------------------------------------
}
procedure TImageIndexPropertyEditor.ListMeasureHeight(const Value: String;
  ACanvas: TCanvas; var AHeight: Integer);
var
  lImgList: TCustomImageList;
begin
  lImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(lImgList) and (lImgList.Height + 4 > AHeight) then
    AHeight := lImgList.Height + 4;
end;  // TImageIndexPropertyEditor.ListMeasureHeight

{-------------------------------------------------------------------------------
}
procedure TImageIndexPropertyEditor.ListMeasureWidth(const Value: String;
  ACanvas: TCanvas; var AWidth: Integer);
var
  lImgList: TCustomImageList;
begin
  lImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(lImgList) then
    Inc(AWidth, lImgList.Width);
end;  // TImageIndexPropertyEditor.ListMeasureWidth

end.
