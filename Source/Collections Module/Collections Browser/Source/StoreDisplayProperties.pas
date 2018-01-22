{===============================================================================
  Unit:        StoreDisplayProperties.pas

  Defines:     TdlgStoreDisplayProperties

  Description: Display Properties for a store on a diagram

  Model:       StoreLayoutDiagram

  Created:     September 2004

  Last revision information:
    $Revision: 5 $
    $Date: 12/10/04 15:32 $
    $Author: Johnvanbreda $

===============================================================================}
unit StoreDisplayProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImageListButton, StdCtrls, RestrictedEdits, Buttons, ExtCtrls,
  ComCtrls, ColorBtn, StoreLayoutDiagram, Math;

type
  TdlgStoreDisplayProperties = class(TForm)
    Bevel5: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    btnApply: TImageListButton;
    btnCancel: TImageListButton;
    btnChangeFont: TBitBtn;
    btnOk: TImageListButton;
    cbtnFillColour: TColorButton;
    cbtnLineColour: TColorButton;
    eLineWeight: TNumberEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lblConceptLabelPreview: TLabel;
    lblWeight: TLabel;
    pnlFont: TPanel;
    pnlPreview: TPanel;
    shpConceptPreview: TShape;
    udLineWeight: TUpDown;
    procedure btnApplyClick(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbtnFillColourChange(Sender: TObject);
    procedure cbtnLineColourChange(Sender: TObject);
    procedure eLineWeightChange(Sender: TObject);
  private
    FDirty: Boolean;
    FDisplayProperties: TStoreDisplayProperties;
    procedure ApplyChanges;
    procedure SetDirty(const Value: Boolean);
    procedure SetDisplayProperties(const Value: TStoreDisplayProperties);
    procedure UpdatePreviewLabel;
    procedure Validate;
  public
    constructor Create(AOwner: TComponent); override;
    property Dirty: Boolean read FDirty write SetDirty;
    property DisplayProperties: TStoreDisplayProperties read FDisplayProperties write
        SetDisplayProperties;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  InterfaceDataModule, ResourceStrings, GeneralFunctions, ExceptionForm;

{-==============================================================================
    TdlgStoreDisplayProperties
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgStoreDisplayProperties.Create(AOwner: TComponent);
begin
  inherited;
  Dirty := False;
end;  // TdlgStoreDisplayProperties.Create 

{-------------------------------------------------------------------------------
  Applies the changes in the form to the settings object and repaints the diagram. 
}
procedure TdlgStoreDisplayProperties.ApplyChanges;
begin
  with FDisplayProperties do begin
    Font.Assign(pnlFont.Font);
    Pen.Color := cbtnLineColour.ActiveColor;
    Pen.Width := udLineWeight.Position;
    Brush.Color := cbtnFillColour.ActiveColor;
  end;
  Dirty := False;
end;  // TdlgStoreDisplayProperties.ApplyChanges 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.btnApplyClick(Sender: TObject);
begin
  Validate;
  ApplyChanges;
  TStoreLayoutDiagram(Owner).Invalidate;
end;  // TdlgStoreDisplayProperties.btnApplyClick 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.btnChangeFontClick(Sender: TObject);
  
  //var
  //  lParent: TWinControl;
  
begin
  with TFontDialog.Create(nil) do
    try
      Font.Assign(pnlFont.Font);
      MaxFontSize := 30;
      Options := Options + [fdLimitSize, fdAnsiOnly, fdForceFontExist];
      if Execute then begin
        pnlFont.Font.Assign(Font);
        pnlPreview.Font.Assign(Font);
        Dirty := True;
      end;
    finally
      Free;
    end; // try
end;  // TdlgStoreDisplayProperties.btnChangeFontClick 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.btnOkClick(Sender: TObject);
begin
  Validate;
  ApplyChanges;
  ModalResult := mrOk;
end;  // TdlgStoreDisplayProperties.btnOkClick 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.cbtnFillColourChange(Sender: TObject);
begin
  Dirty := True;
end;  // TdlgStoreDisplayProperties.cbtnFillColourChange 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.cbtnLineColourChange(Sender: TObject);
begin
  Dirty := True;
end;  // TdlgStoreDisplayProperties.cbtnLineColourChange 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.eLineWeightChange(Sender: TObject);
begin
  Dirty := True;
end;  // TdlgStoreDisplayProperties.eLineWeightChange 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.SetDirty(const Value: Boolean);
begin
  FDirty := Value;
  
  btnApply.Enabled := FDirty;
  UpdatePreviewLabel;
end;  // TdlgStoreDisplayProperties.SetDirty 

{-------------------------------------------------------------------------------
}
procedure TdlgStoreDisplayProperties.SetDisplayProperties(const Value:
    TStoreDisplayProperties);
begin
  FDisplayProperties := Value;
  
  with FDisplayProperties do begin
    pnlFont.Font.Assign(Font);
    cbtnLineColour.ActiveColor := Pen.Color;
    udLineWeight.Position := Pen.Width;
    cbtnFillColour.ActiveColor := Brush.Color;
  end;
end;  // TdlgStoreDisplayProperties.SetDisplayProperties 

{-------------------------------------------------------------------------------
    Updates the preview label font and position. 
}
procedure TdlgStoreDisplayProperties.UpdatePreviewLabel;
begin
  lblConceptLabelPreview.Font.Assign(pnlFont.Font);
  // realign the label
  lblConceptLabelPreview.Left := shpConceptPreview.Left +
    (shpConceptPreview.Width - lblConceptLabelPreview.Width) div 2;
  lblConceptLabelPreview.Top := shpConceptPreview.Top +
    (shpConceptPreview.Height - lblConceptLabelPreview.Height) div 2;
  with shpConceptPreview do begin
    // Ensure shape is big enough
    Left  := Min(lblConceptLabelPreview.Left-10, 91);
    Width := Max(lblConceptLabelPreview.Width+20, 133);
    // Setup colours/line widths
    Pen.Width := udLineWeight.Position;
    Pen.Color := cbtnLineColour.ActiveColor;
    Brush.Color := cbtnFillColour.ActiveColor;
    if Pen.Width=0 then
      Pen.Color := Brush.Color;
  end;
end;  // TdlgStoreDisplayProperties.UpdatePreviewLabel

{-------------------------------------------------------------------------------
  Checks that the forms details are valid before applying. 
}
procedure TdlgStoreDisplayProperties.Validate;
begin
  ValidateValue(IsInt(eLineWeight.Text), Format(ResStr_MustBeInteger,
      [LopColon(lblWeight.Caption)]));
end;  // TdlgStoreDisplayProperties.Validate 

end.





