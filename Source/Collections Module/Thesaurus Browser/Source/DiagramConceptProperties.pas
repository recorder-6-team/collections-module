{===============================================================================
  Unit:        DiagramConceptProperties

  Defines:     TfraDiagramConceptProperties

  Description: A base frame for editing the properties of concepts on the
               diagrams

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 6 $
    $Date: 13/04/04 11:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDiagramObjectProperties, StdCtrls, RestrictedEdits, ColorBtn,
  ExtCtrls, Buttons, ComCtrls, ThesaurusDiagramObjects, ThesaurusDiagram;

type
  {-----------------------------------------------------------------------------
    Specialisation of the TfraBaseDiagramObjectProperties screen specifically 
    for properties pages for concepts.  This class is also specialised further 
    for each of the concept property page types.
    The class inherits controls from TfraBaseDiagramObjectProperties and adds 
    additional controls listed.
  }
  TfraDiagramConceptProperties = class (TfraBaseDiagramObjectProperties)
    cbtnFillColour: TColorButton;
    Label1: TLabel;
    lblConceptLabelPreview: TLabel;
    shpConceptPreview: TShape;
    procedure btnChangeFontClick(Sender: TObject);
    procedure cbtnFillColourChange(Sender: TObject);
    procedure cbtnLineColourChange(Sender: TObject);
    procedure eLineWeightChange(Sender: TObject);
  private
    FProperties: TConceptDisplayProperties;
    procedure UpdatePreviewLabel;
  protected
    procedure EnableControls(AEnabled: boolean); override;
    procedure SetProperties(Value: TConceptDisplayProperties); virtual;
  public
    procedure ApplyChanges; override;
    property Properties: TConceptDisplayProperties read FProperties write 
        SetProperties;
  end;
  

implementation

uses
  ResourceStrings, GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TfraDiagramConceptProperties
===============================================================================}
{-------------------------------------------------------------------------------
  Apply changes to the concept display properties object 
}
procedure TfraDiagramConceptProperties.ApplyChanges;
begin
  if Dirty then
    with FProperties do begin
      Brush.Color := cbtnFillColour.ActiveColor;
      Pen.Color := cbtnLineColour.ActiveColor;
      Pen.Width := udLineWeight.Position;
      Font.Assign(lblConceptLabelPreview.Font);
    end; // with FProperties
  
  inherited ApplyChanges;
end;  // TfraDiagramConceptProperties.ApplyChanges 

{-------------------------------------------------------------------------------
  Update the preview, plus call inherited method. 
}
procedure TfraDiagramConceptProperties.btnChangeFontClick(Sender: TObject);
begin
  inherited;
  UpdatePreviewLabel;
end;  // TfraDiagramConceptProperties.btnChangeFontClick 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramConceptProperties.cbtnFillColourChange(Sender: TObject);
begin
  shpConceptPreview.Brush.Color := cbtnFillColour.ActiveColor;
  Dirty := True;
end;  // TfraDiagramConceptProperties.cbtnFillColourChange 

{-------------------------------------------------------------------------------
  Update the preview, plus call inherited method. 
}
procedure TfraDiagramConceptProperties.cbtnLineColourChange(Sender: TObject);
begin
  inherited;
  shpConceptPreview.Pen.Color := cbtnLineColour.ActiveColor;
end;  // TfraDiagramConceptProperties.cbtnLineColourChange 

{-------------------------------------------------------------------------------
  Update the preview, plus call inherited method. 
}
procedure TfraDiagramConceptProperties.eLineWeightChange(Sender: TObject);
begin
  if Assigned(FProperties) then begin
    if FProperties.Pen.Width <> udLineWeight.Position then
      inherited;
  end
  else
    inherited;
  shpConceptPreview.Pen.Width := udLineWeight.Position;
end;  // TfraDiagramConceptProperties.eLineWeightChange 

{-------------------------------------------------------------------------------
  Override enable controls to include concept specific ones. 
}
procedure TfraDiagramConceptProperties.EnableControls(AEnabled: boolean);
begin
  inherited EnableControls(AEnabled);
  
  cbtnFillColour.Enabled := AEnabled;
end;  // TfraDiagramConceptProperties.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDiagramConceptProperties.SetProperties(Value: 
    TConceptDisplayProperties);
begin
  if FProperties <> Value then
  begin
  
    // If we are linking to a new properties instance, and have an old one that
    // is not applied, then ask if the user wants to update it.
    if Assigned(FProperties) and Dirty then begin
      if MessageDlg(ResStr_PropertiesChangedUpdateConfirmation, mtConfirmation,
          [mbYes, mbNo], 0)=mrYes then begin
        ApplyChanges;
        Diagram.Invalidate;
      end;
    end;
    if Assigned(Value) then
      with Value do begin
        cbtnFillColour.ActiveColor := Brush.Color;
        cbtnLineColour.ActiveColor := Pen.Color;
        udLineWeight.Position := Pen.Width;
        pnlFont.Font.Assign(Font);
        UpdatePreviewLabel;
      end; // with Value
    EnableControls(Assigned(Value));
    Dirty := False;
  
    FProperties := Value;
  
  
  end;
end;  // TfraDiagramConceptProperties.SetProperties 

{-------------------------------------------------------------------------------
  Updates the preview label font and position. 
}
procedure TfraDiagramConceptProperties.UpdatePreviewLabel;
begin
  lblConceptLabelPreview.Font.Assign(pnlFont.Font);
  // realign the label
  lblConceptLabelPreview.Left := shpConceptPreview.Left +
    (shpConceptPreview.Width - lblConceptLabelPreview.Width) div 2;
  lblConceptLabelPreview.Top := shpConceptPreview.Top +
    (shpConceptPreview.Height - lblConceptLabelPreview.Height) div 2;
  // Ensure shape is big enough
  shpConceptPreview.Left  := Min(lblConceptLabelPreview.Left-10, 91);
  shpConceptPreview.Width := Max(lblConceptLabelPreview.Width+20, 133);
end;  // TfraDiagramConceptProperties.UpdatePreviewLabel 



end.








