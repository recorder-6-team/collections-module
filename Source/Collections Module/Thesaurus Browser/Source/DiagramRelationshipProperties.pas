{===============================================================================
  Unit:        BaseDiagramObjectProperties

  Defines:     TfraBaseDiagramObjectProperties

  Description: A frame for editing the properties of objects on the diagrams
               that are generic to all objects

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 4 $
    $Date: 13/04/04 11:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDiagramObjectProperties, StdCtrls, RestrictedEdits, ColorBtn,
  ExtCtrls, Buttons, ComCtrls, ThesaurusDiagramObjects;

type
  {-----------------------------------------------------------------------------
    Specialisation of the TfraBaseDiagramObjectProperties screen specifically 
    for properties pages for relationships.  This class is also specialised 
    further for each of the relationship property page types.
    The class inherits controls from TfraBaseDiagramObjectProperties and adds 
    additional controls listed.
  }
  TfraDiagramRelationshipProperties = class (TfraBaseDiagramObjectProperties)
    Bevel6: TBevel;
    chkForwardTermVisible: TCheckBox;
    chkMainTermVisible: TCheckBox;
    chkReverseTermVisible: TCheckBox;
    Label14: TLabel;
    lblForwardTerm: TLabel;
    lblMainTerm: TLabel;
    lblReverseTerm: TLabel;
    shpBoxLeft: TShape;
    shpBoxRight: TShape;
    shpRelLine: TShape;
    procedure cbtnLineColourChange(Sender: TObject);
    procedure chkForwardTermVisibleClick(Sender: TObject);
    procedure chkMainTermVisibleClick(Sender: TObject);
    procedure chkReverseTermVisibleClick(Sender: TObject);
    procedure eLineWeightChange(Sender: TObject);
  private
    FProperties: TRelationshipDisplayProperties;
  protected
    procedure EnableControls(AEnabled: boolean); override;
    procedure SetProperties(Value: TRelationshipDisplayProperties); virtual;
  public
    procedure ApplyChanges; override;
    property Properties: TRelationshipDisplayProperties read FProperties write 
        SetProperties;
  end;
  
implementation

uses
  ResourceStrings, GeneralFunctions;

{$R *.dfm}

{-==============================================================================
    TfraDiagramRelationshipProperties
===============================================================================}
{-------------------------------------------------------------------------------
  Apply changes to the relationship display properties object 
}
procedure TfraDiagramRelationshipProperties.ApplyChanges;
begin
  if Dirty then
    with FProperties do begin
      Pen.Color := cbtnLineColour.ActiveColor;
      Pen.Width := StrToInt(eLineWeight.Text);
      Font.Assign(pnlFont.Font);
      ShowMainTerm := chkMainTermVisible.Checked;
      ShowForwardTerm := chkForwardTermVisible.Checked;
      ShowReverseTerm := chkReverseTermVisible.Checked;
    end; // with FProperties
  
  inherited ApplyChanges;
end;  // TfraDiagramRelationshipProperties.ApplyChanges 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramRelationshipProperties.cbtnLineColourChange(Sender: 
    TObject);
begin
  inherited;
  shpRelLine.Pen.Color := cbtnLineColour.ActiveColor;
  shpRelLine.Brush.Color := cbtnLineColour.ActiveColor;
  Dirty := True;
end;  // TfraDiagramRelationshipProperties.cbtnLineColourChange 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramRelationshipProperties.chkForwardTermVisibleClick(Sender: 
    TObject);
begin
  lblForwardTerm.Visible := chkForwardTermVisible.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipProperties.chkForwardTermVisibleClick 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramRelationshipProperties.chkMainTermVisibleClick(Sender: 
    TObject);
begin
  lblMainTerm.Visible := chkMainTermVisible.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipProperties.chkMainTermVisibleClick 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramRelationshipProperties.chkReverseTermVisibleClick(Sender: 
    TObject);
begin
  lblReverseTerm.Visible := chkReverseTermVisible.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipProperties.chkReverseTermVisibleClick 

{-------------------------------------------------------------------------------
  Update the preview and set the dirty flag. 
}
procedure TfraDiagramRelationshipProperties.eLineWeightChange(Sender: TObject);
begin
  if Assigned(FProperties) then begin
    if FProperties.Pen.Width <> udLineWeight.Position then
      inherited;
  end
  else
    inherited;
    // set thickness
  shpRelLine.Height := udLineWeight.Position;
  // and re-centre the line
  shpRelLine.Top := 24 - shpRelLine.Height div 2;
end;  // TfraDiagramRelationshipProperties.eLineWeightChange 

{-------------------------------------------------------------------------------
  Override EnableControls to enable relationship specific ones. 
}
procedure TfraDiagramRelationshipProperties.EnableControls(AEnabled: boolean);
begin
  inherited EnableControls(AEnabled);
  
  chkMainTermVisible.Enabled    := AEnabled;
  chkForwardTermVisible.Enabled := AEnabled;
  chkReverseTermVisible.Enabled := AEnabled;
end;  // TfraDiagramRelationshipProperties.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraDiagramRelationshipProperties.SetProperties(Value: 
    TRelationshipDisplayProperties);
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
        cbtnLineColour.ActiveColor := Pen.Color;
        eLineWeight.Text := IntToStr(Pen.Width);
        pnlFont.Font.Assign(Font);
        pnlPreview.Font.Assign(Font);
        chkMainTermVisible.Checked := ShowMainTerm;
        chkForwardTermVisible.Checked := ShowForwardTerm;
        chkReverseTermVisible.Checked := ShowReverseTerm;
      end; // with Value
    EnableControls(Assigned(Value));
    Dirty := False;
  
    FProperties := Value;
  
  end;
end;  // TfraDiagramRelationshipProperties.SetProperties 

end.


