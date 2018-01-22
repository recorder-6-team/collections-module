{===============================================================================
  Unit:        DiagramConceptPropertiesInherited

  Defines:     TfraDiagramConceptPropertiesInherited

  Description: Base display properties frame for a concept group or concept.

  Created:     Nov 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 5 $
    $Date: 13/04/04 11:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramConceptPropertiesInherited;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramConceptProperties, StdCtrls, Buttons, ComCtrls, ColorBtn,
  ExtCtrls, ThesaurusDiagramObjects, ComboListID, LuxIDComboBox,
  RestrictedEdits;

type
  {-----------------------------------------------------------------------------
    Specialisation of TfraDiagramConceptProperties for managing the properties
    of concepts or entire concept groups.  These share the common property that
    they are able to inherit the property settings from a higher level, for
    example a concept group inherits its settings from the diagram unless the
    user specifies otherwise.
    Checkboxes are introduced to the class to allow the user to control which
    groups of settings are inherited from the parent settings and which are
    specified locally.  In addition a combo box is introduced allowing the user
    to select which specific item (concept or concept group) they are editing.
    This allows the user to modify settings for multiple objects without
    closing the dialog.
  }
  TfraDiagramConceptPropertiesInherited = class(TfraDiagramConceptProperties)
    chkOverrideFont: TCheckBox;
    chkOverrideShape: TCheckBox;
    cmbItem: TComboBox;
    lblItem: TLabel;
    procedure chkOverrideFontClick(Sender: TObject);
    procedure chkOverrideShapeClick(Sender: TObject);
    procedure cmbItemDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
  protected
    procedure SetProperties(Value: TConceptDisplayProperties); override;
  public
    procedure ApplyChanges; override;
  end;
  

implementation

{$R *.dfm}

uses
  BaseDiagramObjectProperties, InterfaceDataModule;

{-==============================================================================
    TfraDiagramConceptPropertiesInherited
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDiagramConceptPropertiesInherited.ApplyChanges;
begin
  // skip standard behaviour of ApplyChanges as we need to only do so if not inherited
  if Assigned(Properties) then begin
    Properties.ParentFont := not chkOverrideFont.Checked;
    Properties.ParentBrush := not chkOverrideShape.Checked;
    Properties.ParentPen := not chkOverrideShape.Checked;
    if chkOverrideShape.Checked then begin
      Properties.Brush.Color := cbtnFillColour.ActiveColor;
      Properties.Pen.Color := cbtnLineColour.ActiveColor;
      Properties.Pen.Width := StrToInt(eLineWeight.Text);
    end;
    if chkOverrideFont.Checked then
      Properties.Font.Assign(pnlFont.Font);
  end;
  Dirty := False;
end;  // TfraDiagramConceptPropertiesInherited.ApplyChanges 

{-------------------------------------------------------------------------------
  Set controls to enabled or disabled depending on state of checkboxes to
      override parent settings.
}
procedure TfraDiagramConceptPropertiesInherited.chkOverrideFontClick(Sender:
    TObject);
begin
  inherited;
  btnChangeFont.Enabled := chkOverrideFont.Checked;
  Dirty := True;
end;  // TfraDiagramConceptPropertiesInherited.chkOverrideFontClick 

{-------------------------------------------------------------------------------
  Set controls to enabled or disabled depending on state of checkboxes to
      override parent settings.
}
procedure TfraDiagramConceptPropertiesInherited.chkOverrideShapeClick(Sender:
    TObject);
begin
  inherited;
  cbtnLineColour.Enabled := chkOverrideShape.Checked;
  eLineWeight.Enabled := chkOverrideShape.Checked;
  udLineWeight.Enabled := chkOverrideShape.Checked;
  cbtnFillColour.Enabled := chkOverrideShape.Checked;
  Dirty := True;
end;  // TfraDiagramConceptPropertiesInherited.chkOverrideShapeClick 

{-------------------------------------------------------------------------------
  Draw the combo item with correct term formatting. 
}
procedure TfraDiagramConceptPropertiesInherited.cmbItemDrawItem(Control:
    TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  inherited;
  dmInterface.DrawTerm(cmbItem.Canvas, Rect, cmbItem.Items[Index], odSelected
      in State);
end;  // TfraDiagramConceptPropertiesInherited.cmbItemDrawItem 

{-------------------------------------------------------------------------------
  Accessor method override - also sets the state of the override checkboxes
      according to the assigned properties object.
}
procedure TfraDiagramConceptPropertiesInherited.SetProperties(Value:
    TConceptDisplayProperties);
begin
  inherited SetProperties(Value);
  
  if Assigned(Value) then begin
    chkOverrideFont.Checked := not Value.ParentFont;
    chkOverrideShape.Checked := not (Value.ParentBrush and Value.ParentPen);
    chkOverrideShapeClick(nil);
    chkOverrideFontClick(nil);
  end;
  Dirty := False;
end;  // TfraDiagramConceptPropertiesInherited.SetProperties 


end.








