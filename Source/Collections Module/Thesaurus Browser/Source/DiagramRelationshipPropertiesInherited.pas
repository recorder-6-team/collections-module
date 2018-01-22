{===============================================================================
  Unit:        DiagramRelationshipPropertiesInherited

  Defines:     TfraDiagramRelationshipPropertiesInherited

  Description: A base frame for editing the properties of relationships on the
               diagrams where the settings may be inherited from a higher level

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 13/04/04 11:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramRelationshipPropertiesInherited;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramRelationshipProperties, StdCtrls, Buttons, ComCtrls,
  ColorBtn, ExtCtrls, ThesaurusDiagramObjects, RestrictedEdits;

type
  {-----------------------------------------------------------------------------
    Specialisation of TfraDiagramRelationshipProperties for managing the
    properties of relationships or entire relationship types.  These share the
    common property that they are able to inherit the property settings from a
    higher level, for example a relationship type inherits its settings from
    the diagram unless the user specifies otherwise.
    Checkboxes are introduced to the class to allow the user to control which
    groups of settings are inherited from the parent settings and which are
    specified locally.  In addition a combo box is introduced allowing the user
    to select which specific item (relationship or relationship type) they are
    editing.  This allows the user to modify settings for multiple objects
    without closing the dialog.
  }
  TfraDiagramRelationshipPropertiesInherited = class(
      TfraDiagramRelationshipProperties)
    chkOverrideFont: TCheckBox;
    chkOverrideLabel: TCheckBox;
    chkOverrideLine: TCheckBox;
    cmbItem: TComboBox;
    Label1: TLabel;
    procedure chkOverrideFontClick(Sender: TObject);
    procedure chkOverrideLabelClick(Sender: TObject);
    procedure chkOverrideLineClick(Sender: TObject);
    procedure cmbItemDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
  protected
    procedure SetProperties(Value: TRelationshipDisplayProperties); override;
  public
    procedure ApplyChanges; override;
  end;
  
var
  fraDiagramRelationshipPropertiesInherited:
      TfraDiagramRelationshipPropertiesInherited;

implementation

uses InterfaceDataModule;

{$R *.dfm}

{-==============================================================================
    TfraDiagramRelationshipPropertiesInherited
===============================================================================}
{-------------------------------------------------------------------------------
  Apply changes.  Don't call the inherited method, as we don't want to apply
      changes to a parent display properties object.
}
procedure TfraDiagramRelationshipPropertiesInherited.ApplyChanges;
begin
  // skip standard behaviour of ApplyChanges as we need to only do so if not inherited
  if Dirty then
    with Properties do begin
      ParentFont := not chkOverrideFont.Checked;
      ParentPen := not chkOverrideLine.Checked;
      ParentLabelSettings := not chkOverrideLabel.Checked;
      if chkOverrideLine.Checked then begin
        Pen.Color := cbtnLineColour.ActiveColor;
        Pen.Width := StrToInt(eLineWeight.Text);
      end;
      if chkOverrideFont.Checked then
        Font.Assign(pnlFont.Font);
      ShowMainTerm := chkMainTermVisible.Checked;
      ShowForwardTerm := chkForwardTermVisible.Checked;
      ShowReverseTerm := chkReverseTermVisible.Checked;
    end; // with FProperties
  Dirty := False;
end;  // TfraDiagramRelationshipPropertiesInherited.ApplyChanges 

{-------------------------------------------------------------------------------
  Set controls to enabled or disabled depending on state of checkboxes to
      override parent settings.
}
procedure TfraDiagramRelationshipPropertiesInherited.chkOverrideFontClick(
    Sender: TObject);
begin
  inherited;
  btnChangeFont.Enabled := chkOverrideFont.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipPropertiesInherited.chkOverrideFontClick 

{-------------------------------------------------------------------------------
  Set controls to enabled or disabled depending on state of checkboxes to
      override parent settings.
}
procedure TfraDiagramRelationshipPropertiesInherited.chkOverrideLabelClick(
    Sender: TObject);
begin
  inherited;
  chkMainTermVisible.Enabled := chkOverrideLabel.Checked;
  chkForwardTermVisible.Enabled := chkOverrideLabel.Checked;
  chkReverseTermVisible.Enabled := chkOverrideLabel.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipPropertiesInherited.chkOverrideLabelClick 

{-------------------------------------------------------------------------------
  Set controls to enabled or disabled depending on state of checkboxes to
      override parent settings.
}
procedure TfraDiagramRelationshipPropertiesInherited.chkOverrideLineClick(
    Sender: TObject);
begin
  inherited;
  cbtnLineColour.Enabled := chkOverrideLine.Checked;
  eLineWeight.Enabled := chkOverrideLine.Checked;
  udLineWeight.Enabled := chkOverrideLine.Checked;
  Dirty := True;
end;  // TfraDiagramRelationshipPropertiesInherited.chkOverrideLineClick 

{-------------------------------------------------------------------------------
  Draw the item with correct term formatting. 
}
procedure TfraDiagramRelationshipPropertiesInherited.cmbItemDrawItem(Control:
    TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  inherited;
  dmInterface.DrawTerm(cmbItem.Canvas, Rect, cmbItem.Items[Index], odSelected
      in State);
end;  // TfraDiagramRelationshipPropertiesInherited.cmbItemDrawItem 

{-------------------------------------------------------------------------------
}
procedure TfraDiagramRelationshipPropertiesInherited.SetProperties(Value:
    TRelationshipDisplayProperties);
begin
  inherited SetProperties(Value);
  
  if Assigned(Value) then begin
    chkOverrideFont.Checked  := not Value.ParentFont;
    chkOverrideLine.Checked := not Value.ParentPen;
    chkOverrideLabel.Checked := not Value.ParentLabelSettings;
    chkOverrideLineClick(nil);
    chkOverrideFontClick(nil);
    chkOverrideLabelClick(nil);
  end;
  Dirty := False;
end;  // TfraDiagramRelationshipPropertiesInherited.SetProperties 



end.



