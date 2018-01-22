{===============================================================================
  Unit:        BaseDiagramObjectProperties

  Defines:     TfraBaseDiagramObjectProperties

  Description: A frame for editing the properties of objects on the diagrams
               that are generic to all objects

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/04/04 11:58 $
    $Author: Johnvanbreda $

===============================================================================}
unit BaseDiagramObjectProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RestrictedEdits, ColorBtn, ExtCtrls, Buttons, ComCtrls,
  ThesaurusDiagram;

type
  {-----------------------------------------------------------------------------
    Base class for the properties pages that are used to configure the display 
    of objects on the diagram.  This component is never instantiated in its own 
    right, but contains components and functionality that are common to all the 
    properties pages.
  }
  TfraBaseDiagramObjectProperties = class (TFrame)
    Bevel5: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    btnChangeFont: TBitBtn;
    cbtnLineColour: TColorButton;
    eLineWeight: TNumberEdit;
    Label11: TLabel;
    Label12: TLabel;
    lblWeight: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lblInstructions: TLabel;
    pnlFont: TPanel;
    pnlPreview: TPanel;
    udLineWeight: TUpDown;
    procedure btnChangeFontClick(Sender: TObject);
    procedure cbtnLineColourChange(Sender: TObject);
    procedure eLineWeightChange(Sender: TObject);
  private
    FDiagram: TThesaurusDiagram;
    FDirty: Boolean;
    procedure SetDirty(Value: Boolean);
  protected
    procedure EnableControls(AEnabled: boolean); virtual;
    procedure SetDiagram(Value: TThesaurusDiagram); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    procedure ApplyChanges; virtual;
    procedure Validate; virtual;
    property Diagram: TThesaurusDiagram read FDiagram write SetDiagram;
    property Dirty: Boolean read FDirty write SetDirty;
  end;
  
implementation

uses
  ExceptionForm, GeneralFunctions, ResourceStrings;

{$R *.dfm}

function ActivateRecorder(AHandle: HWND; param: Pointer): BOOL; stdcall;
var
  lWindowCaption: array[0..255] of Char;
  lClassName : array[0..255] of char;
begin
  GetWindowText(AHandle, lWindowCaption,SizeOf(lWindowCaption));
  GetClassName(AHandle, lClassname, 255);
  if (Copy(lWindowCaption, 1, Length(Application.Title)) = Application.Title)
      and
      (lClassname = 'TfrmMain') then begin
    SetActiveWindow(AHandle);
    Result := False;
  end
  else
    Result := True;
end;

{-==============================================================================
    TfraBaseDiagramObjectProperties
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation. 
}
constructor TfraBaseDiagramObjectProperties.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  Dirty := False;
end;  // TfraBaseDiagramObjectProperties.Create 

{-------------------------------------------------------------------------------
  Applies any pending changes to the diagram.  Sets the FDirty flag to false. 
}
procedure TfraBaseDiagramObjectProperties.ApplyChanges;
begin
  Dirty := False;
end;  // TfraBaseDiagramObjectProperties.ApplyChanges 

{-------------------------------------------------------------------------------
  When the change font button is pressed, display a Font dialog and update the 
      settings if a selection is made.
}
procedure TfraBaseDiagramObjectProperties.btnChangeFontClick(Sender: TObject);
var
  lParent: TWinControl;
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
      { The following code fixes a problem with TCommonDialog, where it
           sometimes brings another app to the front! }
      // find Recorder and ensure it is active.
      EnumWindows(@ActivateRecorder, 0);
      // find the form we are on and force it to the front
      lParent := Self;
      while Assigned(lParent) do begin
        if lParent is TForm then begin
          SetForegroundWindow(lParent.Handle);
          Break; // from loop
        end;
        lParent := lParent.Parent;
      end;
    end; // try
end;  // TfraBaseDiagramObjectProperties.btnChangeFontClick 

{-------------------------------------------------------------------------------
}
procedure TfraBaseDiagramObjectProperties.cbtnLineColourChange(Sender: TObject);
begin
  Dirty := True;
end;  // TfraBaseDiagramObjectProperties.cbtnLineColourChange 

{-------------------------------------------------------------------------------
}
procedure TfraBaseDiagramObjectProperties.eLineWeightChange(Sender: TObject);
begin
  Dirty := True;
end;  // TfraBaseDiagramObjectProperties.eLineWeightChange

{-------------------------------------------------------------------------------
  Set the properties selection controls to enabled or disabled.  This occurs 
      when there is no selected item, for example. 
}
procedure TfraBaseDiagramObjectProperties.EnableControls(AEnabled: boolean);
begin
  btnChangeFont.Enabled := AEnabled;
  cbtnLineColour.Enabled := AEnabled;
  eLineWeight.Enabled := AEnabled;
  udLineWeight.Enabled := AEnabled;
end;  // TfraBaseDiagramObjectProperties.EnableControls 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TfraBaseDiagramObjectProperties.SetDiagram(Value: TThesaurusDiagram);
begin
  FDiagram := Value;
end;  // TfraBaseDiagramObjectProperties.SetDiagram 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TfraBaseDiagramObjectProperties.SetDirty(Value: Boolean);
begin
  FDirty := Value;
end;  // TfraBaseDiagramObjectProperties.SetDirty

{-------------------------------------------------------------------------------
  Validate that settings are OK before applying them. 
}
procedure TfraBaseDiagramObjectProperties.Validate;
begin
  ValidateValue(IsInt(eLineWeight.Text), Format(ResStr_MustBeInteger,
      [LopColon(lblWeight.Caption)]));
end;  // TfraBaseDiagramObjectProperties.Validate 

end.








