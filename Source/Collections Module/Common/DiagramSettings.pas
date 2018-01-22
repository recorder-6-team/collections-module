{===============================================================================
  Unit:        DiagramSettings

  Defines:     TdlgDiagramSettings

  Description: Dialog allowing the general settings of a diagram (e.g. grid
               settings) to be configured.

  Model:       Thesaurus.mpb

  Created:     Nov 2003

  Last revision information:
    $Revision: 11 $
    $Date: 6/10/04 11:45 $
    $Author: Johnvanbreda $

===============================================================================}
unit DiagramSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ColorBtn, ComCtrls, ExtCtrls,
  DiagramObjects, RestrictedEdits, ExceptionForm;

resourcestring
  ResStr_MinimumGridSize = 'The minimum grid size allowed is 4 pixels';
  ResStr_GridSizeMustBeWholeNumber = 'The grid size must be specified as a whole number ' +
      'greater than or equal to 4.';

type
  EDiagramSettings = class(TExceptionPath);

  {-----------------------------------------------------------------------------
    Dialog allowing the title, background and grid settings for a diagram to be 
    configured.
  }
  TdlgDiagramSettings = class (TForm)
    btnApply: TImageListButton;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    pnlAll: TPanel;
    pnlBgrndAndGrid: TPanel;
    Label3: TLabel;
    cbtnBackground: TColorButton;
    chkDisplayGrid: TCheckBox;
    chkSnapToGrid: TCheckBox;
    eGridSize: TNumberEdit;
    udGridSize: TUpDown;
    Label1: TLabel;
    pnlTitle: TPanel;
    Label2: TLabel;
    chkDisplayTitle: TCheckBox;
    eTitle: TEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbtnBackgroundChange(Sender: TObject);
    procedure chkDisplayGridClick(Sender: TObject);
    procedure chkDisplayTitleClick(Sender: TObject);
    procedure chkSnapToGridClick(Sender: TObject);
    procedure eGridSizeChange(Sender: TObject);
    procedure eGridSizeExit(Sender: TObject);
    procedure eTitleChange(Sender: TObject);
  private
    FChangesMade: Boolean;
    FDiagramDisplayProperties: TDiagramDisplayProperties;
    procedure Validate;
  public
    constructor Create(AOwner: TComponent; AProperties: 
        TDiagramDisplayProperties); reintroduce;
    procedure HideTitle;
    property ChangesMade: Boolean read FChangesMade;
  end;
  

implementation

uses
  Diagram, GeneralFunctions;

{$R *.dfm}

const
  MIN_GRID_SIZE = 4;

{-==============================================================================
    TdlgDiagramSettings
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation 
}
constructor TdlgDiagramSettings.Create(AOwner: TComponent; AProperties: 
    TDiagramDisplayProperties);
begin
  inherited Create(AOwner);
  FDiagramDisplayProperties := AProperties;
  with FDiagramDisplayProperties do begin
    udGridSize.Position := GridSize;
    chkSnapToGrid.Checked := SnapToGrid;
    chkDisplayGrid.Checked := ShowGrid;
    cbtnBackground.ActiveColor := Color;
    chkDisplayTitle.Checked := ShowTitle;
    eTitle.Text := Title;
  end;
  FChangesMade := False;
end;  // TdlgDiagramSettings.Create 

{-------------------------------------------------------------------------------
  Clicking the apply button copies the settings into the diagram settings class 
      and invlidates the diagram.  This code is also called when the OK button 
      is clicked. 
}
procedure TdlgDiagramSettings.btnApplyClick(Sender: TObject);
begin
  Validate;
  with FDiagramDisplayProperties do begin
    GridSize := udGridSize.Position;
    SnapToGrid := chkSnapToGrid.Checked;
    ShowGrid := chkDisplayGrid.Checked;
    Color := cbtnBackground.ActiveColor;
    ShowTitle := chkDisplayTitle.Checked;
    Title := eTitle.Text;
    TDiagram(Diagram).Invalidate;
    TDiagram(Diagram).Dirty := True;
  end; // with FDiagramDisplayProperties
end;  // TdlgDiagramSettings.btnApplyClick 

{-------------------------------------------------------------------------------
  Closes the dialog and applies any pending changes to the diagram. 
}
procedure TdlgDiagramSettings.btnOkClick(Sender: TObject);
begin
  // Ensure changes are applied
  try
    btnApplyClick(Sender);
  except
    on Exception do begin
      ModalResult := mrNone;
      raise;
    end;
  end;
end;  // TdlgDiagramSettings.btnOkClick 

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.cbtnBackgroundChange(Sender: TObject);
begin
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.cbtnBackgroundChange 

{-------------------------------------------------------------------------------
  When the Show Grid Checkbox is unchecked, disable the relevant controls. 
}
procedure TdlgDiagramSettings.chkDisplayGridClick(Sender: TObject);
begin
  chkSnapToGrid.Enabled := chkDisplayGrid.Checked;
  eGridSize.Enabled     := chkDisplayGrid.Checked;
  udGridSize.Enabled    := chkDisplayGrid.Checked;
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.chkDisplayGridClick 

{-------------------------------------------------------------------------------
  The Title edit control is disabled when chkDisplayTitle is unchecked. 
}
procedure TdlgDiagramSettings.chkDisplayTitleClick(Sender: TObject);
begin
  eTitle.Enabled := chkDisplayTitle.Checked;
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.chkDisplayTitleClick 

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.chkSnapToGridClick(Sender: TObject);
begin
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.chkSnapToGridClick 

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.eGridSizeChange(Sender: TObject);
begin
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.eGridSizeChange 

{-------------------------------------------------------------------------------
  Validate that the grid size is not specified as below 4. 
}
procedure TdlgDiagramSettings.eGridSizeExit(Sender: TObject);
begin
  if IsInt(eGridSize.Text) then
    if StrToInt(eGridSize.Text)<MIN_GRID_SIZE then begin
      ShowInformation(ResStr_MinimumGridSize);
      eGridSize.Text := IntToStr(MIN_GRID_SIZE);
      udGridSize.Position := MIN_GRID_SIZE;
    end;
end;  // TdlgDiagramSettings.eGridSizeExit

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.eTitleChange(Sender: TObject);
begin
  btnApply.Enabled := True;
end;  // TdlgDiagramSettings.eTitleChange

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.HideTitle;
begin
  pnlTitle.Visible := False;
  btnOk.Top := btnOk.Top - pnlTitle.Height;
  btnCancel.Top := btnOk.Top;
  btnApply.Top := btnOk.Top;
  Height := Height - pnlTitle.Height;
  pnlAll.Height := pnlBgrndAndGrid.Height;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgDiagramSettings.Validate;
begin
  if not IsInt(eGridSize.Text) then
    raise EDiagramSettings.CreateValidation(ResStr_GridSizeMustBeWholeNumber,
        eGridSize);
end;

end.






