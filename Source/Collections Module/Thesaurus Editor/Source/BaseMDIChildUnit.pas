{===============================================================================
  Unit:        BaseMDIChildUnit

  Defines:     TfrmBaseMDIChild

  Description: Base form class for MDI windows in the Thesaurus editor

  Created:     January 2007

  Last revision information:
    $Revision: 3 $
    $Date: 12/03/07 11:52 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseMDIChildUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, InterfaceDataModule, ResourceStrings, Menus;

type
  {-----------------------------------------------------------------------------
    Base class for MDI child screens in the Thesaurus Browser.
  }
  TBaseMDIChild = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FWindowMenuItem: TMenuItem;
    procedure UpdateMDIButtons;
    procedure WindowMenuItemClick(Sender: TObject);
  protected
    procedure SetWindowMenuItemCaption(const ACaption: String);
    procedure UpdateControlsAlignment; virtual;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralFunctions, ThesaurusEditorMain;

{-==============================================================================
    TBaseMDIChild
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseMDIChild.Create(AOwner: TComponent);
begin
  inherited;

  FWindowMenuItem := TMenuItem.Create(Self);
  if Active then
    FWindowMenuItem.Checked := True;
  // Space is required to indicate where numbering should go
  FWindowMenuItem.Caption := ' ' + Caption;
  FWindowMenuItem.OnClick := WindowMenuItemClick;

  with frmThesaurusEditor do begin
    WindowMenu.Add(FWindowMenuItem);
    mnuWindowSeparator.Visible := True;
    XPMenu.ActivateMenuItem(FWindowMenuItem, False);
  end;
end;  // TBaseMDIChild.Create

{-------------------------------------------------------------------------------
}
destructor TBaseMDIChild.Destroy;
begin
  frmThesaurusEditor.SetMDIButtonVisible(False);
  inherited;
end;  // TBaseMDIChild.Destroy 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.FormActivate(Sender: TObject);
begin
  UpdateMDIButtons;
  // Check the appropriate menu item.
  if Assigned(FWindowMenuItem) then
    FWindowMenuItem.Checked := True;
end;  // TBaseMDIChild.FormActivate

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.FormClose(Sender: TObject; var Action:
        TCloseAction);
begin
  Action := caFree;
end;  // TBaseMDIChild.FormClose

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.FormDeactivate(Sender: TObject);
begin
  frmThesaurusEditor.SetMDIButtonVisible(False);
  // Check the appropriate menu item.
  if Assigned(FWindowMenuItem) then
    FWindowMenuItem.Checked := False;
end;  // TBaseMDIChild.FormDeactivate

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.FormResize(Sender: TObject);
begin
  UpdateMDIButtons;
end;  // TBaseMDIChild.FormResize

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.UpdateControlsAlignment;
begin
  // Nothing in base class.
end;  // TBaseMDIChild.UpdateControlsAlignment

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.SetWindowMenuItemCaption(const ACaption: String);
begin
  FWindowMenuItem.Caption := ' ' + ACaption;
end;  // TBaseMDIChild.SetWindowMenuItemCaption

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.UpdateMDIButtons;
begin
  frmThesaurusEditor.SetMDIButtonVisible((WindowState = wsMaximized) and
         not (csDestroying in ComponentState));
  // Have to wait for form to be properly loaded.
  if not (csLoading in ComponentState) then
    UpdateControlsAlignment;
end;  // TBaseMDIChild.UpdateMDIButtons

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.WindowMenuItemClick(Sender: TObject);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  BringToFront;
end;  // TBaseMDIChild.WindowMenuItemClick

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChild.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateMDIButtons;
end;  // TBaseMDIChild.WMSize

end.
