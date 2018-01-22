{===============================================================================
  Unit:        BaseMDIChildEditorUnit

  Defines:     TfrmBaseMDIChildEditor

  Description: Base form class for MDI editing windows in the Thesaurus editor

  Created:     June 2003

  Last revision information:
    $Revision: 14 $
    $Date: 24/01/07 11:02 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseMDIChildEditorUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImageListButton, DSSDataTypes, BaseDetailFrameUnit,
  InterfaceDataModule, ResourceStrings, BaseMDIChildUnit, XPMenu;

type
  {-----------------------------------------------------------------------------
    Base class for MDI child editing screens in the Thesaurus Browser.
    Provides Add, Edit and Delete button functionality.  The screen operates in
    2 modes, browse (default) and edit.
    If the user attempts to close the window while there are changed to the
    selected item that have not been saved, the user is prompted to save the
    changes and close, cancel the changes and close or abort the close
    operation.
  }
  TBaseMDIChildEditor = class(TBaseMDIChild)
    BrowserDetailsSplitter: TSplitter;
    btnAdd: TImageListButton;
    btnDelete: TImageListButton;
    btnEdit: TImageListButton;
    pnlBrowser: TPanel;
    pnlButtons: TPanel;
    pnlDetails: TPanel;
    pnlSaveButtons: TPanel;
    pnlSaveBtnsAlign: TPanel;
    btnCancel: TImageListButton;
    btnSave: TImageListButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FEditMode: TEditMode;
    FXPMenu: TXPMenu;
  protected
    FCurrentFrame: TBaseDetailFrame;
    function AskSaveBeforeClosing: Boolean; virtual;
    procedure DoAdd; virtual;
    procedure DoCancel; virtual;
    procedure DoDelete; virtual;
    procedure DoEdit; virtual;
    procedure DoSave; virtual;
    procedure SetEditMode(Value: TEditMode); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    function GetCurrentItemDescription: string; virtual; abstract;
    property CurrentFrame: TBaseDetailFrame read FCurrentFrame;
    property EditMode: TEditMode read FEditMode write SetEditMode;
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralFunctions, ThesaurusEditorMain;

{-==============================================================================
    TBaseMDIChildEditor
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseMDIChildEditor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  // initially no frame
  FCurrentFrame := nil;
  // Set the edit mode to Browse, so that the correct buttons are enabled/disabled.
  EditMode := emBrowse;
end;  // TBaseMDIChildEditor.Create 

{-------------------------------------------------------------------------------
  Virtual method that actually asks the user if they want to save any changes
          they may have made before the frame is closed. Validation needs to be
          called before the save.
  Returns True if the window can close. 
}
function TBaseMDIChildEditor.AskSaveBeforeClosing: Boolean;
var
  lResult: TModalResult;
begin
  Result := True; // default
  if Assigned(FCurrentFrame) then
    if FCurrentFrame.EditMode = emEdit then begin
      lResult :=  MessageDlg(ResStr_FrameToCloseInEditMode, mtWarning,
                                         [mbYes, mbNo, mbCancel], 0);
      if lResult = mrYes then begin
        FCurrentFrame.ValidateContent;
        FCurrentFrame.SaveContent;
      end
      else if lResult = mrCancel then
        Result := False;
    end;
end;  // TBaseMDIChildEditor.AskSaveBeforeClosing 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.btnAddClick(Sender: TObject);
begin
  DoAdd;
end;  // TBaseMDIChildEditor.btnAddClick 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.btnCancelClick(Sender: TObject);
begin
  DoCancel;
end;  // TBaseMDIChildEditor.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.btnDeleteClick(Sender: TObject);
begin
  DoDelete;
end;  // TBaseMDIChildEditor.btnDeleteClick 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.btnEditClick(Sender: TObject);
begin
  DoEdit;
end;  // TBaseMDIChildEditor.btnEditClick 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.btnSaveClick(Sender: TObject);
begin
  DoSave;
end;  // TBaseMDIChildEditor.btnSaveClick 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.DoAdd;
begin
  EditMode := emEdit;
  if Assigned(FCurrentFrame) then
    FCurrentFrame.EditMode := emEdit;
end;  // TBaseMDIChildEditor.DoAdd 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.DoCancel;
begin
  EditMode := emBrowse;
  if Assigned(FCurrentFrame) then begin
    FCurrentFrame.EditMode := emBrowse;
    FCurrentFrame.ReloadContent;
  end;
end;  // TBaseMDIChildEditor.DoCancel 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.DoDelete;
begin
  // No code needed here. If you want to make this abstract, some code would
  // need to be changed in the ListOrganiser.
end;  // TBaseMDIChildEditor.DoDelete 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.DoEdit;
begin
  EditMode := emEdit;
  if Assigned(FCurrentFrame) then
    FCurrentFrame.EditMode := emEdit;
end;  // TBaseMDIChildEditor.DoEdit 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.DoSave;
begin
  EditMode := emBrowse;
  if Assigned(FCurrentFrame) then
    FCurrentFrame.EditMode := emBrowse;
end;  // TBaseMDIChildEditor.DoSave 

{-------------------------------------------------------------------------------
  Event handler that is fired before the frame actually closes, to ask the user
          if they want to save their changes before closing.
}
procedure TBaseMDIChildEditor.FormCloseQuery(Sender: TObject; var CanClose:
        Boolean);
begin
  CanClose := AskSaveBeforeClosing;
end;  // TBaseMDIChildEditor.FormCloseQuery 

{-------------------------------------------------------------------------------
}
procedure TBaseMDIChildEditor.SetEditMode(Value: TEditMode);
begin
  FEditMode := Value;
  
  btnAdd.Enabled := Value=emBrowse;
  btnEdit.Enabled := Value=emBrowse;
  btnDelete.Enabled := Value=emBrowse;
  btnSave.Enabled := Value=emEdit;
  btnCancel.Enabled := Value=emEdit;
end;  // TBaseMDIChildEditor.SetEditMode 

end.
