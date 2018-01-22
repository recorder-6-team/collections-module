{===============================================================================
  Unit:        QEImport.pas

  Defines:     TdlgQEImport

  Description: Screen that allows the user to import data from a variety of
               sources into a quick entry session.

  Model:       -

  Created:     August 2004

  Last revision information:
    $Revision: 5 $
    $Date: 4/10/04 15:02 $
    $Author: Ericsalmon $

===============================================================================}
unit QEImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, QEImportDefinition, QEImportFrameUnit,
  QuickEntryFrame;

resourcestring
  ResStr_Finish = 'Finish';
  ResStr_Next = 'Next';
  ResStr_NoTemplateFields = 'The quick entry template does not define any'
      + ' specimen or occurrence fields.  Please try again with a different'
      + ' template.';

type
  TdlgQEImport = class(TForm)
    ScrollBox: TScrollBox;
    btnCancel: TImageListButton;
    btnNext: TImageListButton;
    procedure btnNextClick(Sender: TObject);
  protected
    FDefinition: TQEImportDefinition;
    FFrame: TQEImportFrame;
    procedure ChangePage(NewPageClass: TQEImportFrameClass); virtual;    
    procedure FrameChangeStatus(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent; SessionFrame: TfraQuickEntry);
        reintroduce;    
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
    function ShowModal: Integer; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  QEImportFileSelect;

{-==============================================================================
    TdlgQEImport
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgQEImport.Create(AOwner: TComponent; SessionFrame:
    TfraQuickEntry);
begin
  inherited Create(AOwner);
  FDefinition := TQEImportDefinition.Create(Self, Handle, SessionFrame);
  ChangePage(TfraQEImportFileSelect);
end;  // TdlgQEImport.Create 

{-------------------------------------------------------------------------------
}
destructor TdlgQEImport.Destroy;
begin
  inherited;
  // destroy definition *after* inherited destructor just in case the frame 
  // that we own wants to refer to the definition in its own destructor.
  FDefinition.Free;
end;  // TdlgQEImport.Destroy

{-------------------------------------------------------------------------------
}
procedure TdlgQEImport.btnNextClick(Sender: TObject);
begin
  FFrame.Validate;
  if FFrame.IsValid then
  begin
    if not Assigned(FFrame.Next) then
      ModalResult := mrOk
    else
      ChangePage(FFrame.Next);
  end;
end;  // TdlgQEImport.btnNextClick 

{-------------------------------------------------------------------------------
}
procedure TdlgQEImport.ChangePage(NewPageClass: TQEImportFrameClass);
begin
  if Assigned(FFrame) then
  begin
    FFrame.SaveContent;
    FreeAndNil(FFrame);
  end;
  FFrame := NewPageClass.Create(FDefinition);
  FFrame.OnChangeStatus := FrameChangeStatus;
  FFrame.Parent := ScrollBox;
  FFrame.Align := alClient;
  if FFrame.IsLast then
    btnNext.Caption := ResStr_Finish
  else
    btnNext.Caption := ResStr_Next;
  FFrame.LoadContent;
  FrameChangeStatus(FFrame);
end;  // TdlgQEImport.ChangePage 

{-------------------------------------------------------------------------------
}
function TdlgQEImport.CloseQuery: Boolean;
begin
  Result := True;
  
  if Assigned(FFrame) and (ModalResult = mrCancel) then
    if FFrame.AllowCancel then
      Result := FFrame.CancelQuery
    else
      Result := FFrame.IsLast and FFrame.IsComplete;

  if Result then 
    Result := inherited CloseQuery;
end;  // TdlgQEImport.CloseQuery 

{+------------------------------------------------------------------------------
  Update the enabled state of the Next/Finish and Cancel buttons according to
  the status of the current frame.
}
procedure TdlgQEImport.FrameChangeStatus(Sender: TObject);
begin
  btnNext.Enabled := FFrame.IsComplete;
  btnCancel.Enabled := FFrame.AllowCancel;
end;  // TdlgQEImport.FrameChangeStatus

{-------------------------------------------------------------------------------
}
function TdlgQEImport.ShowModal: Integer;
begin
  if FDefinition.SessionFieldCount > 0 then
    Result := inherited ShowModal
  else
  begin
    MessageDlg(ResStr_NoTemplateFields, mtWarning, [mbOk], 0);
    Result := mrCancel;
  end;  
end;  // TdlgQEImport.ShowModal

end.
