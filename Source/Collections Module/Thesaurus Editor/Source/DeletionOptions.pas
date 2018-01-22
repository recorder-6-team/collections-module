{===============================================================================
  Unit:        DeletionOptions.pas

  Defines:     TdlgDeletionOptions

  Description: Options screen that allows the user to configure the behaviour
               of the Thesaurus Editor when deleting items.
               The settings in this dialog are saved to the user's registry.

  Model:       ThesaurusEditor.mpb

  Created:     September 2004

  Last revision information:
    $Revision: 3 $
    $Date: 15/10/04 17:47 $
    $Author: Ericsalmon $

===============================================================================}

unit DeletionOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InterfaceDataModule, StdCtrls, ExtCtrls, ImageListButton;

type
  TdlgDeletionOptions = class(TForm)
    btnCancel: TImageListButton;
    btnLog: TButton;
    btnOk: TImageListButton;
    chkLog: TCheckBox;
    chkSynchronise: TCheckBox;
    eLog: TEdit;
    Panel1: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkLogClick(Sender: TObject);
  private
    procedure EnableLogControls(AEnabled: Boolean);
    procedure LoadData;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
//==============================================================================
implementation

uses
  ThesaurusApplicationSettings, ResourceStrings;

{$R *.dfm}

{-==============================================================================
    TdlgDeletionOptions
===============================================================================}
{-------------------------------------------------------------------------------
  Load the data on the constructor. 
}
constructor TdlgDeletionOptions.Create(AOwner: TComponent);
begin
  inherited;
  LoadData;
end;  // TdlgDeletionOptions.Create 

{-------------------------------------------------------------------------------
  Handle a click on the Close button. Discard the changes. 
}
procedure TdlgDeletionOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;  // TdlgDeletionOptions.btnCancelClick 

{-------------------------------------------------------------------------------
  When btnLog is pressed, it brings up a dialog allowing the user to select the
          name of the file they want the script to be saved as.
}
procedure TdlgDeletionOptions.btnLogClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do begin
    try
      Title := ResStr_ScriptOutputFile;
      Filter := 'SQL Files (*.sql)|*.sql|All Files (*.*)|*.*';
      DefaultExt := 'sql';
      if Execute then eLog.Text := FileName;
    finally
      Free;
    end;
  end;
end;  // TdlgDeletionOptions.btnLogClick 

{-------------------------------------------------------------------------------
  Handle a click on the OK button. Save the data to the global variables in
          Thesaurus Application Settings.
}
procedure TdlgDeletionOptions.btnOkClick(Sender: TObject);
begin
  with ThesApplicationSettings do begin
    SyncTaxonDictDeletions  := chkSynchronise.Checked;
    LogDeletions            := chkLog.Checked;
    LogFilePath             := eLog.Text;
  end;
  Close;
end;  // TdlgDeletionOptions.btnOkClick 

{-------------------------------------------------------------------------------
  If chkLog is clicked, make sure eLog and btnLog are correctly
          enabled/disabled
}
procedure TdlgDeletionOptions.chkLogClick(Sender: TObject);
begin
  EnableLogControls(chkLog.Checked);
end;  // TdlgDeletionOptions.chkLogClick 

{-------------------------------------------------------------------------------
  Enable/disable btnLog and eLog. 
}
procedure TdlgDeletionOptions.EnableLogControls(AEnabled: Boolean);
begin
  btnLog.Enabled := AEnabled;
  eLog.Enabled   := AEnabled;
end;  // TdlgDeletionOptions.EnableLogControls 

{-------------------------------------------------------------------------------
  Populate the controls by loading the values from Thesaurus Application
          Settings.
}
procedure TdlgDeletionOptions.LoadData;
begin
  with ThesApplicationSettings do begin
    chkSynchronise.Checked := SyncTaxonDictDeletions;
    chkLog.Checked         := LogDeletions;
    eLog.Text              := LogFilePath;
  end;
  EnableLogControls(chkLog.Checked);
end;  // TdlgDeletionOptions.LoadData 

end.



