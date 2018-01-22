{===============================================================================
  Unit:        ThesaurusEditorMain

  Defines:     TfrmThesaurusEditor

  Description: Main MDI parent screen for the Thesaurus Editor

  Created:     July 2003

  Last revision information:
    $Revision: 43 $
    $Date: 9/06/11 14:26 $
    $Author: Jamesbichard $

===============================================================================}

unit ThesaurusEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BandActn, StdActns, ActnList, ActnCtrls, ToolWin, ActnMan, StdCtrls,
  ActnMenus, XPStyleActnCtrls, ImgList, ExtCtrls, ComCtrls, ActnColorMaps, DropTarget,
  ConceptOrganiser, ListOrganiser, MaintainRelationships, Menus, Registry,
  XPToolButton, XPMenu, DeletionOptions, DatabaseAccessADO, DatabaseOutput, DataClasses,
  LuxembourgConstants, RapTree, ListOrganiserNodes, BaseNavigatorFrame,
  ThesaurusNavigator, BaseMDIChildEditorUnit;

const
  WM_CHECKMDIBUTTONS = WM_APP+1;

type
  {-----------------------------------------------------------------------------
    Main application screen.  This is an MDI parent screen in which the MDI
    children are embedded that allow editing of thesaurus data.  This screen
    also controls the menu and toolbars available within the application.
    The Thesaurus Editor application is available to all users with Full Edit
    rights in Recorder or higher.  If Recorder is running, then the application
    validates itself on startup using the existing login to Recorder.  If
    Recorder is not running, then a login prompt is shown (see Recorder for
    specification of screen).
  }
  TfrmThesaurusEditor = class(TForm)
    actAbout: TAction;
    actConceptOrganiser: TAction;
    actCopy: TEditCopy;
    actCut: TEditCut;
    actDataEntryDeletionOptions: TAction;
    actDelete: TEditDelete;
    actExit: TFileExit;
    actExport: TAction;
    actExportToDict: TAction;
    actImportFromDict: TAction;
    actImportSpreadsheet: TAction;
    ActionList: TActionList;
    actListOrganiser: TAction;
    actMaintainRelationships: TAction;
    actMaintainSemanticRelationships: TAction;
    actPaste: TEditPaste;
    actSelectAll: TEditSelectAll;
    actUndo: TEditUndo;
    actWindowArrange: TWindowArrange;
    actWindowCascade: TWindowCascade;
    actWindowClose: TWindowClose;
    actWindowMinimizeAll: TWindowMinimizeAll;
    actWindowTileHorizontal: TWindowTileHorizontal;
    actWindowTileVertical: TWindowTileVertical;
    Arrange1: TMenuItem;
    Cascade1: TMenuItem;
    clbMain: TCoolBar;
    Close1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Delete1: TMenuItem;
    Export1: TMenuItem;
    ExportConceptGrouptoTaxonDictionary1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    ilMDI: TImageList;
    ilMenu: TImageList;
    ImportChecklistintoThesaurus1: TMenuItem;
    ImportChecklistintoThesaurus2: TMenuItem;
    MinimizeAll1: TMenuItem;
    mnuDataEntry: TMenuItem;
    mnuDataEntryConceptOrganiser: TMenuItem;
    mnuDataEntryListOrganiser: TMenuItem;
    mnuDataEntryMaintainRelationships: TMenuItem;
    mnuDataEntryMaintainSemanticRelationships: TMenuItem;
    mnuDeletionOptions: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuMain: TMainMenu;
    mnuWindow: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Paste1: TMenuItem;
    pnlMenuContainer: TPanel;
    SaveDialog: TSaveDialog;
    SelectAll1: TMenuItem;
    Status: TStatusBar;
    tbMDIButtons: TXPToolbar;
    tbMenu: TXPToolbar;
    tbtnConceptOrganiser: TXPToolbutton;
    tbtnCopy: TXPToolbutton;
    tbtnCut: TXPToolbutton;
    tbtnListOrganiser: TXPToolbutton;
    tbtnMaintainRelationships: TXPToolbutton;
    tbtnMaintainSemanticRelationships: TXPToolbutton;
    tbtnMDIClose: TXPToolbutton;
    tbtnMDIMinimise: TXPToolbutton;
    tbtnMDINormalise: TXPToolbutton;
    tbtnMnuDataEntry: TXPToolbutton;
    tbtnMnuEdit: TXPToolbutton;
    tbtnMnuFile: TXPToolbutton;
    tbtnMnuHelp: TXPToolbutton;
    tbtnMnuWindow: TXPToolbutton;
    tbtnPaste: TXPToolbutton;
    tbtnSep1: TXPToolbutton;
    tbtnSep2: TXPToolbutton;
    tlbMain: TXPToolbar;
    Undo1: TMenuItem;
    mnuDataEntryConceptGroupQualityChecker: TMenuItem;
    actDataEntryConceptGroupQualityChecker: TAction;
    actDataEntryMergeData: TAction;
    mnuDataEntryMergeDataItems: TMenuItem;
    mnuWindowSeparator: TMenuItem;
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDataEntryConceptOrganiserExecute(Sender: TObject);
    procedure actDataEntryDeletionOptionsExecute(Sender: TObject);
    procedure actDataEntryListOrganiserExecute(Sender: TObject);
    procedure actDataEntryRelationshipsExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actExportCGToDictExecute(Sender: TObject);
    procedure actFileExportThesaurusExecute(Sender: TObject);
    procedure actFileImportChklistToThesaurusExecute(Sender: TObject);
    procedure actFileImportSpreadsheetExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actMaintainSemanticRelationshipsExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure StatusResize(Sender: TObject);
    procedure tbtnMDICloseClick(Sender: TObject);
    procedure tbtnMDIMinimiseClick(Sender: TObject);
    procedure tbtnMDINormaliseClick(Sender: TObject);
    procedure actDataEntryConceptGroupQualityCheckerClick(Sender: TObject);
    procedure actDataEntryMergeDataExecute(Sender: TObject);
    procedure mnuWindowClick(Sender: TObject);
  private
    FProgressBar: TProgressBar;
    FXPMenu: TXPMenu;
    procedure WMCheckMDIButtons(var Msg:TMessage); message WM_CHECKMDIBUTTONS;
    function GetKeyFromActiveWindow(const ATableName: String): String;
  protected
    FListOrganiser: TfrmListOrganiser;
    FMaintainRelationships: TfrmMaintainRelationships;
    FMaintainSemanticRelationships: TfrmMaintainRelationships;
    procedure ConceptOrganiserClose(Sender: TObject; var Action: TCloseAction);
            virtual;
    procedure InitProgressBar; virtual;
    procedure ListOrganiserClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure MaintainRelationshipsClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure MaintainSemanticRelationshipsClose(Sender: TObject; var Action:
            TCloseAction); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetMDIButtonVisible(iShow: Boolean);
    procedure SetProgress(Progress, Total: Integer); virtual;
    procedure SetStatus(const AMessage: String; APrcMsgs: Boolean = True); virtual;
    property XPMenu: TXPMenu read FXPMenu;
  end;

var
  frmThesaurusEditor : TfrmThesaurusEditor;

//==============================================================================
implementation

uses
  About,
  ApplicationSettings, ImportSpreadSheet, ImportFromDict, ExportToDict,
  Math, GeneralData, Clipbrd, ThesaurusApplicationSettings,
  ConceptGroupQualityCheckerOptions, ConceptGroupQualityChecker, MergeData;

{$R *.dfm}

type
  TCustomEditHack = class(TCustomEdit)
  end;

{-==============================================================================
    TfrmThesaurusEditor
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmThesaurusEditor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  InitProgressBar;
  FXPMenu := TXPMenu.Create(Self);
  with FXPMenu do begin
    XPControls := [xcMainMenu, xcPopupMenu];
    Active := True;
  end;
end;  // TfrmThesaurusEditor.Create 

{-------------------------------------------------------------------------------
}
destructor TfrmThesaurusEditor.Destroy;
begin
  FProgressBar.Free;
  ThesApplicationSettings.Free;
  inherited;
end;  // TfrmThesaurusEditor.Destroy 

{-------------------------------------------------------------------------------
  Handles a click on Copy in the menu bar. 
}
procedure TfrmThesaurusEditor.actCopyExecute(Sender: TObject);
var
  lHandled: Boolean;
begin
  lHandled := False;
  if Assigned(ActiveMDIChild) then
    if ActiveMDIChild is TfrmConceptOrganiser then
      if Screen.ActiveControl = TfrmConceptOrganiser(ActiveMDIChild).
          fraThesaurusNavigator.tvHierarchy then begin
        TfrmConceptOrganiser(ActiveMDIChild).fraThesaurusNavigator.pmNodeCopyClick(nil);
        lHandled := True;
      end;
  if not lHandled then begin
    if Screen.ActiveControl is TCustomEdit then
      TCustomEdit(Screen.ActiveControl).CopyToClipboard;
  end;
end;  // TfrmThesaurusEditor.actCopyExecute 

{-------------------------------------------------------------------------------
  Handles a click on Cut in the menu bar. 
}
procedure TfrmThesaurusEditor.actCutExecute(Sender: TObject);
var
  lHandled: Boolean;
begin
  lHandled := False;
  if Assigned(ActiveMDIChild) then
    if ActiveMDIChild is TfrmConceptOrganiser then
    begin
      if Screen.ActiveControl = TfrmConceptOrganiser(ActiveMDIChild).
          fraThesaurusNavigator.tvHierarchy then begin
        TfrmConceptOrganiser(ActiveMDIChild).pmNodeCutClick(nil);
        lHandled := True;
      end;
    end else if ActiveMDIChild is TfrmListOrganiser then
      if Screen.ActiveControl = TfrmListOrganiser(ActiveMDIChild).
          fraListOrganiserTree.tvHierarchy then begin
        TfrmListOrganiser(ActiveMDIChild).fraListOrganiserTree.pmCutPasteCutOnClick(nil);
        lHandled := True;
      end;
  if not lHandled then begin
    if Screen.ActiveControl is TCustomEdit then
      TCustomEdit(Screen.ActiveControl).CutToClipboard;
  end;
end;  // TfrmThesaurusEditor.actCutExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actDataEntryConceptOrganiserExecute(Sender: TObject);
var
  lConceptOrganiser: TfrmConceptOrganiser;
begin
  lConceptOrganiser := TfrmConceptOrganiser.Create(self);
  FXPMenu.InitComponent(lConceptOrganiser);
  lConceptOrganiser.XPMenu := FXPMenu;
  lConceptOrganiser.OnClose := ConceptOrganiserClose;
  lConceptOrganiser.Show;
  actExport.Enabled := False;
end;  // TfrmThesaurusEditor.actDataEntryConceptOrganiserExecute

{-------------------------------------------------------------------------------
  Execute the action that will cause the Deletion Options frame to appear. If it currently exists, it will bring it to the front. If it doesn't, it will create a new one.
}
procedure TfrmThesaurusEditor.actDataEntryDeletionOptionsExecute(Sender: TObject);
begin
  with TdlgDeletionOptions.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // TfrmThesaurusEditor.actDataEntryDeletionOptionsExecute

{-------------------------------------------------------------------------------
  Displays the list organiser MDI child. 
}
procedure TfrmThesaurusEditor.actDataEntryListOrganiserExecute(Sender: TObject);
begin
  if Assigned(FListOrganiser) then
    FListOrganiser.BringToFront
  else
  begin
    FListOrganiser := TfrmListOrganiser.Create(Self);
    with FListOrganiser do begin
      OnClose      := ListOrganiserClose;
      ExportAction := actExport;
      Show;
    end;
    FXPMenu.InitComponent(FListOrganiser);
  end;
end;  // TfrmThesaurusEditor.actDataEntryListOrganiserExecute

{-------------------------------------------------------------------------------
  Displays the relationships maintenance MDI child.
}
procedure TfrmThesaurusEditor.actDataEntryRelationshipsExecute(Sender: TObject);
begin
  if Assigned(FMaintainRelationships) then
    FMaintainRelationships.BringToFront
  else
  begin
    FMaintainRelationships := TfrmMaintainRelationships.Create(Self);
    with FMaintainRelationships do begin
      IsSemanticRelation := False;
      PrepareCaption;
      PrepareListBox;
      OnClose := MaintainRelationshipsClose;
      Show;
    end;
    FXPMenu.InitComponent(FMaintainRelationships);
  end;
  actExport.Enabled := False;
end;  // TfrmThesaurusEditor.actDataEntryRelationshipsExecute

{-------------------------------------------------------------------------------
  Handles a click on Delete in the menu bar. 
}
procedure TfrmThesaurusEditor.actDeleteExecute(Sender: TObject);
begin
  if Screen.ActiveControl is TCustomEdit then
    with TCustomEditHack(Screen.ActiveControl) do
    begin
      if Enabled and not ReadOnly then SelText := '';
    end
  else if ActiveMDIChild is TfrmConceptOrganiser then
    with TfrmConceptOrganiser(ActiveMDIChild) do
      if Screen.ActiveControl = fraThesaurusNavigator.tvHierarchy then
        pmNodeDeleteClick(nil);
end;  // TfrmThesaurusEditor.actDeleteExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actExportCGToDictExecute(Sender: TObject);
var
  lConceptGroupKey: String;
  lDomainKey: String;
begin
  lConceptGroupKey := GetKeyFromActiveWindow('ConceptGroup');
  lDomainKey := GetKeyFromActiveWindow('Domain');

  with TdlgExportToDict.Create(nil) do
    try
      SetComboboxes(lDomainKey, lConceptGroupKey);
      ShowModal;
    finally
      Free;
    end;
end;  // TfrmThesaurusEditor.actExportCGToDictExecute

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actFileExportThesaurusExecute(Sender: TObject);
var
  lKeyList, lInvalidKeyList: TEditableKeyList;
  lDatabase: TdmDatabase;
  lDBOutput: TDatabaseOutput;
  lServerName, lDBName: String;
  lTrusted: Boolean;
begin
  // Initialise to NIL so can create inside TRY.
  lKeyList := nil;
  lInvalidKeyList := nil;
  lDatabase := nil;
  lDBOutput := nil;
  if ActiveMDIChild is TfrmListOrganiser then  // Safety, just in case.
    if SaveDialog.Execute then
      try
        lServerName := '';
        lDBName     := '';
        lTrusted    := False;
        // Need to get the correct settings for the connection string.
        with TRegistry.Create do
          try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKeyReadOnly(JNCC_REG_ROOT_PATH) then begin
              if ValueExists('Server Name') then begin
                lServerName := ReadString('Server Name');
                lDBName     := ReadString('Database Name');
                lTrusted    := ReadBool('Trusted Security');
              end;
              CloseKey;
            end;
          finally
            Free;
          end;

        lDatabase := TdmDatabase.Create(nil, lServerName, lDBName, lTrusted);
        lDBOutput := TDataBaseOutput.Create(lDatabase, SaveDialog.FileName, SetStatus, nil);
        lDBOutput.CanExportSystemSupplied := true;

        lDBOutput.UserAccessLevel := AppSettings.UserAccessLevel;

        lKeyList := TEditableKeyList.Create;
        with TBaseListOrganiserNode
            (FListOrganiser.fraListOrganiserTree.Selected) do begin
          lKeyList.SetTable(ClassTableName);
          lKeyList.AddItem(Key, ClassTableName);
        end;
        lInvalidKeyList := TEditableKeyList.Create;

        lDBOutput.Execute(lKeyList, lInvalidKeyList, False);
      finally
        lKeyList.Free;
        lInvalidKeyList.Free;
        lDBOutput.Free;
        lDatabase.Free;
      end;
  SetStatus('');
end;  // TfrmThesaurusEditor.actFileExportThesaurusExecute

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actFileImportChklistToThesaurusExecute(Sender:
        TObject);
begin
  with TdlgImportFromDict.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // TfrmThesaurusEditor.actFileImportChklistToThesaurusExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actFileImportSpreadsheetExecute(Sender: TObject);
begin
  with TdlgImportSpreadsheet.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;  // TfrmThesaurusEditor.actFileImportSpreadsheetExecute 

{-------------------------------------------------------------------------------
  Display the application about box. 
}
procedure TfrmThesaurusEditor.actHelpAboutExecute(Sender: TObject);
begin
  with TdlgAboutThesaurusEditor.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end; // try
end;  // TfrmThesaurusEditor.actHelpAboutExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actMaintainSemanticRelationshipsExecute(Sender:
        TObject);
begin
  if Assigned(FMaintainSemanticRelationships) then
    FMaintainSemanticRelationships.BringToFront
  else begin
    FMaintainSemanticRelationships := TfrmMaintainRelationships.Create(Self);
    with FMaintainSemanticRelationships do begin
      IsSemanticRelation := True;
      PrepareCaption;
      PrepareListBox;
      OnClose := MaintainSemanticRelationshipsClose;
      Show;
    end;
    FXPMenu.InitComponent(FMaintainSemanticRelationships);
  end;
  actExport.Enabled := False;
end;  // TfrmThesaurusEditor.actMaintainSemanticRelationshipsExecute

{-------------------------------------------------------------------------------
  Handles a click on Paste in the menu bar.
}
procedure TfrmThesaurusEditor.actPasteExecute(Sender: TObject);
var
  lHandled: Boolean;
begin
  lHandled := False;
  if Assigned(ActiveMDIChild) then
    if ActiveMDIChild is TfrmMergeData then
      TfrmMergeData(ActiveMDIChild).DoPaste
    else
    if ActiveMDIChild is TfrmConceptOrganiser then
    begin
      if Screen.ActiveControl = TfrmConceptOrganiser(ActiveMDIChild).
          fraThesaurusNavigator.tvHierarchy then
      begin
        TfrmConceptOrganiser(ActiveMDIChild).pmNodePasteClick(nil);
        lHandled := True;
      end;
    end else
    if ActiveMDIChild is TfrmListOrganiser then
      if Screen.ActiveControl = TfrmListOrganiser(ActiveMDIChild).
          fraListOrganiserTree.tvHierarchy then
      begin
        TfrmListOrganiser(ActiveMDIChild).fraListOrganiserTree.pmCutPastePasteOnClick(nil);
        lHandled := True;
      end;
  if not lHandled then begin
    if Screen.ActiveControl is TCustomEdit then
      TCustomEdit(Screen.ActiveControl).PasteFromClipboard;
  end;
end;  // TfrmThesaurusEditor.actPasteExecute 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.ConceptOrganiserClose(Sender: TObject; var
        Action: TCloseAction);
begin
  Action := caFree;
end;  // TfrmThesaurusEditor.ConceptOrganiserClose

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.FormResize(Sender: TObject);
begin
  pnlMenuContainer.Width := Width-17;
  // Also set width of menu bar band, so nothing else can go on the same line.
  clbMain.Bands.FindBand(pnlMenuContainer).MinWidth := pnlMenuContainer.Width;
end;  // TfrmThesaurusEditor.FormResize

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.InitProgressBar;
begin
  FProgressBar := TProgressBar.Create(nil);
  FProgressBar.Parent := Status;
  StatusResize(Status);
end;  // TfrmThesaurusEditor.InitProgressBar

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.ListOrganiserClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FListOrganiser := nil;
  actExport.Enabled := False;
end;  // TfrmThesaurusEditor.ListOrganiserClose

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.MaintainRelationshipsClose(Sender: TObject; var
        Action: TCloseAction);
begin
  Action := caFree;
  FMaintainRelationships := nil;
end;  // TfrmThesaurusEditor.MaintainRelationshipsClose

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.MaintainSemanticRelationshipsClose(Sender:
        TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FMaintainSemanticRelationships := nil;
end;  // TfrmThesaurusEditor.MaintainSemanticRelationshipsClose

{-------------------------------------------------------------------------------
  When the Edit menu option is clicked on the menu bar it needs to check what
          menu items should be enabled.
}
procedure TfrmThesaurusEditor.mnuEditClick(Sender: TObject);
var
  lHaveSelection: Boolean;
  lPasteEnabled: Boolean;
  lShowingHierarchy: Boolean;
begin
  lHaveSelection := False;
  lPasteEnabled := False;
  lShowingHierarchy := True;
  if Screen.ActiveControl is TCustomEdit then
  begin
    lHaveSelection := TCustomEdit(Screen.ActiveControl).SelLength > 0;
    lPasteEnabled := Clipboard.HasFormat(CF_TEXT);
  end else
  if ActiveMDIChild is TfrmMergeData then
    lPasteEnabled := Clipboard.HasFormat(CF_JNCCDATA)
  else
  if ActiveMDIChild is TfrmConceptOrganiser then
  begin
    with TfrmConceptOrganiser(ActiveMDIChild).fraThesaurusNavigator do
      if Screen.ActiveControl = tvHierarchy then
      begin
        lHaveSelection := Assigned(tvHierarchy.Selected);
        lPasteEnabled := lHaveSelection and PasteEnabled and ShowHierarchy;
        lShowingHierarchy := ShowHierarchy;
      end;
  end else if ActiveMDIChild is TfrmListOrganiser then
  begin
    with TfrmListOrganiser(ActiveMDIChild).fraListOrganiserTree do
      if Screen.ActiveControl = tvHierarchy then
      begin
        lHaveSelection := CanCut(tvHierarchy.Selected);
        lPasteEnabled := CanPaste(tvHierarchy.Selected);
      end;
  end;

  Cut1.Enabled    := lHaveSelection and lShowingHierarchy;
  Copy1.Enabled   := lHaveSelection and not (ActiveMDIChild is TfrmListOrganiser);
  Paste1.Enabled  := lPasteEnabled;
  Delete1.Enabled := lHaveSelection and lShowingHierarchy;
end;  // TfrmThesaurusEditor.mnuEditClick

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.SetMDIButtonVisible(iShow: Boolean);
begin
  if not (csDestroying in ComponentState) then begin
    tbMDIButtons.Visible := iShow;
    FormResize(nil); // Ensure buttons go to correct place.
  end;

  // Hide Window menu separator if no windows left.
  mnuWindowSeparator.Visible := MDIChildCount > 0;
end;  // TfrmThesaurusEditor.SetMDIButtonVisible

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.SetProgress(Progress, Total: Integer);
begin
  if (FProgressBar.Position <> Progress) or (FProgressBar.Max <> Total) then
  begin
    FProgressBar.Max := Total;
    FProgressBar.Position := Min(Progress, Total);
  end;
end;  // TfrmThesaurusEditor.SetProgress

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.SetStatus(const AMessage: String; APrcMsgs: Boolean = True);
begin
  if Status.Panels[0].Text <> AMessage then
  begin
    Status.Panels[0].Text := AMessage;
    Status.Refresh;
  end;
  if APrcMsgs then Application.ProcessMessages;
end;  // TfrmThesaurusEditor.SetStatus

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.StatusResize(Sender: TObject);
begin
  // code lifted as-is from the main form of Recorder
  Status.Panels[0].Width := Width div 4 * 3 - 20;  // 20 allows for handle
  Status.Panels[1].Width := Width div 4;
  { The following arbitrary numbers were chosen just to give a nice consistent
      border around the progress bar }
  FProgressBar.Left := Status.Left + Status.Panels[0].Width+4;
  FProgressBar.Width := Status.Panels[1].Width-5;
  FProgressBar.Height := Status.Height-6;
  FProgressBar.Top := 4;
end;  // TfrmThesaurusEditor.StatusResize 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.tbtnMDICloseClick(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ActiveMDIChild.Close;
  // allow form to go away before updating buttons
  PostMessage(Handle, WM_CHECKMDIBUTTONS, 0, 0);
end;  // TfrmThesaurusEditor.tbtnMDICloseClick 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.tbtnMDIMinimiseClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.WindowState := wsMinimized;
  SetMDIButtonVisible(False);
end;  // TfrmThesaurusEditor.tbtnMDIMinimiseClick 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.tbtnMDINormaliseClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.WindowState := wsNormal;
  SetMDIButtonVisible(False);
end;  // TfrmThesaurusEditor.tbtnMDINormaliseClick 

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.WMCheckMDIButtons(var Msg:TMessage);
begin
  { Hide MDI buttons if appropriate }
  if ActiveMDIChild = nil then
    SetMDIButtonVisible(False)
  else if ActiveMDIChild.WindowState <> wsMaximized then
    SetMDIButtonVisible(False);
end;  // TfrmThesaurusEditor.WMCheckMDIButtons 

{-------------------------------------------------------------------------------
  Opens a new Concept Group Quality Checker Options dialog, and presets the
  domain and/or concept group if the current active window is a list or
  concept organiser.
}
procedure TfrmThesaurusEditor.actDataEntryConceptGroupQualityCheckerClick(
            Sender: TObject);
var
  lConceptGroupKey: String;
  lDomainKey: String;
  lOwner: TComponent;
begin
  lConceptGroupKey := GetKeyFromActiveWindow('ConceptGroup');
  lDomainKey := GetKeyFromActiveWindow('Domain');

  lOwner := Self;

  with TdlgConceptGroupQualityCheckerOptions.Create(nil) do
    try
      SetComboboxes(lDomainKey, lConceptGroupKey);

      if ShowModal = mrOk then
        with TfrmConceptGroupQualityChecker.Create(
            lOwner,
            rgScanOptions.ItemIndex = 0,
            ConceptGroupKey,
            ConceptGroupSearchKey,
            '',
            MaxRecords,
            Null,
            false) do
          Show;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.actDataEntryMergeDataExecute(Sender: TObject);
begin
  with TfrmMergeData.Create(self) do Show;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmThesaurusEditor.mnuWindowClick(Sender: TObject);
var
  i, base: integer;
begin
  // Iterate only through the Child window items, copy all text after first space
  // in caption.

  base := mnuWindow.IndexOf(mnuWindowSeparator);
  for i := base + 1 to mnuWindow.Count - 1 do
    mnuWindow.Items[i].Caption :=
        '&'
        + IntToStr(i - base)
        + StringReplace(
            StringReplace(
                Copy(mnuWindow.Items[i].Caption, Pos(' ', mnuWindow.Items[i].Caption), 255),
                '&&', '&', []),
            '&', '&&', []);
end;

{-------------------------------------------------------------------------------
  Returns the key for the specified table from an active concept organiser or
  list organiser window
}
function TfrmThesaurusEditor.GetKeyFromActiveWindow(
  const ATableName: String): String;
begin
  if ActiveMDIChild is TfrmConceptOrganiser then
    Result := TfrmConceptOrganiser(ActiveMDIChild).GetKey(ATableName)
  else if ActiveMDIChild is TfrmListOrganiser then
    Result := TfrmListOrganiser(ActiveMDIChild).GetKey(ATableName);
end;

end.
