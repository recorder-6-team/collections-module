{===============================================================================
  Unit:        ConceptGeneral

  Defines:     TfraConceptGeneral

  Description: General tab page for a term

  Created:     June 2003

  Last revision information:
    $Revision: 42 $
    $Date: 5/11/12 8:36 $
    $Author: Alexanderpadley $

===============================================================================}

unit ConceptGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, ToolWin, BaseTabSheetFrameUnit,
  ExtCtrls, ImageListButton, Buttons, BaseCompositeComponent,
  LinkedControls, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  ResourceStrings, Validation, LuxembourgConstants, DataTypes, DataClasses,
  StrUtils, TermLabel, RegisteredControls, RestrictedEdits, XMLDoc, XMLIntf,
  SearchManager, GeneralFunctions, ExceptionForm, UserMessages, Contnrs, ApplicationSettings,
  PublishedTermRuleSelector, ADODB, TermGenerationFunctions, DSSDataTypes;

resourcestring
  ResStr_TermConstructsMissing = 'The TermConstructs.xml file is missing from the application directory.';
  ResStr_TermConstructsInvalid = 'The TermConstructs.xml file is not valid.';

type
  EConceptGeneral = class(TExceptionPath)
  end;
  
  TForceMode = (fmNone, fmPlaintext, fmItalic);
  {-----------------------------------------------------------------------------
    Class wrapper for the details of a single botanical construct, read from
    the XML file.
  }
  TBotanicalConstruct = class(TObject)
  private
    FForceMode: TForceMode;
    FItemName: string;
    FTerm: string;
  public
    procedure ReadXML(ANode: IXMLNode);
    property ForceMode: TForceMode read FForceMode;
    property ItemName: string read FItemName;
    property Term: string read FTerm;
  end;
  
  {-----------------------------------------------------------------------------
    General details page for aspects of a concept that are not handled through
    other entities.  This is embedded onto TfraConceptDetails when required.
  }
  TfraConceptGeneral = class(TBaseTabSheetFrame)
    btnInsertBotanical: TBitBtn;
    btnItalic: TImageListButton;
    chkPreferred: TCheckBox;
    cmbLanguage: TLuxIDComboBox;
    cmbNameType: TConceptGroupComboBox;
    cmbRank: TLuxIDComboBox;
    eListCode: TEdit;
    eSortCode: TEdit;
    eTerm: TLinkedEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblLanguage: TLabel;
    lblPreview: TTermLabel;
    lblBasicTerm: TLabel;
    pmBotanicalNames: TPopupMenu;
    lblAuthorAndDate: TLabel;
    lblAttributes: TLabel;
    lblPublishedTerm: TLabel;
    eAuthorAndDate: TEdit;
    eAttributes: TEdit;
    ePublishedTerm: TEdit;
    fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector;
    chkAutomaticPublishedTerm: TCheckBox;
    chkAcknoledgeUpdate: TCheckBox;
    procedure btnInsertBotanicalClick(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
    procedure cmbLanguageChange(Sender: TObject);
    procedure cmbLanguagePopulate(Sender: TObject);
    procedure cmbRankPopulate(Sender: TObject);
    procedure eTermChange(Sender: TObject);
    procedure eTermEditMouseDown(Sender: TObject; Button: TMouseButton; Shift:
            TShiftState; X, Y: Integer);
    procedure eTermExit(Sender: TObject);
    procedure eTermKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure eAuthorAndDateChange(Sender: TObject);
    procedure eAttributesChange(Sender: TObject);
    procedure ePublishedTermChange(Sender: TObject);
    procedure cmbRankChange(Sender: TObject);
    procedure fraPublishedTermRuleSelectorcmbPublishedTermRuleChange(
      Sender: TObject);
    function ExistingTermKey(const APlaintext, ALanguageKey: string): string;
    procedure chkAutomaticPublishedTermClick(Sender: TObject);

  private
    FConceptGroupKey: TKeyString;
    FConceptGroupVersionKey: TKeyString;
    FConstructs: TObjectList;
    FEnteredSessionID: TKeyString;
    FLanguageKey: TKeyString;
    FTermKey: TKeyString;
    FTermModified: Boolean;
    FHierarchyRelationTypeKey: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    FTermGeneratorKey: string;
    FTermVersion: string;
    FLeavingEditMode: Boolean;
    FAuthorAndDate: string;
    FAttributes: string;
    FAcknowledgeUpdateWarned: Boolean;
    FAcknowledgeUpdates: Boolean;
    FStartingTerm: string;
    procedure AddOrModifyTerm;
    procedure AddTerm;
    function CountCloseItalics(AText: String): Integer;
    function CountOpenItalics(AText: String): Integer;
    function DeleteConcept: Boolean;
    function GetParams: TVariantArray;
    procedure InsertConstructClick(Sender: TObject);
    procedure ItalicButtonState;
    function ItaliciseSelectedText(const AText: String; const AStartPos:
            Integer; const ALength: Integer): string;
    procedure ModifyTerm;
    procedure ReadXML(const AFileName: string);
    function RemoveItalicTags(AText: String): string;
    function RemoveNextCloseTag(AText: String): string;
    function ReplaceConcept(var AFindDialogKey, AFindDialogText: String):
            Boolean;
    procedure ShowFindDialog(ASearchType: SearchManager.TSearchType; var
            oSearchResultKey: string; var oSearchResultText: string);
    procedure WMReloadOneNodeOnly(var Message : TMessage); message
            WM_RELOAD_ONE_NODE_ONLY;
    procedure UpdatePublishedTerm;
    procedure UpdateTermLabel;
    procedure UpdateConcept(AConceptKey: string;
      ANeedsNewTermVersion: Boolean = false);
    procedure SetHierarchyKey;
  protected
    procedure DeleteData; override;
    function GetCaption: string; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    procedure ValidateData; override;
    procedure ValidateLinkedEdit(ALinkedControl: TRegisteredLinkedControl);
            override;
    procedure WMRefreshScreenInfo(var Message: TMessage); message
            WM_REFRESH_SCREEN_INFO;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function FindTermGeneratorKey: string;
  end;

//==============================================================================

implementation

{$R *.dfm}

uses
  ThesaurusEditorData, InterfaceDataModule, GeneralData, ADOInt,
  BaseDetailFrameUnit, XMLDom, ThesaurusApplicationSettings,
  BaseADODataModule, DefaultPaths;

const
  EL_CONSTRUCT = 'construct';
  EL_CONSTRUCTS = 'constructs';
  EL_ITEMNAME = 'item_name';
  EL_TERM = 'term';
  AT_FORCEITALIC = 'force_italic';
  AT_FORCEPLAINTEXT = 'force_plaintext';

{-==============================================================================
    TBotanicalConstruct
===============================================================================}
{-------------------------------------------------------------------------------
  Read the contents of the construct from the XML node. 
}
procedure TBotanicalConstruct.ReadXML(ANode: IXMLNode);
var
  lItalic: Boolean;
  lPlaintext: Boolean;
  i: Integer;
begin
  // Read Attributes
  if ANode.HasAttribute(AT_FORCEITALIC) then
    lItalic := ANode.Attributes[AT_FORCEITALIC]='1'
  else
    lItalic := False;
  if ANode.HasAttribute(AT_FORCEPLAINTEXT) then
    lPlaintext := ANode.Attributes[AT_FORCEPLAINTEXT]='1'
  else
    lPlaintext := False;
  if lPlaintext and (not lItalic) then
    FForceMode := fmPlaintext
  else if lItalic and (not lPlaintext) then
    FForceMode := fmItalic
  else
    FForceMode := fmNone;
  // Read elements
  for i := 0 to ANode.ChildNodes.Count-1 do begin
    if ANode.ChildNodes[i].NodeName=EL_TERM then
      FTerm := ANode.ChildNodes[i].NodeValue;
    if ANode.ChildNodes[i].NodeName=EL_ITEMNAME then
      FItemName := ANode.ChildNodes[i].NodeValue;
  end;
end;  // TBotanicalConstruct.ReadXML 

{-==============================================================================
    TfraConceptGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. Linked edits don't have their 'anchor' property published, so to
          make sure it resizes when the user resizes the app, it must be
          anchored in the constructor.
}
constructor TfraConceptGeneral.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  eTerm.Anchors := eTerm.Anchors + [akRight];
  eTerm.OnEditMouseDown := eTermEditMouseDown;
  FConstructs := nil;
  FLeavingEditMode := false;
end;  // TfraConceptGeneral.Create

{-------------------------------------------------------------------------------
}
destructor TfraConceptGeneral.Destroy;
begin
  FConstructs.Free;
  inherited;
end;  // TfraConceptGeneral.Destroy

{-------------------------------------------------------------------------------
  Comes here if the term or the language has changed for the concept. 
}
procedure TfraConceptGeneral.AddOrModifyTerm;
var
  lConceptsLinked: Integer;
  lMessage: string;
begin
  lConceptsLinked := (dmGeneral.GetStoredProcOutputParam
                                        ('usp_ConceptsLinkedToTermCount_Get',
                                        ['@ConceptKey', Key],
                                        '@Count'));
  // If there are other concepts linked to the term
  if lConceptsLinked > 0 then begin
    if lConceptsLinked = 1  then lMessage := ResStr_AddOrUpdateTermSingle
                            else lMessage := ResStr_AddOrUpdateTermMultiple;
    // Ask the user whether they want to create a new term or update the old one.
    // Asks 'do you want to update them all to use the new term?'
    case MessageDlg(Format(lMessage, [lConceptsLinked]),
                                  mtInformation, [mbYes, mbNo, mbCancel], 0) of
      mrYes :   begin
                  ModifyTerm;
                  // Event that should cause only the changed node to remain in treeview
                  FTermModified := True;
                end;
      mrNo:     begin
                  AddTerm;
                  FTermModified := False;
                end;
      mrCancel: Abort
    end;
  end
  // If there is just one term linked, just modify the term
  else
    if eTerm.Key = '' then
      begin
        AddTerm;
        FTermModified := False;
      end
    else
      begin
        ModifyTerm;
        FTermModified := True;
      end;
end;  // TfraConceptGeneral.AddOrModifyTerm

{-------------------------------------------------------------------------------
    Get an existing term key if it exists
}
function TfraConceptGeneral.ExistingTermKey(
  const APlaintext, ALanguageKey: string): string;
begin
  with dmGeneral.GetRecordset('usp_Term_Select', [
      '@Plaintext', APlaintext,
      '@LanguageKey', ALanguageKey]) do
  begin
    if not EOF then
      Result := VarToStr(Fields['Term_Key'].Value)
    else
      Result := '';
  end;
end;

{-------------------------------------------------------------------------------
  Adds a new term to the database.
}
procedure TfraConceptGeneral.AddTerm;
begin
  eTerm.Key := ExistingTermKey(eTerm.Text, cmbLanguage.CurrentStrID);

  if eTerm.Key = '' then
    eTerm.Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_TERM,
                                    'usp_Term_Insert',
                                    ['@Key', Key,
                                      '@LanguageKey', cmbLanguage.CurrentStrID,
                                      '@ItemName', eTerm.Text,
                                      '@Plaintext', eTerm.Text],
                                    '@Key'));
end;  // TfraConceptGeneral.AddTerm 

{-------------------------------------------------------------------------------
  Show a pop-up menu when the button is clicked. 
}
procedure TfraConceptGeneral.btnInsertBotanicalClick(Sender: TObject);
var
  lMenuLocation: TPoint;
begin
  ReadXML('TermConstructs.xml');
  lMenuLocation := btnInsertBotanical.ClientToScreen(Point(0, btnInsertBotanical.Height));
  pmBotanicalNames.Popup(lMenuLocation.X, lMenuLocation.Y);
end;  // TfraConceptGeneral.btnInsertBotanicalClick 

{-------------------------------------------------------------------------------
  Inserts open and close italic tags into eTerm.Text when the button is
          pressed.
}
procedure TfraConceptGeneral.btnItalicClick(Sender: TObject);
var
  lText: string;
  lCaretPosition: Integer;
begin
  inherited;
  // ItalicButtonState is called here because TLinkedEdit doesn't have an
  // OnMouseUp event. ItalicButtonState is called on the OnMouseDown event.
  // However, if you are selecting a section of text from right to left, you
  // would be calling the OnMouseDown event in the wrong place. It would be
  // preferable to call ItalicButtonState on an OnMouseUp event, because the
  // mouse button would always be released in the correct place. However, the
  // next best thing is to call it here.
  ItalicButtonState;
  lText := ePublishedTerm.Text;
  lCaretPosition := ePublishedTerm.SelStart + 1;
  
  // If all text is selected and user wants to stop it being italic.
  if (Length(ePublishedTerm.Text) = ePublishedTerm.SelLength) and btnItalic.Down then
    lText := RemoveItalicTags(lText)
  else
  // Handles putting italic tags around selected text
  if ePublishedTerm.SelLength >0 then begin
    lText := ItaliciseSelectedText(lText, ePublishedTerm.SelStart, ePublishedTerm.SelLength);
  end
  // Puts italic tags when nothing is selected
  else begin
    if btnItalic.Down then begin
      Insert('</i>', lText, lCaretPosition);
    end
    else begin
      Insert('<i>', lText, lCaretPosition);
    end;
  end;
  ePublishedTerm.Text := lText;
  ePublishedTerm.SetFocus;
  if btnItalic.Down then ePublishedTerm.SelStart := lCaretPosition + 3
                    else ePublishedTerm.SelStart := lCaretPosition + 2;
  ItalicButtonState;
end;  // TfraConceptGeneral.btnItalicClick 

{-------------------------------------------------------------------------------
  When the language selection changes, we need to clear the key of the term
          field in case the user intends to add a term with the same text, but
          with a different language.
}
procedure TfraConceptGeneral.cmbLanguageChange(Sender: TObject);
begin
  inherited;
  eTerm.Key := '';
end;  // TfraConceptGeneral.cmbLanguageChange 

{-------------------------------------------------------------------------------
  Populates the Languages combo box. 
}
procedure TfraConceptGeneral.cmbLanguagePopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_Languages_Select', []) do
    while not EOF do begin
      cmbLanguage.Add(
          VarToStr(Fields['Language_Key'].Value)
                  + ' - ' + VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Language_Key'].Value));
      MoveNext;
    end;
end;  // TfraConceptGeneral.cmbLanguagePopulate 

{-------------------------------------------------------------------------------
  Populates the rank combo box. 
}
procedure TfraConceptGeneral.cmbRankPopulate(Sender: TObject);
begin
  inherited;

  with dmGeneral.GetRecordset('usp_ConceptRanks_Select',
                              ['@ConceptGroupKey', FConceptGroupKey]) do
    while not EOF do begin
      cmbRank.Add(
          VarToStr(Fields['Item_Name'].Value),
          VarToStr(Fields['Item_Key'].Value));
      MoveNext;
    end;
end;  // TfraConceptGeneral.cmbRankPopulate 

{-------------------------------------------------------------------------------
  Count the number of </i> close italic brackets in a string. 
}
function TfraConceptGeneral.CountCloseItalics(AText: String): Integer;
var
  lPosition: Integer;
begin
  Result := 0;
  lPosition := Pos('</i>', AText);
  
  while lPosition <> 0 do begin
    AText := RightStr(AText, Length(AText) - lPosition - 2);
    Result := Result + 1;
    lPosition := Pos('</i>', AText);
  end;
end;  // TfraConceptGeneral.CountCloseItalics 

{-------------------------------------------------------------------------------
  Count the number of <i> open italic brackets in a string. 
}
function TfraConceptGeneral.CountOpenItalics(AText: String): Integer;
var
  lPosition: Integer;
begin
  Result := 0;
  lPosition := Pos('<i>', AText);
  
  while lPosition <> 0 do begin
    AText := RightStr(AText, Length(AText) - lPosition - 2);
    Result := Result + 1;
    lPosition := Pos('<i>', AText);
  end;
end;  // TfraConceptGeneral.CountOpenItalics 

{-------------------------------------------------------------------------------
}
function TfraConceptGeneral.DeleteConcept: Boolean;
var
  lFindDialogKey, lFindDialogText: string;
  lAssociatedSamples: _Recordset;

  procedure PerformDeletion;
  var
    lParams: Array of Variant;
  begin
    // Make another synonym list preferred (if there are any).
    dmGeneral.RunStoredProc('usp_Concept_ChooseListPreferred_Update',
                                              ['@Key', Key]);
    lParams := VarArrayOf(
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);
    dmGeneral.RunDeleteStoredProc('usp_Concept_Delete', lParams);
  end;

  procedure PerformLogging(const AFindDialogKey: TKeyString);
  var
    lParams: Array of Variant;
  begin
    lParams := nil;  // Gets rid of compiler warning.
    with ThesApplicationSettings do
      if LogDeletions then begin

        if (AFindDialogKey <> '') then
          LogDeletion('usp_ConceptReplace_Update',
                              ['@Key', Key,
                              '@NewConceptKey', AFindDialogKey,
                              '@DeleteConceptRelations', 'true'], true);
        LogDeletion('usp_Concept_ChooseListPreferred_Update', ['@Key', Key],
            AFindDialogKey='');
        lParams := VarArrayOf(
           ['@Key', Key,
            '@Timestamp', FTimestamp,
            '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);

        LogDeletion('usp_Concept_Delete', lParams);
      end;
  end;

  {-----------------------------------------------------------------------------
    Opens a save dialog for an external filter of any samples
    related to the deleted concept
  }
  procedure SaveAssociatedSamplesToExternalFilter;
  var
    lStringList: TStringList;
    lSaveDialog: TSaveDialog;
    lConceptName: string;
  begin
    lStringList := TStringList.Create;
    lSaveDialog := TSaveDialog.Create(self);
    try
      // Get the description line for the filter
      lConceptName := dmGeneral.GetRecordSet(
          'usp_Concept_Select',
          ['@ConceptKey', Key]).Fields['Item_Name'].Value;
      lStringList.Add(
          'Description: Samples associated with the concept ''' + lConceptName
          + ''' that prevented it from being deleted on '
          + FormatDateTime('dd/mm/yyyy', Now)
          + ' at ' + FormatDateTime('hh:mm:ss', Now));

      // Get all of the samples
      while not lAssociatedSamples.EOF do
      begin
        lStringList.Add('Sample' + #9 + lAssociatedSamples.Fields['Sample_Key'].Value);
        lAssociatedSamples.MoveNext;
      end;

      // Set up the save dialog
      lSaveDialog.InitialDir := GetProgramDataFolder(
          ThesApplicationSettings.RecorderInstallationPath,
          PATH_USER_FILES);
      lSaveDialog.Filter := 'Recorder External Filter (*.ref)|*.ref';
      lSaveDialog.DefaultExt := 'ref';
      if lSaveDialog.Execute then
          lStringList.SaveToFile(lSaveDialog.FileName);
    finally
      lStringList.Free;
      lSaveDialog.Free;
    end;
  end;

begin
  Result := True;

  // See if the concept was entered in a previous session and if 'log deletes'
  // was enabled.
  if (FEnteredSessionID <> AppSettings.SessionID) and
                                    (ThesApplicationSettings.LogDeletions) then
    case MessageDlg(ResStr_AskReplaceConceptWithAnother,
                    mtWarning,
                    [mbYes, mbNo, mbCancel],
                    0) of
      mrYes    : begin
                  Result := ReplaceConcept(lFindDialogKey, lFindDialogText);
                 end;
      mrCancel : Result := False;
    end;
    
  if Result then begin
    try
      PerformDeletion;
    except on E: EReferentialIntegrityException do begin
        // At time of writing, the referential integrity exceptions are broken
        // and do not store any information such as the message or table name.
        // Thefore the only way to check whether the exception was caused by
        // the concept being associated with an occurence via a determination
        // is to check if any such records exist in the database.
        lAssociatedSamples := dmGeneral.GetRecordset(
            'usp_Sample_Select_ForSaveFilterByConcept',
            ['@ConceptKey', Key]);
            
        if lAssociatedSamples.RecordCount > 0 then
        begin
          Result := False;
          if MessageDlg(
               ResStr_AskSaveAssociatedSamples,
               mtWarning,
               [mbOK, mbCancel],
               0) = mrOk then
          begin
            SaveAssociatedSamplesToExternalFilter;
          end
        end
        else if CompareText(E.TableName, 'TAXON_DETERMINATION') = 0 then begin
          MessageDlg(
              ResStr_CannotDeleteConceptDueToDeterminations,
              mtWarning,
              [mbOk],
              0);
          Result := False;
        end
        else if CompareText(e.TableName, 'TAXON_LIST_ITEM') = 0 then begin
          MessageDlg(
              ResStr_CannotDeleteConceptDueToTaxonListItems,
              mtWarning,
              [mbOk],
              0);
          Result := False;
        end
        else if (CompareText(e.Column, 'CONCEPT_KEY') = 0)
            or (CompareText(RightStr(e.Column, 12), '_CONCEPT_KEY') = 0) then
        begin
          if MessageDlg(
                 E.Message + ' ' + ResStr_AskReplaceConceptWithAnother,
                 mtWarning,
                 [mbOK, mbCancel],
                 0) = mrCancel then
            Result := False
          else begin
            ReplaceConcept(lFindDialogKey, lFindDialogText);
            PerformDeletion;
          end
        end else
          raise;
      end;
    end;
  end;
  if Result then PerformLogging(lFindDialogKey);
end;  // TfraConceptGeneral.DeleteConcept

{-------------------------------------------------------------------------------
  Delete the contents of the frame. 
}
procedure TfraConceptGeneral.DeleteData;
var
  lNodeClass: string;
  lConceptDeleted: Boolean;
begin
  lNodeClass := AdditionalProperties.GetProperty(PROP_NODE_CLASS);


  with dmGeneral do begin
    if ((lNodeClass = 'TAllSynonymNode')
          or (lNodeClass = 'TListSynonymNode')
          or (lNodeClass = 'TPotentialSynonymNode')) then
      // This proc doesn't need to be logged or synched with the Taxon dictionary
      // because no Concepts are actually deleted.
      RunUpdateStoredProc('usp_Synonym_Delete',
                                         ['@Key', Key, '@Timestamp', FTimestamp])
    else if lNodeClass = 'THomonymNode' then
      RunStoredProc('usp_HomonymPair_Delete',
                      ['@Concept_Key_1', Key,
                       '@Concept_Key_2', ParentKey])

    else if lNodeClass = 'TParentNode' then
      RunDeleteStoredProc('usp_ConceptRelation_Delete_ForPaste',
                                         ['@ToKey', ParentKey, '@FromKey', Key])
    else begin
      Connection.BeginTrans;
      try
        lConceptDeleted := DeleteConcept;
      except
        Connection.RollbackTrans;
        raise;
      end;

      if lConceptDeleted then
        Connection.CommitTrans
      else
      begin
        Connection.RollbackTrans;
        Abort;  // prevent node from being deleted although record was not
      end;
    end;
  end;
end;  // TfraConceptGeneral.DeleteData

{-------------------------------------------------------------------------------
  When the contents of eTerm changes, the lblPreview TTermLabel and the
          lblUnformatted TLabel must change what they display.
}
procedure TfraConceptGeneral.eTermChange(Sender: TObject);
begin
  inherited;
  // For some reason, when I pressed the italic button to change the text
  // the linked edit didn't lose its key automatically.
  eTerm.Key := '';
  UpdatePublishedTerm;
  FAcknowledgeUpdates := false;
  UpdateTermLabel;
end;  // TfraConceptGeneral.eTermChange 

{-------------------------------------------------------------------------------
  Handle a mouse click on the eTerm linked edit. 
}
procedure TfraConceptGeneral.eTermEditMouseDown(Sender: TObject; Button:
        TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(eTerm.OnEditMouseDown) then ItalicButtonState;
end;  // TfraConceptGeneral.eTermEditMouseDown 

{-------------------------------------------------------------------------------
  Trim the contents of eTerm. 
}
procedure TfraConceptGeneral.eTermExit(Sender: TObject);
begin
  inherited;
  // trim the text, only if we have to as this loses the key.
  if eTerm.Text <> TrimLeft(TrimRight(eTerm.Text)) then
      eTerm.Text := TrimLeft(TrimRight(eTerm.Text));
end;  // TfraConceptGeneral.eTermExit 

{-------------------------------------------------------------------------------
  Handle a key press on the eTerm Linked Edit. 
}
procedure TfraConceptGeneral.eTermKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
var
  lKey, lString: string;
begin
  inherited;
  ItalicButtonState;
  
  // If Return is pressed when the Term linked edit contains no text, bring
  // up the Find dialog anyway.
  if (Key = VK_RETURN) and (eTerm.Text = '') then begin
    DoCheck(lKey, lString, stTerm, '', '');
    eTerm.Key := lKey;
    eTerm.Text := lString;
  end;
end;  // TfraConceptGeneral.eTermKeyDown

{-------------------------------------------------------------------------------
  Ensures that the lblPreview and lblUnformatted can't get so long that they
          cause the frame to show horizontal scrollbars.
}
procedure TfraConceptGeneral.FrameResize(Sender: TObject);
begin
  inherited;
  lblPreview.Constraints.MaxWidth := Width - 150;
  lblPreview.Width := Width - 150;
end;  // TfraConceptGeneral.FrameResize 

{-------------------------------------------------------------------------------
  Returns the caption. 
}
function TfraConceptGeneral.GetCaption: string;
begin
  Result := ePublishedTerm.Text;
end;  // TfraConceptGeneral.GetCaption

{-------------------------------------------------------------------------------
  Returns the specialized parameters. 
}
function TfraConceptGeneral.GetParams: TVariantArray;
begin
  Result := VarArrayOf(['@ConceptKey', Key]);
end;  // TfraConceptGeneral.GetParams 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.InsertConstructClick(Sender: TObject);
var
  lStateItalic: Boolean;
  lConstruct: TBotanicalConstruct;
  lInsertText: string;
  i: Integer;
begin
  // use state machine to find current rich text state
  lStateItalic := False;
  for i := 1 to ePublishedTerm.SelStart do
    if lStateItalic then begin
      if CompareText(Copy(ePublishedTerm.Text, i, 4), '</i>')=0 then
        lStateItalic := False;
    end
    else begin
      if CompareText(Copy(ePublishedTerm.Text, i, 3), '<i>')=0 then
        lStateItalic := True;
    end;
  lConstruct := TBotanicalConstruct(Ptr((Sender as TMenuItem).Tag));
  if (lConstruct.ForceMode=fmItalic) and (not lStateItalic) then
    lInsertText := '<i>' + lConstruct.Term + '</i>'
  else if (lConstruct.ForceMode=fmPlainText) and lStateItalic then
    lInsertText := '</i>' + lConstruct.Term + '<i>'
  else
    lInsertText := lConstruct.Term;
  with ePublishedTerm do
    Text := LeftStr(Text, SelStart) + lInsertText +
      RightStr(Text, Length(Text) - (SelStart + SelLength));
end;  // TfraConceptGeneral.InsertConstructClick 

{-------------------------------------------------------------------------------
  Set the italic button to be up or down. 
}
procedure TfraConceptGeneral.ItalicButtonState;
var
  lLeftString: string;
  lOpenItalics, lCloseItalics: Integer;
begin
  lLeftString   := LeftStr(ePublishedTerm.Text, ePublishedTerm.SelStart + 1);
  lOpenItalics  := CountOpenItalics(lLeftString);
  lCloseItalics := CountCloseItalics(lLeftString);
  
  if lOpenItalics > lCloseItalics then btnItalic.Down := True
                                  else btnItalic.Down := False;
  // If lLeftString is '<' then we are dealing with an <i> at the start,
  // so put the italic button down.
  if lLeftString = '<' then btnItalic.Down := True;
end;  // TfraConceptGeneral.ItalicButtonState 

{-------------------------------------------------------------------------------
  This method handles putting italic tags into selected text. It also deals
          with following tags that will be obsolete when the new text has its
          own italic tags.
  
  The string is split into three parts, text that comes before the selection,
          the selected text and text that follows the selection. All tags are
          removed from the selected text. Then, depending on the state of the
          italic button, appropriate action is taken.
}
function TfraConceptGeneral.ItaliciseSelectedText(const AText: String; const
        AStartPos: Integer; const ALength: Integer): string;
var
  lStartString, lMiddleString, lEndString: string;
begin
  lStartString := LeftStr(AText, AStartPos);
  lEndString := RightStr(AText, Length(AText) - (ALength + AStartPos));
  lMiddleString := MidStr(AText, AStartPos + 1, ALength);
  lMiddleString := RemoveItalicTags(lMiddleString);
  if btnItalic.Down = False then begin
  // Add an italic section
    Insert('<i>', lMiddleString, 0);
    Insert('</i>', lMiddleString, Length(lMiddleString) + 1);
    if CountCloseItalics(lEndString) > CountOpenItalics(lEndString) then
      lEndString := RemoveNextCloseTag(lEndString);
  end else begin
  // Add a non italic section
    Insert('</i>', lMiddleString, 0);
    // Don't want to add an <i> unless necessary.
    if CountOpenItalics(lEndString) > CountCloseItalics(lEndString) then
      Insert('<i>', lMiddleString, Length(lMiddleString) + 2);
    if CountCloseItalics(lEndString) > CountOpenItalics(lEndString) then
      lEndString := RemoveNextCloseTag(lEndString);
  end;
  Result := lStartString + lMiddleString + lEndString;
end;  // TfraConceptGeneral.ItaliciseSelectedText 

{-------------------------------------------------------------------------------
  Stores the Timestamp and gets the Concept Group key. 
}
procedure TfraConceptGeneral.LoadData;
var
  acknowledgeUpdate: Boolean;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    with RegisteredRecordsets[0] do begin
      FTimestamp        := Fields['Timestamp'].Value;
      FTermKey          := VarToStr(Fields['Item_Key'].Value);
      FLanguageKey      := VarToStr(Fields['Language_Key'].Value);
      FEnteredSessionID := VarToStr(Fields['Entered_Session_ID'].Value);
      chkAutomaticPublishedTerm.Checked := Fields['Automatic_Published_Term'].Value;
      FTermGeneratorKey := VarToStr(Fields['Term_Generator_Key'].Value);
      FTermVersion      := VarToStr(Fields['Term_Version_Key'].Value);
      FAuthorAndDate    := VarToStr(Fields['Author_And_Date'].Value);
      FAttributes       := VarToStr(Fields['Version_Label'].Value);
      FStartingTerm     := VarToStr(Fields['Item_Name'].Value);
      chkAcknoledgeUpdate.Visible := false;
      if not (chkAutomaticPublishedTerm.Checked or
        Fields['AcknowledgeUpdate'].Value = Null) then
      begin
        if (Fields['AcknowledgeUpdate'].Value = Null) then
        begin
          acknowledgeUpdate := False;
        end
        else
        begin
          acknowledgeUpdate := Fields['AcknowledgeUpdate'].Value;
        end;

        FAcknowledgeUpdates := acknowledgeUpdate;
        chkAcknoledgeUpdate.Checked := acknowledgeUpdate;
        chkAcknoledgeUpdate.Visible := not (FAcknowledgeUpdates);

        UpdateTermLabel;
        {if (FAcknowledgeUpdates) then
        begin
          FAcknowledgeUpdateWarned := false;
          lblPreview.Font.Color := clBlue;
        end}
      end;
    end;

  end;

  if FConceptGroupKey <> AdditionalProperties.GetProperty(PROP_CONCEPT_GROUP_KEY) then begin
    FConceptGroupKey := AdditionalProperties.GetProperty(PROP_CONCEPT_GROUP_KEY);
    // if concept group different, then need to reload the rank combo list
    cmbRankPopulate(nil);
  end;

  // default automatic published term value for new concepts is true
  if Key = '' then
    chkAutomaticPublishedTerm.Checked := true;

  FConceptGroupVersionKey := AdditionalProperties.GetProperty(PROP_CONCEPT_GROUP_VERSION_KEY);

  // Hack: To make tooltip visible.
  // Not sure why it didn't just work in the designer.
  chkAcknoledgeUpdate.ShowHint := True;
  chkAcknoledgeUpdate.Hint := 'The Basic Term has been updated so the Published Term could be out of date. Please tick this box to acknowledge or update the Published term.';

  SetHierarchyKey;
end;  // TfraConceptGeneral.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.SetHierarchyKey;
begin
  with dmGeneral.GetRecordset('usp_ConceptGroup_Select',
      ['@Key', FConceptGroupKey]) do
    FHierarchyRelationTypeKey := VarToStr(Fields['Hierarchy_Relation_Type_Key'].Value);

  if FHierarchyRelationTypeKey = '' then
    fraPublishedTermRuleSelector.Visible := false;
end;

{-------------------------------------------------------------------------------
  Modify a term record. 
}
procedure TfraConceptGeneral.ModifyTerm;
begin
  eTerm.Key := ExistingTermKey(eTerm.Text, cmbLanguage.CurrentStrID);
  if eTerm.Key = '' then
  begin
    dmGeneral.RunUpdateStoredProc('usp_Term_Update',
                                    ['@Key', FTermKey,
                                      '@LanguageKey', cmbLanguage.CurrentStrID,
                                      '@Plaintext', eTerm.Text
                                    ]);
    // The Term itself has changed, so the linked edit should have the same key
    // as before. However, whenever a linked edit's text changes, it loses the key.
    // Therefore we have to give it the key again.
    eTerm.Key := FTermKey;
  end;
end;  // TfraConceptGeneral.ModifyTerm 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.ReadXML(const AFileName: string);
var
  lXMLDoc: IXMLDocument;
  i: Integer;
  lConstruct: TBotanicalConstruct;
  lMenuItem: TMenuItem;
begin
  if not Assigned(FConstructs) then begin
    if not FileExists(ExtractFilePath(Application.Exename) + 'TermConstructs.xml') then
      raise EConceptGeneral.CreateNonCritical(ResStr_TermConstructsMissing);
    try
      FConstructs := TObjectList.Create(True);
      lXMLDoc := TXMLDocument.Create(ExtractFilePath(Application.Exename) + 'TermConstructs.xml');
      for i := 0 to lXMLDoc.DocumentElement.ChildNodes.Count-1 do begin
        if lXMLDoc.DocumentElement.ChildNodes[i].NodeName = EL_CONSTRUCT then begin
          lConstruct := TBotanicalConstruct.Create;
          lConstruct.ReadXML(lXMLDoc.DocumentElement.ChildNodes[i]);
          FConstructs.Add(lConstruct);
          lMenuItem := TMenuItem.Create(Self);
          lMenuItem.Caption := lConstruct.ItemName;
          lMenuItem.Tag := Integer(lConstruct);
          lMenuItem.OnClick := InsertConstructClick;
          pmBotanicalNames.Items.Add(lMenuItem);
        end;
      end;
    except
      on EDomParseError do begin
        // invalid XML
        FreeAndNil(FConstructs);
        pmBotanicalNames.Items.Clear;
        raise EConceptGeneral.CreateNonCritical(ResStr_TermConstructsInvalid);
      end;
    end; // try
  end;
end;  // TfraConceptGeneral.ReadXML 

{-------------------------------------------------------------------------------
  Registers the recordset name and the parameters. 
}
procedure TfraConceptGeneral.RegisterControls;
begin
  inherited;
   // Register recordsets used
  RegisterRecordset('usp_Concept_Select', GetParams);
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eTerm, 'Plaintext', 'Item_Key', True, ResStr_FormattedTerm,
                                CheckLinkedTerm, ResStr_RelatedTo,
                                ConvertTermKeyToCaption);
  RegisterControl(cmbLanguage, 'Language_Name', 'Language_Key', True,
                                ResStr_Language);
  RegisterControl(cmbRank, 'Concept_Rank_Name', 'Concept_Rank_Key');
  RegisterControl(chkPreferred, 'Preferred');
  RegisterControl(cmbNameType, 'Name_Type_Concept_Name', 'Name_Type_Concept_Key',
                                True, ResStr_NameType);
  RegisterConceptGroupComboBox(cmbNameType, CG_THESAURUS_NAME_TYPES);
  RegisterControl(eListCode, 'List_Code');
  RegisterControl(eSortCode, 'Sort_Code');
  RegisterControl(eAuthorAndDate, 'Author_And_Date');
  RegisterControl(eAttributes, 'Version_Label');
  RegisterControl(fraPublishedTermRuleSelector.cmbPublishedTermRule,
                        'Term_Generator_Name',
                        'Term_Generator_Key');
  RegisterControl(ePublishedTerm, 'Item_Name');
  RegisterControl(chkAutomaticPublishedTerm, 'Automatic_Published_Term');

  RegisterControl(chkAcknoledgeUpdate, 'AcknowledgeUpdate');

  fraPublishedTermRuleSelector.SetDefaultItem;
end;  // TfraConceptGeneral.RegisterControls

function ConvertTermKeyToCaption(const AKey: String; const ATableName: String): String;
begin
  Result := VarToStr(dmGeneral.GetStoredProcOutputParam('usp_TermName_Get',
                     ['@Key', AKey], '@Caption'));
end;

{-------------------------------------------------------------------------------
  Remove the open and close italic tags from a string.
}
function TfraConceptGeneral.RemoveItalicTags(AText: String): string;
begin
  Result := StringReplace(AText, '<i>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '</i>', '', [rfReplaceAll]);
end;  // TfraConceptGeneral.RemoveItalicTags 

{-------------------------------------------------------------------------------
  This method is used when a section is being italicised that already contains
          an <i>. This means that the section following the selection would
          contain an </i>. This needs to be removed because the selection will
          be having a <i> and </i> surrounding it, which means there would be a
          spare </i>.
}
function TfraConceptGeneral.RemoveNextCloseTag(AText: String): string;
begin
  Result := StringReplace(AText, '</i>', '', []);
end;  // TfraConceptGeneral.RemoveNextCloseTag 

{-------------------------------------------------------------------------------
}
function TfraConceptGeneral.ReplaceConcept(var AFindDialogKey, AFindDialogText:
        String): Boolean;
begin
  if AFindDialogKey = '' then
    ShowFindDialog(stConcept, AFindDialogKey, AFindDialogText);
  if AFindDialogKey <> '' then begin
    dmGeneral.RunUpdateStoredProc('usp_ConceptReplace_Update',
                          ['@Key', Key,
                          '@NewConceptKey', AFindDialogKey,
                          '@DeleteConceptRelations', 'true']);
    FTimestamp := dmGeneral.GetStoredProcOutputParam('usp_Concept_Timestamp_Get',
                          ['@Key', Key],
                          '@Timestamp');
    Result := True;
  end else
    Result := False;
end;  // TfraConceptGeneral.ReplaceConcept 

{-------------------------------------------------------------------------------
  Save an inserted or updated record to the database. 
}
procedure TfraConceptGeneral.SaveData;
var
  lConceptHistoryRequired: Boolean;
  lNewTermVersionNeeded: Boolean;
begin
  if chkAutomaticPublishedTerm.Checked or chkAcknoledgeUpdate.Checked then
    chkAcknoledgeUpdate.Visible := false;

  if FConceptGroupKey = '' then
    FConceptGroupKey := AdditionalProperties.GetProperty(PROP_CONCEPT_GROUP_KEY);

  eTermExit(Self); // Make sure the contents has been trimmed.
  if (eTerm.Key = '') then
  begin
    AddOrModifyTerm;
  end;

  lConceptHistoryRequired := (FConceptGroupVersionKey <> '') and (Key = '');

  lNewTermVersionNeeded := false;
  if FTermKey <> eTerm.Key then
  begin
    //term has changed, so have to update term version
    lNewTermVersionNeeded := true;
  end
  else
  begin
    //check if term version has been altered
    if (FAuthorAndDate <> eAuthorAndDate.Text) or
      (FAttributes <> eAttributes.Text) then
    begin
      //check if any other concepts share the term version
      with dmGeneral.GetRecordset('usp_Concept_Select_ForTermVersion', [
        '@TermVersionKey', FTermVersion,
        '@ConceptKey', Key]) do
      begin
        if not EOF then
        begin
          //if so, ask if new term version is required
          case MessageDlg(
              ResStr_SharedTermVersion,
              mtConfirmation,
              [mbYes, mbNo, mbCancel], 0) of
            mrNo:     lNewTermVersionNeeded := true;
            mrYes:    lNewTermVersionNeeded := false;
            mrCancel: Abort;
          end;
        end;
      end;
    end;
  end;

  UpdateConcept(Key, lNewTermVersionNeeded);

  // If a Concept_History record is required, now we have the Concept key,
  // we can add it.
  if lConceptHistoryRequired then begin
    dmGeneral.RunInsertStoredProc(TN_CONCEPT_HISTORY,
                      'usp_ConceptHistory_Insert',
                      ['@ConceptKey', Key,
                      '@ConceptGroupVersionFromKey', FConceptGroupVersionKey],
                      '@Key');
    // Once everything is done, get the concept refreshed to show the history node
    PostMessage(Self.Handle, WM_REFRESH_SCREEN_INFO, 0, 0);
  end;

  // Update the node captions in the thesaurus navigator frame
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etRefreshNodeCaption, nil);
  FLeavingEditMode := false;
end;  // TfraConceptGeneral.SaveData

{-------------------------------------------------------------------------------
  Override method to ensure published term list is read only unless in
  edit mode.
}
procedure TfraConceptGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  fraPublishedTermRuleSelector.cmbPublishedTermRule.ReadOnly := not AEnabled;
  ePublishedTerm.Enabled := not chkAutomaticPublishedTerm.Checked;
  btnInsertBotanical.Enabled := btnInsertBotanical.Enabled and
                                (not chkAutomaticPublishedTerm.Checked);
  btnItalic.Enabled := btnItalic.Enabled and
                        (not chkAutomaticPublishedTerm.Checked);
end;

{-------------------------------------------------------------------------------
  Inserts new/updates existing concept record
}
procedure TfraConceptGeneral.UpdateConcept(AConceptKey: string;
  ANeedsNewTermVersion: Boolean);
var
  lParams: Array of Variant;
  acknowledgeUpdate: Boolean;
begin
  acknowledgeUpdate := chkAutomaticPublishedTerm.Checked or chkAcknoledgeUpdate.Checked;
  lParams := VarArrayOf(['@Key', AConceptKey,
                          '@TermKey', eTerm.Key,
                          '@ConceptGroupKey', FConceptGroupKey,
                          '@Preferred', chkPreferred.Checked,
                          '@ConceptRankKey', cmbRank.CurrentStrID,
                          '@NameTypeConceptKey', cmbNameType.CurrentStrID,
                          '@PublishedTerm', ePublishedTerm.Text,
                          '@AutomaticPublishedTerm', chkAutomaticPublishedTerm.Checked,
                          '@TermGeneratorKey', fraPublishedTermRuleSelector.Key,
                          '@NewTermVersionNeeded', ANeedsNewTermVersion,
                          '@AuthorAndDate', eAuthorAndDate.Text,
                          '@Attributes', eAttributes.Text,
                          '@SortCode', eSortCode.Text,
                          '@ListCode', eListCode.Text,
                          '@Timestamp', FTimestamp,
                          '@AcknowledgeUpdate', acknowledgeUpdate
                        ]);

  if AConceptKey = '' then
    Key := dmGeneral.RunInsertStoredProc('TN_CONCEPT',
                  'usp_ConceptSimple_Insert',
                  lParams,
                  '@Key')
  else
    dmGeneral.RunUpdateStoredProc('usp_ConceptSimple_Update', lParams);
end;

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.ShowFindDialog(ASearchType:
        SearchManager.TSearchType; var oSearchResultKey: string; var
        oSearchResultText: string);
begin
  with TSearchManager.Create do begin
    SearchType := ASearchType;
    oSearchResultKey := RunSearch;
    oSearchResultText := ResultText;
    Free;
  end;
end;  // TfraConceptGeneral.ShowFindDialog 

{-------------------------------------------------------------------------------
  Validate the sort code. 
}
procedure TfraConceptGeneral.ValidateData;
begin
  inherited;
  if eSortCode.Text <> '' then begin
    ValidateValue(
        IsInt(eSortCode.Text) = True,
        Format(ResStr_MustBeInteger, [ResStr_SortCode]));
    ValidateValue(
        StrToInt(eSortCode.Text) >= 0,
        Format(ResStr_MustBeNonNegative, [ResStr_SortCode]));
  end;
end;  // TfraConceptGeneral.ValidateData 

{-------------------------------------------------------------------------------
  The 'Formatted Term' linked edit doesn't necessarily have a key at validation
          and could get it during the save. As all validation now occurs before
          anything saves, we need to override the standard validation on linked
          edits to stop it failing unnecessarily.
}
procedure TfraConceptGeneral.ValidateLinkedEdit(ALinkedControl:
        TRegisteredLinkedControl);
begin
  if (eTerm.Key = '') and (TLinkedEdit(ALinkedControl.Control).Text <> '') then
  begin
    with TSearchManager.Create do begin
      try
        SearchType  := stTerm;
        if cmbLanguage.CurrentStrID <> '' then
          SearchKey := cmbLanguage.CurrentStrID;
          eTerm.Key := FindUnique(TLinkedEdit(ALinkedControl.Control).Text);
      finally
        Free;
      end;  // try
    end;  // with TSearchManager.Create
  end;  // if (Text <> '') and (Key = '')
end;  // TfraConceptGeneral.ValidateLinkedEdit 

{-------------------------------------------------------------------------------
  When a concept is saved, if it adds any sub-detail nodes by default (e.g. a
          concept history node) then refreshes the details.
}
procedure TfraConceptGeneral.WMRefreshScreenInfo(var Message: TMessage);
begin
  if Assigned(OnFrameNotification) then
    // Invoke a refresh, so the concept history folder is re-loaded.
    OnFrameNotification(Self, etRefreshSelectedNode, nil);
end;  // TfraConceptGeneral.WMRefreshScreenInfo 

{-------------------------------------------------------------------------------
  When a term is editted that is used by multiple concepts, the one selected node is displayed afterwards. However, this refresh has been moved into a message. This is so that when the message finally gets posted, everything else will already have been sorted in the save, so things won't be out of sync.
}
procedure TfraConceptGeneral.WMReloadOneNodeOnly(var Message : TMessage);
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etTermForManyConceptsChanged, nil);
end;  // TfraConceptGeneral.WMReloadOneNodeOnly

{-------------------------------------------------------------------------------
  Returns a term generator key. Considers cases where a concept key has not
  yet been created.
}
function TfraConceptGeneral.FindTermGeneratorKey: string;
begin
  if FHierarchyRelationTypeKey = '' then
    Result := GetTermGeneratorKey(FConceptGroupKey, true)
  else
  begin
    if fraPublishedTermRuleSelector.Key <> '' then
      Result := fraPublishedTermRuleSelector.Key
    else
      Result := GetTermGeneratorKey(Key, false, true)
  end;
end;

{-------------------------------------------------------------------------------
  Updates the published term field (if required) as the term text box is edited
}
procedure TfraConceptGeneral.UpdatePublishedTerm;
var
  newTermText : String;
begin
  newTermText := GetPublishedTerm(
      FindTermGeneratorKey,
      [eTerm.Text,
        eAuthorAndDate.Text,
        eAttributes.Text,
        cmbRank.CurrentStrID,
        ParentKey]);
  if chkAutomaticPublishedTerm.Checked then
  begin
    FAcknowledgeUpdateWarned := false;
    lblPreview.Font.Color := clBlue;
    chkAcknoledgeUpdate.Visible := false;
    ePublishedTerm.Text := newTermText
  end
end;

procedure TfraConceptGeneral.UpdateTermLabel;
var
  newTermText : String;
begin
  newTermText := GetPublishedTerm(
      FindTermGeneratorKey,
      [eTerm.Text,
        eAuthorAndDate.Text,
        eAttributes.Text,
        cmbRank.CurrentStrID,
        ParentKey]);
  if FAcknowledgeUpdates then
  begin
    FAcknowledgeUpdateWarned := false;
    lblPreview.Font.Color := clBlue;
    lblPreview.Caption := ePublishedTerm.Text;
    chkAcknoledgeUpdate.Visible := false;
  end
  else if  not (ePublishedTerm.Text = '') and
      not FAcknowledgeUpdateWarned and
      not (Key = '') and
      not (newTermText = ePublishedTerm.Text) then
  begin
    FAcknowledgeUpdateWarned := true;
    FAcknowledgeUpdates := false;
    lblPreview.Font.Color := RGB(200, 10, 10);
    lblPreview.Caption := '!' + ePublishedTerm.Text;
    chkAcknoledgeUpdate.Visible := true;
    chkAcknoledgeUpdate.Checked := false;
  end;
end;

{-------------------------------------------------------------------------------
  Synchronise with published term text box
}
procedure TfraConceptGeneral.eAuthorAndDateChange(Sender: TObject);
begin
  inherited;
  UpdatePublishedTerm;
  FAcknowledgeUpdates := false;
  UpdateTermLabel;
end;

{-------------------------------------------------------------------------------
  Synchronise with published term text box
}
procedure TfraConceptGeneral.eAttributesChange(Sender: TObject);
begin
  inherited;
  UpdatePublishedTerm;
  FAcknowledgeUpdates := false;
  UpdateTermLabel;
end;

{-------------------------------------------------------------------------------
  Synchronise preview with published term text box
}
procedure TfraConceptGeneral.ePublishedTermChange(Sender: TObject);
begin
  inherited;

  lblPreview.Caption := ePublishedTerm.Text;
  if FAcknowledgeUpdateWarned then
  begin
    lblPreview.Font.Color := RGB(200, 10, 10);
    lblPreview.Caption := '!' + ePublishedTerm.Text;
  end
  else
    lblPreview.Font.Color := clBlue;
end;

{-------------------------------------------------------------------------------
  Synchronise with published term text box
}
procedure TfraConceptGeneral.cmbRankChange(Sender: TObject);
begin
  inherited;
  UpdatePublishedTerm;
end;

{-------------------------------------------------------------------------------
  Synchronise with published term text box
}
procedure TfraConceptGeneral.fraPublishedTermRuleSelectorcmbPublishedTermRuleChange(
  Sender: TObject);
begin
  inherited;
  UpdatePublishedTerm;
end;

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.chkAutomaticPublishedTermClick(
  Sender: TObject);
begin
  inherited;
  ePublishedTerm.Enabled := not chkAutomaticPublishedTerm.Checked;
  btnInsertBotanical.Enabled := not chkAutomaticPublishedTerm.Checked;
  btnItalic.Enabled := not chkAutomaticPublishedTerm.Checked;
  if chkAutomaticPublishedTerm.Checked then
    UpdatePublishedTerm;
end;

end.



