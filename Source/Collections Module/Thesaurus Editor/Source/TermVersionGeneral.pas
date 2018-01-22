{===============================================================================
  Unit:         TermVersionGeneral

  Defines:      TfraTermVersionGeneral

  Description:  General details tab for a term version

  Created:      June 2003

  Last revision information:
    $Revision: 18 $
    $Date: 17/11/11 16:39 $
    $Author: Jamesbichard $

===============================================================================}

unit TermVersionGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BaseTabSheetFrameUnit, ExtCtrls, DataTypes, DSSDataTypes,
  ApplicationSettings, GeneralFunctions, ComboListID, LuxIDComboBox,
  DataClasses, LuxembourgConstants, ExceptionForm;

type
  ETermVersionGeneral = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    General tab page allowing the details of a term version to be viewed and
    edited.
  }
  TfraTermVersionGeneral = class(TBaseTabSheetFrame)
    Bevel1: TBevel;
    cmbExistingTermVersions: TLuxIDComboBox;
    eAuthorityAndDate: TEdit;
    eLabel: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblNumber: TLabel;
    procedure cmbExistingTermVersionsChange(Sender: TObject);
    procedure cmbExistingTermVersionsPopulate(Sender: TObject);
    procedure eAuthorityAndDateChange(Sender: TObject);
  private
    FConceptKey: string;
    FKeyToSave: TKeyString;
    FTermKey: TKeyString;
    FTimestamp: TSQLSvrTimestamp;
    FAuthorAndDateEdited: Boolean;
    FAutomaticPublishedTerm: Boolean;
    procedure ClearComboBox;
    procedure GetTermKey(AConceptKey: TKeyString);
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: string; override;
    function GetHasData: Boolean; virtual;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  public
    property HasData: Boolean read GetHasData;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses GeneralData, ResourceStrings, ConceptDetailsNodes, ThesaurusApplicationSettings,
  BaseDetailFrameUnit, ComCtrls;

{-==============================================================================
    TfraTermVersionGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Clear the items in cmbExistingTermVersions. This is necessary to stop changed
          term versions appearing as their old versions and as new versions.
}
procedure TfraTermVersionGeneral.ClearComboBox;
var
  i: Integer;
begin
  for i := cmbExistingTermVersions.Count -1 downto 0 do
    cmbExistingTermVersions.Delete(i);
end;  // TfraTermVersionGeneral.ClearComboBox 

{-------------------------------------------------------------------------------
  Handle a change to the cmbExistingTermVersions  combo box. 
}
procedure TfraTermVersionGeneral.cmbExistingTermVersionsChange(Sender: TObject);
begin
  inherited;
  if EditMode=emEdit then begin
    // No change, do noting else.
    if FKeyToSave = cmbExistingTermVersions.CurrentStrID then Exit;

    FKeyToSave := cmbExistingTermVersions.CurrentStrID;

    with dmGeneral.GetRecordset('usp_TermVersion_Select', ['@Key', FKeyToSave]) do begin
      if not Eof then begin
        eLabel.Text            := VarToStr(Fields['Version_Label'].Value);
        eAuthorityAndDate.Text := VarToStr(Fields['Author_And_Date'].Value);
        FTimestamp             := Fields['Timestamp'].Value;
      end else begin
        eLabel.Text            := '';
        eAuthorityAndDate.Text := '';
      end;
      Close;
    end; // with
  end;
end;  // TfraTermVersionGeneral.cmbExistingTermVersionsChange

{-------------------------------------------------------------------------------
  Populate the cmbExistingTermVersions combo box. 
}
procedure TfraTermVersionGeneral.cmbExistingTermVersionsPopulate(Sender:
        TObject);
var
  lItemName: string;
begin
  inherited;
  ClearComboBox;
  
  with dmGeneral.GetRecordset('usp_TermVersions_Select_ForTerm',
                                                  ['@Key', FTermKey]) do begin
    while not Eof do begin
      lItemName :=  VarToStr(Fields['Item_Name'].Value);
      if lItemName = '' then
        lItemName := '<blank>';
      cmbExistingTermVersions.Add(lItemName, VarToStr(Fields['Item_Key'].Value));
      MoveNext;
    end; // while
    Close;
  end; // with
end;  // TfraTermVersionGeneral.cmbExistingTermVersionsPopulate 

{-------------------------------------------------------------------------------
  Deletes the record that was used to load the frame. 
}
procedure TfraTermVersionGeneral.DeleteData;
begin
  // Use save data to clear up, since this is more intelligent than a brute
  // force deletion
  eLabel.Text := '';
  eAuthorityAndDate.Text := '';
  SaveData;
end;  // TfraTermVersionGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Enable the controls. Neither of the fields on this frame are compulsory, but
          something must be typed into at least one of them. So they are both
          set to be light yellow.
}
procedure TfraTermVersionGeneral.EnableControls(AEnabled: Boolean);
begin
  inherited;
  if AEnabled then
    eLabel.Color := MergeColours(AppSettings.MandatoryColour, clWindow, 25)
  else
    eLabel.Color := clWindow;
  eAuthorityAndDate.Color := eLabel.Color;
end;  // TfraTermVersionGeneral.EnableControls 

{-------------------------------------------------------------------------------
  Returns the caption. 
}
function TfraTermVersionGeneral.GetCaption: string;
begin
  // If attached to the current concept directly, then label is different
  if AdditionalProperties.GetProperty(PROP_THISTERMVERSION) then
    Result := ResStr_ThisTermVersion
  else
    Result := ResStr_Version;
  if eLabel.Text<>'' then
    Result := Result + ' ' + eLabel.Text;
  if eAuthorityAndDate.Text<>'' then
    Result := Result + ' (' + eAuthorityAndDate.Text + ')';
end;  // TfraTermVersionGeneral.GetCaption 

{-------------------------------------------------------------------------------
}
function TfraTermVersionGeneral.GetHasData: Boolean;
begin
  Result := (eLabel.Text <> '') or (eAuthorityAndDate.Text <> '');
end;  // TfraTermVersionGeneral.GetHasData 

{-------------------------------------------------------------------------------
  Get the term key for a given concept key. 
}
procedure TfraTermVersionGeneral.GetTermKey(AConceptKey: TKeyString);
begin
  if AConceptKey <> '' then
    with dmGeneral.GetRecordset('usp_Concept_Select', ['@ConceptKey', AConceptKey])
    do begin
      FTermKey := VarToStr(Fields['Item_Key'].Value);
      Close;
    end;
end;  // TfraTermVersionGeneral.GetTermKey 

{-------------------------------------------------------------------------------
  Loads the timestamp. 
}
procedure TfraTermVersionGeneral.LoadData;
var
  lIndex: Integer;
begin
  inherited;
  // If the parent is a concept, convert to a term version key
  if TTabSheet(Parent).PageIndex>0 then
    with dmGeneral.GetRecordset('usp_Concept_Select',
        ['@ConceptKey', Key]) do begin
      FConceptKey := Key;
      Key := VarToStr(Fields['Term_Version_Key'].Value);
      FAutomaticPublishedTerm := Fields['Automatic_Published_Term'].Value;
    end
  else
    with dmGeneral.GetRecordset('usp_Concept_Select',
        ['@ConceptKey', ParentKey]) do
    begin
      FConceptKey := ParentKey;
      Key := VarToStr(Fields['Term_Version_Key'].Value);
      FAutomaticPublishedTerm := Fields['Automatic_Published_Term'].Value;
    end;

  GetTermKey(ParentKey);
  if not RegisteredRecordsets[0].Eof then
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  cmbExistingTermVersionsPopulate(Self);
  FKeyToSave := Key;
  with cmbExistingTermVersions do begin
    lIndex := IDIndexOf(Key);
    if lIndex <> -1 then ItemIndex := lIndex
                    else ItemIndex := 0;
    cmbExistingTermVersionsChange(nil);
  end;
  FAuthorAndDateEdited := False;
end;  // TfraTermVersionGeneral.LoadData 

{-------------------------------------------------------------------------------
  Registers the name of the recordset and the controls. 
}
procedure TfraTermVersionGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  // Find the term version key.  If we are the second tab for a concept, then
  // use a different proc to load data
  if not (Parent is TTabSheet) then
    raise ETermVersionGeneral.Create(Format(ResStr_InvalidMethodCall,
        ['TfraTermVersionGeneral.RegisterControls']));
  if TTabSheet(Parent).PageIndex>0 then
    RegisterRecordset('usp_TermVersion_Select_ForConcept')
  else
    RegisterRecordset('usp_TermVersion_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eLabel, 'Version_Label');
  RegisterControl(eAuthorityAndDate, 'Author_And_Date');
end;  // TfraTermVersionGeneral.RegisterControls

{-------------------------------------------------------------------------------
  Saves an inserted or updated record. 
}
procedure TfraTermVersionGeneral.SaveData;
var
  lParams: Array of Variant;
  lNodeClass: string;
begin
  //Term versions can only be edited when the top level node is selected.
  //Therefore, saving should not do anything when this node is not selected.
  lNodeClass := AdditionalProperties.GetProperty(PROP_NODE_CLASS);
  if (lNodeClass = 'TTermVersionNode') then
  begin
    if (FConceptKey <> '') then
    begin
      if ((Key <> FKeyToSave) or FAuthorAndDateEdited) and (not FAutomaticPublishedTerm) then
      begin
        if MessageDlg(ResStr_ReinstateAutomaticPublishedTerm,
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          FAutomaticPublishedTerm := true;
          dmGeneral.RunUpdateStoredProc('usp_Concept_UpdateAutomaticPublishedTerm', [
            '@ConceptKey', FConceptKey,
            '@AutomaticPublishedTerm', FAutomaticPublishedTerm]);
        end;
      end;
    end;

    // FKeyToSave is used because if the user selects a different Term Version from
    // the combo box we can't store this Term Version Key in Key, because the key
    // gets reloaded to what it originally was prior to the save.
    Key := FKeyToSave;
    lParams := VarArrayOf(['@Key', Key,
            '@ConceptKey', FConceptKey,
            '@VersionLabel', eLabel.Text,
            '@AuthorAndDate', eAuthorityAndDate.Text,
            '@Timestamp', FTimestamp,
             '@SyncTaxonDict', ThesApplicationSettings.SyncTaxonDictDeletions]);

    dmGeneral.RunUpdateStoredProc('usp_TermVersion_Update', lParams);
  end;
end;  // TfraTermVersionGeneral.SaveData

{-------------------------------------------------------------------------------
}
procedure TfraTermVersionGeneral.eAuthorityAndDateChange(Sender: TObject);
begin
  inherited;
  FAuthorAndDateEdited := true;
end;

end.




