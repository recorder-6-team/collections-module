{===============================================================================
  Unit:        DomainConceptGroupSelector

  Defines:     TDomainComboItem
               TfraDomainConceptGroupsSelector


  Description: Frame that encapsulates a domain and a concept group combo box.

  Created:     Dec 2003

  Model:       ThesaurusNavigator.mpb

  Last revision information:
    $Revision: 13 $
    $Date: 2/09/11 16:05 $
    $Author: Jamesbichard $

===============================================================================}
unit DomainConceptGroupSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComboListID, LuxIDComboBox, StdCtrls, ImageListButton, AdoDB,
  ExceptionForm;

type
  {-----------------------------------------------------------------------------
    General exception class for unit.
  }
  EDomainConceptGroupSelectorException = class (TExceptionPath)
  end;
  
  // types of node in the domain combo
  TDomainComboItemType = (itSubjectArea, itDomain);

  {-----------------------------------------------------------------------------
    Storage class to hold details of a single element in the domain combo box.  
    Either a subject area or a domain.
  }
  TDomainComboItem = class (TObject)
  private
    FItemKey: string;
    FItemType: TDomainComboItemType;
  public
    procedure Initialise(AItemType: TDomainComboItemType; const AItemKey: 
        string);
    property ItemKey: string read FItemKey;
    property ItemType: TDomainComboItemType read FItemType;
  end;
  
  {-----------------------------------------------------------------------------
    Frame that contains a reusable domain and concept group combo box.  This 
    frame is used whenever the user is required to select a concept group by 
    first browsing through a domain.  The frame also allows historic concept 
    group versions to be selected.
  }
  TfraDomainConceptGroupsSelector = class (TFrame)
    btnHistory: TImageListButton;
    cmbConceptGroups: TLuxIDComboBox;
    cmbDomains: TComboBox;
    lblConceptGroup: TLabel;
    lblDomain: TLabel;
    procedure btnHistoryClick(Sender: TObject);
    procedure cmbConceptGroupsPopulate(Sender: TObject);
    procedure cmbDomainsDrawItem(Control: TWinControl; Index: Integer; Rect: 
        TRect; State: TOwnerDrawState);
    procedure cmbDomainsKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure cmbDomainsSelect(Sender: TObject);
  private
    function GetConceptGroupKey: string;
    function GetConceptGroupVersionKey: string;
    function GetDomainKey: string;
    function GetHierarchyRelationTypeKey: string;
    procedure PopulateConceptGroups(AStoredProcName: string; AIncludeDates: 
        boolean);
    procedure SetConceptGroupLabel;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    destructor Destroy; override;
    procedure PopulateDomainCombo;
    procedure SelectConceptGroup(const AConceptGroupKey: string);
    procedure SelectDomain(const ADomainKey: string);
    procedure Clear;
    property ConceptGroupKey: string read GetConceptGroupKey;
    property ConceptGroupVersionKey: string read GetConceptGroupVersionKey;
    property DomainKey: string read GetDomainKey;
    property HierarchyRelationTypeKey: string read GetHierarchyRelationTypeKey;
  end;
  
implementation

{$R *.dfm}

uses
  InterfaceDataModule, GeneralData, ResourceStrings, ApplicationSettings,
  GeneralFunctions, BaseADODataModule;

{-==============================================================================
    TDomainComboItem
===============================================================================}
{-------------------------------------------------------------------------------
  Initialisation method for object. 
}
procedure TDomainComboItem.Initialise(AItemType: TDomainComboItemType; const
    AItemKey: string);
begin
  FItemType := AItemType;
  FItemKey := AITemKey;
end;  // TDomainComboItem.Initialise 

{-==============================================================================
    TfraDomainConceptGroupsSelector
===============================================================================}
{-------------------------------------------------------------------------------
  Cleanup object data. 
}
destructor TfraDomainConceptGroupsSelector.Destroy;
var
  lIdx: Integer;
begin
  // Free combo box items
  if Assigned(cmbDomains) then
    for lIdx := 0 to cmbDomains.Items.Count-1 do
      TObject(cmbDomains.Items.Objects[lIdx]).Free;
  inherited;
end;  // TfraDomainConceptGroupsSelector.Destroy 

{-------------------------------------------------------------------------------
  Toggles the frame between concept group mode and concept group version mode. 
}
procedure TfraDomainConceptGroupsSelector.btnHistoryClick(Sender: TObject);
begin
  inherited;
  btnHistory.Down := not btnHistory.Down;
  cmbConceptGroups.Clear;
end;  // TfraDomainConceptGroupsSelector.btnHistoryClick 

{-------------------------------------------------------------------------------
  Populates either the list of concept groups or concept group versions. 
}
procedure TfraDomainConceptGroupsSelector.cmbConceptGroupsPopulate(Sender: 
    TObject);
begin
  if cmbDomains.ItemIndex>-1 then
    if btnHistory.Down then
      PopulateConceptGroups('usp_ConceptGroupVersion_Select_ForDomain', True)
    else
      PopulateConceptGroups('usp_ConceptGroup_Select_ForDomain', False);
end;  // TfraDomainConceptGroupsSelector.cmbConceptGroupsPopulate 

{-------------------------------------------------------------------------------
  Draw the domain combo box with domains indented under the subject areas. 
}
procedure TfraDomainConceptGroupsSelector.cmbDomainsDrawItem(Control: 
    TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  lIndent: Integer;
  lImageIndex: Integer;
begin
  if TDomainComboItem(cmbDomains.Items.Objects[Index]).ItemKey = '' then begin
    lIndent := 0;
    lImageIndex := 0;
    cmbDomains.Canvas.Font.Name  := 'Arial';  //MS Sans Serif font looks rubbish in italic
    cmbDomains.Canvas.Font.Style := [fsItalic];
    cmbDomains.Canvas.Font.Color := GetContrastColour(clGrayText);
    // Don't draw a highlight background for subject area as user cannot select it
    cmbDomains.Canvas.Brush.Color := clGrayText;
  end
  else begin
    lIndent := 25;
    lImageIndex := 1;
    cmbDomains.Canvas.Font.Style := [];
    if odSelected in State then
      cmbDomains.Canvas.Font.Color := clHighlightText
    else
      cmbDomains.Canvas.Font.Color := clWindowText;
  end;
  cmbDomains.Canvas.FillRect(Rect);
  cmbDomains.Canvas.TextOut(Rect.Left+lIndent + 18, Rect.Top + 1, cmbDomains.Items[Index]);
  dmInterface.ilOtherNodes.Draw(cmbDomains.Canvas, Rect.Left + lIndent, Rect.Top, lImageIndex);
end;  // TfraDomainConceptGroupsSelector.cmbDomainsDrawItem

{-------------------------------------------------------------------------------
  Ensure that when the user navigates in the domain list using the keyboard, 
      when they move up onto a subject area the comb box automatically moves to 
      the previous domain, since subject areas are not selectable. 
}
procedure TfraDomainConceptGroupsSelector.cmbDomainsKeyDown(Sender: TObject; 
    var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_UP) and (cmbDomains.ItemIndex>1) then begin
    if TDomainComboItem(
        cmbDomains.Items.Objects[cmbDomains.ItemIndex-1]).ItemType =
        itSubjectArea then begin
      // cancel default handling
      Key := 0;
      // go up to previous domain, skipping the subject area
      cmbDomains.ItemIndex := cmbDomains.ItemIndex-2;
      SetConceptGroupLabel;
      cmbConceptGroups.Clear;
    end;
  end;
end;  // TfraDomainConceptGroupsSelector.cmbDomainsKeyDown

{-------------------------------------------------------------------------------
  When selecting an item in the combo box, ensure that the subject areas are
      not selectable.  Also, update the concept group label.
}
procedure TfraDomainConceptGroupsSelector.cmbDomainsSelect(Sender: TObject);
begin
  if cmbDomains.ItemIndex>-1 then begin
    if TDomainComboItem(
        cmbDomains.Items.Objects[cmbDomains.ItemIndex]).ItemKey='' then
      cmbDomains.ItemIndex := cmbDomains.ItemIndex + 1;
    SetConceptGroupLabel;
    cmbConceptGroups.Clear;
  end;
end;  // TfraDomainConceptGroupsSelector.cmbDomainsSelect

{-------------------------------------------------------------------------------
  Accessor method.  Returns the currently selected concept group key.  Raises
      an exception if concept groups not currently active.
}
function TfraDomainConceptGroupsSelector.GetConceptGroupKey: string;
begin
  if btnHistory.Down then
  begin
    if cmbConceptGroups.CurrentStrID <> '' then
      Result := dmGeneral.GetStoredProcOutputParam
                        ('usp_ConceptGroupKey_Get_ForConceptGroupVersion',
                        ['@Key', Copy(cmbConceptGroups.CurrentStrID, 1, 16)],
                        '@ConceptGroupKey')
    // Taken this error out because if the button is down, and the user wants
    // to drag/drop Concepts around, TfraThesaurusNavigatorEditable.InsertOrUpdateConcept
    // needs the ConceptGroupKey. We have the ConceptGroupVersionKey, so we
    // can get the ConceptGroupKey easily from the database.
      //    raise EDomainConceptGroupSelectorException.Create(
      //        ResStr_ShowingConceptGroupVersions);
    else
      Result := '';
  end
  else
    Result := Copy(cmbConceptGroups.CurrentStrID, 1, 16);
end;  // TfraDomainConceptGroupsSelector.GetConceptGroupKey 

{-------------------------------------------------------------------------------
  Accessor method.  Returns the currently selected concept group version key.  
      Raises an exception if concept group versions not currently active. 
}
function TfraDomainConceptGroupsSelector.GetConceptGroupVersionKey: string;
begin
  if not btnHistory.Down then
    raise EDomainConceptGroupSelectorException.Create(
        ResStr_ShowingConceptGroups);
  Result := Copy(cmbConceptGroups.CurrentStrID, 1, 16);
end;  // TfraDomainConceptGroupsSelector.GetConceptGroupVersionKey 

{-------------------------------------------------------------------------------
  Accessor method. 
}
function TfraDomainConceptGroupsSelector.GetDomainKey: string;
begin
  if cmbDomains.ItemIndex=-1 then
    raise EDomainConceptGroupSelectorException.Create(Format(
        ResStr_NoComboItemSelected,
        ['cmbDomains']));
  Result := TDomainComboItem(
      cmbDomains.Items.Objects[cmbDomains.ItemIndex]).ItemKey
end;  // TfraDomainConceptGroupsSelector.GetDomainKey 

{-------------------------------------------------------------------------------
  Accessor method.  Relationship type used for the hierarchy in the current 
      concept group. 
}
function TfraDomainConceptGroupsSelector.GetHierarchyRelationTypeKey: string;
begin
  if cmbDomains.ItemIndex=-1 then
    raise EDomainConceptGroupSelectorException.Create(Format(
        ResStr_NoComboItemSelected,
        ['cmbDomains']));
  // Extract the second half of the double-key stored in the combo box
  Result := Copy(cmbConceptGroups.CurrentStrID, 17, 16);
end;  // TfraDomainConceptGroupsSelector.GetHierarchyRelationTypeKey 

{-------------------------------------------------------------------------------
  Populates the concept group combo with a list using the supplied stored 
      procedure.  The result is either a list of concept groups or concept 
      group versions depending on the procedure supplied (defined by the status 
      of btnHistory).  AIncludeDates is true for the version list as a date 
      range is included in the label. 
}
procedure TfraDomainConceptGroupsSelector.PopulateConceptGroups(
    AStoredProcName: string; AIncludeDates: boolean);
var
  lItemCaption: string;
  lRecordset: _Recordset;
  lToDate: string;
begin
  lRecordset := dmGeneral.GetRecordset(AStoredProcName, [
      '@Domain', DomainKey
      ]);
  with lRecordset  do
    while not EOF do begin
      lItemCaption := Fields['Item_Name'].Value;
      if AIncludeDates then begin
        // Add version date range to caption
        lItemCaption := lItemCaption + ' (' +
            dmGeneral.GetVagueDateStringFromRecordset(lRecordset, 'From');
        // allow for the To date being null
        lToDate := dmGeneral.GetVagueDateStringFromRecordset(lRecordset, 'To');
        if lToDate = '' then
          lItemCaption := lItemCaption + ')'
        else
          lItemCaption := lItemCaption + ' - ' + lToDate + ')';
      end;
      //Concept group key and hiearchy relation type key are both stored on the comb box
      cmbConceptGroups.Add(
          lItemCaption,
          VarToStr(Fields['Item_Key'].Value) + VarToStr(Fields['Hierarchy_Relation_Type_Key'].Value));
      MoveNext;
    end; // while
end;  // TfraDomainConceptGroupsSelector.PopulateConceptGroups 

{-------------------------------------------------------------------------------
  Populates the Domain and Subject Area combo box, if they are unpopulated. 
}
procedure TfraDomainConceptGroupsSelector.PopulateDomainCombo;
var
  lSubjectArea: string;
  lComboItem: TDomainComboItem;
begin
  if cmbDomains.Items.Count=0 then begin
    lSubjectArea := '';
    with dmGeneral.GetRecordset('usp_SubjectAreaDomains_Select', []) do
      while not EOF do begin
        if lSubjectArea<>Fields['Subject_Area'].Value then begin
          lSubjectArea := Fields['Subject_Area'].Value;
          // starting new subject area, so add heading to combo box
          lComboItem := TDomainComboItem.Create;
          lComboItem.Initialise(itSubjectArea, '');
          cmbDomains.Items.AddObject(Fields['Subject_Area'].Value, lComboItem);
        end;
        // add nested domain
        lComboItem := TDomainComboItem.Create;
        lComboItem.Initialise(itDomain, Fields['Domain_Key'].Value);
        cmbDomains.Items.AddObject(Fields['Domain'].Value, lComboItem);
        MoveNext;
      end;
  end;
end;  // TfraDomainConceptGroupsSelector.PopulateDomainCombo 

{-------------------------------------------------------------------------------
  Selects the concept group in the combo box.  If it does not exist, raises an 
      error. 
}
procedure TfraDomainConceptGroupsSelector.SelectConceptGroup(const 
    AConceptGroupKey: string);
var
  lIdx: Integer;
  lFound: Boolean;
begin
  lFound := False;
  cmbConceptGroupsPopulate(cmbConceptGroups);
  with cmbConceptGroups do
    for lIdx := 0 to Count-1 do begin
      // Check first half of double key in the combo
      if Copy(StrID[lIdx], 1, 16)=AConceptGroupKey then begin
        ItemIndex := lIdx;
        if Assigned(cmbConceptGroups.OnChange) then
          cmbConceptGroups.OnChange(cmbConceptGroups);
        lFound := True;
        break; // no need to loop further
      end;
    end; // for
  if not lFound then
    raise EDomainConceptGroupSelectorException.Create(
        ResStr_RequestedConceptGroupUnavailable);
end;  // TfraDomainConceptGroupsSelector.SelectConceptGroup 

{-------------------------------------------------------------------------------
  Selects the domain in the combo box.  If it does not exist, raises an error. 
}
procedure TfraDomainConceptGroupsSelector.SelectDomain(const ADomainKey: 
    string);
var
  lIdx: Integer;
  lFound: Boolean;
begin
  lFound := False;
  PopulateDomainCombo;
  for lIdx := 0 to cmbDomains.Items.Count-1 do
    if TDomainComboItem(cmbDomains.Items.Objects[lIdx]).ItemKey=ADomainKey then
        begin
      cmbDomains.ItemIndex := lIdx;
      cmbDomainsSelect(cmbDomains);
      lFound := True;
      break; // no need to loop further
    end;
  if not lFound then
    raise EDomainConceptGroupSelectorException.Create(
        ResStr_RequestedDomainUnavailable);
end;  // TfraDomainConceptGroupsSelector.SelectDomain

{-------------------------------------------------------------------------------
  Sets the concept group label according to the selected domain.  For example,
      this might be Term List, Classification or Checklist.
}
procedure TfraDomainConceptGroupsSelector.SetConceptGroupLabel;
var
  lCaption: Variant;
begin
  lCaption := Null;
  lCaption := dmGeneral.GetStoredProcOutputParam(
      'usp_GetConceptGroupLabel', [
      '@Domain', TDomainComboItem(
          cmbDomains.Items.Objects[cmbDomains.ItemIndex]).ItemKey,
      '@Language', AppSettings.ISOLanguage], '@ConceptGroupLabel');

  if VarIsNull(lCaption) then lCaption := ResStr_ConceptGroup;

  lblConceptGroup.Caption := GetTextWithinLimit(
                                    lblConceptGroup.Canvas,
                                    lCaption + ':',
                                    lblConceptGroup.Width);
end;  // TfraDomainConceptGroupsSelector.SetConceptGroupLabel

{-------------------------------------------------------------------------------
  Sets the value of the Enabled property.
}
procedure TfraDomainConceptGroupsSelector.SetEnabled(Value: Boolean);
begin
  inherited;
  lblDomain.Enabled := Value;
  lblConceptGroup.Enabled := Value;
  cmbDomains.Enabled := Value;
  cmbConceptGroups.Enabled := Value;
end;

{-------------------------------------------------------------------------------
  Clears the selections in the drop-down lists.
}
procedure TfraDomainConceptGroupsSelector.Clear;
begin
  cmbDomains.ItemIndex := -1;
  cmbConceptGroups.ItemIndex := -1;
  lblConceptGroup.Caption := ResStr_ConceptGroup;
end;

end.
