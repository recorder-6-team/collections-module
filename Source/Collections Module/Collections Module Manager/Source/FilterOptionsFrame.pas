{===============================================================================
  Unit:        FilterOptionsFrame

  Defines:     TfraFilterOptions

  Description:

  Model:       CollectionsModuleManager.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 18/05/06 13:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit FilterOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst;

type
  TfraFilterOptions = class (TFrame)
    chklbDomains: TCheckListBox;
    Label14: TLabel;
    procedure chklbDomainsClickCheck(Sender: TObject);
  public
    procedure Load;
    procedure Save;
  end;
  
//==============================================================================
implementation

uses
  ApplicationSettings, GeneralData, LuxembourgConstants, DataTypes,
  GeneralFunctions, ResourceStrings;

{$R *.dfm}

{-==============================================================================
    TfraFilterOptions
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFilterOptions.Load;
var
  lIdx: Integer;
begin
  with dmGeneral.GetRecordset('usp_BrowseDomains_Select_ForUser',
                              ['@NameKey', AppSettings.UserID]) do
  begin
    while not Eof do begin
      lIdx := chklbDomains.Items.AddObject(Fields['Item_Name'].Value,
                                           TObject(Integer(Fields['Domain_Mask'].Value)));
      if Fields['Domain_Mask'].Value and AppSettings.DomainMask > 0 then
        chklbDomains.Checked[lIdx] := True;
      MoveNext;
    end;
    Close;
  end;
end;  // TfraFilterOptions.Load 

{-------------------------------------------------------------------------------
}
procedure TfraFilterOptions.Save;
var
  lMask: Integer;
  i: Integer;
begin
  // If list disabled, user can't change anything.
  if chklbDomains.Enabled then begin
    lMask := 0;
    with chklbDomains do
      for i := 0 to Items.Count - 1 do
        if Checked[i] then lMask := lMask or Integer(Items.Objects[i]);

    AppSettings.DomainMask := lMask;
  end;
end;  // TfraFilterOptions.Save 

{-------------------------------------------------------------------------------
  If the user checks a domain which shares a mask with other domains, then check
    it is Ok to also check them.  Also applies to unchecking.
}
procedure TfraFilterOptions.chklbDomainsClickCheck(Sender: TObject);
var
  lMask: integer;
  lChecked: boolean;
  lItemsToUpdate: string;
  i: integer;
begin
  lMask := Integer(chklbDomains.Items.Objects[chklbDomains.ItemIndex]);
  lChecked := chklbDomains.Checked[chklbDomains.ItemIndex];
  // Now check if any items need to be updated
  lItemsToUpdate := '';
  for i := 0 to chklbDomains.Items.Count-1 do
    // if Item shares same domain and is in wrong state
    if (Integer(chklbDomains.Items.Objects[i])=lMask) and
        (chklbDomains.Checked[i]<>lChecked) then
      AddToCommaSeparatedList(lItemsToUpdate, chklbDomains.Items[i]);
  if lItemsToUpdate<>'' then
    if MessageDlg(Format(ResStr_ChangeDomainViewAffectsOthers, [lItemsToUpdate]),
        mtWarning, [mbOk, mbCancel], 0) = mrOk then begin
      for i := 0 to chklbDomains.Items.Count-1 do
        if (Integer(chklbDomains.Items.Objects[i])=lMask) then
          chklbDomains.Checked[i] := lChecked
    end else
      // revert the checkbox
      chklbDomains.Checked[chklbDomains.ItemIndex] := not lChecked;
end;

end.



