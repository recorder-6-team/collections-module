{===============================================================================
  Unit:        BaseIncludedCollectionUnits.pas

  Defines:     TBaseIncludedCollectionUnits

  Description:

  Model:       CollectionsMovements

  Created:     February 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/03/04 14:10 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseCollectionUnitsIncluded;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, CheckLst, exgrid,
  RapTree, DataTypes, DataClasses, GeneralData, InterfaceDataModule;

type
  {-----------------------------------------------------------------------------
    Stores the key of a node, as well as whether the nodes original checked state.
  }
  TKeyAndModified = class (TKey)
  private
    FChecked: Boolean;
  public
    property Checked: Boolean read FChecked write FChecked;
  end;
  
  {-----------------------------------------------------------------------------
    Base frame used by TfraTaskCollectionUnits and TfraMovementDetailsCollectionUnits.
  }
  TBaseIncludedCollectionUnits = class (TBaseTabSheetFrame)
    chklbCollections: TCheckListBox;
    chklbSpecimens: TCheckListBox;
    chklbStorage: TCheckListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    splitCollSpec: TSplitter;
    splitSpecStore: TSplitter;
    procedure ClickCheckChange(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: 
        TOwnerDrawState);
  private
    FChkBoxReadOnly: Boolean;
    procedure AddNode(const AKey: TKeyString; AItemName: String; ACollectionUnitType: Integer; 
        AIncluded: Boolean);
    procedure ClearCheckListBox(ACheckListBox: TCheckListBox);
    procedure InternalPopulateCheckBoxes;
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure SaveCheckBoxData(ACheckListBox: TCheckListBox); virtual; abstract;
    procedure SaveData; override;
  public
    destructor Destroy; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ADOInt, LuxembourgConstants;

{-==============================================================================
    TBaseIncludedCollectionUnits
===============================================================================}
{-------------------------------------------------------------------------------
  Call a method to clear each checklist box. 
}
destructor TBaseIncludedCollectionUnits.Destroy;
begin
  ClearCheckListBox(chklbCollections);
  ClearCheckListBox(chklbSpecimens);
  ClearCheckListBox(chklbStorage);
  inherited;
end;  // TBaseIncludedCollectionUnits.Destroy 

{-------------------------------------------------------------------------------
  Add a node to the correct checklist box and give it the information it requires. 
}
procedure TBaseIncludedCollectionUnits.AddNode(const AKey: TKeyString; AItemName: String; 
    ACollectionUnitType: Integer; AIncluded: Boolean);
var
  lIndex: Integer;
  lKey: TKeyAndModified;
  lListBox: TCheckListBox;
begin
  lKey := TKeyAndModified.Create(AKey);
  
  case ACollectionUnitType of
    0: lListBox := chklbCollections;
    1: lListBox := chklbSpecimens;
    2: lListBox := chklbStorage;
  else
    Exit;
  end;
  
  lIndex := lListBox.Items.Add(AItemName);
  lKey.Checked := AIncluded;
  lListBox.Items.Objects[lIndex] := lKey;
  lListBox.Checked[lIndex]       := AIncluded;
end;  // TBaseIncludedCollectionUnits.AddNode 

{-------------------------------------------------------------------------------
  Go through the items in the checklist box freeing the objects associated with them. Then 
      clear the items. 
}
procedure TBaseIncludedCollectionUnits.ClearCheckListBox(ACheckListBox: TCheckListBox);
var
  i: Integer;
begin
  with ACheckListBox.Items do begin
    for i := 0 to Count - 1 do
      Objects[i].Free;
    Clear;
  end;
end;  // TBaseIncludedCollectionUnits.ClearCheckListBox 

{-------------------------------------------------------------------------------
  If the user 
      isn't in edit mode, then undo a click on the checkbox. The reason for this is because the checklist boxes don't have a read-only mode. The 'enabled' property won't suffice because, if the checklist boxes aren't enabled, you cannot scroll around in them. 
}
procedure TBaseIncludedCollectionUnits.ClickCheckChange(Sender: TObject);
begin
  inherited;
  with TCheckListBox(Sender) do
    if FChkBoxReadOnly then
      Checked[ItemIndex] := not Checked[ItemIndex]  // Undo the change
end;  // TBaseIncludedCollectionUnits.ClickCheckChange 

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TBaseIncludedCollectionUnits.EnableControls(AEnabled: Boolean);
begin
  inherited;
  FChkBoxReadOnly := not AEnabled;
end;  // TBaseIncludedCollectionUnits.EnableControls 

{-------------------------------------------------------------------------------
  Go through the recordset and call a method to add nodes to the relevant checklist box. 
}
procedure TBaseIncludedCollectionUnits.InternalPopulateCheckBoxes;
begin
  with RegisteredRecordsets[0] do begin
    while not EOF do begin
      AddNode(VarToStr(Fields['Collection_Unit_Key'].Value),
              VarToStr(Fields['Item_Name'].Value),
              Integer(Fields['Collection_Unit_Type'].Value),
              Boolean(Fields['Included'].Value));
      MoveNext;
    end;
  end;
end;  // TBaseIncludedCollectionUnits.InternalPopulateCheckBoxes 

{-------------------------------------------------------------------------------
}
procedure TBaseIncludedCollectionUnits.ListDrawItem(Control: TWinControl; Index: Integer; 
    Rect: TRect; State: TOwnerDrawState);
begin
  inherited;
  if Control is TCheckListBox then
    with TCheckListBox(Control) do
      dmInterface.DrawTerm(Canvas, Rect, Items[Index], Index = ItemIndex);
end;  // TBaseIncludedCollectionUnits.ListDrawItem 

{-------------------------------------------------------------------------------
  Clear the checklist boxes of anything they may contain, and then populate them again. 
}
procedure TBaseIncludedCollectionUnits.LoadData;
begin
  inherited;
  ClearCheckListBox(chklbCollections);
  ClearCheckListBox(chklbSpecimens);
  ClearCheckListBox(chklbStorage);
  InternalPopulateCheckBoxes;
end;  // TBaseIncludedCollectionUnits.LoadData 

{-------------------------------------------------------------------------------
  Goes through the checklist boxes calling a method to save their contents to the database. 
}
procedure TBaseIncludedCollectionUnits.SaveData;
begin
  SaveCheckBoxData(chklbCollections);
  SaveCheckBoxData(chklbSpecimens);
  SaveCheckBoxData(chklbStorage);
end;  // TBaseIncludedCollectionUnits.SaveData 

end.




