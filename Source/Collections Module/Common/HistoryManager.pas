{===============================================================================
  Unit:           HistoryManager

  Defines:        TBaseHistoryItem
                  THistoryList
                  THistoryManager

  Description:    Classes for managing screen history

  Model:          ScreenHistory.mpb

  Created:        September 2003

  Last revision information:
    $Revision: 9 $
    $Date: 9/02/04 16:31 $
    $Author: Johnvanbreda $

===============================================================================}
unit HistoryManager;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, GeneralFunctions, ExceptionForm, Menus, ComCtrls,
  InterfaceDataModule, ActnList;

type
  EHistoryException = class (TExceptionPath)
  end;
  
  THistoryDirection = (hdForward, hdBack);

  TBaseHistoryItem = class (TObject)
  private
    FCaption: String;
    FImageIndex: Integer;
    procedure SetCaption(const Value: String);
    procedure SetImageIndex(const Value: Integer);
  protected
    procedure Recall; virtual; abstract;
  public
    constructor Create;
    property Caption: String read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;
  
  THistoryList = class (TList)
  private
    function GetItems(AIndex: Integer): TBaseHistoryItem;
    procedure SetItems(AIndex: Integer; AItem: TBaseHistoryItem);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TBaseHistoryItem): Integer;
    procedure Clear; override;
    function Extract(AItem: TBaseHistoryItem): TBaseHistoryItem;
    function First: TBaseHistoryItem;
    function IndexOf(AItem: TBaseHistoryItem): Integer;
    procedure Insert(AIndex: Integer; AItem: TBaseHistoryItem);
    function Last: TBaseHistoryItem;
    function Remove(AItem: TBaseHistoryItem): Integer;
    property Items[AIndex: Integer]: TBaseHistoryItem read GetItems write SetItems; default;
  end;
  
  TBaseHistoryItemClass = class of TBaseHistoryItem;

  {-----------------------------------------------------------------------------
    Class to manage the history buttons on a screen.
    The class encapsulates the popup menus and behaviour of the buttons.
  }
  THistoryManager = class (TObject)
  private
    FBackButton: TToolButton;
    FBackMenu: TPopupMenu;
    FCurrentPosition: Integer;
    FEnabled: Boolean;
    FForwardButton: TToolButton;
    FForwardMenu: TPopupMenu;
    FHistoryList: THistoryList;
    FImageList: TImageList;
    FMenuActions: TActionList;
    procedure BackButtonClick(Sender: TObject);
    procedure BackHistoryMenuClick(Sender: TObject);
    procedure BackMenuPopup(Sender: TObject);
    procedure ClearMenu(AMenu: TPopupMenu);
    procedure ForwardButtonClick(Sender: TObject);
    procedure ForwardHistoryMenuClick(Sender: TObject);
    procedure ForwardMenuPopup(Sender: TObject);
    function GetBackAction: TAction;
    function GetBackMenuHistoryCount: Integer;
    function GetBackMenuHistoryItem(Index: Integer): TBaseHistoryItem;
    function GetForwardAction: TAction;
    function GetForwardMenuHistoryCount: Integer;
    function GetForwardMenuHistoryItem(Index: Integer): TBaseHistoryItem;
    function NewTermMenuItem(AOwner: TComponent; ATag: integer): TMenuItem;
    procedure SelectItem(ADirection: THistoryDirection; AIndex: integer);
    procedure SetBackButton(Value: TToolButton);
    procedure SetEnabled(Value: Boolean);
    procedure SetForwardButton(Value: TToolButton);
    procedure SetImageList(const Value: TImageList);
    procedure UpdateHistoryButtons;
    property BackMenuHistoryCount: Integer read GetBackMenuHistoryCount;
    property BackMenuHistoryItem[Index: Integer]: TBaseHistoryItem read GetBackMenuHistoryItem;
    property ForwardMenuHistoryCount: Integer read GetForwardMenuHistoryCount;
    property ForwardMenuHistoryItem[Index: Integer]: TBaseHistoryItem read 
        GetForwardMenuHistoryItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItemClass: TBaseHistoryItemClass): TBaseHistoryItem;
    property BackAction: TAction read GetBackAction;
    property BackButton: TToolButton read FBackButton write SetBackButton;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ForwardAction: TAction read GetForwardAction;
    property ForwardButton: TToolButton read FForwardButton write SetForwardButton;
    property ImageList: TImageList read FImageList write SetImageList;
  end;
  

implementation

uses
  ResourceStrings;

const
  // number of items visible in the history menus at one time
  HISTORY_LIST_SIZE=9;

{-==============================================================================
    TBaseHistoryItem
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation, setup defaults. 
}
constructor TBaseHistoryItem.Create;
begin
  FImageIndex := -1;
end;  // TBaseHistoryItem.Create 

{-------------------------------------------------------------------------------
}
procedure TBaseHistoryItem.SetCaption(const Value: String);
begin
  FCaption := Value;
end;  // TBaseHistoryItem.SetCaption 

{-------------------------------------------------------------------------------
}
procedure TBaseHistoryItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;  // TBaseHistoryItem.SetImageIndex 

{-==============================================================================
    THistoryList
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor THistoryList.Create;
begin
  inherited Create;
end;  // THistoryList.Create 

{-------------------------------------------------------------------------------
}
destructor THistoryList.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // THistoryList.Destroy 

{-------------------------------------------------------------------------------
}
function THistoryList.Add(AItem: TBaseHistoryItem): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // THistoryList.Add 

{-------------------------------------------------------------------------------
}
procedure THistoryList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // THistoryList.Clear 

{-------------------------------------------------------------------------------
}
function THistoryList.Extract(AItem: TBaseHistoryItem): TBaseHistoryItem;
begin
  Result := TBaseHistoryItem(inherited Extract(AItem));
end;  // THistoryList.Extract 

{-------------------------------------------------------------------------------
}
function THistoryList.First: TBaseHistoryItem;
begin
  Result := TBaseHistoryItem(inherited First);
end;  // THistoryList.First 

{-------------------------------------------------------------------------------
}
function THistoryList.GetItems(AIndex: Integer): TBaseHistoryItem;
begin
  Result := TBaseHistoryItem(inherited Items[AIndex]);
end;  // THistoryList.GetItems 

{-------------------------------------------------------------------------------
}
function THistoryList.IndexOf(AItem: TBaseHistoryItem): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // THistoryList.IndexOf 

{-------------------------------------------------------------------------------
}
procedure THistoryList.Insert(AIndex: Integer; AItem: TBaseHistoryItem);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // THistoryList.Insert 

{-------------------------------------------------------------------------------
}
function THistoryList.Last: TBaseHistoryItem;
begin
  Result := TBaseHistoryItem(inherited Last);
end;  // THistoryList.Last 

{-------------------------------------------------------------------------------
}
function THistoryList.Remove(AItem: TBaseHistoryItem): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // THistoryList.Remove 

{-------------------------------------------------------------------------------
}
procedure THistoryList.SetItems(AIndex: Integer; AItem: TBaseHistoryItem);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // THistoryList.SetItems 

{-==============================================================================
    THistoryManager
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation. 
}
constructor THistoryManager.Create;
begin
  inherited Create;
  
  FHistoryList := THistoryList.Create;
  FCurrentPosition := -1;
  FMenuActions := TActionList.Create(nil);
  // Create 2 actions on the list
  TAction.Create(FMenuActions).ActionList := FMenuActions;
  TAction.Create(FMenuActions).ActionList := FMenuActions;
  // Setup the actions
  BackAction.Caption := ResStr_MnuBack;
  ForwardAction.Caption := ResStr_MnuForward;
  BackAction.OnExecute := BackButtonClick;
  ForwardAction.OnExecute := ForwardButtonClick;
  
  FBackMenu := TPopupMenu.Create(nil);
  FBackMenu.OnPopup := BackMenuPopup;
  FBackMenu.OwnerDraw := True;
  FBackMenu.AutoHotkeys := maManual;
  FForwardMenu := TPopupMenu.Create(nil);
  FForwardMenu.OnPopup := ForwardMenuPopup;
  FForwardMenu.OwnerDraw := True;
  FForwardMenu.AutoHotkeys := maManual;
  FEnabled := true;
end;  // THistoryManager.Create 

{-------------------------------------------------------------------------------
}
destructor THistoryManager.Destroy;
begin
  FHistoryList.Free;
  FBackMenu.Free;
  FForwardMenu.Free;
  FMenuActions.Free;
  
  inherited Destroy;
end;  // THistoryManager.Destroy 

{-------------------------------------------------------------------------------
  Adds an item to the history.  The current position is changed to point to the new item.
  If the user has navigated to a preceding history item, then the subsequent history 
      information is lost. 
}
function THistoryManager.Add(AItemClass: TBaseHistoryItemClass): TBaseHistoryItem;
var
  lIdx: Integer;
begin
  // Any subsequent items to the current one are removed.
  for lIdx := FHistoryList.Count-1 downto FCurrentPosition+1 do begin
    FHistoryList.Items[lIdx].Free;
    FHistoryList.Delete(lIdx);
  end;
  // Create the new item, add it to the list and make it current
  FHistoryList.Add(AItemClass.Create);
  FCurrentPosition := FHistoryList.Count-1;
  // Return the new item
  Result := FHistoryList.Items[FHistoryList.Count-1];
  //Update forward/back buttons
  UpdateHistoryButtons;
end;  // THistoryManager.Add 

{-------------------------------------------------------------------------------
  Moves back in the list and recalls the history item. 
}
procedure THistoryManager.BackButtonClick(Sender: TObject);
begin
  if FCurrentPosition>0 then begin
    Dec(FCurrentPosition);
    FHistoryList.Items[FCurrentPosition].Recall;
  end;
  UpdateHistoryButtons;
end;  // THistoryManager.BackButtonClick 

{-------------------------------------------------------------------------------
  Click handler for the Back history popup items.  Maps the item to the history manager using 
      the Tag which stores the index. 
}
procedure THistoryManager.BackHistoryMenuClick(Sender: TObject);
begin
  SelectItem(hdBack, TMenuItem(Sender).Tag);
end;  // THistoryManager.BackHistoryMenuClick 

{-------------------------------------------------------------------------------
  Populate the back history menu on demand. 
}
procedure THistoryManager.BackMenuPopup(Sender: TObject);
var
  lIdx: Integer;
  lNewItem: TMenuItem;
begin
  ClearMenu(FBackMenu);
  // Add the new ones
  for lIdx := 0 to BackMenuHistoryCount-1 do begin
    lNewItem := NewTermMenuItem(FBackMenu, lIdx);
    lNewItem.Caption := BackMenuHistoryItem[lIdx].Caption;
    lNewItem.OnClick := BackHistoryMenuClick;
    lNewItem.ImageIndex := BackMenuHistoryItem[lIdx].ImageIndex;
    FBackMenu.Items.Add(lNewItem);
  end;
end;  // THistoryManager.BackMenuPopup 

{-------------------------------------------------------------------------------
  Clears all items from the supplied menu. 
}
procedure THistoryManager.ClearMenu(AMenu: TPopupMenu);
var
  lIdx: Integer;
begin
  // Clear existing menu items
  for lIdx := AMenu.Items.Count-1 downto 0 do
    AMenu.Items[lIdx].Free;
end;  // THistoryManager.ClearMenu 

{-------------------------------------------------------------------------------
  Moves forward in the list and recalls the history item. 
}
procedure THistoryManager.ForwardButtonClick(Sender: TObject);
begin
  if FCurrentPosition<FHistoryList.Count-1 then begin
    Inc(FCurrentPosition);
    FHistoryList.Items[FCurrentPosition].Recall;
  end;
  UpdateHistoryButtons;
end;  // THistoryManager.ForwardButtonClick 

{-------------------------------------------------------------------------------
  Click handler for the Forward history popup items.  Maps the item to the history manager 
      using the Tag which stores the index. 
}
procedure THistoryManager.ForwardHistoryMenuClick(Sender: TObject);
begin
  SelectItem(hdForward, TMenuItem(Sender).Tag);
end;  // THistoryManager.ForwardHistoryMenuClick 

{-------------------------------------------------------------------------------
  Populate the back history menu on demand. 
}
procedure THistoryManager.ForwardMenuPopup(Sender: TObject);
var
  lIdx: Integer;
  lNewItem: TMenuItem;
begin
  ClearMenu(FForwardMenu);
  // Add the new ones
  for lIdx := 0 to ForwardMenuHistoryCount-1 do begin
    lNewItem := NewTermMenuItem(FForwardMenu, lIdx);
    lNewItem.Caption := ForwardMenuHistoryItem[lIdx].Caption;
    lNewItem.OnClick := ForwardHistoryMenuClick;
    lNewItem.ImageIndex := ForwardMenuHistoryItem[lIdx].ImageIndex;
    FForwardMenu.Items.Add(lNewItem);
  end;
end;  // THistoryManager.ForwardMenuPopup 

{-------------------------------------------------------------------------------
}
function THistoryManager.GetBackAction: TAction;
begin
  Result :=TAction(FMenuActions.Actions[0]);
end;  // THistoryManager.GetBackAction 

{-------------------------------------------------------------------------------
  Accessor method.  Returns the number of items in the previous item menu. 
}
function THistoryManager.GetBackMenuHistoryCount: Integer;
begin
  Result := Min(HISTORY_LIST_SIZE, FCurrentPosition);
end;  // THistoryManager.GetBackMenuHistoryCount 

{-------------------------------------------------------------------------------
  Retrieves a history item for the back popup menu at the supplied position. 
}
function THistoryManager.GetBackMenuHistoryItem(Index: Integer): TBaseHistoryItem;
begin
  // First item in list is most recent.
  Result := FHistoryList.Items[FCurrentPosition-Index-1];
end;  // THistoryManager.GetBackMenuHistoryItem 

{-------------------------------------------------------------------------------
}
function THistoryManager.GetForwardAction: TAction;
begin
  Result :=TAction(FMenuActions.Actions[1]);
end;  // THistoryManager.GetForwardAction 

{-------------------------------------------------------------------------------
  Accessor method.  Number of items in the Forward menu. 
}
function THistoryManager.GetForwardMenuHistoryCount: Integer;
begin
  Result := Min(HISTORY_LIST_SIZE, FHistoryList.Count - FCurrentPosition - 1);
end;  // THistoryManager.GetForwardMenuHistoryCount 

{-------------------------------------------------------------------------------
  Retrieves a history item for the forward popup menu at the supplied position. 
}
function THistoryManager.GetForwardMenuHistoryItem(Index: Integer): TBaseHistoryItem;
begin
  // First item in list is immediate subsequent item.
  Result := FHistoryList.Items[FCurrentPosition+Index+1];
end;  // THistoryManager.GetForwardMenuHistoryItem 

{-------------------------------------------------------------------------------
  Creates a new term menu item for the history menus and sets up standard properties. 
}
function THistoryManager.NewTermMenuItem(AOwner: TComponent; ATag: integer): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  Result.Tag := ATag;
  Result.OnDrawItem := dmInterface.TermMenuDrawItem;
end;  // THistoryManager.NewTermMenuItem 

{-------------------------------------------------------------------------------
  Moves the current position in the history list to the item at the selected index in the 
      forward or back menu list. 
}
procedure THistoryManager.SelectItem(ADirection: THistoryDirection; AIndex: integer);
var
  lNewPosition: Integer;
begin
  // Locate the correct item
  if ADirection=hdForward then
    lNewPosition := FCurrentPosition + AIndex + 1
  else
    lNewPosition := FCurrentPosition - AIndex - 1;
  if (lNewPosition >= FHistoryList.Count) or (lNewPosition < 0) then
    raise EHistoryException.Create(ResStr_HistoryItemNotFound);
  FCurrentPosition := lNewPosition;
  FHistoryList.Items[FCurrentPosition].Recall;
  UpdateHistoryButtons;
end;  // THistoryManager.SelectItem 

{-------------------------------------------------------------------------------
  Accessor method.  Sets the back button linked to the history. 
}
procedure THistoryManager.SetBackButton(Value: TToolButton);
begin
  FBackButton := Value;
  
  FBackButton.OnClick := BackButtonClick;
  FBackButton.DropDownMenu := FBackMenu;
end;  // THistoryManager.SetBackButton 

{-------------------------------------------------------------------------------
  Accessor method.  If disabled, buttons are permanently disabled. 
}
procedure THistoryManager.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  
  UpdateHistoryButtons;
end;  // THistoryManager.SetEnabled 

{-------------------------------------------------------------------------------
  Accessor method.  Sets the forward button linked to the history. 
}
procedure THistoryManager.SetForwardButton(Value: TToolButton);
begin
  FForwardButton := Value;
  
  FForwardButton.OnClick := ForwardButtonClick;
  FForwardButton.DropDownMenu := FForwardMenu;
end;  // THistoryManager.SetForwardButton 

{-------------------------------------------------------------------------------
}
procedure THistoryManager.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  
  FForwardMenu.Images := Value;
  FBackMenu.Images := Value;
end;  // THistoryManager.SetImageList 

{-------------------------------------------------------------------------------
  Updates the enabled and disabled states of the linked history buttons. 
}
procedure THistoryManager.UpdateHistoryButtons;
begin
  if Assigned(FBackButton) then
    FBackButton.Enabled := (BackMenuHistoryCount>0) and FEnabled;
  if Assigned(FForwardButton) then
    FForwardButton.Enabled := (ForwardMenuHistoryCount>0) and FEnabled;
  BackAction.Enabled := (BackMenuHistoryCount>0) and FEnabled;
  ForwardAction.Enabled := (ForwardMenuHistoryCount>0) and FEnabled;
end;  // THistoryManager.UpdateHistoryButtons 


end.
