{===============================================================================
  Unit:        BasePageControlFrameUnit.pas

  Defines:     TBasePageControlFrame

  Description: Container frame. Has a TPageControl where the TabSheetFrames are
               loaded. TabSheets are created before creating a new frame onto it.

  Model:       CollectionBrowserFramework.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 34 $
    $Date: 2/09/11 16:04 $
    $Author: Jamesbichard $

===============================================================================}

unit BasePageControlFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDetailFrameUnit, StdCtrls, ImageListButton, ExtCtrls, ComCtrls,
  DataTypes, DataClasses;

type
  {-----------------------------------------------------------------------------
    Store TabSheets info, such as caption, container frame class and whether the frame has
    been loaded inside the tabsheet. This allows delay-loading of the pages, speeding up
    the whole loading of the screen.
  }
  TTabInfo = record
    ContainedFrame: TBaseDetailFrame;
    ContainedFrameClass: TBaseDetailFrameClass;
    IsLoaded: Boolean;
    TabSheet: TTabSheet;
  end;

  TTabInfoArray = Array of TTabInfo;

  {-----------------------------------------------------------------------------
    Base frame for screens made up of a tabbed notebook and multiple tab pages.  Details
    frames descended from this class define the tab pages that are embedded onto the tabbed
    notebook at run-time.
  }
  TBasePageControlFrame = class(TBaseDetailFrame)
    pcDetails: TPageControl;
    procedure pcDetailsChange(Sender: TObject);
  private
    FTabs: TTabInfoArray;
    FTabsCreated: Boolean;
    procedure ChildFrameNotification(Sender: TObject; AType: TEventType; const AParams:
        TVariantArray);
  protected
    procedure AddTab(const TabSheetCaption: String; DetailFrameClass: TBaseDetailFrameClass);
    procedure AddTabs; virtual; abstract;
    procedure CreateWnd; override;
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure LoadPage(APageIndex: Integer; ALoadFrameData: Boolean = True); virtual;
    procedure SaveData; override;
    procedure SaveTabContents; virtual;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); virtual;
    procedure SetParentKey(const Value: TKeyString); override;
    procedure ValidateTabContents; virtual;
    property Tabs: TTabInfoArray read FTabs;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CancelChanges; override;
    procedure DropInterfaceReferences; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings;

{-==============================================================================
    TBasePageControlFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBasePageControlFrame.Create(AOwner: TComponent);
begin
  FTabsCreated := False;
  
  inherited Create(AOwner);
end;  // TBasePageControlFrame.Create 

{-------------------------------------------------------------------------------
  Creates a new tab page within pcDetails and assigns the TabSheetCaption parameter value to
      its caption property.  The tab sheet is linked to the details frame class and the record
      key, but the details frame class is not actually created and embedded onto the tab sheet
      until the user views the tab sheet.  This means that the user only loads data for tab
      sheets they actually view, minimising database access and optimising browse performance.
}
procedure TBasePageControlFrame.AddTab(const TabSheetCaption: String; DetailFrameClass:
    TBaseDetailFrameClass);
var
  lTabSheet: TTabSheet;
begin
  inherited;
  // Add page
  lTabSheet := TTabSheet.Create(pcDetails);
  lTabSheet.PageControl := pcDetails;
  lTabSheet.Caption     := TabSheetCaption;
  
  // Store some values so that the frame object can be created when needed.
  SetLength(FTabs, pcDetails.PageCount);
  with FTabs[High(FTabs)] do begin
    IsLoaded            := False;             // Flag for delayed loading of data
    ContainedFrame      := nil;               // will be created on demand
    ContainedFrameClass := DetailFrameClass;  // and we need to know what to create then.
    TabSheet            := lTabSheet;       // Where to put the frame
  end;
end;  // TBasePageControlFrame.AddTab 

{-------------------------------------------------------------------------------
  Propagate the call to all tabs.
}
procedure TBasePageControlFrame.CancelChanges;
var
  i: Integer;
begin
  inherited CancelChanges;
  
  for i := 0 to High(FTabs) do
    if FTabs[i].IsLoaded then
      FTabs[i].ContainedFrame.CancelChanges;
end;  // TBasePageControlFrame.CancelChanges 

{-------------------------------------------------------------------------------
  Event handler to relay fram notification events from contained frames to the outer container
      control.
}
procedure TBasePageControlFrame.ChildFrameNotification(Sender: TObject; AType: TEventType;
    const AParams: TVariantArray);
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Sender, AType, AParams);
end;  // TBasePageControlFrame.ChildFrameNotification 

{-------------------------------------------------------------------------------
  Use CreateWnd to post a message to this form to delay initialisation of drag and drop until
      everything is ready.  CreateWnd works rather than Create because a handle is available.
}
procedure TBasePageControlFrame.CreateWnd;
begin
  inherited CreateWnd;
  
  // Ensure tabs are created only once, as CreateWnd called each time a handle
  // is created
  if not FTabsCreated then begin
    AddTabs;
    FTabsCreated := True;
  end; // if
end;  // TBasePageControlFrame.CreateWnd 

{-------------------------------------------------------------------------------
}
procedure TBasePageControlFrame.DeleteData;
var
  i: Integer;
begin
  // Delete each frame, starting from the last, as the first one is supposed to be the
  // "master" record, and therefore should be deleted last.
  for i := High(FTabs) downto 0 do begin
    // Need each frame to exist before calling the DeleteContent
    if not FTabs[i].IsLoaded then
      LoadPage(i, False);
    // Call DeleteContent on each frame. Override in class as and when required.
    FTabs[i].ContainedFrame.DeleteContent;
  end;
end;  // TBasePageControlFrame.DeleteData 

{-------------------------------------------------------------------------------
  Ensure child tabs also drop references to IAdditionalProperties when requested
}
procedure TBasePageControlFrame.DropInterfaceReferences;
var
  lIdx: Integer;
begin
  inherited DropInterfaceReferences;
  
  for lIdx := 0 to High(FTabs) do
    if Assigned(FTabs[lIdx].ContainedFrame) then
      FTabs[lIdx].ContainedFrame.DropInterfaceReferences;
end;  // TBasePageControlFrame.DropInterfaceReferences 

{-------------------------------------------------------------------------------
  Overriden method to propagate the EditMode state changes to the frames loaded in the
      PageControl control.
  This method replaces the default EnableControls implementation in the TBaseDetailFrame for
      this particular object, but notifies each of the embedded tab pages to enable or disable
      their controls.  This means that the TBasePageControlFrame instance does not take
      responsibility for enabling controls in each of the embedded tab pages, but delegates
      responibility to the tab pages.  Therefore tab pages are able to override the
      EnableControls method without risk of affecting other tab pages on the same form.
}
procedure TBasePageControlFrame.EnableControls(AEnabled: Boolean);
var
  i: Integer;
begin
  // Pass on to each loaded frame
  for i := 0 to High(FTabs) do
    if FTabs[i].IsLoaded then
      FTabs[i].ContainedFrame.EditMode := EditMode;
end;  // TBasePageControlFrame.EnableControls 

{-------------------------------------------------------------------------------
  
  Whenever the caption is required it will get it from the first tab. This has been changed
      because previously it stored the caption in FCaption when saving the data. However there
      was a problem when the caption was being stored before UpdateNodeRelationship had run
      and inserted its Join records. To generate some of the captions, the join records have
      to already be in place, so we need to be able to get the caption from the frame later (
      e.g. getting the accession number for a new collection).
}
function TBasePageControlFrame.GetCaption: String;
begin
  // Get the caption from BaseDetailFrame as well just in case there isn't
  // an FTabs[0].ContainedFrame.
  
  Result := inherited GetCaption;
  
  if Assigned(FTabs[0].ContainedFrame) then
    Result := FTabs[0].ContainedFrame.Caption;
end;  // TBasePageControlFrame.GetCaption 

{-------------------------------------------------------------------------------
  Loads the data in the displayed frame.  Called when changing record or reloading current
      record (after changes cancelled).  All loaded frames are reset to unloaded. The first
      frame is reloaded as well so that the timestamp is upto date after a save.
}
procedure TBasePageControlFrame.LoadData;
var
  i: Integer;
begin
  for i := 0 to High(FTabs) do
    FTabs[i].IsLoaded := False;
  
  // Doing an insert, force focus on first page, the one allowed to actually do an insert.
  if Key = '' then pcDetails.ActivePageIndex := 0;
  
  // The first tab page always needs to be loaded so that the correct timestamp
  // is currently being stored. The first tab page might not always be at
  // index 0, because it could be that the tab page at index 0 is hidden, because
  // tab page 1 should be shown as the general frame (see FrameMovementDetails).
  // Hence, loop through the tab pages until we get to the first visible one
  // and load this, because this is the one that will have the timestamp.
  for i := 0 to pcDetails.PageCount - 1 do
    if (pcDetails.Pages[i].TabVisible) then begin
      LoadPage(i);
      Break;
    end;
  
  // And now get the data for the new active page (if the current active page
  // hasn't just been loaded above).
  if i <> pcDetails.ActivePageIndex then
    LoadPage(pcDetails.ActivePageIndex);
end;  // TBasePageControlFrame.LoadData 

{-------------------------------------------------------------------------------
  A frame is created if it is displayed for the first time, and its data is loaded is it hasn't been already.
}
procedure TBasePageControlFrame.LoadPage(APageIndex: Integer; ALoadFrameData: Boolean = True);
begin
  with FTabs[APageIndex] do begin
    // If frame hasn't been created, do it.
    if not Assigned(ContainedFrame) then begin
      ContainedFrame := ContainedFrameClass.Create(Owner);
      ContainedFrame.Parent := TabSheet;
      ContainedFrame.Align  := alClient;
      ContainedFrame.OnFrameNotification := ChildFrameNotification;
    end;
  
    // In case contained frame needs additional things setup, do it here.
    SetFrameProperties(ContainedFrame);
    // Setup link to interface.
    if ContainedFrame.AdditionalProperties <> AdditionalProperties then
      ContainedFrame.SetAdditionalProperties(FAdditionalProperties);
  
    // If data hasn't been loaded, and is requested, or should be reloaded, do it.
    if (not IsLoaded) and ALoadFrameData then begin
      IsLoaded := True;
      // Supposedly it's ready to be loaded.
      ContainedFrame.LoadContent;
    end;
    // Make sure the loaded frame's mode is correct
    ContainedFrame.EditMode := EditMode;
  end;
end;  // TBasePageControlFrame.LoadPage 

{-------------------------------------------------------------------------------
  Loads data onto activated tab page by calling LoadPage.
}
procedure TBasePageControlFrame.pcDetailsChange(Sender: TObject);
begin
  inherited;
  LoadPage(pcDetails.ActivePageIndex);
end;  // TBasePageControlFrame.pcDetailsChange 

{-------------------------------------------------------------------------------
  Calls SaveContent on each of the details frame class instances that have been embedded onto
      tab pages.
}
procedure TBasePageControlFrame.SaveData;
begin
  // Default to empty string, then if General tab has not been loaded, the
  // caption will not update.
  FCaption := '';
  ValidateTabContents;
  SaveTabContents;
end;  // TBasePageControlFrame.SaveData 

{-------------------------------------------------------------------------------
}
procedure TBasePageControlFrame.SaveTabContents;
var
  I: Integer;
begin
  for i := 0 to High(FTabs) do
    if FTabs[i].IsLoaded then begin
      // Mostly because of inserts, need to make sure the frame key is up to
      // date
      FTabs[i].ContainedFrame.Key := Key;
      FTabs[i].ContainedFrame.SaveContent;
      // Really necessary only after saving first page, it will have the new key
      if i = 0 then begin
        Key := FTabs[0].ContainedFrame.Key;
      end;
    end;
end;  // TBasePageControlFrame.SaveTabContents 

{-------------------------------------------------------------------------------
}
procedure TBasePageControlFrame.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  { Do nothing in base class. Override if a contained frame needs additional
    properties set before content gets loaded (see Metadata for example).
  }
end;  // TBasePageControlFrame.SetFrameProperties 

{-------------------------------------------------------------------------------
}
procedure TBasePageControlFrame.SetParentKey(const Value: TKeyString);
var
  i: Integer;
begin
  inherited;
  // Save each loaded frame, those that weren't loaded weren't changed.
  for i := 0 to High(FTabs) do
    if FTabs[i].IsLoaded then
      if Assigned(FTabs[i].ContainedFrame) then
        FTabs[i].ContainedFrame.ParentKey := ParentKey;
end;  // TBasePageControlFrame.SetParentKey 

{-------------------------------------------------------------------------------
}
procedure TBasePageControlFrame.ValidateTabContents;
var
  I: Integer;
begin
  for i := 0 to High(FTabs) do
    if FTabs[i].IsLoaded then begin
      // Mostly because of inserts, need to make sure the frame key is up to
      // date
      FTabs[i].ContainedFrame.Key := Key;
      FTabs[i].ContainedFrame.ValidateContent;
      // Really necessary only after saving first page, it will have the new key
      if i = 0 then begin
        Key := FTabs[0].ContainedFrame.Key;
      end;
    end;
end;  // TBasePageControlFrame.ValidateTabContents 


end.



