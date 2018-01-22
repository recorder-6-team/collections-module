{===============================================================================
  Unit:        FrameContainerAsFrame

  Defines:     TfraContainer

  Description: Container frame into which other details frames are embedded
               on the Collections Browser.  Contains the Title label.

  Model:       CollectionBrowserGeneral.mpb

  Created:     September 2003
===============================================================================}

unit FrameContainerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, DataTypes, BaseDetailFrameUnit, RapTree,
  TermLabel, UserMessages, ExtCtrls, DSSDataTypes;

type
  TOnRefreshScreenInfo = procedure (Sender: TObject; const AKey, ACaption: string) of object;
  TOnRefreshNodeCaption = procedure (Sender: TObject; const ACaption: string) of object;
  {-----------------------------------------------------------------------------
    Frame that all details frames are embedded into.  This frame is then in turn embedded
    onto TfrmCBMain.  The frame implements the Selected Item label at the top of each
    details frame as well as the Save and Cancel buttons that appear beneath each details
    frame.
  }
  TfraContainer = class(TFrame)
    pnlCaption: TPanel;
    lblHeading: TLabel;
    lblName: TTermLabel;
    pnlButtons: TPanel;
    btnSave: TImageListButton;
    btnCancel: TImageListButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FContainedFrame: TBaseDetailFrame;
    FContainedFrameClass: TBaseDetailFrameClass;
    FEditMode: TEditMode;
    FOnEditModeChange: TNotifyEvent;
    FOnFrameNotification: TFrameNotificationEvent;
    FOnRefreshNodeCaption: TOnRefreshNodeCaption;
    FOnRefreshScreenInfo: TOnRefreshScreenInfo;
    FTimer: TTimer;
    FUseFullClientArea: Boolean;
    procedure EnableButtons(AEnabled: boolean);
    procedure InvalidSpatialRefHandler(Sender: TObject; var Handled: Boolean);
    procedure LinkSpatialRefHandler;
    procedure SetEditMode(const Value: TEditMode);
    procedure SetOnFrameNotification(Value: TFrameNotificationEvent);
    procedure SetOnRefreshScreenInfo(Value: TOnRefreshScreenInfo);
    procedure TimerFinished(Sender: TObject);
    procedure WMRefreshScreenInfo(var Message: TMessage); message WM_REFRESH_SCREEN_INFO;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelChanges;
    procedure LoadContent(AContainedFrameClass: TBaseDetailFrameClass; AAdditionalProperties:
        TObject; AUseFullClientArea: boolean = False; ANode: TFlyNode = nil);
    procedure SaveChanges;
    procedure UnloadFrames;
    property ContainedFrame: TBaseDetailFrame read FContainedFrame;
    property EditMode: TEditMode read FEditMode write SetEditMode;
    property OnEditModeChange: TNotifyEvent read FOnEditModeChange write FOnEditModeChange;
    property OnFrameNotification: TFrameNotificationEvent read FOnFrameNotification write
        SetOnFrameNotification;
    property OnRefreshNodeCaption: TOnRefreshNodeCaption read FOnRefreshNodeCaption write
        FOnRefreshNodeCaption;
    property OnRefreshScreenInfo: TOnRefreshScreenInfo read FOnRefreshScreenInfo write
        SetOnRefreshScreenInfo;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, GeneralData, InterfaceDataModule,
  SpatialRef, LuxembourgConstants;

{-==============================================================================
    TfraContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FEditMode := emBrowse;
  EnableButtons(False);
  
  // Create the timer and set it up.
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 250; // .25s
  FTimer.OnTimer := TimerFinished;
end;  // TfraContainer.Create 

{-------------------------------------------------------------------------------
}
destructor TfraContainer.Destroy;
begin
  // Unload frames first to avoid Control has no parent window issues.
  UnloadFrames;
  
  inherited Destroy;
end;  // TfraContainer.Destroy 

{-------------------------------------------------------------------------------
  Cancels changes and asks contained frame to reload its content. Sets the EditMode back to
      Browse mode.
}
procedure TfraContainer.btnCancelClick(Sender: TObject);
begin
  CancelChanges;
end;  // TfraContainer.btnCancelClick 

{-------------------------------------------------------------------------------
  Asks the contained frame to validate its data and, if successful, save it. Sets EditMode
      back to Browse mode.
}
procedure TfraContainer.btnSaveClick(Sender: TObject);
begin
  SaveChanges;
end;  // TfraContainer.btnSaveClick 

{-------------------------------------------------------------------------------
  Public method to cancel changes. Can be called directly without to rely on the name of the
      cancel button.
}
procedure TfraContainer.CancelChanges;
begin
  EnableButtons(False);
  try
    // Give contained frame an opportunity to respond
    FContainedFrame.CancelChanges;
  
    { This bit is mainly used when Specimen is added as a Store, or anything like that. Both
      itemsThey share the same key, even though one is brand new. So, to properly discard the
      new item, its key needs to be reset to nothing first. }
    if FContainedFrame.AdditionalProperties.GetProperty(PROP_DISCARD_ON_CANCEL) = True then
      FContainedFrame.Key := '';
  
    if FContainedFrame.Key='' then begin
      EditMode := emBrowse;
      // A new frame was being edited, so discard the entire frame and node.
      PostMessage(Handle, WM_REFRESH_SCREEN_INFO, 0, 0);
    end
    else begin
      // Switch back to browse mode.
      EditMode := emBrowse;
      // And as existing frame was being edited, refresh it to discard all changes
      FContainedFrame.ReloadContent;
    end;
  except on Exception do
    EnableButtons(True);
  end;
end;  // TfraContainer.CancelChanges 

{-------------------------------------------------------------------------------
  Fix to allow the frame to be used as/in an ActiveXControl.
}
procedure TfraContainer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (Parent = nil) and IsLibrary and not (csDestroying in ComponentState) then
    Params.WndParent := ParentWindow;
end;  // TfraContainer.CreateParams 

{-------------------------------------------------------------------------------
  Enable or disable cancel and save buttons.
}
procedure TfraContainer.EnableButtons(AEnabled: boolean);
begin
  btnSave.Enabled := AEnabled;
  btnCancel.Enabled := AEnabled;
end;  // TfraContainer.EnableButtons 

{-------------------------------------------------------------------------------
  Handler for InvalidSpatialRef event.  Determines if the invalid spatial ref should be
      ignored, in the case of clicking the Cancel button.
}
procedure TfraContainer.InvalidSpatialRefHandler(Sender: TObject; var Handled: Boolean);
var
  lPos: TPoint;
begin
  lPos := btnCancel.ScreenToClient(Mouse.CursorPos);
  
  // Check if clicked Cancel
  Handled := (lPos.X in [0..btnCancel.Width]) and (lPos.Y in [0..btnCancel.Height]);
end;  // TfraContainer.InvalidSpatialRefHandler 

{-------------------------------------------------------------------------------
  Find any TSpatialRef component in any contained frames, any level, and link the
      OnInvalidSpatialRef event to private handler. This allows user to click the Cancel
      button wihout the spatial ref component getting in the way.
}
procedure TfraContainer.LinkSpatialRefHandler;
  
  procedure FindSpatialRefComponent(AParent: TWinControl);
  var i: Integer;
  begin
    // Check each control on parent
    for i := AParent.ControlCount - 1 downto 0 do
      // If parent is TWinControl, it can contain more controls, so recurse
      if AParent.Controls[i] is TWinControl then begin
        FindSpatialRefComponent(TWinControl(AParent.Controls[i]));
        // Now check is control is frame, and if so, free it.
        if AParent.Controls[i] is TSpatialRef then
          TSpatialRef(AParent.Controls[i]).OnInvalidSpatialRef := InvalidSpatialRefHandler;
      end;
  end;
  
begin
  FindSpatialRefComponent(Self);
end;  // TfraContainer.LinkSpatialRefHandler 

{-------------------------------------------------------------------------------
  Procedure responsible for instantiating the content frame and ligning it inside the form. If
      the content frame is normally a tabsheet, but is used as standalone frame, its
      ResizeFrame method is called so that it can make use of the additional space.
  The frame is then asked to load its content.
}
procedure TfraContainer.LoadContent(AContainedFrameClass: TBaseDetailFrameClass;
    AAdditionalProperties: TObject; AUseFullClientArea: boolean = False; ANode: TFlyNode =
    nil);
begin
  if not (FContainedFrame is AContainedFrameClass) then begin
    UnloadFrames;
    FContainedFrame := nil;
  end;
  
  FUseFullClientArea := AUseFullClientArea;
  
  if not Assigned(FContainedFrame) then begin
    FContainedFrameClass := AContainedFrameClass;
    FContainedFrame := AContainedFrameClass.Create(Owner);
    //To Allows properties to be available to TBasePageControlFrame, must set
    //properties before parenting because TBasePageControlFrame.CreateWnd calls AddTabs
    //where these properties might be used.
    FContainedFrame.AssociatedContainerNode := ANode;
    FContainedFrame.SetAdditionalProperties(AAdditionalProperties);
    // Set parent, so it appears on the form.
    FContainedFrame.Parent := Self;
    FContainedFrame.TabOrder := 0;
    FContainedFrame.OnFrameNotification := OnFrameNotification;
    FContainedFrame.Align := alClient;
    pnlButtons.Visible := not AUseFullClientArea;
    pnlCaption.Visible := not AUseFullClientArea;
  end
  else begin
    FContainedFrame.AssociatedContainerNode := ANode;
    FContainedFrame.SetAdditionalProperties(AAdditionalProperties);
  end;
  
  // Get the frame to load its data.
  FContainedFrame.LoadContent;
  LinkSpatialRefHandler;
  // Set EditMode, this triggers stuff in Frame too.
  EditMode := emBrowse;
end;  // TfraContainer.LoadContent 

{-------------------------------------------------------------------------------
  Public method to save changes. Can be called directly without to rely on the name of the
      save button.
}
procedure TfraContainer.SaveChanges;
var
  lHoldCursor: TCursor;
  lCaption: String;
begin
  lHoldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  
  try
    EnableButtons(False);
    try
      // Tell the frame to save its data.
      FContainedFrame.SaveContent;
      if Assigned(FOnRefreshScreenInfo) then
        FOnRefreshScreenInfo(Self, FContainedFrame.Key, '');
      // And switch back to browse mode.
      EditMode := emBrowse;
      // Caption is now set separately from the rest. This is because
      // there are some cases where it needs to be done separately, e.g.
      // the Accession number can only be retrieved for a new Collection
      // after UpdateNodeRelationship has been called and it has inserted
      // the necessary join records. RefreshNodeInfo was previously taking
      // a caption, running UpdateNodeRelationship, then setting the old
      // caption. Now we can create joins, retrieve acc number, get correct
      // caption and then assign it to the node.
      lCaption := FContainedFrame.Caption;
      if Assigned(FOnRefreshNodeCaption) then
        FOnRefreshNodeCaption(Self, lCaption);
      // Reload the frame to update timestamps
      FContainedFrame.ReloadContent;
      lblName.Caption := lCaption;
    except
      on Exception do begin
        // Start the timer. Stops user double clicking save button and getting
        // validation message twice.
        FTimer.Enabled := True;
        raise;
      end;
    end; // try..except
  finally
    Screen.Cursor := lHoldCursor;
  end;
end;  // TfraContainer.SaveChanges 

{-------------------------------------------------------------------------------
  Propagate the EditMode setting (Browse/Edit) to the contained frame.
}
procedure TfraContainer.SetEditMode(const Value: TEditMode);
begin
  if FEditMode <> Value then begin
    FEditMode := Value;
    EnableButtons(Boolean (Value));
    // Event triggered only when EditMode has actually changed.
    if Assigned(FOnEditModeChange) then
      FOnEditModeChange(Self);
  end;
  if Assigned(FContainedFrame) then FContainedFrame.EditMode := EditMode;
end;  // TfraContainer.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TfraContainer.SetOnFrameNotification(Value: TFrameNotificationEvent);
begin
  FOnFrameNotification := Value;
end;  // TfraContainer.SetOnFrameNotification 

{-------------------------------------------------------------------------------
  Accessor method.  Event that is triggered when there is new summary information about the
      screen (i.e. a change in caption or key).
}
procedure TfraContainer.SetOnRefreshScreenInfo(Value: TOnRefreshScreenInfo);
begin
  FOnRefreshScreenInfo := Value;
end;  // TfraContainer.SetOnRefreshScreenInfo 

{-------------------------------------------------------------------------------
  Come here once the timer has finished. This will stop the ID 3434 happening. If you double
      clicked on the Save button when validation was going to fail, and this was the first
      validation message you were going to see in the session, you would get the validation
      message twice because two clicks were getting through. The small delay stops this
      occurring.
}
procedure TfraContainer.TimerFinished(Sender: TObject);
begin
  EnableButtons(True);
  FTimer.Enabled := False;
end;  // TfraContainer.TimerFinished 

{-------------------------------------------------------------------------------
}
procedure TfraContainer.UnloadFrames;
  
  procedure FindFrames(AParent: TWinControl);
  var i: Integer;
  begin
    // Check each control on parent
    for i := AParent.ControlCount - 1 downto 0 do
      // If parent is TWinControl, it can contain more controls, so recurse
      if AParent.Controls[i] is TWinControl then begin
        FindFrames(TWinControl(AParent.Controls[i]));
        // Now check is control is frame, and if so, free it.
        if AParent.Controls[i] is TFrame then
          AParent.Controls[i].Free;
      end;
  end;
  
begin
  FindFrames(Self);
end;  // TfraContainer.UnloadFrames 

{-------------------------------------------------------------------------------
}
procedure TfraContainer.WMRefreshScreenInfo(var Message: TMessage);
begin
  if Assigned(FOnRefreshScreenInfo) then
    FOnRefreshScreenInfo(Self, '', '');
end;  // TfraContainer.WMRefreshScreenInfo 

end.


