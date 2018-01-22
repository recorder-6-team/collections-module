{===============================================================================
  Unit:        FrameStorageLayout.pas

  Defines:     TfraStorageLayout

  Description: Frame displaying read only store layout diagram

  Model:       StoreLayoutDiagram

  Created:     September 2004

  Last revision information:
    $Revision: 6 $
    $Date: 2/02/07 16:25 $
    $Author: Ericsalmon $

===============================================================================}
unit FrameStorageLayout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  StoreLayoutDiagram, CollectionsBrowser_TLB, DataTypes;

type
  {-----------------------------------------------------------------------------
    Tab page control allowing the user to view the layout diagram for a store.
  }
  TfraStorageLayout = class(TBaseTabSheetFrame, IRefreshTab)
    btnEdit: TImageListButton;
    btnNavigateUp: TImageListButton;
    procedure btnEditClick(Sender: TObject);
    procedure btnNavigateUpClick(Sender: TObject);
  private
    FParentStore: String;
    pnlDiagram: TStoreLayoutDiagram;
    procedure DiagramNavigate(Sender: TObject; const AKey: string);
  protected
    procedure EnableControls(AEnabled: Boolean); override;
    procedure LoadData; override;
    procedure RefreshTab; safecall;
    procedure SaveData; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
var
  fraStorageLayout: TfraStorageLayout;

implementation

uses
  GeneralData, InterfaceDataModule;

{$R *.dfm}

{-==============================================================================
    TfraStorageLayout
===============================================================================}
{-------------------------------------------------------------------------------
  Object constructor 
}
constructor TfraStorageLayout.Create(AOwner: TComponent);
begin
  inherited;
  pnlDiagram := TStoreLayoutDiagram.Create(Self);
  with pnlDiagram do begin
    Parent := Self;
    SetBounds(12, 12, 341, 217);
    Enabled := False;
    OnNavigate := DiagramNavigate;
  end;
end;  // TfraStorageLayout.Create 

{-------------------------------------------------------------------------------
  Displays frmLayoutEditor for the current store. 
}
procedure TfraStorageLayout.btnEditClick(Sender: TObject);
begin
  inherited;
  with dmGeneral.Recorder.ShowActiveForm(
      CLASS_frmStorageLayout) as IfrmStorageLayout do begin
    StoreKey := Self.Key;
    Caller := Self;
  end;  // TfraStorageLayout.btnEditClick
end;  // TfraStorageLayout.btnEditClick 

{-------------------------------------------------------------------------------
  Enabled if the current store node in the main hierarchy has the usual location specified.
  Displays the top level node for the store location identified by the usual location field,
      and selects the Layout tab.
}
procedure TfraStorageLayout.btnNavigateUpClick(Sender: TObject);
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etNavigateStore, VarArrayOf(['Key', FParentStore]));
end;  // TfraStorageLayout.btnNavigateUpClick 

{-------------------------------------------------------------------------------
}
procedure TfraStorageLayout.EnableControls(AEnabled: Boolean);
begin
  inherited;
  btnEdit.Enabled := True;
  pnlDiagram.Enabled := True;
  pnlDiagram.ReadOnly := True;
  // Navigate button should be disabled when editing
  btnNavigateUp.Enabled := (not AEnabled) and (FParentStore <> '');
end;  // TfraStorageLayout.EnableControls

{-------------------------------------------------------------------------------
  Load the appropriate diagram.
}
procedure TfraStorageLayout.LoadData;
var
  lXml: String;
begin
  pnlDiagram.Zoom := 0.42;  // Moved from constructor.

  with dmGeneral.GetRecordset('usp_Store_LayoutXML_Select', ['@Key', Key]) do
    if not Eof then
      lXML := VarToStr(Fields['Diagram_XML'].Value);
  pnlDiagram.ReadXML(lXML);
  with dmGeneral.GetRecordset('usp_Store_Select', ['@Key', Key]) do
    if not Eof then
      FParentStore := VarToStr(Fields['Current_Container_Collection_Unit_Key'].Value);
  btnNavigateUp.Enabled := FParentStore <> '';
end;  // TfraStorageLayout.LoadData 

{-------------------------------------------------------------------------------
  When requested (because the Apply Changes button was clicked on a diagram editor), reload
      the diagram.
}
procedure TfraStorageLayout.RefreshTab;
begin
  LoadData;
end;  // TfraStorageLayout.RefreshTab 

{-------------------------------------------------------------------------------
}
procedure TfraStorageLayout.SaveData;
begin
  inherited;
  // no implementation, this frame is never editable.
end;  // TfraStorageLayout.SaveData

procedure TfraStorageLayout.DiagramNavigate(Sender: TObject;
  const AKey: string);
begin
  if Assigned(OnFrameNotification) then
    OnFrameNotification(Self, etNavigateStore, VarArrayOf(['Key', AKey]));
end;

end.
