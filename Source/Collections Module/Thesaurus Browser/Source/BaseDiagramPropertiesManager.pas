{===============================================================================
  Unit:        BaseDiagramPropertiesManager

  Defines:     TdlgBaseDiagramPropertiesManager

  Description: Base class dialog for the dialogs used to configure display 
      properties of
               objects on the Thesaurus diagrams.

  Model:       Thesaurus.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/02/04 11:43 $
    $Author: Johnvanbreda $

===============================================================================}
unit BaseDiagramPropertiesManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InterfaceDataModule, StdCtrls, ImageListButton, ComCtrls,
  ExceptionForm;

type
  EBaseDiagramPropertiesManagerException = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Base class dialog for the dialogs used to configure display properties of 
    objects on the Thesaurus diagrams.
  }
  TdlgBaseDiagramPropertiesManager = class (TForm)
    btnApply: TImageListButton;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    pcTabs: TPageControl;
    tsDiagram: TTabSheet;
    tsGroup: TTabSheet;
    tsItem: TTabSheet;
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  public
    constructor Create(AOwner : TComponent); override;
  end;
  
var
  dlgBaseDiagramPropertiesManager: TdlgBaseDiagramPropertiesManager;

implementation

{$R *.dfm}

uses
  ThesaurusDiagram, ResourceStrings, BaseDiagramObjectProperties;

{-==============================================================================
    TdlgBaseDiagramPropertiesManager
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgBaseDiagramPropertiesManager.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  If not (AOwner is TThesaurusDiagram) then
    raise EBaseDiagramPropertiesManagerException.Create(Format(
        ResStr_InvalidMethodCall, [
        'TdlgBaseDiagramPropertiesManager.Create']));
end;  // TdlgBaseDiagramPropertiesManager.Create 

{-------------------------------------------------------------------------------
}
procedure TdlgBaseDiagramPropertiesManager.btnApplyClick(Sender: TObject);
var
  lIdx: Integer;
begin
  for lIdx := 0 to ComponentCount-1 do
    if Components[lIdx] is TfraBaseDiagramObjectProperties then
      TfraBaseDiagramObjectProperties(Components[lIdx]).Validate;
  for lIdx := 0 to ComponentCount-1 do
    if Components[lIdx] is TfraBaseDiagramObjectProperties then
      TfraBaseDiagramObjectProperties(Components[lIdx]).ApplyChanges;
  TThesaurusDiagram(Owner).Invalidate;
end;  // TdlgBaseDiagramPropertiesManager.btnApplyClick 

{-------------------------------------------------------------------------------
}
procedure TdlgBaseDiagramPropertiesManager.btnOkClick(Sender: TObject);
begin
  btnApplyClick(Sender);
end;  // TdlgBaseDiagramPropertiesManager.btnOkClick 

end.



