{===============================================================================
  Unit:        ConceptGeneral

  Defines:     TfraConceptGeneral

  Description: Frame for editing and viewing any fields for a concept that
               aren't covered elsewhere

  Created:     June 2003

  Last revision information:
    $Revision: 2 $
    $Date: 3/07/03 9:16 $
    $Author: Johnvanbreda $

===============================================================================}
unit ConceptGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, StdCtrls, ExtCtrls, Menus, Buttons,
  ImageListButton, ComCtrls, InterfaceDataModule;

type
  {-----------------------------------------------------------------------------
    General details page for aspects of a concept that are not handled through 
    other entities.  This is embedded onto TfraConceptDetails when required.
  }
  TfraConceptGeneral = class (TBaseTabSheetFrame)
    btnInsertBotanical: TImageListButton;
    btnItalic: TImageListButton;
    btnTermFind: TImageListButton;
    chkPreferred: TCheckBox;
    cmbLanguage: TComboBox;
    cmbNameType: TComboBox;
    cmbRank: TComboBox;
    eListCode: TEdit;
    eSortCode: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    lblLanguage: TLabel;
    lblTerm: TLabel;
    lblUnformatted: TLabel;
    pmBotanicalNames: TPopupMenu;
    pmTermBotanicalNames: TMenuItem;
    reTerm: TRichEdit;
    procedure btnInsertBotanicalClick(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
  protected
    procedure EnableControls(AEnabled: Boolean); override;
  public
    procedure SaveContent; override;
  end;
  

implementation

{-==============================================================================
    TfraConceptGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.btnInsertBotanicalClick(Sender: TObject);
var
  lMenuLocation: TPoint;
begin
  lMenuLocation := btnInsertBotanical.ClientToScreen(Point(0, btnInsertBotanical.Height));
  pmBotanicalNames.Popup(lMenuLocation.X, lMenuLocation.Y);
end;  // TfraConceptGeneral.btnInsertBotanicalClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.btnItalicClick(Sender: TObject);
var
  lStyles: TFontStyles;
begin
  inherited;
  lStyles := reTerm.SelAttributes.Style;
  if fsItalic in lStyles then
    Exclude(lStyles, fsItalic)
  else
    Include(lStyles, fsItalic);
  reTerm.SelAttributes.Style := lStyles;
end;  // TfraConceptGeneral.btnItalicClick 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.EnableControls(AEnabled: Boolean);
begin
end;  // TfraConceptGeneral.EnableControls 

{-------------------------------------------------------------------------------
}
procedure TfraConceptGeneral.SaveContent;
begin
end;  // TfraConceptGeneral.SaveContent 

{$R *.dfm}

end.





