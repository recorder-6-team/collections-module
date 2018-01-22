{===============================================================================

  Copyright © Dorset Software Services Ltd, 1999

  Unit:
    InHouseReg7.pas - Eric Salmon 27/07/2002

  Registration of the following components:
    TColorButton     in ColorBtn
    TFolderBrowser   in FolderBrowser
    TListSelect      in ComboListSelect
    TComboSelect     in ComboListSelect
    TDateEdit        in DateEdit
    TRestrictedEdit  in RestrictedEdits
    TNumberEdit      in RestrictedEdits
    TPhoneNumber     in RestrictedEdits
    TImageListButton in ImageListButton
    THotlabel        in HotLabel
    TXPToolBar			 in XPToolButton
    TXPToolButton  	 in XPToolButton

  Updates:
    TDssStringGrid in DssStringGrid

  Packages:
    InHouse7, Delphi 7 package for in house components.

  Description:
    Registers all TESTED and STABLE components going on the In House palette
    tab, removing the need for each component to have its own Register function.
    Glyphs for the components are stored in the associated component resouce
    file InHouseReg5.dcr.

    See individual units for more information on each components.

===============================================================================}

unit InHouseReg7;

interface

uses
  Classes, ColorBtn, FolderBrowser, ComboListSelect, RestrictedEdits, DssStringGrid,
  DateEdit, ComboListID, SortListView, GroupBoxEx, ImageListButton, HotLabel,
  XPToolButton, LicenceCheck, ImgList, ImageIndexPropEd, DesignIntf,
  ControlStringGrid;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('In House', [TColorButton,
                                  TFolderBrowser,
                                  TListSelect,
                                  TLicence,
                                  TComboSelect,
                                  TIDListBox,
                                  TIDComboBox,
                                  TDateEdit,
                                  TRestrictedEdit,
                                  TNumberEdit,
                                  TPhoneNumber,
                                  TSortListView,
                                  TGroupBoxEx,
                                  TImageListButton,
                                  THotLabel,
                                  TXPToolBar,
                                  TXPToolButton,
                                  TDssStringGrid,
                                  TControlStringGrid]);

  RegisterPropertyEditor(TypeInfo(TImageIndex), TImageListButton, 'ImageIndex',
                         TImageIndexPropertyEditor);
end;  { Register }

//==============================================================================
end.

