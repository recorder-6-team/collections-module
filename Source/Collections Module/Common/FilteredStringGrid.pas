{===============================================================================
  Unit:        FilteredStringGrid

  Defines:     TPopupButton
                TFilteredStringGrid

  Description: A DSS String Grid which can contain, for any columm, buttons
                which each display a popup menu. This does not contain any
                methods for performing filtering - this should be handled by the
                frame containing the grid. This class merely provides a
                mechanism for attaching drop down menus to the column headers.

  Created:     May 2011

  Last revision information:
    $Revision: 2 $
    $Date: 25/05/11 10:22 $
    $Author: Jamesbichard $

===============================================================================}

unit FilteredStringGrid;

interface

uses
  DSSStringGrid, Contnrs, StdCtrls, Grids, Windows, Menus,
  Controls, Classes, SysUtils, ImageListButton;

type
  TPopupButton = class(TImageListButton)
  private
    FColumn: Integer;
    FMenu: TPopupMenu;
  protected
    procedure DisplayMenu(Sender: TObject);
  public
    procedure Init(
      AColumn: Integer;
      AMenu: TPopupMenu);
    procedure PositionFilter(ARect: TRect);
    property Column: Integer read FColumn write FColumn;
    property Menu: TPopupMenu read FMenu write FMenu;
  end;

  TFilteredStringGrid = class(TDSSStringGrid)
  private
    { Private declarations }
    FFilters: TObjectList;

  protected
    { Protected declarations }
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    { Public declarations }
    procedure AddFilter(AFilter: TPopupButton);
  published
    { Published declarations }
  end;

implementation

{-------------------------------------------------------------------------------
}
procedure TPopupButton.Init(
      AColumn: Integer;
      AMenu: TPopupMenu);
begin
  Column := AColumn;
  Menu := AMenu;
  OnClick := DisplayMenu;
end;

{-------------------------------------------------------------------------------
}
procedure TPopupButton.DisplayMenu(Sender: TObject);
var
  lPoint: TPoint;
begin
  lPoint := ClientToScreen(Point(0,Height));
  FMenu.Popup(lPoint.X, lPoint.Y);
end;

{-------------------------------------------------------------------------------
}
procedure TPopupButton.PositionFilter(ARect: TRect);
begin
  Left := ARect.Right - 15;
  Top := ARect.Bottom - 15;
end;

{-------------------------------------------------------------------------------
}
procedure TFilteredStringGrid.AddFilter(AFilter: TPopupButton);
begin
  AFilter.PositionFilter(CellRect(AFilter.Column, 0));
  AFilter.Height := 15;
  AFilter.Width := 15;
  if not Assigned(FFilters) then
    FFilters := TObjectList.Create;
  FFilters.Add(AFilter);

end;

{-------------------------------------------------------------------------------
}
procedure TFilteredStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  i: Integer;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);

  if Assigned(FFilters) and (ARow = 0) then
  begin
    for i := 0 to FFilters.Count - 1 do
    begin
      if ACol = TPopupButton(FFilters.Items[i]).Column then
      begin
        TPopupButton(FFilters.Items[i]).PositionFilter(CellRect(ACol, 0));
      end;
    end;
  end;
end;

end.
