{===============================================================================
  Unit:        DebuggerAnalysis

  Defines:     TdlgDebuggerAnalysis

  Description: Displays the results of a debugger session using TDebugger

  Created:     September 2003

  Model: m:\dssvl132\Debugger.mpb

  Last revision information:
    $Revision: $
    $Date: $
    $Author: $

===============================================================================}
unit DebuggerAnalysis;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, exgrid, RapTree, ExtCtrls, Grids, Contnrs, DebuggerTracker,
  ImgList, ComCtrls;

type
  {-----------------------------------------------------------------------------
    Dialog displaying the results of a debugger session using the TDebugger class.
  }
  TdlgDebuggerAnalysis = class (TForm)
    ilTree: TImageList;
    lvCalls: TListView;
    sgSummary: TStringGrid;
    Splitter1: TSplitter;
  public
    procedure SetMethodTrack(AMethodTrack: TObjectList);
  end;
  

implementation

const SPACES='                                                                ';

{-==============================================================================
    TdlgDebuggerAnalysis
===============================================================================}
{-------------------------------------------------------------------------------
  Assigns the list of calls to the form.  Updates the display. 
}
procedure TdlgDebuggerAnalysis.SetMethodTrack(AMethodTrack: TObjectList);
var
  lIdx: Integer;
  lNewItem: TListItem;
  lIndent: Integer;
begin
  lvCalls.Items.Clear;
  lIndent := 0;
  for lIdx := 0 to AMethodTrack.Count-1 do begin
    lNewItem := lvCalls.Items.Add;
    if AMethodTrack.Items[lIdx] is TMethodEvent then
      if TMethodEvent(AMethodTrack.Items[lIdx]).ExitProc then
        Dec(lIndent);
    lNewItem.Caption := Copy(SPACES, 1, lIndent*4) + TEvent(AMethodTrack.Items[lIdx]).Title;
    if AMethodTrack.Items[lIdx] is TExceptionEvent then
      lNewItem.ImageIndex := 2
    else if AMethodTrack.Items[lIdx] is TMessageEvent then
      lNewItem.ImageIndex := -1
    else if AMethodTrack.Items[lIdx] is TMethodEvent then begin
      if TMethodEvent(AMethodTrack.Items[lIdx]).ExitProc then
        lNewItem.ImageIndex := 1
      else begin
        lNewItem.ImageIndex := 0;
        Inc(lIndent);
      end;
    end;
  end;
end;  // TdlgDebuggerAnalysis.SetMethodTrack 

{$R *.dfm}

end.



