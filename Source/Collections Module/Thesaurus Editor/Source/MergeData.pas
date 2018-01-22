{===============================================================================
  Unit:        MergeData

  Defines:     TfrmMergeData

  Description: Base form class for MDI windows in the Thesaurus editor

  Created:     January 2007

  Last revision information:
    $Revision: 2 $
    $Date: 29/01/07 17:30 $
    $Author: Ericsalmon $

===============================================================================}
unit MergeData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, BaseMDIChildUnit, InterfaceDataModule, ResourceStrings,
  BaseDragFrameUnit, FrameMergeData;

type
  {-----------------------------------------------------------------------------
    Base class for MDI child screens in the Thesaurus Browser.
  }
  TfrmMergeData = class(TBaseMDIChild)
    fraMergeData: TfraMergeData;
  private
    procedure fraMergeDataProgressChange(AProgress: Integer; ATotal: Integer);
    procedure fraMergeDataStatusChange(const AMessage: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoPaste;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ThesaurusEditorMain;

{-==============================================================================
    TfrmMergeData
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMergeData.Create(AOwner: TComponent);
begin
  inherited;

  fraMergeData.OnProgressChange := fraMergeDataProgressChange;
  fraMergeData.OnStatusChange := fraMergeDataStatusChange;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMergeData.fraMergeDataProgressChange(AProgress: Integer; ATotal: Integer);
begin
  frmThesaurusEditor.SetProgress(AProgress, ATotal);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMergeData.fraMergeDataStatusChange(const AMessage: String);
begin
  frmThesaurusEditor.SetStatus(AMessage);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmMergeData.DoPaste;
begin
  if (ActiveControl = fraMergeData.tvSourceItem)
  or (ActiveControl = fraMergeData.tvTargetItem) then
    fraMergeData.ExecutePaste(ActiveControl);
end;

end.
