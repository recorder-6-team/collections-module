{===============================================================================
  Unit:        FrameHistory

  Defines:     TfraHistory

  Model:       CollectionsCollectionsAndStorage.mpb

  Description:

  Created:     May 2003

  Last revision information:
    $Revision: 6 $
    $Date: 9/02/04 17:28 $
    $Author: Bencollier $

===============================================================================}

unit FrameHistory;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Screen for viewing and editing an item of History attached to any collection unit.  
    History items represent events that are known to have occurred during the collection 
    unit's lifetime that affected the collection unit.  Empty tab page control onto which the 
    following tabs are embedded as required:
    TfraHistoryGeneral, TfraSources.
  }
  TfraHistory = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameHistoryGeneral, FrameSources;

{-==============================================================================
    TfraHistory
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraHistory.AddTabs;
begin
  AddTab('General', TfraHistoryGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraHistory.AddTabs 

end.


