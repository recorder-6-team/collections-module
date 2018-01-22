{===============================================================================
  Unit:        FrameTask.pas

  Defines:     TfraTask

  Description: Tasks details.

  Created:     May 2003

  Last revision information:
    $Revision: 6 $
    $Date: 19/02/04 16:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit FrameTask;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings,
  FrameTaskCollectionUnits;

type
  {-----------------------------------------------------------------------------
    Details page control for viewing and editing a task record.  This frame consists of 
    an empty tab control onto which the following tabs are embedded in this order: 
    TfraTaskGeneral, TfraSources.
    Tasks are identified as part of the process of performing a conservation condition 
    check.  They are created by adding them to condition checks.  When the Tasks 
    Identified node within a condition check is selected, the Collections Browser's Add 
    button is enabled and allows a new Task to be created.  This is the only means of 
    creating a new task.  Tasks can subsequently be linked to a single existing job.
    When Tasks Identified are listed in the Collections Browser, the Browser's Add button 
    is disabled.
  }
  TfraTask = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameTaskGeneral, FrameSources;

{-==============================================================================
    TfraTask
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraTask.AddTabs;
begin
  AddTab('General', TfraTaskGeneral);
  AddTab('Material Acquired', TfraTaskCollectionUnits);
  AddTab('Sources', TfraSources);
end;  // TfraTask.AddTabs 

end.

