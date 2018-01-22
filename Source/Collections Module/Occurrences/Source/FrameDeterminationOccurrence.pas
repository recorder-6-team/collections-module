{===============================================================================
  Unit:        FrameDeterminationOccurrence.pas

  Defines:     TfraDeterminationOccurrence

  Description:

  Model:       Occurrences.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 2 $
    $Date: 8/12/03 15:29 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameDeterminationOccurrence;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit;

type
  TfraDetermination = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
    function GetCaption: String; override;
    procedure SetFrameProperties(AFrame: TBaseDetailFrame); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameDeterminationGeneral, FrameSources, ResourceStrings;

{-==============================================================================
    TfraDetermination
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDetermination.AddTabs;
begin
  AddTab(ResStr_General, TfraDeterminationGeneral);
  AddTab(ResStr_Sources, TfraSources);
end;  // TfraDetermination.AddTabs 

{-------------------------------------------------------------------------------
}
function TfraDetermination.GetCaption: String;
begin
  if Tabs[0].ContainedFrame is TfraDeterminationGeneral then
    Result := TfraDeterminationGeneral(Tabs[0].ContainedFrame).Caption
  else
    raise EBrowserFrameError.Create(ResStr_InvalidTabPage);
end;  // TfraDetermination.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraDetermination.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraDeterminationGeneral then
    TfraDeterminationGeneral(AFrame).MasterFrameType := mftOccurrence;
end;  // TfraDetermination.SetFrameProperties 

end.

