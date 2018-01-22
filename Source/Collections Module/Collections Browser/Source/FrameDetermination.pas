{===============================================================================
  Unit:        FrameDetermination.pas

  Defines:     TfraDetermination

  Description:

  Model:       CollectionsSpecimens.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 5 $
    $Date: 9/02/04 17:42 $
    $Author: Bencollier $

===============================================================================}

unit FrameDetermination;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, DataTypes,
  BaseDetailFrameUnit, BasePageControlFrameUnit, BaseTabSheetFrameUnit;

type
  {-----------------------------------------------------------------------------
    Contains details of a selected determination node.  Empty page control onto which the 
    following tab pages are embedded as required:
    TfraDeterminationGeneral
    TfraSources
  }
  TfraDetermination = class (TBasePageControlFrame)
  protected
    procedure AddTabs; override;
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
  AddTab('General', TfraDeterminationGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraDetermination.AddTabs 

{-------------------------------------------------------------------------------
}
procedure TfraDetermination.SetFrameProperties(AFrame: TBaseDetailFrame);
begin
  if AFrame is TfraDeterminationGeneral then
    TfraDeterminationGeneral(AFrame).MasterFrameType := mftCollectionUnit;
end;  // TfraDetermination.SetFrameProperties 

end.
