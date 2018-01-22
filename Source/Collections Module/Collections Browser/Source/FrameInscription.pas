{===============================================================================
  Unit:        FrameInscription.pas

  Defines:     TfraFrameInscription

  Description: Container frame for inscription related data.

  Created:     May 2003

  Last revision information:
    $Revision: 3 $
    $Date: 9/02/04 17:43 $
    $Author: Bencollier $

===============================================================================}

unit FrameInscription;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Details screen for an inscription or label attached to a specimen.  Empty page 
    control onto which the following tabs are embedded:
    TfraInscriptionGeneral
    TfraSources
  }
  TfraInscription = class (TBasePageControlFrame)
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameInscriptionGeneral, FrameSources;

{-==============================================================================
    TfraInscription
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraInscription.AddTabs;
begin
  AddTab('General', TfraInscriptionGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraInscription.AddTabs 

end.
