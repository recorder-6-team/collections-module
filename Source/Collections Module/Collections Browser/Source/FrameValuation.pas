{===============================================================================
  Unit:        FrameValuation.pas

  Defines:     TfraValuation

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 5 $
    $Date: 9/02/04 17:22 $
    $Author: Bencollier $

===============================================================================}

unit FrameValuation;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Empty page control that has the following tabs embedded to display details of a valuation:
    TfraValuationGeneral
    TfraSources
  }
  TfraValuation = class (TBasePageControlFrame)
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameValuationGeneral, FrameSources;

{-==============================================================================
    TfraValuation
===============================================================================}
{-------------------------------------------------------------------------------
  Load all contained frames in separate tabsheets. 
}
procedure TfraValuation.AddTabs;
begin
  AddTab('General', TfraValuationGeneral);
  AddTab('Sources', TfraSources);
end;  // TfraValuation.AddTabs 

end.

