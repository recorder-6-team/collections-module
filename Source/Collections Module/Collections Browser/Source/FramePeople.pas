{===============================================================================
  Unit:        FramePeople.pas

  Defines:     fraPeople

  Description: Container frame for People/Organisation details.

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/04/04 16:03 $
    $Author: Anthonysimpson $

===============================================================================}

unit FramePeople;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, LuxembourgConstants;

type
  {-----------------------------------------------------------------------------
    Empty page control that has the following tabs embedded to display details of a person or 
    organisation associated with a collection unit:
    TfraPeopleGeneral (displayed where the person was identified as associated with the 
    collection unit table through the presence of a record in Collection_Unit_Individual)
    TfraPeopleSpCollected (displayed when the top-level node is a Collection and the 
    associated name is a person)
    TfraPeopleSpDetermined (displayed when the top-level node is a Collection and the 
    associated name is a person)
  }
  TfraPeople = class (TBasePageControlFrame)
  protected
    procedure LoadData; override;
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FramePeopleGeneral, FramePeopleSpCollected, FramePeopleSpDetermined,
  BaseTabSheetFrameUnit, ResourceStrings, GeneralData;

{-==============================================================================
    TfraPeople
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraPeople.AddTabs;
begin
  AddTab('General', TfraPeopleGeneral);
  AddTab('Specimens Collected', TfraPeopleSpCollected);
  AddTab('Specimens Determined', TfraPeopleSpDetermined);
end;  // TfraPeople.AddTabs 

{-------------------------------------------------------------------------------
  TfraPeopleGeneral (displayed where the person was identified as associated with the 
      collection unit table through the presence of a record in Collection_Unit_Individual) 
}
procedure TfraPeople.LoadData;
begin
  pcDetails.Pages[0].TabVisible := dmGeneral.GetStoredProcOutputParam(
          'usp_CollectionUnitName_Exists_Get',
          ['@CollectionUnitKey', ParentKey, '@NameKey', Key], '@Exists') <> 0;
  inherited;
end;  // TfraPeople.LoadData 

end.
