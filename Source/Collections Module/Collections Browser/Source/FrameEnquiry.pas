{===============================================================================
  Unit:        FrameEnquiry.pas

  Defines:     TfraEnquiry

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 5 $
    $Date: 9/02/04 17:15 $
    $Author: Bencollier $

===============================================================================}

unit FrameEnquiry;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  BasePageControlFrameUnit, BaseTabSheetFrameUnit, ResourceStrings;

type
  {-----------------------------------------------------------------------------
    Empty page control that has the following tabs embedded to display details of an enquiry:
    TfraEnquiryGeneral
    TfraEnquiryResponse
    TfraSources
  }
  TfraEnquiry = class (TBasePageControlFrame)
  public
    procedure AddTabs; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameEnquiryGeneral, FrameEnquiryResponse, FrameSources;

{-==============================================================================
    TfraEnquiry
===============================================================================}
{-------------------------------------------------------------------------------
  Load all contained frames in separate tabsheets. 
}
procedure TfraEnquiry.AddTabs;
begin
  AddTab('General', TfraEnquiryGeneral);
  AddTab('Response', TfraEnquiryResponse);
  AddTab('Sources', TfraSources);
end;  // TfraEnquiry.AddTabs 

end.

