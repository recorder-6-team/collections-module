{===============================================================================
  Unit:        FrameEnquiryResponse.pas

  Defines:     TfraEnquiryResponse

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 4 $
    $Date: 20/11/03 13:35 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameEnquiryResponse;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, GeneralData, DataTypes;

type
  {-----------------------------------------------------------------------------
    Tab page control holding the response to the original enquiry.
  }
  TfraEnquiryResponse = class (TBaseTabSheetFrame)
    Label6: TLabel;
    mmResponse: TMemo;
  private
    FTimestamp: TSQLSvrTimestamp;
  protected
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfraEnquiryResponse
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraEnquiryResponse.LoadData;
begin
  inherited;
  
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TfraEnquiryResponse.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryResponse.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Enquiry_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(mmResponse, 'Response');
end;  // TfraEnquiryResponse.RegisterControls 

{-------------------------------------------------------------------------------
}
procedure TfraEnquiryResponse.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key',Key,
                          '@Response', mmResponse.Text,
                          '@Timestamp',FTimestamp
                          ]);
  dmGeneral.RunUpdateStoredProc('usp_EnquiryResponse_Update', lParams);
end;  // TfraEnquiryResponse.SaveData 

end.


