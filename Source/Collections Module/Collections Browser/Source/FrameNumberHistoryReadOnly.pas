{===============================================================================
  Unit:        FrameNumberHistoryReadOnly

  Defines:     TfraNumberHistoryReadOnly

  Description:

  Created:

  Last revision information:
    $Revision: 5 $
    $Date: 24/09/03 13:49 $
    $Author: Markaddis $

===============================================================================}

unit FrameNumberHistoryReadOnly;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseNumberHistoryFrameUnit, StdCtrls, ExtCtrls;

type
  {-----------------------------------------------------------------------------
    Tab page for viewing details of an accession number for a collection unit, inherited 
    from TBaseNumberHistoryFrame.  This tab page is displayed alongside registration 
    numbers and other numbers in the Numbering History in order that all numbers may be 
    viewed in one place.  However, Accession numbers are created using the Accessions and 
    Movement screens.
    The mmNotes field (inherited from TBaseNumberHistoryFrame) is made read only with a 
    grey background and blue font to resemble other data labels on this form.  The 
    following details are displayed:
    The Accession's Movement record's Other Party (individual or organisation string)
    The Movement_Of_Ownership's vague date
    The Movement's Staff Responsible (individual string)
    The Department the item was accessioned into (Organisation_Department.Item_Name).
  }
  TfraNumberHistoryReadOnly = class (TBaseNumberHistoryFrame)
    lblNumberType: TLabel;
    lblNumberValue: TLabel;
  protected
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses GeneralData;

{-==============================================================================
    TfraNumberHistoryReadOnly
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraNumberHistoryReadOnly.GetCaption: String;
begin
  Result := (lblNumberType.Caption + '-' + lblNumberValue.Caption);
end;  // TfraNumberHistoryReadOnly.GetCaption 

{-------------------------------------------------------------------------------
}
procedure TfraNumberHistoryReadOnly.LoadData;
var
  OtherPartyName, StaffResponsible, Date, ItemName: String;
begin
  lblNumberType.Caption := 'Accession';
  mmNotes.Clear;

  if not RegisteredRecordSets[0].Eof then begin
    lblNumberValue.Caption := VarToStr(RegisteredRecordSets[0].Fields['Number'].Value);
    OtherPartyName := VarToStr(RegisteredRecordSets[0].Fields['OtherPartyName'].Value);
    StaffResponsible := VarToStr(RegisteredRecordSets[0].Fields['StaffResponsible'].Value);
    Date := VarToStr(dmGeneral.GetVagueDateStringFromRecordset(RegisteredRecordSets[0]));
    ItemName := VarToStr(RegisteredRecordSets[0].Fields['Item_Name'].Value);
  
    if OtherPartyName <> '' then
      mmNotes.Lines.Add('Accessioned From: ' + OtherPartyName);
    if StaffResponsible <> '' then
      mmNotes.Lines.Add('Staff Responsible: ' + StaffResponsible);
    if Date <> '' then
      mmNotes.Lines.Add('Date: ' + Date);
    if ItemName <> '' then
      mmNotes.Lines.Add('Department: ' + ItemName);
  end;
end;  // TfraNumberHistoryReadOnly.LoadData 

{-------------------------------------------------------------------------------
}
procedure TfraNumberHistoryReadOnly.RegisterControls;
begin
  inherited RegisterControls;
  
  RegisterRecordSet('usp_NumberHistoryReadOnly_Select');
end;  // TfraNumberHistoryReadOnly.RegisterControls 

end.






