{===============================================================================
  Unit:        BatchAutoNumber

  Defines:     TdlgBatchAutoNumber

  Description: Allows the user to choose whether the registration numbers to be
               allocated to specimens during batch auto-numbering should be
               set to the "preferred", "new" or "previous" state. When
               the user has selected the desired option and clicks OK, the form
               closes and chosen status is exposed by SelectedNumberStatus.

  Created:     November 2009

  Last revision information:
    $Revision: 4 $
    $Date: 23/11/09 13:41 $
    $Author: Simonlewis $

===============================================================================}
unit BatchAutoNumber;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TNumberStatus = (nsPrevious, nsPreferred, nsNew);

type
  TdlgBatchAutoNumber = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    rgNumberType: TRadioGroup;
    rbPreferred: TRadioButton;
    rbNew: TRadioButton;
    rbPrevious: TRadioButton;
    lblSelect: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure rbPreviousClick(Sender: TObject);
    procedure rbNewClick(Sender: TObject);
    procedure rbPreferredClick(Sender: TObject);
  private
    { Private declarations }
    FSelectedNumberStatus: TNumberStatus;
  public
    { Public declarations }
    property SelectedNumberStatus: TNumberStatus read FSelectedNumberStatus;
  end;

implementation

uses
  ResourceStrings;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Initialise the selected number type to the default value.
}
procedure TdlgBatchAutoNumber.FormCreate(Sender: TObject);
begin
  FSelectedNumberStatus := nsPrevious;
end;

procedure TdlgBatchAutoNumber.rbPreviousClick(Sender: TObject);
begin
  FSelectedNumberStatus := nsPrevious;
end;

procedure TdlgBatchAutoNumber.rbNewClick(Sender: TObject);
begin
  FSelectedNumberStatus := nsNew;
end;

procedure TdlgBatchAutoNumber.rbPreferredClick(Sender: TObject);
begin
  FSelectedNumberStatus := nsPreferred;
end;

end.
