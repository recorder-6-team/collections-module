{===============================================================================
  Unit:        FrameBaseMaintainRelationships.pas

  Defines:     TfraBaseMaintainRelationships

  Description: Base frame to display and save data on details side.

  Model:       ThesaurusEditor.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 8/12/03 12:15 $
    $Author: Anthonysimpson $

===============================================================================}

unit FrameBaseMaintainRelationships;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDetailFrameUnit, StdCtrls;

type
  TfraBaseMaintainRelationships = class(TBaseDetailFrame)
    eRelationshipName: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  protected
    function GetDeleteStoredProcName: string; virtual; abstract;
  public
    property DeleteStoredProcName: string read GetDeleteStoredProcName;
    { Public declarations }
  end;


implementation


{$R *.dfm}

end.
