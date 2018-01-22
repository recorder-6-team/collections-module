{===============================================================================
  Unit:        FrameCollectionTimePeriod.pas

  Defines:     TfraCollectionTimePeriod

  Description:

  Model:       CollectionsCollectionsAndStorage.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 4 $
    $Date: 26/09/03 12:00 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameCollectionTimePeriod;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, VagueDateEdit;

type
  {-----------------------------------------------------------------------------
    Tab page control allowing the user to view and edit the time periods associated with a 
    collection.
  }
  TfraCollectionTimePeriod = class (TBaseTabSheetFrame)
    eCollationFrom: TVagueDateEdit;
    eCollationTo: TVagueDateEdit;
    eFieldFrom: TVagueDateEdit;
    eFieldTo: TVagueDateEdit;
    eGeologicalFrom: TEdit;
    eGeologicalTo: TEdit;
    gbCollation: TGroupBox;
    gbFieldGatheringPeriod: TGroupBox;
    gbHistoricalPeriod: TGroupBox;
    lblCollationFrom: TLabel;
    lblCollationTo: TLabel;
    lblFieldFrom: TLabel;
    lblFieldTo: TLabel;
    lblGeologicalFrom: TLabel;
    lblGeologicalTo: TLabel;
  protected
    procedure SaveData; override;
    procedure ValidateData; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralData, ResourceStrings, VagueDate, ExceptionForm;

{-==============================================================================
    TfraCollectionTimePeriod
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraCollectionTimePeriod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Register recordsets used
  RegisterRecordset('usp_CollectionTimePeriod_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eCollationFrom, 'Collation_From');
  RegisterControl(eCollationTo, 'Collation_To');
  RegisterControl(eFieldFrom, 'Gather_From');
  RegisterControl(eFieldTo, 'Gather_To');
  RegisterControl(eGeologicalFrom, 'Historical_Period_From');
  RegisterControl(eGeologicalTo, 'Historical_Period_To');
end;  // TfraCollectionTimePeriod.Create 

{-------------------------------------------------------------------------------
  Save modified data to the database. 
}
procedure TfraCollectionTimePeriod.SaveData;
begin
  dmGeneral.RunUpdateStoredProc('usp_CollectionTimePeriod_Update',
                                ['@Key', Key,
                                 '@CollationFromStart', eCollationFrom.StartDate,
                                 '@CollationFromEnd', eCollationFrom.EndDate,
                                 '@CollationFromType', eCollationFrom.DateTypeString,
                                 '@CollationToStart', eCollationTo.StartDate,
                                 '@CollationToEnd', eCollationTo.EndDate,
                                 '@CollationToType', eCollationTo.DateTypeString,
                                 '@GatherFromStart', eFieldFrom.StartDate,
                                 '@GatherFromEnd', eFieldFrom.EndDate,
                                 '@GatherFromType', eFieldFrom.DateTypeString,
                                 '@GatherToStart', eFieldTo.StartDate,
                                 '@GatherToEnd', eFieldTo.EndDate,
                                 '@GatherToType', eFieldTo.DateTypeString,
                                 '@HistoricalPeriodFrom', eGeologicalFrom.Text,
                                 '@HistoricalPeriodTo', eGeologicalTo.Text]);
end;  // TfraCollectionTimePeriod.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraCollectionTimePeriod.ValidateData;
begin
  ValidateValue((eCollationFrom.Text = '') or (eCollationFrom.DateTypeString <> 'U'),
      Format(ResStr_InvalidData, ['Collation Period From']), eCollationFrom);
  ValidateValue((eCollationTo.Text = '') or (eCollationTo.DateTypeString <> 'U'),
      Format(ResStr_InvalidData, ['Collation Period To']), eCollationTo);
  
  ValidateValue((eCollationFrom.Text = '') or (eCollationTo.Text = '') or
      (CompareVagueDates(eCollationFrom.VagueDate, eCollationTo.VagueDate) <> dcAfter),
      Format(ResStr_InvalidFromToVagueRange,
             ['Collation Period From', 'Collation Period To']));
  
  ValidateValue((eFieldFrom.Text = '') or (eFieldFrom.DateTypeString <> 'U'),
      Format(ResStr_InvalidData, ['Field Gathering Period From']), eFieldFrom);
  ValidateValue((eFieldTo.Text = '') or (eFieldTo.DateTypeString <> 'U'),
      Format(ResStr_InvalidData, ['Field Gathering Period To']), eFieldTo);
  
  ValidateValue((eFieldFrom.Text = '') or (eFieldTo.Text = '') or
      (CompareVagueDates(eFieldFrom.VagueDate, eFieldTo.VagueDate) <> dcAfter),
      Format(ResStr_InvalidFromToVagueRange,
             ['Field Gathering Period From', 'Field Gathering Period To']));
end;  // TfraCollectionTimePeriod.ValidateData 

end.

