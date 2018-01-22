{===============================================================================
  Unit:        FrameHistoryGeneral

  Defines:     TfraHistoryGeneral

  Description:

  Created:     June 2003

  Last revision information:
    $Revision: 8 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}
unit FrameHistoryGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, ImageListButton,
  InterfaceDataModule, BaseCompositeComponent, LinkedControls, DataTypes,
  VagueDateEdit, DataClasses, ResourceStrings, UserEdit;

type
  {-----------------------------------------------------------------------------
    General tab page control allowing the user to view and edit details associated with a 
    collection unit History item.
  }
  TfraHistoryGeneral = class (TBaseTabSheetFrame)
    eFrom: TVagueDateEdit;
    eSourcePerson: TUserEdit;
    eTitle: TEdit;
    eTo: TVagueDateEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    mmComments: TMemo;
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure DropeSourcePerson(const Sender: TObject; const AFormat: Integer; const 
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
  protected
    procedure DeleteData; override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  

implementation

{$R *.dfm}

uses
  DropTarget, GeneralData, Validation;

{ TfraHistoryGeneral }

{-==============================================================================
    TfraHistoryGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Delete the record from the database that was used to load the frame. 
}
procedure TfraHistoryGeneral.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_CollectionUnitHistory_Delete',
                               ['@Key', Key, '@Timestamp', FTimestamp]);
end;  // TfraHistoryGeneral.DeleteData 

{-------------------------------------------------------------------------------
  Handle the dropping of data onto eSourcePerson. 
}
procedure TfraHistoryGeneral.DropeSourcePerson(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eSourcePerson, 'Individual',
                 'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraHistoryGeneral.DropeSourcePerson 

{-------------------------------------------------------------------------------
  Retrieve the treenode caption when this item is updated. 
}
function TfraHistoryGeneral.GetCaption: String;
begin
  Result := eTitle.Text + ' - ' + eFrom.Text;
end;  // TfraHistoryGeneral.GetCaption 

{-------------------------------------------------------------------------------
  Record the timestamps for use when updating 
}
procedure TfraHistoryGeneral.LoadData;
begin
  inherited;
  if not RegisteredRecordsets[0].Eof then begin
    FTimestamp := RegisteredRecordsets[0].Fields['Timestamp'].Value;
  end;
end;  // TfraHistoryGeneral.LoadData 

{-------------------------------------------------------------------------------
  Register recordsets and controls used to load the data. 
}
procedure TfraHistoryGeneral.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_CollectionUnitHistory_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eTitle, 'Item_Name', True, ResStr_Title);
  RegisterControl(eFrom, 'From', True, ResStr_From);
  RegisterControl(eTo, 'To');
  RegisterControl(eSourcePerson, 'Source', 'Source_Name_Key', CheckLinkedIndividual,
        'Name', ConvertIndividualKeyToCaption);
  RegisterControl(mmComments, 'Comment');
end;  // TfraHistoryGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Register controls for drag and drop operations. 
}
procedure TfraHistoryGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eSourcePerson, DropeSourcePerson, ['NAME', 'INDIVIDUAL'],
                                                       [CF_JNCCDATA]);
end;  // TfraHistoryGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Insert or edit an item in the database. 
}
procedure TfraHistoryGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
                          '@CollectionUnitKey', ParentKey,
                          '@SourceNameKey', eSourcePerson.Key,
                          '@FromVagueDateStart', eFrom.VagueDate.StartDate,
                          '@FromVagueDateEnd', eFrom.VagueDate.EndDate,
                          '@FromVagueDateType', eFrom.VagueDate.DateTypeString,
                          '@ToVagueDateStart', eTo.VagueDate.StartDate,
                          '@ToVagueDateEnd', eTo.VagueDate.EndDate,
                          '@ToVagueDateType', eTo.VagueDate.DateTypeString,
                          '@ItemName', eTitle.Text,
                          '@Comment', mmComments.Text,
                          '@Timestamp', FTimestamp
                        ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc('Collection_Unit_History',
                                          'usp_CollectionUnitHistory_Insert',
                                          lParams,
                                          '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_CollectionUnitHistory_Update', lParams);
end;  // TfraHistoryGeneral.SaveData 

end.




