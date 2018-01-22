{===============================================================================
  Unit:        FrameMaterialsDocumented.pas

  Defines:     TfraMaterialsDocumented

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     June 2003

  Last revision information:
    $Revision: 5 $
    $Date: 15/04/04 12:25 $
    $Author: Anthonysimpson $

===============================================================================}
unit FrameMaterialsDocumented;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameMaterials, StdCtrls, ImageListButton, Grids, ExtCtrls,
  DssStringGrid, ComboListID, LuxIDComboBox, ConceptGroupComboBox,
  BaseCompositeComponent, LinkedControls, DataClasses, Recorder2000_TLB;

type
  {-----------------------------------------------------------------------------
    Tab page for the materials required for a specimen.  Inherits the materials grid 
    functionality from TfraMaterials but additionally implements a Document edit control to 
    link a reference to the list of materials used.  Grid functionality is documented in the 
    TfraMaterials documentation.
    Materials are stored as concepts in the thesaurus.  This tab page maintains a list of 
    materials used plus the amount and unit for each material.
  }
  TfraMaterialsDocumented = class (TfraMaterials)
    eDocuments: TLinkedEdit;
    Label3: TLabel;
    procedure eDocumentsGetData(Sender: TObject);
  private
    procedure DropDocument(const Sender: TObject; const AFormat: Integer; const ASourceData: 
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    function RemoveRichTextFormatting: String;
    procedure SetDocumentsTextKey(const AText: String; const AKey: TKeyString);
    procedure UpdateDocument(const AKeyList: IKeyList);
  protected
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ResourceStrings, Validation, LuxembourgConstants, DropTarget, GeneralData,
  ComCtrls, LuxembourgFunctions, BaseADODataModule;

{ TfraMaterialsDocumented }

{-==============================================================================
    TfraMaterialsDocumented
===============================================================================}
{-------------------------------------------------------------------------------
  Handles the drop of a Document on to the field. It then needs to go to 
      RemoveRichTextFormatting to make sure the text is correct. 
}
procedure TfraMaterialsDocumented.DropDocument(const Sender: TObject; const AFormat: Integer; 
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eDocuments, TN_REFERENCE,
                 'usp_Reference_Get', '@Key', '@Caption');
  RemoveRichTextFormatting;
end;  // TfraMaterialsDocumented.DropDocument 

{-------------------------------------------------------------------------------
  Make sure the magnifying glass loads the Documents module and that it goes to the 
      UpdateDocument method once it returns. 
}
procedure TfraMaterialsDocumented.eDocumentsGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateDocument, 'Document');
end;  // TfraMaterialsDocumented.eDocumentsGetData 

{-------------------------------------------------------------------------------
  Load the Documents field with the data it requires. Use the SetDocumentsTextKey method to 
      make sure that the RichText formatting is removed on the load.  
}
procedure TfraMaterialsDocumented.LoadData;
begin
  inherited;
  with RegisteredRecordsets[0] do
    if not Eof then
      SetDocumentsTextKey(VarToStr(Fields['Reference_Caption'].Value),
                          VarToStr(Fields['Source_Key'].Value));
end;  // TfraMaterialsDocumented.LoadData 

{-------------------------------------------------------------------------------
  Register the recordset, control and relink the OnGetData event to 'eDocumentsGetData' event 
      handler. This needs to be done because the events get unlinked when registering a linked 
      edit. 
}
procedure TfraMaterialsDocumented.RegisterControls;
begin
  inherited;
  RegisterRecordset('usp_SpecimenMaterialDocument_Select');
  RegisterControl(eDocuments , 'Reference_Caption', 'Source_Key',
      False, ResStr_Document, CheckLinkedReference, 'Document',
          ConvertReferenceKeyToCaption);
  eDocuments.OnGetData := eDocumentsGetData;
end;  // TfraMaterialsDocumented.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag/drop component. 
}
procedure TfraMaterialsDocumented.RegisterDragDropComponents;
begin
  inherited;
  RegisterDropComponent(eDocuments, DropDocument, [TN_REFERENCE], [CF_JNCCDATA]);
end;  // TfraMaterialsDocumented.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  We have to use 'usp_Reference_Select_ForInternalSources' to get the text for the document 
      field because it returns a recordset, which means that the length of the returned data 
      from the 'Full_Reference' field will not be cut. 'usp_Reference_get' returns 
      'Full_Reference' in an output parameter, and hence it must be set a maximum length (e.g. 
      varchar(2000)). This would be fine in most cases, except if it is rich text over 2000 
      characters. In that situation, the RT would be invalid and would not be correctly 
      parsed. 
}
function TfraMaterialsDocumented.RemoveRichTextFormatting: String;
var
  lKey: TKeyString;
begin
  lKey := eDocuments.Key;
  with dmGeneral.GetRecordset('usp_Reference_Select_ForInternalSources', ['@Key', lKey]) do
    if not Eof then
      eDocuments.Text := VarToStr(Fields['Full_Reference'].Value);
  
  SetDocumentsTextKey(eDocuments.Text, lKey);
end;  // TfraMaterialsDocumented.RemoveRichTextFormatting 

{-------------------------------------------------------------------------------
  Save the contents of the document field to the database. 
}
procedure TfraMaterialsDocumented.SaveData;
begin
  inherited;
  dmGeneral.RunUpdateStoredProc('usp_SpecimenMaterialDocument_Update',
                                      ['@Key', Key,
                                      '@Source_Key', eDocuments.Key]);
end;  // TfraMaterialsDocumented.SaveData 

{-------------------------------------------------------------------------------
  This method removes the rich text formatting, the extra characters, and sets the key back. 
      It is necessary to change the key back - initially it has the correct key, but changing 
      the text will cause it to be lost, so it needs to be set back again afterwards. 
}
procedure TfraMaterialsDocumented.SetDocumentsTextKey(const AText: String; const AKey: 
    TKeyString);
begin
  eDocuments.Text := RichTextToText(Self, AText);
  eDocuments.Text := StringReplace(eDocuments.Text, #13#10,' ',[rfReplaceAll]);
  eDocuments.Key := AKey;
end;  // TfraMaterialsDocumented.SetDocumentsTextKey 

{-------------------------------------------------------------------------------
  Come here when the user presses F9/Return data button to get a document from the documents 
      module. The rich text formatting needs to be removed. 
}
procedure TfraMaterialsDocumented.UpdateDocument(const AKeyList: IKeyList);
begin
  // If nothing is returned then set lKey to ''. An appropriate message will
  // be displayed below
  if AKeyList.ItemCount = 0 then eDocuments.Key := ''
                            else eDocuments.Key := AKeyList.GetKeyItem(0).KeyField1;
  RemoveRichTextFormatting;
end;  // TfraMaterialsDocumented.UpdateDocument 

end.

