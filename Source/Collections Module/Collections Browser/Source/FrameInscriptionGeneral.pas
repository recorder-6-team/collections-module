{===============================================================================
  Unit:        FrameInscription.pas

  Defines:     TfraInscription

  Description:

  Created:     Septempber 2003

  Last revision information:
    $Revision: 15 $
    $Date: 16/10/12 11:27 $
    $Author: Alexanderpadley $

===============================================================================}

unit FrameInscriptionGeneral;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, ExtCtrls, StdCtrls, Grids, ImageListButton,
  ConceptGroupComboBox, BaseCompositeComponent, LinkedControls,
  DataClasses, LuxIDComboBox, DataTypes, StrUtils, InterfaceDataModule,
  ComboListID, ADOInt, Recorder2000_TLB, UserEdit;

type
  {-----------------------------------------------------------------------------
    Class to handle general data related to selected inscription.
  }
  TfraInscriptionGeneral = class(TBaseTabSheetFrame)
    btnAuthorInferred: TImageListButton;
    cmbConfidence: TConceptGroupComboBox;
    cmbTranslationLanguage: TLuxIDComboBox;
    cmbType: TLuxIDComboBox;
    eAuthor: TUserEdit;
    ePosition: TEdit;
    grbAttributedTo: TGroupBox;
    grbDetail: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    mmComments: TMemo;
    mmInscription: TMemo;
    mmTranslation: TMemo;
    chkCurrent: TCheckBox;
    procedure cmbTranslationLanguagePopulate(Sender: TObject);
    procedure cmbTypePopulate(Sender: TObject);
    procedure DropAuthor(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure eAuthorChange(Sender: TObject);
    procedure eAuthorGetData(Sender: TObject);
    procedure InferenceClick(Sender: TObject);
  private
    FTimestamp: TSQLSvrTimestamp;
    procedure ClearInferredButtons;
    function InferredCaptionToValue(ACaption: String): ShortInt;
    procedure InferredValueToButtonImage(AButton: TImageListButton; AValue: Integer);
    procedure UpdateAuthor(const AKeyList: IKeyList);
  protected
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, GeneralData, ResourceStrings, LuxembourgConstants,
  Validation, DropTarget;

{-==============================================================================
    TfraInscriptionGeneral
===============================================================================}
{-------------------------------------------------------------------------------
  Reset the inferred data buttons so that they show all ticks.  
}
procedure TfraInscriptionGeneral.ClearInferredButtons;
begin
  InferredValueToButtonImage(btnAuthorInferred, 0);
end;  // TfraInscriptionGeneral.ClearInferredButtons 

{-------------------------------------------------------------------------------
  Gets the languages from the Language table. 
}
procedure TfraInscriptionGeneral.cmbTranslationLanguagePopulate(Sender: TObject);
begin
  inherited;
  with dmGeneral.GetRecordset('usp_Languages_Select', []) do
    while not EOF do begin
      cmbTranslationLanguage.Add(
          VarToStr(Fields['Language_Key'].Value) + ' - ' + VarToStr(
              Fields['Item_Name'].Value),
          VarToStr(Fields['Language_Key'].Value));
      MoveNext;
    end;
end;  // TfraInscriptionGeneral.cmbTranslationLanguagePopulate 

{-------------------------------------------------------------------------------
  Hardcodes the Type Combo Box with values in ResourceStrings. 
}
procedure TfraInscriptionGeneral.cmbTypePopulate(Sender: TObject);
begin
  cmbType.Add(ResStr_Label,0);
  cmbType.Add(ResStr_Inscription,1);
end;  // TfraInscriptionGeneral.cmbTypePopulate 

{-------------------------------------------------------------------------------
  Allows an Author to be dropped into the eAuthor text box. 
}
procedure TfraInscriptionGeneral.DropAuthor(const Sender: TObject; const AFormat: Integer;
    const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  DropLinkedEditData(AFormat, ASourceData, AHandled, eAuthor, 'Individual',
      'usp_FormattedNameForNameKey_Get', '@NameKey', '@FormattedName');
end;  // TfraInscriptionGeneral.DropAuthor 

{-------------------------------------------------------------------------------
  If the contents of the eAuthor linked edit changes, then the Confidence combo box
      becomes enabled.
}
procedure TfraInscriptionGeneral.eAuthorChange(Sender: TObject);
begin
  inherited;
  cmbConfidence.Enabled := eAuthor.Text <> '';
  if not cmbConfidence.Enabled then cmbConfidence.ItemIndex := -1;
end;  // TfraInscriptionGeneral.eAuthorChange 

{-------------------------------------------------------------------------------
}
procedure TfraInscriptionGeneral.eAuthorGetData(Sender: TObject);
begin
  inherited;
  InitReturnData(UpdateAuthor, TN_NAME);
end;  // TfraInscriptionGeneral.eAuthorGetData 

{-------------------------------------------------------------------------------
  Gets the caption for this browser node, by removing any tab and new line
  characters from mmInscription.
}
function TfraInscriptionGeneral.GetCaption: String;
begin
  Result := StringReplace(mmInscription.Text, #13#10, '  ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
end;  // TfraInscriptionGeneral.GetCaption

{-------------------------------------------------------------------------------
  Handle a click on one of the inferred buttons.  
}
procedure TfraInscriptionGeneral.InferenceClick(Sender: TObject);
begin
  with TImageListButton(Sender) do begin
    if Caption='' then
      Caption:='!'
    else if Caption='!' then
      Caption:='?'
    else if Caption='?' then
      Caption:='!?'
    else if Caption='!?' then
      Caption:='';
    if Caption='' then
      ImageIndex := 6    // enable the tick
    else
      ImageIndex := -1;  // disable the tick to show the caption
  end;
end;  // TfraInscriptionGeneral.InferenceClick 

{-------------------------------------------------------------------------------
  Convert the caption of a button to a number that can be saved in the database.  
}
function TfraInscriptionGeneral.InferredCaptionToValue(ACaption: String): ShortInt;
begin
  if      ACaption = ''   then Result := 0
  else if ACaption = '!'  then Result := 1
  else if ACaption = '?'  then Result := 2
  else if ACaption = '!?' then Result := 3
  else
    raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
end;  // TfraInscriptionGeneral.InferredCaptionToValue 

{-------------------------------------------------------------------------------
  Give the inferred buttons their images and captions according to the values from the
      database.
}
procedure TfraInscriptionGeneral.InferredValueToButtonImage(AButton: TImageListButton;
    AValue: Integer);
begin
  with AButton do begin
    case AValue of
      0 : begin
            Caption := '';
            ImageIndex := 6;
          end;
      1 : begin
            Caption := '!';
            ImageIndex := -1;
          end;
      2 : begin
            Caption := '?';
            ImageIndex := -1;
          end;
      3 : begin
            Caption := '!?';
            ImageIndex  := -1;
          end;
    else
      raise EBrowserFrameError.Create(ResStr_InferredButtonCaptionIncorrect);
    end;
  end;
end;  // TfraInscriptionGeneral.InferredValueToButtonImage 

{-------------------------------------------------------------------------------
  Loads the data and stores the Timestamp. 
}
procedure TfraInscriptionGeneral.LoadData;
begin
  inherited LoadData;
  
  if not RegisteredRecordsets[0].Eof then begin
    with RegisteredRecordsets[0] do begin
      FTimestamp := Fields['Timestamp'].Value;
      InferredValueToButtonImage(btnAuthorInferred, Fields['Inferred_Author'].Value);
    end;
  end else
    ClearInferredButtons;
end;  // TfraInscriptionGeneral.LoadData

{-------------------------------------------------------------------------------
  Registers controls in the frame. 
}
procedure TfraInscriptionGeneral.RegisterControls;
begin
  inherited;
  RegisterRecordSet('usp_SpecimenLabel_Select');
  RegisterControl(cmbType, '', 'Is_Inscription',True,ResStr_Type);
  RegisterControl(ePosition, 'Label_Position');
  RegisterControl(mmInscription, 'Inscription',True,ResStr_Inscription);
  RegisterControl(mmTranslation, 'Translated');
  RegisterControl(cmbTranslationLanguage, 'Language', 'Translated_Language_Key');
  RegisterControl(mmComments, 'Comments');
  RegisterControl(eAuthor,'AuthorName','Author_Name_Key',False,
      ResStr_Author, CheckLinkedIndividual, 'Name', ConvertIndividualKeyToCaption);
  eAuthor.OnGetData := eAuthorGetData;
  RegisterControl(cmbConfidence, 'Confidence_Name', 'Confidence_Concept_Key');
  RegisterControl(chkCurrent, 'Is_Current');
  RegisterConceptGroupComboBox(cmbConfidence, CG_LEVELS_OF_CONFIDENCE);
end;  // TfraInscriptionGeneral.RegisterControls 

{-------------------------------------------------------------------------------
  Registers drag drop functionality for the controls. 
}
procedure TfraInscriptionGeneral.RegisterDragDropComponents;
begin
  RegisterDropComponent(eAuthor, DropAuthor, [TN_NAME, TN_INDIVIDUAL],
                                                       [CF_JNCCDATA]);
end;  // TfraInscriptionGeneral.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Updates or Saves new data. 
}
procedure TfraInscriptionGeneral.SaveData;
var
  lParams: Array of Variant;
begin
  lParams := VarArrayOf(['@Key', Key,
             '@CollectionUnitKey', ParentKey,
             '@IsInscription', cmbType.CurrentIntID,
             '@Position', ePosition.Text,
             '@Inscription', mmInscription.Text,
             '@Translated', mmTranslation.Text,
             '@LanguageConceptKey', cmbTranslationLanguage.CurrentStrID,
             '@Comments', mmComments.Text,
             '@AuthorNameKey', eAuthor.Key,
             '@ConfidenceConceptKey', cmbConfidence.CurrentStrID,
             '@InferredAuthor', InferredCaptionToValue(btnAuthorInferred.Caption),
             '@Timestamp', FTimestamp,
             '@IsCurrent', chkCurrent.Checked]);
  
  if Key = '' then
       Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_SPECIMEN_LABEL,
                                                      'usp_SpecimenLabel_Insert',
                                                      lParams,
                                                      '@Key'))
  else
      dmGeneral.RunUpdateStoredProc('usp_SpecimenLabel_Update', lParams);
end;  // TfraInscriptionGeneral.SaveData 

{-------------------------------------------------------------------------------
}
procedure TfraInscriptionGeneral.UpdateAuthor(const AKeyList: IKeyList);
var
  lKey: TKeyString;
begin
  if not (AKeyList.ItemCount > 0) then Exit;
  lKey := AKeyList.GetKeyItem(0).KeyField1;
  
  if (dmGeneral.GetStoredProcOutputParam('usp_Name_IsIndividual_Get',
          ['@NameKey', lKey], '@IsIndividual') = true) then begin
    eAuthor.Key := lKey;
    eAuthor.Text := ConvertIndividualKeyToCaption(lKey, TN_INDIVIDUAL);
  end
  else begin
    MessageDlg(ResStr_AuthorMustBeAnIndividual, mtInformation, [mbOK], 0);
    if eAuthor.CanFocus then eAuthor.SetFocus;
  end;
end;  // TfraInscriptionGeneral.UpdateAuthor 

end.


