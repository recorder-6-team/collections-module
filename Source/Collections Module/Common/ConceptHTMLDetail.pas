{===============================================================================
  Unit:        ConceptHTMLDetail

  Defines:     TfraConceptHTMLDetail

  Description: Frame control that displays an HTML viewer containing general
               details of a selected concept.

  Created:     September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 19/01/05 14:07 $
    $Author: Johnvanbreda $

===============================================================================}
unit ConceptHTMLDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTabSheetFrameUnit, BaseDetailFrameUnit, Htmlview, ExtCtrls,
  DBWeb, HTTPApp, HTTPProd, GeneralData, ADODb, HTMLDisplayFuncs, ComCtrls,
  ApplicationSettings, EasyShell, DataClasses, Recorder2000_TLB,
  ThesaurusBrowser_TLB, ActiveX;

type
  {-----------------------------------------------------------------------------
    Frame control that displays an HTML viewer containing general details of a 
    selected concept.
  }
  TfraConceptHTMLDetail = class (TBaseTabSheetFrame, IKeyList, IKeyItem,
      ISynchronizable)
    HTMLViewer: THTMLViewer;
    procedure HTMLViewerHotSpotClick(Sender: TObject; const SRC: String; var 
        Handled: Boolean);
  private
    FMarshalledCallback: IStream;
    FHTMLBody: string;
    FHyperlinkKey: string;
    FRecordsets: array[0..7] of _Recordset;
    function BulletListFirstColumn(const ACaption: string; ARecordset: 
        _Recordset): string;
    function Designations(ARecordset: _Recordset): string;
    function Facts(ARecordset: _Recordset): string;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;
    function Get_ItemCount: Integer; safecall;
    function Get_KeyField1: WideString; safecall;
    function Get_KeyField2: WideString; safecall;
    function Get_TableName: WideString; safecall;
    procedure HTMLDetailsReturned(ATarget: TObject; ADataset: 
        TCustomADODataset);
    function HtmlDocument: string;
    procedure HTMLNamesReturned(ATarget: TObject; ADataset: TCustomADODataset);
    function Links(ARecordset: _Recordset): string;
    procedure LoadHtmlDetails;
    procedure LoadHtmlNames;
    procedure PrepareDetailsHTML;
    procedure PrepareForSynchronizedCallback;
    procedure PrepareNamesHTML;
    function Sources(ARecordset: _Recordset): string;
    procedure Synchronize(ID: Integer);
    procedure SynchronizedCallback(ID: Integer); safecall;
  protected
    procedure DeleteData; override;
    procedure LoadData; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;
  

implementation

{$R *.dfm}

uses
  ComObj, LuxembourgConstants, ResourceStrings;

const
  SOURCE_TAG = '#SOURCE';
  SYNC_NOOP = 0;
  SYNC_LOAD_HTML_NAMES = 1;
  SYNC_LOAD_HTML_DETAILS = 2;

{-==============================================================================
    TfraConceptHTMLDetail
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraConceptHTMLDetail.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;  // TfraConceptHTMLDetail.Create 

{-------------------------------------------------------------------------------
}
destructor TfraConceptHTMLDetail.Destroy;
begin
  if Assigned(FMarshalledCallback) then
    Synchronize(SYNC_NOOP);
  inherited Destroy;
end;  // TfraConceptHTMLDetail.Destroy 

{-------------------------------------------------------------------------------
  Create a HTML bulletted list from the first column of the dataset.  Precede 
      it with the caption. 
}
function TfraConceptHTMLDetail.BulletListFirstColumn(const ACaption: string; 
    ARecordset: _Recordset): string;
begin
  if ARecordset.Recordcount=0 then
    Result := ''
  else begin
    Result := MiniHeaderStyle(ACaption);
    Result := Result + '<MENU>';
    with ARecordset do begin
      MoveFirst;
      while not EOF do begin
        Result := Result + '<LI>' + VarToStr(Fields[0].Value) + '</LI>';
        MoveNext;
      end;
    end; // with
    Result := Result + '</MENU>';
  end;
end;  // TfraConceptHTMLDetail.BulletListFirstColumn 

{-------------------------------------------------------------------------------
  Method to delete the join record between an Enquiry and a Concept/Term. 
}
procedure TfraConceptHTMLDetail.DeleteData;
begin
  dmGeneral.RunDeleteStoredProc('usp_EnquiryConcept_Delete',
                               ['@Key', Key, '@EnquiryKey', ParentKey]);
end;  // TfraConceptHTMLDetail.DeleteData 

{-------------------------------------------------------------------------------
  Generates HTML for the list of designations. 
}
function TfraConceptHTMLDetail.Designations(ARecordset: _Recordset): string;
var
  lStatuses: TStringList;
  ltfFirstStatusDetail: Boolean;
  
  procedure AddStatusDetail(const Title, Detail : string);
  begin
    if Detail<>'' then begin
      // separate details with <br> if not first item
      if not ltfFirstStatusDetail then
        lStatuses[lStatuses.Count-1] := lStatuses[lStatuses.Count-1]+'<br>';
      lStatuses.Add(FontStyle(True, False, False, Title + ': ') + Detail);
      ltfFirstStatusDetail := False;
    end;
  end;
  
begin
  if ARecordset.RecordCount=0 then
    Result := ''
  else begin
    lStatuses := TStringList.Create;
    try
      Result := MiniHeaderStyle(ResStr_Designations);
      lStatuses.Add('<TABLE bgcolor=dimgray cellspacing=1 COLS=2 WIDTH=90%>');
  
      // Add Headers for table
      lStatuses.Add(AddTagText('TH', 'Status', ''));
      lStatuses.Add(AddTagText('TH', 'Details', ''));
      ARecordset.MoveFirst;
      with ARecordset do while not Eof do begin
        lStatuses.Add('<TR>');
        // First Col
        lStatuses.Add('<TD VALIGN=TOP>');
        lStatuses.Add(Fields['Item_Name'].Value);
        lStatuses.Add('</TD>');
        // Second Col
        lStatuses.Add('<TD VALIGN=TOP>');
        ltfFirstStatusDetail := True;
        // Dates
        AddStatusDetail('Date From',VarToStr(Fields['DateFrom'].Value));
        AddStatusDetail('Date To',VarToStr(Fields['DateTo'].Value));
        // Main HTML paragraphs
        AddStatusDetail('Geographic Area',VarToStr(
            Fields['Geographic_Context'].Value));
        AddStatusDetail('Constraint',VarToStr(Fields['Constraints'].Value));
        lStatuses.Add('</TD>');
        lStatuses.Add('</TR>');
        MoveNext;
      end;
      lStatuses.Add('</TABLE>');
      Result := Para(Result + lStatuses.Text);
    finally
      lStatuses.Free;
    end; // try
  end;
end;  // TfraConceptHTMLDetail.Designations 

{-------------------------------------------------------------------------------
  Generates HTML for the list of facts. 
}
function TfraConceptHTMLDetail.Facts(ARecordset: _Recordset): string;
begin
  if ARecordset.RecordCount=0 then
    Result := ''
  else begin
    Result := MiniHeaderStyle(ResStr_Facts);
    with ARecordset do begin
      MoveFirst;
      while not EOF do begin
        Result := Result + SubheaderStyle(Fields['Item_Name'].Value);
        Result := Result + Fields['Data'].Value + LineBreak;
        MoveNext;
      end; // while
    end; // with
    // Fixup any tags that denote paths to known folders
    Result := StringReplace(Result, HTML_IMAGES, AppSettings.DictImagesPath, [
        rfReplaceAll]);
    Result := StringReplace(Result, HTML_LOCAL_IMAGES,
        AppSettings.LocalImagesPath, [
        rfReplaceAll]);
    Result := Para(Result);
  end;
end;  // TfraConceptHTMLDetail.Facts 

{-------------------------------------------------------------------------------
  This class acts as the single key item when suplying a source key to 
      Recorder. 
}
function TfraConceptHTMLDetail.GetKeyItem(iIndex: Integer): IKeyItem;
begin
  Result := Self as IKeyItem;
end;  // TfraConceptHTMLDetail.GetKeyItem 

{-------------------------------------------------------------------------------
  This screen implements a keylist that always returns 1 item. 
}
function TfraConceptHTMLDetail.Get_ItemCount: Integer;
begin
  Result := 1;
end;  // TfraConceptHTMLDetail.Get_ItemCount 

{-------------------------------------------------------------------------------
  Return the last source key. 
}
function TfraConceptHTMLDetail.Get_KeyField1: WideString;
begin
  Result := FHyperlinkKey;
end;  // TfraConceptHTMLDetail.Get_KeyField1 

{-------------------------------------------------------------------------------
  Return the last clicked source, sources require no KeyField2. 
}
function TfraConceptHTMLDetail.Get_KeyField2: WideString;
begin
  Result := '';
end;  // TfraConceptHTMLDetail.Get_KeyField2 

{-------------------------------------------------------------------------------
  Class implements IKeyList to return a clicked SOURCE key to Recorder. 
}
function TfraConceptHTMLDetail.Get_TableName: WideString;
begin
  Result := 'SOURCE';
end;  // TfraConceptHTMLDetail.Get_TableName 

{-------------------------------------------------------------------------------
  Callback for the asynchronous call to the stored procedure which gets the 
      details required for the HTML.  Populates the HTML viewer with the 
      remaining details (the names are already done). 
}
procedure TfraConceptHTMLDetail.HTMLDetailsReturned(ATarget: TObject; ADataset: 
    TCustomADODataset);
var
  lIdx: Integer;
  lDummy: OLEVariant;
begin
  FRecordsets[3] := ADataset.Recordset;
  // Get the secondary recordsets into an array of datasets
  for lIdx := 4 to 6 do
    FRecordsets[lIdx] := FRecordsets[lIdx-1].NextRecordset(lDummy);
  // You have to ask the last recordset for its next recordset before it is populated!
  FRecordsets[6].NextRecordset(lDummy);
  // Initiate loading the HTML Names part
  PrepareDetailsHTML;
  Synchronize(SYNC_LOAD_HTML_DETAILS);
end;  // TfraConceptHTMLDetail.HTMLDetailsReturned 

{-------------------------------------------------------------------------------
  Retrieves the HTML document structure from the pre-prepared components. 
}
function TfraConceptHTMLDetail.HtmlDocument: string;
begin
  Result :=
      '<HTML>'#13#10 +
      '<HEAD>' + Style + '</HEAD>'#13#10 +
      '<BODY>'#13#10 + FHTMLBody + #13#10'</BODY>'#13#10 +
      '</HTML>';
end;  // TfraConceptHTMLDetail.HtmlDocument 

{-------------------------------------------------------------------------------
  Callback for the asynchronous call to the stored procedure which gets the 
      details required for the HTML.  Populates the HTML viewer with the names 
      of the item. 
}
procedure TfraConceptHTMLDetail.HTMLNamesReturned(ATarget: TObject; ADataset: 
    TCustomADODataset);
var
  lIdx: Integer;
  lDummy: OLEVariant;
begin
  FRecordsets[0] := ADataset.Recordset;
  // Get the secondary synonymy recordsets into an array of datasets
  for lIdx := 1 to 2 do
    FRecordsets[lIdx] := FRecordsets[lIdx-1].NextRecordset(lDummy);
  // You have to ask the last recordset for its next recordset before it is populated!
  FRecordsets[2].NextRecordset(lDummy);
  // Initiate loading the HTML Names part
  PrepareNamesHTML;
  Synchronize(SYNC_LOAD_HTML_NAMES);
end;  // TfraConceptHTMLDetail.HTMLNamesReturned 

{-------------------------------------------------------------------------------
  When a hyperlink is clicked, either show the Recorder Documents screen and 
      select the document, or shell to the file. 
}
procedure TfraConceptHTMLDetail.HTMLViewerHotSpotClick(Sender: TObject; const 
    SRC: String; var Handled: Boolean);
begin
  inherited;
  // References hyperlinks call the Documents screen
  if Copy(SRC, 1, Length(SOURCE_TAG))=SOURCE_TAG then begin
    // Store rest of key
    FHyperlinkKey := Copy(SRC, Length(SOURCE_TAG)+1, 255);
    // Show the screen.  This will request the key from us via IKeyList
    dmGeneral.Recorder.MenuOptionClick('Data Entry;Documents...');
    dmGeneral.Recorder.DisplayData('Document', Self as IKeyList);
  end else
    // Others shell the file (e.g to a browser)
    ShellFile(SRC);
end;  // TfraConceptHTMLDetail.HTMLViewerHotSpotClick 

{-------------------------------------------------------------------------------
  Generates HTML for the list of links. 
}
function TfraConceptHTMLDetail.Links(ARecordset: _Recordset): string;
begin
  if ARecordset.RecordCount=0 then
    Result := ''
  else begin
    Result := MiniHeaderStyle(ResStr_Links);
    Result := Result + '<TABLE BORDER=0>';
    with ARecordset do begin
      MoveFirst;
      while not EOF do begin
        Result := Result + '<TR><TD>';
        if not VarIsNull(Fields['Image_File'].Value) then
          Result := Result + '<IMG SRC="' + AppSettings.InstallationPath +
              '\Images\' + Fields['Image_File'].Value + '">';
        Result := Result + '</TD><TD>';
        Result := Result + '<A HREF="' + VarToStr(Fields['Hyperlink'].Value) + 
            '">'+
            Format(ResStr_SearchWebForTerm, [
                VarToStr(FRecordsets[0].Fields['ItemName'].Value),
                VarToStr(Fields['Item_Name'].Value)])+'</A>';
        Result := Result + '</TD></TR>';
        MoveNext;
      end; // while
    end; // with
    Result := Result + '</TABLE>';
    Result := Para(Result);
  end;
end;  // TfraConceptHTMLDetail.Links 

{-------------------------------------------------------------------------------
  Loads the HTML Details for the current key.  This initiates an async 
      procedure to load the names part of the HTML.  This in turn initiates a 
      second async procedure to load the remaining HTML details, which is saved 
      till last as it is sometimes slow to load. 
}
procedure TfraConceptHTMLDetail.LoadData;
begin
  dmGeneral.CancelAsyncCommands(HTMLNamesReturned);
  if Key<>'' then begin
    // Initiate loading of the names.
    PrepareForSynchronizedCallback;
    dmGeneral.GetAsyncRecordset('usp_TermHTMLNames_Select', ['@ConceptKey', Key],
        nil, HTMLNamesReturned);
  end;
end;  // TfraConceptHTMLDetail.LoadData

{-------------------------------------------------------------------------------
}
procedure TfraConceptHTMLDetail.LoadHtmlDetails;
begin
  HTMLViewer.LoadFromString(HtmlDocument);
end;  // TfraConceptHTMLDetail.LoadHtmlDetails

{-------------------------------------------------------------------------------
}
procedure TfraConceptHTMLDetail.LoadHtmlNames;
begin
  HTMLViewer.LoadFromString(HtmlDocument);
  
  dmGeneral.CancelAsyncCommands(HTMLDetailsReturned);  
  PrepareForSynchronizedCallback;
  dmGeneral.GetAsyncRecordset('usp_TermHTMLDetails_Select', ['@ConceptKey',
      Key],
      nil, HTMLDetailsReturned);
end;  // TfraConceptHTMLDetail.LoadHtmlNames

{-------------------------------------------------------------------------------
  Adds the HTML to the document for designations, sources, links and facts. 
}
procedure TfraConceptHTMLDetail.PrepareDetailsHTML;
begin
  FHTMLBody := FHTMLBody + Designations(FRecordsets[3]);
  FHTMLBody := FHTMLBody + Facts(FRecordsets[4]);
  FHTMLBody := FHTMLBody + Sources(FRecordsets[5]);
  FHTMLBody := FHTMLBody + Links(FRecordsets[6]);
end;  // TfraConceptHTMLDetail.PrepareDetailsHTML 

{+------------------------------------------------------------------------------
  Marshal a reference to our ISynchronizable interface on the current thread 
      into a stream referred to by FMarshalledCallback.  This reference can
      be retrieved later in order to invoke SynchronizedCallback on this 
      thread.
}
procedure TfraConceptHTMLDetail.PrepareForSynchronizedCallback;
begin
  OleCheck(
      CoMarshalInterThreadInterfaceInStream(
          IID_ISynchronizable,
          Self,
          FMarshalledCallback));  
end;  // TfraConceptHTMLDetail.PrepareForSynchronizedCallback

{-------------------------------------------------------------------------------
  Prepares the first part of the HTML, up to and including the names of the 
      taxa. 
}
procedure TfraConceptHTMLDetail.PrepareNamesHTML;
begin
  // Main title
  FHTMLBody := AnchorHeading(VarToStr(FRecordsets[0].Fields['ItemName'].Value));
  // List synonyms
  FHTMLBody := FHTMLBody + BulletListFirstColumn(ResStr_ListSynonyms,
      FRecordsets[1]);
  // All known synonyms
  FHTMLBody := FHTMLBody + BulletListFirstColumn(ResStr_AllKnownSynonyms,
      FRecordsets[2]);
end;  // TfraConceptHTMLDetail.PrepareNamesHTML 

{-------------------------------------------------------------------------------
  Generates HTML for the list of sources 
}
function TfraConceptHTMLDetail.Sources(ARecordset: _Recordset): string;
  
  //This function converts RTF to plaintext.
  function RTFToPlain(lstRTF: string): string;
  var
    lRichEdit : TRichEdit;
    BStream: TStringStream;
    j:Integer;
    lForm : TForm;
  begin
   Result:='';
   lForm := TForm.Create(nil);
   lRichEdit := TRichEdit.create(lForm);
   BStream:=TStringStream.create(lstRTF);
   with lRichEdit do
   begin
     try
      visible:=False;
      Parent:=lForm;
      PlainText:=false;
      lines.LoadFromStream(BStream);
      for j := 0 to lines.count-1 do
      begin
        Result:=Result+Lines[j];
      end;
      modified:=True;
      PlainText:=true;
      Result:=Lines.Text;
     finally
      lForm.Free;
      BStream.Free;
     end;
   end;
  end;
  
begin
  if ARecordset.RecordCount=0 then
    Result := ''
  else begin
    Result := MiniHeaderStyle(ResStr_Sources);
    with ARecordset do begin
      MoveFirst;
      while not EOF do begin
        Result := Result +
            '<A HREF="'+SOURCE_TAG+ARecordset.Fields['Source_Key'].Value+'">';
        Result := Result + Fields['AuthorAndDate'].Value + ', ';
        Result := Result + RtfToPlain(Fields['SourceTitle'].Value);
        Result := Result + '</A>';
        MoveNext;
      end; // while
    end; // with
    Result := Para(Result);
  end;
end;  // TfraConceptHTMLDetail.Sources 

{+------------------------------------------------------------------------------
  Call the synchronization interface stored in FMarshalledCallback, and release
  the stream.
}
procedure TfraConceptHTMLDetail.Synchronize(ID: Integer);
var
  lSyncObj: ISynchronizable;
begin
  { need to AddRef here because the next API call implicitly calls Release }
  (FMarshalledCallback as IUnknown)._AddRef; 
  try 
    OleCheck(
        CoGetInterfaceAndReleaseStream(
            FMarshalledCallback,
            IID_ISynchronizable,
            lSyncObj));
  finally
    FMarshalledCallback := nil;
  end;
  
  if ID <> SYNC_NOOP then lSyncObj.SynchronizedCallback(ID);
end;  // TfraConceptHTMLDetail.RetrieveSynchronizationInterface

{-------------------------------------------------------------------------------
}
procedure TfraConceptHTMLDetail.SynchronizedCallback(ID: Integer);
begin
  case ID of
    SYNC_LOAD_HTML_NAMES:   LoadHtmlNames;
    SYNC_LOAD_HTML_DETAILS: LoadHtmlDetails;
  end;
end;  // TfraConceptHTMLDetail.SynchronizedCallback

end.

