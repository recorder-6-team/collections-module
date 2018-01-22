{===============================================================================
  Unit:        ReportObjects

  Defines:     TCollectionsReportObject
               TCollectionsReport
               TDetailsReport
               TListReport
               TReportSection
               TReportBlockInSection
               TReportBlock
               TReportBlockOrder

  Description: Classes managing the collections module reports generation

  Created:     August 2004

  Last revision information:
    $Revision: 13 $
    $Date: 19/09/08 17:07 $
    $Author: Ericsalmon $

===============================================================================}
unit ReportObjects;

interface

uses
  ComObj, ActiveX, SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StandardReports_TLB, ExceptionForm, Contnrs,
  ADODB;

resourcestring
  ResStr_MaxItemsReached = 'The maximum number of items that can be included in a list report is 10,000. '+
      'Only the first 10,000 items will be included.';

type
  EReportObject = class(TExceptionPath)
  end;
  
  TRefreshRowEvent = procedure (Sender: TObject; ASectionIndex: integer) of
      object;
  TReportSettings = class(TObject)
  private
    FAllReportBlocks: TStringList;
    FIsListReport: Boolean;
    FReportedItemKeys: string;
    FReportFile: TextFile;
    FReportFilesDirectory: string;
    FReportKey: string;
    FReportPath: string;
    FReportTitle: string;
    FSummary: string;
    procedure SetIsListReport(const Value: Boolean);
    procedure SetReportKey(const Value: string);
    procedure SetReportPath(const Value: string);
  protected
    procedure AddFileToReportOutput(const AFileName: string);
    procedure AddLineToReportOutput(const AText: string);
    procedure AddLineToReportOutputWithReplace(const AText, ATag,
        AReplacementText: string);
    function PopulateTemplate(ATemplate: string; ARecordset: _Recordset):
        string;
    function ReplaceTags(const AText: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property AllReportBlocks: TStringList read FAllReportBlocks;
    property IsListReport: Boolean read FIsListReport write SetIsListReport;
    property ReportedItemKeys: string read FReportedItemKeys write
        FReportedItemKeys;
    property ReportFile: TextFile read FReportFile write FReportFile;
    property ReportFilesDirectory: string read FReportFilesDirectory write
        FReportFilesDirectory;
    property ReportKey: string read FReportKey write SetReportKey;
    property ReportPath: string read FReportPath write SetReportPath;
    property ReportTitle: string read FReportTitle write FReportTitle;
    property Summary: string read FSummary write FSummary;
  end;
  
  TCollectionsReportObject = class(TObject)
  end;
  
  {--------------------------------------------------------------------------
  ---
    Wrapper class for a Report_Block_Order record.
  }
  TReportBlockOrder = class(TCollectionsReportObject)
  private
    FItemName: string;
    FOrderSQL: string;
    procedure SetItemName(const Value: string);
    procedure SetOrderSQL(const Value: string);
  public
    property ItemName: string read FItemName write SetItemName;
    property OrderSQL: string read FOrderSQL write SetOrderSQL;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Loads the record from the supplied Report_Block_In_Section_Key.
  }
  TReportBlock = class(TCollectionsReportObject)
  private
    FCountError: Boolean;
    FCountErrorHint: string;
    FFooterFile: string;
    FHeaderFile: string;
    FIncludeInReport: Boolean;
    FRecordCount: Integer;
    FReportBlockKey: string;
    FReportBlockOrders: TObjectList;
    FRowFile: string;
    FSelectedOrderIndex: Integer;
    FTitle: string;
    function GetReportBlockOrder(Index: Integer): TReportBlockOrder;
    procedure SetCountError(Value: Boolean);
    procedure SetFooterFile(const Value: string);
    procedure SetHeaderFile(const Value: string);
    procedure SetIncludeInReport(Value: Boolean);
    procedure SetRecordCount(Value: Integer);
    procedure SetReportBlockKey(const Value: string);
    procedure SetRowFile(const Value: string);
    procedure SetSelectedOrderIndex(Value: Integer);
    procedure SetTitle(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadOrders;
    function ReportBlockOrderCount: Integer;
    property CountError: Boolean read FCountError write SetCountError;
    property CountErrorHint: string read FCountErrorHint write FCountErrorHint;
    property FooterFile: string read FFooterFile write SetFooterFile;
    property HeaderFile: string read FHeaderFile write SetHeaderFile;
    property IncludeInReport: Boolean read FIncludeInReport write
        SetIncludeInReport;
    property RecordCount: Integer read FRecordCount write SetRecordCount;
    property ReportBlockKey: string read FReportBlockKey write
        SetReportBlockKey;
    property ReportBlockOrder[Index: Integer]: TReportBlockOrder read
        GetReportBlockOrder;
    property RowFile: string read FRowFile write SetRowFile;
    property SelectedOrderIndex: Integer read FSelectedOrderIndex write
        SetSelectedOrderIndex;
    property Title: string read FTitle write SetTitle;
  end;
  
  {--------------------------------------------------------------------------
  ---
    Update the count for this sections report block.
  }
  TReportBlockInSection = class(TCollectionsReportObject)
  private
    FCountSQL: string;
    FPopulationSQL: string;
    FReportBlock: TReportBlock;
    procedure OutputBlockHeader;
    procedure SetCountSQL(const Value: string);
    procedure SetPopulationSQL(const Value: string);
    procedure SetReportBlock(Value: TReportBlock);
  public
    procedure OutputToFile(const ASectionItemKey: string; AIncBlockHeader: boolean);
    procedure UpdateCount(const AItemKey: string);
    property CountSQL: string read FCountSQL write SetCountSQL;
    property PopulationSQL: string read FPopulationSQL write SetPopulationSQL;
    property ReportBlock: TReportBlock read FReportBlock write SetReportBlock;
  end;
  
  TReportSection = class(TCollectionsReportObject)
  private
    FItemNameMacro: string;
    FReportBlockInSections: TObjectList;
    FSectionListSQL: string;
    FSectionHeader: String;
    function GetReportBlockInSection(Index: Integer): TReportBlockInSection;
    procedure LoadReportBlocks(const AKey: string);
    procedure OutputSectionItem(const ASectionItemRecord: _Recordset; ARepeatIndex: integer);
    procedure SetItemNameMacro(const Value: string);
    procedure SetSectionListSQL(const Value: string);
    procedure SetSectionHeader(const Value: String);
  public
    constructor Create(const AKey: string); overload;
    destructor Destroy; override;
    procedure AddSectionBlock(ASectionBlock: TReportBlockInSection);
    procedure OutputToFile;
    function ReportBlockInSectionCount: Integer;
    procedure UpdateCounts;
    property ItemNameMacro: string read FItemNameMacro write SetItemNameMacro;
    property ReportBlockInSection[Index: Integer]: TReportBlockInSection read GetReportBlockInSection;
    property SectionListSQL: string read FSectionListSQL write SetSectionListSQL;
    property SectionHeader: String read FSectionHeader write SetSectionHeader;
  end;

  TCollectionsReport = class(TAutoObject)
  private
    FFileName: string;
    FReportSections: TObjectList;
    function GetReportSections(Index: Integer): TReportSection;
  protected
    FOnRefreshRowEvent: TRefreshRowEvent;
    FWarning: string;
    procedure CloseFile;
    procedure GenerateReport; virtual; safecall;
    function Get_ItemKey: WideString; safecall;
    function Get_ReportKey: WideString; safecall;
    procedure LoadSections; virtual; abstract;
    procedure OpenFile(const AFileName: string);
    class function RptFooterFile: string;
    class function RptHeaderFile: string;
    class function RptSummaryFile: string;
    procedure Set_ItemKey(const Value: WideString); safecall;
    procedure Set_ReportKey(const Value: WideString); safecall;
  public
    destructor Destroy; override;
    procedure BuildReport(const AFileName: string);
    procedure Initialize; override;
    function ReportSectionCount: Integer;
    procedure UpdateCounts; virtual;
    property ReportSections[Index: Integer]: TReportSection read
        GetReportSections;
  end;

  TListReport = class(TCollectionsReport, IListReportGenerator)
  private
    FItemKeys: TStringList;
  protected
    procedure LoadSections; override;
    procedure GenerateReport; override; safecall;
  public
    destructor Destroy; override;
    procedure AddItemKey(const AKey: widestring); safecall;
    procedure Initialize; override;
  end;
  
  TDetailsReport = class(TCollectionsReport, IDetailsReportGenerator)
  protected
    procedure LoadSections; override;
  public
    procedure UpdateCounts; override;
  end;
  

var
  ReportSettings: TReportSettings;

implementation

uses
  Variants, GeneralData, ResourceStrings, ApplicationSettings, GeneralFunctions,
  ReportConfiguration, ComServ, BaseADODataModule;

{-==============================================================================
    TReportSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TReportSettings.Create;
begin
  FAllReportBlocks := TStringList.Create;
end;  // TReportSettings.Create

{-------------------------------------------------------------------------------
}
destructor TReportSettings.Destroy;
var
  i: Integer;
begin
  with FAllReportBlocks do
    for i := 0 to Count - 1 do
      if Assigned(Objects[i]) then Objects[i].Free;
  FAllReportBlocks.Free;
  
  inherited Destroy;
end;  // TReportSettings.Destroy 

{-------------------------------------------------------------------------------
  Adds the content of the supplied file name (in Standard Report Template Path)
      to the Report output.
}
procedure TReportSettings.AddFileToReportOutput(const AFileName: string);
var
  lFileToAdd: TextFile;
  lStringToCopy: string;
begin
  AssignFile(lFileToAdd, ReportSettings.ReportFilesDirectory + AFileName);
  Reset(lFileToAdd);
  try
    // Copy file across line by line
    while not EOF(lFileToAdd) do begin
      ReadLn(lFileToAdd, lStringToCopy);
      AddLineToReportOutput(lStringToCopy);
    end;
  finally
    CloseFile(lFileToAdd);
  end; // try
end;  // TReportSettings.AddFileToReportOutput 

{-------------------------------------------------------------------------------
  Adds a line of text to the report output. 
}
procedure TReportSettings.AddLineToReportOutput(const AText: string);
var
  lText: string;
begin
  // URIs use '/', not '\' which is Windows path syntax!
  lText := StringReplace(
      AText,
      '<#StandardReportTemplatePath>',
      StringReplace(ReportSettings.ReportFilesDirectory, '\', '/', [rfReplaceAll, rfIgnoreCase]),
      [rfReplaceAll, rfIgnoreCase]);
  lText := StringReplace(lText, '<#ReportTitle>', ReportSettings.ReportTitle,
      [rfReplaceAll, rfIgnoreCase]);
  lText := StringReplace(lText, '<#ReportSummary>', ReportSettings.Summary,
      [rfReplaceAll, rfIgnoreCase]);
  WriteLn(ReportSettings.ReportFile, lText);
end;  // TReportSettings.AddLineToReportOutput 

{-------------------------------------------------------------------------------
  Adds a line of text to the report output. Replaces the specified tag with a
      specified string.
}
procedure TReportSettings.AddLineToReportOutputWithReplace(const AText, ATag,
    AReplacementText: string);
var
  lText: string;
begin
  lText := StringReplace(AText, ATag, AReplacementText,
      [rfReplaceAll, rfIgnoreCase]);
  WriteLn(ReportSettings.ReportFile, lText);
end;  // TReportSettings.AddLineToReportOutputWithReplace 

{-------------------------------------------------------------------------------
  Loops through the specified recordset and replaces the tags in the template
      string with values from the recordset.
}
function TReportSettings.PopulateTemplate(ATemplate: string; ARecordset:
    _Recordset): string;
var
  lIdx: Integer;
  lNewValue: string;
begin
  Result := ATemplate;
  // for each field we have, replace the tags with the supplied value
  for lIdx := 0 to ARecordset.Fields.Count-1 do begin
    lNewValue := VarToStr(ARecordset.Fields[lIdx].Value);
    if lNewValue = '' then lNewValue := '&nbsp;';
    Result := StringReplace(Result, '<#' + ARecordset.Fields[lIdx].Name + '>',
                            lNewValue, [rfReplaceAll, rfIgnoreCase]);
  end;
end;  // TReportSettings.PopulateTemplate 

{-------------------------------------------------------------------------------
  Updates the tags in the supplied report output SQL text. 
}
function TReportSettings.ReplaceTags(const AText: string): string;
begin
  Result := StringReplace(AText, '<#ReportKey>',
      ReportSettings.ReportedItemKeys,
          [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<#ReportKeys>',
      ReportSettings.ReportedItemKeys,
          [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<#UserDomainMask>', IntToStr(
      AppSettings.DomainMask),
          [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<#SessionID>', AppSettings.SessionID,
          [rfReplaceAll, rfIgnoreCase]);
  if AppSettings.DisplayCommonNames then
    Result := StringReplace(Result, '<#ShowCommonNames>', '1',
            [rfReplaceAll, rfIgnoreCase])
  else
    Result := StringReplace(Result, '<#ShowCommonNames>', '0',
            [rfReplaceAll, rfIgnoreCase]);
end;  // TReportSettings.ReplaceTags 

{-------------------------------------------------------------------------------
}
procedure TReportSettings.SetIsListReport(const Value: Boolean);
begin
  FIsListReport := Value;
end;  // TReportSettings.SetIsListReport 

{-------------------------------------------------------------------------------
}
procedure TReportSettings.SetReportKey(const Value: string);
begin
  FReportKey := Value;
end;  // TReportSettings.SetReportKey 

{-------------------------------------------------------------------------------
}
procedure TReportSettings.SetReportPath(const Value: string);
begin
  FReportPath := Value;
end;  // TReportSettings.SetReportPath 

{-==============================================================================
    TReportBlockOrder
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TReportBlockOrder.SetItemName(const Value: string);
begin
  FItemName := Value;
end;  // TReportBlockOrder.SetItemName 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TReportBlockOrder.SetOrderSQL(const Value: string);
begin
  FOrderSQL := Value;
end;  // TReportBlockOrder.SetOrderSQL 

{-==============================================================================
    TReportBlock
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TReportBlock.Create;
begin
  inherited Create;
  
  FReportBlockOrders := TObjectList.Create;
  FRecordCount := 0;
  FSelectedOrderIndex := -1;
  FIncludeInReport := True;
end;  // TReportBlock.Create 

{-------------------------------------------------------------------------------
}
destructor TReportBlock.Destroy;
begin
  FReportBlockOrders.Free;
  
  inherited Destroy;
end;  // TReportBlock.Destroy 

{-------------------------------------------------------------------------------
  Accessor method 
}
function TReportBlock.GetReportBlockOrder(Index: Integer): TReportBlockOrder;
begin
  Result := TReportBlockOrder(FReportBlockOrders[Index]);
end;  // TReportBlock.GetReportBlockOrder 

{-------------------------------------------------------------------------------
  Retrieves details of each available order by clause using the supplied
      Report_Block_In_Section_Key.
}
procedure TReportBlock.LoadOrders;
var
  lReportBlockOrder: TReportBlockOrder;
begin
  with dmGeneral.GetRecordset('usp_ReportBlockOrders_Select',
      ['@ReportBlockKey', ReportBlockKey]) do begin
    while not EOF do begin
      lReportBlockOrder := TReportBlockOrder.Create;
      lReportBlockOrder.ItemName := Fields['Item_Name'].Value;
      lReportBlockOrder.OrderSQL := Fields['Order_Clause_SQL'].Value;
      FReportBlockOrders.Add(lReportBlockOrder);
      MoveNext;
    end;
    // select first listed order as the default
    if ReportBlockOrderCount>0 then
      FSelectedOrderIndex := 0;
  end;
end;  // TReportBlock.LoadOrders 

{-------------------------------------------------------------------------------
  Returns the count of report block orders available. 
}
function TReportBlock.ReportBlockOrderCount: Integer;
begin
  Result := FReportBlockOrders.Count;
end;  // TReportBlock.ReportBlockOrderCount 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetCountError(Value: Boolean);
begin
  FCountError := Value;
end;  // TReportBlock.SetCountError 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetFooterFile(const Value: string);
begin
  FFooterFile := Value;
end;  // TReportBlock.SetFooterFile 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetHeaderFile(const Value: string);
begin
  FHeaderFile := Value;
end;  // TReportBlock.SetHeaderFile 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetIncludeInReport(Value: Boolean);
begin
  FIncludeInReport := Value and (RecordCount<>0);
end;  // TReportBlock.SetIncludeInReport 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TReportBlock.SetRecordCount(Value: Integer);
begin
  FRecordCount := Value;
end;  // TReportBlock.SetRecordCount 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetReportBlockKey(const Value: string);
begin
  FReportBlockKey := Value;
end;  // TReportBlock.SetReportBlockKey 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetRowFile(const Value: string);
begin
  FRowFile := Value;
end;  // TReportBlock.SetRowFile 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TReportBlock.SetSelectedOrderIndex(Value: Integer);
begin
  FSelectedOrderIndex := Value;
end;  // TReportBlock.SetSelectedOrderIndex 

{-------------------------------------------------------------------------------
}
procedure TReportBlock.SetTitle(const Value: string);
begin
  FTitle := Value;
end;  // TReportBlock.SetTitle 

{-==============================================================================
    TReportBlockInSection
===============================================================================}
{-------------------------------------------------------------------------------
  If a block item name macro is present, use it to generate the block header 
}
procedure TReportBlockInSection.OutputBlockHeader;
var
  lTemplate: TStringList;
begin
  if ReportBlock.Title<>'' then begin
    lTemplate := TStringList.Create;
    try
      lTemplate.LoadFromFile(ReportSettings.ReportFilesDirectory +
          'Block_Header.txt');
      ReportSettings.AddLineToReportOutputWithReplace(lTemplate.Text,
          '<#BlockTitle>', FReportBlock.Title);
    finally
      lTemplate.Free;
    end;
  end;
end;  // TReportBlockInSection.OutputBlockHeader 

{-------------------------------------------------------------------------------
  Output the block into the report file, for this block instance within the
      section item instance.
}
procedure TReportBlockInSection.OutputToFile(const ASectionItemKey: string;
    AIncBlockHeader: boolean);
var
  lSQL: string;
  lRecordset: _Recordset;
  lTemplate: TStringList;
  lRowTemplate: string;
  
  // Output a template if the chosen file exists
  procedure OutputTemplate(const AFileName: string);
  begin
    if FileExists(ReportSettings.ReportFilesDirectory + AFileName) then begin
      lTemplate.LoadFromFile(ReportSettings.ReportFilesDirectory +
          AFileName);
      ReportSettings.AddLineToReportOutputWithReplace(lTemplate.Text,
          '<#BlockTitle>', ReportBlock.Title);
    end;
  end;
  
begin
  if ReportBlock.IncludeInReport then begin
    // Prepare the SQL, with order by and all tags done
    lSQL := PopulationSQL;
    if ReportBlock.ReportBlockOrderCount>0 then
      lSQL := lSQL + ' ' + ReportBlock.ReportBlockOrder[
          ReportBlock.SelectedOrderIndex].OrderSQL;
    lSQL := StringReplace(lSQL, '<#SectionKey>', ASectionItemKey, [rfReplaceAll,
        rfIgnoreCase]);
    lSQL := ReportSettings.ReplaceTags(lSQL);
  
    try
      lRecordset := dmGeneral.ExecuteSQL(lSQL, True);
    except on E:Exception do
      raise EReportObject.Create(Format(ResStr_ExecuteSQLFailed,
          [ReportBlock.Title + ': ' + E.Message]));
    end;
    if Assigned(lRecordset) then
      // Only display heading and tables where there actually is data to show.
      if lRecordset.Recordcount > 0 then begin
        lTemplate := TStringList.Create;
        try
          // Add the Title and Header file
          if AIncBlockHeader then
            OutputBlockHeader;
          OutputTemplate(ReportBlock.HeaderFile);
          // Output rows
          if FileExists(ReportSettings.ReportFilesDirectory +
              ReportBlock.RowFile) then begin
            lTemplate.LoadFromFile(ReportSettings.ReportFilesDirectory +
                ReportBlock.RowFile);
            lRowTemplate := lTemplate.Text;
            with lRecordset do begin
              while not EOF do begin
                ReportSettings.AddLineToReportOutput(ReportSettings.
                    PopulateTemplate(lRowTemplate, lRecordset));
                MoveNext;
              end; // while
            end; // with
          end; // if
          OutputTemplate(ReportBlock.FooterFile);
        finally
          lTemplate.Free;
        end;
      end; // if
  end; // if
end;  // TReportBlockInSection.OutputToFile 

{-------------------------------------------------------------------------------
}
procedure TReportBlockInSection.SetCountSQL(const Value: string);
begin
  FCountSQL := Value;
end;  // TReportBlockInSection.SetCountSQL 

{-------------------------------------------------------------------------------
}
procedure TReportBlockInSection.SetPopulationSQL(const Value: string);
begin
  FPopulationSQL := Value;
end;  // TReportBlockInSection.SetPopulationSQL 

{-------------------------------------------------------------------------------
}
procedure TReportBlockInSection.SetReportBlock(Value: TReportBlock);
begin
  FReportBlock := Value;
end;  // TReportBlockInSection.SetReportBlock 

{-------------------------------------------------------------------------------
  Updates the report blocks count for records retrieved for this block in
      section.
}
procedure TReportBlockInSection.UpdateCount(const AItemKey: string);
var
  lCountSQL: string;
begin
  lCountSQL := StringReplace(FCountSQL,'<#SectionKey>', AItemKey,
      [rfReplaceAll, rfIgnoreCase]);
  try
    with dmGeneral.ExecuteSQL(ReportSettings.ReplaceTags(lCountSQL), True) do
      ReportBlock.RecordCount := ReportBlock.RecordCount + Fields[0].Value;
  except
    on E:Exception do begin
      ReportBlock.CountError := True;
      ReportBlock.CountErrorHint := E.Message;
    end;
  end;
end;  // TReportBlockInSection.UpdateCount 

{-==============================================================================
    TReportSection
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TReportSection.Create(const AKey: string);
begin
  inherited Create;
  
  FReportBlockInSections := TObjectList.Create;
  // if a details report, then the section has a key
  if AKey<>'' then
    LoadReportBlocks(AKey);
end;  // TReportSection.Create 

{-------------------------------------------------------------------------------
}
destructor TReportSection.Destroy;
begin
  FReportBlockInSections.Free;
  
  inherited Destroy;
end;  // TReportSection.Destroy 

{-------------------------------------------------------------------------------
  Allows a list report to add a dummy report block in section object to the
      report.
}
procedure TReportSection.AddSectionBlock(ASectionBlock: TReportBlockInSection);
begin
  FReportBlockInSections.Add(ASectionBlock);
end;  // TReportSection.AddSectionBlock 

{-------------------------------------------------------------------------------
}
function TReportSection.GetReportBlockInSection(Index: Integer):
    TReportBlockInSection;
begin
  Result := TReportBlockInSection(FReportBlockInSections[Index]);
end;  // TReportSection.GetReportBlockInSection 

{-------------------------------------------------------------------------------
  Retrieves the list of report blocks for the report section. 
}
procedure TReportSection.LoadReportBlocks(const AKey: string);
var
  lReportBlockInSection: TReportBlockInSection;
  lReportBlockIndex: Integer;
begin
  with dmGeneral.GetRecordset('usp_ReportBlocks_Select_ForSection', ['@Key',
      AKey]) do begin
    while not EOF do begin
      lReportBlockInSection := TReportBlockInSection.Create;
      lReportBlockInSection.PopulationSQL := Fields['Population_SQL'].Value;
      lReportBlockInSection.CountSQL :=
          Fields['Population_SQL_Record_Count'].Value;
      lReportBlockIndex := ReportSettings.AllReportBlocks.IndexOf(
          Fields['Report_Block_Key'].Value);
      if lReportBlockIndex = -1 then begin
        // First time this report block has been encountered in the report, so instantiate
        lReportBlockInSection.ReportBlock := TReportBlock.Create;
        lReportBlockInSection.ReportBlock.ReportBlockKey :=
            Fields['Report_Block_Key'].Value;
        lReportBlockInSection.ReportBlock.Title := Fields['Title'].Value;
        lReportBlockInSection.ReportBlock.HeaderFile := VarToStr(
            Fields['Header_File'].Value);
        lReportBlockInSection.ReportBlock.RowFile := Fields['Row_File'].Value;
        lReportBlockInSection.ReportBlock.FooterFile := VarToStr(
            Fields['Footer_File'].Value);
        lReportBlockInSection.ReportBlock.LoadOrders;
        ReportSettings.AllReportBlocks.AddObject(
            Fields['Report_Block_Key'].Value,
            lReportBlockInSection.ReportBlock);
      end
      else
        // link to existing block
        lReportBlockInSection.ReportBlock := TReportBlock(
            ReportSettings.AllReportBlocks.Objects[lReportBlockIndex]);
      FReportBlockInSections.Add(lReportBlockInSection);
      MoveNext;
    end;
  end;
end;  // TReportSection.LoadReportBlocks 

{-------------------------------------------------------------------------------
  Outputs a single iteration of the section in the section list sql. 
}
procedure TReportSection.OutputSectionItem(const ASectionItemRecord: _Recordset;
    ARepeatIndex: integer);
var
  i: Integer;
  lIncBlockHeader: boolean;

  procedure AddSectionHeader(const ATitle: string);
  var
    lTemplate: TStringList;
    lSectionHeader: string;
  begin
    lTemplate := TStringList.Create;
    try
      if SectionHeader <> '' then
        lTemplate.LoadFromFile(ReportSettings.ReportFilesDirectory + SectionHeader);

      lSectionHeader := lTemplate.Text;
      lSectionHeader := StringReplace(lSectionHeader, '<#Header>', ATitle,
          [rfReplaceAll, rfIgnoreCase]);
      lSectionHeader := StringReplace(lSectionHeader, '<#BlockTitle>', ATitle,
          [rfReplaceAll, rfIgnoreCase]);
      if Assigned(ASectionItemRecord) then
        lSectionHeader := ReportSettings.PopulateTemplate(lSectionHeader,
            ASectionItemRecord);
      ReportSettings.AddLineToReportOutputWithReplace(lSectionHeader,
          '<#SectionNumber>', IntToStr(ARepeatIndex));
    finally
      lTemplate.Free;
    end;
  end;

begin
  // Output section title
  if FItemNameMacro<>'' then
    AddSectionHeader(ItemNameMacro);
  for i := 0 to ReportBlockInSectionCount-1 do begin
    lIncBlockHeader := (ItemNameMacro<>'') and (not ReportSettings.IsListReport)
        and (ReportBlockInSection[i].ReportBlock.RecordCount>0);
    if Assigned(ASectionItemRecord) then
      ReportBlockInSection[i].OutputToFile(VarToStr(
          ASectionItemRecord.Fields[0].Value), lIncBlockHeader)
    else begin
      // if a report block stands on its own, its title is output using the section
      // header style
      if (not (lIncBlockHeader or ReportSettings.IsListReport)) 
          and (ReportBlockInSection[i].ReportBlock.RecordCount>0) then
        AddSectionHeader(ReportBlockInSection[i].ReportBlock.Title);
      ReportBlockInSection[i].OutputToFile('', lIncBlockHeader);
    end;
  end; // for
end;  // TReportSection.OutputSectionItem

{-------------------------------------------------------------------------------
  Output the section's entire content into the report file.
}
procedure TReportSection.OutputToFile;
var
  lTemplate: TStringList;
  lRs: _Recordset;
begin
  lTemplate := TStringList.Create;
  try
  if FSectionListSQL<>'' then begin
    lRs := dmGeneral.ExecuteSQL(ReportSettings.ReplaceTags(FSectionListSQL), True);
    while not lRs.EOF do begin
      OutputSectionItem(lRs, lRs.AbsolutePosition);
      lRs.MoveNext;
    end;
  end else
    // no section items to list, so just output 1
    OutputSectionItem(nil, 0);
  finally
    lTemplate.Free;
  end;
end;  // TReportSection.OutputToFile 

{-------------------------------------------------------------------------------
  Retrieves count of blocks in this section. 
}
function TReportSection.ReportBlockInSectionCount: Integer;
begin
  Result := FReportBlockInSections.Count;
end;  // TReportSection.ReportBlockInSectionCount 

{-------------------------------------------------------------------------------
  Accessor method 
}
procedure TReportSection.SetItemNameMacro(const Value: string);
begin
  FItemNameMacro := Value;
end;  // TReportSection.SetItemNameMacro 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TReportSection.SetSectionListSQL(const Value: string);
begin
  FSectionListSQL := Value;
end;  // TReportSection.SetSectionListSQL

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TReportSection.SetSectionHeader(const Value: String);
begin
  FSectionHeader := Value;
end;  // TReportSection.SetSectionHeader

{-------------------------------------------------------------------------------
  Updates the list of record counts for each block. 
}
procedure TReportSection.UpdateCounts;
var
  i: Integer;
  lRs: _Recordset;
begin
  if FSectionListSQL<>'' then begin
    lRs := dmGeneral.ExecuteSQL(ReportSettings.ReplaceTags(FSectionListSQL), True);
    while not lRs.EOF do begin
      for i := 0 to ReportBlockInSectionCount-1 do
        // first (only) field in section list sql gives us the key to report in the section
        ReportBlockInSection[i].UpdateCount(lRs.Fields[0].Value);
      lRs.MoveNext;
    end;
  end else
    // no section items to list
    for i := 0 to ReportBlockInSectionCount-1 do
      ReportBlockInSection[i].UpdateCount('');
end;  // TReportSection.UpdateCounts 

{-==============================================================================
    TCollectionsReport
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TCollectionsReport.Destroy;
begin
  FreeAndNil(ReportSettings);
  FreeAndNil(FReportSections);
  if TdmGeneral.Allocated then TdmGeneral.Discard;
  inherited;
end;  // TCollectionsReport.Destroy 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReport.BuildReport(const AFileName: string);
var
  i: Integer;
  lCursor: TCursor;
begin
  dmGeneral.Recorder.RecorderMainForm.StartProgressBar;
  dmGeneral.Recorder.RecorderMainForm.Progress := 0;
  lCursor := HourglassCursor;
  try
    OpenFile(AFileName);
    try
      // Add the Report Header file
      ReportSettings.AddFileToReportOutput(RptHeaderFile);
      // Add the Report Summary file if required
      if ReportSettings.Summary <> '' then
        ReportSettings.AddFileToReportOutput(RptSummaryFile);
      dmGeneral.Recorder.RecorderMainForm.Progress := 10;
      for i := 0 to ReportSectionCount-1 do begin
        ReportSections[i].OutputToFile;
        // '* 80' because 10% is allocated for the header and 10% for the footer.
        dmGeneral.Recorder.RecorderMainForm.Progress :=
                (i * 80 div ReportSectionCount) + 10;
      end;
      // Add the report footer file
      ReportSettings.AddFileToReportOutput(RptFooterFile);
      dmGeneral.Recorder.RecorderMainForm.Progress := 100;
    finally
      CloseFile;
    end;
    ShellFile(ReportSettings.ReportPath);
  finally
    DefaultCursor(lCursor);
    dmGeneral.Recorder.RecorderMainForm.Progress := 0;
    dmGeneral.Recorder.RecorderMainForm.StopProgressBar;
  end;
end;  // TCollectionsReport.BuildReport 

{-------------------------------------------------------------------------------
  Closes the text file object used to write the report. 
}
procedure TCollectionsReport.CloseFile;
begin
  System.CloseFile(ReportSettings.ReportFile);
end;  // TCollectionsReport.CloseFile 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReport.GenerateReport;
begin
  LoadSections;
  with TdlgReportConfiguration.Create(nil, Self, FWarning) do begin
    try
      Application.ProcessMessages;
      if ShowModal=mrOk then
        BuildReport(eOutputPath.Text);
    finally
      Free;
    end;
  end;
end;  // TCollectionsReport.GenerateReport 

{-------------------------------------------------------------------------------
  Accessor method 
}
function TCollectionsReport.GetReportSections(Index: Integer): TReportSection;
begin
  Result := TReportSection(FReportSections[Index]);
end;  // TCollectionsReport.GetReportSections

{-------------------------------------------------------------------------------
}
function TCollectionsReport.Get_ItemKey: WideString;
begin
  Result := ReportSettings.ReportedItemKeys;
end;  // TCollectionsReport.Get_ItemKey

{-------------------------------------------------------------------------------
}
function TCollectionsReport.Get_ReportKey: WideString;
begin
  Result := ReportSettings.ReportKey;
end;  // TCollectionsReport.Get_ReportKey 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReport.Initialize;
begin
  inherited;
  ReportSettings := TReportSettings.Create;
  FReportSections := TObjectList.Create;
end;  // TCollectionsReport.Initialize 

{-------------------------------------------------------------------------------
  Creates the physical disk file 
}
procedure TCollectionsReport.OpenFile(const AFileName: string);
begin
  FFileName := AFileName;
  AssignFile(ReportSettings.ReportFile, AFileName);
  Rewrite(ReportSettings.ReportFile);
end;  // TCollectionsReport.OpenFile 

{-------------------------------------------------------------------------------
  Retrieves the count of sections in the report. 
}
function TCollectionsReport.ReportSectionCount: Integer;
begin
  Result := FReportSections.Count;
end;  // TCollectionsReport.ReportSectionCount 

{-------------------------------------------------------------------------------
  Constant.  Retrieve the report header file name. 
}
class function TCollectionsReport.RptFooterFile: string;
begin
  Result := 'Report_Footer.txt';
end;  // TCollectionsReport.RptFooterFile 

{-------------------------------------------------------------------------------
  Constant.  Retrieve the report header file name. 
}
class function TCollectionsReport.RptHeaderFile: string;
begin
  Result := 'Report_Header.txt';
end;  // TCollectionsReport.RptHeaderFile 

{-------------------------------------------------------------------------------
  Constant.  Retrieve the report header file name. 
}
class function TCollectionsReport.RptSummaryFile: string;
begin
  Result := 'Report_Summary.txt';
end;  // TCollectionsReport.RptSummaryFile 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReport.Set_ItemKey(const Value: WideString);
begin
  ReportSettings.ReportedItemKeys := Value;
end;  // TCollectionsReport.Set_ItemKey 

{-------------------------------------------------------------------------------
}
procedure TCollectionsReport.Set_ReportKey(const Value: WideString);
begin
  ReportSettings.ReportKey := Value;
end;  // TCollectionsReport.Set_ReportKey 

{-------------------------------------------------------------------------------
  Update the counts on the report configuration dialog.
  For list reports, the count is already set, so don't need anything in the
      base class implementation.
}
procedure TCollectionsReport.UpdateCounts;
begin
end;  // TCollectionsReport.UpdateCounts 

{-==============================================================================
    TListReport
===============================================================================}
{-------------------------------------------------------------------------------
  Clean up. 
}
destructor TListReport.Destroy;
begin
  FItemKeys.Free;
  inherited;
end;  // TListReport.Destroy 

{-------------------------------------------------------------------------------
  Add the key value of an item to include in the list report. 
}
procedure TListReport.AddItemKey(const AKey: widestring);
begin
  // Add quotes so we can use CommaText to get list for IN clause
  if FItemKeys.Count<10000 then
    FItemKeys.Add('''' + AKey + '''')
  else if FWarning='' then
    FWarning := ResStr_MaxItemsReached;
end;  // TListReport.AddItemKey

{-------------------------------------------------------------------------------
}
procedure TListReport.Initialize;
begin
  inherited;
  FItemKeys := TStringList.Create;
  ReportSettings.IsListReport := True;
end;  // TListReport.Initialize

{-------------------------------------------------------------------------------
  Load the single section that a list report is composed of. 
}
procedure TListReport.LoadSections;
var
  lReportSection: TReportSection;
  lReportBlockInSection: TReportBlockInSection;
  lReportBlockIndex: Integer;
begin
  with dmGeneral.GetRecordset('usp_ListReportAndBlock_Select',
      ['@Key', ReportSettings.ReportKey]) do begin
    ReportSettings.ReportTitle := Fields['Report_Title'].Value;
    lReportSection := TReportSection.Create('');
    lReportBlockInSection := TReportBlockInSection.Create;
  
    lReportBlockInSection.PopulationSQL := Fields['Population_SQL'].Value;
    lReportBlockIndex := ReportSettings.AllReportBlocks.IndexOf(
        Fields['Report_Block_Key'].Value);
    if lReportBlockIndex = -1 then begin
      // First time this report block has been encountered in the report, so instantiate
      lReportBlockInSection.ReportBlock := TReportBlock.Create;
      lReportBlockInSection.ReportBlock.Title := Fields['Report_Title'].Value;
      lReportBlockInSection.ReportBlock.ReportBlockKey :=
          Fields['Report_Block_Key'].Value;
      lReportBlockInSection.ReportBlock.HeaderFile := VarToStr(
          Fields['Header_File'].Value);
      lReportBlockInSection.ReportBlock.RowFile := Fields['Row_File'].Value;
      lReportBlockInSection.ReportBlock.FooterFile := VarToStr(
          Fields['Footer_File'].Value);
      lReportBlockInSection.ReportBlock.LoadOrders;
      ReportSettings.AllReportBlocks.AddObject(
          Fields['Report_Block_Key'].Value,
          lReportBlockInSection.ReportBlock);
    end
    else
      // link to existing block
      lReportBlockInSection.ReportBlock := TReportBlock(
          ReportSettings.AllReportBlocks.Objects[lReportBlockIndex]);
    lReportSection.AddSectionBlock(lReportBlockInSection);
    lReportBlockInSection.ReportBlock.RecordCount := FItemKeys.Count;
    FReportSections.Add(lReportSection);
  end;
end;  // TListReport.LoadSections

{-------------------------------------------------------------------------------
  When genrating the report, pass the list of item keys across
}
procedure TListReport.GenerateReport;
begin
  ReportSettings.ReportedItemKeys := FItemKeys.CommaText;
  inherited;
end;

{-==============================================================================
    TDetailsReport
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TDetailsReport.LoadSections;
var
  lReportSection: TReportSection;
begin
  with dmGeneral.GetRecordset('usp_DetailsReport_Select',
      ['@Key', ReportSettings.ReportKey]) do
    ReportSettings.ReportTitle := Fields['Report_Title'].Value;
  FReportSections.Clear;
  with dmGeneral.GetRecordset('usp_ReportSections_Select', ['@Key', ReportSettings.ReportKey]) do
  begin
    while not Eof do begin
      lReportSection := TReportSection.Create(Fields['Report_Section_Key'].Value);
      lReportSection.SectionListSQL := VarToStr(Fields['Section_List_SQL'].Value);
      lReportSection.SectionHeader  := VarToStr(Fields['Section_Header'].Value);
      lReportSection.ItemNameMacro  := VarToStr(Fields['Item_Name_Macro'].Value);
      FReportSections.Add(lReportSection);
      MoveNext;
    end;
    Close;
  end;
end;  // TDetailsReport.LoadSections 

{-------------------------------------------------------------------------------
}
procedure TDetailsReport.UpdateCounts;
var
  i: Integer;
begin
  for i := 0 to ReportSectionCount-1 do
    ReportSections[i].UpdateCounts;
end;  // TDetailsReport.UpdateCounts



initialization
  TAutoObjectFactory.Create(ComServer, TDetailsReport,
      Class_DetailsReportGenerator,
    ciMultiInstance, tmApartment);

  TAutoObjectFactory.Create(ComServer, TListReport,
      Class_ListReportGenerator,
    ciMultiInstance, tmApartment);

end.
