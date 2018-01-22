unit SpecimenLabel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImageListButton, ExtCtrls, Registry,
  FolderBrowser, XMLDoc, XMLIntf, ExceptionForm, DataClasses, Recorder2000_TLB;

type
  TdlgSpecimenLabel = class(TForm)
    tvSelectedReport: TTreeView;
    lblTreeView: TLabel;
    chkCreateLabel: TCheckBox;
    Bevel1: TBevel;
    txtReportPath: TEdit;
    lblReportPath: TLabel;
    mmReportDescription: TRichEdit;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    procedure btnOkClick(Sender: TObject);
    procedure tvSelectedReportDblClick(Sender: TObject);
    procedure tvSelectedReportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FfileNames : TStringList;
    FfileDescriptions: TStringList;
    FfileNumber : Integer;
    FKeyList: TStringList;
    FReportList: IXmlReportList;
    procedure CreateNewLabelForSpecimen(const AKey: String);
    function GenerateOutput: Boolean;
    procedure LoadTreeView();
    procedure TreeViewAddNode(const menuPath, text : String; const node : TTreeNode);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItemKey(const AKey: String);
  end;

var
  dlgSpecimenLabel: TdlgSpecimenLabel;

implementation

uses
  ApplicationSettings, GeneralFunctions, GeneralData, COMClasses,
  ComObj, LuxembourgConstants, ResourceStrings, InterfaceDataModule;//FilterResult,

{$R *.dfm}
{-==============================================================================
    TdlgSpecimenLabel
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgSpecimenLabel.Create(AOwner: TComponent);
var
  reportRegistry : TRegistry;
begin
  inherited;
  reportRegistry := TRegistry.Create;
  reportRegistry.RootKey := HKEY_CURRENT_USER;
  if reportRegistry.OpenKey('Software\Dorset Software\Recorder 6\Settings', False) then
  begin
    txtReportPath.Text := reportRegistry.ReadString('Report Path');
  end;
  reportRegistry.Free;
  FKeyList := TStringList.Create;
  FfileNames := TStringList.Create;
  FfileDescriptions := TStringList.Create;
  mmReportDescription.Clear;
end;  // TdlgSpecimenLabel.Create


{-------------------------------------------------------------------------------
}
destructor TdlgSpecimenLabel.Destroy;
begin
  FKeyList.Free;
  FfileNames.Free;
  FfileDescriptions.Free;
  inherited;
end;  // TdlgSpecimenLabel.Destroy   


{-------------------------------------------------------------------------------
}
procedure TdlgSpecimenLabel.AddItemKey(const AKey: String);
begin
  FKeyList.Add(AKey);
end;  // TdlgSpecimenLabel.AddItemKey


{-------------------------------------------------------------------------------
A function to generate output - if labels are required these are generated, then
the report requested is run.
Returns : A value indicatining whether the output was sucessfully generated.
-------------------------------------------------------------------------------}
function TdlgSpecimenLabel.GenerateOutput: Boolean;
var
  i, j: Integer;
  lCursor: TCursor;
  lKeyList : TEditablekeylist;
  lComKeyList : TCOMKeyList;
begin
  lCursor := HourglassCursor;//crHourGlass;

  lKeyList := TEditableKeyList.Create;
  lKeylist.SetTable(TN_SPECIMEN_UNIT);

  try
    // Create a new label for each specimen
    if chkCreateLabel.Checked then
    begin
      for i := 0 to FKeyList.Count - 1 do
        CreateNewLabelForSpecimen(FKeyList[i]);
    end;

    // Create Key list to pass to the report
    lKeyList.AddItem(FKeyList[0],'');
    for j := 1 to FKeyList.Count - 1 do
      // Note no ' at either end of the string; they get parsed in later
    begin
      lKeyList.AddItem(FKeyList[j],'');
    end;
    lComKeyList := TComKeyList.Create(lKeyList);
    FReportList.Report[integer(tvSelectedReport.Selected.Data)].Execute(lComKeyList as IKeyList);
    Result := True;
    DefaultCursor(lCursor);
  except
    Result := False;
    DefaultCursor(lCursor);
  end; // try
end;  // TdlgSpecimenLabel.GenerateOutput


{-------------------------------------------------------------------------------
A function to create a label - Details are passed to a stored procedure for this
explicit purpose.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenLabel.CreateNewLabelForSpecimen(const AKey: String);
var
  Params: Array of Variant;
begin
  SetLength(Params, 4);
  Params[0] := '@Collection_Unit_Key';
  Params[1] := AKey;
  Params[2] := '@SessionID';
  Params[3] := AppSettings.SessionID;

  dmGeneral.RunStoredProc('usp_SpecimensLabel_CreateSpecimenLabel', Params);
end; // TdlgSpecimenLabel.CreateNewLabelForSpecimen


{-------------------------------------------------------------------------------
A function to load a hierarchy of reports into the treeview.  The hierarcy is
derived from the CustomReports.menupath attribute of each file.  The names and
descriptions of the files are also stored in appropriate lists.
}
procedure TdlgSpecimenLabel.LoadTreeView();
var
path, text, menuPath: String;
rec : TSearchRec;
i, j, k : Integer;
lXMLDoc : IXMLDocument;
subElements, subSubElements : IXMLNodeList;
validReport : Boolean;
begin
  with (CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder6) do
    FReportList := GetAvailableXmlReports(ResStr_Specimen);
  path := IncludeTrailingPathDelimiter(txtReportPath.Text);
  for i := 0 to FReportList.ReportCount - 1 do
  begin;
    FfileNames.Add('');
    FfileDescriptions.Add('No description available.');
  end;

  tvSelectedReport.Images := dmInterface.ilButtons;
  tvSelectedReport.Items.BeginUpdate;
  tvSelectedReport.Items.Clear;

  if FindFirst(path + '*.xml', faAnyFile - faDirectory, rec) = 0 then
  begin
    try
      repeat
        lXMLDoc := NewXMLDocument;
        lXMLDoc.LoadFromFile(path+rec.Name);
        try
          if (AnsiCompareText(lXMLDoc.DocumentElement.LocalName, 'CustomReport') = 0) then
          begin
            // Find all <SQL> <Where> elements to see whether they have attribute
            // keytype = 'Specimen'
            subElements := lXMLDoc.DocumentElement.ChildNodes;
            validReport := false;
            for j:=0 to subElements.Count - 1 do
            begin
              if (AnsiCompareText (subElements[j].LocalName, 'SQL') = 0) then
                begin
                  subSubElements :=subElements[j].ChildNodes;
                  for k := 0 to subSubElements.Count - 1 do
                    if ((AnsiCompareText (subSubElements[k].LocalName, 'Where') = 0)
                    and (subSubElements[k].Attributes['keytype'] <> null))
                    then
                      if (AnsiCompareText(subSubElements[k].Attributes['keytype'], 'Specimen') = 0) then
                        begin
                          validReport := true;
                          break;
                        end; // if attribute keytype = 'Specimen'
                  break;
                end; // if <sql> element
              end; //for j
            if validReport then
            begin
              text := lXMLDoc.DocumentElement.Attributes['title'];
              menuPath := lXMLDoc.DocumentElement.Attributes['menupath'];
              for i := 0 to FReportList.ReportCount - 1 do
              begin;
                if (FReportList.Report[i].ReportPath + FReportList.Report[i].ReportTitle
                       = menuPath + text) then
                begin
                  FfileNames[i] := rec.Name;
                  FfileNumber := i;
                  if lXMLDoc.DocumentElement.Attributes['description'] <> null then
                    FfileDescriptions[i] := lXMLDoc.DocumentElement.Attributes['description'];
                  break;
                end
              end;
              TreeViewAddNode(menuPath, text, nil);
            end;  // if keytype = 'Specimen'
          end; //  if .xml file is a custom report and has attribut keytype.
        finally
        end;  // try
      until FindNext(rec) <> 0;
    finally
      FindClose(rec);
    end;
  end; // if there exists an .xml file.

  tvSelectedReport.Items.EndUpdate;        
end; // TdlgSpecimenLabel.LoadTreeView


{-------------------------------------------------------------------------------
A function to add a node to the tree view in the correct position.
Parameters :
    menuPath : The menuPath FROM the path of the parent node
    text : The title (or appropriate text) to be associated with the node.
    node : The parent node (or nil if the node is top level).
}
procedure TdlgSpecimenLabel.TreeViewAddNode(const menuPath, text : String ; const node : TTreeNode);
var firstPart, remainingPart : String;
breakPosition, itemCount, i : integer;
added : Boolean;
lNode : TTreeNode;
begin
  if (menuPath = '') then
  begin
    lNode := tvSelectedReport.Items.AddChild(node, text);
    lNode.Data := pointer(FfileNumber);
    lNode.ImageIndex := 1;
    lNode.SelectedIndex := 1;
  end
  else //menuPath <>''
  begin
    breakPosition := AnsiPos('\', menuPath);
    if breakPosition <> 0 then
    begin
      firstPart := Copy(menuPath,0,breakPosition-1);
      remainingPart := Copy(menuPath, breakPosition+1, MaxInt);
    end
    else //breakPosition = 0
    begin
      firstPart := Copy(menuPath, 0, MaxInt);
      remainingPart := '';
    end;

    added := false;

    // If the node is nil then menupath starts at top level node.
    if (node = nil) then
    begin
      itemCount := tvSelectedReport.Items.Count;
      for i:=0 to itemCount - 1 do
      begin
        if (tvSelectedReport.Items[i].Parent = nil) then
        begin
          if (tvSelectedReport.Items[i].Text = firstPart) then
          begin
            TreeViewAddNode(remainingPart, text, tvSelectedReport.Items[i]);
            added := true;
            break;
          end; // if (tvSelectedReport.Items[i].Text = firstPart)
        end; // if (tvSelectedReport.Items[i].Parent = nil)
      end; // for
      if not added then
      begin
        lNode := tvSelectedReport.Items.Add(nil, firstPart);
        lNode.ImageIndex := 11;
        lNode.SelectedIndex := 11;
        TreeViewAddNode(remainingPart, text, lNode);
      end;
    end
    // If node is not nil then menupath does not start at a top level node.
    else
    begin
      lNode := node.getFirstChild;
      added := false;
      while (lNode <> nil) do
      begin;
        if (lNode.Text = firstPart) then
        begin
          TreeViewAddNode(remainingPart, text, lNode);
          added := true;
          break;
        end; // if
        lNode := node.GetNextChild(lNode)
      end; // while
      if not added then
      begin
        lNode := tvSelectedReport.Items.AddChild(node, firstPart);
        lNode.ImageIndex := 11;
        lNode.SelectedIndex := 11;
        TreeViewAddNode(remainingPart, text, lNode);
      end;
    end; // if (node = nil) else
  end;  // if (menuPath = '') else
end; // TdlgSpecimenLabel.TreeViewAddNode


{-------------------------------------------------------------------------------
If output is generated return to calling screen.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenLabel.btnOkClick(Sender: TObject);
begin
   if GenerateOutput then
     ModalResult := mrOk;
end; // TdlgSpecimenLabel.btnOkClick


{-------------------------------------------------------------------------------
If a report is double clicked then call btnOkClick.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenLabel.tvSelectedReportDblClick(Sender: TObject);
begin
   if not tvSelectedReport.Selected.HasChildren then
     Self.btnOkClick(Sender);
end; // TdlgSpecimenLabel.tvSelectedReportDblClick


{-------------------------------------------------------------------------------
A function to populate the Tmemo with the description of the report selected.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenLabel.tvSelectedReportClick(Sender: TObject);
var lNode : TTreeNode;
i : Integer;
begin
  btnOk.Enabled := false;
  mmReportDescription.Clear;
  lNode := tvSelectedReport.Selected;
  if not (lNode = nil) then
  begin
    if not lNode.HasChildren then
    begin
      for i:=0 to tvSelectedReport.Items.Count do
      begin
        if tvSelectedReport.Items[i] = lNode then
        begin
          btnOk.Enabled := true;
          with mmReportDescription do
          begin
            SelAttributes.Color := clBlue;
            SelAttributes.Size := 10;
            SelAttributes.Style := [fsBold];
            Lines.Add(lNode.Text);
            Lines.Add(FfileDescriptions[integer(lNode.Data)]);
          end;
          break;
        end; // if tvSelectedReport.Items[i] = lNode
      end; // for
    end; // if not lNode.HasChildren
  end; // check if lNode exists
end; // TdlgSpecimenLabel.tvSelectedReportClick

{-------------------------------------------------------------------------------
The treeview can't be loaded when the form is created as recorder creates the form
twice - once when the collections module is loaded and once when the form is
displayed - this means it also tries to 'destroy' it twice if it is in the create
method.  This causes issues due to the use of
'CreateOleObject('Recorder2000.AutoApplicationSettings')'.
-------------------------------------------------------------------------------}
procedure TdlgSpecimenLabel.FormCreate(Sender: TObject);
begin
  LoadTreeView;
end;

end.
