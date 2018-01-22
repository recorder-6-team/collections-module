{===============================================================================
  Unit:        ThesaurusNavigatorDiagramContainer

  Defines:     TfraThesaurusNavigatorDiagramContainer

  Description: Page control container for the navigator and diagrams

  Created:     November 2003

  Model:       Thesaurus.mpb

  Last revision information:
    $Revision: 5 $
    $Date: 11/08/04 15:05 $
    $Author: Ericsalmon $

===============================================================================}
unit ThesaurusNavigatorDiagramContainer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDragFrameUnit, ThesaurusNavigator, ComCtrls, ThesaurusDiagram,
  XPMenu;

type
  TNewDiagramEvent = procedure (Sender: TObject; ADiagram: TThesaurusDiagram) 
      of object;
  {-----------------------------------------------------------------------------
    Container frame for the thesaurus navigator and diagrams components.
  }
  TfraThesaurusNavigatorDiagramContainer = class (TBaseDragFrame)
    fraThesaurusNavigator: TfraThesaurusNavigator;
    pcNavigatorAndDiagrams: TPageControl;
    tsNavigator: TTabSheet;
    procedure pcNavigatorAndDiagramsChange(Sender: TObject);
  private
    FOnNewDiagram: TNewDiagramEvent;
    FXPMenu: TXPMenu;
    procedure AddCreateDiagramPage;
    procedure AddDiagramToTab(ADiagram: TThesaurusDiagram);
    procedure SetOnNewDiagram(Value: TNewDiagramEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateNewDiagram(AFileName: string='');
    property OnNewDiagram: TNewDiagramEvent read FOnNewDiagram write
        SetOnNewDiagram;
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
  end;
  
implementation

{$R *.dfm}

uses
  LuxembourgConstants, DropTarget;

{-==============================================================================
    TfraThesaurusNavigatorDiagramContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraThesaurusNavigatorDiagramContainer.Create(AOwner: TComponent);
begin
  inherited;
  AddCreateDiagramPage;
end;  // TfraThesaurusNavigatorDiagramContainer.Create 

{-------------------------------------------------------------------------------
}
destructor TfraThesaurusNavigatorDiagramContainer.Destroy;
begin
  fraThesaurusNavigator.Free;
  inherited;
end;  // TfraThesaurusNavigatorDiagramContainer.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfraThesaurusNavigatorDiagramContainer.AddCreateDiagramPage;
begin
  with TTabSheet.Create(self) do begin
    PageControl := pcNavigatorAndDiagrams;
    Caption := '<New Diagram>';
    Tag := 1; // so we know it is not a diagram.
  end; // with
end;  // TfraThesaurusNavigatorDiagramContainer.AddCreateDiagramPage 

{-------------------------------------------------------------------------------
  Adds a diagram instance to the active tab, and creates a new tab to create 
      the next new diagram. 
}
procedure TfraThesaurusNavigatorDiagramContainer.AddDiagramToTab(ADiagram: 
    TThesaurusDiagram);
begin
  if pcNavigatorAndDiagrams.ActivePage.Tag=1 then begin
    pcNavigatorAndDiagrams.ActivePage.Tag := 0;
    pcNavigatorAndDiagrams.ActivePage.Caption := 'Diagram ' +
        IntToStr(pcNavigatorAndDiagrams.PageCount-1);
    ADiagram.Parent := pcNavigatorAndDiagrams.ActivePage;
    ADiagram.Align := alClient;
    RegisterDropComponent(ADiagram, ADiagram.DropConcept, [TN_CONCEPT],  
        [CF_JNCCDATA]);
    AddCreateDiagramPage;
    if Assigned(FOnNewDiagram) then
      FOnNewDiagram(Self, ADiagram);
  end;
  if Assigned(XPMenu) then XPMenu.InitComponent(ADiagram.PopupMenu);
end;  // TfraThesaurusNavigatorDiagramContainer.AddDiagramToTab

{-------------------------------------------------------------------------------
  Switches to the New Diagram tab and creates a new diagram on it.  Used
      externally when, for example, the File Open dialog causes a new diagram 
      to be created. 
}
procedure TfraThesaurusNavigatorDiagramContainer.CreateNewDiagram(AFileName: 
    string='');
var
  lIdx: Integer;
  lNewDiagram: TThesaurusDiagram;
begin
  for lIdx := 0 to pcNavigatorAndDiagrams.PageCount-1 do
    if pcNavigatorAndDiagrams.Pages[lIdx].Tag = 1 then begin
      pcNavigatorAndDiagrams.ActivePage := pcNavigatorAndDiagrams.Pages[lIdx];
      lNewDiagram := TThesaurusDiagram.Create(self);
      lNewDiagram.ReadXML(AFileName);
      AddDiagramToTab(lNewDiagram);
    end;
end;  // TfraThesaurusNavigatorDiagramContainer.CreateNewDiagram

{-------------------------------------------------------------------------------
  Adds a new diagram to the New Diagram tab.  Adds the selected concept to the
      centre of the diagram, if there is one.
}
procedure TfraThesaurusNavigatorDiagramContainer.pcNavigatorAndDiagramsChange(
    Sender: TObject);
var
  lNewDiagram: TThesaurusDiagram;
begin
  if pcNavigatorAndDiagrams.ActivePage.Tag=1 then begin
    lNewDiagram := TThesaurusDiagram.Create(Self);
    AddDiagramToTab(lNewDiagram);
    if Assigned(fraThesaurusNavigator.tvHierarchy.Selected) then
      // Add the selected concept, approximately in the centre
      lNewDiagram.AddConcept(TTermNode(
          fraThesaurusNavigator.tvHierarchy.Selected).ConceptKey,
          lNewDiagram.Width div 2, lNewDiagram.Height div 2);
  end;
end;  // TfraThesaurusNavigatorDiagramContainer.pcNavigatorAndDiagramsChange

{-------------------------------------------------------------------------------
  Accessor method for event 
}
procedure TfraThesaurusNavigatorDiagramContainer.SetOnNewDiagram(Value: 
    TNewDiagramEvent);
begin
  FOnNewDiagram := Value;
end;  // TfraThesaurusNavigatorDiagramContainer.SetOnNewDiagram 

end.







