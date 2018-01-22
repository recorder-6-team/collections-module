{===============================================================================
  Unit:           CommonNameFetchQueue

  Defines:        TCommonNameFetchQueue

  Description:    Implements a queue for fetching common names to display in a
                  hierarchy

  Created:        August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 23/01/04 9:08 $
    $Author: Johnvanbreda $

===============================================================================}
unit CommonNameFetchQueue;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, RapTree, GeneralData, ADODb, UserMessages, Variants, ExGrid,
  ExceptionForm, ResourceStrings, ApplicationSettings, InterfaceDataModule;

type
  {-----------------------------------------------------------------------------
    Interface allowing a node to control access to the concept name part of the 
    node text.  This is needed if the node has a caption that is not just a 
    concept name, e.g. a specimen determination plus the registration number.
  }
  IConceptNameCaption = interface (IInterface)
    ['{32FA35D2-6256-4975-8083-06730AE1F404}']
    function GetConceptCaption: string;
    procedure SetConceptCaption(const Value: string);
    property ConceptCaption: string read GetConceptCaption write 
        SetConceptCaption;
  end;
  
  ECommonNameFetchQueueException = class (TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    Class that implements a queue of TFlynodes that are awaiting a common name 
    from the database.
  }
  TCommonNameFetchQueue = class (TWinControl)
  private
    FQueue: TStringList;
    FTreeView: TRapidTree;
    procedure GetCommonNameCallback(ATarget: TObject; AValue: Variant);
    procedure ProcessItem;
    procedure SetTreeView(Value: TRapidTree);
    procedure WMGetCommonName(var Msg: TMessage); message WM_GETCOMMONNAME;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const AConceptKey: string; AFlyNode: TFlyNode);
    procedure Clear;
    procedure ProcessWhenReady;
    property TreeView: TRapidTree read FTreeView write SetTreeView;
  end;
  

implementation

const
  IID_IConceptNameCaption = '{32FA35D2-6256-4975-8083-06730AE1F404}';

{-==============================================================================
    TCommonNameFetchQueue
===============================================================================}
{-------------------------------------------------------------------------------
  Construct the object.
  Owner should handle WM_GETCOMMONNAME and call the ProcessItem method. 
}
constructor TCommonNameFetchQueue.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  FQueue := TStringList.Create;
  // As this is a TWinControl, ensure it doesn't appear
  Width := 0;
  Height := 0;
  Visible := False;
end;  // TCommonNameFetchQueue.Create 

{-------------------------------------------------------------------------------
  Destroy the object 
}
destructor TCommonNameFetchQueue.Destroy;
begin
  FQueue.Free;
  
  inherited Destroy;
end;  // TCommonNameFetchQueue.Destroy 

{-------------------------------------------------------------------------------
  Adds a concept key to the list to fetch. 
}
procedure TCommonNameFetchQueue.Add(const AConceptKey: string; AFlyNode: 
    TFlyNode);
begin
  if not Assigned(TreeView) then
    raise ECommonNameFetchQueueException.Create(Format(ResStr_InvalidMethodCall,
        ['TCommonNameFetchQueue.Add']));
  FQueue.AddObject(AConceptKey, AFlyNode);
end;  // TCommonNameFetchQueue.Add 

{-------------------------------------------------------------------------------
  Clears the queue.  Cancels any pending asynchronous queries. 
}
procedure TCommonNameFetchQueue.Clear;
begin
  if FQueue.Count>0 then begin
    dmGeneral.CancelAsyncCommands(GetCommonNameCallback);
    FQueue.Clear;
  end;
end;  // TCommonNameFetchQueue.Clear 

{-------------------------------------------------------------------------------
  Method called after the completion of an asynchronous query to fetch a node 
      common name.  Updates the node and repaints it if it is visible on 
      screen. 
}
procedure TCommonNameFetchQueue.GetCommonNameCallback(ATarget: TObject; AValue: 
    Variant);
var
  lIntf: IConceptNameCaption;
begin
  if not VarIsNull(AValue) then begin
    if AValue<>TFlyNode(ATarget).Text then begin
      if Supports(ATarget, IConceptNameCaption, lIntf) then
        lIntf.ConceptCaption := AValue + ' (' + TFlyNode(ATarget).Text + ')'
      else
        TFlyNode(ATarget).Caption := AValue + ' (' + TFlyNode(ATarget).Text +
            ')';
      // repaint the node if it is visible
      if (TFlyNode(ATarget).GetRow >= TreeView.MouseCoord(0,0).Y)
          and ((TFlyNode(ATarget).GetRow <= TreeView.MouseCoord(0,
              TreeView.ClientHeight).Y)
          or (TreeView.MouseCoord(0, TreeView.ClientHeight).Y=-1)) then
        dmInterface.RepaintNode(TFlyNode(ATarget), TreeView);
    end;
  end;
  // Keep requesting new common names until we have them all
  ProcessWhenReady;
end;  // TCommonNameFetchQueue.GetCommonNameCallback 

{-------------------------------------------------------------------------------
  Processes a single item on the queue.  Triggers an asyncronous query to 
      obtain its common name. 
}
procedure TCommonNameFetchQueue.ProcessItem;
begin
  if FQueue.Count>0 then begin
    dmGeneral.GetAsyncData('usp_CommonName_Get', [
        '@ConceptKey', FQueue[0]],
        '@CommonName', TFlyNode(
            FQueue.Objects[0]),
        GetCommonNameCallback);
    FQueue.Delete(0);
  end; // if
end;  // TCommonNameFetchQueue.ProcessItem 

{-------------------------------------------------------------------------------
  Posts a message to itself to begin processing the queue when all other 
      messages complete. 
}
procedure TCommonNameFetchQueue.ProcessWhenReady;
begin
  if AppSettings.DisplayCommonNames then
    PostMessage(Self.Handle, WM_GETCOMMONNAME, 0, 0);
end;  // TCommonNameFetchQueue.ProcessWhenReady 

{-------------------------------------------------------------------------------
  Accessor method.  Instance of TRapidTree the queue is operating for.
}
procedure TCommonNameFetchQueue.SetTreeView(Value: TRapidTree);
begin
  FTreeView := Value;
end;  // TCommonNameFetchQueue.SetTreeView

{-------------------------------------------------------------------------------
  Message handler that handles WM_GETCOMMONNAME.  Calls ProcessItem. 
}
procedure TCommonNameFetchQueue.WMGetCommonName(var Msg: TMessage);
begin
  ProcessItem;
end;  // TCommonNameFetchQueue.WMGetCommonName 



end.
