{===============================================================================
  Unit:        BrowserViewTypes

  Defines:     Subclasses of TBaseViewType

  Description: Classes managing the view type combo box options for the
               Collections Browser

  Model:       CollectionsBrowserGeneral.mpb

  Created:     Sept 2003

  Last revision information:
    $Revision: 47 $
    $Date: 21/06/16 12:20 $
    $Author: Christopherknight $

===============================================================================}
unit BrowserViewTypes;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  DataTypes, Registry, RapTree, ComCtrls, CollectionsBrowser_TLB,
  BrowserNodeFramework, ADODB;

type

  TViewType = class of TBaseViewType;

  EInvalidSearchIndexException = class(Exception)
  end;
  
  EInvalidViewTypeException = class(Exception)
  end;
  
  EInvalidSortOrderIndexException = class(Exception)
  end;
  
  {-----------------------------------------------------------------------------
    Valuations view type class.
  }
  TValuationsViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Tasks view type class.
  }
  TTasksViewType = class(TBaseViewType, ISortOrderProvider)
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSearchTextRequired(Index: Integer): Boolean; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Stores view type class.
  }
  TStoresViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private 
    FMetadata: TStringList;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  public
    constructor Create(AViewTypeManager: TViewTypeManager);
    destructor Destroy; override;
  end;

  {-----------------------------------------------------------------------------
    Specimens view type class.
  }
  TSpecimensViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    FMetadata: TStringList;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  public
    constructor Create(AViewTypeManager: TViewTypeManager);
    destructor Destroy; override;
  end;
  
  {-----------------------------------------------------------------------------
    Movements view type class.
  }
  TMovementsViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSearchTextRequired(Index: Integer): Boolean; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Loans view type class.
  }
  TLoansViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSearchTextRequired(Index: Integer): Boolean; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Jobs view type class.
  }
  TJobsViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  TInscriptionLabelViewType = class(TBaseViewType, ISortOrderProvider)
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSearchTextRequired(Index: Integer): Boolean; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function Get_SortOrderCaption(AIndex: Integer): WideString; safecall;
    function Get_SortOrderCount: Integer; safecall;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Enquiries view type class.
  }
  TEnquiriesViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSearchTextRequired(Index: Integer): Boolean; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Condition Checks view type class.
  }
  TConditionChecksViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  {-----------------------------------------------------------------------------
    Collections view type class.
  }
  TCollectionsViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    FMetadata: TStringList;
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  public
    constructor Create(AViewTypeManager: TViewTypeManager);
    destructor Destroy; override;
  end;
  
  {-----------------------------------------------------------------------------
    Accessions view type class.
  }
  TAccessionsViewType = class(TBaseViewType, IAddMenuOptions, ISortOrderProvider)
  private
    function AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
    function GetAddButtonMenuCaption(Index: Integer): String;
    function GetAddButtonMenuCaptionsCount: Integer;
    function GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
  protected
    function GetCanAdd: Boolean; override;
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetSortStoredProcIndex(Index: Integer): Integer; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText: string): String;
        override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override;
  end;
  
  TStoreHierarchyViewType = class(TBaseViewType, ISortOrderProvider)
  private
    FMetadata: TStringList;
  protected
    function GetImageIndex: Integer; override;
    function GetName: String; override;
    function GetNodeContext: TNodeContext; override;
    function GetNodeContextText: String; override;
    function GetPopulateTopLevelStoredProcName: String; override;
    function GetSearchCaption(Index: Integer): String; override;
    function GetSearchControlType(Index: integer): TSearchControlType; override;
    function GetSearchCount: Integer; override;
    function GetSearchStoredProcName(Index: Integer): String; override;
    function GetStoredProcParams: TVariantArray; override;
    function GetTopLevelNodeClass: TTopLevelNodeClass; override;
    function InternalGetSortOrderCaption(AIndex: integer): Widestring; override;
    function InternalGetSortOrderCount: Integer; override; 
  public
    constructor Create(AViewTypeManager: TViewTypeManager);
    destructor Destroy; override;
  end;
  
implementation

uses
  InterfaceDataModule, ResourceStrings, ApplicationSettings, Variants,
  BrowserNodeCollectionUnits, BrowserNodeConditionCheck, BrowserNodeSpecimen,
  BrowserNodeMovement, BrowserNodeCommon, LuxembourgConstants, VagueDate,
  GeneralData, BaseADODataModule;

{-==============================================================================
    TValuationsViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TValuationsViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Store node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TValuationTopLevelNode));
  TValuationTopLevelNode(Result).InitialiseNewNode;
end;  // TValuationsViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TValuationsViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TValuationsViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TValuationsViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowFinance and inherited GetCanAdd;
end;  // TValuationsViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetImageIndex: Integer;
begin
  Result := 7;
end;  // TValuationsViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetName: String;
begin
  Result := ResStr_Valuations;
end;  // TValuationsViewType.GetName 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetNodeContext: TNodeContext;
begin
  Result := ncValuation;
end;  // TValuationsViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetNodeContextText: String;
begin
  Result := 'ncValuation';
end;  // TValuationsViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Valuations_Select_ForTopLevel';
end;  // TValuationsViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_ValuationReferenceNumber;
    1:  Result := ResStr_ValuationValue;
    2:  Result := ResStr_ValuationDate;
    3:  Result := ResStr_ValuedBy;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TValuationsViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    1:  Result := ctNumber;
    3:  Result := ctName;
  else
    Result := ctNormal;
  end;
end;  // TValuationsViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetSearchCount: Integer;
begin
  Result := 4;
end;  // TValuationsViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Valuations_Select_ForSearchByReferenceNumber';
    1:  Result := 'usp_Valuations_Select_ForSearchByValue';
    2:  Result := 'usp_Valuations_Select_ForSearchByDate';
    3:  Result := 'usp_Valuations_Select_ForSearchByValuedBy';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TValuationsViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;
  
  SetLength(Result, Length(Result) + 2);
  Result[High(Result) -1] := '@UserID';
  Result[High(Result)] := AppSettings.UserID;
end;  // TValuationsViewType.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TValuationTopLevelNode;
end;  // TValuationsViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TValuationsViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 2 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TValuationsViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TValuationsViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Type;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TValuationsViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TValuationsViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TValuationsViewType.InternalGetSortOrderCount 

{-==============================================================================
    TTasksViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
function TTasksViewType.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TTasksViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetImageIndex: Integer;
begin
  Result := 27;
end;  // TTasksViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetName: String;
begin
  Result := ResStr_TasksIdentified;
end;  // TTasksViewType.GetName 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetNodeContext: TNodeContext;
begin
  Result := ncTask;
end;  // TTasksViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetNodeContextText: String;
begin
  Result := 'ncTasks';
end;  // TTasksViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Tasks_Select_ForTopLevel';
end;  // TTasksViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_TaskDateSet;
    1:  Result := ResStr_TaskIncompleteAndUrgent;
    2:  Result := ResStr_TaskStatus;
    3:  Result := ResStr_TaskType;
    4:  Result := ResStr_TaskPriority;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TTasksViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    2:  Result := ctStatusCombo;
    3:  Result := ctTaskTypeCombo;
    4:  Result := ctPriorityCombo;
  else
    Result := ctNormal;
  end;
end;  // TTasksViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetSearchCount: Integer;
begin
  Result := 5;
end;  // TTasksViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Tasks_Select_ForSearchByDateSet';
    1:  Result := 'usp_Tasks_Select_ForSearchByIncompleteAndUrgent';
    2:  Result := 'usp_Tasks_Select_ForSearchByStatus';
    3:  Result := 'usp_Tasks_Select_ForSearchByType';
    4:  Result := 'usp_Tasks_Select_ForSearchByPriority';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TTasksViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := not (Index = 1);
end;  // TTasksViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TTaskIdentifiedTopLevelNode;
end;  // TTasksViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TTasksViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 0 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else if ASearchCaptionIndex = 4 then begin
    if CompareText(ASearchText, ResStr_Low) = 0 then
      Result := '0'
    else if CompareText(ASearchText, ResStr_Medium) = 0 then
      Result := '1'
    else if CompareText(ASearchText, ResStr_High) = 0 then
      Result := '2'
    else if CompareText(ASearchText, ResStr_Urgent) = 0 then
      Result := '3';
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TTasksViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TTasksViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_DateSet;
    1:  Result := ResStr_Status;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TTasksViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TTasksViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TTasksViewType.InternalGetSortOrderCount 

{-==============================================================================
    TStoresViewType
===============================================================================} 
{-------------------------------------------------------------------------------
}
constructor TStoresViewType.Create(AViewTypeManager: TViewTypeManager);
var
  lParams: TVariantArray;
  lRecordset: _Recordset;
begin

  FMetadata := TStringList.Create;
  SetLength(lParams, 2);
  lParams[High(lParams) -1] := '@TableName';
  lParams[High(lParams)]    := 'Store';
  lRecordset := dmGeneral.GetRecordset('usp_MetadataAndType_Select', lParams);
  if lRecordset.RecordCount > 0 then begin
    lRecordset.MoveFirst;
    while not lRecordset.EOF do begin
      FMetadata.Add(lRecordset.Fields['Item_Name'].Value);
      lRecordset.MoveNext;
    end;
  end;
  inherited Create(AViewTypeManager);
end;  // TStoresViewType.Create

{-------------------------------------------------------------------------------
}
destructor TStoresViewType.Destroy;
begin
  FMetadata.Free;

  inherited Destroy;
end;  // TStoresViewType.Destroy

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TStoresViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Store node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TStoreTopLevelNode));
  TStoreTopLevelNode(Result).InitialiseNewNode;
end;  // TStoresViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TStoresViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TStoresViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TStoresViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TStoresViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetName: String;
begin
  Result := ResStr_Stores;
end;  // TStoresViewType.GetName 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetNodeContext: TNodeContext;
begin
  Result := ncStore;
end;  // TStoresViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetNodeContextText: String;
begin
  Result := 'ncStores';
end;  // TStoresViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Stores_Select_ForTopLevel';
end;  // TStoresViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_StoreName;
    1:  Result := ResStr_StoreReferenceNumber;
    2:  Result := ResStr_UsualLocationCode;
    3:  Result := ResStr_StoreType;
  else
    if (Index <= 3 + FMetadata.Count) then
      Result := FMetadata[Index-4]
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TStoresViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    3:  Result := ctStoreTypeCombo;
  else
    Result := ctNormal;
  end;
end;  // TStoresViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetSearchCount: Integer;
begin
  Result := 4 + FMetadata.Count;
end;  // TStoresViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Stores_Select_ForSearchByName';
    1:  Result := 'usp_Stores_Select_ForSearchByPreferredNumber';
    2:  Result := 'usp_Stores_Select_ForSearchByUsualLocationCode';
    3:  Result := 'usp_Stores_Select_ForSearchByStoreType';
  else
    if (Index <= 3 + FMetadata.Count) then
      Result := 'usp_Stores_Select_ForSearchByMetadata'
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TStoresViewType.GetSearchStoredProcName  

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;

  if (SearchDefaultIndex <= 3 + FMetadata.Count) and (SearchDefaultIndex > 3) then begin
    SetLength(Result, Length(Result) + 2);
    Result[High(Result) -1] := '@MetaDataType';
    Result[High(Result)] := FMetadata[SearchDefaultIndex-4];
  end;
end;  // TStoresViewType.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TStoresViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TStoreTopLevelNode;
end;  // TStoresViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TStoresViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Name;
    1:  Result := ResStr_RegistrationNumber;
    2:  Result := ResStr_CurrentLocationCode;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TStoresViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TStoresViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 3;
end;  // TStoresViewType.InternalGetSortOrderCount

{-==============================================================================
    TSpecimensViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpecimensViewType.Create(AViewTypeManager: TViewTypeManager);
var
  lParams: TVariantArray;
  lRecordset: _Recordset;
begin

  FMetadata := TStringList.Create;
  SetLength(lParams, 2);
  lParams[High(lParams) -1] := '@TableName';
  lParams[High(lParams)]    := 'Specimen';
  lRecordset := dmGeneral.GetRecordset('usp_MetadataAndType_Select', lParams);
  if lRecordset.RecordCount > 0 then begin
    lRecordset.MoveFirst;
    while not lRecordset.EOF do begin
      FMetadata.Add(lRecordset.Fields['Item_Name'].Value);
      lRecordset.MoveNext;
    end;
  end;
  inherited Create(AViewTypeManager);
end;  // TspecimensViewType.Create

{-------------------------------------------------------------------------------
}
destructor TSpecimensViewType.Destroy;
begin
  FMetadata.Free;

  inherited Destroy;
end;  // TspecimensViewType.Destroy

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TSpecimensViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Specimen node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TSpecimenTopLevelNode));
  TSpecimenTopLevelNode(Result).InitialiseNewNode;
end;  // TSpecimensViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TSpecimensViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TSpecimensViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TSpecimensViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetImageIndex: Integer;
begin
  Result := 3;
end;  // TSpecimensViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetName: String;
begin
  Result := ResStr_Specimens;
end;  // TSpecimensViewType.GetName 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetNodeContext: TNodeContext;
begin
  Result := ncSpecimen;
end;  // TSpecimensViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetNodeContextText: String;
begin
  Result := 'ncSpecimen';
end;  // TSpecimensViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Specimens_Select_ForTopLevel';
end;  // TSpecimensViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_PreferredNumber;
    1:  Result := ResStr_PreferredAccessionNumber;
    2:  Result := ResStr_AnyNumber;
    3:  Result := ResStr_PreferredDetermination;
    4:  Result := ResStr_AnyDetermination;
    5:  Result := ResStr_DeterminationInGroup;
    6:  Result := ResStr_SpecimenType;
    7:  Result := ResStr_SpecimenGatheringLocation;
    8:  Result := ResStr_SpecimenGatheringDate;
    9:  Result := ResStr_NomenclaturalStatus;
    10: Result := ResStr_ObservationLocation;
    11: Result := ResStr_GeoArea;
  else
    if (Index <= 11 + FMetadata.Count) then
      Result := FMetadata[Index - 12]
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TSpecimensViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    3, 4, 5:  Result := ctDetermination;
    6:  Result := ctSpecimenTypeCombo;
    7:  Result := ctLocation;
  else
    Result := ctNormal;
  end;
end;  // TSpecimensViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetSearchCount: Integer;
begin
  Result := 12 + FMetaData.Count;
end;  // TSpecimensViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Specimens_Select_ForSearchByPreferredNumber';
    1:  Result := 'usp_Specimens_Select_ForSearchByPreferredAccNumber';
    2:  Result := 'usp_Specimens_Select_ForSearchByAnyNumber';
    3:  Result := 'usp_Specimens_Select_ForSearchByPreferredDetermination';
    4:  Result := 'usp_Specimens_Select_ForSearchByAnyDetermination';
    5:  Result := 'usp_Specimens_Select_ForSearchByDeterminationInGroup';
    6:  Result := 'usp_Specimens_Select_ForSearchByType';
    7:  Result := 'usp_Specimens_Select_ForSearchByGatheringLocation';
    8:  Result := 'usp_Specimens_Select_ForSearchByGatheringDate';
    9:  Result := 'usp_Specimens_Select_ForSearchByNomenclaturalStatus';
    10: Result := 'usp_Specimens_Select_ForSearchByObservationLocation';
    11: Result := 'usp_Specimens_Select_ForSearchByGeoArea';
  else
    if (Index <= 11 + FMetadata.Count) then
      Result := 'usp_Specimens_Select_ForSearchByMetadata'
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TSpecimensViewType.GetSearchStoredProcName

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;

  if (SearchDefaultIndex <= 11 + FMetadata.Count) and (SearchDefaultIndex > 11) then begin
    SetLength(Result, Length(Result) + 2);
    Result[High(Result) -1] := '@MetaDataType';
    Result[High(Result)] := FMetadata[SearchDefaultIndex - 12];
  end;
end;  // TSpecimensViewType.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TSpecimenTopLevelNode;
end;  // TSpecimensViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TSpecimensViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 8 then
  begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TSpecimensViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TSpecimensViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Determination;
    1:  Result := ResStr_RegistrationNumber;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TSpecimensViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TSpecimensViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TSpecimensViewType.InternalGetSortOrderCount

{-==============================================================================
    TMovementsViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TMovementsViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Movement node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TMovementTopLevelNode));
  TMovementTopLevelNode(Result).MovementType := AMenuIndex+4;
  TMovementTopLevelNode(Result).InitialiseNewNode;
  // Set the correct node type
end;  // TMovementsViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddDestroyed;
    1:  Result := ResStr_AddDisposed;
    2:  Result := ResStr_AddInternalTransfer;
    3:  Result := ResStr_AddLost;
    4:  Result := ResStr_AddSold;
    5:  Result := ResStr_AddHostedMaterial;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TMovementsViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 6;
end;  // TMovementsViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TMovementsViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TMovementsViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetImageIndex: Integer;
begin
  Result := 9;
end;  // TMovementsViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetName: String;
begin
  Result := ResStr_Movements;
end;  // TMovementsViewType.GetName 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetNodeContext: TNodeContext;
begin
  Result := ncMovement;
end;  // TMovementsViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetNodeContextText: String;
begin
  Result := 'ncMovement';
end;  // TMovementsViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Movements_Select_ForTopLevel';
end;  // TMovementsViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_ReferenceNumber;
    1:  Result := ResStr_ExpectedCompletionDate;
    2:  Result := ResStr_OtherParty;
    3:  Result := ResStr_Destroyed;
    4:  Result := ResStr_Disposed;
    5:  Result := ResStr_InternalTransfer;
    6: Result := ResStr_Lost;
    7: Result := ResStr_Sold;
    8: Result := ResStr_HostedMaterial;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TMovementsViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    2:  Result := ctName;
  else
    Result := ctNormal;
  end;
end;  // TMovementsViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetSearchCount: Integer;
begin
  Result := 9;
end;  // TMovementsViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Movements_Select_ForSearchByReferenceNumber';
    1:  Result := 'usp_Movements_Select_ForSearchByExpectedCompletionDate';
    2:  Result := 'usp_Movements_Select_ForSearchByOtherParty';
    3,
    4,
    5,
    6,
    7,
    8:  Result := 'usp_Movements_Select_ForSearchByMovementType';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TMovementsViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := (Index in [0, 1, 2]);
end;  // TMovementsViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;
  
  SetLength(Result, Length(Result) + 4);
  Result[High(Result) -3] := '@MovementGroupType';
  Result[High(Result) -2] := 2;
  Result[High(Result) -1] := '@MovementSearchType';
  Result[High(Result)] := SearchDefaultIndex+1;
end;  // TMovementsViewType.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TMovementTopLevelNode;
end;  // TMovementsViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TMovementsViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 1 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TMovementsViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TMovementsViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Number;
    2:  Result := ResStr_Type;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TMovementsViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TMovementsViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 3;
end;  // TMovementsViewType.InternalGetSortOrderCount 

{-==============================================================================
    TLoansViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TLoansViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Loan node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TLoanTopLevelNode));
  TLoanTopLevelNode(Result).MovementType := AMenuIndex+2;
  TLoanTopLevelNode(Result).InitialiseNewNode;
  // Set the correct node type
end;  // TLoansViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddLoanIn;
    1:  Result := ResStr_AddLoanOut;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TLoansViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 2;
end;  // TLoansViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TLoansViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TLoansViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetImageIndex: Integer;
begin
  Result := 10;
end;  // TLoansViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetName: String;
begin
  Result := ResStr_Loans;
end;  // TLoansViewType.GetName 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetNodeContext: TNodeContext;
begin
  Result := ncLoan;
end;  // TLoansViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetNodeContextText: String;
begin
  Result := 'ncLoan';
end;  // TLoansViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Movements_Select_ForTopLevel';
end;  // TLoansViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_ReferenceNumber;
    1:  Result := ResStr_ExpectedCompletionDate;
    2:  Result := ResStr_OtherParty;
    3:  Result := ResStr_Overdue;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TLoansViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    2:  Result := ctName;
  else
    Result := ctNormal;
  end;
end;  // TLoansViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetSearchCount: Integer;
begin
  Result := 4;
end;  // TLoansViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Movements_Select_ForSearchByReferenceNumber';
    1:  Result := 'usp_Movements_Select_ForSearchByExpectedCompletionDate';
    2:  Result := 'usp_Movements_Select_ForSearchByOtherParty';
    3:  Result := 'usp_Movements_Select_ForSearchByOverdue';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TLoansViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := (Index in [0, 1, 2]);
end;  // TLoansViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;
  
  SetLength(Result, Length(Result) + Length(Result) + 2);
  Result[High(Result) -1] := '@MovementGroupType';
  Result[High(Result)] := 1;
end;  // TLoansViewType.GetStoredProcParams 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TLoanTopLevelNode;
end;  // TLoansViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TLoansViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 1 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TLoansViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TLoansViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Number;
    2:  Result := ResStr_Type;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TLoansViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TLoansViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 3;
end;  // TLoansViewType.InternalGetSortOrderCount 

{-==============================================================================
    TJobsViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TJobsViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Job node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TJobTopLevelNode));
  TJobTopLevelNode(Result).InitialiseNewNode;
end;  // TJobsViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TJobsViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TJobsViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  case AMenuIndex of
    0 : Result := True
  else
    Result := False;
  end;
end;  // TJobsViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetImageIndex: Integer;
begin
  Result := 26;
end;  // TJobsViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetName: String;
begin
  Result := ResStr_Jobs;
end;  // TJobsViewType.GetName 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetNodeContext: TNodeContext;
begin
  Result := ncJob;
end;  // TJobsViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetNodeContextText: String;
begin
  Result := 'ncJob';
end;  // TJobsViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Jobs_Select_ForTopLevel';
end;  // TJobsViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_JobReferenceNumber;
    1:  Result := ResStr_JobDate;
    2:  Result := ResStr_JobStaff;
    3:  Result := ResStr_JobStatus;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TJobsViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    2:  Result := ctIndividual;
    3:  Result := ctStatusCombo;
  else
    Result := ctNormal;
  end;
end;  // TJobsViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetSearchCount: Integer;
begin
  Result := 4;
end;  // TJobsViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Jobs_Select_ForSearchByRefNumber';
    1:  Result := 'usp_Jobs_Select_ForSearchByDate';
    2:  Result := 'usp_Jobs_Select_ForSearchByStaff';
    3:  Result := 'usp_Jobs_Select_ForSearchByStatus';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TJobsViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TJobTopLevelNode;
end;  // TJobsViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TJobsViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 1 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TJobsViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TJobsViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Name;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TJobsViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TJobsViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TJobsViewType.InternalGetSortOrderCount 

{-==============================================================================
    TInscriptionLabelViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetCanAdd: Boolean;
begin
  Result := False;
end;  // TInscriptionLabelViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetImageIndex: Integer;
begin
  Result := 40;
end;  // TInscriptionLabelViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetName: String;
begin
  Result := ResStr_Specimens;
end;  // TInscriptionLabelViewType.GetName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetNodeContext: TNodeContext;
begin
  Result := ncInscriptionLabel;
end;  // TInscriptionLabelViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetNodeContextText: String;
begin
  Result := 'ncInscriptionLabel';
end;  // TInscriptionLabelViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_InscriptionsAndLabels_Select_ForTopLevel';
end;  // TInscriptionLabelViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetSearchCaption(Index: Integer): String;
begin
end;  // TInscriptionLabelViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  Result := ctNormal;
end;  // TInscriptionLabelViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetSearchCount: Integer;
begin
  Result := 0;
end;  // TInscriptionLabelViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetSearchStoredProcName(Index: Integer): String;
begin
end;  // TInscriptionLabelViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := false;
end;  // TInscriptionLabelViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TInscriptionLabelTopLevelNode;
end;  // TInscriptionLabelViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.Get_SortOrderCaption(AIndex: Integer): WideString;
begin
end;  // TInscriptionLabelViewType.Get_SortOrderCaption 

{-------------------------------------------------------------------------------
}
function TInscriptionLabelViewType.Get_SortOrderCount: Integer;
begin
  Result := 0;
end;  // TInscriptionLabelViewType.Get_SortOrderCount 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.  There are none available for
      this class.
}
function TInscriptionLabelViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
end;  // TInscriptionLabelViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TInscriptionLabelViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 0;
end;  // TInscriptionLabelViewType.InternalGetSortOrderCount 

{-==============================================================================
    TEnquiriesViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TEnquiriesViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Enquiry node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TEnquiryTopLevelNode));
  TEnquiryTopLevelNode(Result).InitialiseNewNode;
end;  // TEnquiriesViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0 : Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TEnquiriesViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TEnquiriesViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TEnquiriesViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetImageIndex: Integer;
begin
  Result := 5;
end;  // TEnquiriesViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetName: String;
begin
  Result := ResStr_Enquiries;
end;  // TEnquiriesViewType.GetName 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetNodeContext: TNodeContext;
begin
  Result := ncEnquiry;
end;  // TEnquiriesViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetNodeContextText: String;
begin
  Result := 'ncEnquiry';
end;  // TEnquiriesViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Enquiries_Select_ForTopLevel';
end;  // TEnquiriesViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_Enquirer;
    1:  Result := ResStr_EnquiryAnsweredBy;
    2:  Result := ResStr_EnquiryDate;
    3:  Result := ResStr_Department;
    4:  Result := ResStr_EnquiryAnswered;
    5:  Result := ResStr_EnquiryNotAnswered;
    6:  Result := ResStr_PlanToCreateAnObservation;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TEnquiriesViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    0, 1:  Result := ctIndividual;
  else
    Result := ctNormal;
  end;
end;  // TEnquiriesViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetSearchCount: Integer;
begin
  Result := 7;
end;  // TEnquiriesViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Enquiries_Select_ForSearchByEnquirer';
    1:  Result := 'usp_Enquiries_Select_ForSearchByAnsweredBy';
    2:  Result := 'usp_Enquiries_Select_ForSearchByDate';
    3:  Result := 'usp_Enquiries_Select_ForSearchByDepartment';
    4:  Result := 'usp_Enquiries_Select_ForSearchByAnswered';
    5:  Result := 'usp_Enquiries_Select_ForSearchByNotAnswered';
    6:  Result := 'usp_Enquiries_Select_ForSearchByPlanToCreateAnObservation';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TEnquiriesViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetSearchTextRequired(Index: Integer): Boolean;
begin
  Result := (Index in [0, 1, 2, 3]);
end;  // TEnquiriesViewType.GetSearchTextRequired 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TEnquiryTopLevelNode;
end;  // TEnquiriesViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TEnquiriesViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 2 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TEnquiriesViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TEnquiriesViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Type;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TEnquiriesViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TEnquiriesViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TEnquiriesViewType.InternalGetSortOrderCount 

{-==============================================================================
    TConditionChecksViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TConditionChecksViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Condition Check node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TConditionCheckTopLevelNode));
  TConditionCheckTopLevelNode(Result).InitialiseNewNode;
end;  // TConditionChecksViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_Add;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TConditionChecksViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TConditionChecksViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True
end;  // TConditionChecksViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetImageIndex: Integer;
begin
  Result := 25;
end;  // TConditionChecksViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetName: String;
begin
  Result := ResStr_ConditionChecks;
end;  // TConditionChecksViewType.GetName 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetNodeContext: TNodeContext;
begin
  Result := ncConditionCheck;
end;  // TConditionChecksViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetNodeContextText: String;
begin
  Result := 'ncConditionCheck';
end;  // TConditionChecksViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_ConditionChecks_Select_ForTopLevel';
end;  // TConditionChecksViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_CheckedBy;
    1:  Result := ResStr_CheckedDate;
    2:  Result := ResStr_Condition;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TConditionChecksViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    0:  Result := ctIndividual;
    2:  Result := ctConditionCheckConditionCombo;
  else
    Result := ctNormal;
  end;
end;  // TConditionChecksViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetSearchCount: Integer;
begin
  Result := 3;
end;  // TConditionChecksViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_ConditionChecks_Select_ForSearchByCheckedBy';
    1:  Result := 'usp_ConditionChecks_Select_ForSearchByCheckedDate';
    2:  Result := 'usp_ConditionChecks_Select_ForSearchByCondition';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TConditionChecksViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TConditionCheckTopLevelNode;
end;  // TConditionChecksViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TConditionChecksViewType.GetTransformSearchText(ASearchCaptionIndex: Integer;
    ASearchText: string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 1 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TConditionChecksViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TConditionChecksViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_ReferenceNumber;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TConditionChecksViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TConditionChecksViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TConditionChecksViewType.InternalGetSortOrderCount 

{-==============================================================================
    TCollectionsViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TCollectionsViewType.Create(AViewTypeManager: TViewTypeManager);
var
  lParams: TVariantArray;
  lRecordset: _Recordset;
begin

  FMetadata := TStringList.Create;
  SetLength(lParams, 2);
  lParams[High(lParams) -1] := '@TableName';
  lParams[High(lParams)]    := 'Collection';
  lRecordset := dmGeneral.GetRecordset('usp_MetadataAndType_Select', lParams);
  if lRecordset.RecordCount > 0 then begin
    lRecordset.MoveFirst;
    while not lRecordset.EOF do begin
      FMetadata.Add(lRecordset.Fields['Item_Name'].Value);
      lRecordset.MoveNext;
    end;
  end;
  inherited Create(AViewTypeManager);
end;  // TCollectionsViewType.Create

{-------------------------------------------------------------------------------
}
destructor TCollectionsViewType.Destroy;
begin
  FMetadata.Free;

  inherited Destroy;
end;  // TCollectionsViewType.Destroy

{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TCollectionsViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Collections node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TCollectionTopLevelNode));
  TCollectionTopLevelNode(Result).InitialiseNewNode;
end;  // TCollectionsViewType.AddNode

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  if Index = 0 then
    Result := ResStr_Add
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
end;  // TCollectionsViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 1;
end;  // TCollectionsViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TCollectionsViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetImageIndex: Integer;
begin
  Result := 2;
end;  // TCollectionsViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetName: String;
begin
  Result := ResStr_Collections;
end;  // TCollectionsViewType.GetName 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetNodeContext: TNodeContext;
begin
  Result := ncCollection;
end;  // TCollectionsViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetNodeContextText: String;
begin
  Result := 'ncCollection';
end;  // TCollectionsViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Collections_Select_ForTopLevel';
end;  // TCollectionsViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AccessionNumber;
    1:  Result := ResStr_AssembledBy;
    2:  Result := ResStr_CollectionName;
    3:  Result := ResStr_CollectionTopic;
  else
    if (Index <= 3 + FMetadata.Count) then
      Result := FMetadata[Index-4]
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TCollectionsViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    1:  Result := ctName;
  else
    Result := ctNormal;
  end;
end;  // TCollectionsViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetSearchCount: Integer;
begin
  Result := 4 + FMetadata.Count;
end;  // TCollectionsViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Collections_Select_ForSearchByAccessionNumber';
    1:  Result := 'usp_Collections_Select_ForSearchByAssembledBy';
    2:  Result := 'usp_Collections_Select_ForSearchByName';
    3:  Result := 'usp_Collections_Select_ForSearchByTopic';
  else
    if (Index <= 3 + FMetadata.Count) then
      Result := 'usp_Collections_Select_ForSearchByMetadata'
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TCollectionsViewType.GetSearchStoredProcName

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;

  if (SearchDefaultIndex <= 3 + FMetadata.Count) and (SearchDefaultIndex > 3) then begin
    SetLength(Result, Length(Result) + 2);
    Result[High(Result) -1] := '@MetaDataType';
    Result[High(Result)] := FMetadata[SearchDefaultIndex-4];
  end;
end;  // TCollectionsViewType.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TCollectionsViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TCollectionTopLevelNode;
end;  // TCollectionsViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TCollectionsViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_AssembledDate;
    1:  Result := ResStr_CollectionName;
    2:  Result := ResStr_AccessionNumber;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TCollectionsViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TCollectionsViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 3;
end;  // TCollectionsViewType.InternalGetSortOrderCount 

{-==============================================================================
    TAccessionsViewType
===============================================================================}
{-------------------------------------------------------------------------------
  Adds a node to the treeview of the required type.
}
function TAccessionsViewType.AddNode(ATree: TRapidTree; AMenuIndex: integer): TFlyNode;
begin
  // Add new top level Accession / Exchange node
  Result := TBrowserNode(ATree.Items.AddTypedChild(nil, TAccessionTopLevelNode));
  // Set the correct node type
  case AMenuIndex of
    0 : TAccessionTopLevelNode(Result).MovementType := 0;
    1 : begin
          TAccessionTopLevelNode(Result).MovementType := 0;
          TAccessionTopLevelNode(Result).WithAcquisition := true;
          TLeafNode(TBrowserNode(ATree.Items.AddTypedChild(Atree.CurrentTopLevelNode,
              TAcquisitionDetailsLeafNode))).InitialiseNewNode;
        end;
    2 : TAccessionTopLevelNode(Result).MovementType := 1;
  end;
  TAccessionTopLevelNode(Result).InitialiseNewNode;
end;  // TAccessionsViewType.AddNode 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetAddButtonMenuCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AddAccession;
    1:  Result := ResStr_AddAccessionAndAcquisition;
    2:  Result := ResStr_AddExchange;
  else
    raise EAddMenuItemError.Create(ResStr_InvalidAddMenuIndexRequest);
  end;
end;  // TAccessionsViewType.GetAddButtonMenuCaption 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetAddButtonMenuCaptionsCount: Integer;
begin
  Result := 3;
end;  // TAccessionsViewType.GetAddButtonMenuCaptionsCount 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetAddMenuIsAdd(AMenuIndex: Integer): Boolean;
begin
  Result := True;
end;  // TAccessionsViewType.GetAddMenuIsAdd 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetCanAdd: Boolean;
begin
  Result := AppSettings.AllowMovementEdit and inherited GetCanAdd;
end;  // TAccessionsViewType.GetCanAdd 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetImageIndex: Integer;
begin
  Result := 8;
end;  // TAccessionsViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetName: String;
begin
  Result := ResStr_Accessions;
end;  // TAccessionsViewType.GetName 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetNodeContext: TNodeContext;
begin
  Result := ncAccession;
end;  // TAccessionsViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetNodeContextText: String;
begin
  Result := 'ncAccession';
end;  // TAccessionsViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_Movements_Select_ForTopLevel';
end;  // TAccessionsViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_AccessionedBy;
    1:  Result := ResStr_AccessionNumber;
    2:  Result := ResStr_AccessionedDate;
    3:  Result := ResStr_SubjectArea;
    4:  Result := ResStr_AccessionedFrom;
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TAccessionsViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
  Return the control type that should be displayed for each of the searches.
}
function TAccessionsViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    0:  Result := ctIndividual;
    4:  Result := ctName;
  else
    Result := ctNormal;
  end;
end;  // TAccessionsViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetSearchCount: Integer;
begin
  Result := 5;
end;  // TAccessionsViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_Movements_Select_ForSearchByAccessionedBy';
    1:  Result := 'usp_Movements_Select_ForSearchByAccessionNumber';
    2:  Result := 'usp_Movements_Select_ForSearchByAccessionedDate';
    3:  Result := 'usp_Movements_Select_ForSearchBySubjectArea';
    4:  Result := 'usp_Movements_Select_ForSearchByAccessionedFrom';
  else
    raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TAccessionsViewType.GetSearchStoredProcName 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetSortStoredProcIndex(Index: Integer): Integer;
begin
  case Index of
    0:  Result := 0;
    1:  Result := 2;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TAccessionsViewType.GetSortStoredProcIndex 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;

  SetLength(Result, Length(Result) + 2);
  Result[High(Result) -1] := '@MovementGroupType';
  Result[High(Result)] := 0;
end;  // TAccessionsViewType.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TAccessionTopLevelNode;
end;  // TAccessionsViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TAccessionsViewType.GetTransformSearchText(ASearchCaptionIndex: Integer; ASearchText:
    string): String;
var
  lVagueDate: TVagueDate;
begin
  if ASearchCaptionIndex = 2 then begin
    lVagueDate := StringToVagueDate(StringReplace(ASearchText, '*', '', [rfReplaceAll]));
    Result := IntToStr(Trunc(lVagueDate.StartDate)) + ':' +
              IntToStr(Trunc(lVagueDate.EndDate)) + ';' +
              lVagueDate.DateTypeString;
  end
  else
    Result := inherited GetTransformSearchText(ASearchCaptionIndex, ASearchText);
end;  // TAccessionsViewType.GetTransformSearchText 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCaption) to
      retrieve the display caption of a specified sort order.
}
function TAccessionsViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Date;
    1:  Result := ResStr_Type;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TAccessionsViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
  Method used by the base class (which implements ISortOrderProvider.SortOrderCount) to
      retrieve the number of available sort orders for this specific view type.
}
function TAccessionsViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 2;
end;  // TAccessionsViewType.InternalGetSortOrderCount 

{-==============================================================================
    TStoreHierarchyViewType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TStoreHierarchyViewType.Create(AViewTypeManager: TViewTypeManager);
var
  lParams: TVariantArray;
  lRecordset: _Recordset;
begin

  FMetadata := TStringList.Create;
  SetLength(lParams, 2);
  lParams[High(lParams) -1] := '@TableName';
  lParams[High(lParams)]    := 'Store';
  lRecordset := dmGeneral.GetRecordset('usp_MetadataAndType_Select', lParams);
  if lRecordset.RecordCount > 0 then begin
    lRecordset.MoveFirst;
    while not lRecordset.EOF do begin
      FMetadata.Add(lRecordset.Fields['Item_Name'].Value);
      lRecordset.MoveNext;
    end;
  end;
  inherited Create(AViewTypeManager);
end;  // TStoreHierarchyViewType.Create

{-------------------------------------------------------------------------------
}
destructor TStoreHierarchyViewType.Destroy;
begin
  FMetadata.Free;

  inherited Destroy;
end;  // TStoreHierarchyViewType.Destroy

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetImageIndex: Integer;
begin
  Result := 4;
end;  // TStoreHierarchyViewType.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetName: String;
begin
  Result := ResStr_StoreHierarchy;
end;  // TStoreHierarchyViewType.GetName 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetNodeContext: TNodeContext;
begin
  Result := ncStoreHierarchy;
end;  // TStoreHierarchyViewType.GetNodeContext 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetNodeContextText: String;
begin
  Result := 'ncStoreHierarchy';
end;  // TStoreHierarchyViewType.GetNodeContextText 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetPopulateTopLevelStoredProcName: String;
begin
  Result := 'usp_StoreHierarchy_Select_ForTopLevel';
end;  // TStoreHierarchyViewType.GetPopulateTopLevelStoredProcName 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetSearchCaption(Index: Integer): String;
begin
  case Index of
    0:  Result := ResStr_StoreName;
    1:  Result := ResStr_StoreReferenceNumber;
    2:  Result := ResStr_UsualLocationCode;
    3:  Result := ResStr_StoreType;
  else
    if (Index <= 3 + FMetadata.Count) then
      Result := FMetadata[Index-4]
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TStoreHierarchyViewType.GetSearchCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetSearchControlType(Index: integer): TSearchControlType;
begin
  case Index of
    3:  Result := ctStoreTypeCombo;
  else
    Result := ctNormal;
  end;
end;  // TStoreHierarchyViewType.GetSearchControlType 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetSearchCount: Integer;
begin
  Result := 4 + FMetadata.Count;
end;  // TStoreHierarchyViewType.GetSearchCount 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetSearchStoredProcName(Index: Integer): String;
begin
  case Index of
    0:  Result := 'usp_StoreHierarchy_Select_ForSearchByName';
    1:  Result := 'usp_StoreHierarchy_Select_ForSearchByPreferredNumber';
    2:  Result := 'usp_StoreHierarchy_Select_ForSearchByUsualLocationCode';
    3:  Result := 'usp_StoreHierarchy_Select_ForSearchByStoreType';
  else  
    if (Index <= 3 + FMetadata.Count) then
      Result := 'usp_StoreHierarchy_Select_ForSearchByMetadata'
    else
      raise EInvalidSearchIndexException.Create(ResStr_InvalidSearchIndex);
  end;
end;  // TStoreHierarchyViewType.GetSearchStoredProcName  

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetStoredProcParams: TVariantArray;
begin
  Result := inherited GetStoredProcParams;

  if (SearchDefaultIndex <= 3 + FMetadata.Count) and (SearchDefaultIndex > 3) then begin
    SetLength(Result, Length(Result) + 2);
    Result[High(Result) -1] := '@MetaDataType';
    Result[High(Result)] := FMetadata[SearchDefaultIndex-4];
  end;
end;  // TStoreHierarchyViewType.GetStoredProcParams

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.GetTopLevelNodeClass: TTopLevelNodeClass;
begin
  Result := TStoreHierarchyTopLevelNode;
end;  // TStoreHierarchyViewType.GetTopLevelNodeClass 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.InternalGetSortOrderCaption(AIndex: integer): Widestring;
begin
  case AIndex of
    0:  Result := ResStr_Name;
    1:  Result := ResStr_RegistrationNumber;
    2:  Result := ResStr_CurrentLocationCode;
  else
    raise EInvalidSortOrderIndexException.Create(ResStr_InvalidSortOrderIndex);
  end;
end;  // TStoreHierarchyViewType.InternalGetSortOrderCaption 

{-------------------------------------------------------------------------------
}
function TStoreHierarchyViewType.InternalGetSortOrderCount: Integer;
begin
  Result := 3;
end;  // TStoreHierarchyViewType.InternalGetSortOrderCount 



end.
