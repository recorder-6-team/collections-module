{===============================================================================
  Unit:        ReportSeparator

  Defines:     TReportSeparator

  Description: Class that implements a single IDynamicMenu instance for the
               separator that appears in the Report menu.

  Created:     Oct 2003

  Last revision information:
    $Revision: 2 $
    $Date: 3/11/03 15:47 $
    $Author: Johnvanbreda $

===============================================================================}
unit ReportSeparator;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, CollectionsModuleManager_TLB, StdVcl, Recorder2000_TLB;

type
  {-----------------------------------------------------------------------------
    IDymamicMenu implementation for the separator in the Reports menu.
  }
  TReportSeparator = class (TAutoObject, IReportSeparator, IDynamicMenu)
  private
    function Child(Index: Integer): IUnknown; safecall;
    function Execute: IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_ChildCount: Integer; safecall;
    function Get_HasSubmenu: WordBool; safecall;
  end;
  
implementation

uses ComServ;

{-==============================================================================
    TReportSeparator
===============================================================================}
{-------------------------------------------------------------------------------
  Menu item has no children. 
}
function TReportSeparator.Child(Index: Integer): IUnknown;
begin
  Result := nil;
end;  // TReportSeparator.Child 

{-------------------------------------------------------------------------------
}
function TReportSeparator.Execute: IUnknown;
begin
  Result := nil;
end;  // TReportSeparator.Execute 

{-------------------------------------------------------------------------------
}
procedure TReportSeparator.GetItemData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption := '-';
    Enabled := True;
    Hint := '';
    ImageIndex := -1;
  end;
end;  // TReportSeparator.GetItemData 

{-------------------------------------------------------------------------------
  Menu item has no children. 
}
function TReportSeparator.Get_ChildCount: Integer;
begin
  Result := 0;
end;  // TReportSeparator.Get_ChildCount 

{-------------------------------------------------------------------------------
  Menu item has no children. 
}
function TReportSeparator.Get_HasSubmenu: WordBool;
begin
  Result := False;
end;  // TReportSeparator.Get_HasSubmenu 


initialization
  TAutoObjectFactory.Create(ComServer, TReportSeparator, Class_ReportSeparator,
    ciMultiInstance, tmApartment);
end.
