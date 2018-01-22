{===============================================================================
  Unit:           RegisteredControls

  Defines:       TRegisteredContro;
                 TRegisteredVagueDateControl
                 TRegisteredLinkedControl
                 TREgisteredAsyncControl

  Description:   Classes that encapsulate the attributes required for a
                 registered control.

  Created:       Sept 2003

  Model:         CollectionBrowserFramework.mpb

  Last revision information:
    $Revision: 6 $
    $Date: 16/01/04 13:53 $
    $Author: Johnvanbreda $

===============================================================================}
unit RegisteredControls;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ADODb, VagueDateEdit, DataTypes, Validation, GeneralData, 
  BaseADODataModule;

type
  {-----------------------------------------------------------------------------
    Function prototype for setting up parameters before opening a recordset. A default 
    function is already available but is very basic.
  }
  TGetParamsFunction = function: TVariantArray of object;
  TGetStoredProcNameFunction = function: String of object;
  {-----------------------------------------------------------------------------
    Helper class. Base class for automatically populating controls with data.  Control holds a 
    reference (pointer) to the control to populate.  DataFieldName is the name of the field in 
    the recordset from which to get the data from.  RecordsetIndex is the index of the 
    recordset to use for this control, if there are more than one registered recordset 
    available.
  }
  TRegisteredControl = class (TObject)
  private
    FControl: TControl;
    FDataFieldName: String;
    FDisplayFieldName: String;
    FMandatory: Boolean;
    FRecordsetIndex: Integer;
  public
    property Control: TControl read FControl write FControl;
    property DataFieldName: String read FDataFieldName write FDataFieldName;
    property DisplayFieldName: String read FDisplayFieldName write FDisplayFieldName;
    property Mandatory: Boolean read FMandatory write FMandatory;
    property RecordsetIndex: Integer read FRecordsetIndex write FRecordsetIndex;
  end;
  
  {-----------------------------------------------------------------------------
    Class handling VagueDateEdit controls.  This class is used to help handling the vague date 
    data automatically.
  }
  TRegisteredVagueDateControl = class (TRegisteredControl)
  private
    function GetControl: TVagueDateEdit;
    procedure SetControl(const Value: TVagueDateEdit);
  public
    property Control: TVagueDateEdit read GetControl write SetControl;
  end;
  
  {-----------------------------------------------------------------------------
    Class handling IDComboBox and LinkedEdit controls.  CheckLinkedData is a method pointer 
    pointing to a validation method used whenever the OnFindData event is triggered on a 
    LinkedEdit control.  The event is automatically linked when the control is registered.  
    KeyFieldName is the name of the field to use for the Key property of a LinkedEdit control, 
    or when adding items to an IDComboBox.
  }
  TRegisteredLinkedControl = class (TRegisteredControl)
  private
    FCheckLinkedData: TCheckLinkedDataFunction;
    FConvertKeyToCaption: TConvertKeyToCaptionFunction;
    FDataType: String;
    FKeyFieldName: String;
  public
    property CheckLinkedData: TCheckLinkedDataFunction read FCheckLinkedData write 
        FCheckLinkedData;
    property ConvertKeyToCaption: TConvertKeyToCaptionFunction read FConvertKeyToCaption write 
        FConvertKeyToCaption;
    property DataType: String read FDataType write FDataType;
    property KeyFieldName: String read FKeyFieldName write FKeyFieldName;
  end;
  
  {-----------------------------------------------------------------------------
    Class handling asynchronously loaded data.  CallbackProc is the function called when the 
    async stored proc finishes.  OutputParam is the name of the parameter to read the data 
    from.  Params is a variant array of parameter names and values.  StoredProcName is the 
    name of the stored proc to run. This stored proc should only return one value in an output 
    only parameter.
  }
  TRegisteredAsyncControl = class (TRegisteredControl)
  private
    FCallbackProc: TAsyncExecuteCompleteProc;
    FOutputParam: String;
    FParams: TVariantArray;
    FStoredProcName: String;
    procedure SetParams(Value: TVariantArray);
  public
    constructor Create;
    procedure SetParamCount(ACount: integer);
    property CallbackProc: TAsyncExecuteCompleteProc read FCallbackProc write FCallbackProc;
    property OutputParam: String read FOutputParam write FOutputParam;
    property Params: TVariantArray read FParams write SetParams;
    property StoredProcName: String read FStoredProcName write FStoredProcName;
  end;
  

implementation

{-==============================================================================
    TRegisteredVagueDateControl
===============================================================================}
{-------------------------------------------------------------------------------
}
function TRegisteredVagueDateControl.GetControl: TVagueDateEdit;
begin
  Result := TVagueDateEdit(inherited Control);
end;  // TRegisteredVagueDateControl.GetControl 

{-------------------------------------------------------------------------------
}
procedure TRegisteredVagueDateControl.SetControl(const Value: TVagueDateEdit);
begin
  inherited Control := Value;
end;  // TRegisteredVagueDateControl.SetControl 

{-==============================================================================
    TRegisteredAsyncControl
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TRegisteredAsyncControl.Create;
begin
  inherited Create;
  DataFieldName := 'Item_Name';
end;  // TRegisteredAsyncControl.Create 

{-------------------------------------------------------------------------------
  Sets the size of the params variant array.
}
procedure TRegisteredAsyncControl.SetParamCount(ACount: integer);
begin
  SetLength(FParams, ACount);
end;  // TRegisteredAsyncControl.SetParamCount 

{-------------------------------------------------------------------------------
}
procedure TRegisteredAsyncControl.SetParams(Value: TVariantArray);
begin
  FParams := Value;
end;  // TRegisteredAsyncControl.SetParams 


end.
