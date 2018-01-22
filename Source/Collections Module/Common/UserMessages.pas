{===============================================================================
  Unit:        UserMessages

  Defines:     <nothing>

  Description: Contains user-defined messages and any message records to go
               with them.

  Created:     August 2003

  Last revision information:
    $Revision: 19 $
    $Date: 4/10/12 13:36 $
    $Author: Alexanderpadley $

===============================================================================}

unit UserMessages;

interface

uses
  Messages;
  
const
  WM_SETUPDRAGDROP                         = WM_APP + 100;
  WM_DELETEOBJECT                          = WM_APP + 101;
  WM_GETCOMMONNAME                         = WM_APP + 102;
  WM_NAVIGATE                              = WM_APP + 103;
  WM_ADD_SPECIMEN                          = WM_APP + 104;
  WM_ADD_DETERMINATION_EARTH_SCIENCE       = WM_APP + 105;
  WM_ADD_DETERMINATION_LIFE_SCIENCE        = WM_APP + 106;
  WM_REFRESH_SPECIMEN_CAPTION              = WM_APP + 107;
  WM_REFRESH_SCREEN_INFO                   = WM_APP + 108;
  WM_REMOVE_SPECIMEN_WITHOUT_DETERMINATION = WM_APP + 108;
  WM_REMOVE_DELETED_NODE                   = WM_APP + 109;
  WM_VALIDATEROW                           = WM_APP + 110;
  WM_RELOAD_ONE_NODE_ONLY                  = WM_APP + 111;
  WM_NAVIGATE_SYNONYM                      = WM_APP + 112;
  WM_NAVIGATE_SPECIMEN                     = WM_APP + 113;
  WM_REMOVE_DRAGGED_NODE                   = WM_APP + 114;
  WM_NAVIGATE_STORE                        = WM_APP + 115;  
  WM_REFRESH_NODE                          = WM_APP + 116;
  WM_NAVIGATE_MOVEMENT                     = WM_APP + 117;
  WM_NAVIGATE_ACCESSION                    = WM_APP + 118;
  WM_NAVIGATE_LOAN                         = WM_APP + 119;
  WM_NAVIGATE_CONDITION_CHECK              = WM_APP + 120;
  WM_NAVIGATE_JOB                          = WM_APP + 121;
  WM_NAVIGATE_FOLDER                       = WM_APP + 122;

type
  TWMDeleteObject = packed record
    Msg: Cardinal;
    ObjectRef: TObject;
    Additional: LongInt;
    Result: LongInt;
  end;

//==============================================================================
implementation

end.

