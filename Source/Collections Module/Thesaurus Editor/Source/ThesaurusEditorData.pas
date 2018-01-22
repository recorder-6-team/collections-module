{===============================================================================
  Unit:            ThesaurusEditorData

  Defines:         TdmMain

  Description:     Image lists for the Thesaurus Editor

  Created:         Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/02/04 13:14 $
    $Author: Johnvanbreda $

===============================================================================}

unit ThesaurusEditorData;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TdmMain = class (TDataModule)
    ilHierarchy: TImageList;
  end;
  
var
  dmMain: TdmMain;

//==============================================================================
implementation

{$R *.dfm}

end.





