{===============================================================================
  Unit:        MenuImages

  Defines:     TdmMenuImages

  Description: Declares an image list for the menu items

  Created:     Oct 2003

  Model:       CollectionsModuleManager.mpb

  Last revision information:
    $Revision: 2 $
    $Date: 3/11/03 15:47 $
    $Author: Johnvanbreda $

===============================================================================}
unit MenuImages;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TdmMenuImages = class (TDataModule)
    ilImages: TImageList;
  end;
  
implementation

{$R *.dfm}

end.
