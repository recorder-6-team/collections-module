{===============================================================================
  Unit:        About

  Defines:     TdlgAboutThesaurusEditor

  Description: About box

  Created:     18/7/2003

  Last revision information:
    $Revision: 2 $
    $Date: 10/12/03 9:03 $
    $Author: Johnvanbreda $

===============================================================================}
unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  {-----------------------------------------------------------------------------
    About box screen for the Thesaurus Editor.  Displays the application 
    version information.
  }
  TdlgAboutThesaurusEditor = class (TForm)
    Bevel1: TBevel;
    btnClose: TButton;
    Label1: TLabel;
    lblVersion: TLabel;
  public
    constructor Create(AOwner : TComponent); override;
  end;
  
implementation

{$R *.dfm}

uses
  VersionInfo;
  
{-==============================================================================
    TdlgAboutThesaurusEditor
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgAboutThesaurusEditor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  
  lblVersion.Caption := lblVersion.Caption + ' ' + GetFileVersion;
  // Centre the label
  lblVersion.Left := (Width - lblVersion.Width) div 2;
end;  // TdlgAboutThesaurusEditor.Create 


end.

