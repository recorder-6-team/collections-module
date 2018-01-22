{===============================================================================
  Unit:        NumberMacroOptionsFrame

  Defines:     TfraGeneralOptionsFrame

  Description:

  Created:     June 2008

  Last revision information:
    $Revision: 7 $
    $Date: 15/09/10 10:26 $
    $Author: Robertjohnson $

===============================================================================}

unit GeneralOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, FolderBrowser;

type
  TfraGeneralOptions = class(TFrame)
    eStandardReportTemplatePath: TEdit;
    eSpecimentImagePath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btnGetStandardReportTemplatePath: TButton;
    btnGetSpecimentImagePath: TButton;
    fbGetPath: TFolderBrowser;
    chkPreferredSynonyms: TCheckBox;
    chkUseOriginalNames: TCheckBox;
    chkGroupByDomain: TCheckBox;
    chkShowGroupInQuickEntry: TCheckBox;
    chkShowRecorderSpecimensTab: TCheckBox;
    procedure btnGetStandardReportTemplatePathClick(Sender: TObject);
    procedure btnGetSpecimentImagePathClick(Sender: TObject);
  private
    function SelectFolder(const AFolder, AMsg: String): String;
  public
    procedure Load;
    procedure Save;
  end;

implementation

uses
  ApplicationSettings;

resourcestring
  ResStr_SelectFolder           = 'Select the source folder for %s:';
  ResStr_StandardReportTemplate = 'standard report templates';
  ResStr_SpecimenImage          = 'specimen images';

{$R *.dfm}

{-------------------------------------------------------------------------------
}
procedure TfraGeneralOptions.btnGetSpecimentImagePathClick(Sender: TObject);
begin
  eSpecimentImagePath.Text := SelectFolder(
      eSpecimentImagePath.Text, ResStr_StandardReportTemplate);
end;    // TfraGeneralOptionsFrame.btnGetSpecimentImagePathClick

{-------------------------------------------------------------------------------
}
procedure TfraGeneralOptions.btnGetStandardReportTemplatePathClick(Sender: TObject);
begin
  eStandardReportTemplatePath.Text := SelectFolder(
      eStandardReportTemplatePath.Text, ResStr_StandardReportTemplate);
end;    // TfraGeneralOptionsFrame.btnGetStandardReportTemplatePathClick

{-------------------------------------------------------------------------------
  Load the currently selected folders
}
procedure TfraGeneralOptions.Load;
begin
  eStandardReportTemplatePath.Text    := AppSettings.StandardReportTemplatePath;
  eSpecimentImagePath.Text            := AppSettings.SpecimenImagePath;
  chkPreferredSynonyms.Checked        := AppSettings.PreferredSynonymsOnly;
  chkUseOriginalNames.Checked         := AppSettings.UseOriginalSpecimenNames;
  chkGroupByDomain.Checked            := AppSettings.GroupDeterminationsByDomain;
  chkShowGroupInQuickEntry.Checked    := AppSettings.ShowGroupInQuickEntry;
  chkShowRecorderSpecimensTab.Checked := AppSettings.ShowRecorderSpecimensTab;
end;

{-------------------------------------------------------------------------------
  Save the selected folders
}
procedure TfraGeneralOptions.Save;
begin
  AppSettings.StandardReportTemplatePath  := eStandardReportTemplatePath.Text;
  AppSettings.SpecimenImagePath           := eSpecimentImagePath.Text;
  AppSettings.PreferredSynonymsOnly       := chkPreferredSynonyms.Checked;
  AppSettings.UseOriginalSpecimenNames    := chkUseOriginalNames.Checked;
  AppSettings.GroupDeterminationsByDomain := chkGroupByDomain.Checked;
  AppSettings.ShowGroupInQuickEntry       := chkShowGroupInQuickEntry.Checked;
  AppSettings.ShowRecorderSpecimensTab    := chkShowRecorderSpecimensTab.Checked;
end;

{-------------------------------------------------------------------------------
  Select a folder using the folder browser dialog
}
function TfraGeneralOptions.SelectFolder(const AFolder, AMsg: String): String;
begin
  Result := AFolder;
  fbGetPath.Title  := Format(ResStr_SelectFolder, [AMsg]);
  fbGetPath.Folder := ExcludeTrailingPathDelimiter(AFolder);
  if fbGetPath.Execute then
    Result := IncludeTrailingPathDelimiter(fbGetPath.Folder);
end;    // TfraGeneralOptionsFrame.SelectFolder

end.
