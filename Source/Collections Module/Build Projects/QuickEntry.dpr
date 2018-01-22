{===============================================================================
  Library:     QuickEntry.ocx

  Description:

  Created:     2003
===============================================================================}

library QuickEntry;

uses
  madExcept,
  madLinkDisAsm,
  ComServ,
  Forms,
  ExceptionForm,
  Windows,
  ADODB_TLB                    in '..\..\Other Dependencies\DssVcl32\ADODB_TLB.pas',
  BaseADODataModule            in '..\..\Other Dependencies\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},

  XPMenu                       in '..\..\Other Dependencies\Third-Party\XPMenu\XPMenu.pas',

  Recorder2000_TLB             in '..\..\Other Dependencies\JNCC\Recorder2000_TLB.pas',
  Constants                    in '..\..\Other Dependencies\JNCC\Constants.pas',

  DataClasses                  in '..\..\Other Dependencies\JNCC\Components\\DataClasses.pas',
  DropSource                   in '..\..\Other Dependencies\JNCC\Components\\DropSource.pas',
  DropStruct                   in '..\..\Other Dependencies\JNCC\Components\\DropStruct.pas',
  DropTarget                   in '..\..\Other Dependencies\JNCC\Components\\DropTarget.pas',
  GenFuncs                     in '..\..\Other Dependencies\JNCC\Components\\GenFuncs.pas',
  JNCCDatasets                 in '..\..\Other Dependencies\JNCC\Components\\JNCCDatasets.pas',
  JNCCGrid                     in '..\..\Other Dependencies\JNCC\Components\\JNCCGrid.pas',
  SpatialRefFuncs              in '..\..\Other Dependencies\JNCC\Components\\SpatialRefFuncs.pas',
  SQLConverter                 in '..\..\Other Dependencies\JNCC\Components\\SQLConverter.pas',
  VagueDate                    in '..\..\Other Dependencies\JNCC\Components\\VagueDate.pas',
  VagueDateEdit                in '..\..\Other Dependencies\JNCC\Components\\VagueDateEdit.pas',

  BaseCompositeComponent       in '..\..\Other Dependencies\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes                 in '..\..\Other Dependencies\Recorder Addins\Components\DSSDataTypes.pas',
  FixedWidthColumnSelector     in '..\..\Other Dependencies\Recorder Addins\Components\FixedWidthColumnSelector.pas',
  LinkedControls               in '..\..\Other Dependencies\Recorder Addins\Components\LinkedControls.pas',

  CollectionsModuleManager_TLB in 'CollectionsModuleManager_TLB.pas',
  ThesaurusBrowser_TLB         in 'ThesaurusBrowser_TLB.pas',

  ApplicationSettings          in '..\Common\ApplicationSettings.pas',
  BaseDetailFrameUnit          in '..\Common\BaseDetailFrameUnit.pas' {BaseDetailFrame: TFrame},
  BaseDragFrameUnit            in '..\Common\BaseDragFrameUnit.pas' {BaseDragFrame: TFrame},
  BaseTabSheetFrameUnit        in '..\Common\BaseTabSheetFrameUnit.pas' {BaseTabSheetFrame: TFrame},
  ComUnit                      in '..\Common\ComUnit.pas',
  DataTypes                    in '..\Common\DataTypes.pas',
  Find                         in '..\Common\Find.pas' {dlgFind: TForm},
  FrameMeasurementsGeneral     in '..\Common\FrameMeasurementsGeneral.pas' {fraMeasurementsGeneral: TFrame},
  GeneralData                  in '..\Common\GeneralData.pas' {dmGeneral: TDataModule},
  InterfaceDataModule          in '..\Common\InterfaceDataModule.pas' {dmInterface: TDataModule},
  LuxembourgConstants          in '..\Common\LuxembourgConstants.pas',
  LuxembourgDataClasses        in '..\Common\LuxembourgDataClasses.pas',
  LuxembourgFunctions          in '..\Common\LuxembourgFunctions.pas',
  ResourceStrings              in '..\Common\ResourceStrings.pas',
  RegisteredControls           in '..\Common\RegisteredControls.pas',
  SearchManager                in '..\Common\SearchManager.pas',
  UserMessages                 in '..\Common\UserMessages.pas',
  Validation                   in '..\Common\Validation.pas',

  ConceptGroupComboBox         in '..\Components\ConceptGroupComboBox.pas',
  LuxIDComboBox                in '..\Components\LuxIDComboBox.pas',
  MemCheck                     in '..\Components\MemCheck.pas',

  QuickEntry_TLB               in 'QuickEntry_TLB.pas',
  QuickEntryDataControls       in '..\Quick Entry\Source\QuickEntryDataControls.pas',
  QuickEntryForm               in '..\Quick Entry\Source\QuickEntryForm.pas' {QuickEntryForm: TActiveForm},
  QuickEntryMeasurements       in '..\Quick Entry\Source\QuickEntryMeasurements.pas' {frmQuickEntryMeasurements: TForm},
  QEAddNumber                  in '..\Quick Entry\Source\QEAddNumber.pas' {frmQuickEntryNumber},
  QEAddMetadata                in '..\Quick Entry\Source\QEAddMetadata.pas' {frmQEAddMetadata},
  QEMultiValuePopup            in '..\Quick Entry\Source\QEMultiValuePopup.pas' {frmQEMultiValuePopup},
  QuickEntryManagerForm        in '..\Quick Entry\Source\QuickEntryManagerForm.pas' {frmQuickEntryManager: TActiveForm} {TfrmQuickEntryManager: CoClass},
  QuickEntryManagerFrame       in '..\Quick Entry\Source\QuickEntryManagerFrame.pas' {fraQuickEntryManager: TFrame},
  QuickEntryFrame              in '..\Quick Entry\Source\QuickEntryFrame.pas' {fraQuickEntry: TFrame},
  QuickEntryData               in '..\Quick Entry\Source\QuickEntryData.pas' {dmQuickEntry: TDataModule},
  DataStringGrid               in '..\Quick Entry\Source\DataStringGrid.pas',
  QuickEntryInsertTables       in '..\Quick Entry\Source\QuickEntryInsertTables.pas',
  QEImportProgress             in '..\Quick Entry\Source\QEImportProgress.pas' {fraQEImportProgress: TFrame},
  QEImport                     in '..\Quick Entry\Source\QEImport.pas' {dlgQEImport},
  QEImportColumnMatching       in '..\Quick Entry\Source\QEImportColumnMatching.pas' {fraQEImportColumnMatching: TFrame},
  QEImportColWidthsSelect      in '..\Quick Entry\Source\QEImportColWidthsSelect.pas' {fraQEImportColWidthsSelect: TFrame},
  QEImportDefinition           in '..\Quick Entry\Source\QEImportDefinition.pas',
  QEImportFileSelect           in '..\Quick Entry\Source\QEImportFileSelect.pas' {fraQEImportFileSelect: TFrame},
  QEImportFrameUnit            in '..\Quick Entry\Source\QEImportFrameUnit.pas' {QEImportFrame: TFrame},
  QENumberOfSpecimens          in '..\Quick Entry\Source\QENumberOfSpecimens.pas' {dlgQENumberOfSpecimens};

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

var
  RecorderHandle: HWND;
  RecorderProcessId: DWORD;

begin
  RecorderHandle := FindWindow('TApplication', 'Recorder 6');
  if RecorderHandle <> 0 then
  begin
    // Recorder is running
    if GetWindowThreadProcessId(RecorderHandle, RecorderProcessId) <> 0 then
    begin
      if RecorderProcessId = GetCurrentProcessId then
      begin
        // We are running within Recorder
        Application.Handle := FindWindow('TApplication', 'Recorder 6');
        Application.CreateForm(TdmInterface, dmInterface);
        Application.CreateForm(TdmQuickEntry, dmQuickEntry);
        {$IFDEF madExcept}
        RegisterExceptionHandler(TfrmException.MadExceptionHandler, stTrySyncCallAlways);
        {$ELSE}
        Application.OnException := TfrmException.GlobalExceptionHandler;
        {$ENDIF}
        // Set Decimal Separator to '.' for latlong support
        //DecimalSeparator := '.';
        MstErrorMessage := 'An unhandled error has occurred in Recorder 2002.  Don''t ' +
                        'worry.  Send this error message to recorder@jncc.gov.uk.';
        NonCriticalExceptions.Add('EPrinter');
      end;
    end;
  end;
end.
