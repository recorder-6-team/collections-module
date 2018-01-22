{===============================================================================
  Unit:        FrameMultimedia.pas

  Defines:     TfraMultimedia

  Description:

  Model:       CollectionsCommonAndGeneral.mpb

  Created:     May 2003

  Last revision information:
    $Revision: 26 $
    $Date: 16/09/10 15:00 $
    $Author: Robertjohnson $

===============================================================================}

unit FrameMultimedia;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseFullScreenFrameUnit, ExtDlgs, StdCtrls, ImageListButton, JPeg,
  ExtCtrls, BaseDetailFrameUnit, GeneralData, GeneralFunctions,
  InterfaceDataModule, LuxembourgConstants, DataTypes, ResourceStrings,
  BaseTabSheetFrameUnit, EasyShell;

resourcestring
  ResStr_PleaseSpecifyValidFilename = 'The filename specified is invalid.  Please ' +
      'specify a valid filename.';
  ResStr_CannotDisplayImage = 'The system cannot display this file.';
  ResStr_FileNotFound = 'This file cannot be displayed because it no longer exists.'; 
  ResStr_MultipleMessagesCreated = 'This file contains multiple images. Only the first image will be displayed.';
  
type
  {-----------------------------------------------------------------------------
    Details of a selected item of multimedia.  Multimedia items use the existing Recorder 
    Source module to store data in the Source_File table.
  }
  TfraMultimedia = class (TBaseFullScreenFrame)
    btnGo: TImageListButton;
    btnOpen: TImageListButton;
    chkPreferred: TCheckBox;
    dlgOpenPic: TOpenPictureDialog;
    eFileName: TEdit;
    eName: TEdit;
    imgNoPreview: TImage;
    imgPicture: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblImageSize: TLabel;
    lblNoPreview: TLabel;
    rbBestFit: TRadioButton;
    rbFullSize: TRadioButton;
    scbImage: TScrollBox;
    procedure btnGoClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure rbBestFitClick(Sender: TObject);
    procedure rbFullSizeClick(Sender: TObject);
  private
    FCustodian: String;
    FFullSize: Boolean;
    FTopNodeContext: TNodeContext;
    FTimestamp: TSQLSvrTimestamp;
    function IsWebAddress: Boolean;
    procedure LoadPicture(const AFileName: String);
    procedure ResizeImage;
    function UseThumbnailIfAvailable(const AFileName: string): string;
    function ConvertedImageFilename(const AFileName: string): string;
  protected
    procedure DeleteData; override;
    procedure EnableControls(AEnabled: Boolean); override;
    function GetCaption: String; override;
    procedure LoadData; override;
    procedure RegisterControls; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveData; override;
    procedure ValidateData; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ExceptionForm, ActiveX, ComObj, LuxembourgFunctions, ApplicationSettings,
  StrUtils, XMLDom, XMLDoc, XMLIntf;

{-==============================================================================
    TfraMultimedia
===============================================================================}
{-------------------------------------------------------------------------------
  Handles a click on the go button by opening the image. 
}
procedure TfraMultimedia.btnGoClick(Sender: TObject);
begin
  inherited;
  if FileExists(eFileName.Text) then
  begin
    if not ShellFile(ConvertedImageFilename(eFileName.Text)) then
      MessageDlg(ResStr_CannotDisplayImage, mtInformation, [mbOK], 0);
  end else
    MessageDlg(ResStr_FileNotFound, mtInformation, [mbOK], 0);
end;  // TfraMultimedia.btnGoClick 

{-------------------------------------------------------------------------------
  Load the selected image into the preview picture.
}
procedure TfraMultimedia.btnOpenClick(Sender: TObject);
begin
  inherited;
  if dlgOpenPic.Execute then LoadPicture(dlgOpenPic.FileName);
end;  // TfraMultimedia.btnOpenClick

{-------------------------------------------------------------------------------
  If the supplied file is not a recognised image format, convert it to a bitmap
    in the users Windows temporary directory, using the ImageMagick library,
    if it is installed.
}
function TfraMultimedia.ConvertedImageFilename(const AFileName: string): string;
var
  lImageFileMagick: Variant;
  lTempImageName, lOutput: string;
  lCursor: TCursor;

  function MultipleImagesCreated: Boolean;
  var
    lCurrentFile, lFileWithoutExtension, lExtension: string;
  begin
    Result := False;
    lFileWithoutExtension := ChangeFileExt(lTempImageName, '');
    lExtension            := ExtractFileExt(lTempImageName);
    lCurrentFile          := lFileWithoutExtension + '-0' + lExtension;
    if FileExists(lCurrentFile) then
    begin
      lTempImageName := lCurrentFile;
      Result         := True;
    end;
  end;  // MultipleImagesCreated

begin
  lCursor := HourglassCursor;
  try
    Result := AFileName;
    if FormatSupportedByImageMagick(AFileName) then
    begin
      lTempImageName := IncludeTrailingPathDelimiter(GetWindowsTempDir)
          + FN_TEMP_FILE_PREFIX + ChangeFileExt(AFileName, '.bmp');
      try
        lImageFileMagick := CreateOleObject('ImageMagickObject.MagickImage.1');
      except
        on E: EOleSysError do
          // Assume that if instantiation fails, ImageMagick is not installed
          // Could test for specific error message, but could break in other language versions of Windows
          Exit;
      end;
      try
        lOutput := lImageFileMagick.Convert(AFileName, lTempImageName);
        // Need to check for creation of multiple files irrespective of errors
        if FileExists(lTempImageName) then
          Result := lTempImageName
        else
          if MultipleImagesCreated then begin
            Result := lTempImageName;
            ShowInformation(ResStr_MultipleMessagesCreated);
          end else
            Result := AFileName;
      except
        on E: EOleException do begin
          // Some installations of ImageMagick seem to throw an EOleException
          //  exception after creating the new image successfully!
          // Only raise it if the file doesn't seem to have been created successfully.
          if not FileExists(lTempImageName) then
            if MultipleImagesCreated then begin
              Result := lTempImageName;
              ShowInformation(ResStr_MultipleMessagesCreated)
            end else
              raise;
        end;
      end;
    end;    // if FormatSupportedByImageMagick(AFileName)
  finally
    DefaultCursor(lCursor);
  end;
end;    // TfraMultimedia.ConvertedImageFilename

{-------------------------------------------------------------------------------
  Runs the stored proc. to delete the record for the current frame.
}
procedure TfraMultimedia.DeleteData;
var
  lParentKey: String;
  lFileName: String;
  lFileNameWithoutExtension: String;
  lButtonSelected: Integer;
  lRelatedFileSuffixes: TStringList;
  i: Integer;

  {-----------------------------------------------------------------------------
    Loads the given StringList with the suffixes for related image files,
    as defined in the ImageMagick configuration file.
    Eg. _thumbnail.jpg and webimage.bmp.
  }
  procedure GetRelatedFileSuffixes(lRelatedFileSuffixes: TStringList);
  var
    lImageMagickConfigFile: String;
    lConfigFileXMLDocument: IXMLDocument;
    lConfigFileNodeList: IXMLNodeList;
    lConfigFileNode: IXMLNode;
    i: Integer;
  begin
    lImageMagickConfigFile := IncludeTrailingPathDelimiter(AppSettings.AddinPath)
        + 'Collections Library Files\' + FN_IMAGEMAGICK_CONFIG;
    lConfigFileXMLDocument := NewXMLDocument;

    try
      if FileExists(lImageMagickConfigFile) then
      begin
        // Get all the nodes in the ImageMagick config file
        lConfigFileXMLDocument.LoadFromFile(lImageMagickConfigFile);
        lConfigFileNodeList := lConfigFileXMLDocument.DocumentElement.ChildNodes;

        // Obtain the extensions by iterating through the 'ImageSet' nodes
        for i := 0 to lConfigFileNodeList.Count - 1 do begin
          lConfigFileNode := lConfigFileNodeList.Nodes[i];
          if lConfigFileNode.NodeName = 'ImageSet' then
            lRelatedFileSuffixes.Add(
                '_' + lConfigFileNode.ChildNodes.Nodes['Identifier'].text
                + '.' + lConfigFileNode.ChildNodes.Nodes['Extension'].text);
        end;
      end;  // if FileExists(lImageMagickConfigFile)
      finally
        lConfigFileXMLDocument.Active := false;
    end;  // try .. finally
  end;  // GetRelatedFileSuffixes

begin
  lParentKey := AdditionalProperties.GetProperty(PROP_PARENT_KEY);
  lFileName := dmGeneral.GetRecordset('usp_Multimedia_Select', ['@Key', Key])
      .Fields['File_Name'].Value;
  lFileNameWithoutExtension := AnsiLeftStr(
      lFileName,
      Length(lFileName) - Length(ExtractFileExt(lFileName)));

  // If the file path matches the Specimen Image Path setting, then ask the
  // user if they want to delete the corresponding files on the image server
  if ExtractFilePath(lFileName) = AppSettings.SpecimenImagePath then
  begin
    lButtonSelected := MessageDlg(
        ResStr_AskDeleteImagesOnFileServer,
        mtWarning,
        [mbYes, mbNo],
        0);

    if lButtonSelected = mrYes then
    begin
      lRelatedFileSuffixes := TStringList.Create;

      try
        if FileExists(lFileName) then DeleteFile(lFileName);

        GetRelatedFileSuffixes(lRelatedFileSuffixes);
        for i:= 0 to lRelatedFileSuffixes.Count - 1 do
        begin
          if FileExists(lFileNameWithoutExtension + lRelatedFileSuffixes[i]) then
            DeleteFile(lFileNameWithoutExtension + lRelatedFileSuffixes[i]);
        end;
      finally
        lRelatedFileSuffixes.Free;
      end; // try .. finally
    end; // if lButtonSelected = mrYes
  end; // if ExtractFilePath(lFileName) = AppSettings.SpecimenImagePath

  dmGeneral.RunDeleteStoredProc('usp_Multimedia_Delete',
                                ['@Key', Key, '@RecordKey', lParentKey,
                                          '@Timestamp', FTimestamp]);
end;  // TfraMultimedia.DeleteData

{-------------------------------------------------------------------------------
  Enable the controls. 
}
procedure TfraMultimedia.EnableControls(AEnabled: Boolean);
begin
  inherited;
  rbBestFit.Enabled := True;
  rbFullSize.Enabled := True;
  // Go button should always be available
  btnGo.Enabled := True;
end;  // TfraMultimedia.EnableControls 

{-------------------------------------------------------------------------------
  Returns the caption using either the title or the filename (without the path).
}
function TfraMultimedia.GetCaption: String;
begin
  // If there is a title, use that as the caption
  if eName.Text <> '' then
    Result := eName.Text
  
  // Otherwise use the filename (without any path or directory information).
  else begin
    Result := eFileName.Text;
    while (Pos('\', Result) <> 0) do
      Result := Copy(Result, Pos('\', Result) + 1, Length(Result))
  end;
end;  // TfraMultimedia.GetCaption 

{-------------------------------------------------------------------------------
  Returns true if the linked item is a web address. 
}
function TfraMultimedia.IsWebAddress: Boolean;
begin
  Result := (CompareText(Copy(eFileName.Text, 1, 7), 'http://')=0) or
      (CompareText(Copy(eFileName.Text, 1, 4), 'www.')=0);
end;  // TfraMultimedia.IsWebAddress 

{-------------------------------------------------------------------------------
  Load the timestamp, the custodian and the image. 
}
procedure TfraMultimedia.LoadData;
begin
  inherited;

  if not RegisteredRecordsets[0].Eof then begin
     FTimestamp :=
       RegisteredRecordsets[0].Fields['Timestamp'].Value;
     FCustodian :=
       RegisteredRecordsets[0].Fields['Custodian'].Value;
  end;
  
  LoadPicture(eFileName.Text);
end;  // TfraMultimedia.LoadData 

{-------------------------------------------------------------------------------
  Load image from file. 
}
procedure TfraMultimedia.LoadPicture(const AFileName: String);
var
  bmp: TBitmap;
begin
  if FileExists(AFileName) then begin
    eFileName.Text := AFileName;  // Necessary so that eFileName has text when
                                  // an file name is returned from dlgOpenPic.
    try
      imgPicture.Picture.LoadFromFile(UseThumbnailIfAvailable(AFileName));
    except
      on Exception do begin
        imgPicture.Picture.Graphic := nil;
        lblImageSize.Caption := '';
        lblNoPreview.Visible := True;
        imgNoPreview.Visible := True;
        Exit;
      end;
    end;
    lblNoPreview.Visible := False;
    imgNoPreview.Visible := False;
    if imgPicture.Picture.Graphic is TJPEGImage then begin
      bmp := TBitmap.Create;
      try
        bmp.Assign(TJPEGImage(imgPicture.Picture.Graphic));
        imgPicture.Picture.Bitmap.Assign(bmp);
      finally
        bmp.Free;
      end;
    end;
    lblImageSize.Caption := IntToStr(imgPicture.Picture.Width) + 'x' +
                            IntToStr(imgPicture.Picture.Height);
    ResizeImage;
  end
  else begin
    imgPicture.Picture.Graphic := nil;
    lblImageSize.Caption := '';
    lblNoPreview.Visible := True;
    imgNoPreview.Visible := True;
    Exit;
  end;
end;  // TfraMultimedia.LoadPicture 

{-------------------------------------------------------------------------------
  Resize the picture so tha it fits inside the preview component. 
}
procedure TfraMultimedia.rbBestFitClick(Sender: TObject);
begin
  inherited;
  FFullSize := False;
  ResizeImage;
end;  // TfraMultimedia.rbBestFitClick 

{-------------------------------------------------------------------------------
  Resize the picture to its full size. Scrollbars appear if it is bigger than the viewing 
      area. 
}
procedure TfraMultimedia.rbFullSizeClick(Sender: TObject);
begin
  inherited;
  FFullSize := True;
  ResizeImage;
end;  // TfraMultimedia.rbFullSizeClick 

{-------------------------------------------------------------------------------
  Register the recordset and the controls. 
}
procedure TfraMultimedia.RegisterControls;
begin
  inherited;
  // Register recordsets used
  RegisterRecordset('usp_Multimedia_Select');
  
  // Register controls getting their value straight from the registered recordsets.
  RegisterControl(eName, 'Title');
  RegisterControl(eFileName, 'File_Name', True, ResStr_FileName);
  RegisterControl(chkPreferred, 'Preferred');
end;  // TfraMultimedia.RegisterControls 

{-------------------------------------------------------------------------------
  Register the drag drop components. 
}
procedure TfraMultimedia.RegisterDragDropComponents;
begin
  { TODO : Register drag drop controls }
  // Not yet implemented as dragging and dropping files, rather than items
  // from the database.
end;  // TfraMultimedia.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Resize the loaded image according to selected sizing option. 
}
procedure TfraMultimedia.ResizeImage;
var
  lPicWidth, lPicHeight, lBoxWidth, lBoxHeight, lNewWidth, lNewHeight: Integer;
begin
  lPicWidth  := imgPicture.Picture.Width;
  lPicHeight := imgPicture.Picture.Height;
  lBoxWidth  := scbImage.Width;
  lBoxHeight := scbImage.Height;
  
  // Asked full size, or image fits inside display
  if FFullSize or
     ((lPicWidth <= lBoxWidth) and (lPicHeight <= lBoxHeight)) then
  begin
    imgPicture.AutoSize := True;
    imgPicture.Top      := (lBoxHeight - imgPicture.Height) div 2;
    imgPicture.Left     := (lBoxWidth - imgPicture.Width) div 2;
    if imgPicture.Height >= lBoxHeight then imgPicture.Top := 0;
    if imgPicture.Width >= lBoxWidth then imgPicture.Left := 0;
  end else begin
    // Image bigger than display, shrink to fit
    imgPicture.AutoSize := False;
    scbImage.HorzScrollBar.Position := 0;
    scbImage.VertScrollBar.Position := 0;
    lNewWidth  := (lPicWidth * (lBoxHeight - 4)) div lPicHeight;
    lNewHeight := (lPicHeight * (lBoxWidth - 4)) div lPicWidth;
    if (lPicHeight > lPicWidth) or (lNewHeight > lBoxHeight) then
      imgPicture.SetBounds((lBoxWidth - lNewWidth) div 2, 0, lNewWidth, lBoxHeight - 4)
    else
      imgPicture.SetBounds(0, (lBoxHeight - lNewHeight) div 2, lBoxWidth - 4, lNewHeight);
  end;
end;  // TfraMultimedia.ResizeImage 

{-------------------------------------------------------------------------------
  Save the frame's information to the database. 
}
procedure TfraMultimedia.SaveData;
var
  lParams: Array of Variant;
  lTableName: String;
begin
  inherited; // So validation occurs
  if eName.Text = '' then
    if IsWebAddress then
      eName.Text := eFileName.Text
    else
      eName.Text := ExtractWithoutExt(eFileName.Text);
  FTopNodeContext := AdditionalProperties.GetProperty(PROP_TOP_NODE_CONTEXT);
  
  case FTopNodeContext of
    ncCollection:       lTableName := TN_COLLECTION;
    ncSpecimen:         lTableName := TN_SPECIMEN_UNIT;
    ncStore:            lTableName := TN_STORE;
    ncMovement,
    ncLoan,
    ncAccession:        lTableName := TN_MOVEMENT;
    ncJob:              lTableName := TN_CONSERVATION_JOB;
    ncConditionCheck:   lTableName := TN_CONSERVATION_CHECK;
    ncTask:             lTableName := TN_CONSERVATION_TASK;
    ncInscriptionLabel: lTableName := TN_SPECIMEN_LABEL;
  else
    raise EBrowserFrameError.Create(ResStr_InvalidNodeContext);
  end;
  
  lParams := VarArrayOf(['@Key', Key,
                          '@RecordKey', ParentKey,
                          '@Filename', eFileName.Text,
                          '@Title', eName.Text,
                          '@Preferred', chkPreferred.Checked,
                          '@TableName', lTableName,
                          '@Timestamp', FTimestamp
                      ]);
  // Empty key means new record.
  if Key = '' then
    Key := VarToStr(dmGeneral.RunInsertStoredProc(TN_SOURCE_FILE,
                                                        'usp_Multimedia_Insert',
                                                        lParams,
                                                        '@Key'))
  else
    dmGeneral.RunUpdateStoredProc('usp_Multimedia_Update', lParams);
end;  // TfraMultimedia.SaveData

{-------------------------------------------------------------------------------
  If a thumbnail file exists, returns the file name for it,
  otherwise returns the supplied filename
}
function TfraMultimedia.UseThumbnailIfAvailable(const AFileName: string): string;
var
  lFileName: string;
begin
  Result := AFileName;
  if FileExists(AFileName) then begin
    lFileName := AFileName + THUMBNAIL_SUFFIX;
    if FileExists(lFileName) then
      Result := lFileName;
  end;    // if FileExists(AFileName)
end;    // TfraMultimedia.UseThumbnailIfAvailable

{-------------------------------------------------------------------------------
  Implement specific validation
}
procedure TfraMultimedia.ValidateData;
begin
  inherited;
  // Accept web addresses without validation
  if not IsWebAddress then
    ValidateValue(FileExists(eFileName.Text), ResStr_PleaseSpecifyValidFilename);
end;  // TfraMultimedia.ValidateData

end.

