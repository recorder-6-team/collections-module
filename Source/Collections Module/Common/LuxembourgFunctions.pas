{===============================================================================
  Unit:        LuxembourgFunctions.pas

  Defines:     Nothing

  Description: Module containing functions useful throughout the Luxembourg
      Collections app.

  Created:     September 2003

===============================================================================}
unit LuxembourgFunctions;

interface

uses
  Controls,Classes,Windows, Messages,SysUtils;

type
  TImageSet = record
        Identifier : string;
        Extension  : string;
        Dimension  : string;
   end;
   TImageSetArray = array of TImageSet;
   EInvalidXMLFile =  class(Exception);

procedure ParseMeasurementValue(const AMeasurementValue: String;
    out AMeasurementLower, AMeasurementUpper: String);

function BuildMeasurementValue(const AMeasurementLower, AMeasurementUpper: String): String;

function RichTextToText(AParent: TWinControl; const ARichText: String): String;

function FormatSupportedByImageMagick(const AFileName: string): Boolean;

function GetImageMagickParamSet(const AFileName:string): TImageSetArray;

function ConceptHasDomain(const ConceptKey, DomainKey: String): Boolean;


//==============================================================================
implementation

uses
 ComCtrls, StrUtils, ApplicationSettings, GeneralData, XMLDom, XMLDoc, XMLIntf,
  LuxembourgConstants,ResourceStrings, Variants;

{-------------------------------------------------------------------------------
Parses the upper and lower values of the measurement based on the measurement
string.
}
procedure ParseMeasurementValue(const AMeasurementValue: String;
    out AMeasurementLower, AMeasurementUpper: String);
var
  i: Integer;
begin
  try
    //if AMeasurement value is a number, treat it as a single case
    StrToFloat(Trim(AMeasurementValue));
    AMeasurementLower := AMeasurementValue;
    AMeasurementUpper := AMeasurementValue;
  except
    on EConvertError do // not a float
    begin //find out if it's a range
      i := PosEx('TO', UpperCase(AMeasurementValue));
      if i = 0 then
      begin
        i := PosEx('-', AMeasurementValue);
        if i = 0 then
        begin
          // it's not a range.
          AMeasurementLower := AMeasurementValue;
          AMeasurementUpper := AMeasurementValue;
        end else begin
          //it's a range separated by '-'
          AMeasurementLower := Trim(LeftStr(AMeasurementValue, i -1));
          AMeasurementUpper := Trim(RightStr(AMeasurementValue,
                                             Length(AMeasurementValue) - i + 1));
        end;
      end else begin
        //it's a range separated by 'to'
        AMeasurementLower := Trim(LeftStr(AMeasurementValue, i -1));
        AMeasurementUpper := Trim(RightStr(AMeasurementValue,
                                           Length(AMeasurementValue) - i + 2));
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
Returns the measurement based on the upper and lower values.
}
function BuildMeasurementValue(const AMeasurementLower, AMeasurementUpper: String): String;
begin
  if AMeasurementLower = AmeasurementUpper then
    Result := AMeasurementLower
  else
    Result := AMeasurementLower + ' - ' + AMeasurementUpper;
end;

{-------------------------------------------------------------------------------
  Remove richtext formatting and return plaintext string.
}
function RichTextToText(AParent: TWinControl; const ARichText: String): String;
var
  lRE: TRichEdit;
  lMS: TMemoryStream;
begin
  lMS := nil;
  lRE := nil;
  try
    lMS := TMemoryStream.Create;
    lRE := TRichEdit.Create(AParent);
    lRE.Visible := False;
    lRE.Parent := AParent;

    lMS.Write(ARichText[1], Length(ARichText));
    lMS.Position := 0;
    lRE.Lines.LoadFromStream(lMS);
    Result := lRE.Lines.Text;
  finally
    lRE.Free;
    lMS.Free;
  end;
end;  // RichTextToText

{-------------------------------------------------------------------------------
  Determines whether the specified file is of a format supported by ImageMagick.
}
function FormatSupportedByImageMagick(const AFileName: string): Boolean;
var
  lImageMagickExtensionsFile, lFileExtension: string;
  lImageMagickExtensions: TStringList;
  lXmlDocumentation:IXMLDocument;
  lSupportedFileTypes:IXMLNode;
  i: Integer;
begin
  Result := False;
  lImageMagickExtensionsFile := IncludeTrailingPathDelimiter(AppSettings.AddinPath)
      + 'Collections Library Files\' + FN_IMAGEMAGICK_CONFIG;
  if FileExists(lImageMagickExtensionsFile) then begin
    lFileExtension := ExtractFileExt(AFileName);
    if Length(lFileExtension) > 0 then
      if lFileExtension[1] = '.' then
        lFileExtension := Copy(lFileExtension, 2, MaxInt);
    lImageMagickExtensions := TStringList.Create;
    lXmlDocumentation :=NewXMLDocument ;
    lXmlDocumentation.LoadFromFile(lImageMagickExtensionsFile);
    lXmlDocumentation.Active:=true;
    try
      lSupportedFileTypes :=lXmlDocumentation.DocumentElement.ChildNodes.FindNode('SupportedFileTypes');
      if lSupportedFileTypes = nil then
        raise EInvalidXMLFile.Create(ResStr_InvalidNode)
      else begin
        for i := 0 to lSupportedFileTypes.ChildNodes.Count - 1 do
          if lSupportedFileTypes.ChildNodes.Nodes[i].NodeName = 'Extension' then
            lImageMagickExtensions.Add(lSupportedFileTypes.ChildNodes.Nodes[i].Text)
          else
            raise EInvalidXMLFile.Create(ResStr_InvalidNode);
        Result := lImageMagickExtensions.IndexOf(lFileExtension) >= 0;
      end;
    finally
      lImageMagickExtensions.Free;
      lXmlDocumentation.Active:=false;
    end;   //end try...finally
  end;    // if FileExists(lImageMagickExtensionsFile)
end;    // FormatSupportedByImageMagick


{-------------------------------------------------------------------------------
  Get all configurations(e.g. extension, dimention and identifier of image from xml file.
}
function GetImageMagickParamSet(const AFileName:string): TImageSetArray;
var
  lImageMagickExtensionsFile: string;
  lImageMagickSet: TImageSetArray;
  i: Integer;
  lXmlDocumentation: IXMLDocument;
  lImageSet: IXMLNode;
begin
  Result := nil;
  lImageMagickExtensionsFile := IncludeTrailingPathDelimiter(AppSettings.AddinPath) +
                                'Collections Library Files\' + FN_IMAGEMAGICK_CONFIG;
  if FileExists(lImageMagickExtensionsFile) then begin
    lXmlDocumentation := NewXMLDocument;
    lXmlDocumentation.LoadFromFile(lImageMagickExtensionsFile);
    lXmlDocumentation.Active := true;
    try
      if lXmlDocumentation.DocumentElement.ChildNodes.Count > 1 then
      begin
        SetLength(lImageMagickSet,lXmlDocumentation.DocumentElement.ChildNodes.Count - 1);
        lImageSet := lXmlDocumentation.DocumentElement.ChildNodes.FindNode('ImageSet');
        if lImageSet = nil then
          raise EInvalidXMLFile.Create(ResStr_InvalidNode)
        else begin
          for i := 0 to lXmlDocumentation.DocumentElement.ChildNodes.Count - 2 do begin
            if (lImageSet.ChildNodes.FindNode('Identifier') <> nil) and
               (lImageSet.ChildNodes.FindNode('Extension')  <> nil) and
               (lImageSet.ChildNodes.FindNode('DimensionX') <> nil) and
               (lImageSet.ChildNodes.FindNode('DimensionY') <> nil) then begin
              lImageMagickSet[i].Identifier := lImageSet.ChildNodes.FindNode('Identifier').Text;
              lImageMagickSet[i].Extension  := lImageSet.ChildNodes.FindNode('Extension').Text;
              lImageMagickSet[i].Dimension  := lImageSet.ChildNodes.FindNode('DimensionX').Text + 'x' +
                                               lImageSet.ChildNodes.FindNode('DimensionY').Text;
              lImageSet := lImageSet.NextSibling;
            end else   //end..if
              raise EInvalidXMLFile.Create(ResStr_InvalidNode);
          end; // end..for
          Result := lImageMagickSet;
        end; // end.. if
      end else
        raise EInvalidXMLFile.Create(ResStr_MissingImageSetNode);
    finally
      lXmlDocumentation.Active := false;
    end;//end try..finally
  end;  //end if
end; // GetImageMagickParamSet

{-------------------------------------------------------------------------------
  Returns a value indicating whether the specified concept is within a
  particular domain.
}
function ConceptHasDomain(const ConceptKey, DomainKey: String): Boolean;
var
  lConceptDomainKey: String;
begin
  lConceptDomainKey := dmGeneral.GetStoredProcOutputParam(
      'dbo.usp_Domain_Get_ForConcept',
      ['@ConceptKey', ConceptKey],
      '@DomainKey');
  Result := lConceptDomainKey = DomainKey;
end;

end.
