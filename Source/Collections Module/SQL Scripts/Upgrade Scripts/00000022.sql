UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Set of options for better use of vw_ConceptTerm.
SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

DECLARE @Status VARCHAR(50)
DECLARE @SurveyOrganiser VARCHAR(100)
DECLARE @GeographicInfo VARCHAR(8000)
DECLARE @Department VARCHAR(100)
DECLARE @Key CHAR(16)
DECLARE @ShowCommonNames BIT

SET @Key = ''<#ReportKey>''
SET @ShowCommonNames = <#ShowCommonNames>

--Find Nomanclature status
EXEC usp_CollectionUnitDepartment_Get @Key, @Department OUTPUT

--Find specimen status
EXEC usp_CollectionUnitStatus_Get @Key, @Status OUTPUT

--Find Geographic Info
SELECT @GeographicInfo = M.[Text]
FROM Metadata M 
INNER JOIN Metadata_Type MT ON 
    M.Metadata_Type_Key = MT.Metadata_Type_Key
    AND M.Metadata_Type_Key = ''SYSTEM0000000005'' --Geo Info for Specimen
    AND MT.Table_Name = ''Specimen''
WHERE M.Record_Key=@Key

--Find Survey Organiser
SELECT @SurveyOrganiser = dbo.ufn_GetFormattedName(SY.Run_By)
FROM
	SPECIMEN_UNIT SU
	INNER JOIN
		SPECIMEN_FIELD_DATA SFD
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		COLLECTION_UNIT_NUMBER CUN
	ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
	LEFT JOIN 
		OCCURRENCE O
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		TAXON_OCCURRENCE XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN
		SAMPLE SA
	ON (O.Sample_Key = SA.Sample_Key) OR (XO.Sample_Key = SA.Sample_Key)
	INNER JOIN
		Survey_Event SE
	ON SA.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN
		Survey SY
	ON SE.Survey_Key = SY.Survey_Key
WHERE SU.Collection_Unit_Key=@Key

--Find all other required information
SELECT TOP 1 
	CASE WHEN SU.Life_Sciences = 0 THEN 
		ISNULL(dbo.ufn_ConceptItemName_Get(D.Concept_Key, @ShowCommonNames, 0, 1), ''No Determination'') 
	ELSE
		ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames), ''No Determination'') 
	END AS Determination,
	CASE WHEN CTN1.PlainText IS NULL THEN CTN2.PlainText
		ELSE CTN1.PlainText
	END AS NomenclatureStatus,
	CUN.Number AS PrefNumber,
	M.Number AS AccNumber,
	CTType.PlainText AS Type,
	@GeographicInfo AS GeographicInfo,
	@Status AS Status,
	dbo.ufn_GetFormattedName(CUName.Name_Key) AS Owner,
	ISNULL(C.Item_Name + '' - '' + CM.Number, C.Item_Name) AS CollectionName,
	@Department AS Department,
	ISNULL(S.Item_Name + '' - '' + SCUN.Number, S.Item_Name) AS StoreName,
	@SurveyOrganiser AS SurveyOrganiser,
	CU.Current_Location_Code AS StoreCode
FROM				
	SPECIMEN_UNIT AS SU
	INNER JOIN
		Collection_Unit CU
	ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
	INNER JOIN 	
		VW_ConceptTerm AS CTType ON 
	SU.Specimen_Type_Concept_Key = CTType.Concept_Key

	LEFT JOIN
		Collection_Unit_Name CUName 
	ON SU.Collection_Unit_Key = CUName.Collection_Unit_Key
		AND CUName.Relation_Type_Concept_Key = ''SYSTEM00000000I7'' --Owner

	LEFT JOIN
		Store S
	ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN
		Collection_Unit_Number AS SCUN
	ON S.Collection_Unit_Key = SCUN.Collection_Unit_Key
		AND SCUN.Preferred = 1

	LEFT JOIN	
		Collection C 
	ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT CMCU
		INNER JOIN
			MOVEMENT_DIRECTION CMD
		ON CMCU.Movement_Direction_Key = CMD.Movement_Direction_Key 
			AND (CMD.Outbound = 0)
		INNER JOIN
			MOVEMENT CM
		ON CMD.Movement_Key = CM.Movement_Key AND (CM.Movement_Type = 0 OR CM.Movement_Type = 1))
	ON C.Collection_Unit_Key = CMCU.Collection_Unit_Key

	LEFT JOIN 
		Collection_Unit_Number CUN 
	ON SU.Collection_Unit_key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 

	LEFT JOIN 
		Determination D 
	ON D.Determination_Key = SU.Preferred_Determination_Key

	LEFT JOIN 
		VW_ConceptTermPreferred CTN1
	ON D.Nomenclatural_Status_Concept_Key = CTN1.Concept_Key

	LEFT JOIN 
		Taxon_Determination TD 
	ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 
		Index_Taxon_Name ITN 
	ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	LEFT JOIN 
		VW_ConceptTermPreferred CTN2
	ON TD.Nomenclatural_Status_Concept_Key = CTN2.Concept_Key

	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT MCU
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
			AND (MD.Outbound = 0)
		INNER JOIN
			MOVEMENT M
		ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
	ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000000'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

DECLARE @Key CHAR(16)
DECLARE @ShowCommonNames BIT

SET @Key = ''<#ReportKey>''
SET @ShowCommonNames = 1

SET NOCOUNT ON

DECLARE @ResultTable TABLE
(	[Collection_Unit_Key] [char] (16) COLLATE database_default NOT NULL,
	[PrefNo] [varchar] (30) COLLATE database_default NULL,
	[Determination] [nvarchar] (185) COLLATE database_default NOT NULL,
	[Type] [nvarchar] (150) COLLATE database_default NULL,
	[FieldCollector] [varchar] (70) COLLATE database_default NULL,
	[GatheringSite] [varchar] (100) COLLATE database_default NULL,
	[GatheringDate] [varchar] (50) COLLATE database_default NULL,
	[StoreCode] [varchar] (30) COLLATE database_default NULL
)

--Find all basic data
INSERT INTO	@ResultTable
	([Collection_Unit_Key],
	[PrefNo],
	[Determination],
	[Type],
	[FieldCollector],
	[StoreCode],
	[GatheringDate],
	[GatheringSite]
	)
SELECT DISTINCT
	SU.Collection_Unit_Key,
	CUN.Number AS PrefNo,
	CASE WHEN SU.Life_Sciences = 0 THEN 
		ISNULL(dbo.ufn_ConceptItemName_Get(D.Concept_Key, @ShowCommonNames, 0, 1), ''No Determination'') 
	ELSE
		ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames), ''No Determination'') 
	END AS Determination,
	CTType.Item_Name AS Type,
 	dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
	CU.Current_Location_Code AS StoreCode,
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_date_End, S.Vague_Date_Type),
	LN.Item_Name
FROM COLLECTION_UNIT_RELATION CUR
INNER JOIN THESAURUS_RELATION_TYPE TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
INNER JOIN SEMANTIC_RELATION SR
    ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
    AND (CUR.From_Collection_Unit_Key = @Key)
    OR ((CUR.To_Collection_Unit_Key = @Key) AND (SR.Unidirectional = 0))
LEFT JOIN	Specimen_Unit SU 
    ON (CUR.From_Collection_Unit_Key = @Key AND CUR.To_Collection_Unit_Key = SU.Collection_Unit_Key)
		OR (CUR.To_Collection_Unit_Key = @Key AND CUR.From_Collection_Unit_Key = SU.Collection_Unit_Key)
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTType ON SU.Specimen_Type_Concept_Key = CTType.Concept_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON SU.Collection_Unit_key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 
LEFT JOIN Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred CTN1 ON D.Nomenclatural_Status_Concept_Key = CTN1.Concept_Key
LEFT JOIN Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN Sample S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key 
    AND LN.Preferred=1

SELECT 
	[Collection_Unit_Key],
	[PrefNo],
	[Determination],
	[Type],
	[FieldCollector],
	[GatheringSite],
	[GatheringDate],
	[StoreCode]
FROM
	@ResultTable
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000003'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '
SELECT
  ct.Conservation_Task_Key, 
  cc.Ref_Number + '' - '' + dbo.ufn_GetFormattedName(cc.Checked_By_Name_Key) + '' - '' +
    dbo.ufn_GetDateFromVagueDate(cc.Vague_Date_Start, cc.Vague_Date_End, cc.Vague_Date_Type) AS [Check], 
  ctype.PlainText AS Type,
  dbo.ufn_GetTranslation_String_From_Value(ct.Status, ''STATUS'') AS Status,
  dbo.ufn_GetTranslation_String_From_Value(ct.Priority, ''PRIORITY'') AS Priority,
  ct.Task_Action AS [Action],
  CAST(ct.Duration AS VARCHAR(20)) + '' '' + cdur.PlainText AS Duration,
  dbo.ufn_GetFormattedName(ct.Identifier_Name_Key) AS IdentifiedBy,
  dbo.ufn_GetDateFromVagueDate(ct.Set_Vague_Date_Start, ct.Set_Vague_Date_End, ct.Set_Vague_Date_Type) AS DateSet,
  CAST(cj.Job_Number as varchar(20)) + '' - '' + cj.Item_Name + '' - '' +
    dbo.ufn_GetTranslation_String_From_Value(ct.Status, ''STATUS'') AS JobAssignedTo
FROM				Conservation_Task ct
LEFT JOIN		Conservation_Check cc on cc.Conservation_Check_Key = ct.Conservation_Check_Key
INNER JOIN 	VW_ConceptTerm ctype on ctype.Concept_Key = ct.Type_Concept_Key
LEFT JOIN 	VW_ConceptTerm cdur on cdur.Concept_Key = ct.Duration_Unit_Concept_Key
LEFT JOIN 	Conservation_Job cj on cj.Conservation_Job_Key=ct.Conservation_Job_Key
WHERE				ct.Conservation_Task_Key = ''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000A'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
    V.Ref_Number AS ReferenceNumber,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) AS [Date],
    CType.PlainText AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(SUBSTRING(TF.Data, 1, 4), '''') + 
      CASE WHEN V.Value_Amount=ROUND(V.Value_Amount, 0) THEN
        --Lop of the decimal places if not required  
        LEFT(CONVERT(VARCHAR(20), V.Value_Amount, 1), LEN(CONVERT(VARCHAR(20), V.Value_Amount, 1))-3)
      ELSE
        CONVERT(VARCHAR(20), V.Value_Amount, 1)
      END + 
      CASE 
        WHEN TF.DATA IS NULL THEN
          '' '' + Ccurr.Item_Name
        ELSE
          ''''
      END
    AS Value,
    ISNULL(dbo.ufn_GetDateFromVagueDate(V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type), '''') + '' - '' 
        + ISNULL(dbo.ufn_GetDateFromVagueDate(V.Valid_To_Vague_Date_Start, V.Valid_To_Vague_Date_End, V.Valid_To_Vague_Date_Type), '''') 
    AS Valid,
    V.[Description]
FROM			Valuation V
LEFT JOIN	VW_ConceptTerm CType ON V.Type_Concept_Key = CType.Concept_Key
LEFT JOIN	Concept C ON C.Concept_Key = V.Currency_Concept_Key
LEFT JOIN VW_ConceptTerm Ccurr ON Ccurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN	Thesaurus_Fact TF
	ON (TF.Meaning_Key = C.Meaning_Key AND
			TF.Fact_Type_Concept_Key = ''SYSTEM000000009A'')
WHERE			V.Valuation_Key = ''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000E'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '
SELECT DISTINCT
  dbo.ufn_GetPrefNumber(SU.Collection_Unit_Key) AS PrefNo,
  CASE SU.Life_Sciences
    WHEN 1 THEN ITN.Preferred_Name
    ELSE CTP.Item_Name
  END AS Determination,
  CType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(CU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM			Collection_Unit_Valuation AS CUV
JOIN			Collection_Unit AS CU ON CUV.Collection_Unit_Key = CU.Collection_Unit_Key
JOIN			Specimen_Unit	AS SU ON CUV.Collection_Unit_Key = SU.Collection_Unit_Key
LEFT JOIN	(Determination AS D
					JOIN vw_ConceptTermPreferred CTP ON D.Concept_Key = CTP.Concept_Key)
					ON SU.Preferred_Determination_Key = D.Determination_Key
LEFT JOIN	(Taxon_Determination AS TD
					JOIN Index_Taxon_Name AS ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
					ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
LEFT JOIN	vw_ConceptTerm CType ON SU.Specimen_Type_Concept_Key = CType.Concept_Key
LEFT JOIN (Specimen_Field_Data SFD
					LEFT JOIN Taxon_Occurrence AS XO ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
					LEFT JOIN Occurrence AS O ON SFD.Occurrence_Key = O.Occurrence_Key
					JOIN [Sample] AS S ON ISNULL(O.Sample_Key, XO.Sample_Key) = S.Sample_Key
					JOIN Location_Name AS LN ON S.Location_Key = LN.Location_Key AND LN.Preferred = 1)
					ON CU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SFD.Gathering_Event = 1  
WHERE			CUV.Valuation_Key = ''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000F'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '
SELECT M.Number AS AccNo,
					C.Item_Name AS [Name],
					C.Topic AS Topic,
					dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy
FROM			Collection_Unit_Valuation AS CUV
JOIN			Collection_Unit AS CU ON CUV.Collection_Unit_Key = CU.Collection_Unit_Key
LEFT JOIN	(Movement_Collection_Unit AS MCU
					JOIN Movement_Direction AS MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND MD.Outbound = 0
					JOIN Movement AS M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
					JOIN Movement_Of_Ownership AS MOO ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key)
					ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
JOIN			Collection AS C ON CU.Collection_Unit_Key = C.Collection_Unit_Key
LEFT JOIN	Collection_Unit_Number AS CUN
						ON CU.Collection_Unit_Key = CUN.Collection_Unit_Key
						AND CUN.Preferred = 1
WHERE			CUV.Valuation_Key = ''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000G'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT  dbo.ufn_GetDateFromVagueDate(CT.Set_Vague_Date_Start, CT.Set_Vague_Date_End, CT.Set_Vague_Date_Type) AS DateSet, 
CASE CT.Priority WHEN 0 THEN ''Low'' WHEN 1 THEN ''Medium'' WHEN 2 THEN ''High'' WHEN 3 THEN ''Urgent'' END AS Priority, 
CASE CT.Status WHEN 0 THEN ''Pending'' WHEN 1 THEN ''Open'' WHEN 2 THEN ''Closed'' WHEN 3 THEN ''Postponed'' WHEN 4 THEN ''Abandoned'' END AS Status, 
CType.PlainText AS Type, 
CONVERT(VARCHAR(20), Duration) + '' '' + CUnit.PlainText AS Duration, 
CT.Task_Action AS [Action], 
dbo.ufn_GetFormattedName(CT.Identifier_Name_Key) AS IdentifiedBy 
FROM Conservation_Task AS CT 
JOIN Conservation_Check AS CC ON CC.Conservation_Check_Key = CT.Conservation_Check_Key AND CT.Conservation_Job_Key = ''<#ReportKey>'' AND ((CC.Domain_Mask & <#UserDomainMask> > 0) OR (CC.Entered_Session_ID = ''<#SessionID>'') OR (CC.Changed_Session_ID = ''<#SessionID>'') OR (CC.Domain_Mask = 0)) 
LEFT JOIN VW_ConceptTerm AS CType ON CT.Type_Concept_Key = CType.Concept_Key 
LEFT JOIN VW_ConceptTerm AS CUnit ON CT.Duration_Unit_Concept_Key = CUnit.Concept_Key
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000K'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '
SET ANSI_NULLS ON
SET ANSI_PADDING ON  
SET ANSI_WARNINGS ON  
SET CONCAT_NULL_YIELDS_NULL ON  
SET QUOTED_IDENTIFIER ON  
SET NO_BROWSETABLE OFF    

SELECT
  dbo.ufn_GetPrefNumber(SU.Collection_Unit_Key) AS PrefNo,
  CASE  
    WHEN SU.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, ''No Determination'')
  ELSE
    ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
        ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name,
        ITN.Common_Name_Italic, ITN.Authority, 1), ''No Determination'')
  END AS Determination,
  CTType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  dbo.ufn_GetSpecimenGatheringSite(SU.Collection_Unit_Key) AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(
      S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM  Specimen_Unit AS SU  
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = SU.Collection_Unit_Key  
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN  VW_ConceptTerm AS CTType ON CTType.Concept_Key = SU.Specimen_Type_Concept_Key  
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''
LEFT JOIN  Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN  Occurrence O ON O.Occurrence_Key=D.Determination_Key
LEFT JOIN  VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key  
LEFT JOIN  Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key  
LEFT JOIN  Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=TD.Taxon_Determination_Key
LEFT JOIN  Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key  
LEFT JOIN  [Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000N'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '
SELECT 
  CUN.Number,
  S.Item_Name AS [Name],
  CASE
    WHEN SU.Collection_Unit_Key IS NULL THEN dbo.ufn_GetTranslation_String_From_Value(0, ''YESNO'')
    ELSE dbo.ufn_GetTranslation_String_From_Value(1, ''YESNO'') + '' - '' + CTC.Item_Name + '': '' + CUN.Number
  END AS Specimen,
  CU.Current_Location_Code AS StoreCode,
  S.Comment AS Comments
FROM Store S
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=S.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=S.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN VW_ConceptTermCommon CTC
    ON CTC.Concept_Key=CUN.Type_Concept_Key
LEFT JOIN Specimen_Unit SU
    ON SU.Collection_Unit_Key=S.Collection_Unit_Key
WHERE S.Collection_Unit_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000Q'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT 
  M.Number AS AccNo,
  C.Item_Name AS [Name],
  dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  C.Topic,
  CASE 
    WHEN CN.Number IS NULL THEN S.Item_Name
  ELSE
    S.Item_Name + '' - '' + CN.Number
  END AS Store
FROM Collection C
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''
LEFT JOIN Store S 
    ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN 
    (Collection_Unit_Number CN
    INNER JOIN Concept CPT 
        ON CPT.Concept_Key=CN.Type_Concept_Key)
    ON CN.Collection_Unit_Key=S.Collection_Unit_Key
    AND CN.Preferred=1
LEFT JOIN 
    (MOVEMENT_COLLECTION_UNIT MCU 
        INNER JOIN MOVEMENT_DIRECTION MD 
            ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
            AND MD.Outbound = 0
        INNER JOIN MOVEMENT M 
            ON MD.Movement_Key = M.Movement_Key 
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1)) 
    ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000V'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
  dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
  S.Item_Name AS [Name],
  CTS.PlainText AS Type,
  CU.Current_Location_Code AS Store
FROM Store AS S
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = S.Collection_Unit_Key
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN VW_ConceptTerm AS CTS 
    ON CTS.Concept_Key = S.Store_Type_Concept_Key
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000W'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
  SU.Collection_Unit_Key,
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000000Y'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
      dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
      S.Item_Name AS [Name],
      CTS.PlainText AS Type,
      CUS.Current_Location_Code AS Store
  FROM Movement_Direction MD
  INNER JOIN Movement_Collection_Unit MCU
       ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
  INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE MD.Movement_Key=''<#ReportKey>''
      AND MD.Outbound=0

'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000010'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT 
    V.Ref_Number as RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
      END
    AS Value,
    ISNULL (
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type) + '' - '' +
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_To_Vague_Date_Start, V.Valid_To_Vague_Date_End, V.Valid_To_Vague_Date_Type), 
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type)
    ) AS [Valid]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
JOIN Session S ON S.Session_ID=''<#SessionID>''
JOIN [User] U ON U.Name_Key=S.User_Name_Key
WHERE U.Allow_Finance=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000011'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'DECLARE @DomainMask INT
SELECT @DomainMask=Domain_Mask
FROM Conservation_Check
WHERE Conservation_Check_Key=''<#ReportKey>''

DECLARE @Domains VARCHAR(1000)

EXEC usp_DomainsForMask_Get @DomainMask, @Domains OUTPUT

SELECT
    CC.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type) AS Date,
    dbo.ufn_GetFormattedName(CC.Checked_By_Name_Key) + '' - '' + CType.Item_Name AS CheckedByType,
    CCond.Item_Name AS Condition,
    CC.Details,
    @Domains AS Domains
FROM Conservation_Check CC
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=CC.Type_Concept_Key
INNER JOIN VW_ConceptTerm CCond 
    ON CCond.Concept_Key=CC.Condition_Concept_Key
WHERE Conservation_Check_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000014'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
      dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
      S.Item_Name AS [Name],
      CTS.PlainText AS Type,
      CUS.Current_Location_Code AS Store,
      CASE CC.Applies_To_Contained_Specimens
        WHEN 1 THEN ''<img src="<#StandardReportTemplatePath>ok.gif">''
        ELSE ''''
      END AS Contents
  FROM Conservation_Check CC
  INNER JOIN Collection_Unit_Check CUC
      ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
  INNER JOIN Store S
       ON S.Collection_Unit_Key=CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS
       ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS
       ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CC.Conservation_Check_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000015'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Conservation_Check CC
INNER JOIN Collection_Unit_Check CUC
    ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=CUC.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE CC.Conservation_Check_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000017'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT 
    dbo.ufn_GetDateFromVagueDate(CT.Set_Vague_Date_Start, CT.Set_Vague_Date_End, CT.Set_Vague_Date_Type) AS DateSet,
    Task_Action AS Action,
    CType.Item_Name AS Type,
    CAST(Duration AS VARCHAR(20)) + ISNULL('' '' + CUnit.Item_Name, '''') AS Duration,
    dbo.ufn_GetConservationStatus(CT.Status) AS Status,
    dbo.ufn_GetFormattedName(CT.Identifier_Name_Key) AS IdentifiedBy,
    CASE CT.Priority
      WHEN 0 THEN ''Low''
      WHEN 1 THEN ''Medium''
      WHEN 2 THEN ''High''
      WHEN 3 THEN ''Urgent''
    END AS Priority
FROM Conservation_Task CT
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=CT.Type_Concept_Key
LEFT JOIN VW_ConceptTerm CUnit 
    ON CUnit.Concept_Key=CT.Duration_Unit_Concept_Key
WHERE CT.Conservation_Check_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000018'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001C'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001E'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001J'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001L'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001M'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001O'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT 
    V.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
        END
    AS Value,
    [Description]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
LEFT JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001P'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Specimen Stores
SELECT DISTINCT
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Specimen_Unit U
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=U.Collection_Unit_Key
INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE U.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001W'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT CUN.Number AS PrefNo,
    CASE WHEN D.Determination_Key IS NULL THEN
     CASE ITN.Preferred_Name_Italic WHEN 1 THEN
		 ''<i>'' + ITN.Preferred_Name + ''</i>'' 
		ELSE ITN.Preferred_Name END
    ELSE DPref.Item_Name END AS Determination,
    CType.Item_Name AS Type,
    dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
    LN.Item_Name AS GatheringSite,
    dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start,
	S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
    SU.Collection_Unit_Key,
    CU.Current_Location_Code AS Store 
FROM Specimen_Unit SU 
LEFT JOIN Collection_Unit_Number CUN ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key AND CUN.Preferred=1 
LEFT JOIN Determination D ON D.Determination_Key=SU.Preferred_Determination_Key 
LEFT JOIN VW_ConceptTermPreferred DPref ON DPref.Concept_Key=D.Concept_Key 
LEFT JOIN Taxon_Determination TD ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key 
LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key 
INNER JOIN VW_ConceptTerm CType ON CType.Concept_Key=SU.Specimen_Type_Concept_Key 
LEFT JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key AND SFD.Gathering_Event=1 
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key 
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key 
LEFT JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key=XO.Sample_Key 
LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key AND LN.Preferred=1 
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=SU.Collection_Unit_Key 
WHERE SU.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000001X'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Locality
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  RefNo VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Type VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
  Store VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)

DECLARE @Current_Key CHAR(16)
DECLARE @Count INTEGER
SELECT @Count = 0

SELECT @Current_Key = ''<#ReportKey>''

WHILE (@Current_Key IS NOT NULL AND @Current_Key != '''')
BEGIN
  INSERT INTO @ResultsTable (OrderValue, RefNo, Type, Store)
  SELECT
    @Count AS OrderValue,
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CU.Collection_Unit_Key=@Current_Key

  SELECT @Current_Key = CU.Current_Container_Collection_Unit_Key,
    @Count = @Count + 1
  FROM Collection_Unit CU
  WHERE CU.Collection_Unit_Key=@Current_Key
END

SET NOCOUNT OFF

SELECT RefNo, Type, Store FROM @ResultsTable
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000022'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Specimens
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS  NOT NULL,
  Current_Container_Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Done TINYINT DEFAULT 0
)

DECLARE @Current_Key CHAR(16)
DECLARE @Parent_Key CHAR(16)
DECLARE @Count INTEGER
DECLARE @Done INTEGER
SELECT @Count = 1


INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
VALUES (0, ''<#ReportKey>'', NULL, 0)


SELECT @Current_Key = ''<#ReportKey>'', @Parent_Key = NULL

WHILE EXISTS (SELECT * FROM @ResultsTable WHERE Done = 0)
BEGIN
  -- INSERT the children of the current node into the results - if they are not there
  INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
  SELECT TOP 1 0, CU.Collection_Unit_Key, CU.Current_Container_Collection_Unit_Key, 0
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key = CU.Collection_Unit_Key 
  WHERE CU.Current_Container_Collection_Unit_Key = @Current_Key AND CU.Collection_Unit_Key NOT IN (
    SELECT Collection_Unit_Key FROM @ResultsTable
  )
  IF (@@ROWCOUNT > 0)
  BEGIN
  -- If you can :
  --   Go Down 
    SELECT @Parent_Key = @Current_Key, @Current_Key = Collection_Unit_Key
    FROM @ResultsTable WHERE Done = 0 AND Current_Container_Collection_Unit_Key = @Current_Key
    UPDATE @ResultsTable SET OrderValue = @Count WHERE Collection_Unit_Key = @Current_Key
    SELECT @Count = @Count + 1
  END
  ELSE
  BEGIN
  -- If you can''t (this node is done) :
    UPDATE @ResultsTable SET Done = 1 WHERE Collection_Unit_Key = @Current_Key
    SELECT @done = 1
    WHILE (@done = 1 AND @Parent_Key IS NOT NULL)
      SELECT @Current_Key = @Parent_Key, @Parent_Key = Current_Container_Collection_Unit_Key, @done = Done
      FROM @ResultsTable WHERE Collection_Unit_Key = @Parent_Key
  END
END

SET NOCOUNT OFF

SELECT DISTINCT
  R.OrderValue, 
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Specimen_Unit SU 
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN @ResultsTable R ON R.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000023'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Stores
SELECT DISTINCT
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Store S
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE CUS.Current_Container_Collection_Unit_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000025'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT DISTINCT
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000029'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = '-- Stores
SELECT
    dbo.ufn_GetPrefNumber(MCU.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000002B'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL] = 'SELECT 
    V.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
        END
    AS Value,
    [Description]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM000000002C'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL_Record_Count] = '-- Locality
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  RefNo VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Type VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
  Store VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)

DECLARE @Current_Key CHAR(16)
DECLARE @Count INTEGER
SELECT @Count = 0

SELECT @Current_Key = ''<#ReportKey>''
WHILE (@Current_Key IS NOT NULL AND @Current_Key != '''')
BEGIN
  INSERT INTO @ResultsTable (OrderValue, RefNo, Type, Store)
  SELECT
    @Count AS OrderValue,
    dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CU.Collection_Unit_Key=@Current_Key

  SELECT @Current_Key = CU.Current_Container_Collection_Unit_Key,
    @Count = @Count + 1
  FROM Collection_Unit CU
  WHERE CU.Collection_Unit_Key=@Current_Key
END
SET NOCOUNT OFF
SELECT COUNT(*) AS Count FROM @ResultsTable
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000022'

GO

UPDATE [Report_Block_In_Section]
SET [Population_SQL_Record_Count] = '-- Specimens
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS  NOT NULL,
  Current_Container_Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Done TINYINT DEFAULT 0
)

DECLARE @Current_Key CHAR(16)
DECLARE @Parent_Key CHAR(16)
DECLARE @Count INTEGER
DECLARE @Done INTEGER
SELECT @Count = 1


INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
VALUES (0, ''<#ReportKey>'', NULL, 0)


SELECT @Current_Key = ''<#ReportKey>'', @Parent_Key = NULL

WHILE EXISTS (SELECT * FROM @ResultsTable WHERE Done = 0)
BEGIN
  INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
  SELECT TOP 1 0, CU.Collection_Unit_Key, CU.Current_Container_Collection_Unit_Key, 0
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key = CU.Collection_Unit_Key 
  WHERE CU.Current_Container_Collection_Unit_Key = @Current_Key AND CU.Collection_Unit_Key NOT IN (
    SELECT Collection_Unit_Key FROM @ResultsTable
  )

  IF (@@ROWCOUNT > 0)
  BEGIN
  -- If you can :
  --   Go Down 
    SELECT @Parent_Key = @Current_Key, @Current_Key = Collection_Unit_Key
    FROM @ResultsTable WHERE Done = 0 AND Current_Container_Collection_Unit_Key = @Current_Key
    UPDATE @ResultsTable SET OrderValue = @Count WHERE Collection_Unit_Key = @Current_Key
    SELECT @Count = @Count + 1
  END
  ELSE
  BEGIN
  -- If you can''t (this node is done) :
    UPDATE @ResultsTable SET Done = 1 WHERE Collection_Unit_Key = @Current_Key
    SELECT @done = 1
    WHILE (@done = 1 AND @Parent_Key IS NOT NULL)
      SELECT @Current_Key = @Parent_Key, @Parent_Key = Current_Container_Collection_Unit_Key, @done = Done
      FROM @ResultsTable WHERE Collection_Unit_Key = @Parent_Key
  END
END

SELECT COUNT(*) AS Count FROM (
SELECT DISTINCT
  R.OrderValue, 
  CUN.Number AS PrefNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Specimen_Unit SU 
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN @ResultsTable R ON R.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
) AS TempTable
SET NOCOUNT OFF
'
WHERE [Report_Block_In_Section_Key] = 'SYSTEM0000000023'

GO

UPDATE Report_Block_Order
SET Item_Name = 'Pref No', Order_Clause_SQL = 'ORDER BY PrefNo'
WHERE Report_Block_Order_Key = 'SYSTEM0000000013'

GO

UPDATE Report_Block_Order
SET Item_Name = 'Pref No.', Order_Clause_SQL = 'ORDER BY PrefNo'
WHERE Report_Block_Order_Key = 'SYSTEM0000000036'
OR Report_Block_Order_Key = 'SYSTEM000000006U'
OR Report_Block_Order_Key = 'SYSTEM000000007R'

GO

UPDATE [List_Report]
SET [Population_SQL] = '
SELECT
  dbo.ufn_GetPrefNumber(SU.Collection_Unit_Key) AS PrefNo,
  CASE  
    WHEN SU.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, ''No Determination'')
  ELSE
    ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
        ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name,
        ITN.Common_Name_Italic, ITN.Authority, 1), ''No Determination'')
  END AS Determination,
  CTType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  dbo.ufn_GetSpecimenGatheringSite(SU.Collection_Unit_Key) AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(
      S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM  Specimen_Unit AS SU  
INNER JOIN Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key  
INNER JOIN  VW_ConceptTerm AS CTType ON CTType.Concept_Key = SU.Specimen_Type_Concept_Key  
LEFT JOIN  Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN  Occurrence O ON O.Occurrence_Key=D.Determination_Key
LEFT JOIN  VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key  
LEFT JOIN  Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key  
LEFT JOIN  Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=TD.Taxon_Determination_Key
LEFT JOIN  Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key  
LEFT JOIN  [Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)
WHERE  SU.Collection_Unit_Key IN (<#ReportKeys>)
'
WHERE [List_Report_Key] = 'SYSTEM0000000009'

GO

UPDATE [List_Report]
SET [Population_SQL] = '
SELECT
  dbo.ufn_GetPrefNumber(S.Collection_Unit_Key) AS RefNo,
  S.Item_Name AS [Name],
  CTS.PlainText AS Type,
  CU.Current_Location_Code AS Store
FROM Store AS S
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS 
    ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE S.Collection_Unit_Key IN (<#ReportKeys>)
'
WHERE [List_Report_Key] = 'SYSTEM000000000A'

GO