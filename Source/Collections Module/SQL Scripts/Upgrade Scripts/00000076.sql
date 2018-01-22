SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeoArea') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGeoArea;
GO

/*============================================================================*\
  Description:	Returns specimens based on their geographic area. Includes 
				matching on symonymns.
  Created: July 2016

  Last revision information:
		$Revision: 2 $
		$Date: 5/07/16 9:59 $
		$Author: Christopherknight $
\*============================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeoArea]
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
	@SortOrderIndex						TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, C3.Published_Term, 
					CASE Life_Sciences
						WHEN 1 THEN
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Taxon_Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
									AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Taxon_Determination_Key
									FOR XML PATH('')
							), 0, 250)
						ELSE
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
									AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Determination_Key
									FOR XML PATH('')
							), 0, 250) 
					END
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key=SE.Survey_Event_Key
	-- Match on all Concept Children as well as Concept itself
	INNER JOIN VW_ConceptChildren CL ON CL.Parent_Concept_Key=SEGA.Concept_Key
	INNER JOIN Concept C2 ON C2.Concept_Key=CL.Child_Concept_Key
	-- Match on Symonyms as well
	INNER JOIN Concept C3 ON C3.Meaning_Key=C2.Meaning_Key
	WHERE C3.Published_Term LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeoArea') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeoArea'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsForm_Select_ForOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsForm_Select_ForOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique session names associated with quick entry 
				sessions appropriate for occurence data.

  Parameters:	@QE_Template_Key
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsForm_Select_ForOccurence]
    @QE_Template_Key char(16),	
    @Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QS.QE_Session_Key,
		QS.Item_Name
FROM QE_Session QS
		INNER JOIN QE_Template QET ON QET.QE_Template_Key=QS.QE_Template_Key
		LEFT JOIN	Domain				AS	Dom
			ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
		LEFT JOIN	Local_Domain		AS	LD
			ON		Dom.Domain_Key		=	LD.Domain_Key
		LEFT JOIN	Concept_Group		AS	CG
			ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
		LEFT JOIN	Concept				AS	C
			ON		CG.Concept_Group_Key =	C.Concept_Group_Key
		LEFT JOIN	Determination		AS	Det
			ON		C.Concept_Key		=	Det.Concept_Key
			AND		Det.Occurrence_Key	=	@Item_Key
		WHERE		QET.Template_Type	=	@SpecimenType
			AND		(
						QET.Subject_Area_Key	IS NULL
						OR
						Det.Determination_Key	IS NOT NULL
					)
			AND  QET.QE_Template_Key = @QE_Template_Key
        ORDER BY QS.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsForm_Select_ForOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsForm_Select_ForOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForOccurence TO [Dev - JNCC SQL]
END

GO
/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsForm_Select_ForTaxonOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsForm_Select_ForTaxonOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique session names associated with quick entry 
				sessions appropriate for taxon occurence data.

  Parameters:	@QE_Template_Key
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsForm_Select_ForTaxonOccurence]
    @QE_Template_Key char(16),
	@Item_Key CHAR(16)
 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QS.QE_Session_Key,
		QS.Item_Name
FROM QE_Session QS
		INNER JOIN QE_Template QET ON QET.QE_Template_Key=QS.QE_Template_Key
		LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		TD.Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE		QET.Template_Type		=	@SpecimenType
		AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
				)
		AND		QET.QE_Template_Key = @QE_Template_Key
    ORDER BY QS.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsForm_Select_ForTaxonOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QESessionsForm_Select_ForTaxonOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Select_ForTaxonOccurence TO [Dev - JNCC SQL]
END

GO
/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Select_ForOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique form names associated with quick entry 
				sessions appropriate for occurence data.

  Parameters:	@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Select_ForOccurence]
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QET.QE_Template_Key,
		QET.Item_Name
FROM QE_Template QET
		LEFT JOIN	Domain				AS	Dom
			ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
		LEFT JOIN	Local_Domain		AS	LD
			ON		Dom.Domain_Key		=	LD.Domain_Key
		LEFT JOIN	Concept_Group		AS	CG
			ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
		LEFT JOIN	Concept				AS	C
			ON		CG.Concept_Group_Key =	C.Concept_Group_Key
		LEFT JOIN	Determination		AS	Det
			ON		C.Concept_Key		=	Det.Concept_Key
			AND		Det.Occurrence_Key	=	@Item_Key
		WHERE		QET.Template_Type	=	@SpecimenType
			AND		(
						QET.Subject_Area_Key	IS NULL
						OR
						Det.Determination_Key	IS NOT NULL
					)
		ORDER BY QET.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsFormNames_Select_ForOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique form names associated with quick entry 
				sessions appropriate for taxon occurence data.

  Parameters:	@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Select_ForTaxonOccurence]
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QET.QE_Template_Key,
		QET.Item_Name
FROM QE_Template QET
		LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		TD.Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE		QET.Template_Type		=	@SpecimenType
		AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
				)
	ORDER BY QET.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsForm_Count_ForOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsForm_Count_ForOccurence;
GO

/*===========================================================================*\
  Description:	Returns a count of unique session names associated with quick entry 
				sessions appropriate for occurence data.

  Parameters:	@Count int OUTPUT,
				@QE_Template_Key
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsForm_Count_ForOccurence]
    @Count int OUTPUT,
    @QE_Template_Key char(16),	
    @Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT @Count=COUNT(DISTINCT QE_Session_Key)
FROM QE_Session QS
		INNER JOIN QE_Template QET 
			ON QET.QE_Template_Key=QS.QE_Template_Key
				AND QET.Template_Type =	@SpecimenType
				AND  QET.QE_Template_Key = @QE_Template_Key
		LEFT JOIN	Domain				AS	Dom
			ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
		LEFT JOIN	Local_Domain		AS	LD
			ON		Dom.Domain_Key		=	LD.Domain_Key
		LEFT JOIN	Concept_Group		AS	CG
			ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
		LEFT JOIN	Concept				AS	C
			ON		CG.Concept_Group_Key =	C.Concept_Group_Key
		LEFT JOIN	Determination		AS	Det
			ON		C.Concept_Key		=	Det.Concept_Key
			AND		Det.Occurrence_Key	=	@Item_Key
		WHERE		--QET.Template_Type	=	@SpecimenType
			--AND		(
						QET.Subject_Area_Key	IS NULL
						OR
						Det.Determination_Key	IS NOT NULL
			--		)
			--AND  QET.QE_Template_Key = @QE_Template_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsForm_Count_ForOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsForm_Count_ForOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsForm_Count_ForTaxonOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsForm_Count_ForTaxonOccurence;
GO

/*===========================================================================*\
  Description:	Returns a count of unique session names associated with quick entry 
				sessions appropriate for taxon occurence data.

  Parameters:	@Count int OUTPUT,
				@QE_Template_Key
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsForm_Count_ForTaxonOccurence]
    @Count int OUTPUT,
    @QE_Template_Key char(16),
	@Item_Key CHAR(16)
 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT @Count=COUNT(DISTINCT QS.QE_Session_Key)
FROM QE_Session QS
	INNER JOIN QE_Template QET 
		ON QET.QE_Template_Key=QS.QE_Template_Key
			AND QET.Template_Type =	@SpecimenType
			AND	QET.QE_Template_Key = @QE_Template_Key
		LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		TD.Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE		--QET.Template_Type		=	@SpecimenType
		--AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
		--		)
		--AND		QET.QE_Template_Key = @QE_Template_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsForm_Count_ForTaxonOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QESessionsForm_Count_ForTaxonOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsForm_Count_ForTaxonOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsFormNames_Count_ForOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Count_ForOccurence;
GO

/*===========================================================================*\
  Description:	Returns a count of unique form names associated with quick entry 
				sessions appropriate for occurence data.

  Parameters:	@Count int OUTPUT,
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Count_ForOccurence]
	@Count int OUTPUT,
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT @Count=COUNT(DISTINCT QET.QE_Template_Key)
FROM QE_Template QET 
		LEFT JOIN	Domain				AS	Dom
			ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
		LEFT JOIN	Local_Domain		AS	LD
			ON		Dom.Domain_Key		=	LD.Domain_Key
		LEFT JOIN	Concept_Group		AS	CG
			ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
		LEFT JOIN	Concept				AS	C
			ON		CG.Concept_Group_Key =	C.Concept_Group_Key
		LEFT JOIN	Determination		AS	Det
			ON		C.Concept_Key		=	Det.Concept_Key
			AND		Det.Occurrence_Key	=	@Item_Key
		WHERE QET.Template_Type = @SpecimenType		
			AND		(
						QET.Subject_Area_Key	IS NULL
						OR
						Det.Determination_Key	IS NOT NULL
					)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Count_ForOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsFormNames_Count_ForOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence;
GO

/*===========================================================================*\
  Description:	Returns a count of unique form names associated with quick entry 
				sessions appropriate for taxon occurence data.

  Parameters:	@Count int OUTPUT,
				@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Count_ForTaxonOccurence]
	@Count int OUTPUT,	
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT @Count=COUNT(DISTINCT QET.QE_Template_Key)
FROM QE_Template QET 
	LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		TD.Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE QET.Template_Type=	@SpecimenType		
		AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
				)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Count_ForTaxonOccurence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence;
GO

/*===========================================================================*\
  Description:	Inserts @NumberOfRows of data into the QE_Data_Row table for
				the specified session.  Populates associated QE_Data_Item rows
				with data for the specified Occurrence.

  Parameters:	@QE_Session_Key
				@Occurrence_Key
				@NumberOfRows
				@SessionID

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForSecondaryOccurrence]
    @QE_Session_Key CHAR(16),
	@Occurrence_Key CHAR(16),
    @NumberOfRows INT,
	@SessionID CHAR(16)
 AS
BEGIN
SET NOCOUNT ON

DECLARE @DataRowIDs TABLE (QE_Data_Row_Key INT)
DECLARE @Occurrence_Temp TABLE (Occurrence_Key CHAR(16))

-- Insert @NumberOfRows rows into the QE_Data_Row table and get their ids.
INSERT INTO QE_Data_Row (QE_Session_Key, General, Validated, Processed, Entered_Session_ID)
OUTPUT inserted.QE_Data_Row_Key INTO @DataRowIDs
SELECT TOP (@NumberOfRows) 
	@QE_Session_Key, 
	0, 
	0, 
	0, 
	@SessionID 
FROM sys.all_columns

-- Insert relevant taxon occurrence for Cross Join to force prevention
-- of large data sets.
INSERT INTO @Occurrence_Temp
VALUES(@Occurrence_Key)

-- Insert N copies of the taxon occurence data
INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Thesaurus Determination'
				THEN Dp.Concept_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN Dp.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN CTRT.Concept_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), Dp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Thesaurus Determination'
				THEN CT_Dp.Plaintext
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN CTRT.PlainText
				WHEN 'Determiners are Inferred'
				THEN 
					CASE Dp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN @DataRowIDs				AS	DRID
	ON DR.QE_Data_Row_Key = DRID.QE_Data_Row_Key
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
CROSS JOIN  @Occurrence_Temp	AS	OT
INNER JOIN  Occurrence		AS	O
	ON OT.Occurrence_Key = O.Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Occurrence_Data					AS	OD
	ON		O.Occurrence_Key				=	OD.Occurrence_Key
			AND	TF.QE_Field_Key				IS	NULL
			AND TF.Is_Custom				=	1
			AND TF.Measurement_Is_Specimen	=	0
			AND TF.Measurement_Is_TaxonData =	0
			AND TF.Measurement_Applies_To	=	OD.Applies_To
			AND	TF.Measurement_Parameter_Concept_Key			= OD.Parameter_Concept_Key
			AND	ISNULL(TF.Measurement_Method_Concept_Key, '')	= ISNULL(OD.Method_Concept_Key, '')
			AND	ISNULL(TF.Measurement_Duration, '')				= ISNULL(OD.Duration, '')
			AND ISNULL(TF.Measurement_Accuracy, '')				= ISNULL(OD.Accuracy, '')
			AND	ISNULL(TF.Measurement_Unit_Concept_Key, '')		= ISNULL(OD.Unit_Concept_Key, '')
LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		O.Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Determinations (preferred)
LEFT JOIN	Determination			AS	Dp
	ON		O.Occurrence_Key	=	Dp.Occurrence_Key
	AND		Dp.Preferred				=	1
LEFT JOIN	VW_ConceptTerm		AS	CT_Dp
	ON		Dp.Concept_Key		=	CT_Dp.Concept_Key

-- Determiner
LEFT JOIN	Individual	AS	I_TDa
	ON		Dp.Determiner_Name_Key = I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		Dp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS	NULL
			AND SE.Location_Key =	LN.Location_Key))
	AND		F.Item_Name			=	'Gathering Site'
	AND		LN.Preferred		=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	VW_ConceptTerm	AS	CTRT
	ON		F.Item_Name				=	'Field Record Type'
	AND		O.Record_Type_Concept_Key	=	CTRT.Concept_Key

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key


END
GO
 
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence;
GO

/*===========================================================================*\
  Description:	Inserts @NumberOfRows of data into the QE_Data_Row table for
				the specified session.  Populates associated QE_Data_Item rows
				with data for the specified Taxon Occurrence.

  Parameters:	@QE_Session_Key
				@Taxon_Occurrence_Key
				@NumberOfRows
				@SessionID

  Created:	June 2016

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence]
    @QE_Session_Key CHAR(16),
	@Taxon_Occurrence_Key CHAR(16),
    @NumberOfRows INT,
	@SessionID CHAR(16)
 AS
BEGIN
SET NOCOUNT ON

DECLARE @DataRowIDs TABLE (QE_Data_Row_Key INT)
DECLARE @Taxon_Occurrence_Temp TABLE (Taxon_Occurrence_Key CHAR(16))

-- Insert @NumberOfRows rows into the QE_Data_Row table and get their ids.
INSERT INTO QE_Data_Row (QE_Session_Key, General, Validated, Processed, Entered_Session_ID)
OUTPUT inserted.QE_Data_Row_Key INTO @DataRowIDs
SELECT TOP (@NumberOfRows) 
	@QE_Session_Key, 
	0, 
	0, 
	0, 
	@SessionID 
FROM sys.all_columns

-- Insert relevant taxon occurrence for Cross Join to force prevention
-- of large data sets.
INSERT INTO @Taxon_Occurrence_Temp
VALUES(@Taxon_Occurrence_Key)

-- Insert N copies of the taxon occurence data
INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Taxon Determination'
				THEN TDp.Taxon_List_Item_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN TDp.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN RT.Record_Type_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), TDp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Taxon Determination'
				THEN T.Item_Name
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN RT.Short_Name
				WHEN 'Determiners are Inferred'
				THEN 
					CASE TDp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN @DataRowIDs				AS	DRID
	ON DR.QE_Data_Row_Key = DRID.QE_Data_Row_Key
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
CROSS JOIN  @Taxon_Occurrence_Temp	AS	TOT
INNER JOIN  Taxon_Occurrence		AS	"TO"
	ON TOT.Taxon_Occurrence_Key = "TO".Taxon_Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Taxon_Occurrence_Data				AS	TOD
	ON		"TO".Taxon_Occurrence_Key			=	TOD.Taxon_Occurrence_Key
	AND		TF.QE_Field_Key						IS	NULL
	AND		TF.Is_Custom						=	1
	AND		TF.Measurement_Is_Specimen			=	0
	AND		TF.Measurement_Is_TaxonData			=	1
	AND		TF.Taxon_Measurement_Qualifier_Key	= TOD.Measurement_Qualifier_Key
	AND		TF.Taxon_Measurement_Unit_Key		= TOD.Measurement_Unit_Key
	AND		TF.Measurement_Accuracy				= TOD.Accuracy	

LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		"TO".Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Taxon Determinations (preferred)
LEFT JOIN	Taxon_Determination			AS	TDp
	ON		"TO".Taxon_Occurrence_Key	=	TDp.Taxon_Occurrence_Key
	AND		TDp.Preferred				=	1
LEFT JOIN	Taxon_List_Item				AS	TLI
	ON		TDp.Taxon_List_Item_Key		=	TLI.Taxon_List_Item_Key
LEFT JOIN	Taxon_Version				AS	TV 
	ON		TLI.Taxon_Version_Key		=	TV.Taxon_Version_Key
LEFT JOIN	Taxon						AS	T
	ON		TV.Taxon_Key				=	T.Taxon_Key

-- Determiner
LEFT JOIN	Individual		AS	I_TDa
	ON		TDp.Determiner	=	I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		TDp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS NULL
			AND SE.Location_Key = LN.Location_Key))
	AND		F.Item_Name		=	'Gathering Site'
	AND		LN.Preferred	=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	Record_Type				AS	RT
	ON		F.Item_Name				=	'Field Record Type'
	AND		"TO".Record_Type_Key	=	RT.Record_Type_Key

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryTaxonOccurrence TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF Object_ID('dbo.usp_QETemplates_Select_ForOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QETemplates_Select_ForOccurrence
GO

/*===========================================================================*\
  Description:
	Selects all of the templates appropriate for the given occurrence.

  Parameters:
	@Item_Key - The key of the occurrence to search templates for.

  Created:	January 2011

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/16 9:59 $
	$Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForOccurrence(
	@Item_Key CHAR(16)
)
AS

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

	SELECT DISTINCT		
				QET.QE_Template_Key,
				QET.Item_Name,
				QET.Template_Type,
				QET.Subject_Area_Key,
				QET."Timestamp"
	FROM		QE_Template			AS	QET
	LEFT JOIN	Domain				AS	Dom
		ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
	LEFT JOIN	Local_Domain		AS	LD
		ON		Dom.Domain_Key		=	LD.Domain_Key
	LEFT JOIN	Concept_Group		AS	CG
		ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
	LEFT JOIN	Concept				AS	C
		ON		CG.Concept_Group_Key =	C.Concept_Group_Key
	LEFT JOIN	Determination		AS	Det
		ON		C.Concept_Key		=	Det.Concept_Key
		AND		Det.Occurrence_Key	=	@Item_Key
	WHERE		QET.Template_Type	=	@SpecimenType
		AND		(
					QET.Subject_Area_Key	IS NULL
					OR
					Det.Determination_Key	IS NOT NULL
				)
	ORDER BY QET.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QETemplates_Select_ForOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [Dev - JNCC SQL]
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF Object_ID('dbo.usp_QETemplates_Select_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QETemplates_Select_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:
	Selects all of the templates appropriate for the given taxon occurrence.

  Parameters:
	@Item_Key

  Created:	January 2011

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/16 9:59 $
	$Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForTaxonOccurrence(
	@Item_Key CHAR(16)
)
AS

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

	SELECT DISTINCT		
				QET.QE_Template_Key,
				QET.Item_Name,
				QET.Template_Type,
				QET.Subject_Area_Key,
				QET."Timestamp"
	FROM		QE_Template		AS	QET
	LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE		QET.Template_Type		=	@SpecimenType
		AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
				)
    ORDER BY QET.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QETemplates_Select_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [Dev - JNCC SQL]
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select_ForFieldData 
@Key CHAR(16)

AS

--  DESCRIPTION
--  Returns all the Geo Areas associated with the survey event.
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				They Survey_Event_Key to search on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--
SET NOCOUNT ON

SELECT
	T.Plaintext,
	T.Term_Key,
	SEGA.Survey_Event_Geo_Area_Key,
	SEGA.Survey_Event_Key,
	C1.Concept_Key,
	SFD.Custodian,
	SFD.[Timestamp]
FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN Concept C ON C.Concept_Key = SEGA.Concept_Key
	INNER JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C1.List_Preferred = 1
	INNER JOIN Term T ON T.Term_Key = C1.Term_Key

WHERE
	SFD.Specimen_Field_Data_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Delete]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Delete]
  @QESessionKey as int,
  @Timestamp as timestamp,
  @SessionID as char(16)
AS

	SET NOCOUNT ON

	SET Xact_abort ON
	BEGIN TRAN

		DELETE FROM QE_Data_Item 
		WHERE QE_Data_Row_key IN 
		(SELECT QE_Data_Row_Key FROM QE_Data_Row 
		WHERE QE_Session_Key = @QESessionKey)

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Data_Row
		WHERE QE_Session_Key = @QESessionKey
	
		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Session
		WHERE QE_Session_Key = @QESessionKey
			AND ([Timestamp] = @Timestamp
				OR Changed_Session_ID = @SessionID
				OR (Changed_Session_ID IS NULL
					AND Entered_Session_ID = @SessionID))

		DECLARE @Error int
		DECLARE @RecordsAffected int

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/16 9:59 $
    $Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Update]
  @QESessionKey as int,
  @QESessionName as varchar(50),
  @Timestamp as timestamp,
  @SessionID as char(16)
AS

SET NOCOUNT OFF

Update QE_Session
	set Item_Name = @QESessionName,
	Changed_Session_ID = @SessionID
	where 
	QE_Session_Key = @QESessionKey 
	AND ([Timestamp] = @Timestamp
		OR Changed_Session_ID = @SessionID
		OR (Changed_Session_ID IS NULL
			AND Entered_Session_ID = @SessionID))

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		RAISERROR('Record updated by another user', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [Dev - JNCC SQL]
END

GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF Object_ID('dbo.usp_QEDataItem_Insert_ForOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForOccurrence
GO

/*===========================================================================*\
  Description:
	Inserts quick entry data items data items using data from the
	occurrence of the given QE session.	

  Parameters:
	@QE_Session_Key - The quick entry session to which the data items are to
						be associated
	@SessionID

  Created:	January 2011

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/16 9:59 $
	$Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QEDataItem_Insert_ForOccurrence(
	@QE_Session_Key CHAR(16),
	@SessionID		CHAR(16)
)
AS

INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,
			
			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Thesaurus Determination'
				THEN Dp.Concept_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN Dp.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN CTRT.Concept_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), Dp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Thesaurus Determination'
				THEN CT_Dp.Plaintext
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN CTRT.PlainText
				WHEN 'Determiners are Inferred'
				THEN 
					CASE Dp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
INNER JOIN	Occurrence				AS	O
	ON		O.Occurrence_Key		=	Sn.Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Occurrence_Data					AS	OD
	ON		O.Occurrence_Key				=	OD.Occurrence_Key
			AND	TF.QE_Field_Key				IS	NULL
			AND TF.Is_Custom				=	1
			AND TF.Measurement_Is_Specimen	=	0
			AND TF.Measurement_Is_TaxonData =	0
			AND TF.Measurement_Applies_To	=	OD.Applies_To
			AND	TF.Measurement_Parameter_Concept_Key			= OD.Parameter_Concept_Key
			AND	ISNULL(TF.Measurement_Method_Concept_Key, '')	= ISNULL(OD.Method_Concept_Key, '')
			AND	ISNULL(TF.Measurement_Duration, '')				= ISNULL(OD.Duration, '')
			AND ISNULL(TF.Measurement_Accuracy, '')				= ISNULL(OD.Accuracy, '')
			AND	ISNULL(TF.Measurement_Unit_Concept_Key, '')		= ISNULL(OD.Unit_Concept_Key, '')
LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		O.Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Determinations (preferred)
LEFT JOIN	Determination			AS	Dp
	ON		O.Occurrence_Key	=	Dp.Occurrence_Key
	AND		Dp.Preferred				=	1
LEFT JOIN	VW_ConceptTerm		AS	CT_Dp
	ON		Dp.Concept_Key		=	CT_Dp.Concept_Key

-- Determiner
LEFT JOIN	Individual	AS	I_TDa
	ON		Dp.Determiner_Name_Key = I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		Dp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS	NULL
			AND SE.Location_Key =	LN.Location_Key))
	AND		F.Item_Name			=	'Gathering Site'
	AND		LN.Preferred		=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	VW_ConceptTerm	AS	CTRT
	ON		F.Item_Name				=	'Field Record Type'
	AND		O.Record_Type_Concept_Key	=	CTRT.Concept_Key

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key

-- TODO: Possibly need another update section (or alteration to the update
--		above) to ensure that the position values of corresponding determiners
--		and determiner roles are equal.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [Dev - JNCC SQL]
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF Object_ID('dbo.usp_QEDataItem_Insert_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:
	Inserts quick entry data items data items using data from the
	taxon occurrence of the given QE session.	

  Parameters:
	@QE_Session_Key - The quick entry session to which the data items are to
						be associated
	@SessionID

  Created:	January 2011

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/16 9:59 $
	$Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QEDataItem_Insert_ForTaxonOccurrence(
	@QE_Session_Key CHAR(16),
	@SessionID		CHAR(16)
)
AS

INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,
			
			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Taxon Determination'
				THEN TDp.Taxon_List_Item_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN TDp.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN RT.Record_Type_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), TDp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Taxon Determination'
				THEN T.Item_Name
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN RT.Short_Name
				WHEN 'Determiners are Inferred'
				THEN 
					CASE TDp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
INNER JOIN	Taxon_Occurrence		AS	"TO"
	ON		"TO".Taxon_Occurrence_Key = Sn.Taxon_Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Taxon_Occurrence_Data				AS	TOD
	ON		"TO".Taxon_Occurrence_Key			=	TOD.Taxon_Occurrence_Key
	AND		TF.QE_Field_Key						IS	NULL
	AND		TF.Is_Custom						=	1
	AND		TF.Measurement_Is_Specimen			=	0
	AND		TF.Measurement_Is_TaxonData			=	1
	AND		TF.Taxon_Measurement_Qualifier_Key	= TOD.Measurement_Qualifier_Key
	AND		TF.Taxon_Measurement_Unit_Key		= TOD.Measurement_Unit_Key
	AND		TF.Measurement_Accuracy				= TOD.Accuracy	

LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		"TO".Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Taxon Determinations (preferred)
LEFT JOIN	Taxon_Determination			AS	TDp
	ON		"TO".Taxon_Occurrence_Key	=	TDp.Taxon_Occurrence_Key
	AND		TDp.Preferred				=	1
LEFT JOIN	Taxon_List_Item				AS	TLI
	ON		TDp.Taxon_List_Item_Key		=	TLI.Taxon_List_Item_Key
LEFT JOIN	Taxon_Version				AS	TV 
	ON		TLI.Taxon_Version_Key		=	TV.Taxon_Version_Key
LEFT JOIN	Taxon						AS	T
	ON		TV.Taxon_Key				=	T.Taxon_Key

-- Determiner
LEFT JOIN	Individual		AS	I_TDa
	ON		TDp.Determiner	=	I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		TDp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS NULL
			AND SE.Location_Key = LN.Location_Key))
	AND		F.Item_Name		=	'Gathering Site'
	AND		LN.Preferred	=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	Record_Type				AS	RT
	ON		F.Item_Name				=	'Field Record Type'
	AND		"TO".Record_Type_Key	=	RT.Record_Type_Key 

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key

-- TODO: Possibly need another update section (or alteration to the update
--		above) to ensure that the position values of corresponding determiners
--		and determiner roles are equal.

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [Dev - JNCC SQL]
GO