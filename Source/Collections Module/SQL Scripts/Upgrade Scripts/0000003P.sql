SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

-- Add the new column Is_Current, but only if it's not there already.
IF NOT EXISTS
(
	SELECT		*
	FROM		syscolumns
	WHERE		Name			=		N'Is_Current'
	AND			id				=		Object_ID(N'dbo.Specimen_Label')
)
BEGIN
	ALTER TABLE	dbo.Specimen_Label
	ADD Is_Current BIT NOT NULL DEFAULT 0
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Select]
GO

/*===========================================================================*\
  Description:	Returns a Specimen Label record.

  Parameters:	@Key	Specimen Label key

  Created:	Setember 2003

  Last revision information:
    $Revision: 8 $
    $Date: 22.01.10 16:11 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Select]
	@Key char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		S.Specimen_Label_Key,
				S.Collection_Unit_Key,
				S.Is_Inscription,
				S.Label_Position,
				S.Inscription,
				S.Translated,
				S.Translated_Language_Key,
				L.Language_Key + ' - ' + L.Item_Name		AS	Language,
				S.Comments,
				S.Author_Name_Key,
				dbo.ufn_GetFormattedName(S.Author_Name_Key)	AS	AuthorName,
				S.Inferred_Author,
				S.Confidence_Concept_Key,
				TConfidence.Item_Name						AS	Confidence_Name,
				S.Entered_Session_ID,
				S.Changed_Session_ID,
				S.Custodian,
				S.[Timestamp],
				S.Is_Current
	FROM		Specimen_Label								AS	S
	LEFT JOIN 	Language									AS	L
	ON			L.Language_Key								=	S.Translated_Language_Key 
	LEFT JOIN	VW_ConceptTerm								AS	TConfidence
	ON			TConfidence.Concept_Key						=	S.Confidence_Concept_Key
	WHERE 		Specimen_Label_Key							=	@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 8 $
    $Date: 22.01.10 16:11 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100) = NULL,		-- Because called through QuickEntry with no value.
	@Inscription nText,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@InferredAuthor tinyint,
	@Comments text,
	@AuthorNameKey char(16) = NULL,
	@ConfidenceConceptKey char(16),
	@SessionID char(16),
	@IsCurrent bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Specimen_Label', @Key OUTPUT

	BEGIN TRANSACTION	
		INSERT INTO		Specimen_Label
						(
							Specimen_Label_Key,
							Collection_Unit_Key,
							Is_Inscription,
							Label_Position,
							Inscription,
							Translated,
							Translated_Language_Key,
							Inferred_Author,
							Comments,
							Author_Name_Key,
							Confidence_Concept_Key,
							Entered_Session_ID,
							Is_Current
						)
		VALUES			(
							@Key,
							@CollectionUnitKey,
							@IsInscription,
							@Position,
							@Inscription, 
							@Translated,
							@LanguageConceptKey,
							IsNull(@InferredAuthor, 0),
							@Comments,
							@AuthorNameKey,
							@ConfidenceConceptKey,
							@SessionID,
							@IsCurrent
						)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Specimen Label table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 22.01.10 16:11 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100),
	@Inscription ntext,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@Comments text,
	@InferredAuthor tinyint,
	@AuthorNameKey char(16),
	@ConfidenceConceptKey char(16),
	@SessionID char(16),
	@Timestamp timestamp,
	@IsCurrent bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE		Specimen_Label
		SET			Collection_Unit_Key		=	@CollectionUnitKey,
					Is_Inscription			=	@IsInscription,
					Label_Position			=	@Position,
					Inscription				=	@Inscription,
					Translated				=	@Translated,
					Translated_Language_Key	=	@LanguageConceptKey,
					Comments				=	@Comments,
					Inferred_Author			=	@InferredAuthor,
					Author_Name_Key			=	@AuthorNameKey,
					Confidence_Concept_Key	=	@ConfidenceConceptKey,
					Changed_Session_ID		=	@SessionID,
					Is_Current				=	@IsCurrent
		WHERE		Specimen_Label_Key		=	@Key
		AND			[Timestamp]				=	@Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Label WHERE Specimen_Label_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_CreateSpecimenLabel') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensMailMerge_CreateSpecimenLabel]
GO

/*===========================================================================*\
  Description:	Creates a new label record for this specimen.

  Parameters:	Collection_Unit_Key,
				SessionID

  Created:	November 2009

  Last revision information:
    $Revision: 8 $
    $Date: 22.01.10 16:11 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensMailMerge_CreateSpecimenLabel]
(
	@Collection_Unit_Key CHAR(16),
	@SessionID CHAR(16)
)
AS
	DECLARE @Specimen_Label_Key CHAR(16)
	
	DECLARE @Inscription VARCHAR(1024)
	
	SET @Inscription = 'Registration Number: ' +
			dbo.ufn_GetPrefNumber(@Collection_Unit_Key) + CHAR(13) + CHAR(10) +
			'Determinations:' + CHAR(9)
	
	DECLARE @Determinations TABLE
	(
		RowNumber INT IDENTITY(1, 1),
		Domain VARCHAR(100),
		Determination VARCHAR(100)
	)

	INSERT INTO		@Determinations (Domain, Determination)
	SELECT			DOM.Item_Name											AS	Domain,
					CASE
						WHEN SU.Life_Sciences = 0
						THEN ISNULL(CT.Item_Name, 'No Determination') 
						ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Actual_Name, ITN.Actual_Name_Italic,
								ITN.Common_Name, ITN.Common_Name_Italic,
								ITN.Authority, 1), 'No Determination') 
					END														AS	Determination 
	FROM			dbo.Determination										AS	D
	INNER JOIN		Specimen_Unit											AS	SU
	ON				D.Specimen_Collection_Unit_Key							=	SU.Collection_Unit_Key
	LEFT JOIN		Taxon_Determination										AS	TD
	ON				SU.Collection_Unit_Key									=	TD.Specimen_Collection_Unit_Key
	LEFT JOIN		Index_Taxon_Name										AS	ITN
	ON				ITN.Taxon_List_Item_Key									=	TD.Taxon_List_Item_Key
	INNER JOIN 		VW_ConceptTerm											AS	CT
	ON				CT.Concept_Key											=	D.Concept_Key
	INNER JOIN		Concept_Group											AS  g
    ON				g.Concept_Group_Key										=   CT.Concept_Group_Key
    INNER JOIN		Local_Domain											AS  l
    ON				l.Local_Domain_Key										=   g.Local_Domain_Key
    INNER JOIN		Domain													AS  DOM
    ON				DOM.Domain_Key											=   l.Domain_Key
	WHERE			D.Specimen_Collection_Unit_Key							=	@Collection_Unit_Key
	ORDER BY		D.Preferred DESC

	DECLARE @DeterminationCount INT
	SET @DeterminationCount = (SELECT COUNT(*) FROM @Determinations)
	DECLARE @i INT
	SET @i = 1
	
	WHILE @i < @DeterminationCount + 1
	BEGIN
		IF @i = 1
		BEGIN
			SET @Inscription = @Inscription + '*' +
					(SELECT Domain FROM @Determinations WHERE RowNumber = @i) + ' - ' +
					(SELECT Determination FROM @Determinations WHERE RowNumber = @i)
		END
		ELSE
			SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + CHAR(9) + CHAR(9) +
					(SELECT Domain FROM @Determinations WHERE RowNumber = @i) + ' - ' +
					(SELECT Determination FROM @Determinations WHERE RowNumber = @i)
		SET @i = @i + 1
	END
	
	DECLARE @Collectors TABLE
	(
		RowNumber INT IDENTITY(1, 1),
		FullName VARCHAR(51)
	)

	INSERT INTO		@Collectors (FullName)
	SELECT			I.Surname + CASE
									WHEN I.Forename IS NULL
									THEN ''
									ELSE ', ' + I.Forename
								END									AS	FullName
	FROM			Specimen_Field_Data								AS	SFD
	LEFT JOIN		Occurrence										AS	O
	ON				O.Occurrence_Key								=	SFD.Occurrence_Key
	LEFT JOIN		Taxon_Occurrence								AS	XO
	ON				XO.Taxon_Occurrence_Key							=	SFD.Taxon_Occurrence_Key
	INNER JOIN		Sample_Recorder									AS	SR
	ON				(SR.Sample_Key									=	O.Sample_Key
	OR				SR.Sample_Key									=	XO.Sample_Key)
	INNER JOIN		Survey_Event_Recorder							AS	SER
	ON				SER.SE_Recorder_Key								=	SR.SE_Recorder_Key
	INNER JOIN		dbo.Individual										AS	I
	ON				I.Name_Key										=	SER.Name_Key
	WHERE 			SFD.Collection_Unit_Key							=	@Collection_Unit_Key
	AND 			SFD.Gathering_Event								=	1
	ORDER BY		I.Surname ASC	

	DECLARE @CollectorCount INT
	SET @CollectorCount = (SELECT COUNT(*) FROM @Collectors)
	SET @i = 1
	
	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + 'Field Collectors: '

	WHILE @i < @CollectorCount + 1
	BEGIN
		IF @i > 1
			SET @Inscription = @Inscription + '; '

		SET @Inscription = @Inscription + (SELECT FullName FROM @Collectors WHERE RowNumber = @i)
		
		SET @i = @i + 1
	END
	
	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) + 'Gathering Site: ' +
			ISNULL(dbo.ufn_GetSpecimenGatheringSite(@Collection_Unit_Key), '')

	DECLARE @DateGathered VARCHAR(50)
	EXEC usp_SpecimenDate_Get @Collection_Unit_Key, 'dd/MM/yy', @DateGathered OUTPUT	

	SET @Inscription = @Inscription + CHAR(13) + CHAR(10) +
			'Gathering Date: ' + @DateGathered

	BEGIN TRANSACTION	

		EXECUTE spNextKey 'Specimen_Label', @Specimen_Label_Key OUTPUT

		INSERT INTO		dbo.Specimen_Label
						(
							Specimen_Label_Key,
							Collection_Unit_Key,
							Is_Inscription,
							Label_Position,
							Inscription,
							Translated,
							Translated_Language_Key,
							Comments,
							Author_Name_Key,
							Inferred_Author,
							Confidence_Concept_Key,
							Entered_Session_ID,
							Changed_Session_ID,
							Custodian,
							Is_Current
						)
		VALUES			(
							@Specimen_Label_Key,
							@Collection_Unit_Key,
							0,
							NULL,
							@Inscription,
							NULL,
							NULL,
							'Label exported via Specimen Mail Merge',
							NULL,
							0,
							NULL,
							@SessionID,
							NULL,
							NULL,
							1
						)
							
	COMMIT TRANSACTION					
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_CreateSpecimenLabel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensMailMerge_CreateSpecimenLabel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_CreateSpecimenLabel TO [Dev - JNCC SQL]
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_GetListName') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonListItem_GetListName
GO

/*============================================================================*\
  Description:

  Parameters:

  Created:

  Last revision information:
	$Revision: 8 $
	$Date: 22.01.10 16:11 $
	$Author: Simonlewis $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_TaxonListItem_GetListName
	@TaxonListItemKey					CHAR(16),
	@TaxonListName						VARCHAR(200) OUTPUT
AS
	SELECT		@TaxonListName				=	l.ITEM_NAME
	FROM		TAXON_LIST_ITEM				AS	i
	INNER JOIN	TAXON_LIST_VERSION			AS	lv
	ON			lv.TAXON_LIST_VERSION_KEY	=	i.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_LIST					AS	l
	ON			l.TAXON_LIST_KEY			=	lv.TAXON_LIST_KEY
	WHERE		i.TAXON_LIST_ITEM_KEY		=	@TaxonListItemKey
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_GetListName') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_TaxonListItem_GetListName'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_GetListName TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_DeleteForMultiValues') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_DeleteForMultiValues
GO

/*============================================================================*\	
  Description:

  Parameters:

  Created:		November 2009

  Last revision information:
	$Revision: 8 $
	$Date: 22.01.10 16:11 $
	$Author: Simonlewis $		
\*============================================================================*/
CREATE PROCEDURE dbo.usp_QEDataItem_DeleteForMultiValues
	@DataRowKey							INT,
	@TemplateFieldKey					CHAR(16)
AS
	DELETE FROM	dbo.QE_Data_Item
	WHERE		QE_Data_Row_Key			=	@DataRowKey
	AND			QE_Template_Field_Key	=	@TemplateFieldKey
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_DeleteForMultiValues') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_QEDataItem_DeleteForMultiValues'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Concept_GetItemNameAndGroup') IS NOT NULL
	DROP PROCEDURE dbo.usp_Concept_GetItemNameAndGroup
GO

/*============================================================================*\
  Description:	Returns the actual name and group name of a concept.

  Parameters:	@Key					Concept key
				
  Created:		November 2009

  Last revision information:
	$Revision: 8 $
	$Date: 22.01.10 16:11 $
	$Author: Simonlewis $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Concept_GetItemNameAndGroup
	@Key								CHAR(16)
AS
	SELECT		dbo.ufn_ConceptItemName_Get(
						@Key,
						0,
						0,
						0)				AS	Concept_Name,
				g.Item_Name				AS	Group_Name
	FROM		Concept					AS	c
	INNER JOIN	Concept_Group			AS	g
	ON			g.Concept_Group_Key		=	c.Concept_Group_Key
	WHERE		c.Concept_Key			=	@Key
GO

/*============================================================================*\
  Grant permissions
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Concept_GetItemNameAndGroup') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_GetItemNameAndGroup'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GetItemNameAndGroup TO R2k_RecordCardsOnly
END
GO