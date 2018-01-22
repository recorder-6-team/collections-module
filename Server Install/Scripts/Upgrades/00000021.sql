SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Number from the Collection_Unit_Number table.

  Parameters:	@Key	Collection unit key
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Get]
	@Key char(16),
	@Number varchar(30) OUTPUT
AS
	SELECT 	@Number = Number
	FROM	Collection_Unit_Number
	WHERE	Collection_Unit_Key = @Key
	AND	Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Collection Unit Number table.
  Parameters:	@Key 
		@CollectionUnitKey 
		@Number 
		@TypeConceptKey 
		@Preferred
		@Notes 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred bit,
	@Notes text,
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Collection_Unit_Number', @Key OUTPUT

	BEGIN TRANSACTION
		-- Make sure every other Collection_Unit_Number record for this Collection unit record and 
		-- this this type are set to not preferred.
		IF @Preferred = 1 BEGIN
			UPDATE 	Collection_Unit_Number
			SET 	Preferred = 0
			WHERE 	Collection_Unit_Key = @CollectionUnitKey
		
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		-- If there aren't any records that are preferred, make sure this one is set to preferred.
		IF (SELECT COUNT(Collection_Unit_Number_Key)
			FROM 	Collection_Unit_Number
			WHERE 	Collection_Unit_Key = @CollectionUnitKey
			AND	Type_Concept_Key = @TypeConceptKey	
			AND	Preferred = 1) = 0 
		SET @Preferred = 1	
	
		INSERT INTO Collection_Unit_Number (
			Collection_Unit_Number_Key,
			Collection_Unit_Key,
			Number,
			Type_Concept_Key,
			Preferred,
			Notes,
			Entered_Session_ID
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@Number,
			@TypeConceptKey,
			@Preferred,
			@Notes,
			@SessionID )

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Collection Unit Number table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred bit,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	/*---------------------------*\
	  Actually updates the table
	\*---------------------------*/
	BEGIN TRANSACTION

		IF @Preferred = 1
			UPDATE 	Collection_Unit_Number
			SET	Preferred = 0
			WHERE	Collection_Unit_Key = @CollectionUnitKey
			AND	Collection_Unit_Number_Key <> @Key -- So we don't get timestamp problems.
		
		UPDATE 	Collection_Unit_Number
		SET 	Collection_Unit_Key= @CollectionUnitKey,
			Number = @Number,
			Type_Concept_Key = @TypeConceptKey,
			Preferred = @Preferred,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Collection_Unit_Number_Key = @Key
		AND	(@Timestamp = Timestamp)

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitPreferredNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitPreferredNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the preferred number for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Number	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitPreferredNumber_Get]
	@Key char(16),
	@Number varchar(100) OUTPUT
AS
	
	SELECT	TOP 1 	@Number = ISNULL(CN.Number, 'Unknown')
	FROM		Collection_Unit CU
	INNER JOIN Collection_Unit_Number CN ON CN.Collection_Unit_Key=CU.Collection_Unit_Key
	INNER JOIN Concept C ON C.Concept_Key = CN.Type_Concept_Key
	WHERE		CU.Collection_Unit_Key = @Key
	AND CN.Preferred=1

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitPreferredNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitPreferredNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
GO

/*===========================================================================*\
  Description:	Returns informatino about the specified specimens for a 
		Specimens Mail Merge Output report.

  Parameters:	Use #SpecimensMailMergeKeys as source.

  Created:	September 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
AS

	SELECT		dbo.ufn_GetPrefNumber(specimen.Collection_Unit_Key) AS RegNo, 
			CASE WHEN specimen.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, 'No Determination') 
			ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams( 
				ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
				ITN.Common_Name_Italic, ITN.Authority, 1), 'No Determination') 
			END AS Determination, 
			CASE WHEN specimen.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, 'No Determination') 
			ELSE ISNULL(ITN.Actual_Name, 'No Determination') 
			END AS PlainDetermination, 
			dbo.ufn_GetFieldCollectors(specimen.Collection_Unit_Key) AS FieldCollector, 
			dbo.ufn_GetSpecimenGatheringSite(specimen.Collection_Unit_Key) AS GatheringSite, 
			dbo.ufn_GetDateFromVagueDate( 
			S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate 

	FROM  		Specimen_Unit AS specimen 
	LEFT JOIN 	Determination D ON D.Determination_Key = specimen.Preferred_Determination_Key 
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key = D.Determination_Key 
	LEFT JOIN 	VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON specimen.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key 
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key = TD.Taxon_Determination_Key 
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key 
	LEFT JOIN 	[Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key) 

	WHERE		specimen.Collection_Unit_Key IN (SELECT	Specimen_Unit_Key FROM #TempSpecimenKeys)

	ORDER BY	PlainDetermination

	FOR XML AUTO
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensMailMerge_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredNumber]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Preferred Number.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredNumber] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(30),
	@SortOrderIndex TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL
AS

--  DESCRIPTION
--  Returns top level Specimens data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
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
DECLARE @Search TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

IF @Key IS NULL
	IF object_id('tempdb..#TempFilter') is not null
		-- Display data for a list of keys in the #TempFilter table
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN #TempFilter ON #TempFilter.ItemKey=CU.Collection_Unit_Key
	ELSE
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
ELSE
	-- Display data for a single key
	INSERT INTO 
		@Search (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE SU.Collection_Unit_Key=@Key

UPDATE @Search
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @Search SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	AND SDE.Preferred_Determination_key=SDE.Determination_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @Search
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @Search SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
	AND SDL.Preferred_Taxon_Determination_key=SDL.Taxon_Determination_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @Search
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @Search
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByPreferredNumber]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByPreferredNumber]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Registration number to match on

  Created:		September 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:51 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByPreferredNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150),
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Preferred = 1
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	AND			CUN.Number LIKE @SearchText + '%'
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByPreferredNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByPreferredNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByPreferredNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByPreferredNumber]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByPreferredNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Store Registration Number
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByPreferredNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByPreferredNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByPreferredNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Stores data to the CollectionsBrowser
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:		    2003-08-15
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Collection_Unit_Key FROM Store

IF @SortOrderIndex = 0
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY S.Item_Name, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Number, S.Item_Name
END
ELSE IF @SortOrderIndex = 2
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Current_Location_Code AS Hint
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Current_Location_Code, S.Item_Name, Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

