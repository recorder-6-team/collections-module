/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Insert]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Inserts a record into Survey_Event_Geo_Area table

  Parameters:	@Key OUTPUT, the primary key of the new record,
				@SurveyEventKey, the survey event key of the record
				@ConceptKey, the concept key of the record
				@EnteredSessionID, the session the record was entered

  Created:	Sep 2007

  Last revision information:
    $Revision: 2 $
    $Date: 28/11/07 16:08 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Insert 
@Key CHAR(16) OUTPUT,
@SurveyEventKey CHAR(16),
@ConceptKey CHAR(16),
@EnteredSessionID CHAR(16)

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	--find out whether this Survey Event Geo Area already exists
	SET @Key = NULL
	SELECT	@Key = Survey_Event_Geo_Area_Key 
	FROM 	Survey_Event_Geo_Area
	WHERE	Concept_Key = @ConceptKey
	AND	Survey_Event_Key = @SurveyEventKey
	
	IF @Key IS NULL
	BEGIN
		BEGIN TRANSACTION
			EXECUTE spNextKey 'Survey_Event_Geo_Area', @Key OUTPUT

			INSERT INTO Survey_Event_Geo_Area (
				Survey_Event_Geo_Area_Key,
				Survey_Event_Key,
				Concept_Key,
				Entered_Session_ID,
				System_Supplied_Data
			) VALUES (
				@Key,
				@SurveyEventKey,
				@ConceptKey,
				@EnteredSessionID,
				0
			)

			IF @@Error <> 0 GOTO RollBackAndExit

		COMMIT TRANSACTION
	END

	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [Dev - JNCC SQL]
END

GO


