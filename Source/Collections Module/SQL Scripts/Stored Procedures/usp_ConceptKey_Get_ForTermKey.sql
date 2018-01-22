If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptKey_Get_ForTermKey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptKey_Get_ForTermKey]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_ConceptKey_Get_ForTermKey 
	@Key char(16),
	@ConceptKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Term_Key of the concept
--  @SurveyEventKey		The Concept_Key of the concept
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@ConceptKey = Concept_Key
	FROM
	Concept
	WHERE	Term_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptKey_Get_ForTermKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptKey_Get_ForTermKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [Dev - JNCC SQL]
END

GO


