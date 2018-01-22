If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_Get] 
@Key CHAR(16),
@GetPublishedTerm BIT = 0,
@Caption VARCHAR(150) OUTPUT

AS

--  DESCRIPTION
--  Returns a Concept caption given the concept key
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Key of record to be returned
--	@Caption			Output caption
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-11-03
--
SET NOCOUNT ON

SELECT @Caption = CASE
					WHEN @GetPublishedTerm = 0 THEN PlainText
					ELSE Item_Name
				  END
FROM
VW_CONCEPTTERM
WHERE Concept_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Get TO [Dev - JNCC SQL]
END

GO
