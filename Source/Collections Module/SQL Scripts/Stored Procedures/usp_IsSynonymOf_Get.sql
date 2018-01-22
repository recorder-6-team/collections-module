/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IsSynonymOf_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IsSynonymOf_Get]
GO

/*===========================================================================*\
  Description:	Takes two Concept keys and sees if they are synonyms.

  Parameters:	@Key1
		@Key2
		@IsSynonym	Output

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 13/04/04 11:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IsSynonymOf_Get]
	@Key1 char(16),
	@Key2 char(16),
	@IsSynonym bit OUTPUT 
AS
	SELECT 		@IsSynonym = CASE WHEN Count(*) > 0 
					THEN 1
					ELSE 0
					END
	FROM 		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
					AND C2.Concept_Key = @Key2
	WHERE		C1.Concept_Key = @Key1

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IsSynonymOf_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IsSynonymOf_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IsSynonymOf_Get TO [Dev - JNCC SQL]
END

GO