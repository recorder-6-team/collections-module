/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Delete')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Delete
GO

/*===========================================================================*\
  Description:	Deletes an entry from the HomonymPair table.

  Parameters:	@Concept_Key_1,
				@Concept_Key_2	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_HomonymPair_Delete
	@Concept_Key_1	char(16),
	@Concept_Key_2	char(16)
AS
	DECLARE	@Meaning_Key_1	CHAR(16),
			@Meaning_Key_2	CHAR(16)

	SELECT @Meaning_Key_1 = Meaning_Key
	FROM Concept
	WHERE Concept_Key = @Concept_Key_1

	SELECT @Meaning_Key_2 = Meaning_Key
	FROM Concept
	WHERE Concept_Key = @Concept_Key_2

	DELETE FROM Homonym_Pair
	WHERE (Meaning_Key_1 = @Meaning_Key_1 and Meaning_Key_2 = @Meaning_Key_2)
	OR (Meaning_Key_1 = @Meaning_Key_2 and Meaning_Key_2 = @Meaning_Key_1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HomonymPair_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HomonymPair_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_Delete TO [Dev - JNCC SQL]
END

GO