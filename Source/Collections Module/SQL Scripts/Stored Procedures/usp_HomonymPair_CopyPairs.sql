/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_CopyPairs')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_CopyPairs
GO

/*===========================================================================*\
  Description:	This copies all homonym pairs from an old meaning key to a 
				replacement meaning key, for use when changing the meaning key
				of a concept.

  Parameters:	@OldMeaningKey,
				@NewMeaningKey	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_HomonymPair_CopyPairs
	@OldMeaningKey	char(16),
	@NewMeaningKey	char(16)
AS
	DECLARE @HomonymKey char(16)	

	DECLARE keys CURSOR LOCAL FAST_FORWARD FOR
		SELECT 
			CASE 
				WHEN Meaning_Key_1 = @OldMeaningKey THEN Meaning_Key_2
				ELSE Meaning_Key_1
			END as Homonym_Key
		FROM Homonym_Pair
		WHERE Meaning_Key_1 = @OldMeaningKey or Meaning_Key_2 = @OldMeaningKey

		OPEN keys

		WHILE 1=1
		BEGIN
			FETCH keys
			INTO
				@HomonymKey

			IF @@FETCH_STATUS <> 0 BREAK

			EXEC dbo.usp_HomonymPair_Insert_ByMeaning @NewMeaningKey, @HomonymKey

		END
		CLOSE keys
		DEALLOCATE keys
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HomonymPair_CopyPairs') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HomonymPair_CopyPairs'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_CopyPairs TO [Dev - JNCC SQL]
END

GO