/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Insert_ByMeaning')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Insert_ByMeaning
GO

/*===========================================================================*\
  Description:	Inserts an entry to the HomonymPair table.

  Parameters:	@Meaning_Key_1,
				@Meaning_Key_2	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_HomonymPair_Insert_ByMeaning
	@Meaning_Key_1	char(16),
	@Meaning_Key_2	char(16)
AS
	DECLARE	@Min_Key		CHAR(16),
			@Max_Key		CHAR(16)

	SET	@Min_Key =	CASE 
						WHEN @Meaning_Key_1 < @Meaning_Key_2 THEN
								@Meaning_Key_1
						ELSE @Meaning_Key_2
					END
	SET	@Max_Key =	CASE 
						WHEN @Meaning_Key_1 > @Meaning_Key_2 THEN
								@Meaning_Key_1
						ELSE @Meaning_Key_2
					END
	
	IF NOT EXISTS (SELECT * 
					FROM Homonym_Pair
					WHERE Meaning_Key_1 = @Min_Key and Meaning_Key_2 = @Max_Key)
	BEGIN
		INSERT INTO Homonym_Pair(Meaning_Key_1, Meaning_Key_2)
		VALUES (@Min_Key, @Max_Key)
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HomonymPair_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HomonymPair_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HomonymPair_Insert TO [Dev - JNCC SQL]
END

GO