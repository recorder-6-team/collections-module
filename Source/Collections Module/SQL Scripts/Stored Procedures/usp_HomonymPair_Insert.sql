/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Insert')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Insert
GO

/*===========================================================================*\
  Description:	Inserts an entry to the HomonymPair table.

  Parameters:	@Concept_Key_1	The key of the first concept of the
								homonym pair.
				@Concept_Key_2	The key of the second concept of the
								homonym pair. If the pair is being created
								via drag-and-drop, this should be the key
								of the dragged concept.

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_HomonymPair_Insert
	@Concept_Key_1	char(16),
	@Concept_Key_2	char(16)
AS
	DECLARE	@Meaning_Key_1	CHAR(16),
			@Meaning_Key_2	CHAR(16),
			@NewMeaningKey	CHAR(16),
			@Min_Key		CHAR(16),
			@Max_Key		CHAR(16)

	SELECT @Meaning_Key_1 = Meaning_Key
	FROM Concept
	WHERE Concept_Key = @Concept_Key_1

	SELECT @Meaning_Key_2 = Meaning_Key
	FROM Concept
	WHERE Concept_Key = @Concept_Key_2

	--In the case of dragging a synonym into the All Known Homonyms node, the meaning
	--keys will be the same, so a new meaning key should be given to the dragged concept.
	--Note that homonym pair for this concept are copied over for the new meaning key.
	IF @Meaning_Key_1 = @Meaning_Key_2
	BEGIN
		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT


		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@NewMeaningKey)

		EXEC dbo.usp_HomonymPair_CopyPairs @Meaning_Key_2, @NewMeaningKey

		UPDATE	Concept
		SET 	Meaning_Key = @NewMeaningKey
		WHERE	Concept_Key = @Concept_Key_2

		SET @Meaning_Key_2 = @NewMeaningKey
	END

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