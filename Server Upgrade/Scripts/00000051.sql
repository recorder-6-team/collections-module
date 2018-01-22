/*===========================================================================*\
	Create Homonym_Pair table
\*===========================================================================*/
IF NOT EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.Homonym_Pair'))
BEGIN  
	CREATE TABLE dbo.Homonym_Pair (
		Meaning_Key_1 CHAR(16) NOT NULL
			CONSTRAINT FK_Homonym_Pair_Meaning_1 FOREIGN KEY
			REFERENCES dbo.Meaning (Meaning_Key),
		Meaning_Key_2 CHAR(16) NOT NULL
			CONSTRAINT FK_Homonym_Pair_Meaning_2 FOREIGN KEY
			REFERENCES dbo.Meaning (Meaning_Key)
	)

	ALTER TABLE dbo.Homonym_Pair ADD CONSTRAINT PK_Homonym_Pair PRIMARY KEY CLUSTERED 
	(
		Meaning_Key_1 ASC,
		Meaning_Key_2 ASC
	)

	ALTER TABLE dbo.Homonym_Pair ADD CONSTRAINT CK_Homonym_Pair_Order CHECK
	(
		Meaning_Key_1 < Meaning_Key_2
	)

	/*===========================================================================*\
	  Grant permissions.
	\*===========================================================================*/
	IF OBJECT_ID('dbo.Homonym_Pair') IS NOT NULL
	BEGIN
    	PRINT 'Setting up security on table Homonym_Pair'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT SELECT ON dbo.Homonym_Pair TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		BEGIN
			GRANT SELECT ON dbo.Homonym_Pair TO [R2k_Administrator]
			GRANT UPDATE ON dbo.Homonym_Pair TO [R2k_Administrator]
			GRANT INSERT ON dbo.Homonym_Pair TO [R2k_Administrator]
			GRANT DELETE ON dbo.Homonym_Pair TO [R2k_Administrator]
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		BEGIN
			GRANT SELECT ON dbo.Homonym_Pair TO R2k_FullEdit
			GRANT UPDATE ON dbo.Homonym_Pair TO R2k_FullEdit
			GRANT INSERT ON dbo.Homonym_Pair TO R2k_FullEdit
			GRANT DELETE ON dbo.Homonym_Pair TO R2k_FullEdit
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT SELECT ON dbo.Homonym_Pair TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT SELECT ON dbo.Homonym_Pair TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		BEGIN
			GRANT SELECT ON dbo.Homonym_Pair TO [Dev - JNCC SQL]
			GRANT UPDATE ON dbo.Homonym_Pair TO [Dev - JNCC SQL]
			GRANT INSERT ON dbo.Homonym_Pair TO [Dev - JNCC SQL]
			GRANT DELETE ON dbo.Homonym_Pair TO [Dev - JNCC SQL]
		END
	END
END

/*===========================================================================*\
	Create Homonym_Pair table
\*===========================================================================*/
IF NOT EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.Concept_Group_Quality_Check'))
BEGIN
	CREATE TABLE dbo.Concept_Group_Quality_Check (
		Concept_Group_Quality_Check_Key CHAR(16) NOT NULL 
			CONSTRAINT PK_Concept_Group_Quality_Check PRIMARY KEY,	
		Concept_Group_Key CHAR(16) NOT NULL
			CONSTRAINT FK_Concept_Group_Quality_Check_Concept_Group FOREIGN KEY
			REFERENCES dbo.Concept_Group (Concept_Group_Key),
		Checked_Date_Time SMALLDATETIME NOT NULL,
		Checked_By_User CHAR(16) NOT NULL
			CONSTRAINT FK_Concept_Group_Quality_Check_User FOREIGN KEY
			REFERENCES dbo."User"(Name_Key)
	)

	CREATE NONCLUSTERED INDEX IX_Concept_Group_Quality_Check_Concept_Group_Key_Checked_Date_Time
	ON dbo.Concept_Group_Quality_Check
	(
		Concept_Group_Key ASC,
		Checked_Date_Time ASC
	)

	/*===========================================================================*\
	  Grant permissions.
	\*===========================================================================*/
	IF OBJECT_ID('dbo.Concept_Group_Quality_Check') IS NOT NULL
	BEGIN
    	PRINT 'Setting up security on table Concept_Group_Quality_Check'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT SELECT ON dbo.Concept_Group_Quality_Check TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		BEGIN
			GRANT SELECT ON dbo.Concept_Group_Quality_Check TO [R2k_Administrator]
			GRANT UPDATE ON dbo.Concept_Group_Quality_Check TO [R2k_Administrator]
			GRANT INSERT ON dbo.Concept_Group_Quality_Check TO [R2k_Administrator]
			GRANT DELETE ON dbo.Concept_Group_Quality_Check TO [R2k_Administrator]
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		BEGIN
			GRANT SELECT ON dbo.Concept_Group_Quality_Check TO R2k_FullEdit
			GRANT UPDATE ON dbo.Concept_Group_Quality_Check TO R2k_FullEdit
			GRANT INSERT ON dbo.Concept_Group_Quality_Check TO R2k_FullEdit
			GRANT DELETE ON dbo.Concept_Group_Quality_Check TO R2k_FullEdit
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT SELECT ON dbo.Concept_Group_Quality_Check TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT SELECT ON dbo.Concept_Group_Quality_Check TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		BEGIN
			GRANT SELECT ON dbo.Concept_Group_Quality_Check TO [Dev - JNCC SQL]
			GRANT UPDATE ON dbo.Concept_Group_Quality_Check TO [Dev - JNCC SQL]
			GRANT INSERT ON dbo.Concept_Group_Quality_Check TO [Dev - JNCC SQL]
			GRANT DELETE ON dbo.Concept_Group_Quality_Check TO [Dev - JNCC SQL]
		END
	END
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupQualityCheck_Insert')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroupQualityCheck_Insert
GO

CREATE PROCEDURE dbo.usp_ConceptGroupQualityCheck_Insert
	@Concept_Group_Key	char(16),
	@Checked_By_User	char(16)
AS
	DECLARE @Key CHAR(16)
	EXEC dbo.spNextKey 'Concept_Group_Quality_Check', @Key OUTPUT

	INSERT INTO Concept_Group_Quality_Check(
		Concept_Group_Quality_Check_Key,
		Concept_Group_Key,
		Checked_Date_Time,
		Checked_By_User)
	VALUES (
		@Key,
		@Concept_Group_Key,
		GETDATE(),
		@Checked_By_User) 

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupQualityCheck_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupQualityCheck_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroup_Select_History')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroup_Select_History
GO

CREATE PROCEDURE dbo.usp_ConceptGroup_Select_History
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
		CGQC.Concept_Group_Quality_Check_Key as Item_Key, 	
		CGQC.Checked_Date_Time,
		dbo.ufn_GetFormattedName(CGQC.Checked_By_User) as Checked_By,
		CG.Custodian,
		CG.Timestamp
	FROM		Concept_Group AS CG
	LEFT JOIN	Concept_Group_Quality_Check CGQC
		ON		CGQC.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		CG.Concept_Group_Key = @Key
	ORDER BY CGQC.Checked_Date_Time DESC

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_History') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_History'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroup_Select_RecentHistory')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroup_Select_RecentHistory
GO

CREATE PROCEDURE dbo.usp_ConceptGroup_Select_RecentHistory
	@Key char(16),
	@LastCheckedDetails varchar(150) OUTPUT
AS

SET NOCOUNT ON

	SELECT TOP 1		
			@LastCheckedDetails = 
			CASE
				WHEN CGQC.Concept_Group_Quality_Check_Key IS NULL THEN
					'This concept group has never been checked'
				ELSE
					CONVERT(VARCHAR(25), CGQC.Checked_Date_Time, 120) + ' by ' + 
					dbo.ufn_GetFormattedName(CGQC.Checked_By_User)
			END
	FROM		Concept_Group AS CG
	LEFT JOIN	Concept_Group_Quality_Check CGQC
		ON		CGQC.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		CG.Concept_Group_Key = @Key
	ORDER BY CGQC.Checked_Date_Time DESC

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_RecentHistory') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_RecentHistory'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_CopyPairs')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_CopyPairs
GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Delete')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Delete
GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Insert')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Insert
GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_HomonymPair_Insert_ByMeaning')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_HomonymPair_Insert_ByMeaning
GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].usp_PotentialSynonyms_Count') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].usp_PotentialSynonyms_Count
GO

CREATE PROCEDURE [dbo].usp_PotentialSynonyms_Count 
	@ConceptGroupKey		CHAR(16),
	@RowCount				INT OUTPUT
AS
	SELECT DISTINCT @RowCount = COUNT(*)

	FROM			Concept						AS	CSource
	--Consider all synonyms of the source concept
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSynonyms
	ON				TSynonyms.Term_Key			=	CSynonyms.Term_Key
	INNER JOIN		Term						AS	TPotentials
	ON				TPotentials.Plaintext		=	TSynonyms.Plaintext
	AND				TPotentials.Language_Key	=	TSynonyms.Language_Key
	--Join on all concepts whose term matches the term of a synonym of
	--the original concepts
	INNER JOIN		Concept						AS	CPotentials
	ON				CPotentials.Term_Key		=	TPotentials.Term_Key
	--Get all synonyms of the concepts which match the term of one of the
	--synonyms of the original concept. These will be the potential synonyms
	INNER JOIN		Concept						AS	CPotSyn
	ON				CPotSyn.Meaning_Key			=	CPotentials.Meaning_Key
	--Exclude any concepts that match by term but which are already synonyms
	LEFT JOIN		Concept						AS	CExclude
	ON				CExclude.Concept_Key		=	CPotentials.Concept_Key
	AND				CExclude.Meaning_Key		=	CSource.Meaning_Key
	--Exclude any concepts that match by term but whose meaning is already
	--marked as an homonym of the source concept meaning
	LEFT JOIN		Homonym_Pair				AS	H
	ON				(H.Meaning_Key_1			=	CPotentials.Meaning_Key
		AND			H.Meaning_Key_2				=	CSource.Meaning_Key)
	OR				(H.Meaning_Key_1			=	CSource.Meaning_Key
		AND			H.Meaning_Key_2				=	CPotentials.Meaning_Key)
	WHERE			CSource.Concept_Group_Key	=	@ConceptGroupKey
	AND				CExclude.Concept_Key IS NULL
	AND				H.Meaning_Key_1 IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Count') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Count'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_Concept_Select_Homonyms')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_Concept_Select_Homonyms
GO

/*===========================================================================*\
  Description:	Returns all homonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	May 2011

  Last revision information:
	$Revision: 2 $
	$Date: 26/05/11 16:14 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Concept_Select_Homonyms
	@Key		CHAR(16)
AS
	SET NOCOUNT ON

	select 
		h.Concept_Key as Item_Key,
		IsNull(ct.Item_Name + ' ' + tv.Author_And_Date + ' (' + cg.Item_Name + ')', 
				ct.Item_Name + ' (' + cg.Item_Name + ')') as Item_Name
	from		concept c
	inner join	homonym_pair hp
		on		hp.Meaning_Key_1 = c.Meaning_Key or hp.Meaning_Key_2 = c.Meaning_Key
	inner join	concept h
		on		(hp.Meaning_Key_1 = h.Meaning_Key or hp.Meaning_Key_2 = h.Meaning_Key)
		and		(h.Meaning_Key <> c.Meaning_Key)
	inner join 	VW_ConceptTerm ct
	 	on ct.Concept_Key = h.Concept_Key
	left join	Term_Version tv 	
		on tv.Term_Version_Key = h.Term_Version_Key
	inner join	Concept_Group cg
		on cg.Concept_Group_Key = h.Concept_Group_Key
	where c.concept_key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Homonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Homonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Synonym_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Synonym_Delete]
GO

CREATE PROCEDURE [dbo].[usp_Synonym_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE 
			@OldMeaningKey char(16),
			@NewMeaningKey char(16),
			@HomonymKey char(16),
			@Error int

		SELECT @OldMeaningKey = Meaning_Key
		FROM Concept
		WHERE Concept_Key = @Key

		/*-------------------------------------------------------------*\
		  Create a new Meaning Key
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@NewMeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*--------------------------------------------------------------*\
			Copy over existing homonym pairs for new meaning key
		\*--------------------------------------------------------------*/
		EXEC dbo.usp_HomonymPair_CopyPairs @OldMeaningKey, @NewMeaningKey
		

		/*-------------------------------------------------------------*\
		  Update the concept record
		\*-------------------------------------------------------------*/
		UPDATE	Concept
		SET 	Meaning_Key = @NewMeaningKey,	
			List_Preferred = 1
		WHERE	Concept_Key = @Key
		AND	[Timestamp] = @Timestamp

		/*-------------------------------------------------------------*\
		  Get the number of records affected and see if any errors.
		\*-------------------------------------------------------------*/	
		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error
		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Synonym_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Synonym_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Synonym_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@DeleteUnlinkedSynonyms bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp,
			@error				INT,
			@RecordsAffected	INT

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			

		/*====================================*\
		  Delete the synonyms that are no longer
		  required.
		\*====================================*/
		IF @DeleteUnlinkedSynonyms=1
		BEGIN
			DECLARE @SynConceptKey CHAR(16)
			
			DECLARE csr CURSOR FOR
				SELECT CSyn.Concept_Key
				FROM Concept C
				INNER JOIN Concept CSyn 
					ON CSyn.Meaning_Key=C.Meaning_Key
					AND CSyn.Concept_Group_Key=C.Concept_Group_Key
					AND CSyn.List_Preferred=0
					AND C.Concept_Key=@Key
			
			OPEN csr
			WHILE (1=1)
			BEGIN
				FETCH NEXT FROM csr INTO @SynConceptKey

				IF @@FETCH_STATUS <> 0
					BREAK

				-- Recurse to remove synonym concepts
				EXEC usp_Concept_Delete @SynConceptKey
			END
			CLOSE csr
			DEALLOCATE csr
		END
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(@Timestamp = @OriginalTimestamp OR @Timestamp IS NULL)

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		-- Delete the Meaning record if only one Concept uses that Meaning key. Also
		-- delete any Homonym_Pair records with this meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
		BEGIN
			DELETE	Homonym_Pair
			WHERE	Meaning_Key_1 = @MeaningKey
			OR		Meaning_Key_2 = @MeaningKey

			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey
		END

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PotentialSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
GO

CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	/*=============================*\
	  Actually get the results set.
	\*=============================*/
	SELECT DISTINCT
			CPotentials.Concept_Key AS Item_Key, 
			IsNull(CT.Item_Name + ' ' + TV.Author_And_Date + ' (' + CG.Item_Name + ')', 
				CT.Item_Name + ' (' + CG.Item_Name + ')') AS Item_Name
	FROM 		Concept AS CSource
	INNER JOIN 	Concept AS CSynonyms 		ON CSynonyms.Meaning_Key = CSource.Meaning_Key
	INNER JOIn	Term AS TSource			ON TSource.Term_Key = CSource.Term_Key
	INNER JOIN 	Term AS TSynonyms 		ON TSynonyms.Term_Key = CSynonyms.Term_Key
	INNER JOIN 	Term AS TPotentials 		ON TPotentials.Plaintext = TSynonyms.Plaintext
							AND TPotentials.Language_Key = TSource.Language_Key
	INNER JOIN 	Concept AS CPotentials 		ON CPotentials.Term_Key = TPotentials.Term_Key
	LEFT JOIN 	Concept AS CExclude		ON CExclude.Concept_Key = CPotentials.Concept_Key
							AND CExclude.Meaning_Key = CSource.Meaning_Key
	LEFT JOIN Homonym_Pair AS H	ON (H.Meaning_Key_1	= CPotentials.Meaning_Key
								AND	H.Meaning_Key_2	= CSource.Meaning_Key)
								OR (H.Meaning_Key_1 = CSource.Meaning_Key
								AND	H.Meaning_Key_2 = CPotentials.Meaning_Key)
	INNER JOIN 	VW_ConceptTerm AS CT 	ON CT.Concept_Key = CPotentials.Concept_Key
							AND CT.Concept_Key <> CSynonyms.Concept_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CPotentials.Term_Version_Key
	INNER JOIN	Concept_Group AS CG 		ON CG.Concept_Group_Key = CPotentials.Concept_Group_Key
	WHERE 		CSource.Concept_Key = @Key
	AND 		CExclude.Concept_Key IS NULL
	AND			H.Meaning_Key_1 IS NULL	
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MakeConceptsSynonyms_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
GO

CREATE PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
	@ConceptKeySelected char(16),
	@ConceptKeyPasted char(16)
AS

SET NOCOUNT ON

	DECLARE @MeaningKeySelected char(16),
		@MeaningKeyPasted char(16),	
		@MeaningKeyChosen char(16),
		@MeaningKeyDiscarded char(16),
		@ConceptGroupKeySource char(16),
		@ConceptGroupKeyDest char(16),
		@HomonymKey char(16),
		@SiteID char(8)

	BEGIN TRANSACTION

		/*=====================================================================*\
		  Get the meaning keys, and the Concept_Rank_Key of the target Concept.
		\*=====================================================================*/
		SELECT	@MeaningKeySelected = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeySelected
	
		SELECT	@MeaningKeyPasted = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeyPasted
	
		/*================================================================================*\
		  2 concepts that are in the custody of SYSTEM00 cannot be made synonyms of each 
		  other as the software expects both meaning keys to be present. 
		\*================================================================================*/
		IF NOT ((@ConceptKeySelected LIKE 'SYSTEM00%') AND (@ConceptKeyPasted LIKE 'SYSTEM00%'))
		BEGIN
			SELECT	@SiteID = Data
			FROM	Setting
			WHERE	[Name] = 'SiteID'

			/*==================================*\
			  Choose which meaning key to keep. 
			\*==================================*/	

			/*================================================================================*\
			  If 1 concept has a meaning key starting SYSTEM00, then both concepts are 
			  assigned to this key.
			\*================================================================================*/
			IF @MeaningKeySelected LIKE 'SYSTEM00%'
			BEGIN			
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE 'SYSTEM00%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If neither concept has a meaning key starting SYSTEM00, but one of the meaning 
			  keys starts with the Site ID of the current site, then this is the meaning 
			  key adopted.
			\*================================================================================*/
			ELSE IF @MeaningKeySelected LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If a single meaning key is still not selected by these rules, then the meaning 
			  key that belongs to the concept that will be list preferred after the synonymy 
			  is applied is selected.
			\*================================================================================*/
			
			-- Make sure usp_Concept_Update_ForPreferred is run before this stored proc.
			-- That way the preferred and list_preferred fields for the pasted concept
			-- will already have been updated.

			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeySelected
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
	
			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeyPasted
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If this still does not resolve a single meaning key, then the meaning key 
			  adopted is the one associated with the concept selected at the time of 
			  application of synonymy.
			\*================================================================================*/
			ELSE
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			
			/*===========================*\
			  Actually do the update.
			\*===========================*/		
			UPDATE	Concept
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*=======================================================================*\
			  References to @MeaningKeyDiscarded need to be replaced with references
			  to @MeaningKeyChosen before the meaning record can be deleted.
			\*=======================================================================*/	
			UPDATE	Thesaurus_Fact
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	From_Meaning_Key = @MeaningKeyChosen
			WHERE	From_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	To_Meaning_Key = @MeaningKeyChosen
			WHERE	To_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Taxon_Dictionary_Meaning_Mapping
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			

 
			/*========================================================================*\
			  Now all references to the old meaning key have gone, we can delete it 
			  (providing the two concepts aren't already synonyms. If that is the case, 
			  don't delete the meaing key.
			\*========================================================================*/
			IF @MeaningKeySelected <> @MeaningKeyPasted
			BEGIN
				--If @MeaningKeyChosen and @MeaningKeyDiscarded are homonyms, the homonym
				--record must be removed.
				IF EXISTS (SELECT * FROM Homonym_Pair
							WHERE (Meaning_Key_1 = @MeaningKeyChosen 
								AND Meaning_Key_2 = @MeaningKeyDiscarded)
							OR (Meaning_Key_2 = @MeaningKeyChosen 
								AND Meaning_Key_1 = @MeaningKeyDiscarded))
				BEGIN
					DELETE FROM Homonym_Pair
					WHERE (Meaning_Key_1 = @MeaningKeyChosen 
						AND Meaning_Key_2 = @MeaningKeyDiscarded)
					OR (Meaning_Key_2 = @MeaningKeyChosen 
						AND Meaning_Key_1 = @MeaningKeyDiscarded)
				END

				DECLARE partnerKey CURSOR LOCAL FAST_FORWARD FOR 
				SELECT PartnersOfChosen.Homonym_Meaning_Key FROM
				(SELECT
					CASE
						WHEN Meaning_Key_1 = @MeaningKeyChosen THEN Meaning_Key_2
						ELSE Meaning_Key_1
					END as Homonym_Meaning_Key
				FROM Homonym_Pair
				WHERE Meaning_Key_1 = @MeaningKeyChosen OR Meaning_Key_2 = @MeaningKeyChosen)
				AS PartnersOfChosen
				INNER JOIN (SELECT
					CASE
						WHEN Meaning_Key_1 = @MeaningKeyChosen THEN Meaning_Key_2
						ELSE Meaning_Key_1
					END	as Homonym_Meaning_Key
				FROM Homonym_Pair
				WHERE Meaning_Key_1 = @MeaningKeyDiscarded OR Meaning_Key_2 = @MeaningKeyDiscarded)
				AS PartnersOfDiscarded
				ON PartnersOfDiscarded.Homonym_Meaning_Key = PartnersOfChosen.Homonym_Meaning_Key

				OPEN partnerKey

				WHILE 1 = 1
				BEGIN
					FETCH		partnerKey
					INTO		@HomonymKey

					IF @@FETCH_STATUS <> 0 BREAK

					DELETE FROM Homonym_Pair
					WHERE (Meaning_Key_1 = @MeaningKeyDiscarded AND Meaning_Key_2 = @HomonymKey)
					OR (Meaning_Key_2 = @MeaningKeyDiscarded AND Meaning_Key_1 = @HomonymKey)
					
				END

				CLOSE partnerKey
				DEALLOCATE partnerKey

				UPDATE	Homonym_Pair
				SET Meaning_Key_1 = @MeaningKeyChosen
				WHERE Meaning_Key_1 = @MeaningKeyDiscarded
				AND	Meaning_Key_2 > @MeaningKeyChosen

				UPDATE	Homonym_Pair
				SET Meaning_Key_2 = @MeaningKeyChosen
				WHERE Meaning_Key_2 = @MeaningKeyDiscarded
				AND	Meaning_Key_1 < @MeaningKeyChosen
		
				DELETE	Meaning
				WHERE	Meaning_Key = @MeaningKeyDiscarded
			END
		
			/*===========================*\
			  Get the concept group keys.
			\*===========================*/
			SELECT	@ConceptGroupKeySource = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeySelected
		
			SELECT	@ConceptGroupKeyDest = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeyPasted
		
			/*===============================================================*\
			  If the source concept is in the same list as the dest concept, 
			  make it not list preferred.
			\*===============================================================*/
			IF @ConceptGroupKeySource = @ConceptGroupKeyDest
			BEGIN
				UPDATE	Concept
				SET	List_Preferred = 0
				WHERE	Concept_Key = @ConceptGroupKeySource
				AND	List_Preferred = 1
	
				DELETE	Concept_Lineage
				WHERE	Concept_Key = @ConceptGroupKeySource
			END
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MakeConceptsSynonyms_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MakeConceptsSynonyms_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT			DISTINCT
					s.Concept_Key,
					t.Item_Name,
					ISNULL
					(
						TV.Author_And_Date,
						''
					)						AS		Authority, 
					t.PlainText,
					g.Item_Name				AS		Group_Name
	
	FROM			Concept					AS		c
	INNER JOIN		Concept					AS		s
	ON				s.Meaning_Key			=		c.Meaning_Key
	
	INNER JOIN		Term					AS		t
	ON				t.Term_Key				=		s.Term_Key
	

	INNER JOIN		dbo.Term_Version		AS		TV
	ON				TV.Term_Version_Key		=		s.Term_Version_Key

	INNER JOIN		Concept_Group			AS 		g
	ON				g.Concept_Group_Key		=		s.Concept_Group_Key
	WHERE			c.Concept_Key			=		@concept_key
	AND				s.Concept_Key			<>		@concept_key
	ORDER BY		g.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Synonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Synonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select]
GO

CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			TRT.Item_Name AS Hierarchy_Relation_Type_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp],
			CASE WHEN COUNT(CGQC.Checked_Date_Time) > 0 THEN 1
			ELSE 0 END AS HasHistory
	FROM		Concept_Group AS CG
	LEFT JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	LEFT JOIN	Concept_Group_Quality_Check AS CGQC ON CGQC.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		CG.Concept_Group_Key = @Key 
	GROUP BY
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			TRT.Item_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PotentialSynonyms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge]
GO

CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge] 
	@ConceptGroupKey		CHAR(16),
	@ConceptGroupSearchKey	CHAR(16),
	@PreferredSynonymGroup	CHAR(16),
	@MaxRowCount			INT,
	@Timestamp				TIMESTAMP,
	@SessionId				CHAR(16)
AS
	SET NOCOUNT ON
	
	SET ROWCOUNT @MaxRowCount

	SELECT			DISTINCT 
					CSource.Concept_Key			AS	SourceConceptKey,
					CSource.Meaning_Key			AS	SourceMeaningKey,
					CG.Item_Name				AS	SourceGroup,
					ISNULL
					(
						TVSource.Author_And_Date,
						''
					)							AS	SourceAuthority,
					TSource.Item_Name			AS	SourceConcept,
					TSource.PlainText,			-- There for the ORDER BY
					CPotSyn.Concept_Key			AS	SynonymConceptKey,
					CPotSyn.Meaning_Key			AS	SynonymMeaningKey,
					CGPotSyn.Item_Name			AS	SynonymGroup,
					ISNULL
					(
						TVPotSyn.Author_And_Date,
						''
					)							AS	SynonymAuthority,
					TPotSyn.Item_Name			AS	SynonymConcept,
					TPotentials.Item_Name		AS	SharedTerm,
					CSource.Custodian,
					CSource.Timestamp,
					CASE 
						WHEN CPotSyn.Concept_Group_Key = @PreferredSynonymGroup
							THEN 1
						ELSE 0
					END AS InPreferredGroup
	FROM			Concept						AS	CSource
	--Consider all synonyms of the source concept
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSource
	ON				TSource.Term_Key			=	CSource.Term_Key
	INNER JOIN		Term						AS	TSynonyms
	ON				TSynonyms.Term_Key			=	CSynonyms.Term_Key
	INNER JOIN		Term						AS	TPotentials
	ON				TPotentials.Plaintext		=	TSynonyms.Plaintext
	AND				TPotentials.Language_Key	=	TSynonyms.Language_Key
	--Join on all concepts whose term matches the term of a synonym of
	--the original concepts
	INNER JOIN		Concept						AS	CPotentials
	ON				CPotentials.Term_Key		=	TPotentials.Term_Key
	--Get all synonyms of the concepts which match the term of one of the
	--synonyms of the original concept. These will be the potential synonyms
	INNER JOIN		Concept						AS	CPotSyn
	ON				CPotSyn.Meaning_Key			=	CPotentials.Meaning_Key
	INNER JOIN		Term						AS	TPotSyn
	ON				TPotSyn.Term_Key			=	CPotSyn.Term_Key
	--Exclude any concepts that match by term but which are already synonyms
	LEFT JOIN		Concept						AS	CExclude
	ON				CExclude.Concept_Key		=	CPotentials.Concept_Key
	AND				CExclude.Meaning_Key		=	CSource.Meaning_Key
	--Exclude any concepts that match by term but whose meaning is already
	--marked as an homonym of the source concept meaning
	LEFT JOIN		Homonym_Pair				AS	H
	ON				(H.Meaning_Key_1			=	CPotentials.Meaning_Key
		AND			H.Meaning_Key_2				=	CSource.Meaning_Key)
	OR				(H.Meaning_Key_1			=	CSource.Meaning_Key
		AND			H.Meaning_Key_2				=	CPotentials.Meaning_Key)
	INNER JOIN		Concept_Group				AS	CG
	ON				CG.Concept_Group_Key		=	CSource.Concept_Group_Key
	INNER JOIN		Concept_Group				AS	CGPotSyn
	ON				CGPotSyn.Concept_Group_Key	=	CPotSyn.Concept_Group_Key
	LEFT JOIN		dbo.Term_Version			AS	TVSource
	ON				TVSource.Term_Version_Key	=	CSource.Term_Version_Key
	LEFT JOIN		dbo.Term_Version			AS	TVPotSyn
	ON				TVPotSyn.Term_Version_Key	=	CSynonyms.Term_Version_Key
	WHERE			(@ConceptGroupKey IS NULL
	OR				CSource.Concept_Group_Key	=	@ConceptGroupKey)
	AND				(@ConceptGroupSearchKey IS NULL
	OR				CPotSyn.Concept_Group_Key	=	@ConceptGroupSearchKey)
	AND				CExclude.Concept_Key IS NULL
	AND				H.Meaning_Key_1 IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
	AND				CSource.[Timestamp]			>	ISNULL(@Timestamp, 0)
	AND				(@SessionId IS NULL
	OR				CSource.Entered_Session_Id	=	@SessionId)	
	ORDER BY		TSource.PlainText
	
	SET ROWCOUNT 0
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Delete]
GO

CREATE PROCEDURE [dbo].[usp_ConceptGroup_Delete]
	@Key char(16),
	@Timestamp timestamp = null,
	@SyncTaxonDict bit = 0
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE @TaxonListKey char(16)

				SELECT 	@TaxonListKey = Taxon_List_Key
				FROM	Taxon_Dictionary_Concept_Group_Mapping
				WHERE	Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List_Version
				WHERE	@TaxonListKey = Taxon_List_Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Concept_Group_Mapping
				WHERE		Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List
				WHERE	Taxon_List_Key = @TaxonListKey

				IF @@Error <> 0 GOTO RollbackAndExit	
			END
		END
		ELSE
			DELETE	Taxon_Dictionary_Concept_Group_Mapping
			WHERE		Concept_Group_Key = @Key

		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ ON SJ.Source_Key=SF.Source_Key
		WHERE SJ.Table_Name='Concept_Group'
		AND SJ.Record_Key=@Key

		DELETE Source_Join
		WHERE Table_Name='Concept_Group'
		AND Record_Key=@Key

		DELETE Concept_Group_Quality_Check
		WHERE Concept_Group_Key = @Key
	
		DELETE 
		FROM 		Concept_Group
		WHERE		Concept_Group_Key = @Key
		AND			([Timestamp] = @Timestamp	OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Following stored proc is no longer required.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_PotentialSynonyms_Select_ForImportedConcept')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
GO
