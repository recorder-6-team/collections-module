/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SeasonNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SeasonNames_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of season names according to preferred language.

  Parameters:	@Language	The ISO abbreviation of language in use on 
				client machine.

  Created:	March 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/02/07 10:11 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SeasonNames_Select]
	@Language varchar(4)
AS
 	SET NOCOUNT ON

	-- Get the list of seasons concepts
	SELECT Concept_Key, Concept_Group_Key, Sort_Code, Meaning_Key, 
		CAST('' AS VARCHAR(200)) AS Plaintext
	INTO #Seasons
	FROM Concept
	WHERE Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
	AND List_Preferred=1
	
	-- Find the season terms in the preferred language
	UPDATE C1
	SET C1.Plaintext=T.PlainText
	FROM #Seasons C1
	JOIN 	Concept C2 ON C2.Meaning_Key = C1.Meaning_Key
	JOIN 	Term T ON T.Term_Key = C2.Term_Key
	JOIN 	Language L ON L.Language_Key = T.Language_Key
		AND	L.Language_Key = @Language
	WHERE 	C1.Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
	
	-- Now find the season terms in English if preferred language not available
	UPDATE C1
	SET C1.Plaintext=T.PlainText
	FROM #Seasons C1
	JOIN 	Concept C2 ON C2.Meaning_Key = C1.Meaning_Key
	JOIN 	Term T ON T.Term_Key = C2.Term_Key
	JOIN 	Language L ON L.Language_Key = T.Language_Key
	WHERE 	C1.Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
		AND	L.Language_Key = 'en'
		AND C1.Plaintext = ''
	
	SELECT Concept_Key, Plaintext 
	FROM #Seasons
	ORDER BY Sort_Code
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SeasonNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SeasonNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [Dev - JNCC SQL]
END
GO