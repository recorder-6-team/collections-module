/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLNames_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept for the name and synonyms.  This is separated from 
			the rest of the details so that it can be displayed first whilst the 
			rest of the HTML is loaded.
			The following recordsets are returned:
				Title
				List Synonyms
				All Known Synonyms

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 4/08/11 10:34 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLNames_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
  Recordset for HTML Title (name + author).  Single record.
\*===========================================================================*/
	SELECT Item_Name	AS		ItemName
	FROM VW_ConceptTerm
	WHERE Concept_Key=@ConceptKey


/*===========================================================================*\	
  Recordset for list synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT 
		C2.Published_Term AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
				AND C2.Concept_Group_Key=C1.Concept_Group_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
		AND C2.Concept_Key<>@ConceptKey
	ORDER BY Seq, C2.Preferred, T.Plaintext


/*===========================================================================*\	
  Recordset for all synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT DISTINCT
		C2.Published_Term AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
	ORDER BY Seq, T.Plaintext
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [Dev - JNCC SQL]
END

GO