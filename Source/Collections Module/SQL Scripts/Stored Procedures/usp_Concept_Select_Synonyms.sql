/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

/*===========================================================================*\
  Description:	Names of synonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 3/08/11 14:32 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT			DISTINCT
					s.Concept_Key,
					s.Published_Term	AS	Item_Name,
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