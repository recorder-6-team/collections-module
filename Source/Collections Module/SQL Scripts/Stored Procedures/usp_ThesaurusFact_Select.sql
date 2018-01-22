/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 12/12/03 9:35 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 	
			TF.Item_Name,
			TF.Data,
			TF.Meaning_Key,
			TF.Concept_Key,
			TF.Term_Version_Key,
			TF.Related_Term_Versions,
			TF.Inherited,
			TF.Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			TF.Fact_Vague_Date_Start,
			TF.Fact_Vague_Date_End,
			TF.Fact_Vague_Date_Type,
			C.Meaning_Key AS Fact_Type_Meaning_Key,
			CT.Item_Name AS Fact_Type_Concept_Name,
			TF.[Timestamp]
	FROM		Thesaurus_Fact AS TF
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = TF.Fact_Type_Concept_Key
	INNER JOIN	Concept AS C ON C.Concept_Key = TF.Fact_Type_Concept_Key
	INNER JOIN	Language AS L ON L.Language_Key = TF.Language_Key
	WHERE 		Thesaurus_Fact_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select TO [Dev - JNCC SQL]
END

GO