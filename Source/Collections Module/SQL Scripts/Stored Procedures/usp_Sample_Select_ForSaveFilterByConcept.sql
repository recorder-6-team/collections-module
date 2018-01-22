SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/ 
IF OBJECT_ID('dbo.usp_Sample_Select_ForSaveFilterByConcept') IS NOT NULL
	DROP PROCEDURE dbo.usp_Sample_Select_ForSaveFilterByConcept
GO

/*============================================================================*\
Description:	Selects the samples related to the given concept for the the
				purpose of saving to an external filter when failing to delete
				the concept via the Thesaurus Editor.	

Parameters:	@ConceptKey 	- The identifer of the concept with which to search
	
Created:	October 2010

Last revision information:
	$Revision: 1 $
	$Date: 18/10/10 17:12 $
	$Author: Robertjohnson $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Sample_Select_ForSaveFilterByConcept
	@ConceptKey	CHAR(16)
AS
	SELECT		S.Sample_Key	
	FROM		Determination	AS	D
	INNER JOIN	Occurrence		AS	O	ON D.Occurrence_Key		= O.Occurrence_Key
	INNER JOIN	[Sample]		AS	S	ON O.Sample_Key			= S.Sample_Key
	WHERE		D.Concept_Key	=	@ConceptKey
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Select_ForSaveFilterByConcept') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Sample_Select_ForSaveFilterByConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForSaveFilterByConcept TO [Dev - JNCC SQL]
END
GO