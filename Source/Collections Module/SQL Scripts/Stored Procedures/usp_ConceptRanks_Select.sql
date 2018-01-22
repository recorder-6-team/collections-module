/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_Select]
GO

/*===========================================================================*\
  Description: 	Returns the concept ranks available for the concept's
		current domain.		

  Parameters:	@ConceptGroupKey

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/10/04 9:50 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_Select]
	@ConceptGroupKey char(16)
AS

	SELECT
			CR.Concept_Rank_Key AS Item_Key,
			CR.Item_Name 
	FROM 		Concept_Rank AS CR
	INNER JOIN	Domain AS D ON D.Domain_Key = CR.Domain_Key
	INNER JOIN	Local_Domain AS LD ON LD.Domain_Key = D.Domain_Key
	INNER JOIN	Concept_Group AS CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	WHERE		CG.Concept_Group_Key = @ConceptGroupKey
	ORDER BY 	CR.Sort_Order, CR.Concept_Rank_Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [Dev - JNCC SQL]
END

GO