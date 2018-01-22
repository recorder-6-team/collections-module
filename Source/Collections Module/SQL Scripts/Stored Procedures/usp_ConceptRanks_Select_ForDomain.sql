/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_Select_ForDomain]
GO

/*===========================================================================*\
  Description: 	Returns the concept ranks available in the current domain.

  Parameters:	@Key	Domain Key

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 17:03 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_Select_ForDomain]
	@Key char(16)
AS
	SELECT
			Concept_Rank_Key AS Item_Key,
			Item_Name,
			Sort_Order,
			Abbreviation,
			Color_R,
			Color_G,
			Color_B,
			Custodian,
			[Timestamp] 
	FROM 		Concept_Rank
	WHERE		Domain_Key = @Key
	ORDER BY	Sort_Order, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForDomain TO [Dev - JNCC SQL]
END

GO