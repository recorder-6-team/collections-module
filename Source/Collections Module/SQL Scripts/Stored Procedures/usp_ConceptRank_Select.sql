/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRank_Select]
GO

/*===========================================================================*\
  Description: Returns details of a single concept rank

  Parameters:	@ConceptRankKey

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 13:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_Select]
	@ConceptRankKey varchar(100)
AS

SELECT Item_Name, Abbreviation, Color_R, Color_G, Color_B, Sort_Order
FROM Concept_Rank
WHERE Concept_Rank_Key=@ConceptRankKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Select TO [Dev - JNCC SQL]
END

GO