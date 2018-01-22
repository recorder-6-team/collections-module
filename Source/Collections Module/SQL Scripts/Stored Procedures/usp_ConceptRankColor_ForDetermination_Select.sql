/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRankColor_ForDetermination_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRankColor_ForDetermination_Select]
GO

/*===========================================================================*\
  Description:	Selects the RGB color components for a Determination /
		Taxon_Determination key.

  Parameters:	@Key		(Taxon) Determination key
		@IsLifeScience

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/04/04 15:11 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRankColor_ForDetermination_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1 
		SELECT 		IsNull(CR.Color_R, 0) AS Color_R,
				IsNull(CR.Color_G, 0) AS Color_G,
				IsNull(CR.Color_B, 0) AS Color_B
		FROM		Taxon_Determination AS TD
		LEFT JOIN	Concept AS C ON C.Concept_Key = TD.Nomenclatural_Status_Concept_Key 
		LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
		WHERE		TD.Taxon_Determination_Key = @Key
	ELSE
		SELECT 		IsNull(CR.Color_R, 0) AS Color_R,
				IsNull(CR.Color_G, 0) AS Color_G,
				IsNull(CR.Color_B, 0) AS Color_B
		FROM		Determination AS D
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Nomenclatural_Status_Concept_Key
		LEFT JOIN 	Concept_Rank CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
		WHERE		D.Determination_Key = @Key		

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRankColor_ForDetermination_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRankColor_ForDetermination_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [Dev - JNCC SQL]
END

GO