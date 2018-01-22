/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_Select_ForCaptionUpdate')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_Select_ForCaptionUpdate
GO

/*===========================================================================*\
  Description:	For a given concept, returns a list of all concepts whose
				captions may need updating. Includes all child concepts, as
				well as any concepts which share the given concept's term
				or term version.


  Created:	August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 30/08/11 15:25 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_Select_ForCaptionUpdate
	@ConceptKey char(16)
AS
	SELECT relc.Concept_Key
	FROM Concept c
	INNER JOIN Concept relc on relc.Term_Key = c.Term_Key 
		OR relc.Term_Version_Key = c.Term_Version_Key
	WHERE c.Concept_Key = @ConceptKey
	
	UNION 

	-- Get child concepts
	SELECT crelated.concept_key 
	FROM concept c 
	LEFT JOIN concept_lineage cl on cl.concept_key = c.concept_key
	INNER JOIN (
		SELECT 
			cl1.lineage, 
			cl1.lineage_id, 
			c1.concept_group_key, 
			c1.concept_key
		FROM concept_lineage cl1
		INNER JOIN concept c1 on c1.concept_key = cl1.concept_key ) as crelated
		ON crelated.lineage LIKE cl.lineage + '\%' 
			AND c.concept_group_key = crelated.concept_group_key
	WHERE c.concept_key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForCaptionUpdate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForCaptionUpdate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [Dev - JNCC SQL]
END

GO