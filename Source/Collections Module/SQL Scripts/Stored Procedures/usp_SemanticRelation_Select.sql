/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SemanticRelation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SemanticRelation_Select]
GO

/*===========================================================================*\
  Description:	Returns a semantic relation record.

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/04 12:10 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SemanticRelation_Select]
	@Key char(16)
AS

SET NOCOUNT ON
	
	SELECT
			Semantic_Relation_Key, 
			Item_Name, 
			Unidirectional, 
			Forward_Equivalence_Possible,
			Forward_Equivalence_Definite,
			Reverse_Equivalence_Possible,
			Reverse_Equivalence_Definite,
			Proportional_Relationship,
			Chronological_Overlap,
			Adjacent,
			[Description],
			[Timestamp]
	FROM		Semantic_Relation
	WHERE		Semantic_Relation_Key = @Key
	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SemanticRelation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SemanticRelation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Select TO [Dev - JNCC SQL]
END

GO