/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationForwardReverse_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationForwardReverse_Select]
GO

/*===========================================================================*\
  Description:	Returns the forward and reverse terms for each record
		in the Thesaurus_Relation_Type table.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/04/04 15:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationForwardReverse_Select]
	@RelationUsage TINYINT
AS

SET NOCOUNT ON
	SELECT 	 TRT.Thesaurus_Relation_Type_Key, TRT.Forward_Term AS Item_Name, 1 AS IsForward
	FROM 	 Thesaurus_Relation_Type TRT
	INNER JOIN Thesaurus_Relation_Type_Usage U ON U.Thesaurus_Relation_Type_Key=TRT.Thesaurus_Relation_Type_Key
			AND U.Relation_Usage = @RelationUsage
	UNION
	SELECT 	 TRT.Thesaurus_Relation_Type_Key, TRT.Reverse_Term AS Item_Name, 0 AS IsForward
	FROM 	 Thesaurus_Relation_Type TRT
	INNER JOIN Thesaurus_Relation_Type_Usage U ON U.Thesaurus_Relation_Type_Key=TRT.Thesaurus_Relation_Type_Key
			AND U.Relation_Usage = @RelationUsage

	ORDER BY Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationForwardReverse_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationForwardReverse_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationForwardReverse_Select TO [Dev - JNCC SQL]
END

GO