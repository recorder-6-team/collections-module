SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForChild]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForChild]
GO

/*===========================================================================*\
  Description:	
		Selects the parent of the given concept. 

  Parameters:
		@ChildConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.	

  Created:	September 2010

  Last revision information:
    $Revision: 2 $
    $Date: 3/08/11 11:58 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForChild]
	@ChildConceptKey			VARCHAR(100),
  	@HierarchyRelationTypeKey	CHAR(16)
AS
	SELECT DISTINCT 
				C.Concept_Key, 
				C.Published_Term	AS	Item_Name, 
				C.Sort_Code, 
  				CASE 
					WHEN CR2.Concept_Relation_Key IS NULL 
					THEN 0 
					ELSE 1 
				END AS HasParents,
  				C.Concept_Rank_Key
	FROM 		Concept_Relation	AS	CR1
	INNER JOIN 	Concept				AS	C
	ON			C.Concept_Key		=	CR1.From_Concept_Key
	LEFT JOIN 	(Concept_Relation CR2 
				INNER JOIN Concept	AS C2 
				ON	C2.Concept_Key			= CR2.From_Concept_Key
					AND C2.List_Preferred	= 1
					AND C2.Is_Current		= 1)
	ON			CR2.To_Concept_Key				= C.Concept_Key
		AND		CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
	WHERE 		CR1.To_Concept_Key				= @ChildConceptKey
		AND 	CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
		AND 	C.List_Preferred	= 1
		AND 	C.Is_Current		= 1
	ORDER BY 	C.Sort_Code, 
				C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForChild') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForChild'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [Dev - JNCC SQL]
END
GO