/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupPreferredTaxonList_Select')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Select
GO

/*===========================================================================*\
  Description:	Returns all preferred lists for a selected concept group

  Parameters:	@ConceptGroupKey CHAR(16)

  Created:		May 2011

  Last revision information:
	$Revision: 1 $
	$Date: 30/05/11 14:28 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Select
	@ConceptGroupKey CHAR(16)
AS
	SELECT
		cgptl.Priority,
		cgptl.Taxon_List_Key as Item_Key,
		tl.Item_Name as Item_Name
	FROM Concept_Group_Preferred_Taxon_List cgptl
	INNER JOIN Taxon_List tl
	ON tl.Taxon_List_Key = cgptl.Taxon_List_Key
	WHERE cgptl.Concept_Group_Key = @ConceptGroupKey
	ORDER BY Priority		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupPreferredTaxonList_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupPreferredTaxonList_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Select TO [Dev - JNCC SQL]
END
GO