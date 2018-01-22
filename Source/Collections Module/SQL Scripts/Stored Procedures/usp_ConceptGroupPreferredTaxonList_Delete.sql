/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupPreferredTaxonList_Delete')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Delete
GO

/*===========================================================================*\
  Description:	Delete a preferred taxon list

  Parameters:	@ConceptGroupKey CHAR(16),
				@TaxonListKey CHAR(16)

  Created:		May 2011

  Last revision information:
	$Revision: 2 $
	$Date: 9/06/11 16:26 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Delete
	@ConceptGroupKey CHAR(16)
AS
	DELETE FROM Concept_Group_Preferred_Taxon_List
	WHERE Concept_Group_Key = @ConceptGroupKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupPreferredTaxonList_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupPreferredTaxonList_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Delete TO [Dev - JNCC SQL]
END
GO