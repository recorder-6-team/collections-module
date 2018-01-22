/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_Select_NonPreferredLists]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_Select_NonPreferredLists]
GO

/*===========================================================================*\
  Description:	Select all taxon lists which are not preferred for a concept
				group.

  Parameters:	@ConceptGroupKey CHAR(16)

  Created:		May 2011

  Last revision information:
	$Revision: 1 $
	$Date: 30/05/11 14:28 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_Select_NonPreferredLists]
	@ConceptGroupKey CHAR(16)
AS
	SET NOCOUNT ON

	SELECT		tl.Taxon_List_Key as Item_Key,
				tl.Item_Name
	FROM		Taxon_List tl
	LEFT JOIN	(
					SELECT * 
					FROM Concept_Group_Preferred_Taxon_List 
					WHERE Concept_Group_Key = @ConceptGroupKey) cgptl
		ON		cgptl.Taxon_List_Key = tl.Taxon_List_Key
	WHERE		cgptl.Taxon_List_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.[usp_TaxonList_Select_NonPreferredLists]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_TaxonList_Select_NonPreferredLists]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.[usp_TaxonList_Select_NonPreferredLists] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.[usp_TaxonList_Select_NonPreferredLists] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.[usp_TaxonList_Select_NonPreferredLists] TO [Dev - JNCC SQL]
END
GO