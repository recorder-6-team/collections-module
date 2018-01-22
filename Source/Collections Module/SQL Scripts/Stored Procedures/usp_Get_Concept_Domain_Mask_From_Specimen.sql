/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Get_Concept_Domain_Mask_From_Specimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Get_Concept_Domain_Mask_From_Specimen]
GO

/*===========================================================================*\
  Description:	Returns the Domain Mask for the given Specimen Key.
		Use the preferred determination to find the 
		concept key to use.

  Parameters:	@SpecimenKey	Key of specimen we want the mask for.
		@DomainMask	Output. Contains the mask.

  Created:	July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 14/04/04 17:50 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Get_Concept_Domain_Mask_From_Specimen]
	@SpecimenKey char(16), 
	@DomainMask int  OUTPUT
AS
	-- Find the preferred concept for given specimen
	DECLARE	@ConceptKey char(16)

	SELECT 		@ConceptKey = CASE SU.Life_Sciences WHEN 0 THEN CPref.Concept_Key ELSE TDCM.Concept_Key END
	FROM 		Specimen_Unit SU
	INNER JOIN 	Specimen_Unit SUnit ON SUnit.Collection_Unit_Key = SU.Collection_Unit_Key
	LEFT JOIN 	Determination D ON D.Determination_Key = SUnit.Preferred_Determination_Key
	LEFT JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
	LEFT JOIN 	Term TDet ON TDet.Term_Key = C.Term_Key
	LEFT JOIN 	Concept CPref 
		ON CPref.Meaning_Key = C.Meaning_Key 
		AND CPref.List_Preferred = 1 
		AND CPref.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Term TPref ON TPref.Term_Key = CPref.Term_Key
	LEFT JOIN 	Taxon_Determination TD ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
	LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	WHERE 		SU.Collection_Unit_Key = @SpecimenKey


	-- Use found concept to get and return mask
	EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @DomainMask OUTPUT
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Get_Concept_Domain_Mask_From_Specimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Get_Concept_Domain_Mask_From_Specimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask_From_Specimen TO [Dev - JNCC SQL]
END

GO