
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns concept group key and local domain key for the named concept
		group associated with the domain/local domain of the preferred
		determination for the given collection unit.
		System concept group is always returned for Collections and Stores,
		as there is no 100% sure way to identify a single domain for those.

  Parameters:	@Key		
		@DomainConceptGroupName
				Name of the domain concept group where new parameters
				added by users should go. If the concept group exists
				for the domain, its key will be returned, ready for use.

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 12/07/04 17:09 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]
	@Key char(16),
	@DomainConceptGroupName varchar(100),
	@TopLevelNodeContext int
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Use left join at the end to always get the Local_Domain_Key, 
	  even if @DomainConceptGroupName doesn't exist for the 
	  determination's domain.
	\*-------------------------------------------------------------*/
	IF @TopLevelNodeContext = 1	-- i.e. if NodeContext = ncSpecimen
		SELECT 		LD2.Local_Domain_Key, CG2.Concept_Group_Key
		FROM 		Specimen_Unit SU
		LEFT JOIN	Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	(Taxon_Determination TD 
					INNER JOIN Taxon_Dictionary_Concept_Mapping AS TDCM ON TD.Taxon_List_Item_Key = TDCM.Taxon_List_Item_Key)
				ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		INNER JOIN	Concept C ON (C.Concept_Key = D.Concept_Key) OR (C.Concept_Key = TDCM.Concept_Key)
		INNER JOIN	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Local_Domain LD2 ON LD2.Domain_Key = LD.Domain_Key
		LEFT JOIN	Concept_Group CG2 ON CG2.Local_Domain_Key = LD2.Local_Domain_Key
				AND CG2.Item_Name = @DomainConceptGroupName
		WHERE		SU.Collection_Unit_Key = @Key
	ELSE IF @DomainConceptGroupName = 'Measurement Parameters' 
		SELECT 		Local_Domain_Key, Concept_Group_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key = 'SYSTEM000000000E'
	ELSE IF @DomainConceptGroupName = 'Descriptor Parameters'
		SELECT 		Local_Domain_Key, Concept_Group_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key = 'SYSTEM000000000W'

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainConceptGroup_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainConceptGroup_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO