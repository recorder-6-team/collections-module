/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainConceptGroup_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns concept group key and local domain key for the named concept
		group associated with the domain/local domain of the preferred
		determination for the given occurrence.

  Parameters:	
	@Key
	@DomainConceptGroupName	Name of the domain concept group where new parameters
				added by users should go. If the concept group exists
				for the domain, its key will be returned, ready for use.

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 16/04/04 18:04 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForOccurrence]
	@Key char(16),
	@DomainConceptGroupName varchar(100)
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Use left join at the end to always get the Local_Domain_Key, 
	  even if @DomainConceptGroupName doesn't exist for the 
	  determination's domain.
	\*-------------------------------------------------------------*/
	SELECT		LD.Local_Domain_Key, CG2.Concept_Group_Key
	FROM		Local_Domain LD
	INNER JOIN	Concept_Group CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	INNER JOIN	Concept C ON C.Concept_Group_Key = CG.Concept_Group_Key
	INNER JOIN	Determination D ON D.Concept_Key = C.Concept_Key AND D.Preferred = 1
	LEFT JOIN	Concept_Group CG2 
				ON CG2.Local_Domain_Key = LD.Local_Domain_Key 
				AND CG2.Item_Name = @DomainConceptGroupName
	-- Get all determinations, directly linked to Occurrence table and via scenic route, through Specimen_Field_Data.
	WHERE		D.Determination_Key IN (
				SELECT	D.Determination_Key
				FROM	Determination D 
				WHERE	D.Occurrence_Key = @Key
			UNION
				SELECT	D.Determination_Key
				FROM	Specimen_Field_Data SFD 
				JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
				JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
				WHERE	SFD.Occurrence_Key = @Key
			)

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainConceptGroup_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainConceptGroup_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForOccurrence TO [Dev - JNCC SQL]
END