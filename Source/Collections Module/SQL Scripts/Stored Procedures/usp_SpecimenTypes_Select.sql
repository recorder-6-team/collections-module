/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns the Specimen Types for the SpecimenGeneral frame.

  Parameters:	@Mask	The Specimen Unit Mask value

  Created:	Setember 2003

  Last revision information:
    $Revision: 3 $
    $Date: 20/02/04 16:25 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenTypes_Select]
	@Mask int
AS

SET NOCOUNT ON

	SELECT ct.concept_key, ct.plaintext

	FROM VW_ConceptTerm ct
	
	LEFT JOIN concept_group cg ON cg.concept_group_key = ct.concept_group_key
	LEFT JOIN local_domain ld ON ld.local_domain_key = cg.local_domain_Key
	LEFT JOIN domain d ON d.domain_key = ld.domain_key

	WHERE CT.concept_group_key = 'SYSTEM000000000J'
	OR ((D.Domain_Mask & @Mask > 0)
	AND CG.Item_Name = 'Specimen Type')
	AND CT.Is_Current = 1
	AND CT.List_Preferred = 1

	ORDER BY CT.PlainText

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [Dev - JNCC SQL]
END

GO