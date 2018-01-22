/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Get_Concept_Domain_Mask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Get_Concept_Domain_Mask]
GO

/*===========================================================================*\
  Description:	Returns the Domain Mask for the given concept Key.

  Parameters:	@ConceptKey	Key of concept we want the mask for.
		@DomainMask	Output. Contains the mask.

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Get_Concept_Domain_Mask]
	@ConceptKey char(16), 
	@DomainMask int OUTPUT
AS
	SELECT		@DomainMask = Domain_Mask
	FROM		Concept C 
	INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN	Domain D 		ON D.Domain_Key = LD.Domain_Key
	WHERE		Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Get_Concept_Domain_Mask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Get_Concept_Domain_Mask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Get_Concept_Domain_Mask TO [Dev - JNCC SQL]
END

GO