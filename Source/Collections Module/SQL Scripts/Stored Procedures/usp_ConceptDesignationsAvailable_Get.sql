/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignationsAvailable_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignationsAvailable_Get]
GO

/*===========================================================================*\
  Description:	Returns the Concept Group Key of a concept group called 
		'Concept Designation Types' in the current concept group's domain

  Parameters: 
	@Key 						Concept Group Key
	@DesignationTypesGroupKey 	OUTPUT

  Created:	April 2004

  Last revision information:
    $Revision: 2 $
    $Date: 10/09/08 14:36 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignationsAvailable_Get]
	@Key 						CHAR(16),
	@DesignationTypesGroupKey 	CHAR(16) OUTPUT
AS
	SELECT 	@DesignationTypesGroupKey = CGD.Concept_Group_Key
	FROM 	Concept_Group 	CGD
	JOIN 	Local_Domain	LDD ON 	LDD.Local_Domain_Key	=	CGD.Local_Domain_Key
	JOIN 	Local_Domain 	LDC ON 	LDC.Domain_Key			=	LDD.Domain_Key
	JOIN 	Concept_Group	CGC ON 	CGC.Local_Domain_Key	=	LDC.Local_Domain_Key
								AND	CGC.Concept_Group_Key	=	@Key
	WHERE 	CGD.Item_Name		=	'Concept Designation Types'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignationsAvailable_Get') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ConceptDesignationsAvailable_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [Dev - JNCC SQL]
END

GO