/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_GetConceptGroupLabel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_GetConceptGroupLabel]
GO

/*===========================================================================*\
  Description:	Returns the concept group label for a domain

  Parameters:	@Domain - key of the domain
							@Language - ISO code for the preferred language.  If not available
							then English is used.
							@ConceptGroupLabel - Output parameter

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_GetConceptGroupLabel]
	@Domain CHAR(16),
  @Language CHAR(2),
  @ConceptGroupLabel VARCHAR(100) OUTPUT

AS
  
DECLARE @LocalDomainCount int

SELECT @LocalDomainCount = COUNT(*) FROM Local_Domain WHERE Domain_Key=@Domain

IF @LocalDomainCount=1
  SELECT @ConceptGroupLabel=Concept_Group_Label 
  FROM Local_Domain 
  WHERE Domain_Key=@Domain
ELSE BEGIN
  IF EXISTS(SELECT * FROM Local_Domain WHERE Domain_Key=@Domain AND Language_Key=@Language)
    SELECT @ConceptGroupLabel=Concept_Group_Label 
    FROM Local_Domain 
    WHERE Domain_Key=@Domain
    AND Language_Key=@Language
  ELSE 
  IF EXISTS(SELECT * FROM Local_Domain WHERE Domain_Key=@Domain AND Language_Key='en') 
    SELECT @ConceptGroupLabel=Concept_Group_Label 
    FROM Local_Domain 
    WHERE Domain_Key=@Domain
    AND Language_Key='en'
END
  
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetConceptGroupLabel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_GetConceptGroupLabel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GetConceptGroupLabel TO [Dev - JNCC SQL]
END

GO