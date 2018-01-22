/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainConceptGroup_Name_Get]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_DomainConceptGroup_Name_Get]
GO

/*===========================================================================*\
  Description:	Returns the name of the Domain and Concept Group

  Parameters:	@Key	Concept group key
		@Name	Domain and Concept group name

  Created:	13th August 2004

  Last revision information:
	$Revision: 2 $
	$Date: 13/08/04 17:16 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainConceptGroup_Name_Get]
	@Key CHAR(16),
	@Name VARCHAR(201) OUTPUT
AS
	SET NOCOUNT ON

	DECLARE @DomainName varchar(100),
		@ConceptGroupName varchar(100)
	
	SELECT 		@ConceptGroupName = CG.Item_Name,
			@DomainName = D.Item_Name
	FROM 		Concept_Group AS CG
	INNER JOIN	Local_Domain AS LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN	Domain AS D ON D.Domain_Key = LD.Domain_Key
	WHERE		Concept_Group_Key = @Key

	SET @Name = IsNull(@DomainName + '/', '---/') + IsNull(@ConceptGroupName, '---')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainConceptGroup_Name_Get') AND SysStat & 0xf = 4)
BEGIN
  PRINT 'Setting up security on procedure usp_DomainConceptGroup_Name_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [Dev - JNCC SQL]
END
GO