/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectArea_Select_ForTaxon_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectArea_Select_ForTaxon_Get]
GO
    

/*===========================================================================*\
  Description:	Returns the subject area key for a taxon

  Parameters:	
		@Key - Concept_Key
		@Subject_Area_Key OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 20/02/04 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectArea_Select_ForTaxon_Get]
  @Key CHAR(16),
	@Subject_Area_Key CHAR(16) OUTPUT
 AS

SELECT @Subject_Area_Key = D.Subject_Area_Key 
FROM Domain D
INNER JOIN Local_Domain LD ON LD.Domain_Key=D.Domain_Key
INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
INNER JOIN Concept C ON C.Concept_Group_Key=CG.Concept_Group_Key
INNER JOIN Taxon_Dictionary_Concept_Mapping CM ON CM.Concept_Key=C.Concept_Key
		AND CM.Taxon_List_Item_Key=@Key
	
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectArea_Select_ForTaxon_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectArea_Select_ForTaxon_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Select_ForTaxon_Get TO [Dev - JNCC SQL]
END

GO


