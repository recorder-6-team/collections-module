/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the CollectionUnitName table.

  Parameters:	@Key	Enquiry key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/02/04 17:41 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	CN.Collection_Unit_Name_Key,
	dbo.ufn_GetFormattedName(CN.Collection_Unit_Key) AS CollectionUnitName,
	CN.Name_Key,
	dbo.ufn_GetFormattedName(CN.Name_Key) AS RelatedName,
	CN.Relation_Type_Concept_Key,
	CTT.Plaintext AS TypeTerm,
	CN.Vague_Date_Start,
	CN.Vague_Date_End,
	CN.Vague_Date_Type,
	CN.Comment,
	CN.Custodian,
	CN.[Timestamp]
FROM Collection_Unit_Name CN
INNER JOIN VW_ConceptTerm CTT ON CTT.Concept_Key=Relation_Type_Concept_Key
WHERE CN.Collection_Unit_Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Select TO [Dev - JNCC SQL]
END

GO