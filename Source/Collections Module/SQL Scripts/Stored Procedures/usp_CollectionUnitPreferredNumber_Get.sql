/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitPreferredNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitPreferredNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the preferred number for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Number	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 15/06/07 9:46 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitPreferredNumber_Get]
	@Key char(16),
	@Number varchar(100) OUTPUT
AS
	
	SELECT	TOP 1 	@Number = ISNULL(CN.Number, 'Unknown')
	FROM		Collection_Unit CU
	INNER JOIN Collection_Unit_Number CN ON CN.Collection_Unit_Key=CU.Collection_Unit_Key
	INNER JOIN Concept C ON C.Concept_Key = CN.Type_Concept_Key
	WHERE		CU.Collection_Unit_Key = @Key
	AND CN.Preferred=1

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitPreferredNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitPreferredNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitPreferredNumber_Get TO [Dev - JNCC SQL]
END

GO