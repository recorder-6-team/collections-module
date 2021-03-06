/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitAccessionNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitAccessionNumber_Get]
GO

/*===========================================================================*\
  Description:	Returns the accession number for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Number	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 24/03/04 16:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitAccessionNumber_Get]
	@Key char(16),
	@Number varchar(100) OUTPUT
AS
	
	SELECT	TOP 1 	@Number = M.Number
	FROM		Collection_Unit CU
	INNER JOIN 	Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	WHERE		CU.Collection_Unit_Key = @Key
	AND		MD.OutBound = 0			-- Inbound = 0
	AND		M.Movement_Type IN (0, 1)	-- Accession = 0, Exchange = 1
	ORDER BY 	Exp_Vague_Date_Start DESC

	-- Can't have it in select, in case NO records are returned.
	SET @Number = ISNULL(@Number, 'Unknown')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitAccessionNumber_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitAccessionNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitAccessionNumber_Get TO [Dev - JNCC SQL]
END

GO