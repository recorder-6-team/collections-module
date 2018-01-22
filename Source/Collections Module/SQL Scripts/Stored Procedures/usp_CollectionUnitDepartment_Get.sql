/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitDepartment_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitDepartment_Get]
GO

/*===========================================================================*\
  Description:	Returns the department for a collection unit record.

  Parameters:	@Key		Collection unit key
		@Department	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitDepartment_Get]
	@Key char(16),
	@Department varchar(100) OUTPUT
AS
	DECLARE	@DeptName varchar(100),
		@ReceiverKey char(16)

	/*-------------------------------------------------------------*\
	  Get the relevant data.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1 	@DeptName = OD.Item_Name, @ReceiverKey = MD.Receiver_Name_Key
	FROM		Movement_Collection_Unit MCU 
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
	LEFT JOIN 	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	ORDER BY 	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Now work out what to display.
	\*-------------------------------------------------------------*/
	IF @DeptName IS NOT NULL
		SET @Department = @DeptName
	ELSE
	IF @ReceiverKey IS NOT NULL
		SET @Department = dbo.ufn_GetFormattedName(@ReceiverKey)
	ELSE
		SET @Department = 'Unknown'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitDepartment_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitDepartment_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitDepartment_Get TO [Dev - JNCC SQL]
END

GO