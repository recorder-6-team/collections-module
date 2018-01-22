If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DisposalDetails_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DisposalDetails_Select_ForMovement]
GO

CREATE PROCEDURE [dbo].[usp_DisposalDetails_Select_ForMovement] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Disposal Details for a specified movement
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-21
--
SET NOCOUNT ON

SELECT DISTINCT MM.Movement_Of_Material_Key AS Item_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type
FROM
MOVEMENT M
	INNER JOIN 
		MOVEMENT_DIRECTION MD 
	ON M.Movement_Key = MD.Movement_Key 
		AND (M.Movement_Key = @ParentKey)
		AND (MD.Outbound = 1)
	INNER JOIN 
		MOVEMENT_OF_MATERIAL MM
	ON MD.Movement_Direction_Key = MM.Movement_Direction_Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DisposalDetails_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DisposalDetails_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DisposalDetails_Select_ForMovement TO [Dev - JNCC SQL]
END

GO