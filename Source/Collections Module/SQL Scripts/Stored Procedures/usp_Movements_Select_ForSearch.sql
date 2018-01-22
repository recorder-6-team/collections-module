If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForSearch] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Movement_Key and Movement caption as search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-23

SET NOCOUNT ON

	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Display_Caption AS DisplayTerm
	FROM MOVEMENT M
	INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
					 AND (M.Movement_Type >= 4 
					 AND M.Movement_Type <= 9)
	LEFT JOIN 
					MOVEMENT_COLLECTION_UNIT MCU 
				ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
				LEFT JOIN 
					COLLECTION_UNIT CU 
				ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key	
	
	WHERE Search_Caption LIKE @SearchText + '%'
	AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
			
	ORDER BY M.Display_Caption
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearch TO [Dev - JNCC SQL]
END

GO