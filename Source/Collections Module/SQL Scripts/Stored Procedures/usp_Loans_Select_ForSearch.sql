If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Loans_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Loans_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Loans_Select_ForSearch] 
@UserDomainMask INT,
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

--DISTINCT removes duplicate records due to two direction types.
SELECT DISTINCT M.Movement_Key AS Item_Key, Display_Caption AS DisplayTerm
FROM 
	MOVEMENT M
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON M.Movement_Key = MD.Movement_Key 
			AND (M.Movement_Type = 2 OR M.Movement_Type = 3)
		LEFT JOIN 
			(MOVEMENT_COLLECTION_UNIT MCU 
			INNER JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)) 
			)
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		WHERE ((CU.Collection_Unit_Key IS NOT NULL) 
			OR ((MCU.Collection_Unit_Key IS NULL) AND ((M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))))
			AND Search_Caption LIKE @SearchText + '%'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Loans_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Loans_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Loans_Select_ForSearch TO [Dev - JNCC SQL]
END

GO