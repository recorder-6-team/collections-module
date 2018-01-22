If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns top level Collections data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText 		Search text used to find collections
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-08
--

SET NOCOUNT ON
SET NO_BROWSETABLE OFF

SELECT C.Collection_Unit_Key AS Item_Key, 
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS DisplayTerm,
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS SearchTerm

FROM 
	(COLLECTION C
	INNER JOIN
   	    COLLECTION_UNIT CU 
   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	)
	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT MCU
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
			AND (MD.Outbound = 0)
		INNER JOIN
			MOVEMENT M
		ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
		LEFT JOIN
			(CONCEPT CON
			INNER JOIN 
				TERM T
			ON CON.Term_Key = T.Term_Key)
		ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
	ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
WHERE C.Item_Name LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [Dev - JNCC SQL]
END

GO