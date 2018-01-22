If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForSearchByType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByType]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Tasks data based on the search parameter for Type
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			dbo.Search_Term AS ST
		ON C.Concept_Key = ST.Concept_Key
			AND ST.Plaintext LIKE @SearchText + '%'
	ORDER BY CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
		dbo.ufn_GetConservationStatus(Status)

ELSE IF @SortOrderIndex = 1
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			dbo.Search_Term AS ST
		ON C.Concept_Key = ST.Concept_Key
			AND ST.Plaintext LIKE @SearchText + '%'
	ORDER BY dbo.ufn_GetConservationStatus(Status),
		CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForSearchByType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [Dev - JNCC SQL]
END

GO