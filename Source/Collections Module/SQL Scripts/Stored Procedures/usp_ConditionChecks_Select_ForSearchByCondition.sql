If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForSearchByCondition]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearchByCondition]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearchByCondition] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Condition Checks data based on the search parameter for Condition
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SearchText			Text to be used for search
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, CT.Item_Name AS Hint
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = CC.Condition_Concept_Key
		AND CT.Item_Name LIKE @SearchText + '%'
	ORDER BY CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, CC.Ref_Number
ELSE IF @SortOrderIndex = 1
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, CT.Item_Name AS Hint
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = CC.Condition_Concept_Key
		AND CT.Item_Name LIKE @SearchText + '%'
	ORDER BY CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForSearchByCondition') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForSearchByCondition'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCondition TO [Dev - JNCC SQL]
END

GO