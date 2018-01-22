If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Condition Checks data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-18
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Conservation_Check_Key FROM Conservation_Check

IF @SortOrderIndex = 0
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
		INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, T.Plaintext, CC.Ref_Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
	INNER JOIN 
		CONCEPT C
	ON CC.Type_Concept_Key = C.Concept_Key
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
	INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
	INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Ref_Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO