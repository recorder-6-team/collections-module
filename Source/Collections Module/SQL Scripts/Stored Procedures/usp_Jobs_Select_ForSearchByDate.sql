If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Jobs_Select_ForSearchByDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Jobs_Select_ForSearchByDate]
GO

CREATE PROCEDURE [dbo].[usp_Jobs_Select_ForSearchByDate] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns Jobs data based on the search parameter the date range from when the job started until it finished
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SortOrderIndex		Index determining Sort Order
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-31
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT Conservation_Job_Key AS Item_Key, Display_Caption, Job_Number AS Hint
	FROM CONSERVATION_JOB
	WHERE ((Domain_Mask & @UserDomainMask > 0) OR (Entered_Session_ID = @SessionID) 
			OR (Changed_Session_ID = @SessionID) OR (Domain_Mask = 0))
		AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, From_Vague_Date_Start, To_Vague_Date_End) = 1
	ORDER BY From_Vague_Date_Start DESC, From_Vague_Date_End DESC, From_Vague_Date_Type, Item_Name
ELSE IF @SortOrderIndex = 1
	SELECT Conservation_Job_Key AS Item_Key, Display_Caption, Job_Number AS Hint
	FROM CONSERVATION_JOB
	WHERE ((Domain_Mask & @UserDomainMask > 0) OR (Entered_Session_ID = @SessionID) 
			OR (Changed_Session_ID = @SessionID) OR (Domain_Mask = 0))
		AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, From_Vague_Date_Start, To_Vague_Date_End) = 1
	ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Jobs_Select_ForSearchByDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Jobs_Select_ForSearchByDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByDate TO [Dev - JNCC SQL]
END

GO