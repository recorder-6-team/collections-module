If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Jobs_Select_ForSearchByStaff]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Jobs_Select_ForSearchByStaff]
GO

CREATE PROCEDURE [dbo].[usp_Jobs_Select_ForSearchByStaff] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(10)

AS

--  DESCRIPTION
--  Returns Jobs data based on the search parameter for Staff
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
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CJ.Conservation_Job_Key AS Item_Key, CJ.Display_Caption,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Hint
	FROM 
		CONSERVATION_JOB CJ
		INNER JOIN 
			CONSERVATION_JOB_STAFF CJS
		ON CJ.Conservation_Job_Key = CJS.Conservation_Job_Key
			AND ((CJ.Domain_Mask & @UserDomainMask > 0) OR (CJ.Entered_Session_ID = @SessionID) 
				OR (CJ.Changed_Session_ID = @SessionID) OR (CJ.Domain_Mask = 0))
		INNER JOIN
			INDIVIDUAL I
		ON CJS.Name_Key = I.Name_Key
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
	ORDER BY From_Vague_Date_Start DESC, From_Vague_Date_End DESC, From_Vague_Date_Type, Item_Name

ELSE IF @SortOrderIndex = 1
	SELECT CJ.Conservation_Job_Key AS Item_Key, CJ.Display_Caption,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Hint
	FROM 
		CONSERVATION_JOB CJ
		INNER JOIN 
			CONSERVATION_JOB_STAFF CJS
		ON CJ.Conservation_Job_Key = CJS.Conservation_Job_Key
			AND ((CJ.Domain_Mask & @UserDomainMask > 0) OR (CJ.Entered_Session_ID = @SessionID) 
				OR (CJ.Changed_Session_ID = @SessionID) OR (CJ.Domain_Mask = 0))
		INNER JOIN
			INDIVIDUAL I
		ON CJS.Name_Key = I.Name_Key
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
	ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Jobs_Select_ForSearchByStaff') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Jobs_Select_ForSearchByStaff'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForSearchByStaff TO [Dev - JNCC SQL]
END

GO