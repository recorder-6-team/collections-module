If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForSearchByDepartment]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByDepartment]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByDepartment] 
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Enquiries data based on the search parameter for Enquirer
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption, OD.Item_Name AS Hint
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		INNER JOIN
			INDIVIDUAL I
		ON E.Answered_By_Name_Key = I.Name_Key
		INNER JOIN
			ORGANISATION_DEPARTMENT OD
		ON I.Organisation_Department_Key = OD.Organisation_Department_Key
			AND OD.Item_Name LIKE @SearchText + '%'
		ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
ELSE IF @SortOrderIndex = 1
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption, OD.Item_Name AS Hint
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		INNER JOIN
			INDIVIDUAL I
		ON E.Answered_By_Name_Key = I.Name_Key
		INNER JOIN
			ORGANISATION_DEPARTMENT OD
		ON I.Organisation_Department_Key = OD.Organisation_Department_Key
			AND OD.Item_Name LIKE @SearchText + '%'
		ORDER BY T.PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForSearchByDepartment') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForSearchByDepartment'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByDepartment TO [Dev - JNCC SQL]
END

GO
