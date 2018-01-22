If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForSearchByEnquirer]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByEnquirer]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByEnquirer] 
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
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption,
		CASE WHEN (E.Vague_Enquirer LIKE @SearchText + '%') THEN
			E.Vague_Enquirer
		ELSE
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		END AS Hint
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		LEFT JOIN
			INDIVIDUAL I
		ON E.Enquirer_Name_Key = I.Name_Key
	WHERE (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%') 
		OR (E.Vague_Enquirer LIKE @SearchText + '%')
	ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
ELSE IF @SortOrderIndex = 1
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption,
		CASE WHEN (E.Vague_Enquirer LIKE @SearchText + '%') THEN
			E.Vague_Enquirer
		ELSE
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		END AS Hint
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		LEFT JOIN
			INDIVIDUAL I
		ON E.Enquirer_Name_Key = I.Name_Key
	WHERE (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%') 
		OR (E.Vague_Enquirer LIKE @SearchText + '%')
	ORDER BY T.PlainText


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForSearchByEnquirer') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForSearchByEnquirer'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByEnquirer TO [Dev - JNCC SQL]
END

GO
