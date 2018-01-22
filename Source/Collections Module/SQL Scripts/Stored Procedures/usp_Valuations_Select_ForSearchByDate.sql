If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForSearchByDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByDate]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByDate] 
@UserID CHAR(16),
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Valuations based on the search parameter for the Valuation Date
--	@SortOrderIndex		Index determining Sort Order
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-06
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
		dbo.ufn_GetDateFromVagueDate(Vague_Date_Start, Vague_Date_End, Vague_Date_Type) AS Hint
	FROM 
		VALUATION V
		INNER JOIN
			[USER] U
		ON U.Name_Key = @UserID
		LEFT JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = V.Type_Concept_Key
	WHERE U.ALLOW_FINANCE = 1
		AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Vague_Date_Start, Vague_Date_End) = 1
	ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
ELSE IF @SortOrderIndex = 1
	SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
		dbo.ufn_GetDateFromVagueDate(Vague_Date_Start, Vague_Date_End, Vague_Date_Type) AS Hint
	FROM 
		VALUATION V
		INNER JOIN
			[USER] U
		ON U.Name_Key = @UserID
		LEFT JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = V.Type_Concept_Key
	WHERE U.ALLOW_FINANCE = 1
		AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Vague_Date_Start, Vague_Date_End) = 1
	ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForSearchByDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForSearchByDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByDate TO [Dev - JNCC SQL]
END

GO