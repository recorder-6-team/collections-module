If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForSearchByValuedBy]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValuedBy]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValuedBy] 
@UserID CHAR(16),
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Valuations based on the search parameter for the Valued By
--	@SortOrderIndex		Index determining Sort Order
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-17
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
		CASE WHEN N.Organisation = 0 THEN 
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		ELSE 
			CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END
		END AS Hint
	
	FROM 
		VALUATION V
		INNER JOIN
			[USER] U
		ON U.Name_Key = @UserID
		INNER JOIN
			[NAME] N
		ON V.Valued_By_Name_Key = N.Name_Key
		LEFT JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = V.Type_Concept_Key
		LEFT JOIN
			INDIVIDUAL I
		ON N.Name_Key = I.NAME_KEY
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
		LEFT JOIN
			ORGANISATION O
		ON N.Name_Key = O.NAME_KEY
			AND 
			(CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END LIKE @SearchText + '%')
	WHERE (I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL)
		AND U.ALLOW_FINANCE = 1
	ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
ELSE IF @SortOrderIndex = 1
	SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
		CASE WHEN N.Organisation = 0 THEN 
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		ELSE 
			CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END
		END AS Hint
	
	FROM 
		VALUATION V
		INNER JOIN
			[USER] U
		ON U.Name_Key = @UserID
		INNER JOIN
			[NAME] N
		ON V.Valued_By_Name_Key = N.Name_Key
		LEFT JOIN
			VW_ConceptTerm CT
		ON CT.Concept_Key = V.Type_Concept_Key	
		LEFT JOIN
			INDIVIDUAL I
		ON N.Name_Key = I.NAME_KEY
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
		LEFT JOIN
			ORGANISATION O
		ON N.Name_Key = O.NAME_KEY
			AND 
			(CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END LIKE @SearchText + '%')
	WHERE (I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL)
		AND U.ALLOW_FINANCE = 1
	ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForSearchByValuedBy') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForSearchByValuedBy'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValuedBy TO [Dev - JNCC SQL]
END

GO