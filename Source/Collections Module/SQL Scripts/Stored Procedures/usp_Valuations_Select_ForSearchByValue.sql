If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForSearchByValue]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValue]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForSearchByValue] 
@UserID CHAR(16),
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Valuations based on the search parameter for the amount
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
DECLARE @GT BIT, @LT BIT, @GTET BIT, @LTET BIT, @ET BIT, @OperatorFound BIT
DECLARE @Index INT
DECLARE @Amount MONEY

SET @GT = 0
SET @LT = 0
SET @GTET = 0
SET @LTET = 0
SET @ET = 0
SET @OperatorFound = 0

SET @Index = CHARINDEX('>=', @SearchText)
IF @Index > 0 
BEGIN
	SET @GTET = 1
	SET @OperatorFound = 1
	--Remove operator from searchtext
	SET @SearchText = REPLACE(@SearchText, '>=', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('<=', @SearchText)
IF @Index > 0 
BEGIN
	SET @LTET = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '<=', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('>', @SearchText)
IF @Index > 0 
BEGIN
	SET @GT = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '>', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('<', @SearchText)
IF @Index > 0 
BEGIN
	SET @LT = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '<', '')
	GOTO OperatorFound
END

SET @Index = CHARINDEX('=', @SearchText)
IF @Index > 0 
BEGIN
	SET @ET = 1
	SET @OperatorFound = 1
	SET @SearchText = REPLACE(@SearchText, '=', '')
	GOTO OperatorFound
END
OperatorFound:
IF ISNUMERIC(RTRIM(LTRIM(@SearchText)))=1
	SET @Amount = CONVERT(MONEY, RTRIM(LTRIM(@SearchText)))
ELSE
	SET @Amount=NULL



IF @SortOrderIndex = 0
BEGIN
	IF (@OperatorFound = 0) OR ((@OperatorFound = 1) AND (@ET = 1))
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount = @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @GTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount >= @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @LTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount <= @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @GT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount > @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
	ELSE IF @LT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount < @Amount
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
END
ELSE IF @SortOrderIndex = 1
BEGIN
	IF (@OperatorFound = 0) OR ((@OperatorFound = 1) AND (@ET = 1))
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount = @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @GTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount >= @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @LTET = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount <= @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @GT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount > @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
	ELSE IF @LT = 1
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption, 
			CONVERT(VARCHAR(10), V.Value_Amount, 0) + ' ' + CT.Item_Name AS Hint
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Currency_Concept_Key
		WHERE U.ALLOW_FINANCE = 1
			AND V.Value_Amount < @Amount
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForSearchByValue') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForSearchByValue'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForSearchByValue TO [Dev - JNCC SQL]
END

GO