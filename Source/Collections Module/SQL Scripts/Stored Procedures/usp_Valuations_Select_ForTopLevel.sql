If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForTopLevel] 
@UserID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Valuations data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-25
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Valuation_Key FROM Valuation

IF @SortOrderIndex = 0
BEGIN
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Type_Concept_Key	
		INNER JOIN @Search S ON S.ItemKey = V.Valuation_Key
		WHERE U.ALLOW_FINANCE = 1
		ORDER BY V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT V.Valuation_Key AS Item_Key, V.Display_Caption
		FROM 
			VALUATION V
			INNER JOIN
				[USER] U
			ON U.Name_Key = @UserID
			LEFT JOIN
				VW_ConceptTerm CT
			ON CT.Concept_Key = V.Type_Concept_Key	
		INNER JOIN @Search S ON S.ItemKey = V.Valuation_Key
		WHERE U.ALLOW_FINANCE = 1
		ORDER BY CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO