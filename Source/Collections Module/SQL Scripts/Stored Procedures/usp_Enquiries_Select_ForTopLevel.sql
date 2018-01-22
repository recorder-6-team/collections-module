If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForTopLevel] 
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Enquiries data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Enquiry_Key FROM Enquiry

IF @SortOrderIndex = 0
BEGIN
		SELECT Enquiry_Key AS Item_Key, E.Display_Caption
		FROM ENQUIRY E
			INNER JOIN CONCEPT C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
			INNER JOIN @Search S ON S.ItemKey=E.Enquiry_Key
		ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT Enquiry_Key AS Item_Key, E.Display_Caption
		FROM ENQUIRY E
			INNER JOIN CONCEPT C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
			INNER JOIN @Search S ON S.ItemKey=E.Enquiry_Key
		ORDER BY T.PlainText
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO
