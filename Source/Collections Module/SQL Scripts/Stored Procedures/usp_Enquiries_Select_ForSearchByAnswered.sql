If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForSearchByAnswered]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByAnswered]
GO

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForSearchByAnswered] 
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Enquiries that have been answered
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			AND E.Answered = 1
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type

ELSE IF @SortOrderIndex = 1
	SELECT Enquiry_Key AS Item_Key, E.Display_Caption
	
	FROM 
		ENQUIRY E
		INNER JOIN 
			CONCEPT C
		ON E.Enquiry_Type_Concept_Key = C.Concept_Key
			AND E.Answered = 1
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
		ORDER BY T.PlainText

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForSearchByAnswered') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForSearchByAnswered'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForSearchByAnswered TO [Dev - JNCC SQL]
END

GO
