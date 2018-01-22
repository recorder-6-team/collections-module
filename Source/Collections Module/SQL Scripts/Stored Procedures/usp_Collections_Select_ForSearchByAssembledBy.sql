If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByAssembledBy]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAssembledBy]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAssembledBy] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Assembled By
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

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
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
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
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

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
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
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
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

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
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
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
	ORDER BY M.Number


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByAssembledBy') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByAssembledBy'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [Dev - JNCC SQL]
END

GO