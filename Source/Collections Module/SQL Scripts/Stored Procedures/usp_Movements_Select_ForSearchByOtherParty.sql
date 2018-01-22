If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForSearchByOtherParty]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForSearchByOtherParty]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForSearchByOtherParty] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@MovementGroupType TINYINT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Movements data based on the search parameter for the Other Party
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SearchText			Text to be used for search
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-06
--

SET NOCOUNT ON

IF @MovementGroupType = 0
BEGIN
	IF @SortOrderIndex = 0
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 0) OR (M.Movement_Type = 1))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
	END
	ELSE IF @SortOrderIndex = 1
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 0) OR (M.Movement_Type = 1))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Number
	END
	ELSE IF @SortOrderIndex = 2
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 0) OR (M.Movement_Type = 1))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))		
		ORDER BY M.Movement_Type, M.Number
	END
END
ELSE IF @MovementGroupType = 1
BEGIN
	IF @SortOrderIndex = 0
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 2) OR (M.Movement_Type = 3))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
	END
	ELSE IF @SortOrderIndex = 1
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 2) OR (M.Movement_Type = 3))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Number
	END
	ELSE IF @SortOrderIndex = 2
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 2) OR (M.Movement_Type = 3))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))			
		ORDER BY M.Movement_Type, M.Number
	END
END
ELSE IF @MovementGroupType = 2
BEGIN
	IF @SortOrderIndex = 0
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 4) OR (M.Movement_Type = 5) OR (M.Movement_Type = 6)
				OR (M.Movement_Type = 7) OR (M.Movement_Type = 8) OR (M.Movement_Type = 9))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
	END
	ELSE IF @SortOrderIndex = 1
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 4) OR (M.Movement_Type = 5) OR (M.Movement_Type = 6)
				OR (M.Movement_Type = 7) OR (M.Movement_Type = 8) OR (M.Movement_Type = 9))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY Number
	END
	ELSE IF @SortOrderIndex = 2
	BEGIN
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				M.Display_Caption, --DISTINCT removes duplicate records due to two direction types
				CASE WHEN N.Organisation = 0 THEN 
					dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
				ELSE 
					CASE WHEN O.Acronym IS NULL THEN 
						O.Full_Name
					ELSE 
						O.Acronym + ', ' + O.Full_Name
					END
				END AS Hint

		FROM MOVEMENT M
			LEFT JOIN 
				MOVEMENT_DIRECTION MD 
			ON MD.Movement_Key = M.Movement_Key
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.MOVEMENT_DIRECTION_KEY = MCU.MOVEMENT_DIRECTION_KEY
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.COLLECTION_UNIT_KEY = CU.COLLECTION_UNIT_KEY
			LEFT JOIN
				[NAME] N	
			ON M.Other_Party_Name_Key = N.Name_Key
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
		WHERE ((I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL))
			AND ((M.Movement_Type = 4) OR (M.Movement_Type = 5) OR (M.Movement_Type = 6)
				OR (M.Movement_Type = 7) OR (M.Movement_Type = 8) OR (M.Movement_Type = 9))
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
		ORDER BY M.Movement_Type, M.Number
	END
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForSearchByOtherParty') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForSearchByOtherParty'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByOtherParty TO [Dev - JNCC SQL]
END

GO