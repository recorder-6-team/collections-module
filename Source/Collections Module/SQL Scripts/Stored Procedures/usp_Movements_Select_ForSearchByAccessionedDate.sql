If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForSearchByAccessionedDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForSearchByAccessionedDate]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForSearchByAccessionedDate] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@SearchText VARCHAR(30),
@MovementGroupType TINYINT,
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Movements data based on the search parameter for Accessioned Date
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

IF @SortOrderIndex = 0
BEGIN
	--Accessions Data
	IF @MovementGroupType = 0
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
	--Loans Data
	ELSE IF @MovementGroupType = 1
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 2 OR M.Movement_Type = 3)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
	--Movements Data
	ELSE IF @MovementGroupType = 2
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 4 OR M.Movement_Type = 5 OR
					M.Movement_Type = 6 OR M.Movement_Type = 7 OR M.Movement_Type = 8 OR M.Movement_Type = 9)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	--Accessions Data
	IF @MovementGroupType = 0
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Number
	--Loans Data
	ELSE IF @MovementGroupType = 1
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 2 OR M.Movement_Type = 3)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Number
	--Movements Data
	ELSE IF @MovementGroupType = 2
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 4 OR M.Movement_Type = 5 OR
					M.Movement_Type = 6 OR M.Movement_Type = 7 OR M.Movement_Type = 8 OR M.Movement_Type = 9)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY Number
END
ELSE IF @SortOrderIndex = 2
BEGIN
	--Accessions Data
	IF @MovementGroupType = 0
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY M.Movement_Type, M.Number
	--Loans Data
	ELSE IF @MovementGroupType = 1
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 2 OR M.Movement_Type = 3)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY M.Movement_Type, M.Number
	--Movements Data
	ELSE IF @MovementGroupType = 2
		SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, Exp_Vague_Date_Start, Exp_Vague_Date_End,
				Exp_Vague_Date_Type, --DISTINCT removes duplicate records due to two direction types
				Display_Caption,
				Display_Caption AS Hint
		FROM MOVEMENT M
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON M.Movement_Key = MD.Movement_Key				
				AND (M.Movement_Type = 4 OR M.Movement_Type = 5 OR
					M.Movement_Type = 6 OR M.Movement_Type = 7 OR M.Movement_Type = 8 OR M.Movement_Type = 9)
			LEFT JOIN 
				MOVEMENT_COLLECTION_UNIT MCU 
			ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			LEFT JOIN 
				COLLECTION_UNIT CU 
			ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN
				MOVEMENT_OF_OWNERSHIP MOE
			ON MD.Movement_Direction_Key = MOE.Movement_Direction_Key
				AND dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, Exp_Vague_Date_Start, Exp_Vague_Date_End) = 1
		WHERE (CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
				OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)
		ORDER BY M.Movement_Type, M.Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForSearchByAccessionedDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForSearchByAccessionedDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForSearchByAccessionedDate TO [Dev - JNCC SQL]
END

GO