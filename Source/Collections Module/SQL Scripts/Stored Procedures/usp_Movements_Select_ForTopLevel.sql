If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@Key CHAR(16) = NULL,
@MovementGroupType TINYINT,
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Movements data to the top level of the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-15
--
SET NOCOUNT ON

DECLARE @MovementTypeLow INT, @MovementTypeHigh INT

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @MovementGroupType=0 BEGIN -- AccessionsAndExchanges
	SET @MovementTypeLow=0
	SET @MovementTypeHigh=1
END
ELSE IF @MovementGroupType=1 BEGIN -- Loans
	SET @MovementTypeLow=2
	SET @MovementTypeHigh=3
END	
ELSE IF @MovementGroupType=2 BEGIN -- Other movements
	SET @MovementTypeLow=4
	SET @MovementTypeHigh=9
END


IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
BEGIN
	-- This is for selecting Exchanges to show. It is only done for @MovementGroupType 0.
	IF @MovementGroupType = 0
	BEGIN
		-- Select all items
		INSERT INTO @Search 
			SELECT DISTINCT ItemKey 
			FROM #TempFilter 
			INNER JOIN 	Movement M ON M.Movement_Key=#TempFilter.ItemKey
			INNER JOIN 	MOVEMENT_DIRECTION MD1 ON M.Movement_Key = MD1.Movement_Key AND MD1.Outbound=1
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU1 ON MD1.Movement_Direction_Key = MCU1.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU1 ON MCU1.Collection_Unit_Key = CU1.Collection_Unit_Key
			INNER JOIN 	MOVEMENT_DIRECTION MD2 ON M.Movement_Key = MD2.Movement_Key AND MD2.Outbound=0
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU2 ON MD2.Movement_Direction_Key = MCU2.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU2 ON MCU2.Collection_Unit_Key = CU2.Collection_Unit_Key
			WHERE 		((CU1.Domain_Mask IS NULL AND CU2.Domain_Mask IS NULL) OR 
					(CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Domain_Mask = 0) OR
					(CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Domain_Mask = 0))
			AND		M.Movement_type = 1
	END 

	-- Use the temporary filter table to provide list of keys. All movements except Exchanges.
	INSERT INTO @Search 
		SELECT DISTINCT ItemKey 
		FROM #TempFilter 
		INNER JOIN Movement M ON M.Movement_Key=#TempFilter.ItemKey AND M.Movement_Type <> 1
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
			-- If Loan Out we are only interested in the outbound movement because 
			-- it is by following this Movement_Direction that we will get to the Domain_Mask
			-- that we need to compare with the @UserDomainMask.
			AND ((Movement_Type = 3 AND MD.Outbound = 1) 
				-- If it is a Loan In, only interested in inbound movement
				OR (Movement_Type = 2 AND MD.Outbound = 0) 
				-- Otherwise.
				OR NOT Movement_Type IN (1,2,3) )
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh)
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
END	
ELSE BEGIN
	-- This is for selecting Exchanges to show. It is only done for @MovementGroupType 0.
	IF @MovementGroupType = 0
	BEGIN
		-- Select all items
		INSERT INTO @Search 
			SELECT DISTINCT M.Movement_Key
			FROM 		Movement M
			INNER JOIN 	MOVEMENT_DIRECTION MD1 ON M.Movement_Key = MD1.Movement_Key AND MD1.Outbound=1
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU1 ON MD1.Movement_Direction_Key = MCU1.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU1 ON MCU1.Collection_Unit_Key = CU1.Collection_Unit_Key
			INNER JOIN 	MOVEMENT_DIRECTION MD2 ON M.Movement_Key = MD2.Movement_Key AND MD2.Outbound=0
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU2 ON MD2.Movement_Direction_Key = MCU2.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU2 ON MCU2.Collection_Unit_Key = CU2.Collection_Unit_Key
			WHERE 		((CU1.Domain_Mask IS NULL AND CU2.Domain_Mask IS NULL) OR 
					(CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Domain_Mask = 0) OR
					(CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Domain_Mask = 0))
			AND		M.Movement_type = 1
	END

	-- Select all items except Exchanges.
	INSERT INTO @Search 
		SELECT DISTINCT M.Movement_Key 
		FROM Movement M
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
			-- If Loan Out we are only interested in the outbound movement because 
			-- it is by following this Movement_Direction that we will get to the Domain_Mask
			-- that we need to compare with the @UserDomainMask.
			AND ((Movement_Type = 3 AND MD.Outbound = 1) 
				-- If it is a Loan In, only interested in inbound movement
				OR (Movement_Type = 2 AND MD.Outbound = 0) 
				-- Otherwise.
				OR NOT Movement_Type IN (1,2,3) )
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh) 
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
END

IF @SortOrderIndex = 0
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Number
END
ELSE IF @SortOrderIndex = 2
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY M.Movement_Type, M.Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO