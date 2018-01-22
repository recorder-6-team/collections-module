If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns Collections for a specified Store.

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 16 $
    $Date: 23/09/04 18:20 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @Collection TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Collation_From_Vague_Date_Start] [int] NULL,
		[Collation_From_Vague_Date_End] [int] NULL
	)

	INSERT INTO 	@Collection (Collection_Unit_Key)
	SELECT DISTINCT C.Collection_Unit_Key
	FROM			Collection_Unit CU
	INNER JOIN 		Specimen_Unit SU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
						AND CU.Current_Container_Collection_Unit_Key = @ParentKey
	INNER JOIN 		Collection C ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key
	INNER JOIN		Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = C.Collection_Unit_Key
						AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
							OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))

	UPDATE	CList
	SET 	Item_Name = C.Item_Name,
		Number = M.Number,
		Collation_From_Vague_Date_Start = C.Collation_From_Vague_Date_Start,
		Collation_From_Vague_Date_End = C.Collation_From_Vague_Date_End

	FROM 		@Collection CList INNER JOIN Collection C ON CList.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND (MD.Outbound = 0)
			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON C.Collection_Unit_Key = MCU.Collection_Unit_Key

IF @SortOrderIndex = 0
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 2
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [Dev - JNCC SQL]
END
GO