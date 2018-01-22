If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoragePlace_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns successive parent Stores for a specified Store

  Parameters:	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 12 $
    $Date: 7/02/05 11:11 $
    $Author: Ericsalmon $
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @StoragePlace TABLE
	(
		[Item_Index] [int] NOT NULL,
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Join_Key][char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Type] [varchar] (100) NULL,
		[Code] [varchar] (30)
	)
	
	
	DECLARE @Collection_Unit_Key CHAR(16), 
		@Parent_Collection_Unit_Key CHAR(16), 
		@Item_Name VARCHAR(100), 
		@Number varchar(30), 
		@Item_Index INT,
		@Store_Code VARCHAR(30),
		@Store_Type varchar(100)

	--Obtain parent of input store
	SELECT	@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	FROM 	Collection_Unit CU
	WHERE	((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	AND CU.Collection_Unit_Key = @ParentKey

	SET @Item_Index = 0

	--Obtain successive parents
	WHILE @Parent_Collection_Unit_Key IS NOT NULL
	BEGIN
		SELECT	@Collection_Unit_Key = S.Collection_Unit_Key, 
			@Item_Name = S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')),
			@Number = CUN.Number, 
			@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key,
			@Store_Code = CU.Current_Location_Code,
			@Store_Type = CT.Plaintext

		FROM 	Store S
		INNER JOIN	Collection_Unit CU
			ON S.Collection_Unit_Key = CU.Collection_Unit_Key
			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key

		LEFT JOIN 	Collection_Unit_Number CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		LEFT JOIN	vw_ConceptTermPreferred AS CT ON CT.Concept_key = S.Store_Type_Concept_Key
	
		--Ensure we are not joining back to a previous store
		IF (@Item_Name IS NOT NULL) AND (@Collection_Unit_Key <> @ParentKey)
		AND NOT EXISTS(SELECT * FROM @StoragePlace WHERE Collection_Unit_Key = @Collection_Unit_Key)
		BEGIN
			INSERT @StoragePlace (Item_Index, Collection_Unit_Key, Join_Key, Item_Name, Number, Code, Type) 
			VALUES (@Item_Index, @Collection_Unit_Key, @Collection_Unit_Key, @Item_Name, @Number, @Store_Code, @Store_Type)

			SET @Item_Index = @Item_Index + 1
		END ELSE
			BREAK
	END

	--Return hierarchical list
	SELECT 	Collection_Unit_Key AS Item_Key, Join_Key, Item_Name, Number, Code, Type
	FROM 	@StoragePlace
	ORDER BY Item_Index DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoragePlace_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoragePlace_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO