/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreIsBottomLevel_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreIsBottomLevel_Get]
GO

/*===========================================================================*\
  Description:	Takes a store key and returns whether it is a bottom level 
				store or not. To be a bottom level store, it has to not 
				contain any other stores currently (i.e. no other stores have
				this store as their current location).

  Parameters:	@Key
				@IsBottomLevel Output

  Created:		September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/09/04 15:06 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreIsBottomLevel_Get]
	@Key char(16),
	@IsBottomLevel bit OUTPUT 
AS
	IF EXISTS	(SELECT 	*
				FROM 		Store AS S
				INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
				WHERE		CU.Current_Container_Collection_Unit_Key = @Key)
		SET @IsBottomLevel = 0
	ELSE
		SET @IsBottomLevel = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreIsBottomLevel_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreIsBottomLevel_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [Dev - JNCC SQL]
END

GO