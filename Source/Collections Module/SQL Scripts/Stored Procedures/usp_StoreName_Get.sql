/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreName_Get]
GO

/*===========================================================================*\
  Description:	Returns the store name for the given store key.

  Parameters:	@StoreKey
		@StoreName	Output

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 23/09/04 17:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreName_Get]
	@StoreKey char(16),
	@StoreName varchar(100) OUTPUT 
AS
	SELECT		@StoreName = S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, ''))
	FROM		Store AS S
	INNER JOIN 	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @StoreKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreName_Get TO [Dev - JNCC SQL]
END

GO