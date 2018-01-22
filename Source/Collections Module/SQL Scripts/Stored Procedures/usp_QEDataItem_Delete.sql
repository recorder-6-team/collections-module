/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Delete]
GO

/*===========================================================================*\
  Description:	Clears an item from a Quick Entry session

  Parameters:	@Key - QE_Data_Item key
							@Timestamp

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 10:08 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Delete]
	@Key INT,
	@Timestamp TIMESTAMP
AS
  
		DELETE	QE_Data_Item
		WHERE	QE_Data_Item_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Item WHERE QE_Data_Item_Key = @Key)
			RAISERROR('Record updated by another user', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [Dev - JNCC SQL]
END

GO
