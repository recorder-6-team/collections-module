/*===========================================================================*\
  Description:
	Updates for CCN 159

  Created:
	September 2013

  Last revision information:
    $Revision: 1 $
    $Date: 4/09/13 14:33 $
    $Author: Alexanderpadley $

\*===========================================================================*/

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 4/09/13 14:33 $
    $Author: Alexanderpadley $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Update]
  @QEDataItemKey as int,
  @DataValue as varchar(200),
  @DataDisplay as varchar(200),
  @Timestamp as timestamp,
  @SessionID as char(16)

 AS

	SET NOCOUNT OFF

	Update QE_Data_Item
		set Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	where 
		([Timestamp] = @Timestamp OR @Timestamp IS NULL)
		and (QE_Data_Item_Key = @QEDataItemKey)

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Item WHERE QE_Data_Item_Key = @QEDataItemKey)
		RAISERROR('Record updated by another user', 16, 1)

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [Dev - JNCC SQL]
END

GO