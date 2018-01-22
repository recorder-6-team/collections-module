/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Delete_ForSpecimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Delete_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Delete a record from the Store table.
		This stored procedure is different to the normal usp_Store_Delete
		because we don't want to delete the Specimen Unit or 
		Collection Unit records as well.		

  Parameters:	@Key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 11:41 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Delete_ForSpecimen]
	@Key char(16)
AS
	BEGIN TRANSACTION
		-- Delete record from Store table.
		DELETE	Store
		WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Delete_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Delete_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Delete_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Delete_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Delete_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Delete_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Delete_ForSpecimen TO [Dev - JNCC SQL]
END
GO