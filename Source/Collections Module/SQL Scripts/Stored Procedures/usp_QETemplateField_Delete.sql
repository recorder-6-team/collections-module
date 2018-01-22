If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a QETemplateField. Fails if the template has been altered
				since it was first loaded

  Parameters:	@QE_Template_Key  Key of template
				@Timestamp        Previously retrieved timestamp

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 10:21 $
    $Author: Pauldavies $

\*===========================================================================*/
    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Delete]
  @QETemplateFieldKey  Char(16),
  @Timestamp      timestamp
AS
	SET NOCOUNT ON
	SET XACT_ABORT ON

	BEGIN TRAN
		DELETE FROM QE_Data_Item
			WHERE QE_Template_Field_Key = @QETemplateFieldKey

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Template_Field
			WHERE QE_Template_Field_Key = @QETemplateFieldKey
			AND ([Timestamp] = @Timestamp)

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Template_Field WHERE QE_Template_Field_Key = @QETemplateFieldKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [Dev - JNCC SQL]
END

GO

 