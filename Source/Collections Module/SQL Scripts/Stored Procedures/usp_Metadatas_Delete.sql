/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Metadatas_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Metadatas_Delete]
GO

/*===========================================================================*\
  Description:	Delete multiple metadata records from the Metadata table.

  Parameters:	@Key	
		@TableName

  Created:	December 2004

  Last revision information:
    $Revision: 2 $
    $Date: 14/01/04 17:29 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Metadatas_Delete]
	@Key char(16),
	@TableName varchar(50)
AS
	BEGIN TRANSACTION

		DELETE		Metadata
		FROM 		Metadata AS M
		INNER JOIN	Metadata_Type AS MT ON MT.Metadata_Type_Key = M.Metadata_Type_Key
		WHERE		M.Record_Key = @Key
		AND		MT.Table_Name = @TableName
	
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Metadatas_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Metadatas_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Metadatas_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Metadatas_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Metadatas_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Metadatas_Delete TO [Dev - JNCC SQL]
END
GO