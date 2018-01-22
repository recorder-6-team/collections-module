/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectArea_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectArea_Insert]
GO

/*===========================================================================*\
  Description:	Updates a record in the Subject_Area table.

  Parameters:	@Key 
		@ItemName 
		@Comment 
		@SessionID 
		@SystemSuppliedData (optional)

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/12/03 11:43 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectArea_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@Comment text,
	@SessionID char(16),
	@SystemSuppliedData bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Subject_Area', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Subject_Area (
			Subject_Area_Key,
			Item_Name,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@Comment,
			@SessionID,
			@SystemSuppliedData
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectArea_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectArea_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Insert TO [Dev - JNCC SQL]
END
GO