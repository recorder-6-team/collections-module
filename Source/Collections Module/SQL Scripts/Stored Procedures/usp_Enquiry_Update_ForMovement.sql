/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Update_ForMovement]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Update_ForMovement]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Enquiry join table 
		so that there is a relationship between the Enquiry and 
		Movement tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Movement node.
		@ChildKey	The key of the added (child) Enquiry node. 
		@SessionID	The SessionID.
		@JoinKey	Key for new record on join table.

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Update_ForMovement] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@SessionID char(16),
	@JoinKey char(16) OUTPUT
AS

SET NOCOUNT ON

	EXECUTE spNextKey 'Movement_Enquiry', @JoinKey OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Movement_Enquiry (
			Movement_Enquiry_Key,
			Movement_Key,
			Enquiry_Key,
			Entered_Session_ID
		) VALUES (
			@JoinKey,
			@ParentKey,
			@ChildKey,
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Update_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Update_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update_ForMovement TO [Dev - JNCC SQL]
END
GO