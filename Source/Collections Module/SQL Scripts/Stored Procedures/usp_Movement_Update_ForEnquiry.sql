/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Update_ForEnquiry]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Update_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Enquiry join table so that 
		there is a relationship between the Movement and Enquiry tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Enquiry node.
		@ChildKey	The key of the Movement node. 
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Update_ForEnquiry] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@SessionID CHAR(16),
	@IsAccessionOrExchange bit = NULL,
	@JoinKey char(16) OUTPUT
AS

SET NOCOUNT ON

	EXECUTE spNextKey 'Movement_Enquiry', @JoinKey OUTPUT


	BEGIN TRANSACTION
		-- The Accessions and Exchanges folder node is only allowed to contain one node.
		-- As a result, any other accession or exchange movements associated with the 
		-- enquiry should be deleted before the new record is inserted.
		IF @IsAccessionOrExchange = 1
			DELETE		Movement_Enquiry
			FROM		Movement_Enquiry AS ME
			INNER JOIN	Movement AS M ON M.Movement_Key = ME.Movement_Key
							AND M.Movement_Type IN  (0,1)
			WHERE		ME.Enquiry_Key = @ParentKey

		INSERT INTO Movement_Enquiry (
			Movement_Enquiry_Key,
			Movement_Key,
			Enquiry_Key,
			Entered_Session_ID
		) VALUES (
			@JoinKey,
			@ChildKey,
			@ParentKey,
			@SessionID
		)
	
		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Update_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Update_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Update_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Update_ForEnquiry TO [Dev - JNCC SQL]
END
GO