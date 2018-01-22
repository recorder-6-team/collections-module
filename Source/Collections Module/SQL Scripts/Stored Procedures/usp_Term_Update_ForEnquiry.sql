/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Update_ForEnquiry]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Update_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Enquiry_Concept join table so 
		that there is a relationship between the Enquiry and 
		Concept tables.

  Parameters:	
		@ParentKey 	The key of the top level (parent) Enquiry node.
		@ChildKey	The key of the added (child) Concept node. 
		@SessionID
		@JoinKey	Key of new record in Enquiry_Concept

  Created:	Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Update_ForEnquiry] 
	@ParentKey char(16),
	@ChildKey char(16),
	@SessionID char(16),
	@JoinKey char(16) OUTPUT
AS

SET NOCOUNT ON

	BEGIN TRANSACTION

		EXECUTE spNextKey 'Enquiry_Concept', @JoinKey OUTPUT

		INSERT INTO Enquiry_Concept (
			Enquiry_Concept_Key,
			Enquiry_Key,
			Concept_Key,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Update_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Update_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Update_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Update_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Update_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Update_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Update_ForEnquiry TO [Dev - JNCC SQL]
END
GO