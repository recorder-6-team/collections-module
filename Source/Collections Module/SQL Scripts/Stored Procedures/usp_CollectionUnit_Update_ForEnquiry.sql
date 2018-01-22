/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForEnquiry]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Enquiry join table 
		so that there is a relationship between the Enquiry and 
		Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Enquiry node.
		@ChildKey	The key of the added (child) Collection Unit node. 
		@SessionID
		@JoinKey	Key of new record in Collection_Unit_Enquiry

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForEnquiry] 
	@ParentKey char(16),
	@ChildKey char(16),
	@SessionID char(16),
	@JoinKey char(16) OUTPUT
AS

	SET NOCOUNT ON

	EXECUTE spNextKey 'Collection_Unit_Enquiry', @JoinKey OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Enquiry (
			Collection_Unit_Enquiry_Key, Enquiry_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@JoinKey, @ParentKey, @ChildKey, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForEnquiry TO [Dev - JNCC SQL]
END
GO