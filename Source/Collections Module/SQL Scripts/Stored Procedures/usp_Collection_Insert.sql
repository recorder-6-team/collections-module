/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Collection_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Collection_Insert]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection table.
		Collections don't have an initial Domain_Mask, it gets updated
		as specimens are added.

  Parameters:	@Key
		@ParentCollectionKey
		@ItemName
		@AssemblerNameKey
		@Topic
		@RiskConceptKey
		@CurrentContainerKey
		@CurrentLocationCode
		@UsualContainerKey
		@UsualLocationCode
		@SessionID

  Created:	July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Insert]
	-- Collection 
	@Key char(16) OUTPUT,
	@ParentCollectionKey char(16),
	@ItemName varchar(150),
	@AssemblerNameKey char(16),
	@Topic varchar(200),
	@RiskConceptKey char(16),
	-- Collection_Unit
	@CurrentContainerKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerKey char(16),
	@UsualLocationCode varchar(30),
	-- Both
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Collection_Unit', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Collection_Unit first.
		\*-------------------------------------------------------------*/
		INSERT INTO Collection_Unit (
			Collection_Unit_Key, Current_Container_Collection_Unit_Key, Current_Location_Code,
			Usual_Container_Collection_Unit_Key, Usual_Location_Code, Domain_Mask,
			Entered_Session_ID
		) VALUES (
			@Key, @CurrentContainerKey, @CurrentLocationCode, 
			@UsualContainerKey, @UsualLocationCode, 0, -- Domain_Mask is empty for new Collections
			@SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  Then in Collection.
		\*-------------------------------------------------------------*/
		INSERT INTO Collection (
			Collection_Unit_Key, Parent_Collection_Collection_Unit_Key, Item_Name, 
			Assembler_Name_Key, Topic, Risk_Concept_Key, Entered_Session_ID
		) VALUES (
			@Key, @ParentCollectionKey, @ItemName, @AssemblerNameKey, 
			@Topic, @RiskConceptKey, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Insert TO [Dev - JNCC SQL]
END

GO