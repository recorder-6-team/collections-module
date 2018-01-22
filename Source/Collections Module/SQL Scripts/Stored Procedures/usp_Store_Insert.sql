/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Insert]
GO

/*===========================================================================*\
  Description:	Insert a record into the Store table.
		Stores don't have an initial Domain_Mask, it gets updated
		as specimens are added.

  Parameters:	@Key
		@ItemName
		@CurrentContainerKey
		@UsualContainerKey
		@StoreTypeConceptKey
		@Comment
		@SessionID
		@CurrentLocationCode
		@UsualLocationCode

  Created:	July 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/12/03 11:41 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@CurrentContainerKey char(16),
	@UsualContainerKey char(16),
	@StoreTypeConceptKey char(16),
	@Comment text,
	@SessionID char(16),
	@CurrentLocationCode varchar(30),
	@UsualLocationCode varchar(30)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Collection_Unit.
		\*-------------------------------------------------------------*/
		INSERT INTO Collection_Unit (
			Collection_Unit_Key, Current_Container_Collection_Unit_Key, 
			Usual_Container_Collection_Unit_Key, Current_Location_Code,
			Usual_Location_Code, Domain_Mask, Entered_Session_ID
		) VALUES (
			@Key, @CurrentContainerKey, 
			@UsualContainerKey, @CurrentLocationCode,
			@UsualLocationCode, 0, @SessionID
		)
	
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Insert in Store.
		\*-------------------------------------------------------------*/
		INSERT INTO Store (
			Collection_Unit_Key, Item_Name, Store_Type_Concept_Key, Comment,
			Entered_Session_ID
		) VALUES (
			@Key, @ItemName, @StoreTypeConceptKey, @Comment, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Insert TO [Dev - JNCC SQL]
END
GO