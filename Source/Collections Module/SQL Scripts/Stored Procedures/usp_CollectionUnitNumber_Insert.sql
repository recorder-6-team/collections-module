/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Collection Unit Number table.
  Parameters:	@Key 
		@CollectionUnitKey 
		@Number 
		@TypeConceptKey 
		@Preferred
		@Notes 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 9 $
    $Date: 6/11/09 12:02 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred tinyint,
	@Notes text,
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Collection_Unit_Number', @Key OUTPUT

	BEGIN TRANSACTION
		-- Make sure every other Collection_Unit_Number record for this Collection unit record and 
		-- this this type are set to not preferred.
		IF @Preferred = 1 BEGIN
			UPDATE 	Collection_Unit_Number
			SET 	Preferred = 0
			WHERE 	Collection_Unit_Key = @CollectionUnitKey
		
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	
		INSERT INTO Collection_Unit_Number (
			Collection_Unit_Number_Key,
			Collection_Unit_Key,
			Number,
			Type_Concept_Key,
			Preferred,
			Notes,
			Entered_Session_ID
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@Number,
			@TypeConceptKey,
			@Preferred,
			@Notes,
			@SessionID )

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [Dev - JNCC SQL]
END
GO