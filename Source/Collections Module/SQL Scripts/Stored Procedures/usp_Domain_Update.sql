/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Domain table

  Parameters:	@Key 
		@ItemName
		@SubjectAreaKey
		@HasOccurrences 
		@DefaultHierarchyRelationTypeKey 
		@DomainMask
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 6 $
    $Date: 31/08/11 10:55 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@SubjectAreaKey char(16),
	@HasOccurrences bit,
	@DefaultHierarchyRelationTypeKey char(16),
	@DomainMask int,
	@TermGeneratorKey char(16),
	@UpdateDescendents bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_Domain_PublishedTermFields ON Domain
	END

	BEGIN TRANSACTION
		
		
		UPDATE 	Domain
		SET 	Item_Name = @ItemName, 
			Subject_Area_Key = @SubjectAreaKey,
			Has_Occurrences = @HasOccurrences,
			Default_Hierarchy_Relation_Type_Key = @DefaultHierarchyRelationTypeKey,
			Domain_Mask = @DomainMask,
			Changed_Session_ID = @SessionID,
			Term_Generator_Key = @TermGeneratorKey
		WHERE	Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain WHERE Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END;

		ENABLE TRIGGER tr_Domain_PublishedTermFields ON Domain		
	COMMIT TRANSACTION
	
	RETURN 0

RollBackAndExit:
		ENABLE TRIGGER tr_Domain_PublishedTermFields ON Domain 
	ROLLBACK TRANSACTION	
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [Dev - JNCC SQL]
END
GO