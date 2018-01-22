/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Group table.

  Parameters:	@Key 
		@URL
		@SessionID
		@ConceptGroupName 
		@Authority 
		@HierarchyRelationTypeKey 
		@LocalDomainKey
		@RecordsAffected
		@Timestamp 

  Created:	November 2003

  Last revision information:
	$Revision: 11 $
	$Date: 31/08/11 10:55 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Update]
	@Key char(16),
	@LocalDomainKey char(16),
	@ConceptGroupName varchar(100),
	@Authority varchar(100) = NULL,
	@URL varchar(255) = NULL,
	@HierarchyRelationTypeKey char(16) = NULL,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@UpdateDescendents bit,
	@Timestamp timestamp,
	@RecordsAffected int output
AS
	SET NOCOUNT OFF

	DECLARE	@hierarchy_changed BIT,
		@error INT	

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	END

	BEGIN TRANSACTION

		/* has hierarchical relation changed? */
		SELECT	@hierarchy_changed = 	CASE WHEN @HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
							THEN 0
							ELSE 1
						END
		FROM	Concept_Group
		WHERE	Concept_Group_Key = @Key

		UPDATE 	Concept_Group
		SET	Local_Domain_Key = @LocalDomainKey,
			Item_Name = @ConceptGroupName,
			Authority = @Authority,
			URL = @URL,
			Hierarchy_Relation_Type_Key = @HierarchyRelationTypeKey,
			Term_Generator_Key = @TermGeneratorKey,
			Changed_Session_ID = @SessionID
		WHERE	Concept_Group_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/* regenerate concept lineage if hierarchy changed */
		IF @hierarchy_changed = 1
		BEGIN
			EXECUTE	usp_ConceptLineage_GenerateForGroup @concept_group_key = @Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END;
	
		ENABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ENABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [Dev - JNCC SQL]
END

GO	