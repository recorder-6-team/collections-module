/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a Domain record

  Parameters:	@Key 
		@ItemName 
		@SubjectAreaKey 
		@HasOccurrences 
		@DefaultHierarchyRelationTypeKey 
		@DomainMask
		@EnteredSessionID 
		@SystemSuppliedData 

  Created:	Oct 2003

  Last revision information:
    $Revision: 7 $
    $Date: 28/07/11 15:45 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@SubjectAreaKey char(16),
	@HasOccurrences bit,
	@DefaultHierarchyRelationTypeKey char(16),
	@DomainMask int,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Domain', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Domain (
			Domain_Key,
			Item_Name,
			Subject_Area_Key,
			Has_Occurrences,
			Default_Hierarchy_Relation_Type_Key,
			Domain_Mask,
			Term_Generator_Key,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@SubjectAreaKey,
			@HasOccurrences,
			@DefaultHierarchyRelationTypeKey,
			@DomainMask,
			@TermGeneratorKey,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Insert TO [Dev - JNCC SQL]
END
GO