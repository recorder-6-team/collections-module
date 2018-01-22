/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a Local_Domain record
  Parameters:	@Key 
		@ItemName 
		@DomainKey 
		@LanguageKey 
		@ConceptGroupLabel 
		@SessionID 
		@SystemSuppliedData (Optional)

  Created:	Oct 2003

  Last revision information:
    $Revision: 6 $
    $Date: 28/07/11 15:45 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@DomainKey char(16),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Local_Domain', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Local_Domain (
			Local_Domain_Key,
			Item_Name,
			Domain_Key,
			Language_Key,
			Concept_Group_Label,
			Term_Generator_Key,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@DomainKey,
			@LanguageKey,
			CASE WHEN @ConceptGroupLabel IS NULL THEN 'Concept Group' ELSE @ConceptGroupLabel END,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [Dev - JNCC SQL]
END
GO