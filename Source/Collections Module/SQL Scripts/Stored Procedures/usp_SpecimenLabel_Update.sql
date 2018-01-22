/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Specimen Label table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 18/11/09 13:16 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100),
	@Inscription ntext,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@Comments text,
	@InferredAuthor tinyint,
	@AuthorNameKey char(16),
	@ConfidenceConceptKey char(16),
	@SessionID char(16),
	@Timestamp timestamp,
	@IsCurrent bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE		Specimen_Label
		SET			Collection_Unit_Key		=	@CollectionUnitKey,
					Is_Inscription			=	@IsInscription,
					Label_Position			=	@Position,
					Inscription				=	@Inscription,
					Translated				=	@Translated,
					Translated_Language_Key	=	@LanguageConceptKey,
					Comments				=	@Comments,
					Inferred_Author			=	@InferredAuthor,
					Author_Name_Key			=	@AuthorNameKey,
					Confidence_Concept_Key	=	@ConfidenceConceptKey,
					Changed_Session_ID		=	@SessionID,
					Is_Current				=	@IsCurrent
		WHERE		Specimen_Label_Key		=	@Key
		AND			[Timestamp]				=	@Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Label WHERE Specimen_Label_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [Dev - JNCC SQL]
END
GO