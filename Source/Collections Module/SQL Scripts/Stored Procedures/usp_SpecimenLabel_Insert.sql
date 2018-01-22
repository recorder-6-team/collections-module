/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 8 $
    $Date: 18/11/09 13:14 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100) = NULL,		-- Because called through QuickEntry with no value.
	@Inscription nText,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@InferredAuthor tinyint,
	@Comments text,
	@AuthorNameKey char(16) = NULL,
	@ConfidenceConceptKey char(16),
	@SessionID char(16),
	@IsCurrent bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Specimen_Label', @Key OUTPUT

	BEGIN TRANSACTION	
		INSERT INTO		Specimen_Label
						(
							Specimen_Label_Key,
							Collection_Unit_Key,
							Is_Inscription,
							Label_Position,
							Inscription,
							Translated,
							Translated_Language_Key,
							Inferred_Author,
							Comments,
							Author_Name_Key,
							Confidence_Concept_Key,
							Entered_Session_ID,
							Is_Current
						)
		VALUES			(
							@Key,
							@CollectionUnitKey,
							@IsInscription,
							@Position,
							@Inscription, 
							@Translated,
							@LanguageConceptKey,
							IsNull(@InferredAuthor, 0),
							@Comments,
							@AuthorNameKey,
							@ConfidenceConceptKey,
							@SessionID,
							@IsCurrent
						)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [Dev - JNCC SQL]
END
GO