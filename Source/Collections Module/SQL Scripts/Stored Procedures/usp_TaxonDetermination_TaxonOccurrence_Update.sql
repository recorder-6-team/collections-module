/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_TaxonOccurrence_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_TaxonOccurrence_Update]
GO

/*===========================================================================*\


  Parameters:	@TaxonOccurrenceKey	-Taxon Occurrence Key
				@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 16/02/04 10:05 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_TaxonOccurrence_Update]
	@TaxonDeterminationKey char(16),
	@SpecimenUnitKey char(16)
AS

DECLARE @TaxonOccurrenceKey CHAR(16)

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION	
		--Update Taxon_Occurrence field on Taxon_Determination
		SELECT @TaxonOccurrenceKey = XO.Taxon_Occurrence_Key
		FROM Specimen_Unit SU
			INNER JOIN Specimen_Field_Data SFD ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
			INNER JOIN Taxon_Occurrence XO ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
		WHERE SU.Collection_Unit_Key = @SpecimenUnitKey
			AND SFD.Gathering_Event = 1	
		IF @@Error <> 0 GOTO RollbackAndExit


		UPDATE Taxon_Determination
			SET Taxon_Occurrence_Key = @TaxonOccurrenceKey
		WHERE Taxon_Determination_Key = @TaxonDeterminationKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_TaxonOccurrence_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_TaxonOccurrence_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_TaxonOccurrence_Update TO [Dev - JNCC SQL]
END
GO