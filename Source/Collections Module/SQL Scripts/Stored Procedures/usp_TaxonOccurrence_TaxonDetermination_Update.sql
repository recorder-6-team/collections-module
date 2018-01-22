/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	

  Parameters:	@TaxonOccurrenceKey	-Taxon Occurrence Key
		@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 5 $
    $Date: 19/12/07 11:34 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
	@TaxonOccurrenceKey char(16),
	@SpecimenUnitKey char(16)
AS

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION	
		--Update existing Specimen Determinations to link to the new Occurrence
		UPDATE Taxon_Determination
			SET Taxon_Occurrence_Key = @TaxonOccurrenceKey,
				Preferred = 1
		FROM		Taxon_Determination AS TD
		INNER JOIN 	Specimen_Unit AS SU ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		WHERE Specimen_Collection_Unit_Key = @SpecimenUnitKey

		IF @@Error <> 0 GOTO RollBackAndExit

		-- Update Validation flag in Taxon Occurrence
		DECLARE	@ValidationLevel int
		SELECT	@ValidationLevel = Validation_Level
		FROM	Taxon_Determination TD 
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
		WHERE	TD.Specimen_Collection_Unit_Key = @SpecimenUnitKey AND TD.Preferred = 1

		DECLARE	@CompetencyLevel int
		SELECT	@CompetencyLevel = DR.Validation_Competency
		FROM	Taxon_Determination TD 
		JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
		WHERE	TD.Specimen_Collection_Unit_Key = @SpecimenUnitKey AND TD.Preferred = 1

		UPDATE	Taxon_Occurrence
		SET	Verified = 
				CASE 
					WHEN @ValidationLevel IS NULL THEN 0
					WHEN @ValidationLevel <= @CompetencyLevel THEN 2
					ELSE 1
				END
		WHERE	Taxon_Occurrence_Key = @TaxonOccurrenceKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [Dev - JNCC SQL]
END

GO