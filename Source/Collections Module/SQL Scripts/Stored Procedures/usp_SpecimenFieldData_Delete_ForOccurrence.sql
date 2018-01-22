/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Delete_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Delete_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Deletes all records from Specimen_Field_Data related to given
		OccurrenceKey.

  Parameters:	@OccurrenceKey

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 26/02/04 10:20 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Delete_ForOccurrence]
	@OccurrenceKey char(16),
	@IsTaxonOccurrence bit
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Specimen_Field_Data
		WHERE	(@IsTaxonOccurrence = 0 AND Occurrence_Key = @OccurrenceKey)
		OR	(@IsTaxonOccurrence = 1 AND Taxon_Occurrence_Key = @OccurrenceKey)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Delete_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Delete_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete_ForOccurrence TO [Dev - JNCC SQL]
END
GO