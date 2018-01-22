/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Delete_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Delete_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Deletes all records from Occurrence_Data related to the given 
		Occurrence Key. Deletes only either Descriptors or Measurements.

  Parameters:	@OccurrenceKey
		@IsDescriptor	Flag to indicate Measurements or Descriptors

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 8/12/03 11:22 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Delete_ForOccurrence]
	@OccurrenceKey char(16),
	@IsDescriptor bit
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Occurrence_Data
		WHERE	Occurrence_Key = @OccurrenceKey
		AND	Is_Descriptor = @IsDescriptor

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Delete_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceData_Delete_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete_ForOccurrence TO [Dev - JNCC SQL]
END
GO