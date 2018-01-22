/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Occurrence_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Occurrence_Update]
GO

/*===========================================================================*\
  Description:	Sets the Occurrence field on a given determination by using the specimen field data

  Parameters:	@OccurrenceKey		-Occurrence Key
				@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/04 16:35 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Occurrence_Update]
	@DeterminationKey char(16),
	@SpecimenUnitKey char(16)
AS

DECLARE @OccurrenceKey CHAR(16)

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION	
		SELECT @OccurrenceKey = O.Occurrence_Key
		FROM Specimen_Unit SU
			INNER JOIN Specimen_Field_Data SFD ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
			INNER JOIN Occurrence O ON SFD.Occurrence_Key = O.Occurrence_Key
		WHERE SU.Collection_Unit_Key = @SpecimenUnitKey
			AND SFD.Gathering_Event = 1
		IF @@Error <> 0 GOTO RollbackAndExit


		--Update Occurrence field on Determination
		UPDATE Determination
			SET Occurrence_Key = @OccurrenceKey
		WHERE Determination_Key = @DeterminationKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Occurrence_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Occurrence_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Occurrence_Update TO [Dev - JNCC SQL]
END
GO