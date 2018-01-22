/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert_ForSpecimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Used to insert a join record into the Specimen_Field_Data 
		table. This proc is used when the user is in the Field Data
		folder for a Specimen and they click Link To Existing.

  Parameters:	@Key 	Specimen_Field_Data key
		@SpecimenKey
		@TaxonOccurrenceKey
		@OccurrenceKey
		@SessionID

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 19/03/04 12:08 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert_ForSpecimen]
	@Key char(16) OUTPUT,
	@SpecimenKey char(16),
	@TaxonOccurrenceKey char(16),
	@OccurrenceKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		EXECUTE spNextKey 'Specimen_Field_Data', @Key OUTPUT

		/*-------------------------------------------------------------*\
		  Insert in Specimen_Field_Data table.
		\*-------------------------------------------------------------*/
		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, Collection_Unit_Key, Occurrence_Key, 
			Taxon_Occurrence_Key, Inferred_Survey, Inferred_Location, 
			Inferred_Spatial_Ref, Inferred_Sample_Type, Inferred_Date, 
			Inferred_Collectors, Gathering_Event, Entered_Session_ID
		) VALUES (
			@Key, @SpecimenKey, @OccurrenceKey, 
			@TaxonOccurrenceKey, 0, 0, 
			0, 0, 0, 
			0, 0, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert_ForSpecimen TO [Dev - JNCC SQL]
END
GO