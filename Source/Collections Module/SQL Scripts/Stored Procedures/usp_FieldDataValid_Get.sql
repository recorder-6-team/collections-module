/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldDataValid_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldDataValid_Get]
GO

/*===========================================================================*\
  Description:	Used to insert a join record into the Specimen_Field_Data 
		table. This proc is used when the user is in the Field Data
		folder for a Specimen and they click Link To Existing.

  Parameters:	@SpecimenKey
	       	@OccTaxonOccKey		Either the Occurrence or then 
					Taxon_Occurrence key
               	@IsLifeScience		Whether it is a life sciences 
					Specimen or not.

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 19/03/04 12:08 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldDataValid_Get]
	@SpecimenKey char(16),
	@TaxonOccurrenceKey char(16),
	@OccurrenceKey char(16),
	@IsValid bit OUTPUT
AS
	SET NOCOUNT ON

	IF @TaxonOccurrenceKey IS NOT NULL
	BEGIN
		IF EXISTS 	(SELECT *
				FROM	Specimen_Field_Data
				WHERE	Collection_Unit_Key = @SpecimenKey
				AND	Taxon_Occurrence_Key = @TaxonOccurrenceKey)
			SET @IsValid = 0
		ELSE 	SET @IsValid = 1
	END
	ELSE
	BEGIN
		IF EXISTS 	(SELECT *
				FROM	Specimen_Field_Data
				WHERE	Collection_Unit_Key = @SpecimenKey
				AND	Occurrence_Key = @OccurrenceKey)
			SET @IsValid = 0
		ELSE 	SET @IsValid = 1
	END

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldDataValid_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldDataValid_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldDataValid_Get TO [Dev - JNCC SQL]
END

GO