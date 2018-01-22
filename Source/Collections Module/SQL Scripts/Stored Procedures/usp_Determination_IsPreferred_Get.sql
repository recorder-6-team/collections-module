/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_IsPreferred_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_IsPreferred_Get]
GO

/*===========================================================================*\
  Description:	Determines if the Determination is a preferred one.

  Parameters:	@DeterminationKey	Either a Determination_Key or Taxon_Determinaton_Key
		@IsLifeSciences		Specifies whether the record to be found is Life or Earth Sciences
		@SpecimenUnitKey	Specimen Collection_Unit_Key
		@IsPreferred		Output value

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/03/04 11:46 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_IsPreferred_Get] 
	@DeterminationKey char(16),
	@SpecimenUnitKey char(16),
	@IsLifeSciences bit,
	@IsSpecimenUnit bit,
	@IsPreferred bit OUTPUT
AS
	IF @IsSpecimenUnit = 1
		SELECT	@IsPreferred = CASE 
				WHEN (Preferred_Determination_Key = @DeterminationKey) AND (@IsLifeSciences = 0) THEN 1 
				WHEN (Preferred_Taxon_Determination_Key = @DeterminationKey) AND (@IsLifeSciences = 1) THEN 1
				ELSE 0 
			END
		FROM	Specimen_Unit
		WHERE 	Collection_Unit_Key = @SpecimenUnitKey
	ELSE
	IF @IsLifeSciences = 1
		SELECT	@IsPreferred = Preferred
		FROM	Taxon_Determination
		WHERE	Taxon_Determination_Key = @DeterminationKey
	ELSE
		SELECT	@IsPreferred = Preferred
		FROM	Determination
		WHERE	Determination_Key = @DeterminationKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_IsPreferred_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_IsPreferred_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_IsPreferred_Get TO [Dev - JNCC SQL]
END
GO