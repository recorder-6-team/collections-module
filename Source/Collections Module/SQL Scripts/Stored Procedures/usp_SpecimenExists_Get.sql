If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SpecimenExists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SpecimenExists_Get]
GO

/*===========================================================================*\
  Description: 	Checks to see if a Specimen_Unit record exists for a given
		key. This is useful when adding a Store that is also a 
		Specimen. When the Store frame is saved, a Collection_Unit_Key
		is generated. This key is passed to the Specimen frame. However,
		because the Specimen frame has a key, it will try and run the
		update stored proc on a Specimen_Unit record that doesn't 
		exist yet. We need to run the Insert proc, and this proc is
		used to determine that it is the Insert proc, rather than
		the Update proc that needs to be run.		

  Parameters:	@Key 		Collection_Unit_Key
		@Exists 	(output)

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 6/04/04 15:27 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_SpecimenExists_Get] 
	@Key char(16),
	@Exists bit OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@Exists = CASE WHEN Count(*) > 0 THEN 1
						ELSE 0
				END
	FROM	Specimen_Unit
	WHERE	Collection_Unit_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenExists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenExists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenExists_Get TO [Dev - JNCC SQL]
END

GO
