/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreferredDeterminationName_Get_ForOccurrence') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_PreferredDeterminationName_Get_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns the displayname of the preferred determination for 
		the given occurrence. 

  Parameters:	@OccurrenceKey
		@ItemName	OUTPUT

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 27/07/04 16:44 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreferredDeterminationName_Get_ForOccurrence]
	@OccurrenceKey char(16),
	@ItemName varchar(150) OUTPUT
AS

SET NOCOUNT ON

	SET @ItemName = NULL

	SELECT	@ItemName = CT.Item_Name
	FROM	Determination D
	JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	WHERE	D.Occurrence_Key = @OccurrenceKey
	AND	D.Preferred = 1

	-- If it's not direct through Determination, go through Specimen_Field_Data instead
	IF @ItemName IS NULL
		SELECT	@ItemName = CT.Item_Name
		FROM	Specimen_Field_Data SFD 
		JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		JOIN	Determination DSU ON DSU.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND DSU.Preferred = 1
		JOIN	vw_ConceptTermPreferred CT ON CT.Concept_Key = DSU.Concept_Key 
		WHERE	SFD.Occurrence_Key = @OccurrenceKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreferredDeterminationName_Get_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PreferredDeterminationName_Get_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [Dev - JNCC SQL]
END

GO

