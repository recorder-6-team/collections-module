/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
GO

/*===========================================================================*\
  Description:	Returns occurrence records for a specified sample key.

  Parameters:	@SampleKey

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 6/05/04 14:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
	@SampleKey char(16)
AS

SET NOCOUNT ON
	
	DECLARE @Occurrences TABLE (
		Item_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Item_Name varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Checked bit,
		Confidential bit
	)

	INSERT INTO	@Occurrences (Item_Key, Checked, Confidential) 
	SELECT DISTINCT	Occurrence_Key, Checked, Confidential
	FROM		Occurrence
	WHERE		Sample_Key = @SampleKey

	UPDATE		@Occurrences
	SET		Item_Name =
				CASE	WHEN DO.Concept_Key IS NOT NULL THEN CTPO.Item_Name
					WHEN DSU.Concept_Key IS NOT NULL THEN CTPSU.Item_Name
				END
	FROM		@Occurrences O
	LEFT JOIN	Determination DO ON DO.Occurrence_Key = O.Item_Key AND DO.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTPO ON CTPO.Concept_Key = DO.Concept_Key 

	LEFT JOIN	Specimen_Field_Data SFD ON SFD.Occurrence_Key = O.Item_Key
	LEFT JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	Determination DSU ON DSU.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND DSU.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTPSU ON CTPSU.Concept_Key = DSU.Concept_Key 

	SELECT * FROM @Occurrences

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [Dev - JNCC SQL]
END
GO