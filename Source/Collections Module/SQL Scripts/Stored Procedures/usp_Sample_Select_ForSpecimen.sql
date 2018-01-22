/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Select_ForSpecimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Select_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Returns a list of samples linked to items in the collection.

  Parameters:	@Key	Collection key

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/02/04 14:53 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Select_ForSpecimen]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT S.Sample_Key, S.Lat, S.Long, S.Spatial_Ref, S.Spatial_Ref_System, S.Vague_Date_Start
	
	FROM		Specimen_Unit SU 
	INNER JOIN	Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
	LEFT JOIN	Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN	Taxon_Occurrence TOX ON TOX.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
	INNER JOIN	[Sample] S ON S.Sample_Key = O.Sample_Key OR S.Sample_Key = TOX.Sample_Key
	
	WHERE		SU.Collection_Unit_Key = @Key
	-- Only accept Samples that can actually be plotted on the map.
	AND		S.Lat IS NOT NULL
	AND		S.Long IS NOT NULL
	AND		S.Spatial_Ref IS NOT NULL
	AND		S.Spatial_Ref_System IS NOT NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForSpecimen TO [Dev - JNCC SQL]
END
GO