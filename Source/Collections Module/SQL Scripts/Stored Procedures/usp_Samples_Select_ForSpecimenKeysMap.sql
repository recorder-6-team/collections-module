/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Samples_Select_ForSpecimenKeysMap]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Samples_Select_ForSpecimenKeysMap]
GO

/*===========================================================================*\
  Description:	Returns a list of samples that relate to a list of
		specimen unit keys supplied by the #TempFilter table, only including
		the mappable ones

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 30/03/04 16:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Samples_Select_ForSpecimenKeysMap]
AS

SET NOCOUNT ON

IF object_id('tempdb..#TempFilter') IS NULL
  RAISERROR('usp_Samples_Select_ForSpecimenKeysMap expects the #TempFilter table to be populated', 16, 1) 

SELECT DISTINCT S.Sample_Key, S.Spatial_Ref, S.Spatial_Ref_System, S.Lat, S.Long, S.Vague_Date_Start
FROM Specimen_Field_Data SFD
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key
		OR S.Sample_Key=XO.Sample_Key
INNER JOIN #TempFilter TF ON TF.ItemKey= SFD.Collection_Unit_Key
WHERE S.Lat IS NOT NULL AND S.Long IS NOT NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Samples_Select_ForSpecimenKeysMap') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Samples_Select_ForSpecimenKeysMap'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Samples_Select_ForSpecimenKeysMap TO [Dev - JNCC SQL]
END

GO