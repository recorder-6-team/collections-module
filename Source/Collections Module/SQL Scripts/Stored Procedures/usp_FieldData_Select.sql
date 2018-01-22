/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Field Data frame.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 8 $
    $Date: 30/09/05 14:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	SV.Survey_Key,
		SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS Survey_Name,
		SV.Run_By, 
		SFD.Inferred_Survey,
		S.Sample_Key,
		SE.Survey_Event_Key,
		S.Location_Key,
		LN.Item_Name as Location,
		S.Location_Name,
		CASE WHEN S.Location_Key IS NULL THEN S.Spatial_Ref_System ELSE L.Spatial_Ref_System END AS Spatial_Ref_System,
		SFD.Inferred_Location,
		S.Spatial_Ref_Qualifier,
		S.Spatial_Ref,
		S.Lat, 
		S.Long,
		SFD.Inferred_Spatial_Ref,
		S.Sample_Type_Key,
		ST.Short_Name AS Sample_Type,
		S.Vague_Date_Start,
		S.Vague_Date_End,
		S.Vague_Date_Type,
		SFD.Taxon_Occurrence_Key,
		SFD.Occurrence_Key,
		SFD.Inferred_Sample_Type,
		SFD.Inferred_Date,
		SFD.Inferred_Collectors,
		SFD.Gathering_Event,
	  	SFD.Custodian,
		SFD.[Timestamp]
	FROM Specimen_Field_Data SFD
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey SV ON SV.Survey_Key=SE.Survey_Key
	LEFT JOIN Location AS L ON L.Location_Key = S.Location_Key
	LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key
				AND LN.Preferred = 1
	INNER JOIN Sample_Type ST ON ST.Sample_Type_Key=S.Sample_Type_Key
	WHERE SFD.Specimen_Field_Data_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [Dev - JNCC SQL]
END

GO