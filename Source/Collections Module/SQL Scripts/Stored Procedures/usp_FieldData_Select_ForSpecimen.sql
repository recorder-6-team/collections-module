If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_FieldData_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_FieldData_Select_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Returns field data from Recorder for a specified Specimen

  Parameters:	
	@ParentKey Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 18/12/06 14:46 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select_ForSpecimen] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		SFD.Specimen_Field_Data_Key AS Item_Key, SFD.Specimen_Field_Data_Key AS Join_Key,
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Sample_Key 
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Sample_Key 
			END AS Hyperlink_Item_Key,
			CASE	WHEN S1.Sample_Key IS NOT NULL THEN S1.Sample_Key 
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Sample_Key
			END AS Drag_Drop_Item_Key, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_Start
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_Start
			END AS Vague_Date_Start, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_End
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_End
			END AS Vague_Date_End, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_Type
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_Type
			END AS Vague_Date_Type,
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN 
					CASE 	WHEN L1.Location_Name_Key IS NOT NULL THEN L1.Item_Name
						ELSE S1.Location_Name
					END
				WHEN S2.Sample_Key IS NOT NULL THEN 
					CASE 	WHEN L2.Location_Name_Key IS NOT NULL THEN L2.Item_Name
						ELSE S2.Location_Name
					END
			END AS LocationName

	FROM 		Specimen_Unit SU
	INNER JOIN 	Specimen_Field_Data SFD ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT  JOIN 	(Occurrence O 
			INNER JOIN [Sample] S1 ON O.Sample_Key = S1.Sample_Key
			LEFT  JOIN [Location_Name] L1 ON L1.Location_Key = S1.Location_Key AND L1.Preferred = 1
			) 
		ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT  JOIN	(Taxon_Occurrence XO 
			INNER JOIN [Sample] S2 ON XO.Sample_Key = S2.Sample_Key
			LEFT  JOIN [Location_Name] L2 ON L2.Location_Key = S2.Location_Key AND L2.Preferred = 1
			) 
		ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key

	WHERE	SU.Collection_Unit_Key = @ParentKey
	AND	(S1.Sample_Key IS NOT NULL OR S2.Sample_Key IS NOT NULL)

	ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, Vague_Date_Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [Dev - JNCC SQL]
END
GO