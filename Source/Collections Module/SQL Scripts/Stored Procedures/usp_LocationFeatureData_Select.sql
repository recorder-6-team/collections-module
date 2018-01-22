/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Select]
GO

/*===========================================================================*\
  Description:	Returns a measurement or descriptor record from
		Location_Feature_Data table.

  Parameters:	@Key		Occurrence Data key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 17:42 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Select]
	@Key char(16),
	@IsDescriptor bit
AS

SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	IF @IsDescriptor = 1
		-- Descriptor tab page wants to match on the master key, to get all records.
		SELECT 	  	LFD.Location_Feature_Data_Key AS Item_Key,
				LFD.Applies_To,
				LFD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				LFD.Duration,
				LFD.Accuracy,
				LFD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				LFD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				LFD.Lower_Value AS Value,
				LFD.Lower_Value, 
				LFD.Upper_Value,			
				LFD.Custodian,
				LFD.[Timestamp],
				S.Date_Time_Start
		FROM 		Location_Feature_Data LFD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = LFD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = LFD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = LFD.Unit_Concept_Key
		LEFT JOIN 	Session S ON LFD.Entered_Session_ID = S.Session_ID
		WHERE 		LFD.Location_Feature_Key = @Key
		AND		LFD.Is_Descriptor = @IsDescriptor
	ELSE
		-- Measurements wants to match on detail key, to get a single record.
		SELECT 	  	LFD.Location_Feature_Data_Key AS Item_Key,
				LFD.Applies_To,
				LFD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				LFD.Duration,
				LFD.Accuracy,
				LFD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				LFD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				LFD.Lower_Value AS Value,
				LFD.Lower_Value, 
				LFD.Upper_Value,			
				LFD.Custodian,
				LFD.[Timestamp],
				S.Date_Time_Start
		FROM 		Location_Feature_Data LFD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = LFD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = LFD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = LFD.Unit_Concept_Key
		LEFT JOIN 	Session S ON LFD.Entered_Session_ID = S.Session_ID
		WHERE 		LFD.Location_Feature_Data_Key = @Key
		AND		LFD.Is_Descriptor = @IsDescriptor

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFeatureData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [Dev - JNCC SQL]
END
GO