/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Select]
GO

/*===========================================================================*\
  Description:	Returns a measurement or descriptor record from
		Occurrence_Data table.

  Parameters:	@Key		Occurrence Data key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.

  Created:	October 2003

  Last revision information:
    $Revision: 8 $
    $Date: 31/08/04 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Select]
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
		SELECT 	  	OD.Occurrence_Data_Key AS Item_Key,
				OD.Applies_To,
				OD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				OD.Duration,
				OD.Accuracy,
				OD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				OD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				OD.Lower_Value AS Value,
				OD.Lower_Value, 
				OD.Upper_Value,			
				OD.Custodian,
				OD.[Timestamp],
				S.Date_Time_Start
		FROM 		Occurrence_Data OD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = OD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = OD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = OD.Unit_Concept_Key
		LEFT JOIN 	Session S ON OD.Entered_Session_ID = S.Session_ID
		WHERE 		OD.Occurrence_Key = @Key
		AND		OD.Is_Descriptor = @IsDescriptor
	ELSE
		-- Measurements wants to match on detail key, to get a single record.
		SELECT 	  	OD.Occurrence_Data_Key AS Item_Key,
				OD.Applies_To,
				OD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				OD.Duration,
				OD.Accuracy,
				OD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				OD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				OD.Lower_Value AS Value,
				OD.Lower_Value, 
				OD.Upper_Value,			
				OD.Custodian,
				OD.[Timestamp],
				S.Date_Time_Start
		FROM 		Occurrence_Data OD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = OD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = OD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = OD.Unit_Concept_Key
		LEFT JOIN 	Session S ON OD.Entered_Session_ID = S.Session_ID
		WHERE 		OD.Occurrence_Data_Key = @Key
		AND		OD.Is_Descriptor = @IsDescriptor

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [Dev - JNCC SQL]
END