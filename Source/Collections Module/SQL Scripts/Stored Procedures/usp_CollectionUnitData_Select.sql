/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of either measurements or descriptors from
		Collection_Unit_Data table record.

  Parameters:	@Key		Collection key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.
		@DomainConceptGroupName
				Name of the domain concept group where new parameters
				added by users should go. If the concept group exists
				for the domain, its key will be returned, ready for use.

  Created:	September 2003

  Last revision information:
    $Revision: 13 $
    $Date: 29/03/04 13:25 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Select]
	@Key char(16),
	@IsDescriptor bit
AS

SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	-- Descriptor tab page wants to match on Collection_Unit_Key because
	-- there will be multiple rows.
	IF @IsDescriptor = 1 
	BEGIN
		SELECT 		CUD.Collection_Unit_Data_Key AS Item_Key,
				CUD.Applies_To,
				CUD.Method_Concept_Key,
				CTM.Item_Name AS Method_Term,
				CUD.Duration,
				CUD.Accuracy,
				CUD.Parameter_Concept_Key,
				CTP.Item_Name AS Parameter_Term,
				CUD.Unit_Concept_Key,
				CTU.Item_Name AS Unit_Term,
				CUD.Lower_Value AS Value,
				CUD.Lower_Value, 
				CUD.Upper_Value,				
				CUD.Custodian,
				CUD.[Timestamp],
				S.Date_Time_Start
		FROM 		Collection_Unit_Data CUD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
		LEFT JOIN 	Session S ON CUD.Entered_Session_ID = S.Session_ID
		LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
		WHERE 		CUD.Collection_Unit_Key = @Key 
		AND 		CUD.Is_Descriptor = @IsDescriptor
	END
	ELSE
	-- Measurements wants to match on Collection_Unit_Data_Key because this is
	-- the key that is stored in the nodes.
	BEGIN
		SELECT 		CUD.Collection_Unit_Data_Key AS Item_Key,
				CUD.Applies_To,
				CUD.Method_Concept_Key,
				CTM.Item_Name AS Method_Term,
				CUD.Duration,
				CUD.Accuracy,
				CUD.Parameter_Concept_Key,
				CTP.Item_Name AS Parameter_Term,
				CUD.Unit_Concept_Key,
				CTU.Item_Name AS Unit_Term,
				CUD.Lower_Value AS Value,
				CUD.Lower_Value, 
				CUD.Upper_Value,				
				CUD.Custodian,
				CUD.[Timestamp],
				S.Date_Time_Start
		FROM 		Collection_Unit_Data CUD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
		LEFT JOIN 	Session S ON CUD.Entered_Session_ID = S.Session_ID
		LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
		WHERE 		CUD.Collection_Unit_Data_Key = @Key 
		AND 		CUD.Is_Descriptor = @IsDescriptor
	END

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Select TO [Dev - JNCC SQL]
END
GO