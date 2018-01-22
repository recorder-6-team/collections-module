/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Select]
GO

/*===========================================================================*\
  Description:	Selects an occurrence record.

  Parameters:	@Key

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 1/03/04 16:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Select]
	@Key char(16)
AS
	-- Options to get performance benefits from VW_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		O.Occurrence_Key,
			O.Surveyors_Ref,
			O.Record_Type_Concept_Key AS [Record_Type_Key],
			CTRT.Item_Name AS [Record_Type_Name],
			O.Comment, 
			O.Confidential, 
			O.Checked, 
			O.Checked_By,
			O.Checked_Date,
			O.Verified, 
			O.Custodian, 
			O.TimeStamp,
			LD.Domain_Key
	FROM		Occurrence O
	LEFT JOIN 	VW_ConceptTerm CTRT ON CTRT.Concept_Key = O.Record_Type_Concept_Key

	-- Extra bit to try and obtain the Domain key for the Record_Type concept group
	LEFT JOIN	Determination D1 ON D1.Occurrence_Key = O.Occurrence_Key AND D1.Preferred = 1
	LEFT JOIN	Specimen_Field_Data SFD ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	Determination D2 ON D2.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND D2.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTP ON 
				(CTP.Concept_Key = D1.Concept_Key AND D1.Concept_Key IS NOT NULL) OR
				(CTP.Concept_Key = D2.Concept_Key AND D2.Concept_Key IS NOT NULL)
	LEFT JOIN	Concept_Group CG ON CG.Concept_Group_Key = CTP.Concept_Group_Key
	LEFT JOIN	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key

	WHERE		O.Occurrence_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Select TO [Dev - JNCC SQL]
END
GO