/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenNameAndRegistration_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenNameAndRegistration_Get]
GO

/*===========================================================================*\
  Description:	Returns the formatted name of a specimen, including registration 
		number if available.

  Parameters:	@Key
		@Name	OUTPUT 

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/12/07 13:21 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenNameAndRegistration_Get] 
	@Key char(16),
	@Caption varchar(150) OUTPUT
AS
SET NOCOUNT ON

	SELECT		@Caption = 
			CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name 
				ELSE 
					CASE ITN.Actual_Name_Italic 
						WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
						ELSE ITN.Actual_Name 
					END
			END +
			CASE 
				WHEN CUN.Number IS NOT NULL THEN + ' - ' + CUN.Number 
				ELSE + '' 
			END

	FROM		Specimen_Unit SU
	LEFT JOIN	(Determination D 
				INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination TD
				INNER JOIN	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
			ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN	Collection_Unit_Number CUN 
			ON CUN.Collection_Unit_Key = SU.Collection_Unit_Key

	WHERE		SU.Collection_Unit_Key = @Key
	AND		(CUN.Number IS NULL OR CUN.Preferred = 1)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenNameAndRegistration_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenNameAndRegistration_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [Dev - JNCC SQL]
END
GO
