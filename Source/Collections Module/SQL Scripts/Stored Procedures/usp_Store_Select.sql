/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Store_Select]
GO
 
/*===========================================================================*\
  Description:	Returns a store record.

  Parameters:	@Key	Collection_Unit key

  Created:	Oct 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/03/05 10:23 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT		S.Collection_Unit_Key, -- required for async controls
			S.Item_Name,
			S.Store_Type_Concept_Key,
			CTS.PlainText AS TypeTerm,
			CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0 ELSE 1 END AS IsSpecimen,
			CU.Current_Container_Collection_Unit_Key,
			SCurrent.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Container,
			CU.Current_Location_Code,
			CU.Usual_Container_Collection_Unit_Key,
			SUsual.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Container,
			CU.Usual_Location_Code,
			S.Comment,
			CU.Custodian,
			S.[Timestamp] AS StoreTimestamp,
			CU.[Timestamp] AS UnitTimestamp

	FROM 		Store S
	INNER JOIN 	VW_ConceptTerm CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN 	Specimen_Unit SU ON SU.Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN 	Store SCurrent ON SCurrent.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCurrent.Collection_Unit_Key 
	INNER JOIN 	Store SUsual ON SUsual.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
	INNER JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUsual.Collection_Unit_Key 

	WHERE S.Collection_Unit_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	        GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Select TO [Dev - JNCC SQL]
END
GO