/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenName_Get]
GO

/*===========================================================================*\
  Description:	Returns the specimen name for the given specimen key.

  Parameters:	@StoreKey
		@StoreName	Output

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 21/12/07 13:21 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenName_Get]
	@Key char(16),
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SpecimenName varchar(100) OUTPUT 
AS
	SELECT 	@SpecimenName =     
			CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, 1)
			END
	FROM 		Specimen_Unit 			SU
    INNER JOIN	Collection_Unit 		CU	ON 	SU.Collection_Unit_Key 				= CU.Collection_Unit_Key
											AND SU.Collection_Unit_Key			 	= @Key
									        AND ((CU.Domain_Mask & @UserDomainMask 	> 0) 
											OR 	(CU.Entered_Session_ID 				= @SessionID) 
											OR 	(CU.Changed_Session_ID 				= @SessionID) 
											OR 	(CU.Domain_Mask 					= 0))
	LEFT JOIN 	Collection_Unit_Number 	CUN	ON 	SU.Collection_Unit_Key		 		= CUN.Collection_Unit_Key
											AND CUN.Preferred 						= 1 
	LEFT JOIN	Determination 			D 	ON 	SU.Preferred_Determination_Key 		= D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CT	ON 	D.Concept_Key 						= CT.Concept_Key 
	LEFT JOIN 	Taxon_Determination 	TD	ON 	SU.Preferred_Taxon_Determination_Key= TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name 		ITN	ON 	TD.Taxon_List_Item_Key 				= ITN.Taxon_List_Item_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenName_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_SpecimenName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SpecimenName_Get TO [Dev - JNCC SQL]
END
GO