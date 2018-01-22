If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectorsAndDeterminers_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Collectors And Determiners for a specified Collection

  Parameters:	
	@ParentKey	When specified, only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 15/04/04 15:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectorsAndDeterminers_Select_ForCollection] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT	I.Name_Key AS Item_Key,
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Item_Name,
			I.Name_Key AS Join_Key
	
	FROM 		Specimen_Unit AS SU
	INNER JOIN 	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
						AND ((CU.Domain_Mask & @UserDomainMask > 0) 
							OR (CU.Entered_Session_ID = @SessionID) 
							OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Specimen_Field_Data SFD 
			LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
			LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
			INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key	OR S.Sample_Key = XO.Sample_Key)
			INNER JOIN	Sample_Recorder AS SR ON SR.Sample_Key = S.Sample_Key
			INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
			) 
		ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event = 1

	LEFT JOIN 	Determination AS D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
	LEFT JOIN	Taxon_Determination TD 	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
	INNER JOIN	Individual AS I 
		ON (I.Name_Key = SER.Name_Key)
		OR (D.Determiner_Name_Key = I.Name_Key) 
		OR (TD.Determiner = I.Name_Key)

	WHERE		SU.Parent_Collection_Collection_Unit_Key = @ParentKey

	ORDER BY	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectorsAndDeterminers_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectorsAndDeterminers_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectorsAndDeterminers_Select_ForCollection TO [Dev - JNCC SQL]
END
GO