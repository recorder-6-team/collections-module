If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Related Names for a specified Collection

  Parameters:	
	@ParentKey	When specified, only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/08/11 15:51 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS

SET NOCOUNT ON

	SELECT 		N.Name_Key AS Rec_Item_Key, CUN.Collection_Unit_Name_Key AS Item_Key, N.Name_Key AS Join_Key,
			CASE 	WHEN N.Organisation = 0 THEN dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) + ' - ' + C.Published_Term
				ELSE 
					CASE 	WHEN O.Acronym IS NULL THEN O.Full_Name + ' - ' + C.Published_Term
						ELSE O.Acronym + ', ' + O.Full_Name + ' - ' + C.Published_Term
					END
			END AS Item_Name

	FROM 		Collection_Unit_Name CUN
	INNER JOIN	[Name] N ON CUN.Name_Key = N.Name_Key AND CUN.Collection_Unit_Key = @ParentKey
	INNER JOIN	Concept C ON CUN.Relation_Type_Concept_KEY = C.CONCEPT_KEY
	LEFT JOIN	Individual I ON N.Name_Key = I.Name_Key
	LEFT JOIN	Organisation O ON N.Name_Key = O.Name_Key

	WHERE 		(I.Name_Key IS NOT NULL) OR (O.Name_Key IS NOT NULL)

	ORDER BY 	Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PeopleOrganisationsOther_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO