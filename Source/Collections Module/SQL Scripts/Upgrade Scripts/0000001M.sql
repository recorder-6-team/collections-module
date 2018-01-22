/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 30/01/06 10:29 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date, T.Item_Name) AS Item_Name,
			T.Language_Key,
			L.Item_Name AS Language,
			CListSynonyms.Custodian,
			S.User_Name_Key AS Entered_By
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	INNER JOIN 	Language AS L			ON L.Language_Key=T.Language_Key
	LEFT JOIN		Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	LEFT JOIN Session S ON S.Session_ID=CListSynonyms.Entered_Session_ID
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Department_Get_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns an department name for an individual record.

  Parameters:	@Key		Individual key
		@GetAcronym 	If this is set to 1, then the acronym of the 
				department will be returned. If the department
				does not have an acronym, then the department
				name will be returned instead.
		@Output		Department name (Output parameter)
		@ForceHoldingOrg	If 1, then department only returned if
			part of this organisation

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 30/01/06 10:29 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
	@Key char(16),
	@GetAcronym bit = NULL,
	@ForceHoldingOrg bit=0,
	@Output varchar(100) output
AS

SET NOCOUNT ON

IF @ForceHoldingOrg =0
	SELECT 		@Output = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
					THEN IsNull(OD.Acronym, OD.Item_Name)
					ELSE OD.Item_Name
				END 
	FROM 		Organisation_Department OD
	INNER JOIN 	Organisation O ON O.Name_Key = OD.Name_Key
	INNER JOIN 	Individual I ON I.Organisation_Department_Key = OD.Organisation_Department_Key
	WHERE 		I.Name_Key=@Key
ELSE
	SELECT 		@Output = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
					THEN IsNull(OD.Acronym, OD.Item_Name)
					ELSE OD.Item_Name
				END 
	FROM 		Organisation_Department OD
	INNER JOIN 	Organisation O ON O.Name_Key = OD.Name_Key
	INNER JOIN 	Individual I ON I.Organisation_Department_Key = OD.Organisation_Department_Key
	INNER JOIN 	Setting S ON S.[Name]='HoldingOrg' AND S.Data=O.Name_Key
	WHERE 		I.Name_Key=@Key


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Department_Get_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Department_Get_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [Dev - JNCC SQL]
END

GO