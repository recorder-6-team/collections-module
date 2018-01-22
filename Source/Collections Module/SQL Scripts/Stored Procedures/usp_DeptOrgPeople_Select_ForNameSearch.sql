/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DeptOrgPeople_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DeptOrgPeople_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/04/04 11:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeptOrgPeople_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 'Individual' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Name_Key AS Item_Key, 'Organisation' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Organisation_Department_Key + '*' AS Item_Key, 'Department' AS Type,
		Item_Name AS DisplayTerm, 
		Item_Name AS SearchTerm
	FROM	Organisation_Department
	WHERE	Item_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'

	-- Set the order here for all
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeptOrgPeople_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeptOrgPeople_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeptOrgPeople_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO