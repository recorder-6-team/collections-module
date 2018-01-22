/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of stores matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/09/04 18:20 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
	@SearchText varchar(100),
	@UserDomainMask INT,
	@SessionID CHAR(16)
AS
	SELECT		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS DisplayTerm, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS SearchTerm
	FROM		Store S
	INNER JOIN 	COLLECTION_UNIT CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
     				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		Item_Name LIKE @SearchText + '%'
	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO