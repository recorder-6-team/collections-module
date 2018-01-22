If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Conservation_Check_Key and Display_Caption when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-08

SET NOCOUNT ON

SELECT CC.Conservation_Check_Key AS Item_Key, Display_Caption AS DisplayTerm

FROM Conservation_Check CC
	INNER JOIN 	CONCEPT C
	ON 		CC.Type_Concept_Key = C.Concept_Key
	AND 		((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))

WHERE Search_Caption LIKE @SearchText+'%'
ORDER BY Display_Caption
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearch TO [Dev - JNCC SQL]
END

GO