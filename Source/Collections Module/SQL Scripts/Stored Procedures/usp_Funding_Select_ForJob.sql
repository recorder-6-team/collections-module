If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Funding_Select_ForJob]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Funding_Select_ForJob]
GO

CREATE PROCEDURE [dbo].[usp_Funding_Select_ForJob] 
@UserID CHAR(16),
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Funding associated with a specified Job
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserID				Name_Key of current user
--	@ParentKey 			Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-27
--
SET NOCOUNT ON

SELECT Conservation_Job_Funding_Key AS Item_Key, 
	CASE WHEN N.Organisation = 0 THEN 
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
	ELSE 
		CASE WHEN O.Acronym IS NULL THEN 
			O.Full_Name 
		ELSE 
			O.Acronym + ', ' + O.Full_Name
		END
	END AS Item_Name
FROM 
CONSERVATION_JOB_FUNDING CJF
	INNER JOIN
		[NAME] N
	ON CJF.Funded_By_Name_Key = N.Name_Key AND CJF.Conservation_Job_Key = @ParentKey
	INNER JOIN
		[USER] U
	ON U.Name_Key = @UserID 
	LEFT JOIN
		INDIVIDUAL I
	ON N.Name_Key = I.NAME_KEY
	LEFT JOIN
		ORGANISATION O
	ON N.Name_Key = O.NAME_KEY
WHERE U.ALLOW_FINANCE = 1
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Select_ForJob TO [Dev - JNCC SQL]
END

GO