/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectAreaKey_Get_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectAreaKey_Get_ForDomain]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	Domain key
  Return		@SubjectAreaKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectAreaKey_Get_ForDomain]
	@Key char(16),
	@SubjectAreaKey char(16) OUTPUT
AS
	SELECT 		@SubjectAreaKey = Subject_Area_Key
	FROM		Domain
	WHERE		Domain_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectAreaKey_Get_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_SubjectAreaKey_Get_ForDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [Dev - JNCC SQL]
END

GO