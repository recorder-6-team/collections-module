/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainKey_Get_ForLocalDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainKey_Get_ForLocalDomain]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	Local Domain key
  Return		@DomainKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainKey_Get_ForLocalDomain]
	@Key char(16),
	@DomainKey char(16) OUTPUT
AS
	SELECT 		@DomainKey = Domain_Key
	FROM		Local_Domain
	WHERE		Local_Domain_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainKey_Get_ForLocalDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_DomainKey_Get_ForLocalDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [Dev - JNCC SQL]
END

GO

