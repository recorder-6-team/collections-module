/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FormattedNameForNameKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FormattedNameForNameKey_Get]
GO

/*===========================================================================*\
  Description:	Wrapper around the GetFormattedName function.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey
		@FormattedName	Output

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 24/03/04 16:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FormattedNameForNameKey_Get]
	@NameKey char(16),
	@FormattedName varchar(200) OUTPUT 
AS
	IF @NameKey IS NULL
		SET @FormattedName = 'Unknown'
	ELSE
		SELECT @FormattedName = dbo.ufn_GetFormattedName(@NameKey)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FormattedNameForNameKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FormattedNameForNameKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FormattedNameForNameKey_Get TO [Dev - JNCC SQL]
END

GO