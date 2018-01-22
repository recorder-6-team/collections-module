/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectAreaAvailable_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectAreaAvailable_Get]
GO

/*===========================================================================*\
  Description:	Returns a value indicating if the subject area is available
								according to the user's domain mask

  Parameters:	
		@Key - @Subject_Area_Key
		@UserDomainMask		
		@IsAvailable - OUTPUT parameter

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 20/02/04 8:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectAreaAvailable_Get]
	@Key CHAR(16),
	@UserDomainMask INT,
	@IsAvailable BIT OUTPUT
AS

SELECT @IsAvailable = CASE SUM(D.Domain_Mask & @UserDomainMask) WHEN 0 THEN 0 ELSE 1 END
FROM Domain D
WHERE D.Subject_Area_Key=@Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectAreaAvailable_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectAreaAvailable_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectAreaAvailable_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectAreaAvailable_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectAreaAvailable_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectAreaAvailable_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectAreaAvailable_Get TO [Dev - JNCC SQL]
END

GO

