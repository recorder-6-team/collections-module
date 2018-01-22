/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectAreaDomains_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectAreaDomains_Select]
GO

/*===========================================================================*\
  Description:	

  Parameters:	@Domain_Mask - 32 bit integer consisting of the domain's security
		bits to return.  To return all domains, specify -1

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 28/01/04 13:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectAreaDomains_Select]
	@UserDomainMask INT
AS
IF @UserDomainMask>-1 
  -- select domains that the user should see, including all domains that don't have occurrences
  SELECT D.Domain_Key, S.Item_Name as Subject_Area, D.Item_Name as Domain
  FROM Subject_Area S
      INNER JOIN Domain D on D.Subject_Area_Key=S.Subject_Area_Key
      AND (((D.Domain_Mask & @UserDomainMask>0) OR (D.Domain_Mask = 0))
      OR D.Has_Occurrences=0)
  ORDER BY S.Item_Name, D.Item_Name
ELSE
  -- select all domains
  SELECT D.Domain_Key, S.Item_Name as Subject_Area, D.Item_Name as Domain
  FROM Subject_Area S
      INNER JOIN Domain D on D.Subject_Area_Key=S.Subject_Area_Key
  ORDER BY S.Item_Name, D.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectAreaDomains_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectAreaDomains_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectAreaDomains_Select TO [Dev - JNCC SQL]
END

GO