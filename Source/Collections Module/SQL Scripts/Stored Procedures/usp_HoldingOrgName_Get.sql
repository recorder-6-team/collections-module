/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_HoldingOrgName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_HoldingOrgName_Get]
GO

/*===========================================================================*\
  Description:	Get the HoldingOrg name.

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 19/05/04 15:35 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_HoldingOrgName_Get]
	@HoldingOrg varchar(60) OUTPUT 
AS
	SELECT 		@HoldingOrg = O.Full_Name
	FROM 		Setting AS S
	INNER JOIN	Organisation AS O ON O.Name_Key = S.Data
	WHERE 		[Name] = 'HoldingOrg'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HoldingOrgName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HoldingOrgName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [Dev - JNCC SQL]
END

GO