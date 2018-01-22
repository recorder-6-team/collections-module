/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_InternalDepartments_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_InternalDepartments_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of departments within the organisation

  Created:	Feb 2004

  Last revision information:
    $Revision: 3 $
    $Date: 20/02/04 16:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_InternalDepartments_Select]
 AS

SELECT Item_Name, Organisation_Department_Key 
FROM Organisation_Department OD
INNER JOIN Setting S ON S.Data=OD.Name_Key
AND S.[Name]='HoldingOrg'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InternalDepartments_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_InternalDepartments_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_InternalDepartments_Select TO [Dev - JNCC SQL]
END

GO