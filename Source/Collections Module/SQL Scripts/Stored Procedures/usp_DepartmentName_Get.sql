/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DepartmentName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DepartmentName_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Item_Name or acronym from the 
	Organisation_Department table.

  Parameters:	@Key	Collection unit key
		@GetAcronym - if 1 then the acronym is returned if available
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/01/06 9:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DepartmentName_Get]
	@NameKey char(16),
	@GetAcronym bit=0,
	@FormattedName varchar(100) OUTPUT
AS

	SELECT	@FormattedName = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
				THEN IsNull(OD.Acronym, OD.Item_Name)
				ELSE OD.Item_Name
				END
	FROM	Organisation_Department AS OD
	WHERE	OD.Organisation_Department_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DepartmentName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DepartmentName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [Dev - JNCC SQL]
END

GO