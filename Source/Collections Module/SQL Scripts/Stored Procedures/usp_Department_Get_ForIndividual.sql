/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Department_Get_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns an department name for an individual record.

  Parameters:	@Key		Individual key
		@GetAcronym 	If this is set to 1, then the acronym of the 
				department will be returned. If the department
				does not have an acronym, then the department
				name will be returned instead.
		@Output		Department name (Output parameter)

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/04/04 9:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
	@Key char(16),
	@GetAcronym bit = NULL,
	@Output varchar(100) output
AS

SET NOCOUNT ON

SELECT 		@Output = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
				THEN IsNull(OD.Acronym, OD.Item_Name)
				ELSE OD.Item_Name
			END 
FROM 		Organisation_Department OD
INNER JOIN 	Organisation O ON O.Name_Key = OD.Name_Key
INNER JOIN 	Individual I ON I.Organisation_Department_Key = OD.Organisation_Department_Key
WHERE 		I.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Department_Get_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Department_Get_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [Dev - JNCC SQL]
END

GO