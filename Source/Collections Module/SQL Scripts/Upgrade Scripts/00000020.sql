/*===========================================================================*\
  The name of these procedures have changed, so the old procs need to be dropped
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRegistrationNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRegistrationNumber_Get]
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByRegistrationNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByRegistrationNumber]
GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByRegistrationNumber]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByRegistrationNumber]
GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenNameAndRegistration_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenNameAndRegistration_Get]
GO

/*===========================================================================*\
  The name of the function has changed, so the old functon needs to be dropped
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetRegNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetRegNumber
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetPrefNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetPrefNumber
GO

/*===========================================================================*\
  Description:	Returns the preferred number of a collection unit.

  Parameters:	@Key	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 15/06/07 13:07 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetPrefNumber]
	(@Key char(16))
RETURNS varchar(30)
AS
BEGIN
	DECLARE	@CollectionUnitNumber varchar(30)
	SET @CollectionUnitNumber = ''

	SELECT	TOP 1	@CollectionUnitNumber = CUN.Number

	FROM		Collection_Unit_Number AS CUN
	INNER JOIN	Concept AS C ON C.Concept_Key = CUN.Type_Concept_Key

	WHERE		CUN.Collection_Unit_Key = @Key
	AND		C.Preferred = 1
	AND		CUN.Preferred = 1

	RETURN @CollectionUnitNumber
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetPrefNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetPrefNumber'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetPrefNumber TO [Dev - JNCC SQL]
	END
GO