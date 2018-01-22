/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpatialRef_Select_ForLocation]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpatialRef_Select_ForLocation]
GO

/*===========================================================================*\
  Description:	Returns the spatial ref and spatial ref qualifier for a 
		location key.

  Parameters:	@Key	Location key

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 26/03/04 14:53 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpatialRef_Select_ForLocation]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
		Spatial_Ref AS SpatialRef,
		Spatial_Ref_Qualifier AS Qualifier,
		Spatial_Ref_System AS [System],
		Lat,
		Long
	FROM	Location
	WHERE	Location_Key = @Key


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpatialRef_Select_ForLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpatialRef_Select_ForLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpatialRef_Select_ForLocation TO [Dev - JNCC SQL]
END

GO