/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForLocationFeature') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Measurements_Select_ForLocationFeature]
GO

/*===========================================================================*\
  Description:	Returns measurement records for a specified Location Feature key.

  Parameters:	@LocationFeatureKey

  Created:	September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 1/09/04 17:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Measurements_Select_ForLocationFeature]
	@LocationFeatureKey char(16)
AS

SET NOCOUNT ON

	SELECT		LFD.Location_Feature_Data_Key AS [Item_Key], 
			-- Format 107 = "Mmm dd, yyyy".
			LFD.Applies_To + ' - ' + Convert(varchar, S.Date_Time_Start, 107) AS [Item_Name]
	FROM		Location_Feature_Data LFD
	INNER JOIN	Session S ON S.Session_ID = LFD.Entered_Session_ID
	WHERE		Location_Feature_Key = @LocationFeatureKey
	AND		Is_Descriptor = 0	-- Measurements only.
	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForLocationFeature') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Measurements_Select_ForLocationFeature'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [Dev - JNCC SQL]
END
GO