/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Measurements_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Measurements_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns measurement records for a specified occurrence key.

  Parameters:	@OccurrenceKey

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Measurements_Select_ForOccurrence]
	@OccurrenceKey char(16)
AS

SET NOCOUNT ON

	SELECT		OD.Occurrence_Data_Key AS [Item_Key], 
			-- Format 107 = "Mmm dd, yyyy".
			OD.Applies_To + ' - ' + Convert(varchar, S.Date_Time_Start, 107) AS [Item_Name]
	FROM		Occurrence_Data OD
	INNER JOIN	Session S ON S.Session_ID = OD.Entered_Session_ID
	WHERE		Occurrence_Key = @OccurrenceKey
	AND		Is_Descriptor = 0	-- Measurements only.
	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Measurements_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForOccurrence TO [Dev - JNCC SQL]
END

GO