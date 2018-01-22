/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SamplesInSurveyEvent_Count_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SamplesInSurveyEvent_Count_Get]
GO

/*===========================================================================*\
  Description:	Returns the number of Samples in a Survey_Event

  Parameters:	@Key  	Survey_Event_Key
		@Count OUTPUT

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/04/04 15:53 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SamplesInSurveyEvent_Count_Get] 
@Key char(16),
@Count int OUTPUT

AS

SET NOCOUNT ON

	SELECT 	@Count = Count(*)
	FROM	[Sample]
	WHERE	Survey_Event_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SamplesInSurveyEvent_Count_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SamplesInSurveyEvent_Count_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SamplesInSurveyEvent_Count_Get TO [Dev - JNCC SQL]
END

GO
