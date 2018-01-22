If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_ForSample_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_ForSample_Get]
GO

/*===========================================================================*\
  Description: 	Retrieve the Survey Event key for a Sample.

  Parameters:	@Key 		Sample key
		@SurveyEventKey Survey Event Key (output)

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/03/04 14:54 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_SurveyEventKey_ForSample_Get] 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@SurveyEventKey = Survey_Event_Key
	FROM	[Sample]
	WHERE	Sample_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_ForSample_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_ForSample_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_ForSample_Get TO [Dev - JNCC SQL]
END

GO
