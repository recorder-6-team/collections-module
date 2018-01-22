/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainMask_Exists]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainMask_Exists]
GO

/*===========================================================================*\
  Description:	Takes in a Domain Mask and returns either 0 or 1 depending
		on whether that Domain Mask is used by another domain.		

  Parameters:	@DomainMask
		@MatchExists output

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/02/04 17:30 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainMask_Exists]
	@DomainMask int,
	@MatchExists bit output
AS

SET NOCOUNT ON
	
	DECLARE	@Matches int

	SELECT 	@Matches = Count(*)
	FROM	Domain
	WHERE	Domain_Mask = @DomainMask

	IF @Matches > 0 SET @MatchExists = 1
	ELSE 		SET @MatchExists = 0	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainMask_Exists') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainMask_Exists'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainMask_Exists TO [Dev - JNCC SQL]
END

GO