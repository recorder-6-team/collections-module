/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainMask_NextAvailable_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainMask_NextAvailable_Get]
GO

/*===========================================================================*\
  Description:	Goes through the possible Security Bits and returns the
				next available one. As soon as it gets to an available
				Security Bit, it quits the loop and the current
				Security Bit value is returned. If the value returned by this
				proc is 33, we can assume that all of the Domain Masks are
				in use.

  Parameters:	@SecurityBit OUTPUT

  Created:		September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/09/04 16:28 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainMask_NextAvailable_Get]
	@SecurityBit int output
AS

SET NOCOUNT ON
	
	DECLARE @DomainMask INT
	
	SET 	@SecurityBit = 1
	
	WHILE @SecurityBit < 33
	BEGIN
		-- When @SecurityBit = 32 we get an overflow error when converting to the Domain Mask,
		-- so deal with that case later on
		IF @SecurityBit < 32
		BEGIN
			SET @DomainMask = Power(2, @SecurityBit - 1) 
			IF NOT EXISTS(SELECT *
							FROM Domain
							WHERE Domain_Mask = @DomainMask)
				BREAK
		END
		ELSE
		-- If @SecurityBit = 32
		BEGIN
			IF NOT EXISTS(SELECT *
							FROM Domain
							WHERE Domain_Mask = -2147483648)
				BREAK
		END		
			
		SET @SecurityBit = @SecurityBit + 1
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainMask_NextAvailable_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainMask_NextAvailable_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [Dev - JNCC SQL]
END

GO