SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_DomainsForMask_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_DomainsForMask_Get
GO

/*============================================================================*\
  Description:	Returns a semi-colon delimited list of domain names for the 
				given domain mask.

  Parameters:	@Mask					Mask to decode
				@Domains				List of domains

  Created:		August 2003

  Last revision information:
	$Revision: 8 $
	$Date: 4/11/09 12:00 $
	$Author: Andrewkemp $

\*============================================================================*/
CREATE PROCEDURE dbo.usp_DomainsForMask_Get
	@Mask								INT,
	@Domains							VARCHAR(1000) OUTPUT
AS
	SELECT		@Domains				=	dbo.ufn_GetDomainsFromMask(@Mask)
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_DomainsForMask_Get') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_DomainsForMask_Get'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_RecordCardsOnly
END
GO
