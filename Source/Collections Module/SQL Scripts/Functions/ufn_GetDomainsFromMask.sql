SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_GetDomainsFromMask') IS NOT NULL
	DROP FUNCTION dbo.ufn_GetDomainsFromMask
GO

/*============================================================================*\
  Description:	Returns a semi-colon delimited list of domain names for the 
				given domain mask.

  Parameters:	@Mask					Mask to decode

  Created:		November 2009

  Last revision information:
    $Revision: 1 $
    $Date: 4/11/09 11:59 $
    $Author: Andrewkemp $

\*============================================================================*/
CREATE FUNCTION dbo.ufn_GetDomainsFromMask(
	@Mask								INT)
	RETURNS								VARCHAR(1000)
AS
BEGIN
	DECLARE		@Name					VARCHAR(100),
				@Domains				VARCHAR(1000)
	
	SET			@Domains				=	''
	
	DECLARE		domain_cursor CURSOR FOR
	SELECT		Item_Name
	FROM		Domain
	WHERE		Domain_Mask & @Mask		>	0
	ORDER BY	Item_Name
	
	OPEN		domain_cursor

	FETCH NEXT
	FROM		domain_cursor
	INTO		@Name

	WHILE @@FETCH_STATUS = 0 
	BEGIN
		IF @Domains <> ''
			SET			@Domains				=	@Domains + '; ' + @Name
		ELSE
			SET			@Domains				=	@Name

		FETCH NEXT
		FROM		domain_cursor
		INTO		@Name
	END

	CLOSE		domain_cursor
	DEALLOCATE	domain_cursor

	RETURN		@Domains
END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_GetDomainsFromMask') IS NOT NULL
BEGIN
    PRINT 'Setting up security on function ufn_GetDomainsFromMask'
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_RecordCardsOnly
END
GO

