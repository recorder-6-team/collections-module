/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_RemoveHtmlMarkup]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_RemoveHtmlMarkup
GO

/*===========================================================================*\
  Description:	Remove HTML markup from the given string.

				Note that this is currently a very simplistic implementation
				that only deals with italics (the '<i>' element).

  Parameters:	@item_name				Text to be processed

  Created:		November 2003

  Last revision information:
	$Revision: 2 $
	$Date: 6/05/04 11:02 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_RemoveHtmlMarkup(
	@item_name		NVARCHAR(300))
RETURNS
	NVARCHAR(300)
AS
BEGIN
	DECLARE		@result		NVARCHAR(300)

	SET			@result		=	REPLACE (@item_name, '<i>', '')
	SET			@result		=	REPLACE (@result, '</i>', '')

	RETURN		@result
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_RemoveHtmlMarkup]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_RemoveHtmlMarkup'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_RemoveHtmlMarkup TO [Dev - JNCC SQL]
	END
GO