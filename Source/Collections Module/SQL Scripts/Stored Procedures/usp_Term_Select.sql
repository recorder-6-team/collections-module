/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Select]
GO

/*===========================================================================*\
  Description:	Inserts a Term record

  Parameters:	
		@LanguageKey 
		@Plaintext 

  Created:	July 2011

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/11 13:23 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Select]
	@LanguageKey varchar(4),
	@Plaintext nvarchar(150)
AS	
	SELECT Term_Key
	FROM Term 
	WHERE Plaintext = @Plaintext and Language_Key = @LanguageKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.[usp_Term_Select]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.[usp_Term_Select] TO [Dev - JNCC SQL]
END
GO