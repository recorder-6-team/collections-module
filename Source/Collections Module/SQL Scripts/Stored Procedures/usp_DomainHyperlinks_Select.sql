/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainHyperlinks_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainHyperlinks_Select]
GO

/*===========================================================================*\
  Description:	Select records from the Domain_Hyperlink table when given
		a local domain key.

  Parameters:	@Key	Local domain key

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 17:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainHyperlinks_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		
		Domain_Hyperlink_Key AS Item_Key,
		Item_Name,
		Image_File,
		URL,
		Use_Concept_Key,
		Word_Separator,
		Local_Domain_Key,
		[Timestamp],
		Custodian		
	FROM	Domain_Hyperlink
	WHERE	Local_Domain_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainHyperlinks_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainHyperlinks_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlinks_Select TO [Dev - JNCC SQL]
END

GO