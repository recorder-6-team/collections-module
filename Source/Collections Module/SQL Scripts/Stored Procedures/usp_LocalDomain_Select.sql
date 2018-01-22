/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Local Domain General tab page in the 
		Thesaurus Editor.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 28/07/11 15:46 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		LD.Item_Name, 
			LD.Domain_Key,
			LD.Language_Key,
			L.Item_Name AS Language_Name,
			LD.Concept_Group_Label,
			LD.Entered_Session_ID,
			LD.Changed_Session_ID,
			LD.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name,
			LD.System_Supplied_Data,
			LD.Custodian,
			LD.[Timestamp]
	FROM		Local_Domain AS LD
	INNER JOIN	Language AS L ON L.Language_Key = LD.Language_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = LD.Term_Generator_Key
	WHERE		LD.Local_Domain_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [Dev - JNCC SQL]
END

GO