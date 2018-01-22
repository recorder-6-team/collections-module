/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Domain General tab page in the 
		Thesaurus Editor.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 28/07/11 15:45 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		
			D.Item_Name, 
			D.Subject_Area_Key,
			D.Has_Occurrences,
			D.Default_Hierarchy_Relation_Type_Key,
			TRT.Item_Name AS Default_Hierarchy_Relation_Type_Name,
			D.Domain_Mask,
			D.Entered_Session_ID,
			D.Changed_Session_ID,
			D.System_Supplied_Data,
			D.Custodian,
			D.[Timestamp],
			D.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name
	FROM		Domain AS D
	LEFT JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = D.Default_Hierarchy_Relation_Type_Key 
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = D.Term_Generator_Key
	WHERE		D.Domain_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Select TO [Dev - JNCC SQL]
END

GO