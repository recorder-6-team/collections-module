/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForKeyListAndGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
GO
    
/*===========================================================================*\
  Description:	Returns all concept keys relating to the group and in a list of keys.

  Parameters:	@Concept_Group_Key	Key of the Concept group whose members we're retrieving
		@Key#			Keys to match

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 26/08/11 14:53 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
	@Concept_Group_Key1 as Char(16),
	@Concept_Group_Key2 as Char(16) = NULL,
	@Key1 as Char(16),
	@Key2 as Char(16) = NULL,
	@Key3 as Char(16) = NULL,
	@Key4 as Char(16) = NULL,
	@Key5 as Char(16) = NULL,
	@Key6 as Char(16) = NULL,
	@Key7 as Char(16) = NULL,
	@Key8 as Char(16) = NULL,
	@Key9 as Char(16) = NULL,
	@Key10 as Char(16) = NULL
AS
	SET NOCOUNT ON

	SELECT		C.Concept_Key, C.Published_Term as Plaintext
	FROM		Concept C  
	WHERE 		(C.Concept_Group_Key = @Concept_Group_Key1
	OR		 C.Concept_Group_Key = @Concept_Group_Key2)
	AND		C.List_Preferred = 1
	AND		C.Is_Current = 1
	AND		Concept_Key IN (@Key1, @Key2, @Key3, @Key4, @Key5, @Key6, @Key7, @Key8,@Key9, @Key10)
	ORDER BY	C.Sort_Code, C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForKeyListAndGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForKeyListAndGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [Dev - JNCC SQL]
END
GO
