If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_NameRelation_Exists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_NameRelation_Exists_Get]
GO

CREATE PROCEDURE [dbo].[usp_NameRelation_Exists_Get] 
@NameKey1 CHAR(16),
@NameKey2 CHAR(16),
@RelationExists BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether or not a Relationship exists between two Name_Keys
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@NameKey1				First Name_Key
--	@NameKey2				Second Name_Key
--	@RelationExists			Output
--
--
--  AUTHOR:					Ben Collier, Dorset Software
--  CREATED:				01/03/2004
--

IF EXISTS(
	SELECT * 
	FROM Name_Relation 
	WHERE ((Name_Key_1 = @NameKey1) AND (Name_Key_2 = @NameKey2))
		OR ((Name_Key_1 = @NameKey2) AND (Name_Key_2 = @NameKey1)))
	SET @RelationExists = 1
ELSE
	SET @RelationExists = 0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NameRelation_Exists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NameRelation_Exists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NameRelation_Exists_Get TO [Dev - JNCC SQL]
END

GO