/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupQualityCheck_Insert')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroupQualityCheck_Insert
GO

/*===========================================================================*\
  Description:	Inserts an entry to the Concept Group Quality Check table.

  Parameters:	@Concept_Group_Key,
				@Checked_By_User	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroupQualityCheck_Insert
	@Concept_Group_Key	char(16),
	@Checked_By_User	char(16)
AS
	DECLARE @Key CHAR(16)
	EXEC dbo.spNextKey 'Concept_Group_Quality_Check', @Key OUTPUT

	INSERT INTO Concept_Group_Quality_Check(
		Concept_Group_Quality_Check_Key,
		Concept_Group_Key,
		Checked_Date_Time,
		Checked_By_User)
	VALUES (
		@Key,
		@Concept_Group_Key,
		GETDATE(),
		@Checked_By_User) 

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupQualityCheck_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupQualityCheck_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupQualityCheck_Insert TO [Dev - JNCC SQL]
END

GO