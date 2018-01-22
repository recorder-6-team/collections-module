/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Insert_ForSpecimen]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Insert_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Insert a record into the Store table if the user selects
		the Specimen Type to be a store from FrameSpecimenGeneral.

  Parameters:	@Key
		@ItemName
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 11:41 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Insert_ForSpecimen]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Insert in Store. The Store_Type_Concept_Key is hardcoded in
		  and represents 'unknown'
		\*-------------------------------------------------------------*/
		INSERT INTO Store (
			Collection_Unit_Key, Item_Name, Store_Type_Concept_Key,
			Entered_Session_ID
		) VALUES (
			@Key, @ItemName, 'SYSTEM0000000003', @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Insert_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Insert_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Insert_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Insert_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Insert_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Insert_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Insert_ForSpecimen TO [Dev - JNCC SQL]
END
GO