/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForValuation]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForValuation]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Valuation join table 
		so that there is a relationship between the Valuation and 
		Collection Unit tables.

  Parameters:	@Key		The key generated for the Collection_Unit_Valuation is outputted.
		@ParentKey 	The key of the top level (parent) Valuation node.
		@ChildKey	The key of the added (child) Collection Unit node. 

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForValuation] 
	@ParentKey char(16),
	@ChildKey char(16),
	@SessionID char(16),
	@JoinKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Collection_Unit_Valuation', @JoinKey OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Valuation (
			Collection_Unit_Valuation_Key, Valuation_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@JoinKey, @ParentKey, @ChildKey, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForValuation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForValuation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForValuation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForValuation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForValuation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForValuation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForValuation TO [Dev - JNCC SQL]
END
GO