If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_Domain_Update_ForSubjectArea]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Domain_Update_ForSubjectArea]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a Domain to different 
				Subject Area, then it is necessary to recalulate the entire 
				domain structure of the collection database for all relate date

  Parameters:
	@ParentKey	subject_Area_Key
	@ChildKey	Domain_Key
 

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Domain_Update_ForSubjectArea] 
	@ParentKey	CHAR(16),
	@ChildKey	CHAR(16)
AS

	SET NOCOUNT ON

	UPDATE	Domain
	SET		Subject_Area_Key = @ParentKey
	WHERE	Domain_Key		 = @ChildKey

	SET NOCOUNT OFF

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_Domain_Update_ForSubjectArea]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Update_ForSubjectArea'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [Dev - JNCC SQL]
END

GO