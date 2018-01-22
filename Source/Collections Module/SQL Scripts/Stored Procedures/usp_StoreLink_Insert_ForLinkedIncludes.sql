If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoreLink_Insert_ForLinkedIncludes]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoreLink_Insert_ForLinkedIncludes]
GO

/*===========================================================================*\
  Description:	Inserts a parental link between the specified Parent store and Child store

  Parameters:
	@ParentKey 	Key of parent to which child is related to
	@ChildKey 	Key of child

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreLink_Insert_ForLinkedIncludes] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@IsPermanentMove BIT,
	@JoinKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SET @JoinKey = @ChildKey

	UPDATE 	CUChild
	SET 	CUChild.Current_Container_Collection_Unit_Key = @ParentKey,
		CUChild.Usual_Container_Collection_Unit_Key = 
			CASE WHEN @IsPermanentMove = 1 THEN @ParentKey ELSE CUChild.Usual_Container_Collection_Unit_Key END,
		CUChild.Current_Location_Code = CUParent.Usual_Location_Code,
		CUChild.Usual_Location_Code = 
			CASE WHEN @IsPermanentMove = 1 THEN CUParent.Usual_Location_Code ELSE CUChild.Usual_Location_Code END

	FROM 		Collection_Unit CUChild
	INNER JOIN 	Collection_Unit CUParent ON CUParent.Collection_Unit_Key = @ParentKey
	WHERE 		CUChild.Collection_Unit_Key = @ChildKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreLink_Insert_ForLinkedIncludes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreLink_Insert_ForLinkedIncludes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreLink_Insert_ForLinkedIncludes TO [Dev - JNCC SQL]
END
GO