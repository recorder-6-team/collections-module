/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Delete_ForCollectionUnit]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Delete_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Deletes either measurements or descriptors from
		Collection_Unit_Data table.

  Parameters:	@CollectionUnitKey	Collection key
				@IsDescriptor		Flag to indicate whether Measurements or Descriptors
									are to be deleted.

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/01/04 12:59 $
    $Author: Bencollier $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Delete_ForCollectionUnit]
	@CollectionUnitKey CHAR(16),
	@IsDescriptor BIT
AS

SET NOCOUNT ON

DELETE 
FROM
Collection_Unit_Data
WHERE Collection_Unit_Key = @CollectionUnitKey 
	AND Is_Descriptor = @IsDescriptor
SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Delete_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Delete_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO