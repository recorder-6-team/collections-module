If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_MovementMaterialCount_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_MovementMaterialCount_Get]
GO

/*===========================================================================*\
  Description:	For a given Movement_Of_Material_Key, this proc counts the 
		number of other Movement_Of_Material records that have the
		same Movement_Direction key as it. This is needed to decide
		whether to load the MovementDetailsCollectionUnits frame 
		(this tab page is only loaded if there are 2 Movement_Of_Material 
		records for the same Movement_Direction_Key).

  Parameters:	@Key	Movement_Of_Material_Key
		@Count 	OUTPUT	

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/01/04 15:06 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MovementMaterialCount_Get] 
@Key char(16),
@Count int OUTPUT

AS
	SELECT @Count =	Count(DISTINCT MOM_Sharing.Movement_Of_Material_Key)
	FROM		Movement_Of_Material AS MOM_Original
	INNER JOIN	Movement_Of_Material AS MOM_Sharing ON MOM_Sharing.Movement_Direction_Key = MOM_Original.Movement_Direction_Key
	WHERE		MOM_Original.Movement_Of_Material_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialCount_Get TO [Dev - JNCC SQL]
END

GO