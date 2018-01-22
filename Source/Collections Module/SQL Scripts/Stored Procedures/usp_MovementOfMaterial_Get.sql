/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfMaterial_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfMaterial_Get]
GO
/*===========================================================================*\
  Description: 	Gets a detail for the caption of the Materials to unknown 
		destinations tab page control.	
  Parameters:	@Key	Movement of Material table key.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 1/08/08 9:27 $
    $Author: Johndurman $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfMaterial_Get]
	@Key char(16),
	@Caption varchar(100) OUTPUT
AS
	SELECT @Caption = 
		dbo.ufn_GetTranslation_String_From_Value(0, 'MATERIALMOVEMENT')		 	
		+ LOWER(dbo.ufn_GetMovementTypeName(M.Movement_Type))
		+ ' - ' 
		+ dbo.ufn_GetDateFromVagueDate(MOM.Vague_Date_Start, MOM.Vague_Date_End, MOM.Vague_Date_Type)
	FROM Movement_Of_Material MOM
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key = MOM.Movement_Direction_Key
	INNER JOIN Movement M ON M.Movement_Key = MD.Movement_Key
	WHERE MOM.Movement_Of_Material_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfMaterial_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfMaterial_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterial_Get TO [Dev - JNCC SQL]
END

GO