/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_MaterialSummary_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_MaterialSummary_Get]
GO

/*===========================================================================*\
  Description:	Returns counts for the MaterialSummary section
		on FrameMovementGeneral.

  Parameters:	@Key		Movement key
		@Category	Whether we want the count for Collections (0),
				Specimens (1) or Storage (2) to be returned.

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_MaterialSummary_Get]
	@Key char(16),
	@Category tinyint,
	@Count varchar(20) output
AS

SET NOCOUNT ON

	IF @Category = 0 	-- i.e. Collections count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (MCU.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Collection U ON U.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key

	END
	ELSE IF @Category = 1	-- i.e. Specimens count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (MCU.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Specimen_Unit U ON U.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key
	END
	ELSE IF @Category = 2	-- i.e. Store count
	BEGIN
		SELECT @Count=CAST(Count(Distinct (S.Collection_Unit_Key)) AS VARCHAR(20))
		FROM Movement_Collection_Unit MCU
		INNER JOIN Specimen_Unit U ON U.Collection_Unit_Key=MCU.Collection_Unit_Key
		INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=U.Collection_Unit_Key
		INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
		INNER JOIN Movement_Direction MD on MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		WHERE MD.Movement_Key=@Key
	END

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_MaterialSummary_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_MaterialSummary_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_MaterialSummary_Get TO [Dev - JNCC SQL]
END

GO