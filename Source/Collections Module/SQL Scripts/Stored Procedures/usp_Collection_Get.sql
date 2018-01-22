If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Get]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Get] 
@Key CHAR(16),
@Caption VARCHAR(150) OUTPUT

AS

--  DESCRIPTION
--  Returns a collection's caption given the key
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Key of record to be returned
--	@Caption			Output caption
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-11-03
--
SET NOCOUNT ON

SELECT @Caption = C.Item_Name + CASE WHEN M.Number IS NULL THEN '' ELSE ' - ' + M.Number END
FROM 
COLLECTION C
LEFT JOIN
	(MOVEMENT_COLLECTION_UNIT MCU
	INNER JOIN
		MOVEMENT_DIRECTION MD
	ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
		AND (MD.Outbound = 0)
	INNER JOIN
		MOVEMENT M
	ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
	LEFT JOIN
		(CONCEPT CON
		INNER JOIN 
			TERM T
		ON CON.Term_Key = T.Term_Key)
	ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
ON C.Collection_Unit_Key = MCU.Collection_Unit_Key
WHERE C.Collection_Unit_Key = @Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Get TO [Dev - JNCC SQL]
END

GO