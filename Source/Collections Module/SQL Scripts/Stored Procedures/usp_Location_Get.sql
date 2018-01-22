If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Location_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Location_Get]
GO

CREATE PROCEDURE [dbo].[usp_Location_Get] 
@Key CHAR(16),
@Caption VARCHAR(125) OUTPUT

AS

--  DESCRIPTION
--  Returns a Concept caption given the concept key
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

SELECT @Caption = Item_Name + ISNULL(' (' + Spatial_Ref + ')', '')
FROM
LOCATION L
INNER JOIN
	LOCATION_NAME LN
ON L.Location_Key = LN.Location_Key
WHERE L.Location_Key = @Key
	AND Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Location_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Location_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Location_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Location_Get TO [Dev - JNCC SQL]
END

GO