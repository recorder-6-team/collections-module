/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AnyMeasurementParameter_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AnyMeasurementParameter_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of any concept that can be used as a 
		measurement parameter

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 1/03/04 14:45 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AnyMeasurementParameter_Select]

AS

SET NOCOUNT ON

	SELECT CT.Concept_Key, CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS
FROM Concept_Group CG
INNER JOIN VW_ConceptTerm CT ON CT.Concept_Group_Key=CG.Concept_Group_Key
		AND CT.Is_Current=1
INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
INNER JOIN Domain D ON D.Domain_Key=LD.Domain_Key
		AND (D.Has_Occurrences=1 OR D.Domain_Key='SYSTEM00000000')
WHERE CG.Item_Name='Measurement Parameters'
ORDER BY D.Item_Name, CT.Plaintext

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AnyMeasurementParameter_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AnyMeasurementParameter_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AnyMeasurementParameter_Select TO [Dev - JNCC SQL]
END

GO
