/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MemberOfHoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	Takes a Name key and returns 1 if the person is a 
		member of the Holding Organisation and 0 if they aren't.
		If the Name key belongs to an Organisation, then 1 is also
		returned.

  Parameters:	@Key 	Individual_Key
		@IsMember bit (output)

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 19/05/04 15:35 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
	@Key char(16),
	@IsMember bit OUTPUT 
AS
	DECLARE	@HoldingOrgKey char(16)

	/*-----------------------------------------*\
	  Get the Holding Organisation key.
	\*-----------------------------------------*/
	SELECT 	@HoldingOrgKey = Data
	FROM 	Setting
	WHERE	[Name] = 'HoldingOrg'


	/*--------------------------------------------------------------------------------------------*\
	  See if the Individual is a member of the Holding Organisation or actually is the HoldingOrg.
	\*--------------------------------------------------------------------------------------------*/
	SELECT 		@IsMember = CASE WHEN Count(*) = 0 THEN 0 ELSE 1 END
	FROM		Individual AS I
	LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
						AND NR1.Name_Key_2 = @HoldingOrgKey
	LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_1 = @HoldingOrgKey
						AND NR2.Name_Key_2 = I.Name_Key
	INNER JOIN	Organisation AS O ON ((O.Name_Key = NR1.Name_Key_2) OR (O.Name_Key = NR2.Name_Key_1))
	WHERE		I.Name_Key = @Key
	OR		O.Name_Key = @Key   
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MemberOfHoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MemberOfHoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [Dev - JNCC SQL]
END

GO