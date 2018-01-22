/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OtherPartySameOrganisation_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OtherPartySameOrganisation_Get]
GO

/*===========================================================================*\
  Description:	This proc. checks that the 'Other Party' is not - or is not a 
		member of - the same organisation that the Staff Responsible
		comes from. Takes in the key of the 'Other Party' and the
		'Staff Responsible'. 'Other Party' may be the key of an 
		Individual or an Organisation. Returns 1 or 0 depending on 
		whether the Other Party is or isn't related to the organisation
		of the Staff Responsible. 

		We don't want this proc. to be run for Internal Transfers.

  Parameters:	@StaffResponsibleKey
		@OtherPartyKey 
		@SameOrganisation bit OUTPUT 

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/03/04 14:11 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OtherPartySameOrganisation_Get]
	@StaffResponsibleKey char(16),
	@OtherPartyKey char(16),
	@SameOrganisation bit OUTPUT 
AS
	SET NOCOUNT OFF

	DECLARE @OtherPartyIsOrganisation bit,
		@StaffResponsibleOrganisationKey char(16)

	/*--------------------------------------------------------------------*\
	  Staff Responsible must be an Individual. Get their Organisation.
	\*--------------------------------------------------------------------*/	
	SELECT 		@StaffResponsibleOrganisationKey = O.Name_Key
	FROM		Individual AS I
	LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
	LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
	INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
					  OR O.Name_Key = NR2.Name_Key_1)
	WHERE		I.Name_Key = @StaffResponsibleKey
	
	/*--------------------------------------------------------------------*\
	  If the individual is a member of move than one organisation, then
	  set @SameOrganisation to 0, because there is no way of knowing which
	  organisation they are moving the item to.
	\*--------------------------------------------------------------------*/	
	IF @@RowCount > 1 
	BEGIN
		SET @SameOrganisation = 0
		RETURN 0
	END

	/*---------------------------------------*\
	  See if Other Party is an organisation.
	\*---------------------------------------*/
	SELECT 	@OtherPartyIsOrganisation = Organisation
	FROM	[Name]
	WHERE	Name_Key = @OtherPartyKey

	/*--------------------------------------------------------------------*\
	  If Other Party is an organisation, check it isn't the same 
	  organisation that the the Staff Responsible is from.
	\*--------------------------------------------------------------------*/	
	IF @OtherPartyIsOrganisation = 1 BEGIN
		IF @StaffResponsibleOrganisationKey = @OtherPartyKey 
			SET @SameOrganisation = 1
	END
	ELSE

	BEGIN
		/*-----------------------------------------------------------------------------*\
		  If Other Party is an individual, check they aren't from the same 
		  organisation that the the Staff Responsible is from. If the Other Party
		  individual belongs to none, or more than one organisation it is permitted.
		  It is permitted when they belong to more than one organisation because we 
		  can't tell what organisation they are involving in the movement.
		\*-----------------------------------------------------------------------------*/
		SELECT @SameOrganisation = CASE WHEN Count(O.Name_Key) = 1 THEN 1 
									   ELSE 0
					END
		FROM			Individual AS I
		LEFT JOIN		Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
		LEFT JOIN		Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
		INNER JOIN		Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
								OR O.Name_Key = NR2.Name_Key_1)
		WHERE			I.Name_Key = @OtherPartyKey
		AND			O.Name_Key = @StaffResponsibleOrganisationKey
	END

	/*--------------------------------------------------------------------*\
	  Ensure that at least something is returned.
	\*--------------------------------------------------------------------*/	
	IF @SameOrganisation IS NULL SET @SameOrganisation = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OtherPartySameOrganisation_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OtherPartySameOrganisation_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OtherPartySameOrganisation_Get TO [Dev - JNCC SQL]
END

GO