/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Movement Details screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/12/03 17:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Select]
	@Key char(16)
AS

SET NOCOUNT ON

DECLARE @HoldingOrg char(16)
SELECT @HoldingOrg=DATA FROM Setting WHERE [Name]='HoldingOrg'

	SELECT 	
		MOM.Movement_Of_Material_Key,
		MOM.Contact_Name_Key,
		dbo.ufn_GetFormattedName(MOM.Contact_Name_Key) AS Contact_Name,
		MOM.Vague_Date_Start, 
		MOM.Vague_Date_End, 
		MOM.Vague_Date_Type, 
		MOM.Completed,
		MOM.Receiver_Name_Key,
 		CASE 
			-- If the Organisation Department Key is null, then return the receiver_name_key.
			WHEN OD.Organisation_Department_Key IS NULL THEN MOM.Receiver_Name_Key
			-- Otherwise return the Organisation Department Key with a '*' added to the end.
			ELSE OD.Organisation_Department_Key + '*'
		END AS Receiver_Key,
  		CASE 
			-- If the Organisation name is not known, the just display the standard name caption
			WHEN OD.Organisation_Department_Key IS NULL THEN dbo.ufn_GetFormattedName(MOM.Receiver_Name_Key)
			-- If the Organisation name is known...
			ELSE 
				CASE MOM.Receiver_Name_Key
					-- If it is the organisation holding this installation of the software, then
					-- the organisation name is omitted (i.e. just department name).
					WHEN @HoldingOrg THEN OD.Item_Name
					-- Otherwise show the Organisation Name and Department Name.
					ELSE dbo.ufn_GetFormattedName(MOM.Receiver_Name_Key) + ' - ' + OD.Item_Name
				END
		END AS Receiver_Display,
		MOM.Value_Amount,
		MOM.Currency_Concept_Key,
		TCurrency.Item_Name AS Currency_Name,  
		MOM.Acquisition_Method_Concept_Key,
		TAcquisitionMethod.Item_Name AS Acquisition_Method_Name,
		MOM.Notes,
		MOM.Custodian,
		MOM.Timestamp
	
	FROM 	Movement_Of_Material AS MOM
	LEFT JOIN Organisation_Department AS OD ON OD.Organisation_Department_Key = MOM.Receiver_Organisation_Department_Key
	LEFT JOIN VW_ConceptTerm AS TAcquisitionMethod ON MOM.Acquisition_Method_Concept_Key = TAcquisitionMethod.Concept_Key
	LEFT JOIN VW_ConceptTerm AS TCurrency ON MOM.Currency_Concept_Key = TCurrency.Concept_Key

	WHERE MOM.Movement_Of_Material_Key = @Key

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Select TO [Dev - JNCC SQL]
END

GO