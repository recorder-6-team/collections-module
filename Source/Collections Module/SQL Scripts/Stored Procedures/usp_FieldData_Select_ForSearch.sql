If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_FieldData_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_FieldData_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_FieldData_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Sample_Key and Display_Caption when search characters are entered.
--  The Sample table does not have a Display_Caption or Search_Caption field,
--  so the caption must be constructed through joins to other tables.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned.
--	@SessionID 		User's SessionID.
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-09-24

SET NOCOUNT ON

	SELECT 		SFD.Specimen_Field_Data_Key AS Item_Key, 
				dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, 'dd/mm/yyyy') AS DisplayTerm
	FROM 		[Sample] S
	LEFT JOIN 	Occurrence AS O 	ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO 	ON XO.Sample_Key = S.Sample_Key
	INNER JOIN 	Specimen_Field_Data SFD ON (O.Occurrence_Key = SFD.Occurrence_Key
							OR XO.Taxon_Occurrence_key = SFD.Taxon_Occurrence_Key)
	WHERE (([dbo].ufn_GetDateFromVagueDate
		(S.Vague_Date_Start, S.Vague_Date_End, 'dd/mm/yyyy')  IS NOT NULL)
		AND (([dbo].ufn_GetDateFromVagueDate
		(S.Vague_Date_Start, S.Vague_Date_End, 'dd/mm/yyyy') ) LIKE @SearchText + '%'))

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSearch TO [Dev - JNCC SQL]
END

GO